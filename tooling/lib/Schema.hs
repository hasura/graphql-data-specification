{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Schema
  ( generateSchema,
  )
where

import Control.Monad (forM)
import DDL qualified
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.Model.Type.AggregateFunctionFields.Definition qualified as SelectionSetAggregateFunctionFields
import Schema.Model.Type.BooleanExpression.Definition qualified as BooleanExpression
import Schema.Model.Type.ComparisonExpression.Definition qualified as ComparisonExpression
import Schema.Model.Type.SelectionSetAggregate.Definition qualified as SelectionSetAggregate
import Schema.Model.Type.SelectionSetFields.Definition qualified as SelectionSetFields
import Schema.Model.Type.SelectionSetGroup.Definition qualified as SelectionSetGroup
import Schema.Type.QueryRoot.Definition qualified as QueryRoot

type GraphQLTypeMap =
  Map.HashMap TypeGenerationRequest (GraphQL.TypeDefinition () GraphQL.InputValueDefinition)

generateSchema ::
  DDL.Document -> Either [ErrorX] GraphQL.SchemaDocument
generateSchema document = do
  types <-
    fmap Map.elems $
      generateSchema_ (document, DDL.buildEntities document) mempty (Set.singleton TGRQueryRoot)
  pure $ GraphQL.SchemaDocument $ map GraphQL.TypeSystemDefinitionType types

generateSchema_ ::
  (DDL.Document, DDL.Entities) ->
  GraphQLTypeMap ->
  Set.HashSet TypeGenerationRequest ->
  Either [ErrorX] GraphQLTypeMap
generateSchema_ context generatedTypes requestedTypes = do
  let pendingTypes = requestedTypes `Set.difference` Map.keysSet generatedTypes
  if null pendingTypes
    then pure generatedTypes
    else do
      (newlyGeneratedTypes, newlyRequestedTypes) <-
        runGenerate context $
          fmap Map.fromList $
            forM (Set.toList pendingTypes) $ \pendingType ->
              (pendingType,) <$> generateTypeDefinition pendingType
      generateSchema_ context (generatedTypes <> newlyGeneratedTypes) newlyRequestedTypes

generateTypeDefinition ::
  TypeGenerationRequest ->
  Generate (GraphQL.TypeDefinition () GraphQL.InputValueDefinition)
generateTypeDefinition = \case
  TGRSelectionSetFields modelName -> do
    model <- getModel modelName
    pure $ GraphQL.TypeDefinitionObject $ SelectionSetFields.definition model
  TGRSelectionSetAggregate modelName -> do
    model <- getModel modelName
    GraphQL.TypeDefinitionObject <$> SelectionSetAggregate.definition model
  TGRSelectionSetAggregateFunctionFields modelName functionName -> do
    model <- getModel modelName
    GraphQL.TypeDefinitionObject <$> SelectionSetAggregateFunctionFields.definition model functionName
  TGRBooleanExpression modelName -> do
    model <- getModel modelName
    GraphQL.TypeDefinitionInputObject <$> BooleanExpression.definition model
  TGRComparisonExpression scalarName -> do
    GraphQL.TypeDefinitionInputObject <$> ComparisonExpression.definition scalarName
  TGRSelectionSetGroup modelName ->
    GraphQL.TypeDefinitionObject <$> SelectionSetGroup.definition modelName
  TGRQueryRoot -> GraphQL.TypeDefinitionObject <$> QueryRoot.definition
