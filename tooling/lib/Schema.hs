{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Schema
  ( generateSchema,
    getBooleanExpressionNames,
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
import Schema.Model.Type.BooleanExpression.Name qualified as BooleanExpression
import Schema.Model.Type.ComparisonExpression.Definition qualified as ComparisonExpression
import Schema.Model.Type.SelectableField.Definition qualified as SelectableField
import Schema.Model.Type.SelectionSetAggregate.Definition qualified as SelectionSetAggregate
import Schema.Model.Type.SelectionSetFields.Definition qualified as SelectionSetFields
import Schema.Model.Type.SelectionSetGroup.Definition qualified as SelectionSetGroup
import Schema.Type.QueryRoot.Definition qualified as QueryRoot
import Schema.Type.QueryRoot.Name qualified as QueryRoot
import Data.Coerce (coerce)

type GraphQLTypeMap =
  Map.HashMap TypeGenerationRequest (GraphQL.TypeDefinition () GraphQL.InputValueDefinition)

getBooleanExpressionNames :: DDL.Document -> Map.HashMap DDL.ModelName GraphQL.Name
getBooleanExpressionNames document =
  Map.fromList $ flip map document.models $ \model ->
    (model.name, BooleanExpression.name model.name)

generateSchema ::
  DDL.Document -> Either [ErrorX] GraphQL.SchemaDocument
generateSchema document = do
  types <-
    fmap Map.elems $
      generateSchema_ (document, DDL.buildEntities document) mempty (Set.singleton TGRQueryRoot)
  pure $ GraphQL.SchemaDocument (schemaDefinition : map GraphQL.TypeSystemDefinitionType types)
  where
    schemaDefinition =
      GraphQL.TypeSystemDefinitionSchema $
        GraphQL.SchemaDefinition
          { _sdDirectives = Nothing,
            _sdRootOperationTypeDefinitions =
              [ GraphQL.RootOperationTypeDefinition
                  { _rotdOperationType = GraphQL.OperationTypeQuery,
                    _rotdOperationTypeType = QueryRoot.name
                  }
              ]
          }

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
    model <- getModel $ coerce modelName
    GraphQL.TypeDefinitionObject <$> SelectionSetFields.definition model
  TGRSelectionSetAggregate modelName -> do
    model <- getModel $ coerce modelName
    GraphQL.TypeDefinitionObject <$> SelectionSetAggregate.definition model
  TGRSelectionSetAggregateFunctionFields modelName functionName -> do
    model <- getModel $ coerce modelName
    GraphQL.TypeDefinitionObject <$> SelectionSetAggregateFunctionFields.definition model functionName
  TGRBooleanExpression modelName -> do
    model <- getModel $ coerce modelName
    GraphQL.TypeDefinitionInputObject <$> BooleanExpression.definition model
  TGRComparisonExpression scalarName -> do
    GraphQL.TypeDefinitionInputObject <$> ComparisonExpression.definition scalarName
  TGRSelectionSetGroup modelName ->
    GraphQL.TypeDefinitionObject <$> SelectionSetGroup.definition modelName
  TGRSelectableField modelName -> do
    model <- getModel $ coerce modelName
    GraphQL.TypeDefinitionEnum <$> SelectableField.definition model
  TGRQueryRoot -> GraphQL.TypeDefinitionObject <$> QueryRoot.definition
