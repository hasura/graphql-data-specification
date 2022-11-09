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
import qualified Schema.Type.QueryRoot.Definition as QueryRoot
import qualified Schema.Model.Type.SelectionSetFields.Definition as SelectionSetFields

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
  TGRSelectionSetAggregate modelName -> undefined
  TGRSelectionSetGroup modelName -> undefined
  TGRQueryRoot -> GraphQL.TypeDefinitionObject <$> QueryRoot.definition
