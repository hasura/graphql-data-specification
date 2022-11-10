module Schema.Model.Type.BooleanExpression.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import DDL qualified
import Data.Coerce (coerce)
import Data.Maybe (catMaybes, fromMaybe)
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.BooleanExpression.Name (name)
import Schema.NamingConvention

definition ::
  DDL.ModelDTO ->
  Generate (GraphQL.InputObjectTypeDefinition GraphQL.InputValueDefinition)
definition model = do
  fields <- fmap catMaybes $ forM model.fields $ \field -> do
    case field.returns of
      DDL.FieldTypeBase _ reference -> do
        targetEntity <- getEntity reference
        case targetEntity of
          Just (DDL.EntityScalar scalar) -> do
            comparisonExpressionType <- getTypeName $ TGRComparisonExpression scalar.name
            pure $
              Just $
                mkInputValueDefinition (mkFieldName $ coerce field.name) $
                  GraphQL.TypeNamed (GraphQL.Nullability True) comparisonExpressionType
          _ -> pure Nothing
      _ -> pure Nothing
  edges <- fmap concat $ forM (fromMaybe [] model.edges) $ \edge -> do
    target <- getModel $ coerce edge.target
    case edge.kind of
      DDL.Object -> do
        targetBooleanExpressionTypeName <- getTypeName $ TGRBooleanExpression target.name
        let existsField =
              mkInputValueDefinition (mkFieldName $ coerce edge.name) $
                GraphQL.TypeNamed (GraphQL.Nullability True) targetBooleanExpressionTypeName
        pure [existsField]
      DDL.Array -> do
        targetBooleanExpressionTypeName <- getTypeName $ TGRBooleanExpression target.name
        targetBooleanExpressionAggregateTypeName <-
          getTypeName $ TGRBooleanExpressionAggregate target.name
        let listField =
              mkInputValueDefinition (mkFieldName $ edge.name.wrapped <> "_list") $
                GraphQL.TypeNamed (GraphQL.Nullability True) targetBooleanExpressionTypeName
            aggregateField =
              mkInputValueDefinition (mkFieldName $ edge.name.wrapped <> "_aggregate") $
                GraphQL.TypeNamed (GraphQL.Nullability True) targetBooleanExpressionAggregateTypeName
        pure [listField, aggregateField]

  let listType =
        GraphQL.TypeList (GraphQL.Nullability True) $
          GraphQL.TypeNamed (GraphQL.Nullability False) $
            name model.name
      commonFields =
        [ mkInputValueDefinition (GraphQL.unsafeMkName "_and") listType,
          mkInputValueDefinition (GraphQL.unsafeMkName "_or") listType,
          mkInputValueDefinition (GraphQL.unsafeMkName "_not") $
            GraphQL.TypeNamed (GraphQL.Nullability True) $
              name model.name
        ]
  pure $
    mkInputObjectTypeDefinition (name model.name) $
      fields <> commonFields <> edges
