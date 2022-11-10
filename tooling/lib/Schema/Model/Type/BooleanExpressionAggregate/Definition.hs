module Schema.Model.Type.BooleanExpressionAggregate.Definition
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.BooleanExpressionAggregate.Name (name)
import Schema.NamingConvention

definition ::
  DDL.ModelDTO ->
  Generate (GraphQL.InputObjectTypeDefinition GraphQL.InputValueDefinition)
definition model = do
  aggregateFields <-
    mapM
      (aggregateField model.name . DDL.AggregationFunctionName)
      ["count", "sum", "avg"]
  nodes <- nodesField model
  pure $
    mkInputObjectTypeDefinition
      (name model.name)
      (nodes : aggregateFields)

nodesField :: DDL.ModelDTO -> Generate GraphQL.InputValueDefinition
nodesField model = do
  booleanExpressionTypeName <- getTypeName $ TGRBooleanExpression model.name
  let fieldType =
        GraphQL.TypeNamed (GraphQL.Nullability True) booleanExpressionTypeName
  pure $ mkInputValueDefinition fieldName fieldType
  where
    fieldName = mkFieldName "nodes"

aggregateField ::
  DDL.ModelName ->
  DDL.AggregationFunctionName ->
  Generate GraphQL.InputValueDefinition
aggregateField modelName functionName = do
  aggregateFieldTypeName <- getTypeName $ TGRBooleanExpressionAggregateFunction modelName functionName
  let fieldType =
        GraphQL.TypeNamed (GraphQL.Nullability True) aggregateFieldTypeName
  pure $ mkInputValueDefinition fieldName fieldType
  where
    fieldName =
      mkFieldName functionName.wrapped
