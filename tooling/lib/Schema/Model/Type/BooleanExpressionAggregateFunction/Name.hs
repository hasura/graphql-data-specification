module Schema.Model.Type.BooleanExpressionAggregateFunction.Name
  ( name,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.NamingConvention

name :: DDL.ModelName -> DDL.AggregationFunctionName -> GraphQL.Name
name modelName functionName =
  mkTypeName $ modelName.wrapped <> "_boolean_expression_aggregate_" <> functionName.wrapped
