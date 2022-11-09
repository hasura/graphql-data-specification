module Schema.Model.Type.AggregateFunctionFields.Name
  ( name,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL

name :: DDL.ModelName -> DDL.AggregationFunctionName -> GraphQL.Name
name modelName functionName = GraphQL.unsafeMkName $ modelName.wrapped <> "_aggregate_" <> functionName.wrapped
