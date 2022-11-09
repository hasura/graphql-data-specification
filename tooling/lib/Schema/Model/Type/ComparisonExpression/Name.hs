module Schema.Model.Type.ComparisonExpression.Name
  ( name,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL

name :: DDL.ScalarName -> GraphQL.Name
name scalarName = GraphQL.unsafeMkName $ scalarName.wrapped <> "_comparison_expression"
