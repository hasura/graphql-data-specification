module Schema.Model.Type.ComparisonExpression.Name
  ( name,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.NamingConvention

name :: DDL.ScalarName -> GraphQL.Name
name scalarName = mkTypeName $ scalarName.wrapped <> "_comparison_expression"
