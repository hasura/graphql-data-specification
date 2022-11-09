module Schema.Model.Type.SelectionSetAggregate.Name
  ( name,
  )
where

import DDL.Model qualified as DDL
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.NamingConvention

name :: DDL.ModelName -> GraphQL.Name
name modelName = mkTypeName $ modelName.wrapped <> "_aggregate"
