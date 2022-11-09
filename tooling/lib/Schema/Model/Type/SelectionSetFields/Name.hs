module Schema.Model.Type.SelectionSetFields.Name
  ( name,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.NamingConvention

name :: DDL.ModelName -> GraphQL.Name
name modelName = mkTypeName $ modelName.wrapped <> "_fields"
