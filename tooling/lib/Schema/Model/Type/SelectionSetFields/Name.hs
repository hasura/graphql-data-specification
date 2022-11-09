module Schema.Model.Type.SelectionSetFields.Name
  ( name,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL

name :: DDL.ModelName -> GraphQL.Name
name modelName = GraphQL.unsafeMkName $ modelName.wrapped <> "_fields"
