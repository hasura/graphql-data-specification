module Schema.Type.QueryRoot.Name
  ( name,
  )
where

import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.NamingConvention

name :: GraphQL.Name
name = mkTypeName "query_root"
