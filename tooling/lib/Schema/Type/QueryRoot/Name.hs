module Schema.Type.QueryRoot.Name
  ( name,
  )
where

import Language.GraphQL.Draft.Syntax qualified as GraphQL

name :: GraphQL.Name
name = GraphQL.unsafeMkName "query_root"
