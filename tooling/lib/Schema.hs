module Schema
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.SelectionSetSimple qualified as Schema.SelectionSetSimple

generate :: DDL.Document -> GraphQL.SchemaDocument
generate document =
  let allTypes = map GraphQL.TypeSystemDefinitionType simpleSelectionSets
   in GraphQL.SchemaDocument allTypes
  where
    simpleSelectionSets =
      map
        (GraphQL.TypeDefinitionObject . Schema.SelectionSetSimple.generate)
        document.models
