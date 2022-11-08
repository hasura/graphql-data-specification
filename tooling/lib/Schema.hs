module Schema
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.SelectionSetFields qualified as SelectionSetFields
import Schema.Model.Operation.SelectList qualified as SelectList

generate :: DDL.Document -> GraphQL.SchemaDocument
generate document =
  let allTypes =
        map GraphQL.TypeSystemDefinitionType (queryRoot : simpleSelectionSets)
   in GraphQL.SchemaDocument allTypes
  where
    simpleSelectionSets =
      map
        (GraphQL.TypeDefinitionObject . SelectionSetFields.generate)
        document.models

    listSelectionFields = map SelectList.generate document.models

    queryFields = listSelectionFields

    queryRoot =
      GraphQL.TypeDefinitionObject $
        GraphQL.ObjectTypeDefinition
          { _otdDescription = Nothing,
            _otdName = GraphQL.unsafeMkName "query_root",
            _otdImplementsInterfaces = [],
            _otdDirectives = [],
            _otdFieldsDefinition = queryFields
          }
