module Schema
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.SelectionSetFields qualified as SelectionSetFields
import Schema.Model.Operation.SelectList qualified as SelectList
import Schema.Model.Operation.SelectOne qualified as SelectOne

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
    uniqueSelectionFields =
      concatMap
        ( \model ->
            map
              (SelectOne.generate model.name model.fields)
              model.uniqueIdentifiers
        )
        document.models

    queryFields = listSelectionFields <> uniqueSelectionFields

    queryRoot =
      GraphQL.TypeDefinitionObject $
        GraphQL.ObjectTypeDefinition
          { _otdDescription = Nothing,
            _otdName = GraphQL.unsafeMkName "query_root",
            _otdImplementsInterfaces = [],
            _otdDirectives = [],
            _otdFieldsDefinition = queryFields
          }
