module Schema.Type.QueryRoot.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import Control.Monad.Reader (ask)
import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.Model.Operation.SelectAggregate qualified as SelectAggregate
import Schema.Model.Operation.SelectGroup qualified as SelectGroup
import Schema.Model.Operation.SelectList qualified as SelectList
import Schema.Model.Operation.SelectOne qualified as SelectOne
import Schema.Type.QueryRoot.Name (name)

definition :: Generate (GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition)
definition = do
  (document, _entities) <- ask
  listSelectionFields <- mapM SelectList.definition document.models
  uniqueSelectionFields <- fmap concat $ forM document.models $ \model ->
    mapM (SelectOne.definition model.name model.fields) model.uniqueIdentifiers
  aggregateSelectionFields <- mapM SelectAggregate.definition document.models
  groupSelectionFields <- mapM SelectGroup.definition document.models
  let fields =
        listSelectionFields
          <> uniqueSelectionFields
          <> aggregateSelectionFields
          <> groupSelectionFields
  pure $
    GraphQL.ObjectTypeDefinition
      { _otdDescription = Nothing,
        _otdName = name,
        _otdImplementsInterfaces = [],
        _otdDirectives = [],
        _otdFieldsDefinition = fields
      }
