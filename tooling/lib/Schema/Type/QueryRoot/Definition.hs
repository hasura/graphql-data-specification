module Schema.Type.QueryRoot.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import Control.Monad.Reader (ask)
import DDL qualified
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.Model.Operation.SelectAggregate qualified as SelectAggregate
import Schema.Model.Operation.SelectGroup qualified as SelectGroup
import Schema.Model.Operation.SelectList qualified as SelectList
import Schema.Model.Operation.SelectOne qualified as SelectOne
import Schema.NamingConvention
import Schema.Type.QueryRoot.Name (name)

definition :: Generate (GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition)
definition = do
  (document, _entities) <- ask
  modelFields <- fmap concat $ forM document.models $ \model -> do
    let fieldPrefix = mkFieldName $ coerce model.name
    uniqueSelectionFields <-
      mapM (SelectOne.definition model.name model.fields) $
        fromMaybe [] model.uniqueIdentifiers
    listField <- SelectList.definition fieldPrefix model
    aggregateField <- SelectAggregate.definition fieldPrefix model
    groupField <- SelectGroup.definition fieldPrefix model
    pure $ uniqueSelectionFields <> [listField, aggregateField, groupField]
  let fields = modelFields
  pure $
    GraphQL.ObjectTypeDefinition
      { _otdDescription = Nothing,
        _otdName = name,
        _otdImplementsInterfaces = [],
        _otdDirectives = [],
        _otdFieldsDefinition = fields
      }
