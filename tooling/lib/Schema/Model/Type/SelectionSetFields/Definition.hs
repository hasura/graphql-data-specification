module Schema.Model.Type.SelectionSetFields.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import DDL qualified
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.Model.Expression.Field qualified as Schema.Field
import Schema.Model.Operation.SelectAggregate qualified as SelectAggregate
import Schema.Model.Operation.SelectGroup qualified as SelectGroup
import Schema.Model.Operation.SelectList qualified as SelectList
import Schema.Model.Type.SelectionSetFields.Name (name)
import Schema.NamingConvention

definition ::
  DDL.ModelDTO -> Generate (GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition)
definition model = do
  edges <- fmap concat $ forM (fromMaybe [] model.edges) $ \edge -> do
    target <- getModel $ coerce edge.target
    case edge.kind of
      DDL.Object -> do
        selectionSetTypeName <- getTypeName $ TGRSelectionSetFields target.name
        let edgeType = GraphQL.TypeNamed (GraphQL.Nullability True) selectionSetTypeName
        pure [mkFieldDefinition (mkFieldName $ coerce edge.name) [] edgeType]
      DDL.Array -> do
        let fieldPrefix = mkFieldName $ coerce edge.name
        sequence
          [ SelectList.definition fieldPrefix model,
            SelectAggregate.definition fieldPrefix model,
            SelectGroup.definition fieldPrefix model
          ]
  pure $
    GraphQL.ObjectTypeDefinition
      { _otdDescription = Nothing,
        _otdName = name model.name,
        _otdImplementsInterfaces = [],
        _otdDirectives = [],
        _otdFieldsDefinition = fields <> edges
      }
  where
    fields = map Schema.Field.generate model.fields
