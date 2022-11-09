module Schema.Model.Type.SelectableField.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import DDL qualified
import Data.Coerce (coerce)
import Data.Maybe (catMaybes)
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.SelectableField.Name (name)

definition ::
  DDL.ModelDTO ->
  Generate GraphQL.EnumTypeDefinition
definition model = do
  fields <- fmap catMaybes $ forM model.fields $ \field -> do
    case field.returns of
      DDL.FieldTypeBase _ reference -> do
        targetEntity <- getEntity reference
        case targetEntity of
          Just (DDL.EntityScalar _scalar) -> do
            pure $
              Just $
                GraphQL.EnumValueDefinition
                  { _evdDescription = Nothing,
                    _evdName = GraphQL.EnumValue $ GraphQL.unsafeMkName $ coerce field.name,
                    _evdDirectives = []
                  }
          _ -> pure Nothing
      _ -> pure Nothing
  pure $
    GraphQL.EnumTypeDefinition
      { _etdDescription = Nothing,
        _etdName = name model.name,
        _etdDirectives = [],
        _etdValueDefinitions = fields
      }
