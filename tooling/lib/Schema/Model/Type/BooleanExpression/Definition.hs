module Schema.Model.Type.BooleanExpression.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import DDL qualified
import Data.Coerce (coerce)
import Data.Maybe (catMaybes)
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.BooleanExpression.Name (name)
import Schema.NamingConvention

definition ::
  DDL.ModelDTO ->
  Generate (GraphQL.InputObjectTypeDefinition GraphQL.InputValueDefinition)
definition model = do
  fields <- fmap catMaybes $ forM model.fields $ \field -> do
    case field.returns of
      DDL.FieldTypeBase _ reference -> do
        targetEntity <- getEntity reference
        case targetEntity of
          Just (DDL.EntityScalar scalar) -> do
            comparisonExpressionType <- getTypeName $ TGRComparisonExpression scalar.name
            pure $
              Just $
                mkInputValueDefinition (mkFieldName $ coerce field.name) $
                  GraphQL.TypeNamed (GraphQL.Nullability True) comparisonExpressionType
          _ -> pure Nothing
      _ -> pure Nothing
  let listType =
        GraphQL.TypeList (GraphQL.Nullability True) $
          GraphQL.TypeNamed (GraphQL.Nullability False) $
            name model.name
      commonFields =
        [ mkInputValueDefinition (mkFieldName "_and") listType,
          mkInputValueDefinition (mkFieldName "_or") listType,
          mkInputValueDefinition (mkFieldName "_not") $
            GraphQL.TypeNamed (GraphQL.Nullability True) $
              name model.name
        ]
  pure $
    mkInputObjectTypeDefinition (name model.name) $
      fields <> commonFields
