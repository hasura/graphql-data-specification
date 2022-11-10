module Schema.Model.Type.AggregateFunctionFields.Definition
  ( definition,
  )
where

import Control.Monad.Validate (refute)
import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Expression.Field qualified as Schema.Field
import Schema.Model.Type.AggregateFunctionFields.Name (name)

definition ::
  DDL.ModelDTO ->
  DDL.AggregationFunctionName ->
  Generate (GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition)
definition model functionName = do
  let intFields = flip filter model.fields $
        \field -> (DDL.convertFieldType field.returns) `elem` ["Int", "Int!"]
  fields <- case functionName.wrapped of
    "count" -> do
      let modifiedFields =
            flip map model.fields $
              \field -> field {DDL.returns = DDL.FieldTypeBase DDL.NonNull (DDL.Reference "Int")}
      pure $ map Schema.Field.generate modifiedFields
    "sum" -> pure $ map Schema.Field.generate intFields
    "avg" -> pure $ map Schema.Field.generate intFields
    _ ->
      refute $
        pure $
          "this aggregation function isn't supported: " <> functionName.wrapped
  pure $
    GraphQL.ObjectTypeDefinition
      { _otdDescription = Nothing,
        _otdName = name model.name functionName,
        _otdImplementsInterfaces = [],
        _otdDirectives = [],
        _otdFieldsDefinition = fields
      }
