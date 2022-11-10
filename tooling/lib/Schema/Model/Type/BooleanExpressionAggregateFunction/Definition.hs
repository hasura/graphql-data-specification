module Schema.Model.Type.BooleanExpressionAggregateFunction.Definition
  ( definition,
  )
where

import Control.Monad (forM)
import Control.Monad.Validate (refute)
import DDL qualified
import Data.Coerce (coerce)
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.BooleanExpressionAggregateFunction.Name (name)
import Schema.NamingConvention

definition ::
  DDL.ModelDTO ->
  DDL.AggregationFunctionName ->
  Generate (GraphQL.InputObjectTypeDefinition GraphQL.InputValueDefinition)
definition model functionName = do
  let intFields = flip filter model.fields $
        \field -> (DDL.convertFieldType field.returns) `elem` ["Int", "Int!"]
  intFieldExpressions <-
    forM intFields $ \field -> do
      comparisonExpressionType <-
        getTypeName $ TGRComparisonExpression $ DDL.ScalarName "Int"
      pure $
        mkInputValueDefinition (mkFieldName $ coerce field.name) $
          GraphQL.TypeNamed (GraphQL.Nullability True) comparisonExpressionType
  fields <- case functionName.wrapped of
    "count" -> do
      forM model.fields $ \field -> do
        comparisonExpressionType <-
          getTypeName $ TGRComparisonExpression $ DDL.ScalarName "Int"
        pure $
          mkInputValueDefinition (mkFieldName $ coerce field.name) $
            GraphQL.TypeNamed (GraphQL.Nullability True) comparisonExpressionType
    "sum" -> pure intFieldExpressions
    "avg" -> pure intFieldExpressions
    _ ->
      refute $
        pure $
          "this aggregation function isn't supported: " <> functionName.wrapped
  pure $ mkInputObjectTypeDefinition (name model.name functionName) fields
