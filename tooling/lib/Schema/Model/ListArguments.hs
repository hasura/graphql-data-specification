module Schema.Model.ListArguments
  ( arguments,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context

arguments :: DDL.ModelName -> Generate (GraphQL.ArgumentsDefinition GraphQL.InputValueDefinition)
arguments modelName = do
  booleanExpressionTypeName <- getTypeName $ TGRBooleanExpression modelName
  let limit =
        mkInputValueDefinition (GraphQL.unsafeMkName "limit") $
          GraphQL.TypeNamed (GraphQL.Nullability True) $
            GraphQL.unsafeMkName "Int"
      offset =
        mkInputValueDefinition (GraphQL.unsafeMkName "limit") $
          GraphQL.TypeNamed (GraphQL.Nullability True) $
            GraphQL.unsafeMkName "Int"
      whereField =
        mkInputValueDefinition (GraphQL.unsafeMkName "where") $
          GraphQL.TypeNamed (GraphQL.Nullability True) booleanExpressionTypeName
  pure [whereField, limit, offset]
