module Schema.Model.ListArguments
  ( arguments,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.NamingConvention

arguments :: DDL.ModelName -> Generate (GraphQL.ArgumentsDefinition GraphQL.InputValueDefinition)
arguments modelName = do
  booleanExpressionTypeName <- getTypeName $ TGRBooleanExpression modelName
  let limit =
        mkInputValueDefinition (mkFieldName "limit") $
          GraphQL.TypeNamed (GraphQL.Nullability True) $
            mkTypeName "Int"
      offset =
        mkInputValueDefinition (mkFieldName "offset") $
          GraphQL.TypeNamed (GraphQL.Nullability True) $
            mkTypeName "Int"
      whereField =
        mkInputValueDefinition (mkFieldName "where") $
          GraphQL.TypeNamed (GraphQL.Nullability True) booleanExpressionTypeName
  pure [whereField, limit, offset]
