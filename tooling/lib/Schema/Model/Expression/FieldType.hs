module Schema.Model.Expression.FieldType
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.NamingConvention

generate :: DDL.FieldType -> GraphQL.GType
generate = \case
  DDL.FieldTypeBase nullability reference ->
    GraphQL.TypeNamed (transformNullability nullability) $
      mkTypeName reference.wrapped
  DDL.FieldTypeList nullability ty ->
    GraphQL.TypeList (transformNullability nullability) $ generate ty
  where
    transformNullability = \case
      DDL.Null -> GraphQL.Nullability True
      DDL.NonNull -> GraphQL.Nullability False
