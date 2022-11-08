module Schema.Model.Expression.Field
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.FieldType qualified as Schema.FieldType

generate :: DDL.FieldDTO -> GraphQL.FieldDefinition GraphQL.InputValueDefinition
generate field =
  GraphQL.FieldDefinition
    { _fldDescription = Nothing,
      _fldName = fieldName,
      _fldArgumentsDefinition = [],
      _fldType = Schema.FieldType.generate field.returns,
      _fldDirectives = []
    }
  where
    fieldName = GraphQL.unsafeMkName field.name.wrapped
