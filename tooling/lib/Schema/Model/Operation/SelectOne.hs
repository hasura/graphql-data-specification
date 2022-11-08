module Schema.Model.Operation.SelectOne
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.SelectionSetFields qualified as SelectionSetFields

generate ::
  DDL.ModelName ->
  [DDL.FieldDTO] ->
  DDL.UniqueIdentifier ->
  GraphQL.FieldDefinition GraphQL.InputValueDefinition
generate modelName fields uniqueIdentifier =
  GraphQL.FieldDefinition
    { _fldDescription = Nothing,
      _fldName = fieldName,
      _fldArgumentsDefinition = [],
      _fldType = fieldType,
      _fldDirectives = []
    }
  where
    fieldName =
      GraphQL.unsafeMkName $
        modelName.wrapped <> "_find_one_by_" <> uniqueIdentifier.name.wrapped
    fieldType =
      GraphQL.TypeNamed (GraphQL.Nullability True) $
        SelectionSetFields.typeName modelName
