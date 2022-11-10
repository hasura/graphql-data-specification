module Schema.Model.Operation.SelectOne
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.NamingConvention

definition ::
  DDL.ModelName ->
  [DDL.FieldDTO] ->
  DDL.UniqueIdentifier ->
  Generate (GraphQL.FieldDefinition GraphQL.InputValueDefinition)
definition modelName fields uniqueIdentifier = do
  selectionSetTypeName <- getTypeName $ TGRSelectionSetFields modelName
  let fieldType =
        GraphQL.TypeNamed (GraphQL.Nullability True) selectionSetTypeName
  pure $
    GraphQL.FieldDefinition
      { _fldDescription = Nothing,
        _fldName = fieldName,
        _fldArgumentsDefinition = [],
        _fldType = fieldType,
        _fldDirectives = []
      }
  where
    fieldName =
      mkFieldName $
        modelName.wrapped <> "_find_one_by_" <> uniqueIdentifier.name.wrapped
