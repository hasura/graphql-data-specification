module Schema.Model.Operation.SelectAggregate
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.Model.ListArguments qualified as ListArguments
import Schema.NamingConvention

definition :: DDL.ModelDTO -> Generate (GraphQL.FieldDefinition GraphQL.InputValueDefinition)
definition model = do
  selectionSetTypeName <- getTypeName $ TGRSelectionSetAggregate model.name
  let fieldType =
        GraphQL.TypeNamed (GraphQL.Nullability False) selectionSetTypeName
  arguments <- ListArguments.arguments model.name
  pure $
    GraphQL.FieldDefinition
      { _fldDescription = Nothing,
        _fldName = fieldName,
        _fldArgumentsDefinition = arguments,
        _fldType = fieldType,
        _fldDirectives = []
      }
  where
    fieldName = mkFieldName $ model.name.wrapped <> "_aggregate"
