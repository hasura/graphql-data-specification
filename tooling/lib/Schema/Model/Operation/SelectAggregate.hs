module Schema.Model.Operation.SelectAggregate
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context

definition :: DDL.ModelDTO -> Generate (GraphQL.FieldDefinition GraphQL.InputValueDefinition)
definition model = do
  selectionSetTypeName <- getTypeName $ TGRSelectionSetAggregate model.name
  let fieldType =
        GraphQL.TypeNamed (GraphQL.Nullability False) selectionSetTypeName
  pure $
    GraphQL.FieldDefinition
      { _fldDescription = Nothing,
        _fldName = fieldName,
        _fldArgumentsDefinition = [],
        _fldType = fieldType,
        _fldDirectives = []
      }
  where
    fieldName = GraphQL.unsafeMkName $ model.name.wrapped <> "_aggregate"
