module Schema.Model.Operation.SelectGroup
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Context
import Schema.Model.ListArguments qualified as ListArguments

definition :: DDL.ModelDTO -> Generate (GraphQL.FieldDefinition GraphQL.InputValueDefinition)
definition model = do
  selectionSetTypeName <- getTypeName $ TGRSelectionSetGroup model.name
  let fieldType =
        GraphQL.TypeList (GraphQL.Nullability False) $
          GraphQL.TypeNamed (GraphQL.Nullability False) selectionSetTypeName
  arguments <- ListArguments.arguments model.name
  selectableFieldTypeName <- getTypeName $ TGRSelectableField model.name
  let groupByField =
        mkInputValueDefinition (GraphQL.unsafeMkName "group_by") $
          GraphQL.TypeList (GraphQL.Nullability True) $
            GraphQL.TypeNamed (GraphQL.Nullability False) selectableFieldTypeName
  pure $
    GraphQL.FieldDefinition
      { _fldDescription = Nothing,
        _fldName = fieldName,
        _fldArgumentsDefinition = groupByField : arguments,
        _fldType = fieldType,
        _fldDirectives = []
      }
  where
    fieldName = GraphQL.unsafeMkName $ model.name.wrapped <> "_group"
