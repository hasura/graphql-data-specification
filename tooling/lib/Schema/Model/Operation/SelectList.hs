module Schema.Model.Operation.SelectList
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.SelectionSetFields qualified as SelectionSetSimple

generate :: DDL.ModelDTO -> GraphQL.FieldDefinition GraphQL.InputValueDefinition
generate model =
  GraphQL.FieldDefinition
    { _fldDescription = Nothing,
      _fldName = fieldName,
      _fldArgumentsDefinition = [],
      _fldType = fieldType,
      _fldDirectives = []
    }
  where
    fieldName = GraphQL.unsafeMkName $ model.name.wrapped <> "_list"
    fieldType =
      GraphQL.TypeList (GraphQL.Nullability False) $
        GraphQL.TypeNamed (GraphQL.Nullability False) $
          SelectionSetSimple.typeName model.name
