module Schema.Model.Type.SelectionSetGroup.Definition
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.SelectionSetGroup.Name (name)

definition ::
  DDL.ModelName ->
  Generate (GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition)
definition modelName = do
  selectionSetFieldsTypeName <- getTypeName $ TGRSelectionSetFields modelName
  selectionSetAggregateTypeName <- getTypeName $ TGRSelectionSetAggregate modelName
  let groupKey =
        GraphQL.FieldDefinition
          { _fldDescription = Nothing,
            _fldName = GraphQL.unsafeMkName "group_key",
            _fldArgumentsDefinition = [],
            _fldType =
              GraphQL.TypeNamed (GraphQL.Nullability False) selectionSetFieldsTypeName,
            _fldDirectives = []
          }
      groupAggregate =
        GraphQL.FieldDefinition
          { _fldDescription = Nothing,
            _fldName = GraphQL.unsafeMkName "group_aggregate",
            _fldArgumentsDefinition = [],
            _fldType =
              GraphQL.TypeNamed (GraphQL.Nullability False) selectionSetAggregateTypeName,
            _fldDirectives = []
          }
  pure $
    GraphQL.ObjectTypeDefinition
      { _otdDescription = Nothing,
        _otdName = name modelName,
        _otdImplementsInterfaces = [],
        _otdDirectives = [],
        _otdFieldsDefinition = [groupKey, groupAggregate]
      }
