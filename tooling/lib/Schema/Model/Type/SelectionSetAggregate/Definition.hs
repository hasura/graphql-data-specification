module Schema.Model.Type.SelectionSetAggregate.Definition
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.SelectionSetAggregate.Name (name)

definition ::
  DDL.ModelDTO ->
  Generate (GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition)
definition model = do
  aggregateFields <-
    mapM
      (aggregateField model.name . DDL.AggregationFunctionName)
      ["count", "sum", "avg"]
  nodes <- nodesField model
  pure $
    GraphQL.ObjectTypeDefinition
      { _otdDescription = Nothing,
        _otdName = name model.name,
        _otdImplementsInterfaces = [],
        _otdDirectives = [],
        _otdFieldsDefinition = nodes : aggregateFields
      }

nodesField :: DDL.ModelDTO -> Generate (GraphQL.FieldDefinition GraphQL.InputValueDefinition)
nodesField model = do
  selectionSetTypeName <- getTypeName $ TGRSelectionSetFields model.name
  let fieldType =
        GraphQL.TypeList (GraphQL.Nullability False) $
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
    fieldName = GraphQL.unsafeMkName "nodes"

aggregateField ::
  DDL.ModelName ->
  DDL.AggregationFunctionName ->
  Generate (GraphQL.FieldDefinition GraphQL.InputValueDefinition)
aggregateField modelName functionName = do
  selectionSetTypeName <- getTypeName $ TGRSelectionSetAggregateFunctionFields modelName functionName
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
      GraphQL.unsafeMkName functionName.wrapped
