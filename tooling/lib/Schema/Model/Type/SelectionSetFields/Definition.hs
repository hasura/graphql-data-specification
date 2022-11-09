module Schema.Model.Type.SelectionSetFields.Definition
  ( definition,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.Field qualified as Schema.Field
import Schema.Model.Type.SelectionSetFields.Name (name)

definition :: DDL.ModelDTO -> GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition
definition model =
  GraphQL.ObjectTypeDefinition
    { _otdDescription = Nothing,
      _otdName = name model.name,
      _otdImplementsInterfaces = [],
      _otdDirectives = [],
      _otdFieldsDefinition = fields
    }
  where
    fields = map Schema.Field.generate model.fields
