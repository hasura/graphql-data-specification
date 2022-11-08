module Schema.Model.Expression.SelectionSetFields
  ( generate,
    typeName,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.Field qualified as Schema.Field

typeName :: DDL.ModelName -> GraphQL.Name
typeName modelName = GraphQL.unsafeMkName $ modelName.wrapped <> "_fields"

generate :: DDL.ModelDTO -> GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition
generate model =
  GraphQL.ObjectTypeDefinition
    { _otdDescription = Nothing,
      _otdName = typeName model.name,
      _otdImplementsInterfaces = [],
      _otdDirectives = [],
      _otdFieldsDefinition = fields
    }
  where
    fields = map Schema.Field.generate model.fields
