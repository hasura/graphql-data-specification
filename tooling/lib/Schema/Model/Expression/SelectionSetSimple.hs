module Schema.Model.Expression.SelectionSetSimple
  ( generate,
  )
where

import DDL qualified
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Expression.Field qualified as Schema.Field

generate :: DDL.ModelDTO -> GraphQL.ObjectTypeDefinition GraphQL.InputValueDefinition
generate model =
  GraphQL.ObjectTypeDefinition
    { _otdDescription = Nothing,
      _otdName = typeName,
      _otdImplementsInterfaces = [],
      _otdDirectives = [],
      _otdFieldsDefinition = fields
    }
  where
    typeName = GraphQL.unsafeMkName model.name.wrapped
    fields = map Schema.Field.generate model.fields
