module Schema.Model.Type.ComparisonExpression.Definition
  ( definition,
  )
where

import DDL qualified
import Data.Coerce (coerce)
import Language.GraphQL.Draft.Syntax as GraphQL
import Schema.Context
import Schema.Model.Type.ComparisonExpression.Name (name)
import Schema.NamingConvention

definition ::
  DDL.ScalarName ->
  Generate (GraphQL.InputObjectTypeDefinition GraphQL.InputValueDefinition)
definition scalarName = do
  let eqField =
        GraphQL.InputValueDefinition
          { _ivdDescription = Nothing,
            _ivdName = mkFieldName "_eq",
            _ivdType =
              GraphQL.TypeNamed (GraphQL.Nullability True) $
                mkTypeName $
                  coerce scalarName,
            _ivdDefaultValue = Nothing,
            _ivdDirectives = []
          }
  pure $
    GraphQL.InputObjectTypeDefinition
      { _iotdDescription = Nothing,
        _iotdName = name scalarName,
        _iotdDirectives = [],
        _iotdValueDefinitions = [eqField]
      }
