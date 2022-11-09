module Schema.NamingConvention (mkTypeName, mkFieldName) where

import Data.Text (Text)
import Data.Text.Manipulate qualified as Text
import Language.GraphQL.Draft.Syntax qualified as GraphQL

mkTypeName :: Text -> GraphQL.Name
mkTypeName = GraphQL.unsafeMkName . Text.toPascal

mkFieldName :: Text -> GraphQL.Name
mkFieldName = GraphQL.unsafeMkName . Text.toCamel
