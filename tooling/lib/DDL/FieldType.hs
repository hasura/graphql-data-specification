module DDL.FieldType
  ( FieldType (..),
    Nullability (..),
  )
where

import DDL.Reference
import Data.Aeson qualified as Json
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Language.GraphQL.Draft.Parser qualified as GraphQL
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Data.Hashable (Hashable)

data Nullability = Null | NonNull
  deriving (Show, Eq, Generic)
instance Hashable Nullability

data FieldType
  = FieldTypeBase Nullability Reference
  | FieldTypeList Nullability FieldType
  deriving (Show, Eq, Generic)
instance Hashable FieldType

instance Json.ToJSON FieldType where
  toJSON = Json.toJSON . convertFieldType

instance Json.FromJSON FieldType where
  parseJSON = Json.withText "FieldType" $ \s ->
    either (fail . Text.unpack) pure $ parseFieldType s

parseFieldType :: Text -> Either Text FieldType
parseFieldType t =
  transform <$> GraphQL.runParser GraphQL.graphQLType t
  where
    transform = \case
      GraphQL.TypeNamed nullability name ->
        FieldTypeBase (transformNullability nullability) $ Reference name.unName
      GraphQL.TypeList nullability ty ->
        FieldTypeList (transformNullability nullability) $ transform ty
    transformNullability (GraphQL.Nullability nullable) =
      if nullable then Null else NonNull

convertFieldType :: FieldType -> Text
convertFieldType = \case
  FieldTypeBase nullability reference ->
    reference.wrapped <> nullabilitySuffix nullability
  FieldTypeList nullability fieldType ->
    "[" <> convertFieldType fieldType <> "]" <> nullabilitySuffix nullability
  where
    nullabilitySuffix = \case
      Null -> ""
      NonNull -> "!"

-- data FieldBaseType
--   = FieldBaseTypeScalar ScalarType
--   | FieldTypeTypeModel ModelName
--   | FieldTypeTypeVirtualModel VirtualModelName
--   | FieldTypeTypeEnum EnumName
--   deriving (Show, Eq, Generic)
--
--
--  action: select_article
--    filter:
--      of_type:
--    masks:
--
--
--
-- type article_model {
--   id
--   name
-- }
-- article_group_by: [article_group]
-- type article_group {
--   group_key: article
--   group_aggregate: article_aggregate
-- }
--
-- authors whose technology category avg rating > 4
-- author.articles_grouped(group_by: [category])
--       .exists(
--          group_key.category = 'technology'
--          AND group_aggregate.avg.rating > 4
--       )
