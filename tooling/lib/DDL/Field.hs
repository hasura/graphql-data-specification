module DDL.Field
  ( Field (..),
    FieldName (..),
  )
where

import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)
import Data.Hashable (Hashable)

newtype FieldName = FieldName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data Field modelReference fieldType = Field
  { name :: FieldName,
    input :: Maybe modelReference,
    returns :: fieldType
  }
  deriving (Show, Eq, Generic)

instance
  (Json.ToJSON fieldType, Json.ToJSON modelReference) =>
  Json.ToJSON (Field modelReference fieldType)
  where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance
  (Json.FromJSON modelReference, Json.FromJSON fieldType) =>
  Json.FromJSON (Field modelReference fieldType)

instance
  (Hashable modelReference, Hashable fieldType) =>
  Hashable (Field modelReference fieldType)
