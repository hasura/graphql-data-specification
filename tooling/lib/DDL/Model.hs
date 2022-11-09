module DDL.Model
  ( Model (..),
    ModelName (..),
  )
where

import DDL.Edge
import DDL.Field
import DDL.UniqueIdentifier (UniqueIdentifier)
import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)
import Data.Hashable (Hashable)

newtype ModelName = ModelName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Json.ToJSONKey, Json.FromJSONKey, Hashable)

data Model modelReference fieldType = Model
  { name :: ModelName,
    fields :: [Field modelReference fieldType],
    edges :: [Edge modelReference],
    uniqueIdentifiers :: [UniqueIdentifier],
    supportedOperations :: Maybe SupportedOperations
  }
  deriving (Show, Eq, Generic)

instance
  (Json.ToJSON modelReference, Json.ToJSON fieldType) =>
  Json.ToJSON (Model modelReference fieldType)
  where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance
  (Json.FromJSON modelReference, Json.FromJSON fieldType) =>
  Json.FromJSON (Model modelReference fieldType)

instance
  (Hashable modelReference, Hashable fieldType) =>
  Hashable (Model modelReference fieldType)

data SupportedOperations = SupportedOperations
  { insert :: Bool,
    update :: Bool,
    delete :: Bool,
    select :: Bool
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SupportedOperations where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON SupportedOperations

instance Hashable SupportedOperations

-- data SomeModelName
--   = SomeModelNameModel ModelName
--   | SomeModelNameVirtualModel VirtualModelName
--   deriving (Show, Eq, Generic)
