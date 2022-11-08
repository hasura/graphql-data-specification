module DDL.Model
  ( Model (..),
    ModelName (..),
  )
where

import DDL.Edge
import DDL.Field
import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype ModelName = ModelName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

data Model modelReference fieldType = Model
  { name :: ModelName,
    fields :: [Field modelReference fieldType],
    edges :: [Edge modelReference],
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

-- data SomeModelName
--   = SomeModelNameModel ModelName
--   | SomeModelNameVirtualModel VirtualModelName
--   deriving (Show, Eq, Generic)
