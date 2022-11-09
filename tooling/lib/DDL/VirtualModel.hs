module DDL.VirtualModel
  ( VirtualModelName (..),
    VirtualModel (..),
  )
where

import DDL.Edge
import DDL.Field
import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)
import Data.Hashable (Hashable)

newtype VirtualModelName = VirtualModelName {getVirtualModelName :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data VirtualModel modelReference fieldType = VirtualModel
  { name :: VirtualModelName,
    fields :: [Field modelReference fieldType],
    edges :: [Edge modelReference],
    implementedBy :: [modelReference]
  }
  deriving (Show, Eq, Generic)

instance
  (Json.ToJSON modelReference, Json.ToJSON fieldType) =>
  Json.ToJSON (VirtualModel modelReference fieldType)
  where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance
  (Json.FromJSON modelReference, Json.FromJSON fieldType) =>
  Json.FromJSON (VirtualModel modelReference fieldType)

instance
  (Hashable modelReference, Hashable fieldType) =>
  Hashable (VirtualModel modelReference fieldType)

