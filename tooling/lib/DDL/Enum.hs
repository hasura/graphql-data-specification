module DDL.Enum
  ( EnumName (..),
    EnumValue (..),
    Enum (..),
  )
where

import Data.Aeson qualified as Json
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype EnumName = EnumName {getEnumName :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

newtype EnumValue = EnumValue {getEnumValue :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data Enum = Enum
  { name :: EnumName,
    values :: [EnumValue]
  }
  deriving (Show, Eq, Generic)

instance Hashable Enum

instance Json.ToJSON Enum where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON Enum
