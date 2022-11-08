module DDL.Enum
  ( EnumName (..),
    EnumValue (..),
    Enum(..)
  )
where

import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype EnumName = EnumName {getEnumName :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

newtype EnumValue = EnumValue {getEnumValue :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

data Enum = Enum
  { name :: EnumName,
    values :: [EnumValue]
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON Enum where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON Enum

