module DDL.UniqueIdentifier
  ( UniqueIdentifierName (..),
    UniqueIdentifier (..),
  )
where

import DDL.Field (FieldName)
import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype UniqueIdentifierName = UniqueIdentifierName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

data UniqueIdentifier = UniqueIdentifier
  { name :: UniqueIdentifierName,
    fields :: [FieldName]
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON UniqueIdentifier where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON UniqueIdentifier
