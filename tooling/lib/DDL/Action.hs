module DDL.Action
  ( Action (..),
  )
where

import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype ActionName = ActionName {getActionName :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)

data ActionKind = Query | Mutation
  deriving (Show, Eq, Generic)

instance Json.ToJSON ActionKind where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON ActionKind

data Action modelReference fieldType = Action
  { name :: ActionName,
    input :: modelReference,
    output :: fieldType,
    kind :: ActionKind
  }
  deriving (Show, Eq, Generic)

instance
  (Json.ToJSON modelReference, Json.ToJSON fieldType) =>
  Json.ToJSON (Action modelReference fieldType)
  where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance
  (Json.FromJSON modelReference, Json.FromJSON fieldType) =>
  Json.FromJSON (Action modelReference fieldType)

