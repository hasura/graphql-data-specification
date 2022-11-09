module DDL.Edge
  ( Edge (..),
    EdgeName (..),
    EdgeKind (..),
  )
where

import Data.Aeson qualified as Json
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

data EdgeKind = Object | Array
  deriving (Show, Eq, Generic)

instance Hashable EdgeKind

instance Json.ToJSON EdgeKind where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON EdgeKind

newtype EdgeName = EdgeName {getEdgeName :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data Edge modelReference = Edge
  { name :: EdgeName,
    kind :: EdgeKind,
    target :: modelReference
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON modelReference => Json.ToJSON (Edge modelReference) where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON modelReference => Json.FromJSON (Edge modelReference)

instance Hashable modelReference => Hashable (Edge modelReference)
