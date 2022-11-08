module DDL.Scalar
  ( Scalar (..),
  )
where

import Data.Aeson qualified as Json
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype Scalar = Scalar {getScalar :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON)
