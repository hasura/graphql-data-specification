module DDL.Reference
  ( Reference (..),
  )
where

import Data.Aeson qualified as Json
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | A reference to an entity in the domain model:
-- model/virtualmodel/scalar/enum
newtype Reference = Reference {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)
