module DDL
  ( module M,
    Document (..),
    ModelDTO,
    FieldDTO,
    ScalarDTO,
    VirtualModelDTO,
  )
where

import DDL.Action qualified as M
import DDL.Edge as M
import DDL.Enum as M
import DDL.Field as M
import DDL.FieldType as M
import DDL.Model as M
import DDL.Reference as M
import DDL.Scalar as M
import DDL.UniqueIdentifier as M
import DDL.VirtualModel as M
import Data.Aeson qualified as Json
import GHC.Generics (Generic)

type ModelDTO = M.Model M.Reference M.FieldType

type FieldDTO = M.Field M.Reference M.FieldType

type VirtualModelDTO = M.VirtualModel M.Reference M.FieldType

type ActionDTO = M.Action M.Reference M.FieldType

type ScalarDTO = M.Scalar M.FieldType

data Document = Document
  { models :: [ModelDTO],
    virtualModels :: [VirtualModelDTO],
    enums :: [M.Enum],
    scalars :: [ScalarDTO],
    actions :: [ActionDTO]
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON Document where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON Document
