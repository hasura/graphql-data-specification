module DDL
  ( module M,
    Document (..),
    Entity (..),
    EntityName (..),
    Entities,
    buildEntities,
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
import Data.Coerce (coerce)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.Text (Text)
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

newtype EntityName = EntityName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data Entity
  = EntityModel !ModelDTO
  | EntityVirtualModel !VirtualModelDTO
  | EntityEnum !M.Enum
  | EntityScalar !ScalarDTO
  deriving (Show, Eq, Generic)

type Entities = Map.HashMap DDL.EntityName DDL.Entity

buildEntities :: Document -> Entities
buildEntities document =
  models <> virtualModels <> enums <> scalars
  where
    models = Map.fromList $ flip map document.models $ \model ->
      (coerce model.name, EntityModel model)

    virtualModels = Map.fromList $ flip map document.virtualModels $ \virtualModel ->
      (coerce virtualModel.name, EntityVirtualModel virtualModel)

    enums = Map.fromList $ flip map document.enums $ \enum ->
      (coerce enum.name, EntityEnum enum)

    scalars = Map.fromList $ flip map document.scalars $ \scalar ->
      (coerce scalar.name, EntityScalar scalar)
