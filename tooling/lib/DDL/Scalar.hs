module DDL.Scalar
  ( Scalar (..),
    ScalarName (..),
    ScalarProfile (..),
    Operator (..),
    AggregationFunction (..),
  )
where

import Data.Aeson qualified as Json
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Prelude hiding (Enum)

newtype ScalarName = ScalarName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data Scalar typeDeclaration = Scalar
  { name :: ScalarName,
    profiles :: [ScalarProfile typeDeclaration]
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON typeDeclaration => Json.ToJSON (Scalar typeDeclaration) where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON typeDeclaration => Json.FromJSON (Scalar typeDeclaration)

instance Hashable typeDeclaration => Hashable (Scalar typeDeclaration)

newtype ScalarProfileName = ScalarProfileName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data ScalarProfile typeDeclaration = ScalarProfile
  { name :: ScalarProfileName,
    operators :: [Operator typeDeclaration],
    aggregationFunctions :: [AggregationFunction typeDeclaration]
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON typeDeclaration => Json.ToJSON (ScalarProfile typeDeclaration) where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON typeDeclaration => Json.FromJSON (ScalarProfile typeDeclaration)

instance Hashable typeDeclaration => Hashable (ScalarProfile typeDeclaration)

newtype AggregationFunctionName = AggregationFunctionName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data AggregationFunction fieldType = AggregationFunction
  { name :: AggregationFunctionName,
    output :: fieldType
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON typeDeclaration => Json.ToJSON (AggregationFunction typeDeclaration) where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON typeDeclaration => Json.FromJSON (AggregationFunction typeDeclaration)

instance Hashable typeDeclaration => Hashable (AggregationFunction typeDeclaration)

newtype OperatorName = OperatorName {wrapped :: Text}
  deriving (Show, Eq, Generic, Json.FromJSON, Json.ToJSON, Hashable)

data Operator rhsType = Operator
  { name :: OperatorName,
    rhs :: rhsType
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON rhsType => Json.ToJSON (Operator rhsType) where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON rhsType => Json.FromJSON (Operator rhsType)

instance Hashable rhsType => Hashable (Operator rhsType)
