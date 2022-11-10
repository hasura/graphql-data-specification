module Schema.Context
  ( TypeGenerationRequest (..),
    ErrorX,
    Generate,
    runGenerate,
    getTypeName,
    getEntity,
    getModel,
    mkInputValueDefinition,
    mkInputObjectTypeDefinition,
  )
where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Validate
import DDL qualified
import Data.Coerce (coerce)
import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Schema.Model.Type.AggregateFunctionFields.Name qualified as SelectionSetAggregateFunctionFields
import Schema.Model.Type.BooleanExpression.Name qualified as BooleanExpression
import Schema.Model.Type.ComparisonExpression.Name qualified as ComparisonExpression
import Schema.Model.Type.SelectableField.Name qualified as SelectableField
import Schema.Model.Type.SelectionSetAggregate.Name qualified as SelectionSetAggregate
import Schema.Model.Type.SelectionSetFields.Name qualified as SelectionSetFields
import Schema.Model.Type.SelectionSetGroup.Name qualified as SelectionSetGroup
import Schema.Type.QueryRoot.Name qualified as QueryRoot

type ErrorX = Text

data TypeGenerationRequest
  = TGRSelectionSetFields DDL.ModelName
  | TGRSelectionSetAggregate DDL.ModelName
  | TGRSelectionSetAggregateFunctionFields DDL.ModelName DDL.AggregationFunctionName
  | TGRSelectableField DDL.ModelName
  | TGRSelectionSetGroup DDL.ModelName
  | TGRBooleanExpression DDL.ModelName
  | TGRComparisonExpression DDL.ScalarName
  | TGRQueryRoot
  deriving (Show, Eq, Generic)

instance Hashable TypeGenerationRequest

type Generate a =
  ReaderT
    (DDL.Document, DDL.Entities)
    (StateT (Set.HashSet TypeGenerationRequest) (Validate [ErrorX]))
    a

runGenerate ::
  (DDL.Document, DDL.Entities) -> Generate a -> Either [ErrorX] (a, Set.HashSet TypeGenerationRequest)
runGenerate entities action =
  runValidate $ flip runStateT mempty $ runReaderT action entities

getTypeName :: TypeGenerationRequest -> Generate GraphQL.Name
getTypeName request = do
  modify' (Set.insert request)
  pure $ case request of
    TGRSelectionSetFields modelName -> SelectionSetFields.name modelName
    TGRSelectionSetAggregate modelName -> SelectionSetAggregate.name modelName
    TGRSelectionSetAggregateFunctionFields modelName functionName ->
      SelectionSetAggregateFunctionFields.name modelName functionName
    TGRSelectableField modelName -> SelectableField.name modelName
    TGRSelectionSetGroup modelName -> SelectionSetGroup.name modelName
    TGRComparisonExpression scalarName -> ComparisonExpression.name scalarName
    TGRBooleanExpression modelName -> BooleanExpression.name modelName
    TGRQueryRoot -> QueryRoot.name

getModel :: DDL.ModelName -> Generate DDL.ModelDTO
getModel modelName = do
  (_document, entities) <- ask
  case Map.lookup (coerce modelName) entities of
    Just (DDL.EntityModel model) -> pure model
    _ -> refute ["failed to lookup model: " <> modelName.wrapped]

getEntity :: DDL.Reference -> Generate (Maybe DDL.Entity)
getEntity modelName = do
  (_document, entities) <- ask
  pure $ Map.lookup (coerce modelName) entities

mkInputObjectTypeDefinition ::
  GraphQL.Name ->
  [GraphQL.InputValueDefinition] ->
  GraphQL.InputObjectTypeDefinition GraphQL.InputValueDefinition
mkInputObjectTypeDefinition name fields =
  GraphQL.InputObjectTypeDefinition
    { _iotdDescription = Nothing,
      _iotdName = name,
      _iotdDirectives = [],
      _iotdValueDefinitions = fields
    }

mkInputValueDefinition ::
  GraphQL.Name ->
  GraphQL.GType ->
  GraphQL.InputValueDefinition
mkInputValueDefinition name fieldType =
  GraphQL.InputValueDefinition
    { _ivdDescription = Nothing,
      _ivdName = name,
      _ivdType = fieldType,
      _ivdDefaultValue = Nothing,
      _ivdDirectives = []
    }
