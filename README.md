# TODO

- [ ] Wrap up Readme (Tanmai)
- [ ] Reference implementation of DGDL to GraphQL schema (Vamshi)
- [ ] GDS website with DGDL input, GraphiQL browser and NLS input (Rikin, Rishi)


# GraphQL Data Specification (GDS)

GDS is a fluent GraphQL API specification for transactional, analytical and streaming workloads across multiple data sources that contain semantically related data.

GDS solves for the following requirements:
- High performance out of the box: high-concurrency & low-latency
	- Automated query planning (compilation with JSON aggregation > data-loader > n+1)
	- Automated caching (cache-key discovery)
	- Authorization rules integrated into data fetching automatically
- Security
	- Intuitive fine-grained authorization
	- Declarative
- Standardization without losing flexibility
	- Federation across multiple types of sources (databases, API services)
	- Expose data-source specific capabilities (eg: specific types, operators or aggregation functions from upstream source)

There are 3 main components of this specification:
1. Domain Graph Description Language
2. Node Level Security
3. GraphQL Schema and API specification

## Domain Graph Description Language

The Domain Graph Description Language is a DSL that describes the business domain of the user and the relationships between nodes of that domain graph. This domain graph can be used by GraphQL engines to generate a GraphQL schema and API that allows clients to access and operate on the domain graph.

The DGDL has the following concepts:

- [Model](#model)
- [VirtualModel](#virtual-model)
- [Fields & Edges](#fields-&-edges)
- [Actions](#actions)
- [Boolean expressions)(#boolean-expressions)

### Model
A model that is backed by a data source and connected to other models in the same or other data sources. This model indicates whether it can be read from, and written to.
```
Model
  name :: String
  fields :: [Field]
  edges :: [Edge]
  selectable :: Boolean
  insertable :: Boolean
  updateable :: Boolean  // (only if selectable)
  deleteable :: Boolean  // (only if selectable)
  implements :: [VirtualModelName]
  implemented_by :: [VirtualModelName]
```

### Virtual Model
A virtual model is a complex entity of the domain that may not exist concretely in the upstream data sources but is brought into existence as a part of the data graph during the lifetime of a data API request/response.

A virtual model would typically be used to represent one of 3 types of concepts:
1. A complex property of a model
2. A piece of data that is required as an input to a data API request
3. A peice of data is output from a data API request

Like a model, a virtual model can also be semantically connected to other models and virtual models in the domain's data graph.

```
VirtualModel
	name :: String
  fields :: [Field]
  edges :: [Edge]
  implements :: [VirtualModelName]
  implemented_by :: [VirtualModelName]
```

### Fields & edges

Fields and edges represent properties of a model or a virtual model. Fields are used to represent a property of the model itself, and edges are properties of the model that reference other models or virtual models that the parent model is "related to".


```

Field
	name :: FieldName 
	input :: VirtualModel
	output :: FieldType

FieldType
	FieldTypeNamed FieldTypeName | 
	FieldTypeList FieldType | 
	FieldTypeNotNull FieldType

FieldTypeName 
	FieldTypeNameScalar ScalarFieldTypeName | 
	FieldTypeNameModel ModelName | 
	FieldTypeNameVirtualModel VirtualModelName | 
	FieldTypeNameEnum EnumName

ScalarFieldTypeName 
	Int | 
	String |
	Boolean | 
	Id | ... | 

Edge
	name :: String
	target :: VirtualModel | Model
  kind :: Object | Array

Enum
	name :: String
	values: [String]
```

### Actions

Actions are methods that operate on the domain graph. They take a node as an input, perform an operation on the underlying data sources and return a node as an output. The logic that executes inside an action is opaque to the DGDL and not its concern.

```
Action
	name :: ActionName
  input :: VirtualModel
  output :: VirtualModel
  operationType :: Read | Write
```

### Boolean Expressions

Boolean expressions that allow evaluating any part of the data graph against dynamic input. Since they are type-safe, boolean expressions are uniquely generated for every model and virtual model and allow the composition of fields & edges.

**For every model or virtual model, the following boolean expression is generated:**

`FieldBooleanExpression`:
1. `And` / `Or` / `Not` expressions that allow composition with boolean algebra
2. Boolean expressions for scalar fields that follow the following syntax:
```
   Field: { FieldTypeOperator: Input }
```
3. Boolean expressions for fields that take an input, follow the following syntax:
```
	Field: { arguments: {...}. output: FieldBooleanExpression}
```
4. Boolean expressions for fields that are models or virtual models, follow the following syntax:
```
	ModelName: {FieldName: FieldBooleanExpression}
```

Boolean expressions are the core of what allow validation, filtering and fine-grained security when accessing or operating on a data graph.

### Nodel Level Security

Node level security is a authorization policy engine that allows creating fine-grained policies and privileges to scope access and operations on a data graph for end users.

Node level security rules can be applied to Models, Virtual Models & Actions.

#### NLS for models

- Read operations:
	- Role name
	- Which fields can be selected
	- A filter: A boolean expression condition of the model that must evaluate to true to allow that "node" of the graph, or that instance of the model to be read
- Insert operations:
	- Role name
	- Which fields can be inserted to
	- Preset values for fields - from the data graph or the session object
	- A constraint: A boolean expression condition of the model that must evaluate to true to allow that "node" to be inserted into the data graph
- Update operations:
	- Role name
	- Which fields can be inserted to
	- Preset values for fields - from the data graph or the session object
	- A filter: A boolean expression condition of the model that must evaluate to true to allow that "node" to be updateable
- Delete operations:
	- Role name
	- A filter: A boolean expression condition of the model that must evaluate to true to allow that "node" to be deleteable
	
#### NLS for virtual models

A virtual model doesn't support any particular operations but only exists in the context of an operation on the data graph (an entity in the data API request/response).

- Role name
- Constraint: A boolean expression condition of the model that must evaluate to true to allow that virtual model's existence in the data graph to be allowed
	

## GraphQL schema & API

The GraphQL schema which defines the permitted list of operations on the data graph is created automatically from the Domain Graph Description Language with the following specification.

### Query

The Query field of the GraphQL API contains:
- For each model:
	- A GraphQL field to select one model, a list of models or an aggregate property of the model
- For each action:
	- A GraphQL field to invoke the action
	
#### GraphQL field to select one model or a list of models

Follows the following convention:
- GraphQL field name: ModelName or ModelName_One
- GraphQL field arguments:
	- where: ModelBooleanExpression
	- limit, offset
	- order_by: ModelSortExpression
- GraphQL field selection set:
	- The fields of the model
	- For each edge:
		- A field that represents select one instance or a list of instances from the edge
		- A field that allows selecting an aggregate property from the edge
		
#### GraphQL field for actions of operationType read

*TODO*

### Mutation

#### GraphQL field to insert models

*TODO*

#### GraphQL field to update models

*TODO*
	
#### GraphQL field to delete models

*TODO*

#### GraphQL field for actions of operationType write

*TODO*

### Subscription

#### GraphQL field for live queries

*TODO*

#### GraphQL field for streaming models

*TODO*


------------------


## Data source mapping

A data source contains one or both of the following entities:
1. Logical or physical data models that can be read or written
2. Methods that access underlying data models (read or write) and expose a view of the data

A data source can be any kind of database or an API service.

### Conventions for exposing models from a database
- The mapping layer should provide a way of creating a logical model from the underlying data model in the database
- The logical model definition should only depend on other entities within the same data source
- This includes computed fields, views, parameterized queries
- The logical model should indicate if it can be read from or written to (insert, update, delete)
- Each data source also indicates a set of types, and operators & functions that can operate on each type

**Examples:**
- Postgres table with computed fields
- Parameterized query (a parameterized view)

### Conventions for exposing endpoints from an API service
- The mapping layer should provide a way of annotating the following properties of an API backed resource:
	- (required) A Get method to fetch one instance of the resource
	- (optional) A List method to fetch multiple instances of the resource
	- (optional) A set of key parameters that are required to access the resource

**Examples:**
- An algolia search endpoint
- A createUser API endpoint
