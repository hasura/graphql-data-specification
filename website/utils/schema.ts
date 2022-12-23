import { buildClientSchema, buildASTSchema, buildSchema } from 'graphql/utilities'
import fetch from 'isomorphic-fetch'
import { load as loadYaml  } from 'js-yaml'
import { camelize } from 'inflection';
import { parse as parseSdl, Kind } from 'graphql/language'
import { isObjectType, isEnumType, isInputObjectType, TypeKind, GraphQLSchema, GraphQLScalarType, GraphQLNamedInputType, GraphQLType, GraphQLList, isScalarType, getNamedType, GraphQLInputObjectType, GraphQLInputType, GraphQLEnumType} from 'graphql/type'
import { DGDL_PROCESSING_SCHEMA_API } from './constants'

export const sampleDGDL = `models:
  - name: author
    uniqueIdentifiers:
      - name: id
        fields: [id]
    fields:
      - name: id
        returns: Int
      - name: name
        returns: String
    edges: []
  - name: article
    uniqueIdentifiers:
      - name: id
        fields: [id]
    fields:
      - name: id
        returns: Int
      - name: title
        returns: String
      - name: content
        returns: String
      - name: authorId
        returns: Int
    edges: []
virtualModels: []
enums: []
scalars:
  - name: Int
  - name: String
actions: []
`;

const sampleModelsResponse = {
  "booleanExpressionNames": {
    "author": "AuthorBooleanExpression",
    "article": "ArticleBooleanExpression"
  },
  "sdl": "schema  { query: QueryRoot }\n\ntype ArticleAggregate { avg: ArticleAggregateAvg\n  count: ArticleAggregateCount\n  nodes: [ArticleFields!]!\n  sum: ArticleAggregateSum\n}\n\ntype ArticleAggregateAvg { authorId: Int\n  id: Int\n}\n\ntype ArticleAggregateCount { authorId: Int!\n  content: Int!\n  id: Int!\n  title: Int!\n}\n\ntype ArticleAggregateSum { authorId: Int\n  id: Int\n}\n\ntype ArticleFields { authorId: Int\n  content: String\n  id: Int\n  title: String\n}\n\ntype ArticleGroup { groupAggregate: ArticleAggregate!\n  groupKey: ArticleFields!\n}\n\ntype AuthorAggregate { avg: AuthorAggregateAvg\n  count: AuthorAggregateCount\n  nodes: [AuthorFields!]!\n  sum: AuthorAggregateSum\n}\n\ntype AuthorAggregateAvg { id: Int\n}\n\ntype AuthorAggregateCount { id: Int!\n  name: Int!\n}\n\ntype AuthorAggregateSum { id: Int\n}\n\ntype AuthorFields { id: Int\n  name: String\n}\n\ntype AuthorGroup { groupAggregate: AuthorAggregate!\n  groupKey: AuthorFields!\n}\n\ntype QueryRoot { articleAggregate(where: ArticleBooleanExpression, limit: Int, offset: Int): ArticleAggregate!\n  articleFindOneById: ArticleFields\n  articleGroup(groupBy: [ArticleField!], where: ArticleBooleanExpression, limit: Int, offset: Int): [ArticleGroup!]!\n  articleList(where: ArticleBooleanExpression, limit: Int, offset: Int): [ArticleFields!]!\n  authorAggregate(where: AuthorBooleanExpression, limit: Int, offset: Int): AuthorAggregate!\n  authorFindOneById: AuthorFields\n  authorGroup(groupBy: [AuthorField!], where: AuthorBooleanExpression, limit: Int, offset: Int): [AuthorGroup!]!\n  authorList(where: AuthorBooleanExpression, limit: Int, offset: Int): [AuthorFields!]!\n}\n\nenum ArticleField {AuthorId \n  Content \n  Id \n  Title \n}\n\nenum AuthorField {Id \n  Name \n}\n\ninput ArticleBooleanExpression {and: [ArticleBooleanExpression!]\n  authorId: IntComparisonExpression\n  content: StringComparisonExpression\n  id: IntComparisonExpression\n  not: ArticleBooleanExpression\n  or: [ArticleBooleanExpression!]\n  title: StringComparisonExpression\n}\n\ninput AuthorBooleanExpression {and: [AuthorBooleanExpression!]\n  id: IntComparisonExpression\n  name: StringComparisonExpression\n  not: AuthorBooleanExpression\n  or: [AuthorBooleanExpression!]\n}\n\ninput IntComparisonExpression {eq: Int\n}\n\ninput StringComparisonExpression {eq: String\n}"
}

type SchemaResponse = {
	booleanExpressionNames: Record<string, string>,
	sdl: string
}

export const generateGraphQLSchemaFromModels = async (ddl=sampleDGDL): Promise<SchemaResponse> => {
	try {
		const ddglJson: any = loadYaml(ddl);
		const response = await fetch(DGDL_PROCESSING_SCHEMA_API, {
			method: 'POST',
			body: JSON.stringify(ddglJson)
		})
		if (response.status >= 300) {
			throw new Error('failed processing the DGDL yaml');
		}
		const resJson: SchemaResponse = await response.json();
		return resJson

	} catch (e: any) {
		console.error(e);
		throw new Error(`Error: ${e.message || "failed generating GraphQL schema for the given YAML"}`)
	}
}

export type Model = {
	name: string,
	boolExpression: {
		name: string,
		type: GraphQLInputType
	},
	fieldsType: GraphQLType
}

export const getGraphQLSchema = (schemaResponse: SchemaResponse) => {
	let schema: GraphQLSchema;
	try {
		schema = buildSchema(schemaResponse.sdl);
		return schema;
	} catch (e) {
		console.log(e);
		throw new Error('could not parse the generated GraphQL schema');
	}
}

export const getModels = (boolExp: SchemaResponse['booleanExpressionNames'], schema: GraphQLSchema): Model[] => {

	try {

		const schemaTypeMap = schema.getTypeMap()

		let models: Model[] = [];
		const allModels = Object.entries(boolExp);
		allModels.forEach(([ modelName, boolExpName]) => {
			const modelFieldsType = getNamedType(schemaTypeMap[camelize(`${modelName}_field`)])
			const boolExpType = getNamedType(schemaTypeMap[boolExpName]);
			if (isEnumType(modelFieldsType) && isInputObjectType(boolExpType)) {
				models.push({
					name: modelName,
					boolExpression: {
						name: boolExpName,
						type: boolExpType
					},
					fieldsType: modelFieldsType
				})
			}
		})

	  return models;

	} catch (e) {
		console.error(e);
		return []
	}
}

type NlsRule = {
	fields: GraphQLList<GraphQLType>,
	constraint: GraphQLInputType
}

export const generateNlsTypesFromModels = (models: Model[], modelName: string): NlsRule => {
	
	const map = {};

	const model = models.find(m => m.name === modelName);

	if (!model) {
		return {
			fields: [],
			constraint: {},
		} as any;
	}

	return {
		fields: new GraphQLList(model.fieldsType),
		constraint: model.boolExpression.type
	}
}
