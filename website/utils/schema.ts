import { buildClientSchema } from 'graphql/utilities'
import { isObjectType, TypeKind, GraphQLScalarType, GraphQLNamedInputType, GraphQLType, GraphQLList, isScalarType, getNamedType, GraphQLInputObjectType, GraphQLInputType, GraphQLEnumType} from 'graphql/type'

export const sampleSchemaJson = {
	"data": {
		"__schema": {
			"queryType": {
				"name": "query_root"
			},
			"mutationType": {
				"name": "mutation_root"
			},
			"subscriptionType": {
				"name": "subscription_root"
			},
			"types": [{
				"kind": "SCALAR",
				"name": "Boolean",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "SCALAR",
				"name": "Float",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "SCALAR",
				"name": "Int",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "Int_comparison_exp",
				"description": "Boolean expression to compare columns of type \"Int\". All fields are combined with logical 'AND'.",
				"fields": null,
				"inputFields": [{
					"name": "_eq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_in",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_is_null",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Boolean",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_neq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nin",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "SCALAR",
				"name": "String",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "String_comparison_exp",
				"description": "Boolean expression to compare columns of type \"String\". All fields are combined with logical 'AND'.",
				"fields": null,
				"inputFields": [{
					"name": "_eq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_ilike",
					"description": "does the column match the given case-insensitive pattern",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_in",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "String",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_iregex",
					"description": "does the column match the given POSIX regular expression, case insensitive",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_is_null",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Boolean",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_like",
					"description": "does the column match the given pattern",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_neq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nilike",
					"description": "does the column NOT match the given case-insensitive pattern",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nin",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "String",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_niregex",
					"description": "does the column NOT match the given POSIX regular expression, case insensitive",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nlike",
					"description": "does the column NOT match the given pattern",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nregex",
					"description": "does the column NOT match the given POSIX regular expression, case sensitive",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nsimilar",
					"description": "does the column NOT match the given SQL regular expression",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_regex",
					"description": "does the column match the given POSIX regular expression, case sensitive",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_similar",
					"description": "does the column match the given SQL regular expression",
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "__Directive",
				"description": null,
				"fields": [{
					"name": "args",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__InputValue",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "description",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "isRepeatable",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "locations",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "__EnumValue",
				"description": null,
				"fields": [{
					"name": "deprecationReason",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "description",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "isDeprecated",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "__Field",
				"description": null,
				"fields": [{
					"name": "args",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__InputValue",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "deprecationReason",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "description",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "isDeprecated",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "type",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "__InputValue",
				"description": null,
				"fields": [{
					"name": "defaultValue",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "description",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "type",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "__Schema",
				"description": null,
				"fields": [{
					"name": "description",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "directives",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Directive",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "mutationType",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "queryType",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "subscriptionType",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "types",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "__Type",
				"description": null,
				"fields": [{
					"name": "description",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "enumValues",
					"description": null,
					"args": [{
						"name": "includeDeprecated",
						"description": null,
						"type": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						},
						"defaultValue": "false"
					}],
					"type": {
						"kind": "OBJECT",
						"name": "__EnumValue",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "fields",
					"description": null,
					"args": [{
						"name": "includeDeprecated",
						"description": null,
						"type": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						},
						"defaultValue": "false"
					}],
					"type": {
						"kind": "OBJECT",
						"name": "__Field",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "inputFields",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__InputValue",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "interfaces",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "kind",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "ENUM",
							"name": "__TypeKind",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "ofType",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "possibleTypes",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "__Type",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "__TypeKind",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "ENUM",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "INPUT_OBJECT",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "INTERFACE",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "LIST",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "NON_NULL",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "OBJECT",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "SCALAR",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "UNION",
					"description": null,
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article",
				"description": "columns and relationships of \"article\"",
				"fields": [{
					"name": "author",
					"description": "An object relationship",
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "author",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "content",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "title",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_aggregate",
				"description": "aggregated selection of \"article\"",
				"fields": [{
					"name": "aggregate",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_aggregate_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "nodes",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "article",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_aggregate_bool_exp",
				"description": null,
				"fields": null,
				"inputFields": [{
					"name": "count",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_aggregate_bool_exp_count",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_aggregate_bool_exp_count",
				"description": null,
				"fields": null,
				"inputFields": [{
					"name": "arguments",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "ENUM",
								"name": "article_select_column",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "distinct",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Boolean",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "filter",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "predicate",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "INPUT_OBJECT",
							"name": "Int_comparison_exp",
							"ofType": null
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_aggregate_fields",
				"description": "aggregate fields of \"article\"",
				"fields": [{
					"name": "avg",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_avg_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "count",
					"description": null,
					"args": [{
						"name": "columns",
						"description": null,
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "distinct",
						"description": null,
						"type": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "max",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_max_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "min",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_min_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "stddev",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_stddev_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "stddev_pop",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_stddev_pop_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "stddev_samp",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_stddev_samp_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "sum",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_sum_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "var_pop",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_var_pop_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "var_samp",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_var_samp_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "variance",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "article_variance_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_aggregate_order_by",
				"description": "order by aggregate values of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "avg",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_avg_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "count",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "max",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_max_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "min",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_min_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "stddev",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_stddev_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "stddev_pop",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_stddev_pop_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "stddev_samp",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_stddev_samp_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "sum",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_sum_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "var_pop",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_var_pop_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "var_samp",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_var_samp_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "variance",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_variance_order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_arr_rel_insert_input",
				"description": "input type for inserting array relation for remote table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "data",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_insert_input",
									"ofType": null
								}
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "on_conflict",
					"description": "upsert condition",
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_on_conflict",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_avg_fields",
				"description": "aggregate avg on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_avg_order_by",
				"description": "order by avg() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_bool_exp",
				"description": "Boolean expression to filter rows from the table \"article\". All fields are combined with a logical 'AND'.",
				"fields": null,
				"inputFields": [{
					"name": "_and",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "article_bool_exp",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_not",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_or",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "article_bool_exp",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "author",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "Int_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "String_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "Int_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "String_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "article_constraint",
				"description": "unique or primary key constraints on table \"article\"",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "article_pkey",
					"description": "unique or primary key constraint on columns \"id\"",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_inc_input",
				"description": "input type for incrementing numeric columns in table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_insert_input",
				"description": "input type for inserting data into table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_obj_rel_insert_input",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_max_fields",
				"description": "aggregate max on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "content",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "title",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_max_order_by",
				"description": "order by max() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_min_fields",
				"description": "aggregate min on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "content",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "title",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_min_order_by",
				"description": "order by min() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_mutation_response",
				"description": "response of any mutation on the table \"article\"",
				"fields": [{
					"name": "affected_rows",
					"description": "number of rows affected by the mutation",
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "returning",
					"description": "data from the rows affected by the mutation",
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "article",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_on_conflict",
				"description": "on_conflict condition type for table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "constraint",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "ENUM",
							"name": "article_constraint",
							"ofType": null
						}
					},
					"defaultValue": null
				}, {
					"name": "update_columns",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_update_column",
									"ofType": null
								}
							}
						}
					},
					"defaultValue": "[]"
				}, {
					"name": "where",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_order_by",
				"description": "Ordering options when selecting data from \"article\".",
				"fields": null,
				"inputFields": [{
					"name": "author",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_pk_columns_input",
				"description": "primary key columns input for table: article",
				"fields": null,
				"inputFields": [{
					"name": "id",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "article_select_column",
				"description": "select columns of table \"article\"",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "author_id",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "content",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "title",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_set_input",
				"description": "input type for updating data in table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_stddev_fields",
				"description": "aggregate stddev on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_stddev_order_by",
				"description": "order by stddev() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_stddev_pop_fields",
				"description": "aggregate stddev_pop on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_stddev_pop_order_by",
				"description": "order by stddev_pop() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_stddev_samp_fields",
				"description": "aggregate stddev_samp on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_stddev_samp_order_by",
				"description": "order by stddev_samp() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_stream_cursor_input",
				"description": "Streaming cursor of the table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "initial_value",
					"description": "Stream column input with initial value",
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "INPUT_OBJECT",
							"name": "article_stream_cursor_value_input",
							"ofType": null
						}
					},
					"defaultValue": null
				}, {
					"name": "ordering",
					"description": "cursor ordering",
					"type": {
						"kind": "ENUM",
						"name": "cursor_ordering",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_stream_cursor_value_input",
				"description": "Initial value of the column from where the streaming should start",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "content",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "title",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_sum_fields",
				"description": "aggregate sum on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_sum_order_by",
				"description": "order by sum() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "article_update_column",
				"description": "update columns of table \"article\"",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "author_id",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "content",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "title",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_updates",
				"description": null,
				"fields": null,
				"inputFields": [{
					"name": "_inc",
					"description": "increments the numeric columns with given value of the filtered values",
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_inc_input",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_set",
					"description": "sets the columns of the filtered rows to the given values",
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_set_input",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "where",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_var_pop_fields",
				"description": "aggregate var_pop on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_var_pop_order_by",
				"description": "order by var_pop() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_var_samp_fields",
				"description": "aggregate var_samp on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_var_samp_order_by",
				"description": "order by var_samp() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "article_variance_fields",
				"description": "aggregate variance on columns",
				"fields": [{
					"name": "author_id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "article_variance_order_by",
				"description": "order by variance() on columns of table \"article\"",
				"fields": null,
				"inputFields": [{
					"name": "author_id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author",
				"description": "columns and relationships of \"author\"",
				"fields": [{
					"name": "articles",
					"description": "An array relationship",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "article",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "articles_aggregate",
					"description": "An aggregate relationship",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "article_aggregate",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "birthdate",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "date",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "created_at",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "timestamptz",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "String",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_aggregate",
				"description": "aggregated selection of \"author\"",
				"fields": [{
					"name": "aggregate",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_aggregate_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "nodes",
					"description": null,
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "author",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_aggregate_fields",
				"description": "aggregate fields of \"author\"",
				"fields": [{
					"name": "avg",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_avg_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "count",
					"description": null,
					"args": [{
						"name": "columns",
						"description": null,
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "author_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "distinct",
						"description": null,
						"type": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "max",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_max_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "min",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_min_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "stddev",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_stddev_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "stddev_pop",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_stddev_pop_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "stddev_samp",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_stddev_samp_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "sum",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_sum_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "var_pop",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_var_pop_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "var_samp",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_var_samp_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "variance",
					"description": null,
					"args": [],
					"type": {
						"kind": "OBJECT",
						"name": "author_variance_fields",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_avg_fields",
				"description": "aggregate avg on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_bool_exp",
				"description": "Boolean expression to filter rows from the table \"author\". All fields are combined with a logical 'AND'.",
				"fields": null,
				"inputFields": [{
					"name": "_and",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "author_bool_exp",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_not",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_or",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "author_bool_exp",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "articles",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "articles_aggregate",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_aggregate_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "birthdate",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "date_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "created_at",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "timestamptz_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "Int_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "name",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "String_comparison_exp",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "author_constraint",
				"description": "unique or primary key constraints on table \"author\"",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "author_pkey",
					"description": "unique or primary key constraint on columns \"id\"",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_inc_input",
				"description": "input type for incrementing numeric columns in table \"author\"",
				"fields": null,
				"inputFields": [{
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_insert_input",
				"description": "input type for inserting data into table \"author\"",
				"fields": null,
				"inputFields": [{
					"name": "articles",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_arr_rel_insert_input",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "birthdate",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "created_at",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "name",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_max_fields",
				"description": "aggregate max on columns",
				"fields": [{
					"name": "birthdate",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "created_at",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_min_fields",
				"description": "aggregate min on columns",
				"fields": [{
					"name": "birthdate",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "created_at",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_mutation_response",
				"description": "response of any mutation on the table \"author\"",
				"fields": [{
					"name": "affected_rows",
					"description": "number of rows affected by the mutation",
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "returning",
					"description": "data from the rows affected by the mutation",
					"args": [],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "author",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_obj_rel_insert_input",
				"description": "input type for inserting object relation for remote table \"author\"",
				"fields": null,
				"inputFields": [{
					"name": "data",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "INPUT_OBJECT",
							"name": "author_insert_input",
							"ofType": null
						}
					},
					"defaultValue": null
				}, {
					"name": "on_conflict",
					"description": "upsert condition",
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_on_conflict",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_on_conflict",
				"description": "on_conflict condition type for table \"author\"",
				"fields": null,
				"inputFields": [{
					"name": "constraint",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "ENUM",
							"name": "author_constraint",
							"ofType": null
						}
					},
					"defaultValue": null
				}, {
					"name": "update_columns",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "author_update_column",
									"ofType": null
								}
							}
						}
					},
					"defaultValue": "[]"
				}, {
					"name": "where",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_bool_exp",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_order_by",
				"description": "Ordering options when selecting data from \"author\".",
				"fields": null,
				"inputFields": [{
					"name": "articles_aggregate",
					"description": null,
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "article_aggregate_order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "birthdate",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "created_at",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "name",
					"description": null,
					"type": {
						"kind": "ENUM",
						"name": "order_by",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_pk_columns_input",
				"description": "primary key columns input for table: author",
				"fields": null,
				"inputFields": [{
					"name": "id",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "author_select_column",
				"description": "select columns of table \"author\"",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "birthdate",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "created_at",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_set_input",
				"description": "input type for updating data in table \"author\"",
				"fields": null,
				"inputFields": [{
					"name": "birthdate",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "created_at",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "name",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_stddev_fields",
				"description": "aggregate stddev on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_stddev_pop_fields",
				"description": "aggregate stddev_pop on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_stddev_samp_fields",
				"description": "aggregate stddev_samp on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_stream_cursor_input",
				"description": "Streaming cursor of the table \"author\"",
				"fields": null,
				"inputFields": [{
					"name": "initial_value",
					"description": "Stream column input with initial value",
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "INPUT_OBJECT",
							"name": "author_stream_cursor_value_input",
							"ofType": null
						}
					},
					"defaultValue": null
				}, {
					"name": "ordering",
					"description": "cursor ordering",
					"type": {
						"kind": "ENUM",
						"name": "cursor_ordering",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_stream_cursor_value_input",
				"description": "Initial value of the column from where the streaming should start",
				"fields": null,
				"inputFields": [{
					"name": "birthdate",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "created_at",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "id",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "name",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "String",
						"ofType": null
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_sum_fields",
				"description": "aggregate sum on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Int",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "author_update_column",
				"description": "update columns of table \"author\"",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "birthdate",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "created_at",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "id",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "name",
					"description": "column name",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "author_updates",
				"description": null,
				"fields": null,
				"inputFields": [{
					"name": "_inc",
					"description": "increments the numeric columns with given value of the filtered values",
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_inc_input",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_set",
					"description": "sets the columns of the filtered rows to the given values",
					"type": {
						"kind": "INPUT_OBJECT",
						"name": "author_set_input",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "where",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "INPUT_OBJECT",
							"name": "author_bool_exp",
							"ofType": null
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_var_pop_fields",
				"description": "aggregate var_pop on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_var_samp_fields",
				"description": "aggregate var_samp on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "author_variance_fields",
				"description": "aggregate variance on columns",
				"fields": [{
					"name": "id",
					"description": null,
					"args": [],
					"type": {
						"kind": "SCALAR",
						"name": "Float",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "cursor_ordering",
				"description": "ordering argument of a cursor",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "ASC",
					"description": "ascending ordering of the cursor",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "DESC",
					"description": "descending ordering of the cursor",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "SCALAR",
				"name": "date",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "date_comparison_exp",
				"description": "Boolean expression to compare columns of type \"date\". All fields are combined with logical 'AND'.",
				"fields": null,
				"inputFields": [{
					"name": "_eq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_in",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "date",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_is_null",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Boolean",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_neq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "date",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nin",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "date",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "mutation_root",
				"description": "mutation root",
				"fields": [{
					"name": "delete_article",
					"description": "delete data from the table: \"article\"",
					"args": [{
						"name": "where",
						"description": "filter the rows which have to be deleted",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "article_bool_exp",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article_mutation_response",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "delete_article_by_pk",
					"description": "delete single row from the table: \"article\"",
					"args": [{
						"name": "id",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "delete_author",
					"description": "delete data from the table: \"author\"",
					"args": [{
						"name": "where",
						"description": "filter the rows which have to be deleted",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "author_bool_exp",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author_mutation_response",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "delete_author_by_pk",
					"description": "delete single row from the table: \"author\"",
					"args": [{
						"name": "id",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "insert_article",
					"description": "insert data into the table: \"article\"",
					"args": [{
						"name": "objects",
						"description": "the rows to be inserted",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "LIST",
								"name": null,
								"ofType": {
									"kind": "NON_NULL",
									"name": null,
									"ofType": {
										"kind": "INPUT_OBJECT",
										"name": "article_insert_input",
										"ofType": null
									}
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "on_conflict",
						"description": "upsert condition",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_on_conflict",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article_mutation_response",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "insert_article_one",
					"description": "insert a single row into the table: \"article\"",
					"args": [{
						"name": "object",
						"description": "the row to be inserted",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "article_insert_input",
								"ofType": null
							}
						},
						"defaultValue": null
					}, {
						"name": "on_conflict",
						"description": "upsert condition",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_on_conflict",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "insert_author",
					"description": "insert data into the table: \"author\"",
					"args": [{
						"name": "objects",
						"description": "the rows to be inserted",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "LIST",
								"name": null,
								"ofType": {
									"kind": "NON_NULL",
									"name": null,
									"ofType": {
										"kind": "INPUT_OBJECT",
										"name": "author_insert_input",
										"ofType": null
									}
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "on_conflict",
						"description": "upsert condition",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_on_conflict",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author_mutation_response",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "insert_author_one",
					"description": "insert a single row into the table: \"author\"",
					"args": [{
						"name": "object",
						"description": "the row to be inserted",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "author_insert_input",
								"ofType": null
							}
						},
						"defaultValue": null
					}, {
						"name": "on_conflict",
						"description": "upsert condition",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_on_conflict",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "update_article",
					"description": "update data of the table: \"article\"",
					"args": [{
						"name": "_inc",
						"description": "increments the numeric columns with given value of the filtered values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_inc_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "_set",
						"description": "sets the columns of the filtered rows to the given values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_set_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows which have to be updated",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "article_bool_exp",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article_mutation_response",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "update_article_by_pk",
					"description": "update single row of the table: \"article\"",
					"args": [{
						"name": "_inc",
						"description": "increments the numeric columns with given value of the filtered values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_inc_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "_set",
						"description": "sets the columns of the filtered rows to the given values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_set_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "pk_columns",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "article_pk_columns_input",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "update_article_many",
					"description": "update multiples rows of table: \"article\"",
					"args": [{
						"name": "updates",
						"description": "updates to execute, in order",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "LIST",
								"name": null,
								"ofType": {
									"kind": "NON_NULL",
									"name": null,
									"ofType": {
										"kind": "INPUT_OBJECT",
										"name": "article_updates",
										"ofType": null
									}
								}
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "article_mutation_response",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "update_author",
					"description": "update data of the table: \"author\"",
					"args": [{
						"name": "_inc",
						"description": "increments the numeric columns with given value of the filtered values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_inc_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "_set",
						"description": "sets the columns of the filtered rows to the given values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_set_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows which have to be updated",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "author_bool_exp",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author_mutation_response",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "update_author_by_pk",
					"description": "update single row of the table: \"author\"",
					"args": [{
						"name": "_inc",
						"description": "increments the numeric columns with given value of the filtered values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_inc_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "_set",
						"description": "sets the columns of the filtered rows to the given values",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_set_input",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "pk_columns",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "INPUT_OBJECT",
								"name": "author_pk_columns_input",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "update_author_many",
					"description": "update multiples rows of table: \"author\"",
					"args": [{
						"name": "updates",
						"description": "updates to execute, in order",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "LIST",
								"name": null,
								"ofType": {
									"kind": "NON_NULL",
									"name": null,
									"ofType": {
										"kind": "INPUT_OBJECT",
										"name": "author_updates",
										"ofType": null
									}
								}
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "author_mutation_response",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "ENUM",
				"name": "order_by",
				"description": "column ordering options",
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": [{
					"name": "asc",
					"description": "in ascending order, nulls last",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "asc_nulls_first",
					"description": "in ascending order, nulls first",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "asc_nulls_last",
					"description": "in ascending order, nulls last",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "desc",
					"description": "in descending order, nulls first",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "desc_nulls_first",
					"description": "in descending order, nulls first",
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "desc_nulls_last",
					"description": "in descending order, nulls last",
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "query_root",
				"description": null,
				"fields": [{
					"name": "article",
					"description": "fetch data from the table: \"article\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "article",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "article_aggregate",
					"description": "fetch aggregated fields from the table: \"article\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "article_aggregate",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "article_by_pk",
					"description": "fetch data from the table: \"article\" using primary key columns",
					"args": [{
						"name": "id",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author",
					"description": "fetch data from the table: \"author\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "author_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "author_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "author",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author_aggregate",
					"description": "fetch aggregated fields from the table: \"author\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "author_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "author_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "author_aggregate",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author_by_pk",
					"description": "fetch data from the table: \"author\" using primary key columns",
					"args": [{
						"name": "id",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "OBJECT",
				"name": "subscription_root",
				"description": null,
				"fields": [{
					"name": "article",
					"description": "fetch data from the table: \"article\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "article",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "article_aggregate",
					"description": "fetch aggregated fields from the table: \"article\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "article_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "article_aggregate",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "article_by_pk",
					"description": "fetch data from the table: \"article\" using primary key columns",
					"args": [{
						"name": "id",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "article",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "article_stream",
					"description": "fetch data from the table in a streaming manner: \"article\"",
					"args": [{
						"name": "batch_size",
						"description": "maximum number of rows returned in a single batch",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}, {
						"name": "cursor",
						"description": "cursor to stream the results returned by the query",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "LIST",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "article_stream_cursor_input",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "article_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "article",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author",
					"description": "fetch data from the table: \"author\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "author_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "author_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "author",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author_aggregate",
					"description": "fetch aggregated fields from the table: \"author\"",
					"args": [{
						"name": "distinct_on",
						"description": "distinct select on columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "ENUM",
									"name": "author_select_column",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "limit",
						"description": "limit the number of rows returned",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "offset",
						"description": "skip the first n rows. Use only with order_by",
						"type": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						},
						"defaultValue": null
					}, {
						"name": "order_by",
						"description": "sort the rows by one or more columns",
						"type": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "author_order_by",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "OBJECT",
							"name": "author_aggregate",
							"ofType": null
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author_by_pk",
					"description": "fetch data from the table: \"author\" using primary key columns",
					"args": [{
						"name": "id",
						"description": null,
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "OBJECT",
						"name": "author",
						"ofType": null
					},
					"isDeprecated": false,
					"deprecationReason": null
				}, {
					"name": "author_stream",
					"description": "fetch data from the table in a streaming manner: \"author\"",
					"args": [{
						"name": "batch_size",
						"description": "maximum number of rows returned in a single batch",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "Int",
								"ofType": null
							}
						},
						"defaultValue": null
					}, {
						"name": "cursor",
						"description": "cursor to stream the results returned by the query",
						"type": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "LIST",
								"name": null,
								"ofType": {
									"kind": "INPUT_OBJECT",
									"name": "author_stream_cursor_input",
									"ofType": null
								}
							}
						},
						"defaultValue": null
					}, {
						"name": "where",
						"description": "filter the rows returned",
						"type": {
							"kind": "INPUT_OBJECT",
							"name": "author_bool_exp",
							"ofType": null
						},
						"defaultValue": null
					}],
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "LIST",
							"name": null,
							"ofType": {
								"kind": "NON_NULL",
								"name": null,
								"ofType": {
									"kind": "OBJECT",
									"name": "author",
									"ofType": null
								}
							}
						}
					},
					"isDeprecated": false,
					"deprecationReason": null
				}],
				"inputFields": null,
				"interfaces": [],
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "SCALAR",
				"name": "timestamptz",
				"description": null,
				"fields": null,
				"inputFields": null,
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}, {
				"kind": "INPUT_OBJECT",
				"name": "timestamptz_comparison_exp",
				"description": "Boolean expression to compare columns of type \"timestamptz\". All fields are combined with logical 'AND'.",
				"fields": null,
				"inputFields": [{
					"name": "_eq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_gte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_in",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "timestamptz",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}, {
					"name": "_is_null",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "Boolean",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lt",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_lte",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_neq",
					"description": null,
					"type": {
						"kind": "SCALAR",
						"name": "timestamptz",
						"ofType": null
					},
					"defaultValue": null
				}, {
					"name": "_nin",
					"description": null,
					"type": {
						"kind": "LIST",
						"name": null,
						"ofType": {
							"kind": "NON_NULL",
							"name": null,
							"ofType": {
								"kind": "SCALAR",
								"name": "timestamptz",
								"ofType": null
							}
						}
					},
					"defaultValue": null
				}],
				"interfaces": null,
				"enumValues": null,
				"possibleTypes": null
			}],
			"directives": [{
				"name": "include",
				"description": "whether this query should be included",
				"locations": ["FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"],
				"args": [{
					"name": "if",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						}
					},
					"defaultValue": null
				}]
			}, {
				"name": "skip",
				"description": "whether this query should be skipped",
				"locations": ["FIELD", "FRAGMENT_SPREAD", "INLINE_FRAGMENT"],
				"args": [{
					"name": "if",
					"description": null,
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						}
					},
					"defaultValue": null
				}]
			}, {
				"name": "cached",
				"description": "whether this query should be cached (Hasura Cloud only)",
				"locations": ["QUERY"],
				"args": [{
					"name": "ttl",
					"description": "measured in seconds",
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Int",
							"ofType": null
						}
					},
					"defaultValue": "60"
				}, {
					"name": "refresh",
					"description": "refresh the cache entry",
					"type": {
						"kind": "NON_NULL",
						"name": null,
						"ofType": {
							"kind": "SCALAR",
							"name": "Boolean",
							"ofType": null
						}
					},
					"defaultValue": "false"
				}]
			}]
		}
	}
}

const getSchema = (ddl='') => {
	return Promise.resolve(sampleSchemaJson)
}

export type Model = {
	name: string,
	boolExpression: {
		name: string,
		type: GraphQLInputType
	},
	fieldsType: GraphQLType
}

export const getAllModels= (schema: any): Model[] => {
	try {
		// build a client schema from JSON schema
		const clientSchema = buildClientSchema(schema);

		// get the query root node
		const queryRoot= clientSchema.getQueryType()
		if (!queryRoot) {
			return [];
		}

		const rootFields = queryRoot.getFields()

	  let models: Model[] = [];
	  Object.entries(rootFields).forEach(([typename, field]) => {
	  	const fieldType = getNamedType(field.type);
	  	if (isObjectType(fieldType)) {
	  		const typeBoolExp = field.args.find(a => a.name === 'where');
	  		const selectColumnEnum = field.args.find(a => a.name === 'distinct_on');
	  		if (typeBoolExp) {
	  			models.push({
	  				name: field.name,
	  				boolExpression: {
	  					name: getNamedType(typeBoolExp.type).name,
	  					type: getNamedType(typeBoolExp.type)
	  				},
	  				fieldsType: selectColumnEnum?.type ? getNamedType(selectColumnEnum.type) : new GraphQLScalarType({
						  name: 'Test',
						  serialize: (value: unknown) => value,
						  parseValue: (value: unknown) => value,
						  parseLiteral() {
						  	return null;
						  }
						})
	  			})
	  		}
	  	}
	  })
	  return models;

	} catch (e) {
		console.error(e);
		return []
	}
}

export const generateNlsTypesFromModels = (models: Model[], modelName: string) => {
	
	const map = {};

	const model = models.find(m => m.name === modelName);

	if (!model) {
		return {}
	}

	return {
		fields: new GraphQLList(model.fieldsType),
		constraint: model.boolExpression.type
	}
}

export const generateNlsTypesFromSchema = (schema: any, modelName: string) => {
	const models = getAllModels(schema)
	return generateNlsTypesFromModels(models, modelName);
}
