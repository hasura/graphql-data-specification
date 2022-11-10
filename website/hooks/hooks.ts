import * as React from 'react';
import { generateGraphQLSchemaFromModels, Model, getModels, getGraphQLSchema, sampleDGDL } from '../utils/schema'

export const useDGDLEditor = (initDgDl=sampleDGDL) => {

	const [value, setValue] = React.useState(initDgDl);
	const [gqlSchema, setGqlSchema] = React.useState<any | null>(null);
	const [models, setModels] = React.useState<Model[]>([])
	const [generatingGqlSchema, setGeneratingGqlSchema] = React.useState(false);
	const [error, setError] = React.useState('');

	const submitDGDL = async (dgdl: string) => {
		setGeneratingGqlSchema(true)
		try {
			const schemaResponse = await generateGraphQLSchemaFromModels(dgdl);
			const schema = await getGraphQLSchema(schemaResponse);
			const models = await getModels(schemaResponse.booleanExpressionNames, schema);
			console.log(models);
			console.log(schema)
			setGqlSchema(schema);
			setModels(models)
			setGeneratingGqlSchema(false);
		} catch (e: any) {
			console.log(e)
			setError(e.message);
			setGeneratingGqlSchema(false)
		}
	}

	return {
		schema: gqlSchema,
		generatingGqlSchema,
		generateGqlSchema: submitDGDL,
		dgdl: value,
		onDgdlChange: (v: string) => {
			setGqlSchema(null)
			setValue(v)
		},
		models
	}
}
