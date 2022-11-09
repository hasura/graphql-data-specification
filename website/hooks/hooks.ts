import * as React from 'react';
import { getSchemaFromGDGL } from '../utils/schema'

export const useDGDLEditor = () => {

	const [value, setValue] = React.useState('');
	const [gqlSchema, setGqlSchema] = React.useState<any | null>(null);
	const [generatingGqlSchema, setGeneratingGqlSchema] = React.useState(false);
	const [error, setError] = React.useState('');

	const submitDGDL = async (dgdl: string) => {
		setGeneratingGqlSchema(true)
		try {
			const schema = await getSchemaFromGDGL(dgdl);
			setGqlSchema(schema);
			setGeneratingGqlSchema(false);
		} catch (e: any) {
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
		}
	}
}
