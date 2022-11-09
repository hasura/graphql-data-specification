import * as React from 'react';
import { render } from "react-dom";
import AceEditor from "react-ace";
import { getSchemaFromGDGL } from '../utils/schema'

import "ace-builds/src-noconflict/mode-yaml";
import "ace-builds/src-noconflict/theme-github";
import "ace-builds/src-noconflict/ext-language_tools";

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

export const DGDLEditor: React.VFC<{
	value: string,
	onChange: (value: string) => void
}> = ({onChange, value }) => {
	return (
	  <AceEditor
	    mode="yaml"
	    value={value}
	    width="100%"
	    fontSize="18px"
	    theme="github"
	    onChange={(value) => onChange(value)}
	    name="dgdl-editor"
	    editorProps={{ $blockScrolling: true }}
	  />
	);
}
