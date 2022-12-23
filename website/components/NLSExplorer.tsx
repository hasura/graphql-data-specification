import React from 'react';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import { GraphQLSchema } from 'graphql/type'
import { VariableEditor, QueryEditor, useVariableEditor, useEditorContext } from '@graphiql/react';
import { getOperationFacts } from 'graphql-language-service'
import { GraphiQLProvider } from 'graphiql'
import { parse } from 'graphql'
import { getModels, Model, generateNlsTypesFromModels} from '../utils/schema'
import isoFetch from 'isomorphic-fetch';
import '@graphiql/react/dist/style.css'
import 'graphiql/graphiql.css';

const defaultNlsRule = `
{
  "fields": [],
  "constraint": {}
}
`

export const NLSExplorer: React.VFC<{ schema: GraphQLSchema, models: Model[]}> = (props) => {

  const { schema, models } = props;

  const [allowRender, setAllowRender] = React.useState(false);
  const [variableTypes, setVariableTypes] = React.useState<any>(undefined);
  const [variables, setVariables] = React.useState(defaultNlsRule);
  const onVariablesChange = (v: string) => {
  	setVariables(v);
  }

  const [role, setRole] = React.useState('');

	const [model, setModel] = React.useState<Model | null>(null);

  const fetcher = () => Promise.resolve(schema);

  React.useEffect(() => {
    if (process.browser) {
      setAllowRender(true)
    }
  }, [process.browser])

  React.useEffect(() => {
    if (process.browser && model) {
      setVariableTypes(generateNlsTypesFromModels(models, model?.name || ''))
    }
  }, [model])


  return process.browser ? (
    <div className="w-full">
      <div className="flex justify-between items-center mb-2 w-full">
        <p>Define NLS rules</p>
        <div>
        	<select
        		value={model?.name}
        		onChange={e => {
        			setModel(models.find(m => m.name === e.target.value) || null)
        		}}
        	>
        		<option value="" selected={model === null}>--select a model--</option>
        		{
        			models.map(m => (
        				<option selected={m.name === model?.name} value={m.name} key={m.name}>{m.name}</option>
        			))
        		}
        	</select>	
        </div>
        <div>
        	<input
        		type="text"
        		className="h-8 border border-gray-200 p-1"
        		placeholder="Role"
        		value={role}
        		onChange={(e) => setRole(e.target.value)}
        	/>
        </div>
      </div>
      <GraphiQLProvider fetcher={fetcher} dangerouslyAssumeSchemaIsValid schema={schema} variables={variables}>
        <div className="graphiql-container h-96 border border border-gray-200 rounded" style={{maxHeight: '100%', height: '400px'}}>
        	<NLSEditor variableTypes={variableTypes} schema={schema} modelName={model?.name || ''} onVariablesChange={onVariablesChange}/>
        </div>
      </GraphiQLProvider>
      <p className="text-gray-400 text-sm">Hit Ctrl+Space for autocompletion</p>
    </div>
  ) : null;
}

const NLSEditor: React.VFC<{variableTypes: any, schema: GraphQLSchema, modelName: string, onVariablesChange: (v: string) => void}> = ({ variableTypes, schema, modelName, onVariablesChange }) => {
  const { variableEditor } = useEditorContext({ nonNull: true, caller: NLSEditor})

  React.useEffect(() => {
  	if (variableEditor) {
  		onVariablesChange(defaultNlsRule)
  	}
  }, [modelName])

  React.useEffect(() => {
  	if (variableTypes) {
  		if (variableEditor) {
        variableEditor.state.lint.linterOptions.variableToType = variableTypes
        variableEditor.options.lint.variableToType = variableTypes
        variableEditor.options.hintOptions.variableToType = variableTypes
      }
  	}
  }, [variableTypes, variableEditor])

  return <VariableEditor onEdit={onVariablesChange} />;
}