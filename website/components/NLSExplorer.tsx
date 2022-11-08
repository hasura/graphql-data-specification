import React from 'react';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import { VariableEditor, QueryEditor, useVariableEditor, useEditorContext } from '@graphiql/react';
import { getOperationFacts } from 'graphql-language-service'
import { GraphiQLProvider } from 'graphiql'
import { parse } from 'graphql'
import { getAllModels, sampleSchemaJson, generateNlsTypesFromSchema, Model} from '../utils/schema'
import isoFetch from 'isomorphic-fetch';
import '@graphiql/react/dist/style.css'
import 'graphiql/graphiql.css';

export const NLSExplorer: React.VFC<{ schema: any}> = (props) => {

  const [allowRender, setAllowRender] = React.useState(false);
  const [variableTypes, setVariableTypes] = React.useState<any>(undefined);
  const [models, setModels] = React.useState<Model[]>([])
  const [variables, setVariables] = React.useState('');
  const onVariablesChange = (v: string) => {
  	setVariables(v);
  }

	const [model, setModel] = React.useState<Model | null>(null);

  const fetcher = () => Promise.resolve(props.schema);

  React.useEffect(() => {
    if (process.browser) {
      setAllowRender(true)
      setModels(getAllModels(props.schema))
    }
  }, [process.browser])

  React.useEffect(() => {
    if (process.browser && model) {
      setVariableTypes(generateNlsTypesFromSchema(props.schema, model?.name || ''))
    }
  }, [model])


  return allowRender ? (
    <div className="w-full">
      <div className="flex justify-between mb-2 w-full">
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
      </div>
      <GraphiQLProvider fetcher={fetcher} dangerouslyAssumeSchemaIsValid schema={props.schema} variables={variables}>
        <div className="graphiql-container h-96" style={{maxHeight: '100%', height: '400px'}}>
        	<NLSEditor variableTypes={variableTypes} schema={props.schema} modelName={model?.name || ''} onVariablesChange={onVariablesChange}/>
        </div>
      </GraphiQLProvider>
    </div>
  ) : null;
}

const NLSEditor: React.VFC<{variableTypes: any, schema: any, modelName: string, onVariablesChange: (string) => void}> = ({ variableTypes, schema, modelName, onVariablesChange }) => {
  const { variableEditor } = useEditorContext({ nonNull: true, caller: NLSEditor})

  React.useEffect(() => {
  	if (variableEditor) {
  		onVariablesChange("")
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