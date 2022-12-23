import React from 'react';
import { GraphQLSchema } from 'graphql/type'
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import { QueryEditor, VariableEditor, ExplorerSection, DocExplorer } from '@graphiql/react';
import { GraphiQLProvider, GraphiQL } from 'graphiql'
import isoFetch from 'isomorphic-fetch';
import '@graphiql/react/dist/style.css'
import 'graphiql/graphiql.css';

const graphqlURL = 'https://smart-kangaroo-46.hasura.app/v1/graphql'

export const SchemaExplorer: React.VFC<{ schema: GraphQLSchema}> = (props) => {

  const { schema } = props;
  const [allowRender, setAllowRender] = React.useState(false);

  const fetcher = () => Promise.resolve({});

  React.useEffect(() => {
    if (process.browser) {
      setAllowRender(true)
    }
  }, [process.browser])

  return allowRender ? (
    <GraphiQLProvider fetcher={fetcher} dangerouslyAssumeSchemaIsValid query="# Try writing a GraphQL query across the generated GraphQL schema" schema={schema}>
      <div className="graphiql-container h-96 border border border-gray-200 rounded" style={{maxHeight: '100%', height: '400px'}}>
        <QueryEditor/>
      </div>
      <p className="text-gray-400 text-sm">Hit Ctrl+Space for autocompletion</p>
    </GraphiQLProvider>
  ) : null;
}
