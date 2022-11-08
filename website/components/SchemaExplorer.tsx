import React from 'react';
import { createGraphiQLFetcher } from '@graphiql/toolkit';
import { QueryEditor, VariableEditor, ExplorerSection, DocExplorer } from '@graphiql/react';
import { GraphiQLProvider, GraphiQL } from 'graphiql'
import isoFetch from 'isomorphic-fetch';
import '@graphiql/react/dist/style.css'
import 'graphiql/graphiql.css';

const graphqlURL = 'https://smart-kangaroo-46.hasura.app/v1/graphql'

export const SchemaExplorer: React.VFC<{ schema: any}> = (props) => {

  const [allowRender, setAllowRender] = React.useState(false);


  const fetcher = () => Promise.resolve({
    data: props.schema
  });

  React.useEffect(() => {
    if (process.browser) {
      setAllowRender(true)
    }
  }, [process.browser])

  return allowRender ? (
    <div className="w-full">
      <div className="mb-2">
        <p>Explore the GraphQL schema</p>
      </div>
      <GraphiQLProvider fetcher={fetcher} query="# Try writing a GraphQL query across the generated GraphQL schema">
        <div className="graphiql-container h-96 border border border-gray-200 rounded" style={{maxHeight: '100%', height: '400px'}}>
          <QueryEditor/>
        </div>
      </GraphiQLProvider>
      </div>
  ) : null;
}
