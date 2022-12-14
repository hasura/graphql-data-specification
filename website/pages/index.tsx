import Head from 'next/head'
import Image from 'next/image'
import dynamic from 'next/dynamic'
import styles from '../styles/Home.module.css'
import { SchemaExplorer } from '../components/SchemaExplorer'
import { NLSExplorer } from '../components/NLSExplorer'
import { useDGDLEditor } from '../hooks/hooks'
import { ClientSideRender } from '../components/ClientSideRender'

const DGDLEditor = dynamic(() => import('../components/DGDLEditor'), {
  ssr: false
})

const title = 'GraphQL Data Specification'

export default function Home() {
  const {
    dgdl,
    onDgdlChange,
    generateGqlSchema,
    generatingGqlSchema,
    schema,
    models,
    error
  } = useDGDLEditor()

  return (
    <div className={styles.container}>
      <Head>
        <title>{`${title} - Playground`} </title>
        <meta name="description" content="A playground for the emerging GraphQL Data Specification" />
        <link rel="icon" href="/hasura-icon-mono-dark.svg" />
      </Head>

      <main className={styles.main}>
        <h1 className={styles.title}>
          {title}
        </h1>

        <div className={styles.description}>
          <p>
          A playground for the emerging GraphQL Data Specification
          </p>
          <p className="text-sm">
            Specification:&nbsp;
            <a
              href="https://github.com/hasura/graphql-data-specification"
              target="_blank"
              rel="noopener noreferrer"
              className="text-blue-500"
            >
              github.com/hasura/graphql-data-specification
            </a>
          </p>
        </div>
        <div className={`${styles.grid} flex-col`}>
        <div className="flex flex-col mb-4 w-full">
          <p className="w-full mb-2">Enter your DGDL Yaml</p>
          <div className="border border-gray-200 rounded mb-2">
            <ClientSideRender>
              <DGDLEditor
                value={dgdl}
                onChange={onDgdlChange}
              />
            </ClientSideRender>
            {
              error && (
                <p className="w-full text-left text-red-700 mt-1">{error}</p>
              )
            }
          </div>
          <div className="w-full flex justify-center">
          <button
            className={`flex items-center justify-center w-auto bg-zinc-500 ${!generatingGqlSchema ? "hover:shadow hover:bg-zinc-600" : ""} text-white text-sm py-2 px-4 rounded-full`}
            onClick={() => generateGqlSchema(dgdl)}
            disabled={generatingGqlSchema}
          >
            {generatingGqlSchema && (
              <div className="w-3 h-3 border-l-2 border-gray-100 rounded-full animate-spin mr-1"></div>
            )}
            Generate GraphQL Schema
          </button> 
          </div>
        </div>
        {
          schema && (
            <>
              <div className={`w-full mb-4`}>
                <NLSExplorer schema={schema} models={models}/>
              </div>
              <div className={`w-full mb-4`}>
                <div className="mb-2">
                  <p>Explore the GraphQL schema</p>
                </div>
                <SchemaExplorer schema={schema}/>
              </div>
            </>
          )
        }
        </div>
      </main>

      <footer className={styles.footer}>
        <a
          className="flex items-center"
          href="https://hasura.io"
          target="_blank"
          rel="noopener noreferrer"
        >
          <p>
            Made by{' '}
          </p>
          <span className={styles.logo}>
            <Image src="/hasura-brand.svg" alt="Hasura Logo" width={80} height={20} />
          </span>
        </a>
      </footer>
    </div>
  )
}
