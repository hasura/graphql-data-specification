import Head from 'next/head'
import Image from 'next/image'
import styles from '../styles/Home.module.css'
import { sampleSchemaJson } from '../utils/schema'
import { SchemaExplorer } from '../components/SchemaExplorer'
import { NLSExplorer } from '../components/NLSExplorer'

const title = 'GDS'

export default function Home() {
  return (
    <div className={styles.container}>
      <Head>
        <title>{title}</title>
        <meta name="description" content="Modern Standard for GraphQL APIs" />
        <link rel="icon" href="/hasura-icon-primary.svg" />
      </Head>

      <main className={styles.main}>
        <h1 className={styles.title}>
          Domain Graph Definition Language 
        </h1>

        <p className={styles.description}>
          Modern standard for powerful GraphQL APIs{' '}
        </p>

        <div className={`${styles.grid} mb-4`}>
          <SchemaExplorer schema={sampleSchemaJson.data}/>
        </div>
        <div className={`${styles.grid} mb-4`}>
          <NLSExplorer schema={sampleSchemaJson.data}/>
        </div>
      </main>

      <footer className={styles.footer}>
        <a
          href="https://hasura.io"
          target="_blank"
          rel="noopener noreferrer"
        >
          Powered by{' '}
          <span className={styles.logo}>
            <Image src="/hasura-icon-primary.svg" alt="Vercel Logo" width={16} height={16} />
          </span>
        </a>
      </footer>
    </div>
  )
}
