module Main where

import DDL qualified
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Builder qualified as Text
import Language.GraphQL.Draft.Printer qualified as GraphQL.Printer
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Schema
import Web.Twain qualified as Twain

main :: IO ()
main = do
  run 8080 $
    logStdout $
      foldr
        ($)
        (Twain.notFound missing)
        [ Twain.post "schema" getSchema
        ]

getSchema :: Twain.ResponderM a
getSchema = do
  document <- Twain.fromBody @DDL.Document
  let result = Schema.generateSchema document
  case result of
    Left errors ->
      Twain.send $
        Twain.status Twain.status400 $
          Twain.json errors
    Right schema ->
      Twain.send $
        Twain.text $
          Text.toStrict $
            Text.toLazyText $
              GraphQL.Printer.schemaDocument schema

missing :: Twain.ResponderM a
missing = Twain.send $ Twain.html "Not found..."
