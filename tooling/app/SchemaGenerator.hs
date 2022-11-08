module Main where

import DDL qualified
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Builder qualified as Text
import Language.GraphQL.Draft.Printer qualified as GraphQL.Printer
import Network.Wai.Handler.Warp (run)
import Schema
import Web.Twain qualified as Twain

main :: IO ()
main = do
  run 8080 $
    foldr
      ($)
      (Twain.notFound missing)
      [ Twain.post "schema" getSchema
      ]

getSchema :: Twain.ResponderM a
getSchema = do
  document <- Twain.fromBody @DDL.Document
  Twain.send $
    Twain.text $
      Text.toStrict $
        Text.toLazyText $
          GraphQL.Printer.schemaDocument $
            Schema.generate document

missing :: Twain.ResponderM a
missing = Twain.send $ Twain.html "Not found..."
