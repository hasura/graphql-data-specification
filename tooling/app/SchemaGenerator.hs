module Main where

import DDL qualified
import Data.Aeson qualified as Json
import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.Builder qualified as Text
import GHC.Generics (Generic)
import Language.GraphQL.Draft.Printer qualified as GraphQL.Printer
import Language.GraphQL.Draft.Syntax qualified as GraphQL
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Schema qualified
import Web.Twain qualified as Twain
import System.IO qualified as IO

data Schema = Schema
  { booleanExpressionNames :: Map.HashMap DDL.ModelName GraphQL.Name,
    sdl :: Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON Schema where
  toEncoding = Json.genericToEncoding Json.defaultOptions

instance Json.FromJSON Schema

getSchema :: Twain.ResponderM a
getSchema = do
  document <- Twain.fromBody @DDL.Document
  let result = Schema.generateSchema document
      booleanExpressionNames = Schema.getBooleanExpressionNames document
  case result of
    Left errors ->
      Twain.send $
        Twain.status Twain.status400 $
          Twain.json errors
    Right schema -> do
      let sdl = Text.toStrict $ Text.toLazyText $ GraphQL.Printer.schemaDocument schema
      Twain.send $ Twain.json $ Schema booleanExpressionNames sdl

main :: IO ()
main = do
  putStrLn "Starting web server at port 8080"
  IO.hFlush IO.stdout
  run 8080 $
    logStdout $
      foldr
        ($)
        (Twain.notFound missing)
        [ Twain.post "schema" getSchema
        ]

missing :: Twain.ResponderM a
missing = Twain.send $ Twain.html "Not found..."
