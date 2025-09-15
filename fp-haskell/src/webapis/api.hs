{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Text.Read

newtype Message = Message {message :: Text} deriving (Show, Generic)
instance ToJSON Message

newtype Item = Item {itemId :: Int} deriving (Show, Generic)
instance ToJSON Item

-- run server
main :: IO ()
main = do
    putStrLn "Warp server started at http://localhost:8080/, press Ctrl+C to quit"
    run 8080 app

-- application 
app :: Application
app request respond = respond $ case (requestMethod request, pathInfo request) of
    ("GET", []) -> root
    ("GET", ["item", itemId]) -> case parseItemId itemId of
        Left _ -> validationError
        Right parsedItemId -> getItem parsedItemId

parseItemId :: Text -> Either String Int
parseItemId text = readEither $ unpack text

-- get root function
root :: Response
root = responseLBS
    status200
    [(hContentType, "application/json")]
    (encode Message {message = "Hello World!"})

validationError :: Response
validationError = responseLBS
    status422
    [(hContentType, "application/json")]
    (encode Message {message = "Validation error"})

-- get item by ID
getItem :: Int -> Response
getItem item = responseLBS
    status200
    [(hContentType, "application/json")]
    (encode Item {itemId = item})
