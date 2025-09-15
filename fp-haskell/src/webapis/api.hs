{-# LANGUAGE OverloadedStrings #-}
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- run server
main :: IO ()
main = do
    putStrLn "Warp server started at http://localhost:8080/, press Ctrl+C to quit"
    run 8080 app

-- application 
app :: Application
app request respond = respond $ case (requestMethod request, rawPathInfo request) of
        ("GET", "/") -> root

-- get root function
root :: Response
root = responseLBS
    status200
    [(hContentType, "text/plain")]
    "Hello World!"
