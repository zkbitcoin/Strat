{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-unused-binds #-}

module StratWeb.YesodMain (webInit) where

import Checkers
import CheckersJson
import Data.Aeson
import Data.IORef
import Data.Text (Text)
import Data.Tree
import qualified StratWeb.WebRunner as WR
import System.Random
import Yesod hiding (insert)
import Yesod.Static
import qualified Data.Map.Strict as M
import Network.Wai.Middleware.Cors
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)

corsPolicy :: CorsResourcePolicy
--corsPolicy = simpleCorsResourcePolicy
---    { corsOrigins = Just (["http://localhost:8080"], True) -- Allow requests from localhost:8080 only
---    , corsMethods = ["GET", "POST", "OPTIONS", "PUT", "DELETE"] -- Allowed HTTP methods
--    , corsRequestHeaders = ["Content-Type", "Authorization", "X-Requested-With"] -- Allowed request headers
--    }

corsPolicy = simpleCorsResourcePolicy
     { corsOrigins = Just (["http://localhost:3000", "http://localhost:4000"], True) -- Allow requests from localhost:3000 and localhost:8080
            , corsMethods = ["GET", "POST", "OPTIONS", "PUT", "DELETE"]  -- Specify allowed methods
            , corsRequestHeaders = ["Content-Type", "Accept", "If-None-Match"]  -- Include If-None-Match
            }

staticFilesList "src/StratWeb/Static" ["gameboard.html", "bundle.js", "checker_1_king_48.png",
    "checker_1_plain_48.png", "checker_2_king_48.png", "checker_2_plain_48.png",
    "black_image_48.png", "no_image_48.png", "favicon.ico"]

data GameApp = GameApp { getStatic :: Static,
                         getCounter :: IORef Integer,
                         getMap :: IORef (M.Map Text (Tree CkNode, StdGen)) }

emptyMap :: M.Map Text (Tree CkNode, StdGen)
emptyMap = M.empty

mkYesod "GameApp" [parseRoutes|
/ RootR GET
/options OptionsR OPTIONS
/new NewGameR GET
/computerMove ComputerMoveR GET
/playerMove PlayerMoveR POST
/count CounterR GET
/static StaticR Static getStatic
|]

instance Yesod GameApp

-- OPTIONS Handler
optionsOptionsR :: Handler Value
optionsOptionsR = do
    --addHeader "Access-Control-Allow-Origin" "http://localhost:8080"
    --addHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
    --addHeader "Access-Control-Allow-Headers" "Content-Type"
    returnJson ("CORS headers for OPTIONS" :: Text)

getCounterR :: Handler Html
getCounterR = do
    yesod <- getYesod
    cnt <- liftIO $ incCount $ getCounter yesod
    liftIO $ putStrLn $ "Sending Response " ++ show cnt
    defaultLayout [whamlet|Hello World #{cnt}|]

incCount :: Num a => IORef a -> IO a
incCount counter = atomicModifyIORef counter (\c -> (c + 1, c))

updateMap :: IORef (M.Map Text (Tree CkNode, StdGen)) -> Text -> (Tree CkNode, StdGen) -> IO ()
updateMap mapRef key value = do
    theMap <- readIORef mapRef
    writeIORef mapRef (M.insert key value theMap)

uniqueIdKey :: Text
uniqueIdKey = "uniqueId"

getRootR :: Handler Html
getRootR = do
    liftIO $ putStrLn "Incoming 'home' request"
    _ <- getGameSession
    -- Set CORS headers for the response
    --addHeader "Access-Control-Allow-Origin" "http://localhost:8080"
    --addHeader "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
    --addHeader "Access-Control-Allow-Headers" "Content-Type"
    -- Redirect to the static file
    redirect (StaticR gameboard_html)

getGameSession :: HandlerFor GameApp Text
getGameSession = do
    idMay <- lookupSession uniqueIdKey
    case idMay of
        Nothing -> do
            liftIO $ putStrLn "Adding new unique id to session: "
            newId <- addUniqueId
            liftIO $ putStrLn $ "uniqueId: " ++ show newId
            return newId
        Just theId -> do
            liftIO $ putStrLn $ "Retrieved id from session: " ++ show theId
            return theId

addUniqueId :: Handler Text
addUniqueId = do
    newUnique <- newIdent
    setSession uniqueIdKey newUnique
    return newUnique

getNewGameR :: Handler Value
getNewGameR = do
    liftIO $ putStrLn "Incoming new game request"
    uniqueId <- getGameSession
    (wrapper, gen) <- liftIO $ WR.processStartGame (fst (getStartNode "new_game")) False
    let node = WR.getNode wrapper
    let jAble = WR.getJsonable wrapper
    yesod <- getYesod
    liftIO $ updateMap (getMap yesod) uniqueId (node, gen)
    case jAble of
        WR.Jsonable j -> returnJson j

getComputerMoveR :: Handler Value
getComputerMoveR = do
    liftIO $ putStrLn "Incoming computer move request"
    uniqueId <- getGameSession
    yesod <- getYesod
    theMap <- liftIO $ readIORef $ getMap yesod
    (wrapper, gen) <- liftIO $ case M.lookup uniqueId theMap of
        Nothing -> do
            liftIO $ putStrLn "getComputerMoveR - no tree found in the map"
            liftIO $ WR.processStartGame (fst (getStartNode "new_game")) False
        Just (t, g) -> do
            wrp <- WR.processComputerMove t g
            return (wrp, g)
    liftIO $ updateMap (getMap yesod) uniqueId (WR.getNode wrapper, gen)
    case WR.getJsonable wrapper of
        WR.Jsonable j -> returnJson j

postPlayerMoveR :: Handler Value
postPlayerMoveR = do
    liftIO $ putStrLn "Incoming player move"
    (resultM :: Result JsonMove) <- parseInsecureJsonBody
    yesod <- getYesod
    theMap <- liftIO $ readIORef $ getMap yesod
    uniqueId <- getGameSession
    (wrapper, gen) <- liftIO $ case M.lookup uniqueId theMap of
        Nothing -> do
            liftIO $ putStrLn "getPlayerMoveR - no tree found in the map"
            liftIO $ WR.processStartGame (fst (getStartNode "new_game")) False
        Just (tree, g) ->
            case resultM of
                Error e -> do
                    liftIO $ putStrLn "getPlayerMoveR - could not parse the json from the client"
                    liftIO $ putStrLn ("Error returned: " ++ e)
                    liftIO $ WR.processStartGame (fst (getStartNode "new_game")) False
                Success jMove -> do
                    liftIO $ putStrLn $ "Player move: " ++ show jMove
                    case jsonMoveToCkMove jMove of
                        Nothing -> do
                            liftIO $ putStrLn "getPlayerMoveR - could not convert to CkMove"
                            liftIO $ WR.processStartGame (fst (getStartNode "new_game")) False
                        Just move -> do
                            wrp <- WR.processPlayerMove tree move True g
                            return (wrp, g)
    liftIO $ updateMap (getMap yesod) uniqueId (WR.getNode wrapper, gen)
    case WR.getJsonable wrapper of
      WR.Jsonable j -> do
            liftIO $ putStrLn $ " ** Computer's move: " ++ show (rootLabel (WR.getNode wrapper))
            returnJson j

webInit :: IO ()
webInit = do
    counter <- newIORef 0
    newMap <- newIORef emptyMap
    s <- staticDevel "src/StratWeb/Static"

    putStrLn "\n--------------------------------------------------------"
    putStrLn "To play, point your web browser to http://localhost:3000"
    putStrLn "--------------------------------------------------------\n"

    -- Create the application
    let app = GameApp { getStatic = s, getCounter = counter, getMap = newMap }

    -- Convert to Wai Application
    waiApp <- toWaiApp app

    -- Define CORS middleware
    let corsMiddleware :: Middleware
        corsMiddleware = cors (const $ Just corsPolicy)

    -- Start the server
    run 3000 $ corsMiddleware waiApp
