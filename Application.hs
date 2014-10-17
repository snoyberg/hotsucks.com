{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers hiding (getRobotsR)
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), IPAddrSource (..), destination
    )
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import Network.HTTP.Client.Conduit (newManager)
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Logger (clockDateCacher)
import Data.Default (def)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import GithubRepo
import Filesystem (listDirectory)
import Filesystem.Path (splitExtensions)
import Text.Markdown (markdown)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
    s <- staticSite

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    cr <- githubRepo "https://github.com/snoyberg/hotsucks-content" "master" loadContent

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App
            { settings = conf
            , getStatic = s
            , httpManager = manager
            , appLogger = logger
            , contentRepo = cr
            }

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

loadContent :: FilePath -> IO (Map Text (Text, Html), Html)
loadContent dir = do
    m <- runResourceT
         $ sourceDirectoryDeep False dir
        $$ concatMapC toPair
        =$ foldMapMC reader
    home <- markdown def <$> readFile (dir </> "HOME.md")
    return (m, home)
  where
    toPair x =
        case splitExtensions $ filename x of
            (name, ["md"]) | name `notMember` reserved -> Just (fpToText name, x)
            _ -> Nothing

    reserved = asSet $ setFromList
        [ "README"
        , "HOME"
        ]

    reader (name, fp) =
        sourceFile fp $$ takeCE 1000000 =$ do
            title <- lineC foldC
            content <- sinkLazy
            return $ singletonMap name (title, markdown def content)