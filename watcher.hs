{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import System.Console.Haskeline hiding (throwIO)
import Control.Monad (foldM, filterM, forever, void, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import System.Directory
import System.Process (
  callCommand, readProcess, createProcess, waitForProcess, proc
                      )
import Control.Concurrent (threadDelay)
import System.Environment
import System.FilePath.Posix (takeFileName)
import System.FilePath ((</>))
import System.IO
import System.IO.Error
import Control.Exception (try, SomeException, Exception, throwIO)

import Data.List (elemIndex)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (pack, unpack, stripPrefix)
import Data.Time.Clock (getCurrentTime)
import System.Timeout (timeout)


data BreakLoopException = BreakLoopException
    deriving (Show)

instance Exception BreakLoopException

time_period = 1000000
time_out = 1000000

data Color = Red | Green | Blue

type MyCommand = String

data Setting = Setting { cat :: Bool, files :: [FilePath], tidy :: Maybe MyCommand }
  deriving (Eq, Show, Read)

data Input
    = RunClangTidy
    | Compile
    | SwitchCat
    | GDB
    | ListFiles
    | AddFiles [String]
    deriving (Read, Show)

parseInput :: String -> Maybe Input
parseInput ":gdb" = Just GDB
parseInput ":tidy" = Just RunClangTidy
parseInput ":compile" = Just Compile
parseInput ":c" = Just Compile
parseInput ":cat" = Just SwitchCat
parseInput ":ls" = Just ListFiles
parseInput str = case stripPrefix (pack ":add ") (pack str) of
    Just rest -> Just $ AddFiles $ words (unpack rest)
    Nothing -> Nothing

colorString :: Color -> String
colorString Red = "91"
colorString Green = "32"
colorString Blue = "96"

putStrFormated :: Color -> Bool -> String -> IO ()
putStrFormated color True =
    \x -> putStrLn $ "\x1b[1;" ++ colorString color ++ "m" ++ x ++ "\x1b[0m"
putStrFormated color False =
    \x -> putStrLn $ "\x1b[" ++ colorString color ++ "m" ++ x ++ "\x1b[0m"

putBoldBlue = putStrFormated Blue True
putBoldGreen = putStrFormated Green True
putGreen = putStrFormated Green False
putBoldRed = putStrFormated Red True
putRed = putStrFormated Red False

mapToSpaces :: String -> String
mapToSpaces = replicate <$> length <*> const ' '


getBinaryName :: String -> String
getBinaryName output =
    let wordsInOutput = words output
        targetIndex_build = elemIndex "Built" wordsInOutput
    in case targetIndex_build of
       Just idx -> case wordsInOutput !! (idx + 1) of
           "target" -> wordsInOutput !! (idx + 2)
           _ -> "NO_BINARY_FOUND_I_GUESS"
       _ -> "NO_BINARY_FOUND"


compileFile :: Setting -> IO ()
compileFile (Setting doCat filepaths may_command) = do
    callCommand "clear"
    --compileResult <- readProcess "make" [] "" >>= putGreen
    --compileResult <- try (readProcess "make" [takeWhile isAlphaNum filepath] "")
    compileResult <- try (readProcess "make" [] "")
    case compileResult of
        Right x -> do
            putBoldGreen "Compilation successful:"
            putBoldBlue "Output:"
            runErr <- either (\err -> return $ Left (err :: IOError))
                             (\out -> putGreen "Run OK" >> return (Right out))
                             -- =<< try (callCommand $ "make run-" ++ filepath)
                             =<< try (callCommand $ "./" ++ (getBinaryName x))
                             -- =<< try (callCommand command)
            if doCat then do
                putBoldGreen "\nFile:"
                --void $ callCommand $ "cat " ++ filepath
                mapM_ (\f -> callCommand $ "cat " ++ f) filepaths
            else pure ()

        Left (err :: SomeException) -> do
            putBoldRed $ "\nCompilation failed with:\n"
              ++ (show err)
            pure ()


startWith :: String -> [String] -> [String]
startWith prefix = filter (\s -> take (length prefix) s == prefix)


getFilesInPath :: FilePath -> IO [FilePath]
getFilesInPath path = do
    files <- getDirectoryContents path
    return $ filter (\f -> f /= "." && f /= "..") files


completerFilename :: FilePath -> String -> IO [Completion]
completerFilename path stub = do
    files <- getFilesInPath path
    return $ map (simpleCompletion . takeFileName) $ startWith stub files


filenameCompleter :: CompletionFunc IO
filenameCompleter = completeWord Nothing " \t" $ \stub -> do
    let path = takeWhile (/= ' ') stub
    isDir <- doesDirectoryExist path
    if isDir
        then completerFilename path (drop (length path) stub)
        else return []


completer2Loop :: Maybe FilePath -> String -> InputT IO (String)
completer2Loop mpath promt = do
    --minput <- getInputLine ""
    --case minput of
       --Nothing -> return ""
       --Just input -> do
    input <- liftIO getLine
    let args = words input
    let input' = reverse $ dropWhile isSpace $ reverse input
    if null input'
       then completer2Loop mpath promt
       else do
           case mpath of
             Just file -> do
                 --completions <- liftIO (completerFilename file (reverse input))
                 completions <- liftIO (completerFilename file input)
                 let completionTexts = map (simpleCompletion . replacement) completions
                 outputStrLn $ unlines $ show <$> completionTexts
             Nothing -> return ()
           return input'


completerLoop :: Maybe FilePath -> IO (String)
completerLoop mpath = runInputT defaultSettings (loop' path)
    where
      path = maybe (".") id mpath
      loop' xpath = do
        liftIO $ putStr "\b"
        --minput <- getInputLine ""
        liftIO $ hFlush stdout
        minput <- fmap (fmap (filter (/= '\n'))) $ getInputLine ""
        case minput of
          Nothing -> return ""
          Just input -> do
            let args = words input
            case args of
              [cmd] | cmd == "quit" -> return ""
              _ -> do
                completions <- liftIO (completerFilename xpath (reverse input))
                let completionTexts = map (simpleCompletion . replacement) completions
                outputStr $ unlines $ show <$> completionTexts
                loop' xpath


-- completerLoop :: Maybe FilePath -> IO (String)
-- completerLoop mpath = runInputT defaultSettings (loop' path)
--     where
--       path = maybe (".") id mpath
--       loop' xpath = do
--         minput <- getInputLine "> "
--         case minput of
--           Nothing -> return ""
--           Just input -> do
--             let args = words input
--             case args of
--               [cmd] | cmd == "quit" -> return ""
--               _ -> do
--                 completions <- liftIO (completerFilename xpath (reverse input))
--                 let completionTexts = map (simpleCompletion . replacement) completions
--                 outputStrLn $ unlines $ show <$> completionTexts
--                 loop' xpath


watchFiles :: Setting -> IO ()
watchFiles (Setting doCat filepaths may_command) = do
    initTimeRef <- newIORef =<< maximum <$> mapM getModificationTime filepaths
    forever $ do
        --putStrLn "new loop"
        threadDelay time_period
        initTime <- readIORef initTimeRef
        modifiedTime <- maximum <$> mapM (getModificationTime) filepaths
        --threadDelay time_period >> putStrLn (filepath ++ ": " ++ show modifiedTime)
        --          >> putStrLn ((mapToSpaces filepath) ++ ": " ++ show initTime)

        when (modifiedTime > initTime) $ do
        --when (modifiedTime >) <=< readIORef $ initTimeRef $ do
            compileFile $ Setting doCat filepaths may_command
            writeIORef initTimeRef modifiedTime
            pure ()

        putStr "λ> "
        hFlush stdout

        breakLoop <- newIORef False
        result <- try (forever $ do
          --readIORef breakLoop >>= putBoldGreen . show
          readIORef breakLoop >>= \case
              --True -> break ()
              True -> throwIO BreakLoopException
              False -> pure ()

          input <- timeout time_out getLine
          --input <- timeout time_out (completerLoop Nothing)
          --input <- timeout time_out $ runInputT defaultSettings (completer2Loop Nothing "> ")
          -- FIXME
          case input of
            Just input -> do
              let maybeInput = parseInput input

              case maybeInput of 
                  Just Compile -> do
                      compileFile $ Setting doCat filepaths may_command
                      return ()
                  Just SwitchCat -> watchFiles $ Setting (not doCat) (filepaths) (may_command)
                  Just ListFiles -> mapM_ (putStrLn <$> (" " ++)) filepaths
                  Just (AddFiles newfiles) -> do
                      exFiles <- filterM doesFileExist newfiles
                      case exFiles of
                          [] -> putRed "[]"
                          xs -> putStrLn $ show xs
                      watchFiles $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command)
                  Just GDB -> do
                      compileFile $ Setting doCat filepaths Nothing -- FIXME
                      putBoldBlue "Gimmie path to binary"
                      binary <- getLine
                      putBoldBlue "Gimmie seq of instructions"
                      -- TODO
                      -- Gimmie seq of instructions
                      -- :b x.cpp; :threadinfo; run;
                      -- [":b","x.cpp;",":threadinfo;","run;"]
                      instructions <- getLine >>= putStrLn . show . words
                      gdbResult <- try (callCommand $ "gdb " ++ binary) -- FIXME
                      case gdbResult of
                          Left (err :: SomeException) -> do
                              putBoldRed $ "\nBTW gdb failed...\n"
                              putBoldRed $ show err
                              pure ()
                          _ -> pure ()

                      return ()
                  Just RunClangTidy -> case may_command of
                      Just cmd -> do
                          tidyResult <- try (callCommand cmd)
                          case tidyResult of
                              Right _ -> putBoldGreen "TIDY done" >> pure ()
                              Left (err :: SomeException) -> do
                                  putBoldRed $ "\nBTW files won't compile...\n"
                                  putBoldRed $ show err
                                  pure ()

                      Nothing -> pure()
                  _ -> putBoldBlue "Tell this to your mother"
              putStr "λ> " >> pure ()

            Nothing -> do
                initTime <- readIORef initTimeRef
                modifiedTime <- maximum <$> mapM (getModificationTime) filepaths
                when (modifiedTime > initTime) $ writeIORef breakLoop True
                pure ()
            ) 
        
        case result of
            Left (BreakLoopException) -> pure ()
            Right _ -> pure ()

        pure ()


getCommand :: [String] -> Maybe MyCommand
getCommand [] = Nothing
getCommand (x:xs) = Just x


parseArgs :: [String] -> (Bool, Maybe FilePath, Maybe MyCommand)
parseArgs [] = (False, Nothing, Nothing)
parseArgs [filepath] = (False, Just filepath, Nothing)
parseArgs [may_cat, may_filepath]
    | (may_cat == "-doCat") || (may_cat == "-noCat") = (
            may_cat == "-doCat", Just may_filepath, Nothing
         )
    | otherwise = (False, Just may_cat, Just may_filepath)
parseArgs (x:filename:xs) = (x == "-docat", Just filename, getCommand xs)


main :: IO ()
main = do
    args <- getArgs

    case parseArgs args of
        (doCat, Just filepath, command) -> watchFiles $ Setting doCat [filepath] command
        _ -> putStrLn "Usage: watchfile ?-doCat? <filepath> ?tidy command?."
