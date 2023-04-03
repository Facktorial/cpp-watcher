{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import System.Console.Haskeline hiding (throwIO)
import Control.Monad (foldM, forM, filterM, forever, void, when, (<=<))
import Control.Monad.IO.Class (liftIO)
import System.Directory hiding (findFiles)
import System.Process (
  callCommand, readProcess, createProcess, waitForProcess, proc
                      )
import Control.Concurrent (threadDelay, forkIO, MVar)
import System.Environment
import System.FilePath.Posix (takeFileName)
import System.FilePath ((</>))
import System.IO
import System.IO.Error
import Control.Exception (try, SomeException, Exception, throwIO)

import Data.List (elemIndex)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (isAlphaNum, isSpace)
import Data.Text (replace, pack, unpack, stripPrefix)
import Data.Time.Clock (getCurrentTime)
import System.Timeout (timeout)


data UpdateMode = ADD_FLAGS | REMOVE_FLAGS

data BreakLoopException = BreakLoopException
    deriving (Show)

instance Exception BreakLoopException

time_period = 1000000
time_out = 1000000

data Color = Red | Green | Blue

type MyCommand = String
type Flags = [String]

data Setting = Setting { cat :: Bool, files :: [FilePath], tidy :: Maybe MyCommand, ignores :: Flags }
  deriving (Eq, Show, Read)

data Input
    = RunClangTidy
    | Compile
    | SwitchCat
    | GDB
    | ListFiles
    | ListFlags
    | AddFiles [String]
    | Ignore [String]
    | RemoveFlags [String]
    deriving (Read, Show)

parseInput :: String -> Maybe Input
parseInput ":gdb" = Just GDB
parseInput ":tidy" = Just RunClangTidy
parseInput ":compile" = Just Compile
parseInput ":c" = Just Compile
parseInput ":cat" = Just SwitchCat
parseInput ":ls" = Just ListFiles
parseInput ":reels" = Just ListFlags
parseInput str = case stripPrefix (pack ":add ") (pack str) of
    Just rest -> Just $ AddFiles $ words (unpack rest)
    Nothing -> case stripPrefix (pack ":ig ") (pack str) of
        Just rest -> Just $ Ignore $ words (unpack rest)
        Nothing -> case stripPrefix (pack ":rm_flags ") (pack str) of
            Just rest -> Just $ RemoveFlags $ words (unpack rest)
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

findFiles :: String -> FilePath -> IO [FilePath]
findFiles fileName path = do
  contents <- getDirectoryContents path
  let files = filter (\f -> f /= "." && f /= "..") contents
  paths <- forM files $ \name -> do
    let file = path </> name
    isDirectory <- doesDirectoryExist file
    if isDirectory
      then findFiles fileName file
      else return [file | name == fileName]
  return (concat paths)


updateFlags :: [String] -> UpdateMode -> IO (String)
updateFlags ignores mod = do
    foundlings <- findFiles "flags.make" "."
    putStrLn $ show $ head foundlings

    flags <- try (readProcess "grep" ["CXX_FLAGS", (head foundlings)] "") >>=
        \str -> case str of
            Left (err :: SomeException) -> do
                putBoldRed $ "\nGrep failed with:\n" ++ (show err)
                pure ""
            Right str -> return str

    let command = case mod of
         ADD_FLAGS -> "sed -i \'/^CXX_FLAGS/ s/$/"
             ++ (concat $ map (\x -> " "++ x) ignores) ++ "/\' " ++ (head foundlings)
         REMOVE_FLAGS -> "sed -i \'s/^CXX_FLAGS.*/" ++
             (unwords $ filter (\x -> x `notElem` ignores) $ words flags) ++
             "/\' " ++ (head foundlings)
    putStrLn command

    return command


compileFile :: Setting -> IO ()
compileFile (Setting doCat filepaths may_command ignores) = do
    callCommand "clear"
    --compileResult <- readProcess "make" [] "" >>= putGreen
    --compileResult <- try (readProcess "make" [takeWhile isAlphaNum filepath] "")

    command <- updateFlags ignores ADD_FLAGS

    cmdOut <- try (callCommand command) >>=
        \x -> case x of
            Left (err :: SomeException) -> do
                putBoldRed $ "\nExport failed with:\n" ++ (show err)
                pure ()
            Right x -> pure ()
 

    compileResult <- try (
        readProcess "make" ["V=1"] "")
    case compileResult of
        Right x -> do
            putBoldGreen "Compilation successful:"
            putBoldBlue "Output:"
            runErr <- either (\err -> return $ Left (err :: IOError))
                             (\out -> putGreen "Run OK" >> return (Right out))
                             -- =<< try (callCommand $ "make run-" ++ filepath)
                             =<< try (callCommand $ "./" ++ (getBinaryName x))
                             -- =<< try (callCommand command)
            pure ()
        Left (err :: SomeException) -> do
            putBoldRed $ "\nCompilation failed with:\n"
              ++ (show err)
            pure ()

    if doCat then do
        putBoldGreen "\nFile:"
        --void $ callCommand $ "cat " ++ filepath
        mapM_ (\f -> callCommand $ "cat " ++ f) filepaths
    else pure ()


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


watchFiles :: MVar Setting -> IO ()
--watchFiles (Setting doCat filepaths may_command flag_ignores) = do
watchFiles settingsMVar = do
    initTimeRef <- newIORef =<< maximum <$> mapM getModificationTime filepaths
    forever $ do
        --putStrLn "new loop"
        threadDelay time_period
        initTime <- readIORef initTimeRef
        modifiedTime <- maximum <$> mapM (getModificationTime) filepaths
        --threadDelay time_period >> putStrLn (filepath ++ ": " ++ show modifiedTime)
        --          >> putStrLn ((mapToSpaces filepath) ++ ": " ++ show initTime)

        when (modifiedTime > initTime) $ do
            settings <- takeMVar settingMVar

        --when (modifiedTime >) <=< readIORef $ initTimeRef $ do
            compileFile settings
            writeIORef initTimeRef modifiedTime
            pure ()

watchPromt :: MVar Setting -> IO ()
--watchPromt (Setting doCat filepaths may_command flag_ignores) = do
watchPromt settingMVar = do
    Setting { cat = doCat
            , files = filepaths
            , tidy = may_command
            , ignores = flag_ignores
            } <- takeMVar settingMVar
    putStr "λ> "
    hFlush stdout

    --breakLoop <- newIORef False
    --result <- try (forever $ do
    forever $ do
      --readIORef breakLoop >>= putBoldGreen . show
      --readIORef breakLoop >>= \case
          --True -> break ()
          --True -> throwIO BreakLoopException
          --False -> pure ()

      input <- timeout time_out getLine
      --input <- timeout time_out (completerLoop Nothing)
      --input <- timeout time_out $ runInputT defaultSettings (completer2Loop Nothing "> ")
      -- FIXME
      case input of
        Just input -> do
          let maybeInput = parseInput input

          case maybeInput of 
              Just Compile -> do
                  compileFile $ Setting doCat filepaths may_command flag_ignores
                  return ()
              --Just SwitchCat -> watchPromt $ Setting (not doCat) (filepaths) (may_command) flag_ignores
              --Just SwitchCat -> putMVar settingMVar $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command) flag_ignores
              Just ListFiles -> mapM_ (putStrLn <$> (" " ++)) filepaths
              Just ListFlags -> mapM_ (putStrLn <$> (" " ++)) flag_ignores
              Just (AddFiles newfiles) -> do
                  exFiles <- filterM doesFileExist newfiles
                  case exFiles of
                      [] -> putRed "[]"
                      xs -> putStrLn $ show xs
                  --watchPromt $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command) flag_ignores
                  --putMVar settingMVar $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command) flag_ignores
              Just (Ignore newfiles) -> do
                  case newfiles of
                      [] -> putRed "[]"
                      xs -> putStrLn $ show xs
                  --watchPromt $ Setting (doCat) filepaths (may_command) (concat [flag_ignores, newfiles])
                  --putMVar settingMVar $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command) flag_ignores
              Just (RemoveFlags remove_those) -> do
                  let flags = filter (\x -> x `notElem` remove_those) flag_ignores

                  command <- updateFlags remove_those REMOVE_FLAGS 
                  cmdOut <- try (callCommand command) >>=
                      \x -> case x of
                          Left (err :: SomeException) -> do
                              putBoldRed $ "\nRM using sed failed:\n" ++ (show err)
                              pure ()
                          Right x -> pure ()
                  pure ()
                  --watchPromt $ Setting (doCat) filepaths (may_command) flags
                  --putMVar settingMVar $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command) flag_ignores
              Just GDB -> do
                  compileFile $ Setting doCat filepaths Nothing flag_ignores -- FIXME
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
          putMVar settingMVar $ Setting (doCat) (concat [filepaths,  exFiles]) (may_command) flag_ignores
          putStr "λ> " >> pure ()

        -- Nothing -> do
        --     initTime <- readIORef initTimeRef
        --     modifiedTime <- maximum <$> mapM (getModificationTime) filepaths
        --     when (modifiedTime > initTime) $ writeIORef breakLoop True
        --     pure ()
        --) 
    
    --case result of
        --Left (BreakLoopException) -> pure ()
        --Right _ -> pure ()

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
        (doCat, Just filepath, command) -> do
            mv <- putMVar $ Setting doCat [filepath] command []
            forkIO $ watchFiles mv
            forkIO $ watchPromt mv
        _ -> putStrLn "Usage: watchfile ?-doCat? <filepath> ?tidy command?."
