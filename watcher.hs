{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (forever, void, when, (<=<))
import System.Directory
import System.Process (
  callCommand, readProcess, createProcess, waitForProcess, proc
                      )
import Control.Concurrent (threadDelay)
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception (try, SomeException, Exception, throwIO)

import Data.List (elemIndex)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Char (isAlphaNum)
import Data.Time.Clock (getCurrentTime)
import System.Timeout (timeout)


data BreakLoopException = BreakLoopException
    deriving (Show)

instance Exception BreakLoopException

time_period = 1000000
time_out = 3000000

data Color = Red | Green | Blue

type MyCommand = String

data Input
    = RunClangTidy
    | Compile
    | SwitchCat
    deriving (Read, Show)

parseInput :: String -> Maybe Input
parseInput ":tidy" = Just RunClangTidy
parseInput ":compile" = Just Compile
parseInput ":c" = Just Compile
parseInput ":cat" = Just SwitchCat
parseInput _ = Nothing

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


compileFile :: Bool -> FilePath -> Maybe MyCommand -> IO ()
compileFile doCat filepath may_command = do
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
            if doCat then
                putBoldGreen "\nFile:" >>
                --void $ callCommand $ "cat " ++ filepath
                (callCommand $ "cat " ++ filepath)
            else pure ()

        Left (err :: SomeException) -> do
            putBoldRed $ "\nCompilation failed with:\n"
              ++ (show err)
            pure ()


watchFile :: Bool -> FilePath -> Maybe MyCommand -> IO ()
watchFile doCat filepath may_command = do
    initTimeRef <- newIORef =<< getModificationTime filepath
    forever $ do
        --putStrLn "new loop"
        threadDelay time_period
        initTime <- readIORef initTimeRef
        modifiedTime <- getModificationTime filepath
        --threadDelay time_period >> putStrLn (filepath ++ ": " ++ show modifiedTime)
        --          >> putStrLn ((mapToSpaces filepath) ++ ": " ++ show initTime)

        when (modifiedTime > initTime) $ do
        --when (modifiedTime >) <=< readIORef $ initTimeRef $ do
            compileFile doCat filepath may_command
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
          -- FIXME
          case input of
            Just input -> do
              let maybeInput = parseInput input

              case maybeInput of 
                  Just Compile -> do
                      compileFile doCat filepath may_command
                      return ()
                  Just SwitchCat -> watchFile (not doCat) (filepath) (may_command)
                  Just RunClangTidy -> case may_command of
                      Just cmd -> do
                          tidyResult <- try (callCommand cmd)
                          case tidyResult of
                              Right _ -> putBoldGreen "TIDY done" >> pure ()
                              Left (err :: SomeException) -> do
                                  putBoldRed $ "\nBTW file won't compile...\n"
                                  putBoldRed $ show err
                                  pure ()

                      Nothing -> pure()
                  _ -> putBoldBlue "Tell this to your mother"
              putStr "λ> " >> pure ()

            Nothing -> do
                initTime <- readIORef initTimeRef
                modifiedTime <- getModificationTime filepath
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
        (doCat, Just filepath, command) -> watchFile doCat filepath command
        _ -> putStrLn "Usage: watchfile ?-doCat? <filepath> ?tidy command?."
