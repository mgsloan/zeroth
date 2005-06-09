module Setup where

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Directory
import Data.Maybe

data ConfigFlags
    = ConfigFlags
    { configGHCPath    :: FilePath
    , configInputFile  :: FilePath
    , configOutputFile :: FilePath
    , configCpphsPath  :: FilePath
    , configGHCArgs    :: [String]
    , configCpphsArgs  :: [String]
    } deriving Show

getExecutable :: String -> Maybe FilePath -> IO FilePath
getExecutable _ (Just path) = return path
getExecutable name Nothing  = fmap (fromMaybe (error errMsg)) (findExecutable name)
    where errMsg = "Couldn't find: "++name
                              
                                         

mkConfigFlags :: TempFlags -> IO ConfigFlags
mkConfigFlags tmpFlags
    = do ghcPath   <- getExecutable "ghc" (tempGHCPath tmpFlags)
         cpphsPath <- getExecutable "cpphs" (tempCpphsPath tmpFlags)
         return (ConfigFlags
                 { configGHCPath    = ghcPath
                 , configCpphsPath  = cpphsPath
                 , configInputFile  = tempInputFile tmpFlags
                 , configOutputFile = tempOutputFile tmpFlags
                 , configGHCArgs    = tempGHCArgs tmpFlags
                 , configCpphsArgs  = tempCpphsArgs tmpFlags})

data TempFlags
    = TempFlags
    { tempGHCPath    :: Maybe FilePath
    , tempInputFile  :: FilePath
    , tempOutputFile :: FilePath
    , tempCpphsPath  :: Maybe FilePath
    , tempGHCArgs    :: [String]
    , tempCpphsArgs  :: [String]
    } deriving Show

data Flag
    = WithGHC FilePath
    | InputFile FilePath
    | OutputFile FilePath
    | WithCpphs FilePath
    | GHCArgs String
    | CpphsArgs String
    | HelpFlag

-- We don't want to use elem, because that imposes Eq a
hasHelpFlag :: [Flag] -> Bool
hasHelpFlag flags = not . null $ [ () | HelpFlag <- flags ]


emptyTempFlags :: TempFlags
emptyTempFlags =
    TempFlags
    { tempGHCPath    = Nothing
    , tempInputFile  = "-"
    , tempOutputFile = "-"
    , tempCpphsPath  = Nothing
    , tempGHCArgs    = []
    , tempCpphsArgs  = []
    }


globalOptions :: [OptDescr Flag]
globalOptions =
    [ Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"
    , Option "w" ["ghc"] (ReqArg WithGHC "PATH") "Use this GHC"
    , Option "" ["cpphs"] (ReqArg WithCpphs "PATH") "Use this cpphs"
    , Option "i" ["input"] (ReqArg InputFile "PATH") "Input file"
    , Option "o" ["output"] (ReqArg OutputFile "PATH") "Output file"
    , Option "" ["ghc-args"] (ReqArg GHCArgs "Arguments") "Arguments to GHC"
    , Option "" ["cpphs-args"] (ReqArg CpphsArgs "Arguments") "Arguments to cpphs"
    ]

printHelp :: IO ()
printHelp =
    do pname <- getProgName
       let syntax_line = concat [ "Usage: ", pname
                                , " [FLAGS]\n"]
       putStrLn (usageInfo syntax_line globalOptions)

parseArgs :: [String] -> IO TempFlags
parseArgs args =
  case getOpt' RequireOrder globalOptions args of
    (flags, _, _, []) | hasHelpFlag flags -> do
      printHelp
      exitWith ExitSuccess
    (flags, [], _, []) -> return (mkTempFlags flags emptyTempFlags)
    (_, _, _, errs) -> do putStrLn "Errors:"
                          mapM_ putStrLn errs
                          exitWith (ExitFailure 1)

mkTempFlags :: [Flag] -> TempFlags -> TempFlags
mkTempFlags = updateCfg
  where updateCfg [] t = t
        updateCfg (fl:flags) t = updateCfg flags $
          case fl of
            WithGHC path    -> t { tempGHCPath    = Just path }
            InputFile path  -> t { tempInputFile  = path }
            OutputFile path -> t { tempOutputFile = path }
            WithCpphs path  -> t { tempCpphsPath  = Just path }
            GHCArgs args    -> t { tempGHCArgs    = words args }
            CpphsArgs args  -> t { tempCpphsArgs  = words args }
            _               -> error $ "Unexpected flag!"