module Language.Haskell.TH.ZeroTH.GetOpt where

import Control.Applicative    ( (<$>) )
import System.Console.GetOpt  ( ArgDescr (..), OptDescr (..), usageInfo, getOpt', ArgOrder (..) )
import System.Environment     ( getProgName )
import System.Exit            ( exitWith, ExitCode (..) )
import System.Directory       ( findExecutable )
import Data.Maybe             ( fromMaybe )

import Language.Haskell.TH.ZeroTH.Config ( Config(..) )

getExecutable :: String -> Maybe FilePath -> IO FilePath
getExecutable _ (Just path) = return path
getExecutable name Nothing  = fromMaybe (error errMsg) <$> findExecutable name
    where errMsg = "Couldn't find: "++name

mkConfig :: TempFlags -> IO Config
mkConfig tmpFlags
    = do ghcPath'   <- getExecutable "ghc" (tempGHCPath tmpFlags)
         cpphsPath' <- getExecutable "cpphs" (tempCpphsPath tmpFlags)
         return (Config
                 { ghcPath    = ghcPath'
                 , cpphsPath  = cpphsPath'
                 , inputFile  = tempInputFile tmpFlags
                 , outputFile = tempOutputFile tmpFlags
                 , ghcArgs    = tempGHCArgs tmpFlags
                 , cpphsArgs  = tempCpphsArgs tmpFlags
                 , dropImport = tempDropImport tmpFlags
                 , wholeFile  = tempWholeFile tmpFlags})

data TempFlags
    = TempFlags
    { tempGHCPath    :: Maybe FilePath
    , tempInputFile  :: FilePath
    , tempOutputFile :: FilePath
    , tempCpphsPath  :: Maybe FilePath
    , tempGHCArgs    :: [String]
    , tempCpphsArgs  :: [String]
    , tempDropImport :: [String]
    , tempWholeFile  :: Bool
    } deriving Show

data Flag
    = WithGHC FilePath
    | InputFile FilePath
    | OutputFile FilePath
    | WithCpphs FilePath
    | GHCArgs String
    | CpphsArgs String
    | DropImport String
    | WholeFile
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
    , tempDropImport = ["Language.Haskell.TH"]  -- The only way to override this is to edit this line
    , tempWholeFile  = False
    }

globalOptions :: [OptDescr Flag]
globalOptions =
    [ Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"
    , Option "" ["whole-file"] (NoArg WholeFile) "Pass the whole file to GHC (for future use)"
    , Option "w" ["ghc"] (ReqArg WithGHC "PATH") "Use this GHC"
    , Option "" ["cpphs"] (ReqArg WithCpphs "PATH") "Use this cpphs"
    , Option "i" ["input"] (ReqArg InputFile "PATH") "Input file"
    , Option "o" ["output"] (ReqArg OutputFile "PATH") "Output file"
    , Option "" ["ghc-args"] (ReqArg GHCArgs "Arguments") "Arguments to GHC"
    , Option "" ["cpphs-args"] (ReqArg CpphsArgs "Arguments") "Arguments to cpphs"
    , Option "d" ["drop-import"] (ReqArg DropImport "Prefix") "Any import that starts with this prefix will be removed"
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
            GHCArgs args    -> t { tempGHCArgs    = tempGHCArgs t ++ words args }
            CpphsArgs args  -> t { tempCpphsArgs  = tempCpphsArgs t ++ words args }
            DropImport pre  -> t { tempDropImport = tempDropImport t ++ words pre }
            WholeFile       -> t { tempWholeFile  = True }
            HelpFlag        -> t