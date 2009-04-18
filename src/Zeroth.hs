module Zeroth
    ( prettyPrintAll, zeroth, zerothInternal
    ) where

import Language.Haskell.Exts hiding ( comments )
import System.Process        ( runInteractiveProcess, waitForProcess )
import System.Info           ( os )
import System.IO             ( hPutStr, hClose, hGetContents, openTempFile, stdin )
import System.Directory      ( removeFile, getTemporaryDirectory )
import System.Exit           ( ExitCode (..) )
import Control.Applicative   ( (<$>) )
import Control.Monad         ( guard, when )
import Data.Generics.Aliases ( mkT )
import Data.Generics.Schemes ( everywhere )
import Data.List             ( (\\), intersperse, isInfixOf, isPrefixOf, nub, stripPrefix )
import Data.Maybe            ( catMaybes, fromMaybe, mapMaybe )

import Comments              ( Location, parseComments, mixComments )
import ListUtils             ( replaceAll )
import ZerothHelper          ( idPrefix )

readFromFile :: FilePath -> IO String
readFromFile "-"  = hGetContents stdin
readFromFile path = readFile path

zeroth :: FilePath -- ^ Path to GHC
       -> FilePath -- ^ Path to cpphs
       -> [String] -- ^ GHC options
       -> [String] -- ^ cpphs options
       -> String   -- ^ Input filename, or "-" for stdin
       -> [String] -- ^ Import prefixes to drop
       -> IO String
zeroth ghcPath cpphsPath ghcOpts cpphsOpts inputFile dropImports
    = prettyPrintAll <$> zerothInternal ghcPath cpphsPath ghcOpts cpphsOpts inputFile dropImports

data ZeroTHOutput
    = ZeroTHOutput { originalSource :: String
                   , combinedOutput :: Module
                   , thOutput :: [(Location, String)]
                   }

zerothInternal :: FilePath -- ^ Path to GHC
               -> FilePath -- ^ Path to cpphs
               -> [String] -- ^ GHC options
               -> [String] -- ^ cpphs options
               -> String   -- ^ Input filename, or "-" for stdin
               -> [String] -- ^ Import prefixes to drop
               -> IO ZeroTHOutput
zerothInternal ghcPath cpphsPath ghcOpts cpphsOpts inputFile dropImports
    = do input       <- readFromFile inputFile
         tmpDir      <- getTemporaryDirectory
         (inputFile2, tmpHandle) <- case inputFile of
                                       "-" -> openTempFile tmpDir "TH.cpphs.zeroth"
                                       _   -> return (inputFile, undefined)
         when (inputFile == "-") $ hPutStr tmpHandle input >> hClose tmpHandle
         let firstLine      = head $ lines input
             shouldRunCpphs = "-cpp" `elem` ghcOpts || " -cpp " `isInfixOf` firstLine || " CPP " `isInfixOf` firstLine 
         thInput     <- if shouldRunCpphs then preprocessCpphs cpphsPath (["--noline","-DHASTH"]++cpphsOpts) inputFile2
                                          else return input
         zerothInput <- if shouldRunCpphs then preprocessCpphs cpphsPath (["--noline"]++cpphsOpts) inputFile2
                                          else return input
         (thData, qualImports) <- case parseModule thInput of
                                     ParseOk m -> unzip <$> runTH ghcPath m ghcOpts
                                     e -> error $ show e
         let reattach :: [Decl] -> [Decl]
             reattach (SpliceDecl sLoc _ : t) = (parseDecls . fromMaybe err $ lookup (location sLoc) thData) ++ t
                 where
                     err = error $ "Could not find splice at " ++ show (location sLoc) ++ " in " ++ show thData
             reattach x                       = x
         combinedData <- case parseModule zerothInput of
                           ParseOk (Module loc m pragmas mWarn exports im decls)
                             -> return (Module loc m pragmas mWarn exports (postProcessImports dropImports im $ concat qualImports)
                                        (everywhere (mkT reattach) decls))
                           e -> error $ show e
         when (inputFile == "-") $ removeFile inputFile2
         return $ ZeroTHOutput { originalSource = input
                               , combinedOutput = combinedData
                               , thOutput = thData
                               }
    where parseDecls s = case parseModule $ "module Main where\n" ++ s of
                           ParseOk (Module _ _ _ _ _ _ decls) -> decls
                           e -> error $ show e

prettyPrintAll :: ZeroTHOutput -> String
prettyPrintAll out = unlines . mixComments (parseComments $ originalSource out) . numberAndPrettyPrint $ combinedOutput out

location :: SrcLoc -> Location
location sLoc = (srcLine sLoc, srcColumn sLoc)

numberAndPrettyPrint :: Module -> [(Location, String)]
numberAndPrettyPrint (Module mLoc m prags mbWarn exports imp decls)
    = (nAndPPrag =<< prags)
      ++ (location mLoc, concat $ "module "
                                 : prettyPrint m
                                 : catMaybes [ ppWarnText <$> mbWarn
                                             , (\es -> " (" ++ concat (intersperse ", " $ prettyPrint <$> es) ++ ")") <$> exports
                                             ]
                                 ++ [" where"])
         : (((\i -> (location (importLoc i), prettyPrint i)) <$> imp) ++ (nAndPDec =<< decls))
    where nAndPDec d@(TypeDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DataDecl loc _ _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(GDataDecl loc _ _ _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InfixDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(ClassDecl loc _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InstDecl loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DefaultDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(SpliceDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(TypeSig loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec (FunBind matches) = (\match@(Match loc _ _ _ _ _) -> (location loc, prettyPrint match)) <$> matches
          nAndPDec d@(PatBind loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(ForImp loc _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(ForExp loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DataFamDecl loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DataInsDecl loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DeprPragmaDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(DerivDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(GDataInsDecl loc _ _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InlineSig loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(InstSig loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(RulePragmaDecl loc _) = [(location loc, prettyPrint d)]
          nAndPDec d@(SpecInlineSig loc _ _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(SpecSig loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(TypeFamDecl loc _ _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(TypeInsDecl loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(UnknownDeclPragma loc _ _) = [(location loc, prettyPrint d)]
          nAndPDec d@(WarnPragmaDecl loc _) = [(location loc, prettyPrint d)]
          nAndPPrag (LanguagePragma loc names)
              | null filteredNames = []
              | otherwise          = [(location loc, prettyPrint $ LanguagePragma loc filteredNames)]
              where
                  filteredNames = names \\ (Ident <$> unwantedLanguageOptions)
          nAndPPrag p@(IncludePragma loc _) = [(location loc, prettyPrint p)]
          nAndPPrag p@(CFilesPragma loc _) = [(location loc, prettyPrint p)]
          nAndPPrag (OptionsPragma loc mt s) = [(location loc, prettyPrint . OptionsPragma loc mt $ filterOptions s)]
          nAndPPrag p@(UnknownTopPragma loc _ _) = [(location loc, prettyPrint p)]
          filterOptions optStr = foldr (\opt -> replaceAll (" -" ++ opt ++ " ") " ") optStr $ "cpp" : "fth" : (('X' :) <$> unwantedLanguageOptions)
          unwantedLanguageOptions = ["CPP", "TemplateHaskell"]

ppWarnText :: WarningText -> String
ppWarnText (DeprText s) = "{-# DEPRECATED" ++ s ++ "#-}"
ppWarnText (WarnText s) = "{-# WARNING" ++ s ++ "#-}"

-- Removes TH imports, and adds any qualified imports needed by generated TH code
postProcessImports :: [String] -> [ImportDecl] -> [String] -> [ImportDecl]
postProcessImports dropPrefixes oldImports qNames
    = nub $ removeTH
            ++ mapMaybe (\q -> do guard . not $ any (maybe False (\(ModuleName m) -> m == q) . importAs) removeTH
                                  return $ ImportDecl { importLoc = emptySrcLoc
                                                      , importModule = ModuleName q
                                                      , importQualified = True
                                                      , importSrc = False
                                                      , importAs = Nothing
                                                      , importSpecs = Nothing })
                        qNames
    where
        removeTH = filter (not . (\(ModuleName m) -> any (`isPrefixOf` m) dropPrefixes) . importModule) oldImports

preprocessCpphs :: FilePath -- ^ Path to cpphs
                -> [String]
                -> String
                -> IO String
preprocessCpphs cpphs args inputFile
    = do (inH,outH,_,pid) <- runInteractiveProcess cpphs (inputFile:args) Nothing Nothing
         hClose inH
         output <- hGetContents outH
         length output `seq` hClose outH
         eCode <- waitForProcess pid
         case eCode of
           ExitFailure err -> error $ "Failed to run cpphs: " ++ show err
           ExitSuccess -> return output

runTH :: FilePath -- ^ Path to GHC
      -> Module 
      -> [String]
      -> IO ([((Location,String),[String])])
runTH ghcPath (Module _ _ pragmas _ _ imports decls) ghcOpts
    = do tmpDir <- getTemporaryDirectory
         (tmpInPath,tmpInHandle) <- openTempFile tmpDir "TH.source.zeroth.hs"
         hPutStr tmpInHandle realM
         hClose tmpInHandle
         let args = [tmpInPath]++ghcOpts++extraOpts
         --putStrLn $ "Module:\n" ++ realM
         --putStrLn $ "Running: " ++ unwords (ghcPath:args)
         (inH,outH,errH,pid) <- runInteractiveProcess ghcPath args Nothing Nothing
         hClose inH
         output <- hGetContents outH
         --putStrLn $ "TH Data:\n" ++ output
         length output `seq` hClose outH
         errMsg <- hGetContents errH
         length errMsg `seq` hClose errH
         eCode <- waitForProcess pid
         -- removeFile tmpInPath
         let check :: [(((Location,String),[String]), String)] -> ((Location,String),[String])
             check [(ret,_)] = ret
             check _         = error $ "Failed to parse result:\n" ++ output
         case eCode of
           ExitFailure err -> error (unwords (ghcPath:args) ++ ": failure: " ++ show err ++ ":\n" ++ errMsg)
           ExitSuccess | not (null errMsg) -> error (unwords (ghcPath:args) ++ ": failure:\n" ++ errMsg)
                       | otherwise -> return . mapMaybe (fmap (check . reads) . stripPrefix idPrefix) $ lines output
    where pp :: (Pretty a) => a -> String
          pp = prettyPrintWithMode (defaultMode{layout = PPInLine})
          realM = unlines $ (pp . disableWarnings <$> pragmas)
                            ++ ["module ZerothTemp where"]
                            ++ (pp <$> imports)
                            ++ ["import qualified ZerothHelper"]
                            ++ (prettyPrint <$> everywhere (mkT editSplice) decls)
          editSplice :: Decl -> Decl
          editSplice (SpliceDecl loc splice)
              = SpliceDecl loc
                  . ParenSplice
                  . App (App (Var . Qual (ModuleName "ZerothHelper") $ Ident "helper")
                             (Paren $ spliceToExp splice))
                  . Tuple
                  $ Lit . Int . fromIntegral <$> [ srcLine loc, srcColumn loc ]
          editSplice x = x
          spliceToExp (ParenSplice e) = e
          spliceToExp _ = error "TH: FIXME!"
          nullFile
              | "mingw" `isPrefixOf` os = "NUL:"
              | otherwise               = "/dev/null"
          extraOpts = ["-w", "-package", "base", "-package", "zeroth", "-o", nullFile, "-ohi", nullFile, "-fno-code"]
          extraOpts' = (' ' :) =<< extraOpts
          disableWarnings (OptionsPragma loc Nothing    s) = OptionsPragma loc Nothing $ s ++ extraOpts' -- Turn off all warnings (works for GHC)
          disableWarnings (OptionsPragma loc (Just GHC) s) = OptionsPragma loc (Just GHC) $ s ++ extraOpts'
          disableWarnings x = x

emptySrcLoc :: SrcLoc
emptySrcLoc = SrcLoc "" 0 0

{-

module Test where
$(test) -- line 2
$(jalla) -- line 3
svend = svend


-------------------------------

module Main where
import Language.Haskell.TH
main = do decs <- sequence [runQ test
                           ,runQ jalla]
          mapM_ (putStrLn.pprint) (zip decs [2,3])


-}
