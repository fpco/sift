{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-incomplete-patterns -optc-DNON_POSIX_SOURCE -fno-warn-warnings-deprecations #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-----------------------------------------------------------------------------
--
-- GHC Driver program
--
-- (c) The University of Glasgow 2005
--
-----------------------------------------------------------------------------

module Main (main) where

-- The official GHC API
import qualified Data.Version (showVersion)
import qualified GHC
import GHC              ( -- DynFlags(..), HscTarget(..),
                          -- GhcMode(..), GhcLink(..),
                          Ghc, GhcMonad(..),
                          LoadHowMuch(..) )
import           CmdLineParser
import qualified Paths_sift_compiler

-- ghci-ng
import qualified GHC.Paths
import Sift.Compat

-- Implementations of the various modes (--show-iface, mkdependHS. etc.)
import           LoadIface ( showIface )
import           HscMain ( newHscEnv )
import           DriverPipeline ( oneShot, compileFile )
import           DriverMkDepend ( doMkDependHS )
#ifdef GHCI
import           InteractiveUI ( interactiveUI, ghciWelcomeMsg, defaultGhciSettings )
#endif


-- Various other random stuff that we need
import           Config
import           Constants
import           HscTypes
#if __GLASGOW_HASKELL__ < 709
import           Packages ( dumpPackages )
#else
import           Packages ( pprPackages )
#endif
import           DriverPhases
import           BasicTypes ( failed )
#if __GLASGOW_HASKELL__ < 802
import           StaticFlags
#endif
import           DynFlags
import           ErrUtils
import           FastString
import           Outputable
import           SrcLoc
import           Util
import           Panic
import           MonadUtils ( liftIO )

-- Imports for --abi-hash
import           LoadIface ( loadUserInterface )
import           Module ( mkModuleName )
#if __GLASGOW_HASKELL__ >= 802
import           Finder ( findImportedModule, cannotFindModule )
#else
import           Finder ( findImportedModule, cannotFindInterface )
#endif
import           TcRnMonad ( initIfaceCheck )
#if __GLASGOW_HASKELL__ >= 802
import           Binary ( openBinMem, put_ )
import           BinFingerprint ( fingerprintBinMem )
#else
import           Binary ( openBinMem, put_, fingerprintBinMem )
#endif

-- Standard Haskell libraries
import           System.IO
import           System.Environment
import           System.Exit
import           System.FilePath
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe

-----------------------------------------------------------------------------
-- ToDo:

-- time commands when run with -v
-- user ways
-- Win32 support: proper signal handling
-- reading the package configuration file is too slow
-- -K<size>

-----------------------------------------------------------------------------
-- GHC's command-line interface

main :: IO ()
main = do
   env <- getEnvironment
   initGCStatistics -- See Note [-Bsymbolic and hooks]
   hSetBuffering stdout LineBuffering
   hSetBuffering stderr LineBuffering
   GHC.defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    -- 1. extract the -B flag from the args
    argv00 <- getArgs
    if elem "--version" argv00
       then do putStrLn ("Sift " ++ Data.Version.showVersion Paths_sift_compiler.version)
               exitSuccess
       else return ()
    case lookup "STACK_EXE" env of
      Just{} -> return ()
      Nothing ->
        hPutStr stderr ("WARNING: it is HIGHLY RECOMMENDED to use sift with stack:\n\n"
                       ++ "  To install:\n"
                       ++ "    stack build sift\n\n"
                       ++ "  To run with no project:\n"
                       ++ "    stack exec sift\n"
                       ++ "  To run with your project:\n"
                       ++ "    stack ghci --with-ghc sift\n\n")
    let argv0 = ("-B" ++ GHC.Paths.libdir) :
                if any (`elem` argv00) ["--info", "--interactive", "--make", "-c"]
                  then argv00 -- needed for "cabal repl --with-ghc=ghci-ng"
                  else "--interactive" : argv00

    let (minusB_args, argv1) = partition ("-B" `isPrefixOf`) argv0
        mbMinusB | null minusB_args = Nothing
                 | otherwise = Just (drop 2 (last minusB_args))

    let argv1' = map (mkGeneralLocated "on the commandline") argv1

#if __GLASGOW_HASKELL__ >= 802
    (mode, argv3, modeFlagWarnings) <- parseModeFlags argv1'
    let flagWarnings = modeFlagWarnings
#else
    (argv2, staticFlagWarnings) <- GHC.parseStaticFlags argv1'
    (mode, argv3, modeFlagWarnings) <- parseModeFlags argv2
    let flagWarnings = staticFlagWarnings ++ modeFlagWarnings
#endif

    -- If all we want to do is something like showing the version number
    -- then do it now, before we start a GHC session etc. This makes
    -- getting basic information much more resilient.

    -- In particular, if we wait until later before giving the version
    -- number then bootstrapping gets confused, as it tries to find out
    -- what version of GHC it's using before package.conf exists, so
    -- starting the session fails.
    case mode of
        Left preStartupMode ->
            do case preStartupMode of
                   ShowSupportedExtensions -> showSupportedExtensions
                   ShowVersion             -> showVersion
                   ShowNumVersion          -> putStrLn cProjectVersion
                   ShowOptions             -> showOptions
        Right postStartupMode ->
            -- start our GHC session
            GHC.runGhc mbMinusB $ do

            dflags <- GHC.getSessionDynFlags

            case postStartupMode of
                Left preLoadMode ->
                    liftIO $ do
                        case preLoadMode of
                            ShowInfo               -> showInfo dflags
                            ShowGhcUsage           -> showGhcUsage  dflags
                            ShowGhciUsage          -> showGhciUsage dflags
                            PrintWithDynFlags f    -> putStrLn (f dflags)
                Right postLoadMode ->
                    main' postLoadMode dflags argv3 flagWarnings

main' :: PostLoadMode -> DynFlags -> [Located String] -> [Located String]
      -> Ghc ()
main' postLoadMode dflags0 args flagWarnings = do
  -- set the default GhcMode, HscTarget and GhcLink.  The HscTarget
  -- can be further adjusted on a module by module basis, using only
  -- the -fvia-C and -fasm flags.  If the default HscTarget is not
  -- HscC or HscAsm, -fvia-C and -fasm have no effect.
  let dflt_target = hscTarget dflags0
      (mode, lang, link)
         = case postLoadMode of
               DoInteractive   -> (CompManager, HscInterpreted, LinkInMemory)
               DoEval _        -> (CompManager, HscInterpreted, LinkInMemory)
               DoMake          -> (CompManager, dflt_target,    LinkBinary)
               DoMkDependHS    -> (MkDepend,    dflt_target,    LinkBinary)
               DoAbiHash       -> (OneShot,     dflt_target,    LinkBinary)
               _               -> (OneShot,     dflt_target,    LinkBinary)

  let dflags1 = case lang of
                HscInterpreted ->
                    let platform = targetPlatform dflags0
                        dflags0a = updateWays $ dflags0 { ways = interpWays }
                        dflags0b = foldl gopt_set dflags0a
                                 $ concatMap (wayGeneralFlags platform)
                                             interpWays
                        dflags0c = foldl gopt_unset dflags0b
                                 $ concatMap (wayUnsetGeneralFlags platform)
                                             interpWays
                    in dflags0c
                _ ->
                    dflags0
      dflags2 = dflags1{ ghcMode   = mode,
                         hscTarget = lang,
                         ghcLink   = link,
                         verbosity = case postLoadMode of
                                         DoEval _ -> 0
                                         _other   -> 1
                        }

      -- turn on -fimplicit-import-qualified for GHCi now, so that it
      -- can be overriden from the command-line
      -- XXX: this should really be in the interactive DynFlags, but
      -- we don't set that until later in interactiveUI
      dflags3  | DoInteractive <- postLoadMode = imp_qual_enabled
               | DoEval _      <- postLoadMode = imp_qual_enabled
               | otherwise                     = dflags2
        where imp_qual_enabled = dflags2 `gopt_set` Opt_ImplicitImportQualified

        -- The rest of the arguments are "dynamic"
        -- Leftover ones are presumably files
  (dflags4, fileish_args, dynamicFlagWarnings) <- GHC.parseDynamicFlags dflags3 args

  GHC.prettyPrintGhcErrors dflags4 $ do

  let flagWarnings' = (map ghc_mkWarn flagWarnings) ++ dynamicFlagWarnings

  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
         liftIO $ handleFlagWarnings dflags4 flagWarnings'

        -- make sure we clean up after ourselves
  GHC.defaultCleanupHandler dflags4 $ do

  liftIO $ showBanner postLoadMode dflags4

  let
     -- To simplify the handling of filepaths, we normalise all filepaths right
     -- away - e.g., for win32 platforms, backslashes are converted
     -- into forward slashes.
    normal_fileish_paths = map (normalise . unLoc) fileish_args
    (srcs, objs)         = partition_args normal_fileish_paths [] []

    dflags5 = dflags4 { ldInputs = map (FileOption "") objs
                                   ++ ldInputs dflags4 }

  -- we've finished manipulating the DynFlags, update the session
  _ <- GHC.setSessionDynFlags dflags5
  dflags6 <- GHC.getSessionDynFlags
  hsc_env <- GHC.getSession

        ---------------- Display configuration -----------
  when (verbosity dflags6 >= 4) $
#if __GLASGOW_HASKELL__ >= 802
        let dumpPackages flags = putStrLn $ show $ runSDoc (pprPackages flags) ctx
                where ctx = initSDocContext flags (defaultDumpStyle dflags6)
        in
#elif __GLASGOW_HASKELL__ >= 709
        let dumpPackages flags = putStrLn $ show $ runSDoc (pprPackages flags) ctx
                where ctx = initSDocContext flags defaultDumpStyle
        in
#endif
        liftIO $ dumpPackages dflags6

# if __GLASGOW_HASKELL__ < 802
  when (verbosity dflags6 >= 3) $ do
        liftIO $ hPutStrLn stderr ("Hsc static flags: " ++ unwords staticFlags)
#endif

        ---------------- Final sanity checking -----------
  liftIO $ checkOptions postLoadMode dflags6 srcs objs

  ---------------- Do the business -----------
  handleSourceError (\e -> do
       GHC.printException e
       liftIO $ exitWith (ExitFailure 1)) $ do
    case postLoadMode of
       ShowInterface f        -> liftIO $ doShowIface dflags6 f
       DoMake                 -> doMake srcs
       DoMkDependHS           -> doMkDependHS (map fst srcs)
       StopBefore p           -> liftIO (oneShot hsc_env p srcs)
       DoInteractive          -> ghciUI srcs Nothing
       DoEval exprs           -> ghciUI srcs $ Just $ reverse exprs
       DoAbiHash              -> abiHash srcs

  liftIO $ dumpFinalStats dflags6

ghciUI :: [(FilePath, Maybe Phase)] -> Maybe [String] -> Ghc ()
#ifndef GHCI
ghciUI _ _ = throwGhcException (CmdLineError "not built for interactive use")
#else
ghciUI     = interactiveUI defaultGhciSettings
#endif

-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || '.' `notElem` m

-- -----------------------------------------------------------------------------
-- Option sanity checks

-- | Ensure sanity of options.
--
-- Throws 'UsageError' or 'CmdLineError' if not.
checkOptions :: PostLoadMode -> DynFlags -> [(String,Maybe Phase)] -> [String] -> IO ()
     -- Final sanity checking before kicking off a compilation (pipeline).
checkOptions mode dflags srcs objs = do
     -- Complain about any unknown flags
   let unknown_opts = [ f | (f@('-':_), _) <- srcs ]
   when (notNull unknown_opts) (unknownFlagsErr unknown_opts)

   when (notNull (filter wayRTSOnly (ways dflags))
         && isInterpretiveMode mode) $
        hPutStrLn stderr ("Warning: -debug, -threaded and -ticky are ignored by GHCi")

        -- -prof and --interactive are not a good combination
   when ((filter (not . wayRTSOnly) (ways dflags) /= interpWays)
         && isInterpretiveMode mode) $
      do throwGhcException (UsageError
                   "--interactive can't be used with -prof or -unreg.")
        -- -ohi sanity check
   if (isJust (outputHi dflags) &&
      (isCompManagerMode mode || srcs `lengthExceeds` 1))
        then throwGhcException (UsageError "-ohi can only be used when compiling a single source file")
        else do

        -- -o sanity checking
   if (srcs `lengthExceeds` 1 && isJust (outputFile dflags)
         && not (isLinkMode mode))
        then throwGhcException (UsageError "can't apply -o to multiple source files")
        else do

   let not_linking = not (isLinkMode mode) || isNoLink (ghcLink dflags)

   when (not_linking && not (null objs)) $
        hPutStrLn stderr ("Warning: the following files would be used as linker inputs, but linking is not being done: " ++ unwords objs)

        -- Check that there are some input files
        -- (except in the interactive case)
   if null srcs && (null objs || not_linking) && needsInputsMode mode
        then throwGhcException (UsageError "no input files")
        else do

     -- Verify that output files point somewhere sensible.
   verifyOutputFiles dflags


-- Compiler output options

-- called to verify that the output files & directories
-- point somewhere valid.
--
-- The assumption is that the directory portion of these output
-- options will have to exist by the time 'verifyOutputFiles'
-- is invoked.
--
verifyOutputFiles :: DynFlags -> IO ()
verifyOutputFiles dflags = do
  -- not -odir: we create the directory for -odir if it doesn't exist (#2278).
  let ofile = outputFile dflags
  when (isJust ofile) $ do
     let fn = fromJust ofile
     flg <- doesDirNameExist fn
     when (not flg) (nonExistentDir "-o" fn)
  let ohi = outputHi dflags
  when (isJust ohi) $ do
     let hi = fromJust ohi
     flg <- doesDirNameExist hi
     when (not flg) (nonExistentDir "-ohi" hi)
 where
   nonExistentDir flg dir =
     throwGhcException (CmdLineError ("error: directory portion of " ++
                             show dir ++ " does not exist (used with " ++
                             show flg ++ " option.)"))

-----------------------------------------------------------------------------
-- GHC modes of operation

type Mode = Either PreStartupMode PostStartupMode
type PostStartupMode = Either PreLoadMode PostLoadMode

data PreStartupMode
  = ShowVersion             -- ghc -V/--version
  | ShowNumVersion          -- ghc --numeric-version
  | ShowSupportedExtensions -- ghc --supported-extensions
  | ShowOptions             -- ghc --show-options

showVersionMode, showNumVersionMode, showSupportedExtensionsMode, showOptionsMode :: Mode
showVersionMode             = mkPreStartupMode ShowVersion
showNumVersionMode          = mkPreStartupMode ShowNumVersion
showSupportedExtensionsMode = mkPreStartupMode ShowSupportedExtensions
showOptionsMode             = mkPreStartupMode ShowOptions

mkPreStartupMode :: PreStartupMode -> Mode
mkPreStartupMode = Left

isShowVersionMode :: Mode -> Bool
isShowVersionMode (Left ShowVersion) = True
isShowVersionMode _ = False

isShowNumVersionMode :: Mode -> Bool
isShowNumVersionMode (Left ShowNumVersion) = True
isShowNumVersionMode _ = False

data PreLoadMode
  = ShowGhcUsage                           -- ghc -?
  | ShowGhciUsage                          -- ghci -?
  | ShowInfo                               -- ghc --info
  | PrintWithDynFlags (DynFlags -> String) -- ghc --print-foo

showGhcUsageMode, showGhciUsageMode, showInfoMode :: Mode
showGhcUsageMode = mkPreLoadMode ShowGhcUsage
showGhciUsageMode = mkPreLoadMode ShowGhciUsage
showInfoMode = mkPreLoadMode ShowInfo

printSetting :: String -> Mode
printSetting k = mkPreLoadMode (PrintWithDynFlags f)
    where f dflags = fromMaybe (panic ("Setting not found: " ++ show k))
                   $ lookup k (compilerInfo dflags)

mkPreLoadMode :: PreLoadMode -> Mode
mkPreLoadMode = Right . Left

isShowGhcUsageMode :: Mode -> Bool
isShowGhcUsageMode (Right (Left ShowGhcUsage)) = True
isShowGhcUsageMode _ = False

isShowGhciUsageMode :: Mode -> Bool
isShowGhciUsageMode (Right (Left ShowGhciUsage)) = True
isShowGhciUsageMode _ = False

data PostLoadMode
  = ShowInterface FilePath  -- ghc --show-iface
  | DoMkDependHS            -- ghc -M
  | StopBefore Phase        -- ghc -E | -C | -S
                            -- StopBefore StopLn is the default
  | DoMake                  -- ghc --make
  | DoInteractive           -- ghc --interactive
  | DoEval [String]         -- ghc -e foo -e bar => DoEval ["bar", "foo"]
  | DoAbiHash               -- ghc --abi-hash

doMkDependHSMode, doMakeMode, doInteractiveMode, doAbiHashMode :: Mode
doMkDependHSMode = mkPostLoadMode DoMkDependHS
doMakeMode = mkPostLoadMode DoMake
doInteractiveMode = mkPostLoadMode DoInteractive
doAbiHashMode = mkPostLoadMode DoAbiHash

showInterfaceMode :: FilePath -> Mode
showInterfaceMode fp = mkPostLoadMode (ShowInterface fp)

stopBeforeMode :: Phase -> Mode
stopBeforeMode phase = mkPostLoadMode (StopBefore phase)

doEvalMode :: String -> Mode
doEvalMode str = mkPostLoadMode (DoEval [str])

mkPostLoadMode :: PostLoadMode -> Mode
mkPostLoadMode = Right . Right

isDoInteractiveMode :: Mode -> Bool
isDoInteractiveMode (Right (Right DoInteractive)) = True
isDoInteractiveMode _ = False

isStopLnMode :: Mode -> Bool
isStopLnMode (Right (Right (StopBefore StopLn))) = True
isStopLnMode _ = False

isDoMakeMode :: Mode -> Bool
isDoMakeMode (Right (Right DoMake)) = True
isDoMakeMode _ = False

#ifdef GHCI
isInteractiveMode :: PostLoadMode -> Bool
isInteractiveMode DoInteractive = True
isInteractiveMode _             = False
#endif

-- isInterpretiveMode: byte-code compiler involved
isInterpretiveMode :: PostLoadMode -> Bool
isInterpretiveMode DoInteractive = True
isInterpretiveMode (DoEval _)    = True
isInterpretiveMode _             = False

needsInputsMode :: PostLoadMode -> Bool
needsInputsMode DoMkDependHS    = True
needsInputsMode (StopBefore _)  = True
needsInputsMode DoMake          = True
needsInputsMode _               = False

-- True if we are going to attempt to link in this mode.
-- (we might not actually link, depending on the GhcLink flag)
isLinkMode :: PostLoadMode -> Bool
isLinkMode (StopBefore StopLn) = True
isLinkMode DoMake              = True
isLinkMode DoInteractive       = True
isLinkMode (DoEval _)          = True
isLinkMode _                   = False

isCompManagerMode :: PostLoadMode -> Bool
isCompManagerMode DoMake        = True
isCompManagerMode DoInteractive = True
isCompManagerMode (DoEval _)    = True
isCompManagerMode _             = False

-- -----------------------------------------------------------------------------
-- Parsing the mode flag

parseModeFlags :: [Located String]
               -> IO (Mode,
                      [Located String],
                      [Located String])
parseModeFlags args = do
  let ((leftover, errs1, warns), (mModeFlag, errs2, flags')) =
          runCmdLine (processArgs mode_flags args)
                     (Nothing, [], [])
      mode = case mModeFlag of
             Nothing     -> doMakeMode
             Just (m, _) -> m
      errs = errs1 ++ map ghc_mkErr (map (mkGeneralLocated "on the commandline") errs2)
  when (not (null errs)) $ throwGhcException
#if __GLASGOW_HASKELL__ < 709
                             $ errorsToGhcException errs
#else
                             $ errorsToGhcException $ map (\(L sp e) -> (show sp, e)) (map ghc_errMsg errs)
#endif
  return (mode, flags' ++ leftover, map ghc_warnMsg warns)

type ModeM = CmdLineP (Maybe (Mode, String), [String], [Located String])
  -- mode flags sometimes give rise to new DynFlags (eg. -C, see below)
  -- so we collect the new ones and return them.

mode_flags :: [Flag ModeM]
#if __GLASGOW_HASKELL__ < 709
mode_flags = flags
#else
mode_flags = zipWith ($) flags ghcModes
#endif
  where flags = concat [help, othr, prim]
        ------- help / version -------------------------------------------------
        help = [
                 Flag "?"                     (PassFlag (setMode showGhcUsageMode))
               , Flag "-help"                 (PassFlag (setMode showGhcUsageMode))
               , Flag "V"                     (PassFlag (setMode showVersionMode))
               , Flag "-version"              (PassFlag (setMode showVersionMode))
               , Flag "-numeric-version"      (PassFlag (setMode showNumVersionMode))
               , Flag "-info"                 (PassFlag (setMode showInfoMode))
               , Flag "-show-options"         (PassFlag (setMode showOptionsMode))
               , Flag "-supported-languages"  (PassFlag (setMode showSupportedExtensionsMode))
               , Flag "-supported-extensions" (PassFlag (setMode showSupportedExtensionsMode))
               ]
        othr = [ Flag k'                      (PassFlag (setMode (printSetting k)))
               | k <- ["Project version",
                       "Booter version",
                       "Stage",
                       "Build platform",
                       "Host platform",
                       "Target platform",
                       "Have interpreter",
                       "Object splitting supported",
                       "Have native code generator",
                       "Support SMP",
                       "Unregisterised",
                       "Tables next to code",
                       "RTS ways",
                       "Leading underscore",
                       "Debug on",
                       "LibDir",
                       "Global Package DB",
                       "C compiler flags",
                       "Gcc Linker flags",
                       "Ld Linker flags"],
                 let k' = "-print-" ++ map (replaceSpace . toLower) k
                     replaceSpace ' ' = '-'
                     replaceSpace c   = c
               ]
        ------- interfaces -----------------------------------------------------
        prim = [ Flag "-show-iface"  (HasArg (\f -> setMode (showInterfaceMode f)
                                                            "--show-iface"))

        ------- primary modes --------------------------------------------------
               , Flag "c"            (PassFlag (\f -> do setMode (stopBeforeMode StopLn) f
                                                         addFlag "-no-link" f))
               , Flag "M"            (PassFlag (setMode doMkDependHSMode))
               , Flag "E"            (PassFlag (setMode (stopBeforeMode anyHsc)))
               , Flag "C"            (PassFlag (setMode (stopBeforeMode HCc)))
               , Flag "S"            (PassFlag (setMode (stopBeforeMode (as False))))
               , Flag "-make"        (PassFlag (setMode doMakeMode))
               , Flag "-interactive" (PassFlag (setMode doInteractiveMode))
               , Flag "-abi-hash"    (PassFlag (setMode doAbiHashMode))
               , Flag "e"            (SepArg   (\s -> setMode (doEvalMode s) "-e"))
               ]
#if __GLASGOW_HASKELL__ >= 709
        ghcModes = cycle [AllModes]
#endif

setMode :: Mode -> String -> EwM ModeM ()
setMode newMode newFlag = liftEwM $ do
    (mModeFlag, errs, flags') <- getCmdLineState
    let (modeFlag', errs') =
            case mModeFlag of
            Nothing -> ((newMode, newFlag), errs)
            Just (oldMode, oldFlag) ->
                case (oldMode, newMode) of
                    -- -c/--make are allowed together, and mean --make -no-link
                    _ |  isStopLnMode oldMode && isDoMakeMode newMode
                      || isStopLnMode newMode && isDoMakeMode oldMode ->
                      ((doMakeMode, "--make"), [])

                    -- If we have both --help and --interactive then we
                    -- want showGhciUsage
                    _ | isShowGhcUsageMode oldMode &&
                        isDoInteractiveMode newMode ->
                            ((showGhciUsageMode, oldFlag), [])
                      | isShowGhcUsageMode newMode &&
                        isDoInteractiveMode oldMode ->
                            ((showGhciUsageMode, newFlag), [])
                    -- Otherwise, --help/--version/--numeric-version always win
                      | isDominantFlag oldMode -> ((oldMode, oldFlag), [])
                      | isDominantFlag newMode -> ((newMode, newFlag), [])
                    -- We need to accumulate eval flags like "-e foo -e bar"
                    (Right (Right (DoEval esOld)),
                     Right (Right (DoEval [eNew]))) ->
                        ((Right (Right (DoEval (eNew : esOld))), oldFlag),
                         errs)
                    -- Saying e.g. --interactive --interactive is OK
                    _ | oldFlag == newFlag -> ((oldMode, oldFlag), errs)
                    -- Otherwise, complain
                    _ -> let err = flagMismatchErr oldFlag newFlag
                         in ((oldMode, oldFlag), err : errs)
    putCmdLineState (Just modeFlag', errs', flags')
  where isDominantFlag f = isShowGhcUsageMode   f ||
                           isShowGhciUsageMode  f ||
                           isShowVersionMode    f ||
                           isShowNumVersionMode f

flagMismatchErr :: String -> String -> String
flagMismatchErr oldFlag newFlag
    = "cannot use `" ++ oldFlag ++  "' with `" ++ newFlag ++ "'"

addFlag :: String -> String -> EwM ModeM ()
addFlag s flag = liftEwM $ do
  (m, e, flags') <- getCmdLineState
  putCmdLineState (m, e, mkGeneralLocated loc s : flags')
    where loc = "addFlag by " ++ flag ++ " on the commandline"

-- ----------------------------------------------------------------------------
-- Run --make mode

doMake :: [(String,Maybe Phase)] -> Ghc ()
doMake srcs  = do
    let (hs_srcs, non_hs_srcs) = partition haskellish srcs

        haskellish (f,Nothing) =
          looksLikeModuleName f || isHaskellUserSrcFilename f || '.' `notElem` f
        haskellish (_,Just phase) =
          phase `notElem` [as True, Cc, Cobjc, CmmCpp, Cmm, StopLn]

    hsc_env <- GHC.getSession

    -- if we have no haskell sources from which to do a dependency
    -- analysis, then just do one-shot compilation and/or linking.
    -- This means that "ghc Foo.o Bar.o -o baz" links the program as
    -- we expect.
    if (null hs_srcs)
       then liftIO (oneShot hsc_env StopLn srcs)
       else do

    o_files <- mapM (\x -> liftIO $ compileFile hsc_env StopLn x)
                 non_hs_srcs
    dflags <- GHC.getSessionDynFlags
    let dflags' = dflags { ldInputs = map (FileOption "") o_files
                                      ++ ldInputs dflags }
    _ <- GHC.setSessionDynFlags dflags'

    targets <- mapM (uncurry GHC.guessTarget) hs_srcs
    GHC.setTargets targets
    ok_flag <- GHC.load LoadAllTargets

    when (failed ok_flag) (liftIO $ exitWith (ExitFailure 1))
    return ()


-- ---------------------------------------------------------------------------
-- --show-iface mode

doShowIface :: DynFlags -> FilePath -> IO ()
doShowIface dflags file = do
  hsc_env <- newHscEnv dflags
  showIface hsc_env file

-- ---------------------------------------------------------------------------
-- Various banners and verbosity output.

showBanner :: PostLoadMode -> DynFlags -> IO ()
showBanner _postLoadMode dflags = do
   let verb = verbosity dflags

#ifdef GHCI
   -- Show the GHCi banner
   when (isInteractiveMode _postLoadMode && verb >= 1) $ putStrLn ghciWelcomeMsg
#endif

   -- Display details of the configuration in verbose mode
   when (verb >= 2) $
    do hPutStr stderr "Glasgow Haskell Compiler, Version "
       hPutStr stderr cProjectVersion
       hPutStr stderr ", stage "
       hPutStr stderr cStage
       hPutStr stderr " booted by GHC version "
       hPutStrLn stderr cBooterVersion

-- We print out a Read-friendly string, but a prettier one than the
-- Show instance gives us
showInfo :: DynFlags -> IO ()
showInfo dflags = do
        let sq x = " [" ++ x ++ "\n ]"
        putStrLn $ sq $ intercalate "\n ," $ map show $ compilerInfo dflags

showSupportedExtensions :: IO ()
showSupportedExtensions = mapM_ putStrLn supportedLanguagesAndExtensions

showVersion :: IO ()
showVersion = putStrLn (cProjectName ++ ", version " ++ cProjectVersion)

showOptions :: IO ()
showOptions = putStr (unlines availableOptions)
    where
#if __GLASGOW_HASKELL__ >= 802
      availableOptions     = map ((:) '-') $
                             getFlagNames mode_flags   ++
                             getFlagNames flagsDynamic
#else
      availableOptions     = map ((:) '-') $
                             getFlagNames mode_flags   ++
                             getFlagNames flagsDynamic ++
                             (filterUnwantedStatic . getFlagNames $ flagsStatic) ++
                             flagsStaticNames
      -- this is a hack to get rid of two unwanted entries that get listed
      -- as static flags. Hopefully this hack will disappear one day together
      -- with static flags
      filterUnwantedStatic      = filter (\x -> not (x `elem` ["f", "fno-"]))
#endif
      getFlagNames opts         = map getFlagName opts
#if __GLASGOW_HASKELL__ >= 710
      getFlagName (Flag name _ _) = name
#else
      getFlagName (Flag name _) = name
#endif

showGhcUsage :: DynFlags -> IO ()
showGhcUsage = showUsage False

showGhciUsage :: DynFlags -> IO ()
showGhciUsage = showUsage True

showUsage :: Bool -> DynFlags -> IO ()
showUsage ghci dflags = do
  let usage_path = if ghci then ghciUsagePath dflags
                           else ghcUsagePath dflags
  usage <- readFile usage_path
  dump usage
  where
     dump ""          = return ()
     dump ('$':'$':s) = putStr progName >> dump s
     dump (c:s)       = putChar c >> dump s

dumpFinalStats :: DynFlags -> IO ()
dumpFinalStats dflags =
  when (gopt Opt_D_faststring_stats dflags) $ dumpFastStringStats dflags

dumpFastStringStats :: DynFlags -> IO ()
dumpFastStringStats dflags = do
  buckets <- getFastStringTable
  let (entries, longest, has_z) = countFS 0 0 0 buckets
      msg = text "FastString stats:" $$
            nest 4 (vcat [text "size:           " <+> int (length buckets),
                          text "entries:        " <+> int entries,
                          text "longest chain:  " <+> int longest,
                          text "has z-encoding: " <+> (has_z `pcntOf` entries)
                         ])
        -- we usually get more "has z-encoding" than "z-encoded", because
        -- when we z-encode a string it might hash to the exact same string,
        -- which will is not counted as "z-encoded".  Only strings whose
        -- Z-encoding is different from the original string are counted in
        -- the "z-encoded" total.
  putMsg dflags msg
  where
   x `pcntOf` y = int ((x * 100) `quot` y) Outputable.<> char '%'

countFS :: Int -> Int -> Int -> [[FastString]] -> (Int, Int, Int)
countFS entries longest has_z [] = (entries, longest, has_z)
countFS entries longest has_z (b:bs) =
  let
        len = length b
        longest' = max len longest
        entries' = entries + len
        has_zs = length (filter hasZEncoding b)
  in
        countFS entries' longest' (has_z + has_zs) bs

-- -----------------------------------------------------------------------------
-- ABI hash support

{-
        ghc --abi-hash Data.Foo System.Bar

Generates a combined hash of the ABI for modules Data.Foo and
System.Bar.  The modules must already be compiled, and appropriate -i
options may be necessary in order to find the .hi files.

This is used by Cabal for generating the InstalledPackageId for a
package.  The InstalledPackageId must change when the visible ABI of
the package chagnes, so during registration Cabal calls ghc --abi-hash
to get a hash of the package's ABI.
-}

abiHash :: [(String, Maybe Phase)] -> Ghc ()
abiHash strs = do
  hsc_env <- getSession
  let dflags = hsc_dflags hsc_env

  liftIO $ do

  let find_it str = do
         let modname = mkModuleName str
         r <- findImportedModule hsc_env modname Nothing
         case r of
           Found _ m -> return m
           _error    -> throwGhcException $ CmdLineError $ showSDoc dflags $
#if __GLASGOW_HASKELL__ >= 802
                          cannotFindModule dflags modname r
#else
                          cannotFindInterface dflags modname r
#endif

  mods <- mapM find_it (map fst strs)

  let get_iface modl = loadUserInterface False (text "abiHash") modl
#if __GLASGOW_HASKELL__ >= 802
  ifaces <- initIfaceCheck (text "") hsc_env $ mapM get_iface mods
#else
  ifaces <- initIfaceCheck hsc_env $ mapM get_iface mods
#endif

  bh <- openBinMem (3*1024) -- just less than a block
  put_ bh hiVersion
    -- package hashes change when the compiler version changes (for now)
    -- see #5328
  mapM_ (put_ bh . mi_mod_hash) ifaces
  f <- fingerprintBinMem bh

  putStrLn (showPpr dflags f)

-- -----------------------------------------------------------------------------
-- Util

unknownFlagsErr :: [String] -> a
unknownFlagsErr fs = throwGhcException $ UsageError $ concatMap oneError fs
  where
    oneError f =
        "unrecognised flag: " ++ f ++ "\n" ++
        (case fuzzyMatch f (nub compat_allFlags) of
            [] -> ""
            suggs -> "did you mean one of:\n" ++ unlines (map ("  " ++) suggs))

{- Note [-Bsymbolic and hooks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-Bsymbolic is a flag that prevents the binding of references to global
symbols to symbols outside the shared library being compiled (see `man
ld`). When dynamically linking, we don't use -Bsymbolic on the RTS
package: that is because we want hooks to be overridden by the user,
we don't want to constrain them to the RTS package.

Unfortunately this seems to have broken somehow on OS X: as a result,
defaultHooks (in hschooks.c) is not called, which does not initialize
the GC stats. As a result, this breaks things like `:set +s` in GHCi
(#8754). As a hacky workaround, we instead call 'defaultHooks'
directly to initalize the flags in the RTS.

A biproduct of this, I believe, is that hooks are likely broken on OS
X when dynamically linking. But this probably doesn't affect most
people since we're linking GHC dynamically, but most things themselves
link statically.
-}

foreign import ccall safe "initGCStatistics"
  initGCStatistics :: IO ()

-- | Compatibility between GHC 7.8.2 -> GHC 7.8.3.
as :: Bool -> Phase
#if MIN_VERSION_ghc(7,8,3)
as = As
#else
as _ = As
#endif

compat_allFlags :: [String]
#if  __GLASGOW_HASKELL__ < 800
compat_allFlags = allFlags
#else
compat_allFlags = allNonDeprecatedFlags
#endif
