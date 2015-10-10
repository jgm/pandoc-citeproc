{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.InstallDirs (mandir)
import Distribution.PackageDescription (PackageDescription(..), Executable(..))
import Distribution.Simple.Program (simpleProgram, Program(..))
import Distribution.Simple.Utils ( rawSystemExitCode, findProgramVersion )
import Data.Version
import System.Exit
import System.Directory ( findExecutable )
import Distribution.Simple.Utils (info, notice, rawSystemExit, installOrdinaryFiles)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity
import qualified Control.Exception as E


main :: IO ()
main =
  defaultMainWithHooks $ simpleUserHooks {
      -- enable hsb2hs preprocessor for .hsb files
      hookedPreProcessors = [ppBlobSuffixHandler]
    , hookedPrograms = [(simpleProgram "hsb2hs"){
                           programFindVersion = \verbosity fp ->
                             findProgramVersion "--version" id verbosity fp }]
    , postCopy = installManPage
    }

installManPage :: Args -> CopyFlags
               -> PackageDescription -> LocalBuildInfo -> IO ()
installManPage _ flags pkg lbi = do
  let verbosity = fromFlag (copyVerbosity flags)
  let copydest  = fromFlag (copyDest flags)
  let mandest   = mandir (absoluteInstallDirs pkg lbi copydest)
                     ++ "/man1"
  notice verbosity $ "Copying man page to " ++ mandest
  installOrdinaryFiles verbosity mandest
     [("man/man1", "pandoc-citeproc.1")]

ppBlobSuffixHandler :: PPSuffixHandler
ppBlobSuffixHandler = ("hsb", \_ _ ->
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity ->
      do info verbosity $ "Preprocessing " ++ infile ++ " to " ++ outfile
         ec <- rawSystemExitCode verbosity "hsb2hs" [infile, infile, outfile]
         case ec of
              ExitSuccess   -> return ()
              ExitFailure _ -> error "hsb2hs is needed to build this program"
  })
