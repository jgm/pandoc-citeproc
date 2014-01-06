{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
         (copyDest, copyVerbosity, fromFlag, installVerbosity, BuildFlags(..))
import Distribution.PackageDescription (PackageDescription(..), Executable(..))
import Distribution.Simple.LocalBuildInfo
         (LocalBuildInfo(..), absoluteInstallDirs)
import Distribution.Verbosity ( Verbosity, silent )
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.Utils (installOrdinaryFiles, info)
import Prelude hiding (catch)
import System.Process ( rawSystem )
import System.FilePath ( (</>) )
import System.Directory ( findExecutable )
import System.Exit

main :: IO ()
main = do
  defaultMainWithHooks $ simpleUserHooks {
      postCopy = \ _ flags pkg lbi ->
         installManpages pkg lbi (fromFlag $ copyVerbosity flags)
              (fromFlag $ copyDest flags)
    , postInst = \ _ flags pkg lbi ->
         installManpages pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest
    , hookedPreProcessors = [ppBlobSuffixHandler]
    }
  exitWith ExitSuccess

manpages :: [FilePath]
manpages = ["man1" </> "pandoc-citeproc.1"]

manDir :: FilePath
manDir = "man"

installManpages :: PackageDescription -> LocalBuildInfo
                -> Verbosity -> CopyDest -> IO ()
installManpages pkg lbi verbosity copy =
  installOrdinaryFiles verbosity (mandir (absoluteInstallDirs pkg lbi copy))
             (zip (repeat manDir) manpages)

ppBlobSuffixHandler :: PPSuffixHandler
ppBlobSuffixHandler = ("hsb", \_ _ ->
  PreProcessor {
    platformIndependent = True,
    runPreProcessor = mkSimplePreProcessor $ \infile outfile verbosity ->
      do info verbosity $ "Preprocessing " ++ infile ++ " to " ++ outfile
         hsb2hsPath <- findExecutable "hsb2hs"
         case hsb2hsPath of
            Just p  -> rawSystem p [infile, infile, outfile]
            Nothing -> error "hsb2hs is needed to build this program: cabal install hsb2hs"
         return ()

  })
