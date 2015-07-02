{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.Simple.InstallDirs (mandir)
import Distribution.PackageDescription (PackageDescription(..), Executable(..))
import System.Process ( rawSystem )
import System.FilePath ( (</>) )
import System.Directory ( findExecutable )
import Distribution.Simple.Utils (info, notice, rawSystemExit, installOrdinaryFiles)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Verbosity


main :: IO ()
main =
  defaultMainWithHooks $ simpleUserHooks {
      -- enable hsb2hs preprocessor for .hsb files
      hookedPreProcessors = [ppBlobSuffixHandler]
    , postCopy = installManPage
    }

installManPage :: Args -> CopyFlags
               -> PackageDescription -> LocalBuildInfo -> IO ()
installManPage _ flags pkg lbi = do
  let verbosity = fromFlag (copyVerbosity flags)
  let copydest  = fromFlag (copyDest flags)
  let mandest   = mandir (absoluteInstallDirs pkg lbi copydest)
                     </> "man1"
  notice verbosity $ "Copying man page to " ++ mandest
  installOrdinaryFiles verbosity mandest
     [("man" </> "man1", "pandoc-citeproc.1")]

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
