{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.Utils (notice, installOrdinaryFiles)
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo


main :: IO ()
main =
  defaultMainWithHooks $ simpleUserHooks {
      postCopy = installManPage
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

