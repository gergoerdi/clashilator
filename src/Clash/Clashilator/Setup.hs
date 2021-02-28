{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
module Clash.Clashilator.Setup
    ( clashToVerilog
    , buildVerilator
    , clashilate

    , clashilatorMain
    , clashilatorBuildHook
    ) where

import qualified Clash.Main as Clash
import qualified Clash.Clashilator as Clashilator
import Clash.Driver.Types (Manifest)

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Simple.Utils (infoNoWrap)
import Distribution.Verbosity
import Distribution.ModuleName
import Distribution.Types.UnqualComponentName

import Distribution.Types.Lens
import Control.Lens hiding ((<.>))
import Control.Monad (forM, foldM)
import Data.String (fromString)
import Data.List (intercalate)
import Data.Maybe (maybeToList, fromMaybe)
import System.FilePath

lookupX :: String -> BuildInfo -> Maybe String
lookupX key buildInfo = lookup ("x-clashilator-" <> key) (view customFieldsBI buildInfo)

clashToVerilog :: LocalBuildInfo -> BuildFlags -> [FilePath] -> BuildInfo -> ModuleName -> String -> FilePath -> IO (FilePath, Manifest)
clashToVerilog localInfo buildFlags srcDirs buildInfo mod entity outDir = do
    pkgdbs <- absolutePackageDBPaths $ withPackageDB localInfo
    let dbflags = concat [ ["-package-db", path] | SpecificPackageDB path <- pkgdbs ]
        iflags = [ "-i" <> dir | dir <- srcDirs ]
        clashflags = maybe [] words $ lookupX "clash-flags" buildInfo

    let args = concat
            [ [ "--verilog"
              , "-outputdir", outDir
              , "-main-is", entity
              , intercalate "." (components mod)
              ]
            , iflags
            , dbflags
            , clashflags
            ]
    infoNoWrap verbosity $ unwords $ "Clash.defaultMain" : args
    Clash.defaultMain args

    let (modDir:_) = components mod
        verilogDir = outDir </> "verilog" </> modDir </> entity
    manifest <- read <$> readFile (verilogDir </> entity <.> "manifest")

    return (verilogDir, manifest)
  where
    verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)

buildVerilator :: LocalBuildInfo -> BuildFlags -> Maybe UnqualComponentName -> BuildInfo -> IO BuildInfo
buildVerilator localInfo buildFlags compName buildInfo = case top of
    Nothing -> return buildInfo
    Just mod -> buildVerilator' localInfo buildFlags compName buildInfo (fromString mod) entity
  where
    top = lookupX "top-is" buildInfo
    entity = fromMaybe "topEntity" $ lookupX "entity" buildInfo

buildVerilator' :: LocalBuildInfo -> BuildFlags -> Maybe UnqualComponentName -> BuildInfo -> ModuleName -> String -> IO BuildInfo
buildVerilator' localInfo buildFlags compName buildInfo mod entity = do
    cflags <- do
        mpkgConfig <- needProgram verbosity pkgConfigProgram (withPrograms localInfo)
        case mpkgConfig of
            Nothing -> error "Cannot find pkg-config program"
            Just (pkgConfig, _) -> getProgramOutput verbosity pkgConfig ["--cflags", "verilator"]

    -- TODO: dependency tracking
    (verilogDir, manifest) <- clashToVerilog localInfo buildFlags srcDirs buildInfo mod entity synDir
    Clashilator.generateFiles (Just cflags) verilogDir verilatorDir (fromString <$> clk) manifest

    -- TODO: get `make` location from configuration
    _ <- getProgramInvocationOutput verbosity $
         simpleProgramInvocation "make" ["-f", verilatorDir </> "Makefile"]

    let incDir = verilatorDir </> "src"
        libDir = verilatorDir </> "obj"
        lib = "VerilatorFFI"

    let fixupOptions f (PerCompilerFlavor x y) = PerCompilerFlavor (f x) (f y)

        compileFlags =
            [ "-fPIC"
            , "-pgml c++"
            ]

        ldFlags =
            [ "-Wl,--whole-archive"
            , "-Wl,-Bstatic"
            , "-Wl,-l" <> lib
            , "-Wl,-Bdynamic"
            , "-Wl,--no-whole-archive"
            ]

    return $ buildInfo
      & includeDirs %~ (incDir:)
      & extraLibDirs %~ (libDir:)
      & extraLibs %~ ("stdc++":)
      & options %~ fixupOptions (compileFlags++)
      & ldOptions %~ (ldFlags++)
      & hsSourceDirs %~ (incDir:)
      & otherModules %~ (fromComponents ["Clash", "Clashilator", "FFI"]:)
  where
    verbosity = fromFlagOrDefault normal (buildVerbosity buildFlags)

    clk = lookup "x-clashilator-clock" $ view customFieldsBI buildInfo

    -- TODO: Maybe we could add extra source dirs from "x-clashilator-source-dirs"?
    srcDirs = view hsSourceDirs buildInfo
    outDir = case compName of
        Nothing -> buildDir localInfo
        Just name -> buildDir localInfo </> unUnqualComponentName name
    verilatorDir = outDir </> "_clashilator" </> "verilator"
    synDir = outDir </> "_clashilator" </> "clash-syn"

data Clashilatable where
    Clashilatable
        :: (HasBuildInfo a)
        => Traversal' PackageDescription a
        -> (a -> Maybe UnqualComponentName)
        -> Clashilatable

clashilatables :: [Clashilatable]
clashilatables =
    [ Clashilatable (executables . each) (Just . view exeName)
    , Clashilatable (library . each)     (const Nothing)
    , Clashilatable (testSuites . each)  (Just . view testName)
    , Clashilatable (benchmarks . each)  (Just . view benchmarkName)
    ]

itagged :: Traversal' s a -> (a -> b) -> IndexedTraversal' b s a
itagged l f = reindexed f (l . selfIndex)

clashilate :: PackageDescription -> LocalBuildInfo -> BuildFlags -> IO PackageDescription
clashilate pkg localInfo buildFlags =
    foldM (&) pkg $
      [ itraverseOf focus $ buildVerilator localInfo buildFlags
      | Clashilatable component getName <- clashilatables
      , let focus = itagged component getName <. buildInfo
      ]

clashilatorMain :: IO ()
clashilatorMain = defaultMainWithHooks simpleUserHooks
    { buildHook = clashilatorBuildHook
    }

clashilatorBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
clashilatorBuildHook pkg localInfo userHooks buildFlags = do
    pkg' <- clashilate pkg localInfo buildFlags
    buildHook simpleUserHooks pkg' localInfo userHooks buildFlags
