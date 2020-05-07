module Clash.Clashilator.Setup
    ( clashToVerilog
    , buildVerilator
    , clashilate
    ) where

import qualified Clash.Main as Clash
import qualified Clash.Clashilator as Clashilator
import Clash.Driver.Types (Manifest)

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Compiler
import Distribution.Simple.Program
import Distribution.Verbosity
import Distribution.ModuleName (fromString)
import Distribution.Types.UnqualComponentName

import Distribution.Types.Lens
import Control.Lens
import Control.Monad (forM)
import Data.String (fromString)
import System.FilePath

clashToVerilog :: LocalBuildInfo -> BuildFlags -> [FilePath] -> String -> FilePath -> IO (FilePath, Manifest)
clashToVerilog localInfo buildFlags srcDirs mod outDir = do
    pkgdbs <- absolutePackageDBPaths $ withPackageDB localInfo
    let dbflags = concat [ ["-package-db", path] | SpecificPackageDB path <- pkgdbs ]
        iflags = [ "-i" <> dir | dir <- srcDirs ]

    Clash.defaultMain $ concat
      [ [ "--verilog"
        , "-outputdir", outDir
        , mod
        ]
      , iflags
      , dbflags
      ]

    let modDir = mod -- TODO: turn '.' into '/' or somesuch
        verilogDir = outDir </> "verilog" </> modDir </> "topEntity"
    manifest <- read <$> readFile (verilogDir </> "topEntity.manifest")

    return (verilogDir, manifest)

buildVerilator :: LocalBuildInfo -> BuildFlags -> Maybe UnqualComponentName -> BuildInfo -> IO BuildInfo
buildVerilator localInfo buildFlags compName buildInfo = case top of
    Nothing -> return buildInfo
    Just topEntityModule -> buildVerilator' localInfo buildFlags compName buildInfo topEntityModule
  where
    top = lookup "x-clashilator-top-is" $ view customFieldsBI buildInfo

buildVerilator' :: LocalBuildInfo -> BuildFlags -> Maybe UnqualComponentName -> BuildInfo -> String -> IO BuildInfo
buildVerilator' localInfo buildFlags compName buildInfo topEntityModule = do
    cflags <- do
        mpkgConfig <- needProgram verbosity pkgConfigProgram (withPrograms localInfo)
        case mpkgConfig of
            Nothing -> error "Cannot find pkg-config program"
            Just (pkgConfig, _) -> getProgramOutput verbosity pkgConfig ["--cflags", "verilator"]

    -- TODO: dependency tracking
    (verilogDir, manifest) <- clashToVerilog localInfo buildFlags srcDirs topEntityModule synDir
    Clashilator.generateFiles (Just cflags) verilogDir verilatorDir (fromString <$> clk) manifest

    -- TODO: get `make` location from configuration
    _ <- getProgramInvocationOutput verbosity $
         simpleProgramInvocation "make" ["-f", verilatorDir </> "Makefile"]

    let incDir = verilatorDir </> "src"
        libDir = verilatorDir </> "obj"
        lib = "VerilatorFFI"

    let fixupOptions f (PerCompilerFlavor x y) = PerCompilerFlavor (f x) (f y)

        linkFlags =
            [ "-fPIC"
            , "-pgml", "g++"
            , "-optl-Wl,--whole-archive"
            , "-optl-Wl,-Bstatic"
            , "-optl-Wl,-l" <> lib
            , "-optl-Wl,-Bdynamic"
            , "-optl-Wl,--no-whole-archive"
            ]

    return $ foldr ($) buildInfo $
      [ includeDirs %~ (incDir:)
      , extraLibDirs %~ (libDir:)
      , options %~ fixupOptions (linkFlags++)

      , hsSourceDirs %~ (incDir:)
      , otherModules %~ (fromString lib:)
      ]
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

clashilate :: PackageDescription -> LocalBuildInfo -> BuildFlags -> IO PackageDescription
clashilate pkg localInfo buildFlags = do
    -- TODO: there must be a Control.Lens-y way to do this more succintly...
    let exes = view executables pkg
    exes' <- forM exes $ \exe -> do
        buildInfo' <- buildVerilator localInfo buildFlags (Just $ view exeName exe) (view buildInfo exe)
        return $ exe & buildInfo .~ buildInfo'

    return $ pkg & executables .~ exes'
