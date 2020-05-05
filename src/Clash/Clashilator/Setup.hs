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
import Distribution.ModuleName (fromString)

import Distribution.Types.Lens
import Control.Lens

import System.FilePath
import Development.Shake.Command

clashToVerilog :: LocalBuildInfo -> BuildFlags -> IO (FilePath, Manifest)
clashToVerilog localInfo buildFlags = do
    pkgdbs <- absolutePackageDBPaths $ withPackageDB localInfo
    let dbflags = concat [ ["-package-db", path] | SpecificPackageDB path <- pkgdbs ]

    let mod = "Hello" -- TODO

    let outDir = buildDir localInfo
        clashDir = "_clash-syn"

    Clash.defaultMain $
      [ "--verilog"
      , "-outputdir", outDir </> clashDir
      , "./src/" <> mod <> ".hs"
      ] ++ dbflags

    let verilogDir = clashDir </> "verilog" </> mod </> "topEntity"
    manifest <- read <$> readFile (outDir </> verilogDir </> "topEntity.manifest")

    return (verilogDir, manifest)

-- TODO: Should we also edit `Library` components?
buildVerilator :: LocalBuildInfo -> BuildFlags -> IO (Executable -> Executable)
buildVerilator localInfo buildFlags = do
    let outDir = buildDir localInfo
    (verilogDir, manifest) <- clashToVerilog localInfo buildFlags

    let verilatorDir = "_verilator"
    Clashilator.generateFiles (".." </> verilogDir) (outDir </> verilatorDir) manifest

    -- TODO: bake in `pkg-config --cflags verilator`
    () <- cmd (Cwd (outDir </> verilatorDir)) "make"

    let incDir = outDir </> verilatorDir </> "src"
        libDir = outDir </> verilatorDir </> "obj"
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

        fixupExe = foldr (.) id $
            [ includeDirs %~ (incDir:)
            , extraLibDirs %~ (libDir:)
            , options %~ fixupOptions (linkFlags++)

            , hsSourceDirs %~ (incDir:)
            , otherModules %~ (fromString lib:)
            ]

    return fixupExe

clashilate :: LocalBuildInfo -> BuildFlags -> IO (PackageDescription -> PackageDescription)
clashilate localInfo buildFlags = do
    fixupExe <- buildVerilator localInfo buildFlags

    return $ foldr (.) id $
        [ executables %~ map fixupExe
        ]
