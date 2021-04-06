{-# LANGUAGE GADTs, RankNTypes #-}
module Clash.Clashilator.Cabal
    ( BuildHook
    , ComponentHook
    , withComponentHook
    , packageDBs
    ) where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildTarget
import Distribution.Simple.Setup
import Distribution.Simple.Register
import Distribution.Types.ComponentRequestedSpec

import Distribution.Types.Lens
import Control.Lens hiding ((<.>))
import Control.Monad (unless, when)

type BuildHook = PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
type ComponentHook = LocalBuildInfo -> BuildFlags -> Component -> IO BuildInfo

withComponentHook :: ComponentHook -> BuildHook -> BuildHook
withComponentHook componentHook nextBuildHook pkg lbi userHooks flags = do
    let reqSpec = componentEnabledSpec lbi
    withAllComponentsInBuildOrder pkg lbi $ \c clbi -> do
        flags <- return $ restrictBuildFlags pkg c flags
        when (componentEnabled reqSpec c && not (null $ buildArgs flags)) $ do
            bi <- componentHook lbi flags c
            pkg <- return $ updateBuildInfo c bi pkg
            nextBuildHook pkg lbi userHooks flags

packageDBs :: LocalBuildInfo -> BuildFlags -> IO [PackageDB]
packageDBs lbi flags = do
    pkgdb0 <- do
        let dbPath = internalPackageDBPath lbi distPref
        existsAlready <- doesPackageDBExist dbPath
        unless existsAlready $ do
            createPackageDB verbosity (compiler lbi) (withPrograms lbi) False dbPath
        return $ SpecificPackageDB dbPath
    pkgdbs <- absolutePackageDBPaths $ withPackageDB lbi

    return $ pkgdb0 : pkgdbs
  where
    verbosity = fromFlag (buildVerbosity flags)
    distPref  = fromFlag (buildDistPref flags)

data NamedComponent where
    NamedComponent
        :: (HasBuildInfo a)
        => Traversal' PackageDescription a
        -> (a -> ComponentName)
        -> NamedComponent

namedComponents :: [NamedComponent]
namedComponents =
    [ NamedComponent (library . each)      (CLibName . view libName)
    , NamedComponent (subLibraries . each) (CLibName . view libName)
    , NamedComponent (executables . each)  (CExeName . view exeName)
    , NamedComponent (testSuites . each)   (CTestName . view testName)
    , NamedComponent (benchmarks . each)   (CBenchName . view benchmarkName)
    ]

itagged :: Traversal' s a -> (a -> b) -> IndexedTraversal' b s a
itagged l f = reindexed f (l . selfIndex)

updateBuildInfo :: Component -> BuildInfo -> PackageDescription -> PackageDescription
updateBuildInfo c bi pkg = foldr ($) pkg $
    [ iover focus $ \ name -> if name == componentName c then const bi else id
    | NamedComponent component getName <- namedComponents
    , let focus = itagged component getName <. buildInfo
    ]

restrictBuildFlags :: PackageDescription -> Component -> BuildFlags -> BuildFlags
restrictBuildFlags pkg c buildFlags = buildFlags
    { buildArgs = selectedArgs
    }
  where
    selectedArgs = [showBuildTarget (packageId pkg) $ BuildTargetComponent $ componentName c]
