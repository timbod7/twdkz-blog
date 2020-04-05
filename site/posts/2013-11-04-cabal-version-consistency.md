---
title: "Cabal version consistency"
author: Tim Docker
date: 2013-11-04
tags: [programming]
---
Thanks to some great work done over the google summer of code, the
[chart library](https://github.com/timbod7/haskell-chart/wiki) has
gained much new functionality over the last 6 months. A consequence of
this is that it has gained plenty of dependencies on other software.
Furthermore, where the library previously had 2 cabal files to build the
system, it now has 4. It's important the the versioning of dependencies
is consistent across these cabal files, but manually checking is
tedious. As best I could tell there is not yet a tool to facilitate
this.

Hence, I spend a little time learning about the cabal API, and wrote a
short script that:

1.  reads several cabal files specified on the command line
2.  merges these into one overall set of dependencies
3.  displays the depencies in such a way that inconsistent version
    constrains are obvious

Here's some example output:

    $ runghc ~/repos/merge-cabal-deps/mergeCabalDeps.hs `find . -name '*.cabal'`
    * loaded Chart-gtk-1.1
    * loaded Chart-1.1
    * loaded Chart-tests-1.1
    * loaded Chart-cairo-1.1
    * loaded Chart-diagrams-1.1
    Chart:
        >=1.1 && <1.2 (Chart-cairo,Chart-diagrams,Chart-gtk,Chart-tests)
    Chart-cairo:
        >=1.1 && <1.2 (Chart-gtk,Chart-tests)
    Chart-diagrams:
        >=1.1 && <1.2 (Chart-tests)
    Chart-gtk:
        >=1.1 && <1.2 (Chart-tests)
    SVGFonts:
        >=1.4 && <1.5 (Chart-diagrams)
    array:
        -any (Chart,Chart-cairo,Chart-gtk,Chart-tests)
    base:
        >=3 && <5 (Chart,Chart-cairo,Chart-diagrams,Chart-gtk,Chart-tests)
    blaze-svg:
        >=0.3.3 (Chart-diagrams,Chart-tests)
    bytestring:
        >=0.9 && <1.0 (Chart-diagrams,Chart-tests)
    cairo:
        >=0.9.11 (Chart-cairo,Chart-gtk,Chart-tests)
    colour:
        >=2.2.0 (Chart-diagrams)
        >=2.2.1 && <2.4 (Chart,Chart-cairo,Chart-gtk,Chart-tests)
    containers:
        >=0.4 && <0.6 (Chart-diagrams,Chart-tests)
    data-default-class:
        <0.1 (Chart,Chart-cairo,Chart-diagrams,Chart-tests)
    diagrams-cairo:
        >=0.7 && <0.8 (Chart-tests)
    diagrams-core:
        >=0.7 && <0.8 (Chart-diagrams,Chart-tests)
    diagrams-lib:
        >=0.7 && <0.8 (Chart-diagrams,Chart-tests)
    ...
    $ 

As should be evident, all of the imported cabal packages are referenced
with consistent version constraints except for colour (which is lacking
an upper bound in Chart-diagrams).

The script is pretty straightforward:

    import Control.Monad
    import Data.List(intercalate)
    import System.Environment(getArgs)

    import qualified Data.Map as Map
    import qualified Data.Set as Set

    import Distribution.Package
    import Distribution.Version
    import Distribution.Verbosity
    import Distribution.Text(display)
    import Distribution.PackageDescription
    import Distribution.PackageDescription.Parse
    import Distribution.PackageDescription.Configuration

    type VersionRangeS = String

    type DependencyMap = Map.Map PackageName (Map.Map VersionRangeS (Set.Set PackageName))

    getDependencyMap :: PackageDescription -> DependencyMap
    getDependencyMap pd = foldr f Map.empty (buildDepends pd)
      where
        f :: Dependency -> DependencyMap  -> DependencyMap
        f (Dependency p vr) = Map.insert p (Map.singleton (display vr) (Set.singleton (pkgName (package pd))))

    printMergedDependencies :: [PackageDescription] -> IO ()
    printMergedDependencies pds = do
      forM_ (Map.toList dmap) $ \(pn,versions) -> do
        putStrLn (display pn ++ ":")
        forM_ (Map.toList versions) $ \(version,pnset) -> do
           putStrLn ("    " ++ version ++ " (" ++ intercalate "," (map display (Set.toList pnset)) ++ ")")
      where
        dmap :: DependencyMap
        dmap = Map.unionsWith (Map.unionWith Set.union) (map getDependencyMap pds)

    scanPackages :: [FilePath] -> IO ()
    scanPackages fpaths = do
        pds <- mapM loadPackageDescription fpaths
        printMergedDependencies pds
      where
        loadPackageDescription path = do
          pd <- fmap flattenPackageDescription (readPackageDescription silent path)
          putStrLn ("* loaded " ++ display (package pd))
          return pd

    main = getArgs >>= scanPackages      

I'd be interested in other tools used for managing suites of cabal
configurations.
