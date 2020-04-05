---
title: Accessing the cabal version from an application
author: Tim Docker
date: 2011-11-21
tags: [haskell]
---
I wanted the --version flag in an application to return the version from
the cabal file. Unable to find solution for this on the net, I ventured
into the darcs source code to for a solution. It's actually pretty easy:

Step 1
------

Change the Build-Type field in the cabal file to be "Custom". This means
cabal will look for a Setup.hs file to control the build.

Step 2
------

Create a Setup.hs that autogenerates a haskell module containing the
version number. Here's mine:

    import Distribution.Simple(defaultMainWithHooks, UserHooks(..), simpleUserHooks )
    import Distribution.Simple.Utils(rewriteFile)
    import Distribution.Package(packageVersion)
    import Distribution.Simple.BuildPaths(autogenModulesDir)
    import System.FilePath((</>))
    import Data.Version(showVersion)
    generateVersionModule pkg lbi = do
    let dir = autogenModulesDir lbi
    let version = packageVersion pkg

    rewriteFile (dir </> "Version.hs") $ unlines
    ["module Version where"
    ,"version :: String"
    ,"version = \"" ++ showVersion version ++ "\""
    ]

    myBuildHook pkg lbi hooks flags = do
    generateVersionModule pkg lbi
    buildHook simpleUserHooks pkg lbi hooks flags

    main = defaultMainWithHooks simpleUserHooks {
    buildHook=myBuildHook
    }

Step 3
------

Change your program to access the created Version module. It's actually
generated in the ./dist/build./autogen directory, but this seems to be
correctly on the source path by default.
