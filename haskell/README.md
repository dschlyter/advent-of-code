Tooling

* Stack
* VS Code
* Haskell Simple GHC Integration

The docs:

* http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
* http://cheatsheet.codeslower.com/CheatSheet.pdf

Packages needed
---------------

This not best practice but I don't have time to set up haskells broken tooling properly

    stack install containers

Use [hoogle](https://www.haskell.org/hoogle/) to find and install what you dont have.

How to run
----------

    cd src
    stack runhaskell 2018/day1.hs

REPL usage
----------

Use the REPL from VS Code

cmd-j to focus terminal (hacky)
cmd-1 to return to main code window

    cd src/2018
    stack ghci --ghci-options day1.hs
    :set +t
    -- *do changes in file*
    -- reload
    :r 
    main

Use `set +s` to time functions

Play around with the input
--------------------------

    l <- load
    -- play around with l