Tooling

* VS Code
* Haskell Simple GHC Integration

The docs:

* http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html
* http://cheatsheet.codeslower.com/CheatSheet.pdf

How to run
----------

    cd src/2018
    stack runhaskell day1.hs

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

Play around with the input
--------------------------

    l <- load
    -- play around with l