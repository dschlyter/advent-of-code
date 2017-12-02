Advent of code
==============

In clojure, using boot

Running
-------

    ./run.sh dev
    
To start the program, with an nrepl server and autorefresh on file changes

    ./run.sh repl
    
To connect repl (optional, code will autorefresh and print output)

You need to switch namespace after connecting.

    (ns advent.advent-2017)

Even better is to connect in cursive and get autocomplete and other nice stuff.

Look in the script for more details.

Cursive
-------

Leiningens boot support seems a bit quirky, the hack serves as a workaround and is included in build.boot

https://github.com/boot-clj/boot/wiki/For-Cursive-Users

