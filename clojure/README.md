Advent of code
==============

In Clojure, using boot

Running
-------

    ./run.sh dev

To start the program.

Boot is set up to autorelead the code near instantly on any changes. This enables a very quick feedback loop.

An nrepl is also started, however it does not work great with the autoreload, so it is mostly useful for quick experimentation. Start with:

    ./run.sh repl

Even better is to connect in cursive and get autocomplete and other nice stuff.

Look in the script for more details.

Cursive
-------

Cursives boot support seems a bit quirky, the hack serves as a workaround and is included in build.boot

https://github.com/boot-clj/boot/wiki/For-Cursive-Users
