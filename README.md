fantasyinternet.poem
====================
Fantasy Internet library for Poetry.

Setup
-----
    $ npm init
    $ npm i -D poetry-compiler fantasyinternet.poem

Code
----
File `boot.poem` :

    include "fantasyinternet.poem/display.poem"

    export "init" init
      set_display_mode 0 80 20
      print "Hello Fantasy Internet!"

Compile
-------
    $ npx poetry boot.poem -b boot.wasm
    $ [/path/to/]cyberterminal .

See [wiki](https://github.com/FantasyInternet/fantasyinternet.poem/wiki) for more examples and documentation.
