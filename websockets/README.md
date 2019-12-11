In the `server` directory is the source code for a simple echo server in Haskell.

It's based on https://jaspervdj.be/websockets/

To run it [install stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
and then execute `stack run` in the `server` subdir.
This will start the echo server on `ws://127.0.0.1:9160` which is the default address in the elm client too.
