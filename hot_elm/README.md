This folder brings a little boilerplate setup to get you jumpstarted with a local Elm dev setup without a server or external dependencies.
Just execute `bin/build.sh`, open the `index.html` and modify the `src/Main.elm` - you'll get instant feedback in your browser window.

The only requirement is that you [install Elm](https://guide.elm-lang.org/install/elm.html) itself first.

**Tested only under MacOS**

The main file is the `bin/build.sh`. Executing will start watching for file changes in the `src` folder and recompile the Elm app.

The `index.html` will embed the Elm app indirectly like described in the 
[Elm guide](https://guide.elm-lang.org/interop#compiling-to-javascript). It's however not included directly, but through the `assets/loader.js`.
This vanilla Javascript file polls the `tmp/timestamp.js` to see if the build has changed. If it has changed it will reload the Elm app or display build errors if there were any.

![How it works](hotelm.gif)
