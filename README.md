In this repository I keep a list of examples of common constructs
in Elm. These examples are stripped down to their bare minimum.

The reason behind that is, that some of the examples from the offical
guide are a bit tricky to get running.
When a chapter is embedded in other chapters the code is sometimes mixed
with other aspects, and when you're trying to use the example code it's 
not entirely clear which parts to take.

The examples are completely self sufficient and only rely on the `elm` executable.
To run an example change to the respective subfolder and:

```bash
$ elm make src/Main.elm --output=elm.js
```

Then open the `index.html` in that folder.

## List of examples ##

  * [Incoming ports](port_incoming) - How to send information from JavaScript to Elm
  * [Outgoing ports](port_outgoing) - How to send information from Elm to JavaScript
  * [Component](component) - How to encapsulate UI functionality into reusable components
