Build: 
```
    clear; elm make .\web\play\Main.elm --output=web/play/Main.js
```
Then open index.html either directly or through a server. Example:
    python -m http.server -d web


Pain points currently:
    - Build has to be run manually
        - Possibly helpful: https://github.com/wking-io/elm-live
    - Static analysis reports fake errors
        - May need to switch to a different extension?


Historical info:
    We were using `elm reactor` to run the server, but the python server has the benefit of being able to specify the base path.