Build: 
```
    elm make .\src\Main.elm --output=build/Main.js
```
Then open index.html either directly or through the reactor server
    elm reactor


Pain points currently:
- Build has to be run manually
  - Possibly helpful: https://github.com/wking-io/elm-live
- Static analysis reports fake errors
  - May need to switch to a different extension?