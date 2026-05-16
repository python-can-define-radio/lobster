## Running and building

To run the app,
activate and use [`package:webdev`](https://dart.dev/tools/webdev):

```
dart pub global activate webdev
webdev serve
```

To build a production version ready for deployment:

1. Run `$ webdev build`, which will create a `build` folder
2. Rename that `build` folder according to our version naming convention ("v1", "v2", ...)
3. Move the renamed folder to our deployment location, which is currently "python-can-define-radio.github.io" repo in "lobster" folder
