# Sequence-Database

Starter [Pux](https://github.com/alexmingoia/purescript-pux/) application
using webpack with hot-reloading and
time-travel debug using
[pux-devtool](https://github.com/alexmingoia/pux-devtool).

See the [Guide](https://alexmingoia.github.io/purescript-pux) for help learning
Pux.
## With Electron
```sh
npm run build
cp -r dist/ ../purescript-electron-quickstart
cd ../purescript-electron-quickstart
# edit src/Main.purs to use path to index.html ala `mainWindow `loadURL` "file:///abs/path/purescript-electron-quickstart/dist/index.html"`
# edit dist/index.html's routes to app.css and js file routes to be relative (/ -> ./)
npm start # also can be opened `firefox dist/index.html`
```

### TODO
* Keep the `Host` Enum or just use `hostString`?
* changing Seq.State is expensive
* Data normalization -- given a list of strings that may be missing e.g. `Segment`, do your best to parse
* Use `readFile` somehow
* Port firefox readFile?
* It looks like the autocomplete via `<datalist>` doesn't work in electron :(
* Electron-pux-starter-app with readFile
* Download should only download checked choices
* fix toggle-all checkbox to toggle all on (unless all are already on, in which case toggle all off)
* Fix CSV and Fasta download formatting
* Reorganize components
* Reorganize code
* Test parsing (serialize -> deserialize?)
* Test querying

![Pux starter app animation](support/pux-starter-app.gif)

## Installation

```sh
git clone git://github.com/alexmingoia/pux-starter-app.git example
cd example
npm install
npm start
```

Visit `http://localhost:3000` in your browser, edit `src/purs/Layout.purs`
and watch the magic!

## Available scripts

### watch

`npm start` will start a development server, which hot-reloads your
application when sources changes.

### serve

`npm run serve` serves your application without watching for changes or hot-reloading. For
production run `npm run serve:prod`.

### build

`npm run build` bundles and minifies your application to run in production mode.
