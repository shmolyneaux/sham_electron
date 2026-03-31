# SHAM

SHAM is a desktop asset management application built with [Electron](https://www.electronjs.org/) and [Elm](https://elm-lang.org/). It provides a UI for browsing, searching, uploading, and tagging assets served by a local backend on `localhost:8000`. The frontend uses [elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/) for layout and includes custom HTML elements for previewing text and image assets.

## Building and Running

Prerequisites: [Node.js](https://nodejs.org/), [Elm 0.19.1](https://guide.elm-lang.org/install/elm.html)

```bash
# Install dependencies
npm install

# Compile the Elm frontend
elm make src/Main.elm --output=elm.js

# Start the Electron app
npm start
```

For live-reloading during development, run `./live_reload.js` in a separate terminal. It watches `src/` for `.elm` file changes and automatically recompiles `elm.js`. The Electron app also watches for `.js` and `.html` changes and reloads the window automatically.

## Features

- **Asset browsing** — lists assets fetched from a local server with an outline view showing asset names and associated tags
- **Tag management** — create, assign, and remove key/value tags on assets; tags are displayed as colored chits in the asset outline
- **Asset preview** — custom `<asset-preview>` web component renders text and image previews based on MIME type
- **File upload** — select and upload files to the backend server
- **Search and Upload tabs** — tabbed interface to switch between searching/browsing assets and uploading new ones
- **Info panel** — displays details and tags for a selected asset, with controls to add or remove tags
- **Frameless window** — custom draggable title bar styled to match the dark UI theme

## Limitations

- Requires a separate backend server running on `http://localhost:8000` (not included in this repository)
- The live-reload script invokes Elm via Python as a workaround for shell issues on Cygwin
- No automated tests are included
- Uses an older version of Electron (8.x)

## History

Development on SHAM started 2020-04-25, and largely stopped on 2021-11-20:

- 2020-04-25 — Initial commit with Electron scaffold, elm-ui layout, and live-reload watcher
- 2020-05-02 — Added Elm Architecture (`Browser.element`), HTTP requests, search field, and basic app structure
- 2021-11-20 — Major feature update: asset/tag CRUD operations, file upload, tabbed UI (Search/Upload), asset preview via custom web component, info panel with tag management, frameless window with custom title bar, and additional Elm dependencies (`elm/json`, `elm/file`, `elm/bytes`, `remotedata`, `dict-extra`)
