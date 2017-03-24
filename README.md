Kakuro in [Elm](http://elm-lang.org/).

It's live at [kakuro-dojo.com](https://kakuro-dojo.com/).

Links to the iOS and Android app stores are at [GibGoyGames.com](https://gibgoygames.com/).

Code is in the `src` directory, scripts in the `bin` directory.

Due to its port, which it uses to save game state, you cannot develop with `elm-reactor`.

`bin/build-site` compiles the application to `site/kakuro.js`.

Then you can aim your browser at file:///.../kakuro-master/site/index.html.

`bin/update-site` builds the application and syncs the `site` directory with the live site.

`bin/m file` compiles `src/file.elm`, directing output to `/dev/null`. Useful for syntax checking while editing.

`bin/docs` generates `documentation.json`, suitable for upload to [package.elm-lang.org/help/docs-preview](http://package.elm-lang.org/help/docs-preview).

`bin/rsyncit` is a synchronization script I've been using for years. It is documented [here](https://steemit.com/hacking/@billstclair/rsyncit).

I didn't bother to write Windows scripts, nor will the symbolic link from `src/images` to `site/images` work on Windows. Use Linux or macOS.

See the readme in the [cordova](cordova/) directory for building the app for iOS and Android devices.

Articles at <a href='https://steemit.com/created/kakuro-master'>steemit.com/created/kakuro-master</a>.
