Kakuro in [Elm](http://elm-lang.org/).

Currently, the UI shows only a 10x10 board, with 66 layouts, selectable via the "Next" button. This will change. I have data for 8x8 and 10x10 boards.

It now shows the solution. Next I show the sums, and add the game play.

Code is in the ```src``` directory, scripts in the ```bin``` directory.

If you run ```elm-reactor``` in this directory, you can navigate to [```src/kakuro.elm```](src/kakuro.elm) for development.

```bin/build-site``` compiles the application to ```site/index.html```.

```bin/update-site``` builds the application and syncs the ```site``` directory with the live site.

"```bin/m file```" compiles ```src/file.elm```, directing output to ```/dev/null```. Useful for syntax checking while editing.

```bin/docs``` generates ```documentation.json```, suitable for upload to [package.elm-lang.org/help/docs-preview](http://package.elm-lang.org/help/docs-preview).

```bin/rsyncit``` is a synchronization script I've been using for years. It is documented [here](https://steemit.com/hacking/@billstclair/rsyncit).

I didn't bother to write Windows scripts, nor will the symbolic link from ```src/images``` to ```site/images``` work on Windows. Use Linux or macOS.

Articles at <a href='https://steemit.com/created/kakuro-master'>steemit.com/created/kakuro-master</a>.
