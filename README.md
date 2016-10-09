Kakuro in [Elm](http://elm-lang.org/).

There is no game play yet. I'm working on the board generation code, so the display shows the numbers in each square, not the sums of rows and columns. Read the articles linked at the bottom of this file for an idea of my intentions.

Code is in the ```src``` directory, scripts in the ```bin``` directory.

If you run ```elm-reactor``` in this directory, you can navigate to [```src/kakuro.elm```](src/kakuro.elm) for development.

```bin/build-site``` compiles the application to ```site/index.html```.

```bin/update-site``` builds the application and syncs the ```site``` directory with the live site.

"```bin/m file```" compiles ```src/file.elm```, directing output to ```/dev/null```. Useful for syntax checking while editing.

```bin/docs``` generates ```documentation.json```, suitable for upload to [package.elm-lang.org/help/docs-preview](http://package.elm-lang.org/help/docs-preview).

```bin/rsyncit``` is a synchronization script I've been using for years. It is documented [here](https://steemit.com/hacking/@billstclair/rsyncit).

The old JavaScript version is in the ```old``` directory. It is live at <a href='http://kakuro-master.com/'>kakuro-master.com</a>, until I get the generation code updated. Then it goes away and gets replaced with the Elm version.

I didn't bother to write Windows scripts, nor will the symbolic link from ```src/images``` to ```site/images``` work on Windows. Use Linux or macOS.

Articles at <a href='https://steemit.com/created/kakuro-master'>steemit.com/created/kakuro-master</a>.
