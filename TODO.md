List of tasks remaining for [Kakuro-Dojo.com](https:/Kakuro-Dojo.com/).

1. Create encoders and decoders for stored JSON state. [**done**]
2. Switch backend to save/restore strings, and use the encoders/decoders on the Elm side. Use [billstclair/elm-versioned-json](http://package.elm-lang.org/packages/billstclair/elm-versioned-json/latest) to make it easy to change the state in the future. [**done**]
3. Integrate [billstclair/elm-dynamodb](http://package.elm-lang.org/packages/billstclair/elm-dynamodb/latest), to share state across browsers/devices. [**won't do**]
4. Minimize the home page, with links to go to Rules, Tutorial, Links. [**done**]
5. Write Rules. [**done**]
6. Write Tutorial, with screen shots showing the solution of an easy and harder puzzle. [**done**]
7. Wrap the whole thing in an iOS app, likely with [Apache Cordova](https://cordova.apache.org/), [Adobe PhoneGap](http://phonegap.com/), or [Ionic](http://ionicframework.com/). [See [Native Apps Are Doomed](https://medium.com/javascript-scene/native-apps-are-doomed-ac397148a2c0#.48qr70u0a)]
  1. "?" button -> link on second line. [**done**]
  2. "X" button offers "Clear All", with confirmation. [**done**]
  3. "Board Number" becomes link to dialog to type it. [**done**]
  4. Remember current board for each of 6x6, 8x8, and 10x10. [**done**]
  5. Apache Cordova logo link at bottom of help pages. [**done**]
  6. Fix app icon on switch apps screen (double-tab home button). [**done**]
  7. Force portrait orientation. [**done**]
  8. Link to GibGoyGames.com with GibGoyGames@gmail.com email. 
  9. "Credits" page.
  10. Eraser icon instead of blank square on keypad.
  11. App Store screen shots and text.
  12. Consider offering it for free with five layouts for each size (or just for the 6x6 size), and adding the rest as a $1.00 in-app purchase. See [Alex Disler's article](https://alexdisler.com/2016/02/29/in-app-purchases-ionic-cordova/) about his Cordova plugin.
8. Profit!
9. Wrap in Android app and add to Google Play App Store.
10. Profit more!
11. Add user-settable styles, allowing selection of colors.
12. Add iCloud and Android persistence of state.
13. Add my original idea, trial points. Some of the advanced 6x6 and 8x8 boards will be easier to solve with this.
14. Profit even more!

Nits:

1. Would be nice to use the fixed "keyboard" for typing the board number.
