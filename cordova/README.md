# Apache Cordova wrapper for Kakuro Dojo

See [cordova.apache.org](https://cordova.apache.org/) for documentation.

Currently, iOS is the only supported platform, but adding Android should be very easy, once I get the iOS app deployed.

Do once:

1. `cd .../`
2. `git clone git@github.com:billstclair/kakuro-master.git`
3. `npm install -g cordova`
4. `npm install -g ios-deploy`
5. `cd .../kakuro-master/cordova`
6. `./configure`            # Add platforms and plugins
7. `cordova requirements`   # Do whatever it tells you to complete.
8. `cordova run ios --list` # Do whenever you want to see the emulated device list

After that, you can test changes with:

1. `cd .../kakuro-master`
2. `bin/build-site`
3. `cd cordova`
4. `cordova run ios --emulator --target=iPad-Air-2`  # (`--emulator` is the default. `--target` defaults to `iPhone-SE`)
5. `./run` is a script for the `cordova run...` command above.
6. `./prepare` is a script for the `cordova prepare...` command.
  1. If you prefer, as I do, to do your compilation and running in Xcode, use this, and operate on `platforms/ios/Kakuro Dojo.xcodeproj` in Xcode.
  2. You'll need to set your development team in Xcode.
  3. This makes it easy to load the app onto a real device.
7. To debug the JavaScript from the simulator or an iOS device:
  1. In iOS Settings/Safari/Advanced, enable "Web Inspector".
  2. After starting the app, in Safari on your Mac, open "Develop/&lt;iOS device&gt;/Kakuro Dojo/Kakuro Dojo".
  3. If it fails before you can get there, try, from the `cordova` directory, `cordova serve`, aim your web browser at the printed location (`http://localhost:8000`), and click on `iOS`.

## Android notes

The `./configure` script in the `cordova` directory may fail for Android while executing `cordova plugin add cordova-plugin-device`. If it does, you need to edit `cordova/platforms/android/CordovaLib/build.gradle`, and add `jcenter()` to the repositories:

```
    repositories {
        mavenCentral()
        jcenter()
    }
```

Then retry `cordova plugin add cordova-plugin-device`.

Doing `cordova run android` tends to fail. I prefer to just use `./prepare android`, and then import the run `File/New/Import Project...` from Android Studio, choosing the `cordova/platforms/android` directory. Then build and run the project in Android Studio.

Some changes you make, followed by `./prepare android`, will require `Build/Clean Project` or `Build/Rebuild Project` in Android Studio. Others it will properly notice and do the right thing. I haven't figured out which is which.

You may also need to add `~/Library/Android/sdk/tools/bin` (in MacOS, or the equivalent in Linux) to your shell PATH.
