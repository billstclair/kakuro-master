# Apache Cordova wrapper for Kakuro Dojo

See [cordova.apache.org](https://cordova.apache.org/) for documentation.

Currently, iOS is the only supported platform, but adding Android should be very easy, once I get the iOS app deployed.

Do once:

{{{
cd .../
git clone git@github.com:billstclair/kakuro-master.git
}}}

1. `npm install -g cordova`
2. `npm install -g ios-deploy1`
3. `cd .../kakuro-master/cordova`
4. `cordova add platform ios`
5. `cordova requirements` and do whatever it tells you to complete.
6. `cordova run ios --list` -- Do whenever you want to see the emulated device list

After that, you can test changes with:

1. `cd .../kakuro-master`
2. `bin/build-site` (I haven't updated this yet for Cordova)
3. `cd cordova`
4. `cordova run ios --emulator --target=iPad-Air-2` (`--emulator` is the default. `--target` defaults to `iPhone-SE`)


