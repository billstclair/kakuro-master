//////////////////////////////////////////////////////////////////////
//
// kakuroPorts.js
// JavaScript side of ports from kakuro.elm
// Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

var kakuroPorts = {};

(function () {

  var storageName = 'kakuro-dojo';

  kakuroPorts.init = init;
  kakuroPorts.storageName = storageName;

  function specHash(spec) {
    var hash = sha256(spec);      // Defined in sha256.js
    return hash.substring(0, 8);
  }

  function log (x) {
    console.log(x+"\n");
  }

  function init() {
    var storedState = localStorage.getItem(storageName);

    //log("storedState: " + storedState + "\n")

    var kakuro = Elm.Kakuro.fullscreen([app.isCordova(), storedState]);
    kakuroPorts.kakuro = kakuro

    kakuro.ports.setStorage.subscribe(function(json) {
      //log("setStorage: " + json + "\n")
      localStorage.setItem(storageName, json);
    });

    kakuro.ports.setTitle.subscribe(function(title) {
      document.title = title;
    })

    kakuro.ports.saveGame.subscribe(function(specAndState) {
      var spec = specAndState[0]
      var json = specAndState[1]
      var hash = specHash(spec);
      //log("Saving: " + hash + ", from: " + spec + " as: " + json);
      localStorage.setItem(hash, json);
    });

    kakuro.ports.requestGame.subscribe(function(spec) {
      var hash = specHash(spec);
      var json = localStorage.getItem(hash);
      //log ("Restored: " + hash + " from: " + spec + " as: " + json);
      kakuro.ports.receiveGame.send(json);
    });

    kakuro.ports.confirmDialog.subscribe(function(query) {
      app.confirm(query, function(answer) {
        kakuro.ports.confirmAnswer.send([query, answer]);
      });
    });

    kakuro.ports.multiConfirmDialog.subscribe(function(mtr) {
      var message = mtr[0];
      var title = mtr[1];
      var responses = mtr[2];
      app.multiConfirm(message, title, responses, function(answer) {
        kakuro.ports.multiConfirmAnswer.send([message, answer]);
      });
    });
  }

})();
