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


  function log (x) {
    console.log(x+"\n");
  }

  function attachFastClick() {
    // https://github.com/ftlabs/fastclick/blob/master/README.md
    if (Origami && CustomEvent) { // old browsers don't support CustomEvent
      var attachFastClick = Origami.fastclick;
      if (attachFastClick) {
        attachFastClick(document.body);
      }
    }
  }

  function init() {
    attachFastClick();
    var storedState = localStorage.getItem(storageName);

    log("storedState: " + storedState + "\n")

    var alist = [];

    // The platform isn't valid in Cordova until device ready
    var kakuro = Elm.Kakuro.init({
        node: document.getElementById("elm-node"),
        flags: [app.platform(), alist, storedState]
    });
    kakuroPorts.kakuro = kakuro;


    kakuro.ports.setStorage.subscribe(function(json) {
      //log("setStorage: " + json + "\n")
      if (json === null) {
        localStorage.clear();        // Bye,  bye, birdy.
      } else {
        localStorage.setItem(storageName, json);
      }
    });

    kakuro.ports.setTitle.subscribe(function(title) {
      document.title = title;
    });

    kakuro.ports.saveGame.subscribe(function(specAndState) {
      var spec = specAndState[0]
      var json = specAndState[1]
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

    kakuro.ports.promptDialog.subscribe(function(qd) {
      var question = qd[0];
      var deflt = qd[1];
      var answer = window.prompt(question, deflt);
      if (!(answer === null)) {
        kakuro.ports.promptAnswer.send([question, answer]);
      }
//      app.promptDialog(question, default, function(answer) {
//        kakuro.ports.promptAnswer.send([question, answer]);
//      });
    });

    kakuro.ports.makeClickSound.subscribe(function() {
      app.makeClickSound();
    });

    app.registerDeviceReady(function() {
      var platform = app.platform();
      kakuroPorts.registeredDeviceReady = true;
      kakuroPorts.platform = platform;
      // The platform is now valid in Cordova
      kakuro.ports.deviceReady.send(platform)
    });
  }

})();
