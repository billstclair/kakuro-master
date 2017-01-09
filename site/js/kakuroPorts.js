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
  var propertiesName = 'kakuro-properties';

  kakuroPorts.init = init;
  kakuroPorts.storageName = storageName;
  kakuroPorts.propertiesName = propertiesName;
  kakuroPorts.setProperty = setProperty;
  kakuroPorts.getProperty = getProperty;

  function specHash(spec) {
    var hash = sha256(spec);      // Defined in sha256.js
    return hash.substring(0, 8);
  }

  function log (x) {
    console.log(x+"\n");
  }

  function getProperties() {
    var json = localStorage.getItem(propertiesName);
    return json ? JSON.parse(json) : {};
  }

  function setProperties(properties) {
    localStorage.setItem(propertiesName, JSON.stringify(properties));
  }

  // This isn't used by the Elm code, but is useful for debugging
  // in the JavaScript console.
  function getProperty(property) {
    var props = getProperties();
    return props[property];
  }

  function setProperty(property, value) {
    var props = getProperties();
    if (value == null) {
      delete props[property];
    } else {
      props[property] = value;
    }
    setProperties(props);
  }

  function init() {
    var storedState = localStorage.getItem(storageName);

    //log("storedState: " + storedState + "\n")

    var alist = [];
    var properties = getProperties()
    for (key in properties) {
      alist.push([key, properties[key]]);
    }
    var kakuro = Elm.Kakuro.fullscreen([app.isCordova(), alist, storedState]);
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

    kakuro.ports.setProperty.subscribe(function(pv) {
      setProperty(pv[0], pv[1])
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

    kakuro.ports.iapGetProducts.subscribe(function(pids) {
      app.iapGetProducts(pids, function(res) {
        if (typeof(res) == 'string') {
            res =  [null, res];
        } else if (typeof(res) == 'object') {
          var msg = res.errorMessage;
          if (msg) {
            var code = res.errorCode;
            if (code) {
              msg = msg + ", code: " + code;
            }
            res = [null, msg];
          } else {
            var prods = [];
            // Not really necessary, but avoids conversion-to-Elm runtime error,
            // if return value isn't the correct shape.
            for (var prod in res) {
              prod = res[prod];
              prods.push({ productId: prod.productId || "",
                           title: prod.title || "",
                           description: prod.description || "",
                           price: prod.price || "" })
            }
            res = [prods, null]
          }
        } else {
          res = [null, "Bad return from iapGetProducts: " + JSON.stringify(res)];
        }
        kakuro.ports.iapProducts.send(res);
      });
    });

    kakuro.ports.iapBuy.subscribe(function(pid) {
      app.iapBuy(pid, function(res) {
        res = (typeof(res) == 'string') ?
          [pid, null, res] :
          [pid, res.transactionId, null];
        kakuro.ports.iapBuyResponse.send(res);
      });
    });

    kakuro.ports.iapRestorePurchases.subscribe(function() {
      app.iapRestorePurchases(function(res) {
        if (typeof(res) == 'string') {
            res = [null, res];
        } else {
          // Must match IapPurchase in SharedTypes.elm
          var purchases = [];
          for (var purchase in res) {
            purchase = res[purchase];
            purchase = { productId: res.productId || "",
                         transactionId: res.transactionId || "",
                         date: res.date || 0
                       };
            purchases.push(purchase);
          }
          res = [purchases, null];
        }
        kakuro.ports.iapPurchases.send(res);
      });
    });

    app.registerDeviceReady(function() {
      kakuroPorts.registeredDeviceReady = true;
      kakuro.ports.deviceReady.send(true);
    });
  }

})();
