/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

var app = {
    receivedDeviceReady: false,
    deviceReadyCallback: null,

    // Application Constructor
    initialize: function() {
        document.addEventListener('deviceready', this.onDeviceReady.bind(this), false);
    },

    registerDeviceReady: function(callback) {
      app.deviceReadyCallback = callback;
      if (app.receivedDeviceReady) {
        callback();
      }
    },

    // deviceready Event Handler
    //
    // Bind any cordova events here. Common events are:
    // 'pause', 'resume', etc.
    onDeviceReady: function() {
      app.receivedDeviceReady = true;
      if (app.deviceReadyCallback) {
        app.deviceReadyCallback();
      }
    },

    confirm: function(query, callback) {
      navigator.notification.confirm(query, function(idx) {
        callback(idx == 1);
      });
    },

    multiConfirm: function(message, title, responses, callback) {
      navigator.notification.confirm(message,
                                     function(idx) {
                                       callback(idx-1);
                                     },
                                     title, responses);
    },

    platform: function() {
      var defaultDevice = 'iOS';
      if (typeof(device) == 'object') {
        return device.platform || defaultDevice;
      } else {
        return defaultDevice;
      }
    },
  
  iapGetProducts: function(pids, callback) {
    var timeout = false;
    var toFun = function() {
      timeout = false;
      var prod = { productId: pids[0],
                   title: 'Additional Puzzles',
                   description: 'Add 190 puzzles, split between 6x6, 8x8, and 10x10 layouts.',
                   price: "$0.99 (US)"
                 };
      callback([prod]);
    }, 
    timeout = window.setTimeout(toFun, 1000);
    var done = function(res) {
      var to = timeout;
      if (to) {
        window.clearTimeout(to);
      }
      // Protect against empty return
      if (typeof(res) == 'object' && (res.length === undefined || res.length == 0)) {
        toFun();
      } else {
        callback(res);
      }
    };
    inAppPurchase
      .getProducts(pids)
      .then(done)
      .catch(done);
  },

  restoredPurchases: false,

  iapBuy: function(pid, callback) {
    if (app.restoredPurchases) {
      inAppPurchase
        .buy(pid)
        .then(callback)
        .catch(callback);
    } else {
      app.iapRestorePurchases(function(res) {
        app.restoredPurchases = true;
        if (typeof(res) == 'object' && !res.errorMessage) {
          var tid = null;
          for (var purchase in res) {
            purchase = res[purchase];
            if (purchase.productId == pid && purchase.transactionId) {
              callback(purchase);
              return;
            }
          }
          app.iapBuy(pid, callback);
        }
      });
    }
  },

  iapRestorePurchases: function(callback) {
    var simulateRestore = false; // true to fake restorePurchases in simulator
    if (simulateRestore) {
      var now = new Date();
      now = now.getTime();
      var res = { productId: "puzzles2",
                  transactionId: "1",
                  date: now
                }
      callback([res]);
    } else {
      var cb = function(ignore) {
        inAppPurchase
          .restorePurchases()
          .then(callback)
          .catch(callback);
      };
      var prods = ["puzzles2"];
      // Make sure to call getProducts before anything else.
      inAppPurchase
        .getProducts(prods)
        .then(cb)
        .catch(callback);
    }
  }
};

app.initialize();
kakuroPorts.init();
