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

    isCordova: function() {
      return true;
    },

  iapGetProducts: function(pids, callback) {
    inAppPurchase
      .getProducts(pids)
      .then(callback)
      .catch(callback);
  },

  iapBuy: function(pid, callback) {
    inAppPurchase
      .buy(pid)
      .then(callback)
      .catch(callback);
  },

  iapRestorePurchases: function(callback) {
    inAppPurchase
      .restorePurchases()
      .then(callback)
      .catch(callback);
  }

};

// https://github.com/ftlabs/fastclick/blob/master/README.md
var attachFastClick = Origami.fastclick;
attachFastClick(document.body);

app.initialize();
kakuroPorts.init();
