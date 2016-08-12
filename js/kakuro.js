//////////////////////////////////////////////////////////////////////
//
// kakuro.js
// kakuro functions
// Copyright (c) 2016 Bill St. Clair
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

var kakuro = new Kakuro()

function Kakuro() {
  var self = this;

  self.generate = generate;
  self.get = get;

  // Only for debugging
  self.rand = rand;
  self.genlen = genlen;
  self.initNums = initNums;
  self.pickNum = pickNum;
  self.logboard = logboard;

  var width = 10;
  var height = 10;

  function get(board, x, y) {
    return board[y][x];
  }

  // Generate an integer between 1 and max, inclusive
  function rand(max) {
    var r = Math.random();
    return Math.floor(r*max) + 1;
  }

  // Generate an integer between min and max, inclusive
  function genlen(max, min) {
    if (max === undefined) {
      max = width-1;
    } else if (max < 1) {
      max = 1;
    }
    if (min === undefined) {
      min = 1;
    }
    if (min > max) {
      min = max;
    }
    return rand(max-min+1) + min-1;
  }

  // Generate a board, an array of arrays, whose elements
  // are either numbers or pairs of [rowsum, colsum],
  // Where rowsum and colsum are the sums of the row to the right
  // and column below, with 0 if there is none.
  // *** TODO ***:
  //   Check for duplicate numbers in the same column in other rows
  //   Check for non-uniqueness:
  //     get(x, y) == get(a, b) and get(a, y) == get(x, b)
  //   Keep a backtrack stack, so we can back out and try again.
  //   Do random column run sizes and ensure none < 2.
  //     Don't know whether to continue the row or stop the column
  //   Do random unused space sizes, especially at left and top.
  function generate(maxlen, minlen, maxUnused) {
    if (!maxlen || maxlen >= width) {
      maxlen = width-1
    }

    if (!minlen) {
      minlen = 2;
    }
    if (minlen > maxlen) {
      minlen = maxlen;
    }

    if (!maxUnused || maxUnused >= width) {
      maxUnused = Math.min(maxlen, width-maxlen);
    }

    var board = new Array(height);

    var colLens = new Array(width);
    colLens[0] = height;
    for (var i=1; i<width; i++) {
      colLens[i] = 0;
    }

    for (var j=0; j<height; j++) {
      var row = new Array(width);
      board[j] = row;
      var left = 0;
      var numbers;
      for (var i=0; i<width; i++) {
        if (left < 0) {
          // Need to check for column length < 2 here and collissions with earlier rows
          row[i] = null;
          left++;
          if (left == 0) {
            if ((j > 0) && (width-i >= 3)) {
              left = genlen(Math.min(maxlen, width-i), minlen);
              numbers = initNums();
            } else {
              left = -width;
            }
          }
        } else if (left == 0) {
          left = -genlen(maxUnused, 1)
        } else {
          row[i] = pickNum(numbers);
          left--;
        }
      }
    }

    return board;
  }

  function logboard(board) {
    if (!console) return;
    for (var i=0; i<height; i++) {
      console.error(board[i]);
    }
  }

  // Return an array of integers: [1,2,3,4,5,6,7,8,9]
  function initNums() {
    var res = new Array(9);
    for (var i=0; i<9; i++) {
      res[i] = i+1;
    }
    return res;
  }

  // Randomly pick a number from the array returned by initNums and null
  // it out so it can't be picked again.
  // If all 9 numbers have been returned, return null.
  function pickNum(nums) {
    var cnt = 0;
    for (var i=0; i<9; i++) {
      if (nums[i]) {
        cnt++;
      }
    }
    var rnd = rand(cnt);
    var i = 0;
    var res = null;
    for (var c=0; c<rnd; c++) {
      while (true) {
        if (i >= 9) {
          return null;
        }
        res = nums[i++];
        if (res) {
          break;
        }
      }
    }
    if (res) {
      nums[i-1] = null;
    }
    return res;
  }

}
