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
    var res = board[y][x];
    //log("get(,"+x+","+y+") = "+res);
    return res;
  }

  // Generate an integer between 1 and max, inclusive
  function rand(max) {
    var r = Math.random();
    return Math.floor(r*max) + 1;
  }

  // Generate an integer between min and max, inclusive
  function genlen(max, min) {
    if (max === undefined) {
      max = 9;
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
  //   Do random column run sizes and ensure none < 2.
  //     Don't know whether to continue the row or stop the column
  function generate(maxlen, minlen, maxUnused) {
    //log("generate");
    if (!maxlen || maxlen > 9) {
      maxlen = 9
    }

    if (!minlen) {
      minlen = 2;
    }
    if (minlen > maxlen) {
      minlen = maxlen;
    }

    if (!maxUnused || maxUnused >= width) {
      maxUnused = Math.max(1, Math.min(maxlen, width-maxlen-1));
    }

    var board = new Array(height);

    var colLens = new Array(width);
    colLens[0] = height;
    for (var i=1; i<width; i++) {
      colLens[i] = 0;
    }

    for (var j=0; j<height; j++) {
      var row = new Array(width);
      if (j != 0) {
        row[0] = spanit(j);   // for debugging
      }
      board[j] = row;
      var left = 0;
      if (j == 0) {
        left = -width;   // First row is all empty
      }
      var numbers;
      for (var i=0; i<width; i++) {
        if (left < 0) {
          if (j==0 && i!=0) {
            row[i] = spanit(i);   // for debugging
          } else {
            row[i] = null;
            checkShortColumn(board, i, j-1);
          }
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
          while (true) {
            var num = pickNum(numbers);
            if (!num) {
              // Could do a whole backup stack thing here, but it's easier to just punt.
              log("punt at ("+i+","+j+")");
              checkShortRow(board, i, j, true);
              left = -1;
              i--;
              break;
            }
            if (!(isDuplicateInColumn(board, num, i, j) ||
                  isNonUnique(board, num, i, j))) {
              row[i] = num
              left--;
              break;
            }
          }
        }
      }
    }

    return board;
  }

  function spanit(x) {
    return "<span style='font-size: 50%;'>"+x+"</span>";
  }

  function logErase(i, j) {
    log("  erase ("+i+","+j+")");
  }    

  // When a cell is left blank or erased,
  // Need to erase the cell before & after, if they are all alone.
  function checkShortRow(board, i, j, noAfter) {
    var ii = i - 1;
    if (ii <= 0 || j <= 0) return;
    var row = board[j];
    if (row[ii] && (ii==1 || !row[ii-1])) {
      if (j==1 || !board[j-1][ii]) {
        logErase(ii, j);
        row[ii] = null;
        checkShortColumn(board, ii, j-1);
      }
    }
    ii = i + 1;
    if (!noAfter && ii<width && row[ii] && ((ii+1)==width || !row[ii+1])) {
      if (j==1 || !board[j-1][ii]) {
        logErase(ii, j);
        row[ii] = null;
        checkShortColumn(board, ii, j-1);
      }
    }
  }

  // When a row cell is left blank or erased,
  // need to check for a short column above it.
  function checkShortColumn(board, i, j) {
    if (i<=0 || j<=0) return;
    if (board[j][i] && (j==1 || !board[j-1][i])) {
      if ((i==1 || !board[j][i-1]) && ((i+1)==width || !board[j][i+1])) {
        logErase(i, j);
        board[j][i] = null;
        checkShortRow(board, i, j);
      }
    }
  }

  function isDuplicateInColumn(board, num, i, j) {
    for (var jj=j-1; jj>0; jj--) {
      var numjj = get(board, i, jj);
      if (numjj == num) {
        //log("Duplicate "+num+" at ("+i+","+j+") with row "+jj+": ");
        return true;
      }
      if (!numjj) {
        break;
      }
    }
    return false
  }

  function isNonUnique(board, num, i, j) {
    for (var jj=j-1; jj>0; jj--) {
      var numijj = get(board, i, jj);
      if (!numijj) {
        return false;
      }
      for (var ii=i-1; ii>0; ii--) {
        var numiij = get(board, ii, j);
        var numiijj = get(board, ii, jj);
        if (!(numiij && numiijj)) {
          return false;
        }
        if (num==numiijj && numijj==numiij) {
          log("non unique at ("+ii+","+jj+") ("+i+","+j+") = "+num+", "+numijj)
          return true;
        }
      }
    }
    return false;
  }

  function log(msg) {
    if (console) {
      console.error(msg);
    }
  }

  function logboard(board) {
    if (!console) return;
    for (var i=1; i<height; i++) {
      var row = board[i];
      if (!row) return;
      log(board[i]);
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
