----------------------------------------------------------------------
--
-- Generate.elm
-- Rectangular game board with integer elements
-- Copyright (c) 2016 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Generate exposing
  ( generate
  )

{-| Code to generate a new Kakuro board.

@docs generate
-}

import Board exposing (Board, make, get, set, getRow)

import Array exposing (Array)

{-| Create a new Board with the same shape as the input Board.

    generate maxrun board
-}
generate : Int -> Board -> Board
generate maxrun board =
  board


{-
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

  var gencount = 0;

  // Generate a board, an array of arrays, whose elements
  // are either numbers or pairs of [rowsum, colsum],
  // Where rowsum and colsum are the sums of the row to the right
  // and column below, with 0 if there is none.
  // *** TODO ***:
  //   Do random column run sizes and ensure none < 2.
  //     Don't know whether to continue the row or stop the column
  function generate(maxlen, minlen, maxUnused) {
    //log("generate");
    if (!maxlen || maxlen > 9) maxlen = 9
    if (maxlen < 2) maxlen = 2;
    if (!minlen) minlen = 2;
    if (minlen > maxlen) minlen = maxlen;

    if (!maxUnused || maxUnused >= width) {
      maxUnused = Math.max(1, Math.min(maxlen, width-maxlen-1));
    }

    gencount++;
    log("maxlen: "+maxlen+", minlen: "+minlen+", maxUnused: "+maxUnused);

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
              left = genlen(Math.min(maxlen, Math.max(2, width-i)), minlen);
              //log('run length:'+left);
              // Make sure we connect with the row above
              check:
              if (j > 1) {
                // See if we already will connect
                // Remember, i will be incremented before the start.
                for (var ii=i; ii<i+left; ii++) {
                  if (get(board, ii+1, j-1)) break check;
                }
                for (var ii=i-1; ii>0; ii--) {
                  if (ii>0 && get(board, ii, j)) break;
                  if (get(board, ii+1, j-1)) {
                    i = ii;
                    break check;
                  }
                }
                for (var ii=i+1+left; ii<width; ii++) {
                  if (get(board, ii+1, j-1)) {
                    i = ii + 1 - left;
                    break check;
                  }
                }
                left = -width;
              }
              //log("i: "+(i+1)+", j: "+j+", left: "+left);
              if (left > 0) numbers = initNums();
            } else {
              left = -width;
            }
          }
        } else if (left == 0) {
          left = -genlen(maxUnused, 1)
        } else {
          var tried = new Array();
          var duplicates = new Array();
          var nonuniques = new Array();
          while (true) {
            var num = pickNum(numbers);
            if (num == 0) throw("zero");
            if (!num) {
              // Could do a whole backup stack thing here, but it's easier to just punt.
              log("punt at ("+i+","+j+"), left: "+left+", tried: "+tried+", dups: "+duplicates+", nonuniques: "+nonuniques);
              checkShortRow(board, i, j, true);
              left = -1;
              i--;
              break;
            }
            tried.push(num);
            if (isDuplicateInColumn(board, num, i, j)) {
              duplicates.push(num);
            } else {
              if (isNonUnique(board, num, i, j)) {
                nonuniques.push(num);
              } else {
                // Return the numbers we skipped to the pool for the next column
                for (var n=0; n<duplicates.length; n++) {
                  numbers.push(duplicates[n]);
                }
                for (var n=0; n<nonuniques.length; n++) {
                  numbers.push(nonuniques[n]);
                }
                row[i] = num
                left--;
                //log("row["+i+"+] = "+num+", left: "+left+", dups: "+duplicates+", nonunique: "+nonuniques+", numbers: "+numbers);
                break;
              }
            }
          }
        }
      }
      // Eliminate orphan runs in the row above.
      // Maybe it would be better to ensure there aren't any.
      // This actually isn't good enough.
      // It doesn't guarantee that there aren't islands of connected rows.
      if (j > 1) {
        var jj = j-1;
        var start = false;
        var found = false;
        for (var ii=1; ii<=width; ii++) {
          if (!start) {
            if (ii<width && get(board, ii, jj)) {
              start = ii;
              if ((jj>1 && get(board, ii, jj-1)) ||
                  (jj<height && get(board, ii, jj+1))) {
                found = true;
              }
            }
          } else {
            if (ii==width || !get(board, ii, jj)) {
              if (!found) {
                for (iii=start; iii<ii; iii++) {
                  board[jj][iii] = null;
                }
              }
              start = false;
              found = false;
            } else {
              if ((jj>1 && get(board, ii, jj-1)) ||
                  (jj<height && get(board, ii, jj+1))) {
                found = true;
              }
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
      if (!numjj) break;
      if (numjj == num) {
        //log("Duplicate "+num+" at ("+i+","+j+") with row "+jj+": ");
        return true;
      }
    }
    return false
  }

  // True if value exists in the same run in row y or column x in board
  function existsInRowOrColumn(board, value, x, y) {
    // So I don't need to do this inline in the caller
    if (value<1 || value>9) {
      return true
    }
    var w = board[0].length;
    var h = board.length;
    for (var i=x-1; i>0; --i) {
      var v = get(board, i, y);
      if (!v) break
      if (v == value) return true;
    }
    for (var i=x+1; i<w; i++) {
      var v = get(board, i, y);
      if (!v) break;
      if (v == value) return true;
    }
    for (var j=y-1; j>0; --j) {
      var v = get(board, x, j);
      if (!v) break;
      if (v == value) return true;
    }
    for (var j=y+1; j<h; j++) {
      var v = get(board, x, j);
      if (!v) break;
      if (v == value) return true;
    }
    return false;
  }

  // This used to just check for get(i, j)==get(ii,jj) && get(i,jj)==get(ii,j),
  // but it really needs to be better than that.
  // It needs to check whether incrementing/decrementing the corners
  // and decrementing/incrementing the other corners can be done without collision.
  // If so, this is also a non-unique situation.
  // Fixing this may require doing backtracking instead of punting in generate().
  function isNonUnique(board, num, i, j) {
    for (var ii=i-1; ii>0; ii--) {
      var numiij = get(board, ii, j);
      if (!numiij) return false;
      middle:
      for (var jj=j-1; jj>0; jj--) {
        var numijj = get(board, i, jj);
        var numiijj = get(board, ii, jj);
        if (!numijj) return false;
        if (!numiijj) break;
        for (iii=ii+1; iii<i; iii++) {
          if (!get(board, iii, j)) break middle;
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
      console.error(""+gencount+": "+msg);
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
    var res = new Array()
    for (var i=1; i<=9; i++) {
      res.push(i);
    }
    return res;
  }

  // Randomly pick a number from the array returned by initNums and null
  // it out so it can't be picked again.
  // If all 9 numbers have been returned, return null.
  function pickNum(nums) {
    var cnt = nums.length;
    if (cnt == 0) return null;
    var i = rand(cnt) - 1;
    var res = nums[i];
    nums.splice(i, 1);
    return res;
  }

}

-}
