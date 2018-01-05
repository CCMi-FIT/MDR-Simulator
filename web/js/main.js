"use strict";
// This object will hold all exports.
var Haste = {};
if(typeof window === 'undefined') window = global;

/* Constructor functions for small ADTs. */
function T0(t){this._=t;}
function T1(t,a){this._=t;this.a=a;}
function T2(t,a,b){this._=t;this.a=a;this.b=b;}
function T3(t,a,b,c){this._=t;this.a=a;this.b=b;this.c=c;}
function T4(t,a,b,c,d){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;}
function T5(t,a,b,c,d,e){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;}
function T6(t,a,b,c,d,e,f){this._=t;this.a=a;this.b=b;this.c=c;this.d=d;this.e=e;this.f=f;}

/* Thunk
   Creates a thunk representing the given closure.
   If the non-updatable flag is undefined, the thunk is updatable.
*/
function T(f, nu) {
    this.f = f;
    if(nu === undefined) {
        this.x = __updatable;
    }
}

/* Hint to optimizer that an imported symbol is strict. */
function __strict(x) {return x}

// A tailcall.
function F(f) {
    this.f = f;
}

// A partially applied function. Invariant: members are never thunks.
function PAP(f, args) {
    this.f = f;
    this.args = args;
    this.arity = f.length - args.length;
}

// "Zero" object; used to avoid creating a whole bunch of new objects
// in the extremely common case of a nil-like data constructor.
var __Z = new T0(0);

// Special object used for blackholing.
var __blackhole = {};

// Used to indicate that an object is updatable.
var __updatable = {};

// Indicates that a closure-creating tail loop isn't done.
var __continue = {};

/* Generic apply.
   Applies a function *or* a partial application object to a list of arguments.
   See https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/HaskellExecution/FunctionCalls
   for more information.
*/
function A(f, args) {
    while(true) {
        f = E(f);
        if(f instanceof Function) {
            if(args.length === f.length) {
                return f.apply(null, args);
            } else if(args.length < f.length) {
                return new PAP(f, args);
            } else {
                var f2 = f.apply(null, args.slice(0, f.length));
                args = args.slice(f.length);
                f = B(f2);
            }
        } else if(f instanceof PAP) {
            if(args.length === f.arity) {
                return f.f.apply(null, f.args.concat(args));
            } else if(args.length < f.arity) {
                return new PAP(f.f, f.args.concat(args));
            } else {
                var f2 = f.f.apply(null, f.args.concat(args.slice(0, f.arity)));
                args = args.slice(f.arity);
                f = B(f2);
            }
        } else {
            return f;
        }
    }
}

function A1(f, x) {
    f = E(f);
    if(f instanceof Function) {
        return f.length === 1 ? f(x) : new PAP(f, [x]);
    } else if(f instanceof PAP) {
        return f.arity === 1 ? f.f.apply(null, f.args.concat([x]))
                             : new PAP(f.f, f.args.concat([x]));
    } else {
        return f;
    }
}

function A2(f, x, y) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 2:  return f(x, y);
        case 1:  return A1(B(f(x)), y);
        default: return new PAP(f, [x,y]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 2:  return f.f.apply(null, f.args.concat([x,y]));
        case 1:  return A1(B(f.f.apply(null, f.args.concat([x]))), y);
        default: return new PAP(f.f, f.args.concat([x,y]));
        }
    } else {
        return f;
    }
}

function A3(f, x, y, z) {
    f = E(f);
    if(f instanceof Function) {
        switch(f.length) {
        case 3:  return f(x, y, z);
        case 2:  return A1(B(f(x, y)), z);
        case 1:  return A2(B(f(x)), y, z);
        default: return new PAP(f, [x,y,z]);
        }
    } else if(f instanceof PAP) {
        switch(f.arity) {
        case 3:  return f.f.apply(null, f.args.concat([x,y,z]));
        case 2:  return A1(B(f.f.apply(null, f.args.concat([x,y]))), z);
        case 1:  return A2(B(f.f.apply(null, f.args.concat([x]))), y, z);
        default: return new PAP(f.f, f.args.concat([x,y,z]));
        }
    } else {
        return f;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f !== __blackhole) {
            if(t.x === __updatable) {
                var f = t.f;
                t.f = __blackhole;
                t.x = f();
            } else {
                return t.f();
            }
        }
        if(t.x === __updatable) {
            throw 'Infinite loop!';
        } else {
            return t.x;
        }
    } else {
        return t;
    }
}

/* Tail call chain counter. */
var C = 0, Cs = [];

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    Cs.push(C);
    while(f instanceof F) {
        var fun = f.f;
        f.f = __blackhole;
        C = 0;
        f = fun();
    }
    C = Cs.pop();
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw E(err);
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return {_:0, a:(a-a%b)/b, b:a%b};
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return {_:0, a:x & 0xffffffff, b:x > 0x7fffffff};
}

function subC(a, b) {
    var x = a-b;
    return {_:0, a:x & 0xffffffff, b:x < -2147483648};
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, __Z);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [str.charCodeAt(i), acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return {_:1,a:str.charCodeAt(i),b:new T(function() {
            return unAppCStr(str,chrs,i+1);
        })};
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str._ == 1; str = E(str.b)) {
        s += String.fromCharCode(E(str.a));
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x.a;
    return x.b;
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return 0;
    } else if(a == b) {
        return 1;
    }
    return 2;
}

/* Convert a JS exception into a Haskell JSException */
function __hsException(e) {
  e = e.toString();
  var x = new Long(2904464383, 3929545892, true);
  var y = new Long(3027541338, 3270546716, true);
  var t = new T5(0, x, y
                  , new T5(0, x, y
                            , unCStr("haste-prim")
                            , unCStr("Haste.Prim.Foreign")
                            , unCStr("JSException")), __Z, __Z);
  var show = function(x) {return unCStr(E(x).a);}
  var dispEx = function(x) {return unCStr("JavaScript exception: " + E(x).a);}
  var showList = function(_, s) {return unAppCStr(e, s);}
  var showsPrec = function(_, _p, s) {return unAppCStr(e, s);}
  var showDict = new T3(0, showsPrec, show, showList);
  var self;
  var fromEx = function(_) {return new T1(1, self);}
  var dict = new T5(0, t, showDict, null /* toException */, fromEx, dispEx);
  self = new T2(0, dict, new T1(0, e));
  return self;
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        if(typeof e._ === 'undefined') {
            e = __hsException(e);
        }
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Object) {
        return x._;
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round, rintDouble = jsRound, rintFloat = jsRound;
var jsTrunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
};
function jsRoundW(n) {
    return Math.abs(jsTrunc(n));
}
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt64(i) {
    return popCnt(i.low) + popCnt(i.high);
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function __clz(bits, x) {
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    } else {
        return bits - (1 + Math.floor(Math.log(x)/Math.LN2));
    }
}

// TODO: can probably be done much faster with arithmetic tricks like __clz
function __ctz(bits, x) {
    var y = 1;
    x &= (Math.pow(2, bits)-1);
    if(x === 0) {
        return bits;
    }
    for(var i = 0; i < bits; ++i) {
        if(y & x) {
            return i;
        } else {
            y <<= 1;
        }
    }
    return 0;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    if(x === 0) {
        return __decodedZeroF;
    }
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return {_:0, a:sign*man, b:exp};
}

var __decodedZero = {_:0,a:1,b:0,c:0,d:0};
var __decodedZeroF = {_:0,a:1,b:0};

function decodeDouble(x) {
    if(x === 0) {
        // GHC 7.10+ *really* doesn't like 0 to be represented as anything
        // but zeroes all the way.
        return __decodedZero;
    }
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return {_:0, a:sign, b:manHigh, c:manLow, d:exp};
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

window['jsGetMouseCoords'] = function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs._) {
        strs = E(strs);
        arr.push(E(strs.a));
        strs = E(strs.b);
    }
    return arr.join(sep);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return __Z;
    }
    return {_:1,a:hs};
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return {_:0, a:jsRead(obj)};
    case 'string':
        return {_:1, a:obj};
    case 'boolean':
        return {_:2, a:obj}; // Booleans are special wrt constructor tags!
    case 'object':
        if(obj instanceof Array) {
            return {_:3, a:arr2lst_json(obj, 0)};
        } else if (obj == null) {
            return {_:5};
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = {_:1, a:{_:0, a:ks[i], b:toHS(obj[ks[i]])}, b:xs};
            }
            return {_:4, a:xs};
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1, a:toHS(arr[elem]), b:new T(function() {return arr2lst_json(arr,elem+1);}),c:true}
}

/* gettimeofday(2) */
function gettimeofday(tv, _tz) {
    var t = new Date().getTime();
    writeOffAddr("i32", 4, tv, 0, (t/1000)|0);
    writeOffAddr("i32", 4, tv, 1, ((t%1000)*1000)|0);
    return 0;
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

/* bn.js by Fedor Indutny, see doc/LICENSE.bn for license */
var __bn = {};
(function (module, exports) {
'use strict';

function BN(number, base, endian) {
  // May be `new BN(bn)` ?
  if (number !== null &&
      typeof number === 'object' &&
      Array.isArray(number.words)) {
    return number;
  }

  this.negative = 0;
  this.words = null;
  this.length = 0;

  if (base === 'le' || base === 'be') {
    endian = base;
    base = 10;
  }

  if (number !== null)
    this._init(number || 0, base || 10, endian || 'be');
}
if (typeof module === 'object')
  module.exports = BN;
else
  exports.BN = BN;

BN.BN = BN;
BN.wordSize = 26;

BN.max = function max(left, right) {
  if (left.cmp(right) > 0)
    return left;
  else
    return right;
};

BN.min = function min(left, right) {
  if (left.cmp(right) < 0)
    return left;
  else
    return right;
};

BN.prototype._init = function init(number, base, endian) {
  if (typeof number === 'number') {
    return this._initNumber(number, base, endian);
  } else if (typeof number === 'object') {
    return this._initArray(number, base, endian);
  }
  if (base === 'hex')
    base = 16;

  number = number.toString().replace(/\s+/g, '');
  var start = 0;
  if (number[0] === '-')
    start++;

  if (base === 16)
    this._parseHex(number, start);
  else
    this._parseBase(number, base, start);

  if (number[0] === '-')
    this.negative = 1;

  this.strip();

  if (endian !== 'le')
    return;

  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initNumber = function _initNumber(number, base, endian) {
  if (number < 0) {
    this.negative = 1;
    number = -number;
  }
  if (number < 0x4000000) {
    this.words = [ number & 0x3ffffff ];
    this.length = 1;
  } else if (number < 0x10000000000000) {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff
    ];
    this.length = 2;
  } else {
    this.words = [
      number & 0x3ffffff,
      (number / 0x4000000) & 0x3ffffff,
      1
    ];
    this.length = 3;
  }

  if (endian !== 'le')
    return;

  // Reverse the bytes
  this._initArray(this.toArray(), base, endian);
};

BN.prototype._initArray = function _initArray(number, base, endian) {
  if (number.length <= 0) {
    this.words = [ 0 ];
    this.length = 1;
    return this;
  }

  this.length = Math.ceil(number.length / 3);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  var off = 0;
  if (endian === 'be') {
    for (var i = number.length - 1, j = 0; i >= 0; i -= 3) {
      var w = number[i] | (number[i - 1] << 8) | (number[i - 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  } else if (endian === 'le') {
    for (var i = 0, j = 0; i < number.length; i += 3) {
      var w = number[i] | (number[i + 1] << 8) | (number[i + 2] << 16);
      this.words[j] |= (w << off) & 0x3ffffff;
      this.words[j + 1] = (w >>> (26 - off)) & 0x3ffffff;
      off += 24;
      if (off >= 26) {
        off -= 26;
        j++;
      }
    }
  }
  return this.strip();
};

function parseHex(str, start, end) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r <<= 4;

    // 'a' - 'f'
    if (c >= 49 && c <= 54)
      r |= c - 49 + 0xa;

    // 'A' - 'F'
    else if (c >= 17 && c <= 22)
      r |= c - 17 + 0xa;

    // '0' - '9'
    else
      r |= c & 0xf;
  }
  return r;
}

BN.prototype._parseHex = function _parseHex(number, start) {
  // Create possibly bigger array to ensure that it fits the number
  this.length = Math.ceil((number.length - start) / 6);
  this.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    this.words[i] = 0;

  // Scan 24-bit chunks and add them to the number
  var off = 0;
  for (var i = number.length - 6, j = 0; i >= start; i -= 6) {
    var w = parseHex(number, i, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
    off += 24;
    if (off >= 26) {
      off -= 26;
      j++;
    }
  }
  if (i + 6 !== start) {
    var w = parseHex(number, start, i + 6);
    this.words[j] |= (w << off) & 0x3ffffff;
    this.words[j + 1] |= w >>> (26 - off) & 0x3fffff;
  }
  this.strip();
};

function parseBase(str, start, end, mul) {
  var r = 0;
  var len = Math.min(str.length, end);
  for (var i = start; i < len; i++) {
    var c = str.charCodeAt(i) - 48;

    r *= mul;

    // 'a'
    if (c >= 49)
      r += c - 49 + 0xa;

    // 'A'
    else if (c >= 17)
      r += c - 17 + 0xa;

    // '0' - '9'
    else
      r += c;
  }
  return r;
}

BN.prototype._parseBase = function _parseBase(number, base, start) {
  // Initialize as zero
  this.words = [ 0 ];
  this.length = 1;

  // Find length of limb in base
  for (var limbLen = 0, limbPow = 1; limbPow <= 0x3ffffff; limbPow *= base)
    limbLen++;
  limbLen--;
  limbPow = (limbPow / base) | 0;

  var total = number.length - start;
  var mod = total % limbLen;
  var end = Math.min(total, total - mod) + start;

  var word = 0;
  for (var i = start; i < end; i += limbLen) {
    word = parseBase(number, i, i + limbLen, base);

    this.imuln(limbPow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }

  if (mod !== 0) {
    var pow = 1;
    var word = parseBase(number, i, number.length, base);

    for (var i = 0; i < mod; i++)
      pow *= base;
    this.imuln(pow);
    if (this.words[0] + word < 0x4000000)
      this.words[0] += word;
    else
      this._iaddn(word);
  }
};

BN.prototype.copy = function copy(dest) {
  dest.words = new Array(this.length);
  for (var i = 0; i < this.length; i++)
    dest.words[i] = this.words[i];
  dest.length = this.length;
  dest.negative = this.negative;
};

BN.prototype.clone = function clone() {
  var r = new BN(null);
  this.copy(r);
  return r;
};

// Remove leading `0` from `this`
BN.prototype.strip = function strip() {
  while (this.length > 1 && this.words[this.length - 1] === 0)
    this.length--;
  return this._normSign();
};

BN.prototype._normSign = function _normSign() {
  // -0 = 0
  if (this.length === 1 && this.words[0] === 0)
    this.negative = 0;
  return this;
};

var zeros = [
  '',
  '0',
  '00',
  '000',
  '0000',
  '00000',
  '000000',
  '0000000',
  '00000000',
  '000000000',
  '0000000000',
  '00000000000',
  '000000000000',
  '0000000000000',
  '00000000000000',
  '000000000000000',
  '0000000000000000',
  '00000000000000000',
  '000000000000000000',
  '0000000000000000000',
  '00000000000000000000',
  '000000000000000000000',
  '0000000000000000000000',
  '00000000000000000000000',
  '000000000000000000000000',
  '0000000000000000000000000'
];

var groupSizes = [
  0, 0,
  25, 16, 12, 11, 10, 9, 8,
  8, 7, 7, 7, 7, 6, 6,
  6, 6, 6, 6, 6, 5, 5,
  5, 5, 5, 5, 5, 5, 5,
  5, 5, 5, 5, 5, 5, 5
];

var groupBases = [
  0, 0,
  33554432, 43046721, 16777216, 48828125, 60466176, 40353607, 16777216,
  43046721, 10000000, 19487171, 35831808, 62748517, 7529536, 11390625,
  16777216, 24137569, 34012224, 47045881, 64000000, 4084101, 5153632,
  6436343, 7962624, 9765625, 11881376, 14348907, 17210368, 20511149,
  24300000, 28629151, 33554432, 39135393, 45435424, 52521875, 60466176
];

BN.prototype.toString = function toString(base, padding) {
  base = base || 10;
  var padding = padding | 0 || 1;
  if (base === 16 || base === 'hex') {
    var out = '';
    var off = 0;
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var w = this.words[i];
      var word = (((w << off) | carry) & 0xffffff).toString(16);
      carry = (w >>> (24 - off)) & 0xffffff;
      if (carry !== 0 || i !== this.length - 1)
        out = zeros[6 - word.length] + word + out;
      else
        out = word + out;
      off += 2;
      if (off >= 26) {
        off -= 26;
        i--;
      }
    }
    if (carry !== 0)
      out = carry.toString(16) + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else if (base === (base | 0) && base >= 2 && base <= 36) {
    var groupSize = groupSizes[base];
    var groupBase = groupBases[base];
    var out = '';
    var c = this.clone();
    c.negative = 0;
    while (c.cmpn(0) !== 0) {
      var r = c.modn(groupBase).toString(base);
      c = c.idivn(groupBase);

      if (c.cmpn(0) !== 0)
        out = zeros[groupSize - r.length] + r + out;
      else
        out = r + out;
    }
    if (this.cmpn(0) === 0)
      out = '0' + out;
    while (out.length % padding !== 0)
      out = '0' + out;
    if (this.negative !== 0)
      out = '-' + out;
    return out;
  } else {
    throw 'Base should be between 2 and 36';
  }
};

BN.prototype.toJSON = function toJSON() {
  return this.toString(16);
};

BN.prototype.toArray = function toArray(endian, length) {
  this.strip();
  var littleEndian = endian === 'le';
  var res = new Array(this.byteLength());
  res[0] = 0;

  var q = this.clone();
  if (!littleEndian) {
    // Assume big-endian
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[res.length - i - 1] = b;
    }
  } else {
    for (var i = 0; q.cmpn(0) !== 0; i++) {
      var b = q.andln(0xff);
      q.iushrn(8);

      res[i] = b;
    }
  }

  if (length) {
    while (res.length < length) {
      if (littleEndian)
        res.push(0);
      else
        res.unshift(0);
    }
  }

  return res;
};

if (Math.clz32) {
  BN.prototype._countBits = function _countBits(w) {
    return 32 - Math.clz32(w);
  };
} else {
  BN.prototype._countBits = function _countBits(w) {
    var t = w;
    var r = 0;
    if (t >= 0x1000) {
      r += 13;
      t >>>= 13;
    }
    if (t >= 0x40) {
      r += 7;
      t >>>= 7;
    }
    if (t >= 0x8) {
      r += 4;
      t >>>= 4;
    }
    if (t >= 0x02) {
      r += 2;
      t >>>= 2;
    }
    return r + t;
  };
}

// Return number of used bits in a BN
BN.prototype.bitLength = function bitLength() {
  var hi = 0;
  var w = this.words[this.length - 1];
  var hi = this._countBits(w);
  return (this.length - 1) * 26 + hi;
};

BN.prototype.byteLength = function byteLength() {
  return Math.ceil(this.bitLength() / 8);
};

// Return negative clone of `this`
BN.prototype.neg = function neg() {
  if (this.cmpn(0) === 0)
    return this.clone();

  var r = this.clone();
  r.negative = this.negative ^ 1;
  return r;
};

BN.prototype.ineg = function ineg() {
  this.negative ^= 1;
  return this;
};

// Or `num` with `this` in-place
BN.prototype.iuor = function iuor(num) {
  while (this.length < num.length)
    this.words[this.length++] = 0;

  for (var i = 0; i < num.length; i++)
    this.words[i] = this.words[i] | num.words[i];

  return this.strip();
};

BN.prototype.ior = function ior(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuor(num);
};


// Or `num` with `this`
BN.prototype.or = function or(num) {
  if (this.length > num.length)
    return this.clone().ior(num);
  else
    return num.clone().ior(this);
};

BN.prototype.uor = function uor(num) {
  if (this.length > num.length)
    return this.clone().iuor(num);
  else
    return num.clone().iuor(this);
};


// And `num` with `this` in-place
BN.prototype.iuand = function iuand(num) {
  // b = min-length(num, this)
  var b;
  if (this.length > num.length)
    b = num;
  else
    b = this;

  for (var i = 0; i < b.length; i++)
    this.words[i] = this.words[i] & num.words[i];

  this.length = b.length;

  return this.strip();
};

BN.prototype.iand = function iand(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuand(num);
};


// And `num` with `this`
BN.prototype.and = function and(num) {
  if (this.length > num.length)
    return this.clone().iand(num);
  else
    return num.clone().iand(this);
};

BN.prototype.uand = function uand(num) {
  if (this.length > num.length)
    return this.clone().iuand(num);
  else
    return num.clone().iuand(this);
};


// Xor `num` with `this` in-place
BN.prototype.iuxor = function iuxor(num) {
  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  for (var i = 0; i < b.length; i++)
    this.words[i] = a.words[i] ^ b.words[i];

  if (this !== a)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];

  this.length = a.length;

  return this.strip();
};

BN.prototype.ixor = function ixor(num) {
  //assert((this.negative | num.negative) === 0);
  return this.iuxor(num);
};


// Xor `num` with `this`
BN.prototype.xor = function xor(num) {
  if (this.length > num.length)
    return this.clone().ixor(num);
  else
    return num.clone().ixor(this);
};

BN.prototype.uxor = function uxor(num) {
  if (this.length > num.length)
    return this.clone().iuxor(num);
  else
    return num.clone().iuxor(this);
};


// Add `num` to `this` in-place
BN.prototype.iadd = function iadd(num) {
  // negative + positive
  if (this.negative !== 0 && num.negative === 0) {
    this.negative = 0;
    var r = this.isub(num);
    this.negative ^= 1;
    return this._normSign();

  // positive + negative
  } else if (this.negative === 0 && num.negative !== 0) {
    num.negative = 0;
    var r = this.isub(num);
    num.negative = 1;
    return r._normSign();
  }

  // a.length > b.length
  var a;
  var b;
  if (this.length > num.length) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) + (b.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    this.words[i] = r & 0x3ffffff;
    carry = r >>> 26;
  }

  this.length = a.length;
  if (carry !== 0) {
    this.words[this.length] = carry;
    this.length++;
  // Copy the rest of the words
  } else if (a !== this) {
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  }

  return this;
};

// Add `num` to `this`
BN.prototype.add = function add(num) {
  if (num.negative !== 0 && this.negative === 0) {
    num.negative = 0;
    var res = this.sub(num);
    num.negative ^= 1;
    return res;
  } else if (num.negative === 0 && this.negative !== 0) {
    this.negative = 0;
    var res = num.sub(this);
    this.negative = 1;
    return res;
  }

  if (this.length > num.length)
    return this.clone().iadd(num);
  else
    return num.clone().iadd(this);
};

// Subtract `num` from `this` in-place
BN.prototype.isub = function isub(num) {
  // this - (-num) = this + num
  if (num.negative !== 0) {
    num.negative = 0;
    var r = this.iadd(num);
    num.negative = 1;
    return r._normSign();

  // -this - num = -(this + num)
  } else if (this.negative !== 0) {
    this.negative = 0;
    this.iadd(num);
    this.negative = 1;
    return this._normSign();
  }

  // At this point both numbers are positive
  var cmp = this.cmp(num);

  // Optimization - zeroify
  if (cmp === 0) {
    this.negative = 0;
    this.length = 1;
    this.words[0] = 0;
    return this;
  }

  // a > b
  var a;
  var b;
  if (cmp > 0) {
    a = this;
    b = num;
  } else {
    a = num;
    b = this;
  }

  var carry = 0;
  for (var i = 0; i < b.length; i++) {
    var r = (a.words[i] | 0) - (b.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }
  for (; carry !== 0 && i < a.length; i++) {
    var r = (a.words[i] | 0) + carry;
    carry = r >> 26;
    this.words[i] = r & 0x3ffffff;
  }

  // Copy rest of the words
  if (carry === 0 && i < a.length && a !== this)
    for (; i < a.length; i++)
      this.words[i] = a.words[i];
  this.length = Math.max(this.length, i);

  if (a !== this)
    this.negative = 1;

  return this.strip();
};

// Subtract `num` from `this`
BN.prototype.sub = function sub(num) {
  return this.clone().isub(num);
};

function smallMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  var len = (self.length + num.length) | 0;
  out.length = len;
  len = (len - 1) | 0;

  // Peel one iteration (compiler can't do it, because of code complexity)
  var a = self.words[0] | 0;
  var b = num.words[0] | 0;
  var r = a * b;

  var lo = r & 0x3ffffff;
  var carry = (r / 0x4000000) | 0;
  out.words[0] = lo;

  for (var k = 1; k < len; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = carry >>> 26;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = (k - j) | 0;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;
    }
    out.words[k] = rword | 0;
    carry = ncarry | 0;
  }
  if (carry !== 0) {
    out.words[k] = carry | 0;
  } else {
    out.length--;
  }

  return out.strip();
}

function bigMulTo(self, num, out) {
  out.negative = num.negative ^ self.negative;
  out.length = self.length + num.length;

  var carry = 0;
  var hncarry = 0;
  for (var k = 0; k < out.length - 1; k++) {
    // Sum all words with the same `i + j = k` and accumulate `ncarry`,
    // note that ncarry could be >= 0x3ffffff
    var ncarry = hncarry;
    hncarry = 0;
    var rword = carry & 0x3ffffff;
    var maxJ = Math.min(k, num.length - 1);
    for (var j = Math.max(0, k - self.length + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = self.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      ncarry = (ncarry + ((r / 0x4000000) | 0)) | 0;
      lo = (lo + rword) | 0;
      rword = lo & 0x3ffffff;
      ncarry = (ncarry + (lo >>> 26)) | 0;

      hncarry += ncarry >>> 26;
      ncarry &= 0x3ffffff;
    }
    out.words[k] = rword;
    carry = ncarry;
    ncarry = hncarry;
  }
  if (carry !== 0) {
    out.words[k] = carry;
  } else {
    out.length--;
  }

  return out.strip();
}

BN.prototype.mulTo = function mulTo(num, out) {
  var res;
  if (this.length + num.length < 63)
    res = smallMulTo(this, num, out);
  else
    res = bigMulTo(this, num, out);
  return res;
};

// Multiply `this` by `num`
BN.prototype.mul = function mul(num) {
  var out = new BN(null);
  out.words = new Array(this.length + num.length);
  return this.mulTo(num, out);
};

// In-place Multiplication
BN.prototype.imul = function imul(num) {
  if (this.cmpn(0) === 0 || num.cmpn(0) === 0) {
    this.words[0] = 0;
    this.length = 1;
    return this;
  }

  var tlen = this.length;
  var nlen = num.length;

  this.negative = num.negative ^ this.negative;
  this.length = this.length + num.length;
  this.words[this.length - 1] = 0;

  for (var k = this.length - 2; k >= 0; k--) {
    // Sum all words with the same `i + j = k` and accumulate `carry`,
    // note that carry could be >= 0x3ffffff
    var carry = 0;
    var rword = 0;
    var maxJ = Math.min(k, nlen - 1);
    for (var j = Math.max(0, k - tlen + 1); j <= maxJ; j++) {
      var i = k - j;
      var a = this.words[i] | 0;
      var b = num.words[j] | 0;
      var r = a * b;

      var lo = r & 0x3ffffff;
      carry += (r / 0x4000000) | 0;
      lo += rword;
      rword = lo & 0x3ffffff;
      carry += lo >>> 26;
    }
    this.words[k] = rword;
    this.words[k + 1] += carry;
    carry = 0;
  }

  // Propagate overflows
  var carry = 0;
  for (var i = 1; i < this.length; i++) {
    var w = (this.words[i] | 0) + carry;
    this.words[i] = w & 0x3ffffff;
    carry = w >>> 26;
  }

  return this.strip();
};

BN.prototype.imuln = function imuln(num) {
  // Carry
  var carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = (this.words[i] | 0) * num;
    var lo = (w & 0x3ffffff) + (carry & 0x3ffffff);
    carry >>= 26;
    carry += (w / 0x4000000) | 0;
    // NOTE: lo is 27bit maximum
    carry += lo >>> 26;
    this.words[i] = lo & 0x3ffffff;
  }

  if (carry !== 0) {
    this.words[i] = carry;
    this.length++;
  }

  return this;
};

BN.prototype.muln = function muln(num) {
  return this.clone().imuln(num);
};

// `this` * `this`
BN.prototype.sqr = function sqr() {
  return this.mul(this);
};

// `this` * `this` in-place
BN.prototype.isqr = function isqr() {
  return this.mul(this);
};

// Shift-left in-place
BN.prototype.iushln = function iushln(bits) {
  var r = bits % 26;
  var s = (bits - r) / 26;
  var carryMask = (0x3ffffff >>> (26 - r)) << (26 - r);

  if (r !== 0) {
    var carry = 0;
    for (var i = 0; i < this.length; i++) {
      var newCarry = this.words[i] & carryMask;
      var c = ((this.words[i] | 0) - newCarry) << r;
      this.words[i] = c | carry;
      carry = newCarry >>> (26 - r);
    }
    if (carry) {
      this.words[i] = carry;
      this.length++;
    }
  }

  if (s !== 0) {
    for (var i = this.length - 1; i >= 0; i--)
      this.words[i + s] = this.words[i];
    for (var i = 0; i < s; i++)
      this.words[i] = 0;
    this.length += s;
  }

  return this.strip();
};

BN.prototype.ishln = function ishln(bits) {
  return this.iushln(bits);
};

// Shift-right in-place
BN.prototype.iushrn = function iushrn(bits, hint, extended) {
  var h;
  if (hint)
    h = (hint - (hint % 26)) / 26;
  else
    h = 0;

  var r = bits % 26;
  var s = Math.min((bits - r) / 26, this.length);
  var mask = 0x3ffffff ^ ((0x3ffffff >>> r) << r);
  var maskedWords = extended;

  h -= s;
  h = Math.max(0, h);

  // Extended mode, copy masked part
  if (maskedWords) {
    for (var i = 0; i < s; i++)
      maskedWords.words[i] = this.words[i];
    maskedWords.length = s;
  }

  if (s === 0) {
    // No-op, we should not move anything at all
  } else if (this.length > s) {
    this.length -= s;
    for (var i = 0; i < this.length; i++)
      this.words[i] = this.words[i + s];
  } else {
    this.words[0] = 0;
    this.length = 1;
  }

  var carry = 0;
  for (var i = this.length - 1; i >= 0 && (carry !== 0 || i >= h); i--) {
    var word = this.words[i] | 0;
    this.words[i] = (carry << (26 - r)) | (word >>> r);
    carry = word & mask;
  }

  // Push carried bits as a mask
  if (maskedWords && carry !== 0)
    maskedWords.words[maskedWords.length++] = carry;

  if (this.length === 0) {
    this.words[0] = 0;
    this.length = 1;
  }

  this.strip();

  return this;
};

BN.prototype.ishrn = function ishrn(bits, hint, extended) {
  return this.iushrn(bits, hint, extended);
};

// Shift-left
BN.prototype.shln = function shln(bits) {
  var x = this.clone();
  var neg = x.negative;
  x.negative = false;
  x.ishln(bits);
  x.negative = neg;
  return x;
};

BN.prototype.ushln = function ushln(bits) {
  return this.clone().iushln(bits);
};

// Shift-right
BN.prototype.shrn = function shrn(bits) {
  var x = this.clone();
  if(x.negative) {
      x.negative = false;
      x.ishrn(bits);
      x.negative = true;
      return x.isubn(1);
  } else {
      return x.ishrn(bits);
  }
};

BN.prototype.ushrn = function ushrn(bits) {
  return this.clone().iushrn(bits);
};

// Test if n bit is set
BN.prototype.testn = function testn(bit) {
  var r = bit % 26;
  var s = (bit - r) / 26;
  var q = 1 << r;

  // Fast case: bit is much higher than all existing words
  if (this.length <= s) {
    return false;
  }

  // Check bit and return
  var w = this.words[s];

  return !!(w & q);
};

// Add plain number `num` to `this`
BN.prototype.iaddn = function iaddn(num) {
  if (num < 0)
    return this.isubn(-num);

  // Possible sign change
  if (this.negative !== 0) {
    if (this.length === 1 && (this.words[0] | 0) < num) {
      this.words[0] = num - (this.words[0] | 0);
      this.negative = 0;
      return this;
    }

    this.negative = 0;
    this.isubn(num);
    this.negative = 1;
    return this;
  }

  // Add without checks
  return this._iaddn(num);
};

BN.prototype._iaddn = function _iaddn(num) {
  this.words[0] += num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] >= 0x4000000; i++) {
    this.words[i] -= 0x4000000;
    if (i === this.length - 1)
      this.words[i + 1] = 1;
    else
      this.words[i + 1]++;
  }
  this.length = Math.max(this.length, i + 1);

  return this;
};

// Subtract plain number `num` from `this`
BN.prototype.isubn = function isubn(num) {
  if (num < 0)
    return this.iaddn(-num);

  if (this.negative !== 0) {
    this.negative = 0;
    this.iaddn(num);
    this.negative = 1;
    return this;
  }

  this.words[0] -= num;

  // Carry
  for (var i = 0; i < this.length && this.words[i] < 0; i++) {
    this.words[i] += 0x4000000;
    this.words[i + 1] -= 1;
  }

  return this.strip();
};

BN.prototype.addn = function addn(num) {
  return this.clone().iaddn(num);
};

BN.prototype.subn = function subn(num) {
  return this.clone().isubn(num);
};

BN.prototype.iabs = function iabs() {
  this.negative = 0;

  return this;
};

BN.prototype.abs = function abs() {
  return this.clone().iabs();
};

BN.prototype._ishlnsubmul = function _ishlnsubmul(num, mul, shift) {
  // Bigger storage is needed
  var len = num.length + shift;
  var i;
  if (this.words.length < len) {
    var t = new Array(len);
    for (var i = 0; i < this.length; i++)
      t[i] = this.words[i];
    this.words = t;
  } else {
    i = this.length;
  }

  // Zeroify rest
  this.length = Math.max(this.length, len);
  for (; i < this.length; i++)
    this.words[i] = 0;

  var carry = 0;
  for (var i = 0; i < num.length; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    var right = (num.words[i] | 0) * mul;
    w -= right & 0x3ffffff;
    carry = (w >> 26) - ((right / 0x4000000) | 0);
    this.words[i + shift] = w & 0x3ffffff;
  }
  for (; i < this.length - shift; i++) {
    var w = (this.words[i + shift] | 0) + carry;
    carry = w >> 26;
    this.words[i + shift] = w & 0x3ffffff;
  }

  if (carry === 0)
    return this.strip();

  carry = 0;
  for (var i = 0; i < this.length; i++) {
    var w = -(this.words[i] | 0) + carry;
    carry = w >> 26;
    this.words[i] = w & 0x3ffffff;
  }
  this.negative = 1;

  return this.strip();
};

BN.prototype._wordDiv = function _wordDiv(num, mode) {
  var shift = this.length - num.length;

  var a = this.clone();
  var b = num;

  // Normalize
  var bhi = b.words[b.length - 1] | 0;
  var bhiBits = this._countBits(bhi);
  shift = 26 - bhiBits;
  if (shift !== 0) {
    b = b.ushln(shift);
    a.iushln(shift);
    bhi = b.words[b.length - 1] | 0;
  }

  // Initialize quotient
  var m = a.length - b.length;
  var q;

  if (mode !== 'mod') {
    q = new BN(null);
    q.length = m + 1;
    q.words = new Array(q.length);
    for (var i = 0; i < q.length; i++)
      q.words[i] = 0;
  }

  var diff = a.clone()._ishlnsubmul(b, 1, m);
  if (diff.negative === 0) {
    a = diff;
    if (q)
      q.words[m] = 1;
  }

  for (var j = m - 1; j >= 0; j--) {
    var qj = (a.words[b.length + j] | 0) * 0x4000000 +
             (a.words[b.length + j - 1] | 0);

    // NOTE: (qj / bhi) is (0x3ffffff * 0x4000000 + 0x3ffffff) / 0x2000000 max
    // (0x7ffffff)
    qj = Math.min((qj / bhi) | 0, 0x3ffffff);

    a._ishlnsubmul(b, qj, j);
    while (a.negative !== 0) {
      qj--;
      a.negative = 0;
      a._ishlnsubmul(b, 1, j);
      if (a.cmpn(0) !== 0)
        a.negative ^= 1;
    }
    if (q)
      q.words[j] = qj;
  }
  if (q)
    q.strip();
  a.strip();

  // Denormalize
  if (mode !== 'div' && shift !== 0)
    a.iushrn(shift);
  return { div: q ? q : null, mod: a };
};

BN.prototype.divmod = function divmod(num, mode, positive) {
  if (this.negative !== 0 && num.negative === 0) {
    var res = this.neg().divmod(num, mode);
    var div;
    var mod;
    if (mode !== 'mod')
      div = res.div.neg();
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.add(num);
    }
    return {
      div: div,
      mod: mod
    };
  } else if (this.negative === 0 && num.negative !== 0) {
    var res = this.divmod(num.neg(), mode);
    var div;
    if (mode !== 'mod')
      div = res.div.neg();
    return { div: div, mod: res.mod };
  } else if ((this.negative & num.negative) !== 0) {
    var res = this.neg().divmod(num.neg(), mode);
    var mod;
    if (mode !== 'div') {
      mod = res.mod.neg();
      if (positive && mod.neg)
        mod = mod.isub(num);
    }
    return {
      div: res.div,
      mod: mod
    };
  }

  // Both numbers are positive at this point

  // Strip both numbers to approximate shift value
  if (num.length > this.length || this.cmp(num) < 0)
    return { div: new BN(0), mod: this };

  // Very short reduction
  if (num.length === 1) {
    if (mode === 'div')
      return { div: this.divn(num.words[0]), mod: null };
    else if (mode === 'mod')
      return { div: null, mod: new BN(this.modn(num.words[0])) };
    return {
      div: this.divn(num.words[0]),
      mod: new BN(this.modn(num.words[0]))
    };
  }

  return this._wordDiv(num, mode);
};

// Find `this` / `num`
BN.prototype.div = function div(num) {
  return this.divmod(num, 'div', false).div;
};

// Find `this` % `num`
BN.prototype.mod = function mod(num) {
  return this.divmod(num, 'mod', false).mod;
};

BN.prototype.umod = function umod(num) {
  return this.divmod(num, 'mod', true).mod;
};

// Find Round(`this` / `num`)
BN.prototype.divRound = function divRound(num) {
  var dm = this.divmod(num);

  // Fast case - exact division
  if (dm.mod.cmpn(0) === 0)
    return dm.div;

  var mod = dm.div.negative !== 0 ? dm.mod.isub(num) : dm.mod;

  var half = num.ushrn(1);
  var r2 = num.andln(1);
  var cmp = mod.cmp(half);

  // Round down
  if (cmp < 0 || r2 === 1 && cmp === 0)
    return dm.div;

  // Round up
  return dm.div.negative !== 0 ? dm.div.isubn(1) : dm.div.iaddn(1);
};

BN.prototype.modn = function modn(num) {
  var p = (1 << 26) % num;

  var acc = 0;
  for (var i = this.length - 1; i >= 0; i--)
    acc = (p * acc + (this.words[i] | 0)) % num;

  return acc;
};

// In-place division by number
BN.prototype.idivn = function idivn(num) {
  var carry = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var w = (this.words[i] | 0) + carry * 0x4000000;
    this.words[i] = (w / num) | 0;
    carry = w % num;
  }

  return this.strip();
};

BN.prototype.divn = function divn(num) {
  return this.clone().idivn(num);
};

BN.prototype.isEven = function isEven() {
  return (this.words[0] & 1) === 0;
};

BN.prototype.isOdd = function isOdd() {
  return (this.words[0] & 1) === 1;
};

// And first word and num
BN.prototype.andln = function andln(num) {
  return this.words[0] & num;
};

BN.prototype.cmpn = function cmpn(num) {
  var negative = num < 0;
  if (negative)
    num = -num;

  if (this.negative !== 0 && !negative)
    return -1;
  else if (this.negative === 0 && negative)
    return 1;

  num &= 0x3ffffff;
  this.strip();

  var res;
  if (this.length > 1) {
    res = 1;
  } else {
    var w = this.words[0] | 0;
    res = w === num ? 0 : w < num ? -1 : 1;
  }
  if (this.negative !== 0)
    res = -res;
  return res;
};

// Compare two numbers and return:
// 1 - if `this` > `num`
// 0 - if `this` == `num`
// -1 - if `this` < `num`
BN.prototype.cmp = function cmp(num) {
  if (this.negative !== 0 && num.negative === 0)
    return -1;
  else if (this.negative === 0 && num.negative !== 0)
    return 1;

  var res = this.ucmp(num);
  if (this.negative !== 0)
    return -res;
  else
    return res;
};

// Unsigned comparison
BN.prototype.ucmp = function ucmp(num) {
  // At this point both numbers have the same sign
  if (this.length > num.length)
    return 1;
  else if (this.length < num.length)
    return -1;

  var res = 0;
  for (var i = this.length - 1; i >= 0; i--) {
    var a = this.words[i] | 0;
    var b = num.words[i] | 0;

    if (a === b)
      continue;
    if (a < b)
      res = -1;
    else if (a > b)
      res = 1;
    break;
  }
  return res;
};
})(undefined, __bn);

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return {_:0, a:0, b:undefined};
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return {_:0, a:1, b:val};
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;
var __stable_table;

function makeStableName(x) {
    if(x instanceof Object) {
        if(!x.stableName) {
            x.stableName = __next_stable_name;
            __next_stable_name += 1;
        }
        return {type: 'obj', name: x.stableName};
    } else {
        return {type: 'prim', name: Number(x)};
    }
}

function eqStableName(x, y) {
    return (x.type == y.type && x.name == y.name) ? 1 : 0;
}

// TODO: inefficient compared to real fromInt?
__bn.Z = new __bn.BN(0);
__bn.ONE = new __bn.BN(1);
__bn.MOD32 = new __bn.BN(0x100000000); // 2^32
var I_fromNumber = function(x) {return new __bn.BN(x);}
var I_fromInt = I_fromNumber;
var I_fromBits = function(lo,hi) {
    var x = new __bn.BN(lo >>> 0);
    var y = new __bn.BN(hi >>> 0);
    y.ishln(32);
    x.iadd(y);
    return x;
}
var I_fromString = function(s) {return new __bn.BN(s);}
var I_toInt = function(x) {return I_toNumber(x.mod(__bn.MOD32));}
var I_toWord = function(x) {return I_toInt(x) >>> 0;};
// TODO: inefficient!
var I_toNumber = function(x) {return Number(x.toString());}
var I_equals = function(a,b) {return a.cmp(b) === 0;}
var I_compare = function(a,b) {return a.cmp(b);}
var I_compareInt = function(x,i) {return x.cmp(new __bn.BN(i));}
var I_negate = function(x) {return x.neg();}
var I_add = function(a,b) {return a.add(b);}
var I_sub = function(a,b) {return a.sub(b);}
var I_mul = function(a,b) {return a.mul(b);}
var I_mod = function(a,b) {return I_rem(I_add(b, I_rem(a, b)), b);}
var I_quotRem = function(a,b) {
    var qr = a.divmod(b);
    return {_:0, a:qr.div, b:qr.mod};
}
var I_div = function(a,b) {
    if((a.cmp(__bn.Z)>=0) != (a.cmp(__bn.Z)>=0)) {
        if(a.cmp(a.rem(b), __bn.Z) !== 0) {
            return a.div(b).sub(__bn.ONE);
        }
    }
    return a.div(b);
}
var I_divMod = function(a,b) {
    return {_:0, a:I_div(a,b), b:a.mod(b)};
}
var I_quot = function(a,b) {return a.div(b);}
var I_rem = function(a,b) {return a.mod(b);}
var I_and = function(a,b) {return a.and(b);}
var I_or = function(a,b) {return a.or(b);}
var I_xor = function(a,b) {return a.xor(b);}
var I_shiftLeft = function(a,b) {return a.shln(b);}
var I_shiftRight = function(a,b) {return a.shrn(b);}
var I_signum = function(x) {return x.cmp(new __bn.BN(0));}
var I_abs = function(x) {return x.abs();}
var I_decodeDouble = function(x) {
    var dec = decodeDouble(x);
    var mantissa = I_fromBits(dec.c, dec.b);
    if(dec.a < 0) {
        mantissa = I_negate(mantissa);
    }
    return {_:0, a:dec.d, b:mantissa};
}
var I_toString = function(x) {return x.toString();}
var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    if(x.isNegative()) {
        return I_negate(I_fromInt64(x.negate()));
    } else {
        return I_fromBits(x.low, x.high);
    }
}

function I_toInt64(x) {
    if(x.negative) {
        return I_toInt64(I_negate(x)).negate();
    } else {
        return new Long(I_toInt(x), I_toInt(I_shiftRight(x,32)));
    }
}

function I_fromWord64(x) {
    return I_fromBits(x.toInt(), x.shru(32).toInt());
}

function I_toWord64(x) {
    var w = I_toInt64(x);
    w.unsigned = true;
    return w;
}

/**
 * @license long.js (c) 2013 Daniel Wirtz <dcode@dcode.io>
 * Released under the Apache License, Version 2.0
 * see: https://github.com/dcodeIO/long.js for details
 */
function Long(low, high, unsigned) {
    this.low = low | 0;
    this.high = high | 0;
    this.unsigned = !!unsigned;
}

var INT_CACHE = {};
var UINT_CACHE = {};
function cacheable(x, u) {
    return u ? 0 <= (x >>>= 0) && x < 256 : -128 <= (x |= 0) && x < 128;
}

function __fromInt(value, unsigned) {
    var obj, cachedObj, cache;
    if (unsigned) {
        if (cache = cacheable(value >>>= 0, true)) {
            cachedObj = UINT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache)
            UINT_CACHE[value] = obj;
        return obj;
    } else {
        if (cache = cacheable(value |= 0, false)) {
            cachedObj = INT_CACHE[value];
            if (cachedObj)
                return cachedObj;
        }
        obj = new Long(value, value < 0 ? -1 : 0, false);
        if (cache)
            INT_CACHE[value] = obj;
        return obj;
    }
}

function __fromNumber(value, unsigned) {
    if (isNaN(value) || !isFinite(value))
        return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0)
            return UZERO;
        if (value >= TWO_PWR_64_DBL)
            return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL)
            return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL)
            return MAX_VALUE;
    }
    if (value < 0)
        return __fromNumber(-value, unsigned).neg();
    return new Long((value % TWO_PWR_32_DBL) | 0, (value / TWO_PWR_32_DBL) | 0, unsigned);
}
var pow_dbl = Math.pow;
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = __fromInt(TWO_PWR_24_DBL);
var ZERO = __fromInt(0);
Long.ZERO = ZERO;
var UZERO = __fromInt(0, true);
Long.UZERO = UZERO;
var ONE = __fromInt(1);
Long.ONE = ONE;
var UONE = __fromInt(1, true);
Long.UONE = UONE;
var NEG_ONE = __fromInt(-1);
Long.NEG_ONE = NEG_ONE;
var MAX_VALUE = new Long(0xFFFFFFFF|0, 0x7FFFFFFF|0, false);
Long.MAX_VALUE = MAX_VALUE;
var MAX_UNSIGNED_VALUE = new Long(0xFFFFFFFF|0, 0xFFFFFFFF|0, true);
Long.MAX_UNSIGNED_VALUE = MAX_UNSIGNED_VALUE;
var MIN_VALUE = new Long(0, 0x80000000|0, false);
Long.MIN_VALUE = MIN_VALUE;
var __lp = Long.prototype;
__lp.toInt = function() {return this.unsigned ? this.low >>> 0 : this.low;};
__lp.toNumber = function() {
    if (this.unsigned)
        return ((this.high >>> 0) * TWO_PWR_32_DBL) + (this.low >>> 0);
    return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
};
__lp.isZero = function() {return this.high === 0 && this.low === 0;};
__lp.isNegative = function() {return !this.unsigned && this.high < 0;};
__lp.isOdd = function() {return (this.low & 1) === 1;};
__lp.eq = function(other) {
    if (this.unsigned !== other.unsigned && (this.high >>> 31) === 1 && (other.high >>> 31) === 1)
        return false;
    return this.high === other.high && this.low === other.low;
};
__lp.neq = function(other) {return !this.eq(other);};
__lp.lt = function(other) {return this.comp(other) < 0;};
__lp.lte = function(other) {return this.comp(other) <= 0;};
__lp.gt = function(other) {return this.comp(other) > 0;};
__lp.gte = function(other) {return this.comp(other) >= 0;};
__lp.compare = function(other) {
    if (this.eq(other))
        return 0;
    var thisNeg = this.isNegative(),
        otherNeg = other.isNegative();
    if (thisNeg && !otherNeg)
        return -1;
    if (!thisNeg && otherNeg)
        return 1;
    if (!this.unsigned)
        return this.sub(other).isNegative() ? -1 : 1;
    return (other.high >>> 0) > (this.high >>> 0) || (other.high === this.high && (other.low >>> 0) > (this.low >>> 0)) ? -1 : 1;
};
__lp.comp = __lp.compare;
__lp.negate = function() {
    if (!this.unsigned && this.eq(MIN_VALUE))
        return MIN_VALUE;
    return this.not().add(ONE);
};
__lp.neg = __lp.negate;
__lp.add = function(addend) {
    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = addend.high >>> 16;
    var b32 = addend.high & 0xFFFF;
    var b16 = addend.low >>> 16;
    var b00 = addend.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 + b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 + b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 + b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 + b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.subtract = function(subtrahend) {return this.add(subtrahend.neg());};
__lp.sub = __lp.subtract;
__lp.multiply = function(multiplier) {
    if (this.isZero())
        return ZERO;
    if (multiplier.isZero())
        return ZERO;
    if (this.eq(MIN_VALUE))
        return multiplier.isOdd() ? MIN_VALUE : ZERO;
    if (multiplier.eq(MIN_VALUE))
        return this.isOdd() ? MIN_VALUE : ZERO;

    if (this.isNegative()) {
        if (multiplier.isNegative())
            return this.neg().mul(multiplier.neg());
        else
            return this.neg().mul(multiplier).neg();
    } else if (multiplier.isNegative())
        return this.mul(multiplier.neg()).neg();

    if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24))
        return __fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);

    var a48 = this.high >>> 16;
    var a32 = this.high & 0xFFFF;
    var a16 = this.low >>> 16;
    var a00 = this.low & 0xFFFF;

    var b48 = multiplier.high >>> 16;
    var b32 = multiplier.high & 0xFFFF;
    var b16 = multiplier.low >>> 16;
    var b00 = multiplier.low & 0xFFFF;

    var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
    c00 += a00 * b00;
    c16 += c00 >>> 16;
    c00 &= 0xFFFF;
    c16 += a16 * b00;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c16 += a00 * b16;
    c32 += c16 >>> 16;
    c16 &= 0xFFFF;
    c32 += a32 * b00;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a16 * b16;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c32 += a00 * b32;
    c48 += c32 >>> 16;
    c32 &= 0xFFFF;
    c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
    c48 &= 0xFFFF;
    return new Long((c16 << 16) | c00, (c48 << 16) | c32, this.unsigned);
};
__lp.mul = __lp.multiply;
__lp.divide = function(divisor) {
    if (divisor.isZero())
        throw Error('division by zero');
    if (this.isZero())
        return this.unsigned ? UZERO : ZERO;
    var approx, rem, res;
    if (this.eq(MIN_VALUE)) {
        if (divisor.eq(ONE) || divisor.eq(NEG_ONE))
            return MIN_VALUE;
        else if (divisor.eq(MIN_VALUE))
            return ONE;
        else {
            var halfThis = this.shr(1);
            approx = halfThis.div(divisor).shl(1);
            if (approx.eq(ZERO)) {
                return divisor.isNegative() ? ONE : NEG_ONE;
            } else {
                rem = this.sub(divisor.mul(approx));
                res = approx.add(rem.div(divisor));
                return res;
            }
        }
    } else if (divisor.eq(MIN_VALUE))
        return this.unsigned ? UZERO : ZERO;
    if (this.isNegative()) {
        if (divisor.isNegative())
            return this.neg().div(divisor.neg());
        return this.neg().div(divisor).neg();
    } else if (divisor.isNegative())
        return this.div(divisor.neg()).neg();

    res = ZERO;
    rem = this;
    while (rem.gte(divisor)) {
        approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
        var log2 = Math.ceil(Math.log(approx) / Math.LN2),
            delta = (log2 <= 48) ? 1 : pow_dbl(2, log2 - 48),
            approxRes = __fromNumber(approx),
            approxRem = approxRes.mul(divisor);
        while (approxRem.isNegative() || approxRem.gt(rem)) {
            approx -= delta;
            approxRes = __fromNumber(approx, this.unsigned);
            approxRem = approxRes.mul(divisor);
        }
        if (approxRes.isZero())
            approxRes = ONE;

        res = res.add(approxRes);
        rem = rem.sub(approxRem);
    }
    return res;
};
__lp.div = __lp.divide;
__lp.modulo = function(divisor) {return this.sub(this.div(divisor).mul(divisor));};
__lp.mod = __lp.modulo;
__lp.not = function not() {return new Long(~this.low, ~this.high, this.unsigned);};
__lp.and = function(other) {return new Long(this.low & other.low, this.high & other.high, this.unsigned);};
__lp.or = function(other) {return new Long(this.low | other.low, this.high | other.high, this.unsigned);};
__lp.xor = function(other) {return new Long(this.low ^ other.low, this.high ^ other.high, this.unsigned);};

__lp.shl = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long(this.low << numBits, (this.high << numBits) | (this.low >>> (32 - numBits)), this.unsigned);
    else
        return new Long(0, this.low << (numBits - 32), this.unsigned);
};

__lp.shr = function(numBits) {
    if ((numBits &= 63) === 0)
        return this;
    else if (numBits < 32)
        return new Long((this.low >>> numBits) | (this.high << (32 - numBits)), this.high >> numBits, this.unsigned);
    else
        return new Long(this.high >> (numBits - 32), this.high >= 0 ? 0 : -1, this.unsigned);
};

__lp.shru = function(numBits) {
    numBits &= 63;
    if (numBits === 0)
        return this;
    else {
        var high = this.high;
        if (numBits < 32) {
            var low = this.low;
            return new Long((low >>> numBits) | (high << (32 - numBits)), high >>> numBits, this.unsigned);
        } else if (numBits === 32)
            return new Long(high, 0, this.unsigned);
        else
            return new Long(high >>> (numBits - 32), 0, this.unsigned);
    }
};

__lp.toSigned = function() {return this.unsigned ? new Long(this.low, this.high, false) : this;};
__lp.toUnsigned = function() {return this.unsigned ? this : new Long(this.low, this.high, true);};

// Int64
function hs_eqInt64(x, y) {return x.eq(y);}
function hs_neInt64(x, y) {return x.neq(y);}
function hs_ltInt64(x, y) {return x.lt(y);}
function hs_leInt64(x, y) {return x.lte(y);}
function hs_gtInt64(x, y) {return x.gt(y);}
function hs_geInt64(x, y) {return x.gte(y);}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shl(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shr(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shru(bits);}
function hs_int64ToInt(x) {return x.toInt();}
var hs_intToInt64 = __fromInt;

// Word64
function hs_wordToWord64(x) {return __fromInt(x, true);}
function hs_word64ToWord(x) {return x.toInt(x);}
function hs_mkWord64(low, high) {return new Long(low,high,true);}
function hs_and64(a,b) {return a.and(b);};
function hs_or64(a,b) {return a.or(b);};
function hs_xor64(a,b) {return a.xor(b);};
function hs_not64(x) {return x.not();}
var hs_eqWord64 = hs_eqInt64;
var hs_neWord64 = hs_neInt64;
var hs_ltWord64 = hs_ltInt64;
var hs_leWord64 = hs_leInt64;
var hs_gtWord64 = hs_gtInt64;
var hs_geWord64 = hs_geInt64;
var hs_quotWord64 = hs_quotInt64;
var hs_remWord64 = hs_remInt64;
var hs_uncheckedShiftL64 = hs_uncheckedIShiftL64;
var hs_uncheckedShiftRL64 = hs_uncheckedIShiftRL64;
function hs_int64ToWord64(x) {return x.toUnsigned();}
function hs_word64ToInt64(x) {return x.toSigned();}

// Joseph Myers' MD5 implementation, ported to work on typed arrays.
// Used under the BSD license.
function md5cycle(x, k) {
    var a = x[0], b = x[1], c = x[2], d = x[3];

    a = ff(a, b, c, d, k[0], 7, -680876936);
    d = ff(d, a, b, c, k[1], 12, -389564586);
    c = ff(c, d, a, b, k[2], 17,  606105819);
    b = ff(b, c, d, a, k[3], 22, -1044525330);
    a = ff(a, b, c, d, k[4], 7, -176418897);
    d = ff(d, a, b, c, k[5], 12,  1200080426);
    c = ff(c, d, a, b, k[6], 17, -1473231341);
    b = ff(b, c, d, a, k[7], 22, -45705983);
    a = ff(a, b, c, d, k[8], 7,  1770035416);
    d = ff(d, a, b, c, k[9], 12, -1958414417);
    c = ff(c, d, a, b, k[10], 17, -42063);
    b = ff(b, c, d, a, k[11], 22, -1990404162);
    a = ff(a, b, c, d, k[12], 7,  1804603682);
    d = ff(d, a, b, c, k[13], 12, -40341101);
    c = ff(c, d, a, b, k[14], 17, -1502002290);
    b = ff(b, c, d, a, k[15], 22,  1236535329);

    a = gg(a, b, c, d, k[1], 5, -165796510);
    d = gg(d, a, b, c, k[6], 9, -1069501632);
    c = gg(c, d, a, b, k[11], 14,  643717713);
    b = gg(b, c, d, a, k[0], 20, -373897302);
    a = gg(a, b, c, d, k[5], 5, -701558691);
    d = gg(d, a, b, c, k[10], 9,  38016083);
    c = gg(c, d, a, b, k[15], 14, -660478335);
    b = gg(b, c, d, a, k[4], 20, -405537848);
    a = gg(a, b, c, d, k[9], 5,  568446438);
    d = gg(d, a, b, c, k[14], 9, -1019803690);
    c = gg(c, d, a, b, k[3], 14, -187363961);
    b = gg(b, c, d, a, k[8], 20,  1163531501);
    a = gg(a, b, c, d, k[13], 5, -1444681467);
    d = gg(d, a, b, c, k[2], 9, -51403784);
    c = gg(c, d, a, b, k[7], 14,  1735328473);
    b = gg(b, c, d, a, k[12], 20, -1926607734);

    a = hh(a, b, c, d, k[5], 4, -378558);
    d = hh(d, a, b, c, k[8], 11, -2022574463);
    c = hh(c, d, a, b, k[11], 16,  1839030562);
    b = hh(b, c, d, a, k[14], 23, -35309556);
    a = hh(a, b, c, d, k[1], 4, -1530992060);
    d = hh(d, a, b, c, k[4], 11,  1272893353);
    c = hh(c, d, a, b, k[7], 16, -155497632);
    b = hh(b, c, d, a, k[10], 23, -1094730640);
    a = hh(a, b, c, d, k[13], 4,  681279174);
    d = hh(d, a, b, c, k[0], 11, -358537222);
    c = hh(c, d, a, b, k[3], 16, -722521979);
    b = hh(b, c, d, a, k[6], 23,  76029189);
    a = hh(a, b, c, d, k[9], 4, -640364487);
    d = hh(d, a, b, c, k[12], 11, -421815835);
    c = hh(c, d, a, b, k[15], 16,  530742520);
    b = hh(b, c, d, a, k[2], 23, -995338651);

    a = ii(a, b, c, d, k[0], 6, -198630844);
    d = ii(d, a, b, c, k[7], 10,  1126891415);
    c = ii(c, d, a, b, k[14], 15, -1416354905);
    b = ii(b, c, d, a, k[5], 21, -57434055);
    a = ii(a, b, c, d, k[12], 6,  1700485571);
    d = ii(d, a, b, c, k[3], 10, -1894986606);
    c = ii(c, d, a, b, k[10], 15, -1051523);
    b = ii(b, c, d, a, k[1], 21, -2054922799);
    a = ii(a, b, c, d, k[8], 6,  1873313359);
    d = ii(d, a, b, c, k[15], 10, -30611744);
    c = ii(c, d, a, b, k[6], 15, -1560198380);
    b = ii(b, c, d, a, k[13], 21,  1309151649);
    a = ii(a, b, c, d, k[4], 6, -145523070);
    d = ii(d, a, b, c, k[11], 10, -1120210379);
    c = ii(c, d, a, b, k[2], 15,  718787259);
    b = ii(b, c, d, a, k[9], 21, -343485551);

    x[0] = add32(a, x[0]);
    x[1] = add32(b, x[1]);
    x[2] = add32(c, x[2]);
    x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
    a = add32(add32(a, q), add32(x, t));
    return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
    return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
    return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
    return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
    return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s, n) {
    var a = s['v']['w8'];
    var orig_n = n,
        state = [1732584193, -271733879, -1732584194, 271733878], i;
    for (i=64; i<=n; i+=64) {
        md5cycle(state, md5blk(a.subarray(i-64, i)));
    }
    a = a.subarray(i-64);
    n = n < (i-64) ? 0 : n-(i-64);
    var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
    for (i=0; i<n; i++)
        tail[i>>2] |= a[i] << ((i%4) << 3);
    tail[i>>2] |= 0x80 << ((i%4) << 3);
    if (i > 55) {
        md5cycle(state, tail);
        for (i=0; i<16; i++) tail[i] = 0;
    }
    tail[14] = orig_n*8;
    md5cycle(state, tail);
    return state;
}
window['md51'] = md51;

function md5blk(s) {
    var md5blks = [], i;
    for (i=0; i<64; i+=4) {
        md5blks[i>>2] = s[i]
            + (s[i+1] << 8)
            + (s[i+2] << 16)
            + (s[i+3] << 24);
    }
    return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
    var s='', j=0;
    for(; j<4; j++)
        s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
        + hex_chr[(n >> (j * 8)) & 0x0F];
    return s;
}

function hex(x) {
    for (var i=0; i<x.length; i++)
        x[i] = rhex(x[i]);
    return x.join('');
}

function md5(s, n) {
    return hex(md51(s, n));
}

window['md5'] = md5;

function add32(a, b) {
    return (a + b) & 0xFFFFFFFF;
}

function __hsbase_MD5Init(ctx) {}
// Note that this is a one time "update", since that's all that's used by
// GHC.Fingerprint.
function __hsbase_MD5Update(ctx, s, n) {
    ctx.md5 = md51(s, n);
}
function __hsbase_MD5Final(out, ctx) {
    var a = out['v']['i32'];
    a[0] = ctx.md5[0];
    a[1] = ctx.md5[1];
    a[2] = ctx.md5[2];
    a[3] = ctx.md5[3];
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = new Array(n);
    for(var i = 0; i < n; ++i) {
        arr[i] = x;
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    return new ByteArray(new ArrayBuffer(n));
}

// Wrap a JS ArrayBuffer into a ByteArray. Truncates the array length to the
// closest multiple of 8 bytes.
function wrapByteArr(buffer) {
    var diff = buffer.byteLength % 8;
    if(diff != 0) {
        var buffer = buffer.slice(0, buffer.byteLength-diff);
    }
    return new ByteArray(buffer);
}

function ByteArray(buffer) {
    var views =
        { 'i8' : new Int8Array(buffer)
        , 'i16': new Int16Array(buffer)
        , 'i32': new Int32Array(buffer)
        , 'w8' : new Uint8Array(buffer)
        , 'w16': new Uint16Array(buffer)
        , 'w32': new Uint32Array(buffer)
        , 'f32': new Float32Array(buffer)
        , 'f64': new Float64Array(buffer)
        };
    this['b'] = buffer;
    this['v'] = views;
    this['off'] = 0;
}
window['newArr'] = newArr;
window['newByteArr'] = newByteArr;
window['wrapByteArr'] = wrapByteArr;
window['ByteArray'] = ByteArray;

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function writeOffAddr64(addr, off, x) {
    addr['v']['w32'][addr.off/8 + off*2] = x.low;
    addr['v']['w32'][addr.off/8 + off*2 + 1] = x.high;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

function readOffAddr64(signed, addr, off) {
    var w64 = hs_mkWord64( addr['v']['w32'][addr.off/8 + off*2]
                         , addr['v']['w32'][addr.off/8 + off*2 + 1]);
    return signed ? hs_word64ToInt64(w64) : w64;
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

// "Weak Pointers". Mostly useless implementation since
// JS does its own GC.

function mkWeak(key, val, fin) {
    fin = !fin? function() {}: fin;
    return {key: key, val: val, fin: fin};
}

function derefWeak(w) {
    return {_:0, a:1, b:E(w).val};
}

function finalizeWeak(w) {
    return {_:0, a:B(A1(E(w).fin, __Z))};
}

/* For foreign import ccall "wrapper" */
function createAdjustor(args, f, a, b) {
    return function(){
        var x = f.apply(null, arguments);
        while(x instanceof F) {x = x.f();}
        return x;
    };
}

var __apply = function(f,as) {
    var arr = [];
    for(; as._ === 1; as = as.b) {
        arr.push(as.a);
    }
    arr.reverse();
    return f.apply(null, arr);
}
var __app0 = function(f) {return f();}
var __app1 = function(f,a) {return f(a);}
var __app2 = function(f,a,b) {return f(a,b);}
var __app3 = function(f,a,b,c) {return f(a,b,c);}
var __app4 = function(f,a,b,c,d) {return f(a,b,c,d);}
var __app5 = function(f,a,b,c,d,e) {return f(a,b,c,d,e);}
var __jsNull = function() {return null;}
var __eq = function(a,b) {return a===b;}
var __createJSFunc = function(arity, f){
    if(f instanceof Function && arity === f.length) {
        return (function() {
            var x = f.apply(null,arguments);
            if(x instanceof T) {
                if(x.f !== __blackhole) {
                    var ff = x.f;
                    x.f = __blackhole;
                    return x.x = ff();
                }
                return x.x;
            } else {
                while(x instanceof F) {
                    x = x.f();
                }
                return E(x);
            }
        });
    } else {
        return (function(){
            var as = Array.prototype.slice.call(arguments);
            as.push(0);
            return E(B(A(f,as)));
        });
    }
}


function __arr2lst(elem,arr) {
    if(elem >= arr.length) {
        return __Z;
    }
    return {_:1,
            a:arr[elem],
            b:new T(function(){return __arr2lst(elem+1,arr);})};
}

function __lst2arr(xs) {
    var arr = [];
    xs = E(xs);
    for(;xs._ === 1; xs = E(xs.b)) {
        arr.push(E(xs.a));
    }
    return arr;
}

var __new = function() {return ({});}
var __set = function(o,k,v) {o[k]=v;}
var __get = function(o,k) {return o[k];}
var __has = function(o,k) {return o[k]!==undefined;}

var _0/* () */ = 0,
_1/* $fFromAny()4 */ = function(_/* EXTERNAL */){
  return _0/* GHC.Tuple.() */;
},
_2/* resize2 */ = "(function (jq) { jq.resize(); })",
_3/* $wa18 */ = function(_4/* s9N8 */, _/* EXTERNAL */){
  var _5/* s9Nd */ = eval/* EXTERNAL */(E(_2/* JQuery.resize2 */)),
  _6/* s9Nl */ = __app1/* EXTERNAL */(E(_5/* s9Nd */), _4/* s9N8 */);
  return _4/* s9N8 */;
},
_7/* eqString */ = function(_8/* s3mQ */, _9/* s3mR */){
  while(1){
    var _a/* s3mS */ = E(_8/* s3mQ */);
    if(!_a/* s3mS */._){
      return (E(_9/* s3mR */)._==0) ? true : false;
    }else{
      var _b/* s3mY */ = E(_9/* s3mR */);
      if(!_b/* s3mY */._){
        return false;
      }else{
        if(E(_a/* s3mS */.a)!=E(_b/* s3mY */.a)){
          return false;
        }else{
          _8/* s3mQ */ = _a/* s3mS */.b;
          _9/* s3mR */ = _b/* s3mY */.b;
          continue;
        }
      }
    }
  }
},
_c/* $w$c==2 */ = function(_d/* s4y9 */, _e/* s4ya */, _f/* s4yb */, _g/* s4yc */){
  switch(E(_d/* s4y9 */)){
    case 0:
      if(!E(_f/* s4yb */)){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 1:
      if(E(_f/* s4yb */)==1){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 2:
      if(E(_f/* s4yb */)==2){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 3:
      if(E(_f/* s4yb */)==3){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 4:
      if(E(_f/* s4yb */)==4){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 5:
      if(E(_f/* s4yb */)==5){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 6:
      if(E(_f/* s4yb */)==6){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 7:
      if(E(_f/* s4yb */)==7){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    case 8:
      if(E(_f/* s4yb */)==8){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
      break;
    default:
      if(E(_f/* s4yb */)==9){
        return new F(function(){return _7/* GHC.Base.eqString */(_e/* s4ya */, _g/* s4yc */);});
      }else{
        return false;
      }
  }
},
_h/* $w$c==1 */ = function(_i/* s8ZK */, _j/* s8ZL */, _k/* s8ZM */, _l/* s8ZN */, _m/* s8ZO */, _n/* s8ZP */){
  if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_i/* s8ZK */, _j/* s8ZL */, _l/* s8ZN */, _m/* s8ZO */))){
    return false;
  }else{
    var _o/* s8ZR */ = E(_k/* s8ZM */);
    if(!_o/* s8ZR */._){
      return (E(_n/* s8ZP */)._==0) ? true : false;
    }else{
      var _p/* s8ZV */ = E(_n/* s8ZP */);
      if(!_p/* s8ZV */._){
        return false;
      }else{
        return new F(function(){return _7/* GHC.Base.eqString */(_o/* s8ZR */.a, _p/* s8ZV */.a);});
      }
    }
  }
},
_q/* $fEqOUAssocInst_$c==1 */ = function(_r/* s8ZX */, _s/* s8ZY */){
  var _t/* s8ZZ */ = E(_r/* s8ZX */),
  _u/* s902 */ = E(_t/* s8ZZ */.a),
  _v/* s905 */ = E(_s/* s8ZY */),
  _w/* s908 */ = E(_v/* s905 */.a);
  return new F(function(){return _h/* Metamodel.UfoAInst.$w$c==1 */(_u/* s902 */.a, _u/* s902 */.b, _t/* s8ZZ */.b, _w/* s908 */.a, _w/* s908 */.b, _v/* s905 */.b);});
},
_x/* $w$c== */ = function(_y/* s4yF */, _z/* s4yG */, _A/* s4yH */, _B/* s4yI */, _C/* s4yJ */, _D/* s4yK */, _E/* s4yL */, _F/* s4yM */, _G/* s4yN */, _H/* s4yO */){
  var _I/* s4yP */ = function(_J/* s4yQ */){
    var _K/* s4yR */ = function(_L/* s4yS */){
      var _M/* s4yT */ = E(_B/* s4yI */),
      _N/* s4yW */ = E(_G/* s4yN */),
      _O/* s4yZ */ = E(_M/* s4yT */.a),
      _P/* s4z2 */ = E(_N/* s4yW */.a);
      if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_O/* s4yZ */.a, _O/* s4yZ */.b, _P/* s4z2 */.a, _P/* s4z2 */.b))){
        return false;
      }else{
        if(!B(_7/* GHC.Base.eqString */(_M/* s4yT */.b, _N/* s4yW */.b))){
          return false;
        }else{
          var _Q/* s4z7 */ = E(_C/* s4yJ */),
          _R/* s4za */ = E(_H/* s4yO */),
          _S/* s4zd */ = E(_Q/* s4z7 */.a),
          _T/* s4zg */ = E(_R/* s4za */.a);
          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_S/* s4zd */.a, _S/* s4zd */.b, _T/* s4zg */.a, _T/* s4zg */.b))){
            return false;
          }else{
            return new F(function(){return _7/* GHC.Base.eqString */(_Q/* s4z7 */.b, _R/* s4za */.b);});
          }
        }
      }
    },
    _U/* s4zk */ = E(_z/* s4yG */);
    if(!_U/* s4zk */._){
      if(!E(_E/* s4yL */)._){
        if(!B(_7/* GHC.Base.eqString */(_A/* s4yH */, _F/* s4yM */))){
          return false;
        }else{
          return new F(function(){return _K/* s4yR */(_/* EXTERNAL */);});
        }
      }else{
        return false;
      }
    }else{
      var _V/* s4zp */ = E(_E/* s4yL */);
      if(!_V/* s4zp */._){
        return false;
      }else{
        if(!B(_7/* GHC.Base.eqString */(_U/* s4zk */.a, _V/* s4zp */.a))){
          return false;
        }else{
          if(!B(_7/* GHC.Base.eqString */(_A/* s4yH */, _F/* s4yM */))){
            return false;
          }else{
            return new F(function(){return _K/* s4yR */(_/* EXTERNAL */);});
          }
        }
      }
    }
  },
  _W/* s4zt */ = E(_y/* s4yF */);
  switch(_W/* s4zt */._){
    case 0:
      if(!E(_D/* s4yK */)._){
        return new F(function(){return _I/* s4yP */(_/* EXTERNAL */);});
      }else{
        return false;
      }
      break;
    case 1:
      if(E(_D/* s4yK */)._==1){
        return new F(function(){return _I/* s4yP */(_/* EXTERNAL */);});
      }else{
        return false;
      }
      break;
    case 2:
      if(E(_D/* s4yK */)._==2){
        return new F(function(){return _I/* s4yP */(_/* EXTERNAL */);});
      }else{
        return false;
      }
      break;
    default:
      var _X/* s4zy */ = E(_D/* s4yK */);
      if(_X/* s4zy */._==3){
        if(!B(_7/* GHC.Base.eqString */(_W/* s4zt */.a, _X/* s4zy */.a))){
          return false;
        }else{
          return new F(function(){return _I/* s4yP */(_/* EXTERNAL */);});
        }
      }else{
        return false;
      }
  }
},
_Y/* $w$c== */ = function(_Z/* s91P */, _10/* s91Q */, _11/* s91R */, _12/* s91S */, _13/* s91T */, _14/* s91U */, _15/* s91V */, _16/* s91W */, _17/* s91X */, _18/* s91Y */, _19/* s91Z */, _1a/* s920 */, _1b/* s921 */, _1c/* s922 */){
  if(!B(_x/* Metamodel.UfoA.$w$c== */(_Z/* s91P */, _10/* s91Q */, _11/* s91R */, _12/* s91S */, _13/* s91T */, _16/* s91W */, _17/* s91X */, _18/* s91Y */, _19/* s91Z */, _1a/* s920 */))){
    return false;
  }else{
    var _1d/* s924 */ = E(_14/* s91U */),
    _1e/* s927 */ = E(_1d/* s924 */.a),
    _1f/* s92a */ = E(_1b/* s921 */),
    _1g/* s92c */ = _1f/* s92a */.b,
    _1h/* s92d */ = E(_1f/* s92a */.a);
    if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_1e/* s927 */.a, _1e/* s927 */.b, _1h/* s92d */.a, _1h/* s92d */.b))){
      return false;
    }else{
      var _1i/* s92h */ = E(_1d/* s924 */.b);
      if(!_1i/* s92h */._){
        if(!E(_1g/* s92c */)._){
          return new F(function(){return _q/* Metamodel.UfoAInst.$fEqOUAssocInst_$c==1 */(_15/* s91V */, _1c/* s922 */);});
        }else{
          return false;
        }
      }else{
        var _1j/* s92l */ = E(_1g/* s92c */);
        if(!_1j/* s92l */._){
          return false;
        }else{
          if(!B(_7/* GHC.Base.eqString */(_1i/* s92h */.a, _1j/* s92l */.a))){
            return false;
          }else{
            return new F(function(){return _q/* Metamodel.UfoAInst.$fEqOUAssocInst_$c==1 */(_15/* s91V */, _1c/* s922 */);});
          }
        }
      }
    }
  }
},
_1k/* $fEqOUAssocInst_$c/= */ = function(_1l/* s92K */, _1m/* s92L */){
  var _1n/* s92M */ = E(_1l/* s92K */),
  _1o/* s92Q */ = E(_1n/* s92M */.a),
  _1p/* s92W */ = E(_1m/* s92L */),
  _1q/* s930 */ = E(_1p/* s92W */.a);
  return (!B(_Y/* Metamodel.UfoAInst.$w$c== */(_1o/* s92Q */.a, _1o/* s92Q */.b, _1o/* s92Q */.c, _1o/* s92Q */.d, _1o/* s92Q */.e, _1n/* s92M */.b, _1n/* s92M */.c, _1q/* s930 */.a, _1q/* s930 */.b, _1q/* s930 */.c, _1q/* s930 */.d, _1q/* s930 */.e, _1p/* s92W */.b, _1p/* s92W */.c))) ? true : false;
},
_1r/* $fEqOUAssocInst_$c== */ = function(_1s/* s92o */, _1t/* s92p */){
  var _1u/* s92q */ = E(_1s/* s92o */),
  _1v/* s92u */ = E(_1u/* s92q */.a),
  _1w/* s92A */ = E(_1t/* s92p */),
  _1x/* s92E */ = E(_1w/* s92A */.a);
  return new F(function(){return _Y/* Metamodel.UfoAInst.$w$c== */(_1v/* s92u */.a, _1v/* s92u */.b, _1v/* s92u */.c, _1v/* s92u */.d, _1v/* s92u */.e, _1u/* s92q */.b, _1u/* s92q */.c, _1x/* s92E */.a, _1x/* s92E */.b, _1x/* s92E */.c, _1x/* s92E */.d, _1x/* s92E */.e, _1w/* s92A */.b, _1w/* s92A */.c);});
},
_1y/* $fEqOUAssocInst */ = new T2(0,_1r/* Metamodel.UfoAInst.$fEqOUAssocInst_$c== */,_1k/* Metamodel.UfoAInst.$fEqOUAssocInst_$c/= */),
_1z/* $fEqOUAssocPH_$c==1 */ = function(_1A/* s4BQ */, _1B/* s4BR */){
  switch(E(_1A/* s4BQ */)){
    case 0:
      return (E(_1B/* s4BR */)==0) ? true : false;
    case 1:
      return (E(_1B/* s4BR */)==1) ? true : false;
    case 2:
      return (E(_1B/* s4BR */)==2) ? true : false;
    default:
      return (E(_1B/* s4BR */)==3) ? true : false;
  }
},
_1C/* $w$c==1 */ = function(_1D/* s4BX */, _1E/* s4BY */, _1F/* s4BZ */, _1G/* s4C0 */, _1H/* s4C1 */, _1I/* s4C2 */, _1J/* s4C3 */, _1K/* s4C4 */, _1L/* s4C5 */, _1M/* s4C6 */){
  if(!B(_7/* GHC.Base.eqString */(_1D/* s4BX */, _1I/* s4C2 */))){
    return false;
  }else{
    if(!B(_7/* GHC.Base.eqString */(_1E/* s4BY */, _1J/* s4C3 */))){
      return false;
    }else{
      var _1N/* s4C9 */ = E(_1F/* s4BZ */),
      _1O/* s4Cc */ = E(_1K/* s4C4 */),
      _1P/* s4Cf */ = E(_1N/* s4C9 */.a),
      _1Q/* s4Ci */ = E(_1O/* s4Cc */.a);
      if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_1P/* s4Cf */.a, _1P/* s4Cf */.b, _1Q/* s4Ci */.a, _1Q/* s4Ci */.b))){
        return false;
      }else{
        if(!B(_7/* GHC.Base.eqString */(_1N/* s4C9 */.b, _1O/* s4Cc */.b))){
          return false;
        }else{
          var _1R/* s4Cn */ = E(_1G/* s4C0 */),
          _1S/* s4Cq */ = E(_1L/* s4C5 */),
          _1T/* s4Ct */ = E(_1R/* s4Cn */.a),
          _1U/* s4Cw */ = E(_1S/* s4Cq */.a);
          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_1T/* s4Ct */.a, _1T/* s4Ct */.b, _1U/* s4Cw */.a, _1U/* s4Cw */.b))){
            return false;
          }else{
            if(!B(_7/* GHC.Base.eqString */(_1R/* s4Cn */.b, _1S/* s4Cq */.b))){
              return false;
            }else{
              return new F(function(){return _1z/* Metamodel.UfoA.$fEqOUAssocPH_$c==1 */(_1H/* s4C1 */, _1M/* s4C6 */);});
            }
          }
        }
      }
    }
  }
},
_1V/* $w$c==2 */ = function(_1W/* s90x */, _1X/* s90y */, _1Y/* s90z */, _1Z/* s90A */, _20/* s90B */, _21/* s90C */, _22/* s90D */, _23/* s90E */, _24/* s90F */, _25/* s90G */, _26/* s90H */, _27/* s90I */, _28/* s90J */, _29/* s90K */){
  if(!B(_1C/* Metamodel.UfoA.$w$c==1 */(_1W/* s90x */, _1X/* s90y */, _1Y/* s90z */, _1Z/* s90A */, _20/* s90B */, _23/* s90E */, _24/* s90F */, _25/* s90G */, _26/* s90H */, _27/* s90I */))){
    return false;
  }else{
    var _2a/* s90M */ = E(_21/* s90C */),
    _2b/* s90P */ = E(_2a/* s90M */.a),
    _2c/* s90S */ = E(_28/* s90J */),
    _2d/* s90U */ = _2c/* s90S */.b,
    _2e/* s90V */ = E(_2c/* s90S */.a);
    if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_2b/* s90P */.a, _2b/* s90P */.b, _2e/* s90V */.a, _2e/* s90V */.b))){
      return false;
    }else{
      var _2f/* s90Z */ = E(_2a/* s90M */.b);
      if(!_2f/* s90Z */._){
        if(!E(_2d/* s90U */)._){
          return new F(function(){return _q/* Metamodel.UfoAInst.$fEqOUAssocInst_$c==1 */(_22/* s90D */, _29/* s90K */);});
        }else{
          return false;
        }
      }else{
        var _2g/* s913 */ = E(_2d/* s90U */);
        if(!_2g/* s913 */._){
          return false;
        }else{
          if(!B(_7/* GHC.Base.eqString */(_2f/* s90Z */.a, _2g/* s913 */.a))){
            return false;
          }else{
            return new F(function(){return _q/* Metamodel.UfoAInst.$fEqOUAssocInst_$c==1 */(_22/* s90D */, _29/* s90K */);});
          }
        }
      }
    }
  }
},
_2h/* $fEqOUAssocPHInst_$c/= */ = function(_2i/* s91s */, _2j/* s91t */){
  var _2k/* s91u */ = E(_2i/* s91s */),
  _2l/* s91y */ = E(_2k/* s91u */.a),
  _2m/* s91E */ = E(_2j/* s91t */),
  _2n/* s91I */ = E(_2m/* s91E */.a);
  return (!B(_1V/* Metamodel.UfoAInst.$w$c==2 */(_2l/* s91y */.a, _2l/* s91y */.b, _2l/* s91y */.c, _2l/* s91y */.d, _2l/* s91y */.e, _2k/* s91u */.b, _2k/* s91u */.c, _2n/* s91I */.a, _2n/* s91I */.b, _2n/* s91I */.c, _2n/* s91I */.d, _2n/* s91I */.e, _2m/* s91E */.b, _2m/* s91E */.c))) ? true : false;
},
_2o/* $fEqOUAssocPHInst_$c== */ = function(_2p/* s916 */, _2q/* s917 */){
  var _2r/* s918 */ = E(_2p/* s916 */),
  _2s/* s91c */ = E(_2r/* s918 */.a),
  _2t/* s91i */ = E(_2q/* s917 */),
  _2u/* s91m */ = E(_2t/* s91i */.a);
  return new F(function(){return _1V/* Metamodel.UfoAInst.$w$c==2 */(_2s/* s91c */.a, _2s/* s91c */.b, _2s/* s91c */.c, _2s/* s91c */.d, _2s/* s91c */.e, _2r/* s918 */.b, _2r/* s918 */.c, _2u/* s91m */.a, _2u/* s91m */.b, _2u/* s91m */.c, _2u/* s91m */.d, _2u/* s91m */.e, _2t/* s91i */.b, _2t/* s91i */.c);});
},
_2v/* $fEqOUAssocPHInst */ = new T2(0,_2o/* Metamodel.UfoAInst.$fEqOUAssocPHInst_$c== */,_2h/* Metamodel.UfoAInst.$fEqOUAssocPHInst_$c/= */),
_2w/* $wlvl */ = function(_2x/* s4sU */){
  var _2y/* s4sV */ = function(_2z/* s4sW */){
    var _2A/* s4sX */ = function(_2B/* s4sY */){
      if(_2x/* s4sU */<48){
        switch(E(_2x/* s4sU */)){
          case 45:
            return true;
          case 95:
            return true;
          default:
            return false;
        }
      }else{
        if(_2x/* s4sU */>57){
          switch(E(_2x/* s4sU */)){
            case 45:
              return true;
            case 95:
              return true;
            default:
              return false;
          }
        }else{
          return true;
        }
      }
    };
    if(_2x/* s4sU */<97){
      return new F(function(){return _2A/* s4sX */(_/* EXTERNAL */);});
    }else{
      if(_2x/* s4sU */>122){
        return new F(function(){return _2A/* s4sX */(_/* EXTERNAL */);});
      }else{
        return true;
      }
    }
  };
  if(_2x/* s4sU */<65){
    return new F(function(){return _2y/* s4sV */(_/* EXTERNAL */);});
  }else{
    if(_2x/* s4sU */>90){
      return new F(function(){return _2y/* s4sV */(_/* EXTERNAL */);});
    }else{
      return true;
    }
  }
},
_2C/* $fOUIdentifiedOUAssoc1 */ = function(_2D/* s4td */){
  return new F(function(){return _2w/* Metamodel.UfoA.$wlvl */(E(_2D/* s4td */));});
},
_2E/* $fOUIdentifiedOUAssocInst2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Assoc_"));
}),
_2F/* $fOUIdentifiedOUAssocInst1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": "));
}),
_2G/* ++ */ = function(_2H/* s3hJ */, _2I/* s3hK */){
  var _2J/* s3hL */ = E(_2H/* s3hJ */);
  return (_2J/* s3hL */._==0) ? E(_2I/* s3hK */) : new T2(1,_2J/* s3hL */.a,new T(function(){
    return B(_2G/* GHC.Base.++ */(_2J/* s3hL */.b, _2I/* s3hK */));
  }));
},
_2K/* $woueiLabel */ = function(_2L/* s8V6 */, _2M/* s8V7 */){
  var _2N/* s8V8 */ = E(_2M/* s8V7 */);
  if(!_2N/* s8V8 */._){
    return new F(function(){return _2G/* GHC.Base.++ */(_2F/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocInst1 */, new T(function(){
      return E(E(_2L/* s8V6 */).b);
    },1));});
  }else{
    var _2O/* s8Vi */ = new T(function(){
      return B(_2G/* GHC.Base.++ */(_2F/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocInst1 */, new T(function(){
        return E(E(_2L/* s8V6 */).b);
      },1)));
    },1);
    return new F(function(){return _2G/* GHC.Base.++ */(_2N/* s8V8 */.a, _2O/* s8Vi */);});
  }
},
_2P/* filter */ = function(_2Q/*  s9DD */, _2R/*  s9DE */){
  while(1){
    var _2S/*  filter */ = B((function(_2T/* s9DD */, _2U/* s9DE */){
      var _2V/* s9DF */ = E(_2U/* s9DE */);
      if(!_2V/* s9DF */._){
        return __Z/* EXTERNAL */;
      }else{
        var _2W/* s9DG */ = _2V/* s9DF */.a,
        _2X/* s9DH */ = _2V/* s9DF */.b;
        if(!B(A1(_2T/* s9DD */,_2W/* s9DG */))){
          var _2Y/*   s9DD */ = _2T/* s9DD */;
          _2Q/*  s9DD */ = _2Y/*   s9DD */;
          _2R/*  s9DE */ = _2X/* s9DH */;
          return __continue/* EXTERNAL */;
        }else{
          return new T2(1,_2W/* s9DG */,new T(function(){
            return B(_2P/* GHC.List.filter */(_2T/* s9DD */, _2X/* s9DH */));
          }));
        }
      }
    })(_2Q/*  s9DD */, _2R/*  s9DE */));
    if(_2S/*  filter */!=__continue/* EXTERNAL */){
      return _2S/*  filter */;
    }
  }
},
_2Z/* $w$couId */ = function(_30/* s8XG */, _31/* s8XH */){
  var _32/* s8Y0 */ = new T(function(){
    var _33/* s8XI */ = E(_30/* s8XG */),
    _34/* s8XL */ = new T(function(){
      var _35/* s8XM */ = E(_31/* s8XH */);
      return B(_2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_35/* s8XM */.a, _35/* s8XM */.b))));
    }),
    _36/* s8XQ */ = function(_37/*  s8XR */){
      while(1){
        var _38/*  s8XQ */ = B((function(_39/* s8XR */){
          var _3a/* s8XS */ = E(_39/* s8XR */);
          if(!_3a/* s8XS */._){
            return E(_34/* s8XL */);
          }else{
            var _3b/* s8XU */ = _3a/* s8XS */.b,
            _3c/* s8XV */ = E(_3a/* s8XS */.a);
            if(!B(_2w/* Metamodel.UfoA.$wlvl */(_3c/* s8XV */))){
              _37/*  s8XR */ = _3b/* s8XU */;
              return __continue/* EXTERNAL */;
            }else{
              return new T2(1,_3c/* s8XV */,new T(function(){
                return B(_36/* s8XQ */(_3b/* s8XU */));
              }));
            }
          }
        })(_37/*  s8XR */));
        if(_38/*  s8XQ */!=__continue/* EXTERNAL */){
          return _38/*  s8XQ */;
        }
      }
    };
    return B(_36/* s8XQ */(B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_33/* s8XI */.a, _33/* s8XI */.b))));
  },1);
  return new F(function(){return _2G/* GHC.Base.++ */(_2E/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocInst2 */, _32/* s8Y0 */);});
},
_3d/* appearJq5 */ = "(function (key, val, jq) { jq.css(key, val); return jq; })",
_3e/* $wa2 */ = function(_3f/* sa37 */, _3g/* sa38 */, _3h/* sa39 */, _/* EXTERNAL */){
  var _3i/* sa3o */ = eval/* EXTERNAL */(E(_3d/* JQuery.appearJq5 */));
  return new F(function(){return __app3/* EXTERNAL */(E(_3i/* sa3o */), toJSStr/* EXTERNAL */(E(_3f/* sa37 */)), toJSStr/* EXTERNAL */(E(_3g/* sa38 */)), _3h/* sa39 */);});
},
_3j/* selectSVG2 */ = "(function (selector, jq) { if (jq[0].contentDocument !== null) { var res = $(selector, jq[0].contentDocument.documentElement); if (res.length === 0) { console.warn(\'empty $ selection \' + selector); }; return res; } else {console.error(\'empty contentDocument\'); return jq;} })",
_3k/* $wa20 */ = function(_3l/* sa26 */, _3m/* sa27 */, _/* EXTERNAL */){
  var _3n/* sa2h */ = eval/* EXTERNAL */(E(_3j/* JQuery.selectSVG2 */));
  return new F(function(){return __app2/* EXTERNAL */(E(_3n/* sa2h */), toJSStr/* EXTERNAL */(E(_3l/* sa26 */)), _3m/* sa27 */);});
},
_3o/* diagAJq2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ufoa-inst-svg"));
}),
_3p/* getALine3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("#"));
}),
_3q/* hideJq2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("hidden"));
}),
_3r/* hideJq3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("visibility"));
}),
_3s/* selectById2 */ = "(function (id) { return $(\'#\' + id); })",
_3t/* selectById1 */ = function(_3u/* sa0a */, _/* EXTERNAL */){
  var _3v/* sa0k */ = eval/* EXTERNAL */(E(_3s/* JQuery.selectById2 */));
  return new F(function(){return __app1/* EXTERNAL */(E(_3v/* sa0k */), toJSStr/* EXTERNAL */(E(_3u/* sa0a */)));});
},
_3w/* a9 */ = function(_3x/*  scZx */, _/* EXTERNAL */){
  while(1){
    var _3y/*  a9 */ = B((function(_3z/* scZx */, _/* EXTERNAL */){
      var _3A/* scZz */ = E(_3z/* scZx */);
      if(!_3A/* scZz */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _3B/* scZC */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _3C/* scZN */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _3D/* scZH */ = E(_3A/* scZz */.a);
          return B(_2Z/* Metamodel.UfoAInst.$w$couId */(_3D/* scZH */.b, _3D/* scZH */.c));
        },1))), E(_3B/* scZC */), _/* EXTERNAL */)),
        _3E/* scZS */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _3q/* JQuery.hideJq2 */, E(_3C/* scZN */), _/* EXTERNAL */));
        _3x/*  scZx */ = _3A/* scZz */.b;
        return __continue/* EXTERNAL */;
      }
    })(_3x/*  scZx */, _/* EXTERNAL */));
    if(_3y/*  a9 */!=__continue/* EXTERNAL */){
      return _3y/*  a9 */;
    }
  }
},
_3F/* $sa */ = function(_3G/* scZe */, _3H/* scZf */, _3I/* scZg */, _/* EXTERNAL */){
  var _3J/* scZi */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
  _3K/* scZp */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
    return B(_2Z/* Metamodel.UfoAInst.$w$couId */(_3G/* scZe */, _3H/* scZf */));
  },1))), E(_3J/* scZi */), _/* EXTERNAL */)),
  _3L/* scZu */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _3q/* JQuery.hideJq2 */, E(_3K/* scZp */), _/* EXTERNAL */));
  return new F(function(){return _3w/* Main.a9 */(_3I/* scZg */, _/* EXTERNAL */);});
},
_3M/* $fOUIdentifiedOUAssocPHInst1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("AssocPH_"));
}),
_3N/* $w$couId1 */ = function(_3O/* s8Y6 */, _3P/* s8Y7 */){
  var _3Q/* s8Yq */ = new T(function(){
    var _3R/* s8Y8 */ = E(_3O/* s8Y6 */),
    _3S/* s8Yb */ = new T(function(){
      var _3T/* s8Yc */ = E(_3P/* s8Y7 */);
      return B(_2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_3T/* s8Yc */.a, _3T/* s8Yc */.b))));
    }),
    _3U/* s8Yg */ = function(_3V/*  s8Yh */){
      while(1){
        var _3W/*  s8Yg */ = B((function(_3X/* s8Yh */){
          var _3Y/* s8Yi */ = E(_3X/* s8Yh */);
          if(!_3Y/* s8Yi */._){
            return E(_3S/* s8Yb */);
          }else{
            var _3Z/* s8Yk */ = _3Y/* s8Yi */.b,
            _40/* s8Yl */ = E(_3Y/* s8Yi */.a);
            if(!B(_2w/* Metamodel.UfoA.$wlvl */(_40/* s8Yl */))){
              _3V/*  s8Yh */ = _3Z/* s8Yk */;
              return __continue/* EXTERNAL */;
            }else{
              return new T2(1,_40/* s8Yl */,new T(function(){
                return B(_3U/* s8Yg */(_3Z/* s8Yk */));
              }));
            }
          }
        })(_3V/*  s8Yh */));
        if(_3W/*  s8Yg */!=__continue/* EXTERNAL */){
          return _3W/*  s8Yg */;
        }
      }
    };
    return B(_3U/* s8Yg */(B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_3R/* s8Y8 */.a, _3R/* s8Y8 */.b))));
  },1);
  return new F(function(){return _2G/* GHC.Base.++ */(_3M/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocPHInst1 */, _3Q/* s8Yq */);});
},
_41/* a10 */ = function(_42/*  sd0e */, _/* EXTERNAL */){
  while(1){
    var _43/*  a10 */ = B((function(_44/* sd0e */, _/* EXTERNAL */){
      var _45/* sd0g */ = E(_44/* sd0e */);
      if(!_45/* sd0g */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _46/* sd0j */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _47/* sd0u */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _48/* sd0o */ = E(_45/* sd0g */.a);
          return B(_3N/* Metamodel.UfoAInst.$w$couId1 */(_48/* sd0o */.b, _48/* sd0o */.c));
        },1))), E(_46/* sd0j */), _/* EXTERNAL */)),
        _49/* sd0z */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _3q/* JQuery.hideJq2 */, E(_47/* sd0u */), _/* EXTERNAL */));
        _42/*  sd0e */ = _45/* sd0g */.b;
        return __continue/* EXTERNAL */;
      }
    })(_42/*  sd0e */, _/* EXTERNAL */));
    if(_43/*  a10 */!=__continue/* EXTERNAL */){
      return _43/*  a10 */;
    }
  }
},
_4a/* $sa1 */ = function(_4b/* scZV */, _4c/* scZW */, _4d/* scZX */, _/* EXTERNAL */){
  var _4e/* scZZ */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
  _4f/* sd06 */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
    return B(_3N/* Metamodel.UfoAInst.$w$couId1 */(_4b/* scZV */, _4c/* scZW */));
  },1))), E(_4e/* scZZ */), _/* EXTERNAL */)),
  _4g/* sd0b */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _3q/* JQuery.hideJq2 */, E(_4f/* sd06 */), _/* EXTERNAL */));
  return new F(function(){return _41/* Main.a10 */(_4d/* scZX */, _/* EXTERNAL */);});
},
_4h/* setHtml2 */ = "(function (html, jq) { jq.html(html); return jq; })",
_4i/* $wa28 */ = function(_4j/* sa3N */, _4k/* sa3O */, _/* EXTERNAL */){
  var _4l/* sa3Y */ = eval/* EXTERNAL */(E(_4h/* JQuery.setHtml2 */));
  return new F(function(){return __app2/* EXTERNAL */(E(_4l/* sa3Y */), toJSStr/* EXTERNAL */(E(_4j/* sa3N */)), _4k/* sa3O */);});
},
_4m/* $fEqOUElementInst_$c/= */ = function(_4n/* s90b */, _4o/* s90c */){
  var _4p/* s90d */ = E(_4n/* s90b */),
  _4q/* s90g */ = E(_4p/* s90d */.a),
  _4r/* s90j */ = E(_4o/* s90c */),
  _4s/* s90l */ = _4r/* s90j */.b,
  _4t/* s90m */ = E(_4r/* s90j */.a);
  if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_4q/* s90g */.a, _4q/* s90g */.b, _4t/* s90m */.a, _4t/* s90m */.b))){
    return true;
  }else{
    var _4u/* s90q */ = E(_4p/* s90d */.b);
    if(!_4u/* s90q */._){
      return (E(_4s/* s90l */)._==0) ? false : true;
    }else{
      var _4v/* s90u */ = E(_4s/* s90l */);
      return (_4v/* s90u */._==0) ? true : (!B(_7/* GHC.Base.eqString */(_4u/* s90q */.a, _4v/* s90u */.a))) ? true : false;
    }
  }
},
_4w/* $fEqOUElementInst */ = new T2(0,_q/* Metamodel.UfoAInst.$fEqOUAssocInst_$c==1 */,_4m/* Metamodel.UfoAInst.$fEqOUElementInst_$c/= */),
_4x/* [] */ = __Z/* EXTERNAL */,
_4y/* == */ = function(_4z/* scBJ */){
  return E(E(_4z/* scBJ */).a);
},
_4A/* elem */ = function(_4B/* s9uW */, _4C/* s9uX */, _4D/* s9uY */){
  while(1){
    var _4E/* s9uZ */ = E(_4D/* s9uY */);
    if(!_4E/* s9uZ */._){
      return false;
    }else{
      if(!B(A3(_4y/* GHC.Classes.== */,_4B/* s9uW */, _4C/* s9uX */, _4E/* s9uZ */.a))){
        _4D/* s9uY */ = _4E/* s9uZ */.b;
        continue;
      }else{
        return true;
      }
    }
  }
},
_4F/* oumsElements_go */ = function(_4G/*  s4mU */, _4H/*  s4mV */){
  while(1){
    var _4I/*  oumsElements_go */ = B((function(_4J/* s4mU */, _4K/* s4mV */){
      var _4L/* s4mW */ = E(_4J/* s4mU */);
      if(!_4L/* s4mW */._){
        return E(_4K/* s4mV */);
      }else{
        _4G/*  s4mU */ = _4L/* s4mW */.b;
        _4H/*  s4mV */ = new T(function(){
          return B(A1(_4L/* s4mW */.a,_4K/* s4mV */));
        });
        return __continue/* EXTERNAL */;
      }
    })(_4G/*  s4mU */, _4H/*  s4mV */));
    if(_4I/*  oumsElements_go */!=__continue/* EXTERNAL */){
      return _4I/*  oumsElements_go */;
    }
  }
},
_4M/* oumsElements1 */ = function(_4N/* s4n0 */, _4O/* s4n1 */){
  return new F(function(){return _4F/* Metamodel.UfoB.oumsElements_go */(E(_4O/* s4n1 */).b, _4N/* s4n0 */);});
},
_4P/* oumsAssocs_go */ = function(_4Q/*  s4nX */, _4R/*  s4nY */){
  while(1){
    var _4S/*  oumsAssocs_go */ = B((function(_4T/* s4nX */, _4U/* s4nY */){
      var _4V/* s4nZ */ = E(_4T/* s4nX */);
      if(!_4V/* s4nZ */._){
        return E(_4U/* s4nY */);
      }else{
        _4Q/*  s4nX */ = _4V/* s4nZ */.b;
        _4R/*  s4nY */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_4U/* s4nY */, _4V/* s4nZ */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_4Q/*  s4nX */, _4R/*  s4nY */));
    if(_4S/*  oumsAssocs_go */!=__continue/* EXTERNAL */){
      return _4S/*  oumsAssocs_go */;
    }
  }
},
_4W/* oumsAssocs_go1 */ = function(_4X/*  s4nR */, _4Y/*  s4nS */){
  while(1){
    var _4Z/*  oumsAssocs_go1 */ = B((function(_50/* s4nR */, _51/* s4nS */){
      var _52/* s4nT */ = E(_50/* s4nR */);
      if(!_52/* s4nT */._){
        return E(_51/* s4nS */);
      }else{
        _4X/*  s4nR */ = _52/* s4nT */.b;
        _4Y/*  s4nS */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_51/* s4nS */, _52/* s4nT */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_4X/*  s4nR */, _4Y/*  s4nS */));
    if(_4Z/*  oumsAssocs_go1 */!=__continue/* EXTERNAL */){
      return _4Z/*  oumsAssocs_go1 */;
    }
  }
},
_53/* $woumsAssocs */ = function(_54/* s4o3 */, _55/* s4o4 */){
  return new F(function(){return _2P/* GHC.List.filter */(function(_56/* s4o5 */){
    var _57/* s4o6 */ = E(_56/* s4o5 */);
    if(!B(_4A/* GHC.List.elem */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _57/* s4o6 */.b, B(_4W/* Metamodel.UfoB.oumsAssocs_go1 */(_55/* s4o4 */, _4x/* GHC.Types.[] */))))){
      return false;
    }else{
      return new F(function(){return _4A/* GHC.List.elem */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _57/* s4o6 */.c, B(_4P/* Metamodel.UfoB.oumsAssocs_go */(_55/* s4o4 */, _4x/* GHC.Types.[] */)));});
    }
  }, _54/* s4o3 */);});
},
_58/* oumsAssocsPH_go */ = function(_59/*  s4ov */, _5a/*  s4ow */){
  while(1){
    var _5b/*  oumsAssocsPH_go */ = B((function(_5c/* s4ov */, _5d/* s4ow */){
      var _5e/* s4ox */ = E(_5c/* s4ov */);
      if(!_5e/* s4ox */._){
        return E(_5d/* s4ow */);
      }else{
        _59/*  s4ov */ = _5e/* s4ox */.b;
        _5a/*  s4ow */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_5d/* s4ow */, _5e/* s4ox */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_59/*  s4ov */, _5a/*  s4ow */));
    if(_5b/*  oumsAssocsPH_go */!=__continue/* EXTERNAL */){
      return _5b/*  oumsAssocsPH_go */;
    }
  }
},
_5f/* oumsAssocsPH_go1 */ = function(_5g/*  s4op */, _5h/*  s4oq */){
  while(1){
    var _5i/*  oumsAssocsPH_go1 */ = B((function(_5j/* s4op */, _5k/* s4oq */){
      var _5l/* s4or */ = E(_5j/* s4op */);
      if(!_5l/* s4or */._){
        return E(_5k/* s4oq */);
      }else{
        _5g/*  s4op */ = _5l/* s4or */.b;
        _5h/*  s4oq */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_5k/* s4oq */, _5l/* s4or */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_5g/*  s4op */, _5h/*  s4oq */));
    if(_5i/*  oumsAssocsPH_go1 */!=__continue/* EXTERNAL */){
      return _5i/*  oumsAssocsPH_go1 */;
    }
  }
},
_5m/* $woumsAssocsPH */ = function(_5n/* s4oB */, _5o/* s4oC */){
  return new F(function(){return _2P/* GHC.List.filter */(function(_5p/* s4oD */){
    var _5q/* s4oE */ = E(_5p/* s4oD */);
    if(!B(_4A/* GHC.List.elem */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _5q/* s4oE */.b, B(_5f/* Metamodel.UfoB.oumsAssocsPH_go1 */(_5o/* s4oC */, _4x/* GHC.Types.[] */))))){
      return false;
    }else{
      return new F(function(){return _4A/* GHC.List.elem */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _5q/* s4oE */.c, B(_58/* Metamodel.UfoB.oumsAssocsPH_go */(_5o/* s4oC */, _4x/* GHC.Types.[] */)));});
    }
  }, _5n/* s4oB */);});
},
_5r/* oumiGeneralizations_go */ = function(_5s/* s8Vn */){
  var _5t/* s8Vo */ = E(_5s/* s8Vn */);
  if(!_5t/* s8Vo */._){
    return __Z/* EXTERNAL */;
  }else{
    return new F(function(){return _2G/* GHC.Base.++ */(_5t/* s8Vo */.a, new T(function(){
      return B(_5r/* Metamodel.UfoAInst.oumiGeneralizations_go */(_5t/* s8Vo */.b));
    },1));});
  }
},
_5u/* oumsGeneralizations_go */ = function(_5v/*  s4nv */, _5w/*  s4nw */){
  while(1){
    var _5x/*  oumsGeneralizations_go */ = B((function(_5y/* s4nv */, _5z/* s4nw */){
      var _5A/* s4nx */ = E(_5y/* s4nv */);
      if(!_5A/* s4nx */._){
        return E(_5z/* s4nw */);
      }else{
        _5v/*  s4nv */ = _5A/* s4nx */.b;
        _5w/*  s4nw */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_5z/* s4nw */, _5A/* s4nx */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_5v/*  s4nv */, _5w/*  s4nw */));
    if(_5x/*  oumsGeneralizations_go */!=__continue/* EXTERNAL */){
      return _5x/*  oumsGeneralizations_go */;
    }
  }
},
_5B/* $woumsGeneralizations */ = function(_5C/* s4nB */, _5D/* s4nC */){
  return new F(function(){return _2P/* GHC.List.filter */(function(_5E/* s4nD */){
    return new F(function(){return _4A/* GHC.List.elem */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, E(_5E/* s4nD */).c, B(_5u/* Metamodel.UfoB.oumsGeneralizations_go */(_5D/* s4nC */, _4x/* GHC.Types.[] */)));});
  }, B(_5r/* Metamodel.UfoAInst.oumiGeneralizations_go */(_5C/* s4nB */)));});
},
_5F/* deleteBy */ = function(_5G/* s1WJH */, _5H/* s1WJI */, _5I/* s1WJJ */){
  var _5J/* s1WJK */ = E(_5I/* s1WJJ */);
  if(!_5J/* s1WJK */._){
    return __Z/* EXTERNAL */;
  }else{
    var _5K/* s1WJL */ = _5J/* s1WJK */.a,
    _5L/* s1WJM */ = _5J/* s1WJK */.b;
    return (!B(A2(_5G/* s1WJH */,_5H/* s1WJI */, _5K/* s1WJL */))) ? new T2(1,_5K/* s1WJL */,new T(function(){
      return B(_5F/* Data.OldList.deleteBy */(_5G/* s1WJH */, _5H/* s1WJI */, _5L/* s1WJM */));
    })) : E(_5L/* s1WJM */);
  }
},
_5M/* delete */ = function(_5N/* s1WJZ */, _5O/* s1WK0 */, _5P/* s1WK1 */){
  return new F(function(){return _5F/* Data.OldList.deleteBy */(new T(function(){
    return B(_4y/* GHC.Classes.== */(_5N/* s1WJZ */));
  }), _5O/* s1WK0 */, _5P/* s1WK1 */);});
},
_5Q/* \\ */ = function(_5R/* s1WK3 */, _5S/* s1WK4 */, _5T/* s1WK5 */){
  var _5U/* s1WK6 */ = function(_5V/* s1WK7 */, _5W/* s1WK8 */){
    while(1){
      var _5X/* s1WK9 */ = E(_5V/* s1WK7 */);
      if(!_5X/* s1WK9 */._){
        return E(_5W/* s1WK8 */);
      }else{
        var _5Y/*  s1WK8 */ = B(_5M/* Data.OldList.delete */(_5R/* s1WK3 */, _5X/* s1WK9 */.a, _5W/* s1WK8 */));
        _5V/* s1WK7 */ = _5X/* s1WK9 */.b;
        _5W/* s1WK8 */ = _5Y/*  s1WK8 */;
        continue;
      }
    }
  };
  return new F(function(){return _5U/* s1WK6 */(_5T/* s1WK5 */, _5S/* s1WK4 */);});
},
_5Z/* showJq2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("visible"));
}),
_60/* a11 */ = function(_61/*  sd0H */, _/* EXTERNAL */){
  while(1){
    var _62/*  a11 */ = B((function(_63/* sd0H */, _/* EXTERNAL */){
      var _64/* sd0J */ = E(_63/* sd0H */);
      if(!_64/* sd0J */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _65/* sd0M */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _66/* sd0X */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _67/* sd0R */ = E(_64/* sd0J */.a);
          return B(_2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_67/* sd0R */.a, _67/* sd0R */.b))));
        },1))), E(_65/* sd0M */), _/* EXTERNAL */)),
        _68/* sd12 */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _5Z/* JQuery.showJq2 */, E(_66/* sd0X */), _/* EXTERNAL */));
        _61/*  sd0H */ = _64/* sd0J */.b;
        return __continue/* EXTERNAL */;
      }
    })(_61/*  sd0H */, _/* EXTERNAL */));
    if(_62/*  a11 */!=__continue/* EXTERNAL */){
      return _62/*  a11 */;
    }
  }
},
_69/* $fOUIdentifiedOUGeneralizationInst1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("gi_"));
}),
_6a/* $w$couId2 */ = function(_6b/* s8X9 */, _6c/* s8Xa */, _6d/* s8Xb */){
  var _6e/* s8XA */ = new T(function(){
    var _6f/* s8Xz */ = new T(function(){
      var _6g/* s8Xh */ = E(_6c/* s8Xa */),
      _6h/* s8Xk */ = new T(function(){
        var _6i/* s8Xl */ = E(_6d/* s8Xb */);
        return B(_2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_6i/* s8Xl */.a, _6i/* s8Xl */.b))));
      }),
      _6j/* s8Xp */ = function(_6k/*  s8Xq */){
        while(1){
          var _6l/*  s8Xp */ = B((function(_6m/* s8Xq */){
            var _6n/* s8Xr */ = E(_6m/* s8Xq */);
            if(!_6n/* s8Xr */._){
              return E(_6h/* s8Xk */);
            }else{
              var _6o/* s8Xt */ = _6n/* s8Xr */.b,
              _6p/* s8Xu */ = E(_6n/* s8Xr */.a);
              if(!B(_2w/* Metamodel.UfoA.$wlvl */(_6p/* s8Xu */))){
                _6k/*  s8Xq */ = _6o/* s8Xt */;
                return __continue/* EXTERNAL */;
              }else{
                return new T2(1,_6p/* s8Xu */,new T(function(){
                  return B(_6j/* s8Xp */(_6o/* s8Xt */));
                }));
              }
            }
          })(_6k/*  s8Xq */));
          if(_6l/*  s8Xp */!=__continue/* EXTERNAL */){
            return _6l/*  s8Xp */;
          }
        }
      };
      return B(_6j/* s8Xp */(B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_6g/* s8Xh */.a, _6g/* s8Xh */.b))));
    },1);
    return B(_2G/* GHC.Base.++ */(E(_6b/* s8X9 */).a, _6f/* s8Xz */));
  },1);
  return new F(function(){return _2G/* GHC.Base.++ */(_69/* Metamodel.UfoAInst.$fOUIdentifiedOUGeneralizationInst1 */, _6e/* s8XA */);});
},
_6q/* a12 */ = function(_6r/*  sd1b */, _/* EXTERNAL */){
  while(1){
    var _6s/*  a12 */ = B((function(_6t/* sd1b */, _/* EXTERNAL */){
      var _6u/* sd1d */ = E(_6t/* sd1b */);
      if(!_6u/* sd1d */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _6v/* sd1g */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _6w/* sd1r */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _6x/* sd1l */ = E(_6u/* sd1d */.a);
          return B(_6a/* Metamodel.UfoAInst.$w$couId2 */(_6x/* sd1l */.a, _6x/* sd1l */.b, _6x/* sd1l */.c));
        },1))), E(_6v/* sd1g */), _/* EXTERNAL */)),
        _6y/* sd1w */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _5Z/* JQuery.showJq2 */, E(_6w/* sd1r */), _/* EXTERNAL */));
        _6r/*  sd1b */ = _6u/* sd1d */.b;
        return __continue/* EXTERNAL */;
      }
    })(_6r/*  sd1b */, _/* EXTERNAL */));
    if(_6s/*  a12 */!=__continue/* EXTERNAL */){
      return _6s/*  a12 */;
    }
  }
},
_6z/* a13 */ = function(_6A/*  sd1z */, _/* EXTERNAL */){
  while(1){
    var _6B/*  a13 */ = B((function(_6C/* sd1z */, _/* EXTERNAL */){
      var _6D/* sd1B */ = E(_6C/* sd1z */);
      if(!_6D/* sd1B */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _6E/* sd1E */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _6F/* sd1P */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _6G/* sd1J */ = E(_6D/* sd1B */.a);
          return B(_2Z/* Metamodel.UfoAInst.$w$couId */(_6G/* sd1J */.b, _6G/* sd1J */.c));
        },1))), E(_6E/* sd1E */), _/* EXTERNAL */)),
        _6H/* sd1U */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _5Z/* JQuery.showJq2 */, E(_6F/* sd1P */), _/* EXTERNAL */));
        _6A/*  sd1z */ = _6D/* sd1B */.b;
        return __continue/* EXTERNAL */;
      }
    })(_6A/*  sd1z */, _/* EXTERNAL */));
    if(_6B/*  a13 */!=__continue/* EXTERNAL */){
      return _6B/*  a13 */;
    }
  }
},
_6I/* a14 */ = function(_6J/*  sd1X */, _/* EXTERNAL */){
  while(1){
    var _6K/*  a14 */ = B((function(_6L/* sd1X */, _/* EXTERNAL */){
      var _6M/* sd1Z */ = E(_6L/* sd1X */);
      if(!_6M/* sd1Z */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _6N/* sd22 */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _6O/* sd2d */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _6P/* sd27 */ = E(_6M/* sd1Z */.a);
          return B(_3N/* Metamodel.UfoAInst.$w$couId1 */(_6P/* sd27 */.b, _6P/* sd27 */.c));
        },1))), E(_6N/* sd22 */), _/* EXTERNAL */)),
        _6Q/* sd2i */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _5Z/* JQuery.showJq2 */, E(_6O/* sd2d */), _/* EXTERNAL */));
        _6J/*  sd1X */ = _6M/* sd1Z */.b;
        return __continue/* EXTERNAL */;
      }
    })(_6J/*  sd1X */, _/* EXTERNAL */));
    if(_6K/*  a14 */!=__continue/* EXTERNAL */){
      return _6K/*  a14 */;
    }
  }
},
_6R/* $fOUIdentifiedOUElementInst_$couId */ = function(_6S/* s8X4 */){
  var _6T/* s8X5 */ = E(_6S/* s8X4 */);
  return new F(function(){return _2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_6T/* s8X5 */.a, _6T/* s8X5 */.b)));});
},
_6U/* disableJq5 */ = "(function (k, v, jq) { jq.attr(k, v); return jq; })",
_6V/* $wa6 */ = function(_6W/* sa2y */, _6X/* sa2z */, _6Y/* sa2A */, _/* EXTERNAL */){
  var _6Z/* sa2P */ = eval/* EXTERNAL */(E(_6U/* JQuery.disableJq5 */));
  return new F(function(){return __app3/* EXTERNAL */(E(_6Z/* sa2P */), toJSStr/* EXTERNAL */(E(_6W/* sa2y */)), toJSStr/* EXTERNAL */(E(_6X/* sa2z */)), _6Y/* sa2A */);});
},
_70/* getARect2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" polygon"));
}),
_71/* getARect1 */ = function(_72/* scXK */, _73/* scXL */, _/* EXTERNAL */){
  var _74/* scXN */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */));
  return new F(function(){return _3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
    return B(_2G/* GHC.Base.++ */(B(A1(_72/* scXK */,_73/* scXL */)), _70/* Main.getARect2 */));
  },1))), E(_74/* scXN */), _/* EXTERNAL */);});
},
_75/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("stroke"));
}),
_76/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("stroke-width"));
}),
_77/* lvl7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("white"));
}),
_78/* lvl8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("0"));
}),
_79/* a15 */ = function(_7a/* sd2l */, _/* EXTERNAL */){
  while(1){
    var _7b/* sd2n */ = E(_7a/* sd2l */);
    if(!_7b/* sd2n */._){
      return _0/* GHC.Tuple.() */;
    }else{
      var _7c/* sd2q */ = B(_71/* Main.getARect1 */(_6R/* Metamodel.UfoAInst.$fOUIdentifiedOUElementInst_$couId */, _7b/* sd2n */.a, _/* EXTERNAL */)),
      _7d/* sd2v */ = B(_6V/* JQuery.$wa6 */(_75/* Main.lvl1 */, _77/* Main.lvl7 */, E(_7c/* sd2q */), _/* EXTERNAL */)),
      _7e/* sd2A */ = B(_6V/* JQuery.$wa6 */(_76/* Main.lvl3 */, _78/* Main.lvl8 */, E(_7d/* sd2v */), _/* EXTERNAL */));
      _7a/* sd2l */ = _7b/* sd2n */.b;
      continue;
    }
  }
},
_7f/* lvl2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("red"));
}),
_7g/* lvl4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("3"));
}),
_7h/* a16 */ = function(_7i/* sd2J */, _/* EXTERNAL */){
  while(1){
    var _7j/* sd2L */ = E(_7i/* sd2J */);
    if(!_7j/* sd2L */._){
      return _0/* GHC.Tuple.() */;
    }else{
      var _7k/* sd2O */ = B(_71/* Main.getARect1 */(_6R/* Metamodel.UfoAInst.$fOUIdentifiedOUElementInst_$couId */, _7j/* sd2L */.a, _/* EXTERNAL */)),
      _7l/* sd2T */ = B(_6V/* JQuery.$wa6 */(_75/* Main.lvl1 */, _7f/* Main.lvl2 */, E(_7k/* sd2O */), _/* EXTERNAL */)),
      _7m/* sd2Y */ = B(_6V/* JQuery.$wa6 */(_76/* Main.lvl3 */, _7g/* Main.lvl4 */, E(_7l/* sd2T */), _/* EXTERNAL */));
      _7i/* sd2J */ = _7j/* sd2L */.b;
      continue;
    }
  }
},
_7n/* $fOUIdentifiedOUAssocInst_$couId */ = function(_7o/* s8Y1 */){
  var _7p/* s8Y2 */ = E(_7o/* s8Y1 */);
  return new F(function(){return _2Z/* Metamodel.UfoAInst.$w$couId */(_7p/* s8Y2 */.b, _7p/* s8Y2 */.c);});
},
_7q/* disableJq4 */ = function(_7r/* sa31 */, _7s/* sa32 */, _7t/* sa33 */, _/* EXTERNAL */){
  return new F(function(){return _6V/* JQuery.$wa6 */(_7r/* sa31 */, _7s/* sa32 */, E(_7t/* sa33 */), _/* EXTERNAL */);});
},
_7u/* getALine2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" path"));
}),
_7v/* getALine1 */ = function(_7w/* scXV */, _7x/* scXW */, _/* EXTERNAL */){
  var _7y/* scXY */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */));
  return new F(function(){return _3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
    return B(_2G/* GHC.Base.++ */(B(A1(_7w/* scXV */,_7x/* scXW */)), _7u/* Main.getALine2 */));
  },1))), E(_7y/* scXY */), _/* EXTERNAL */);});
},
_7z/* lvl5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("black"));
}),
_7A/* lvl6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("1"));
}),
_7B/* a4 */ = function(_7C/* scYh */, _7D/* scYi */, _/* EXTERNAL */){
  var _7E/* scYk */ = B(_7v/* Main.getALine1 */(_7C/* scYh */, _7D/* scYi */, _/* EXTERNAL */)),
  _7F/* scYp */ = B(_6V/* JQuery.$wa6 */(_75/* Main.lvl1 */, _7z/* Main.lvl5 */, E(_7E/* scYk */), _/* EXTERNAL */));
  return new F(function(){return _7q/* JQuery.disableJq4 */(_76/* Main.lvl3 */, _7A/* Main.lvl6 */, _7F/* scYp */, _/* EXTERNAL */);});
},
_7G/* a17 */ = function(_7H/* sd31 */, _/* EXTERNAL */){
  while(1){
    var _7I/* sd33 */ = E(_7H/* sd31 */);
    if(!_7I/* sd33 */._){
      return _0/* GHC.Tuple.() */;
    }else{
      var _7J/* sd36 */ = B(_7B/* Main.a4 */(_7n/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocInst_$couId */, _7I/* sd33 */.a, _/* EXTERNAL */));
      _7H/* sd31 */ = _7I/* sd33 */.b;
      continue;
    }
  }
},
_7K/* a3 */ = function(_7L/* scY6 */, _7M/* scY7 */, _/* EXTERNAL */){
  var _7N/* scY9 */ = B(_7v/* Main.getALine1 */(_7L/* scY6 */, _7M/* scY7 */, _/* EXTERNAL */)),
  _7O/* scYe */ = B(_6V/* JQuery.$wa6 */(_75/* Main.lvl1 */, _7f/* Main.lvl2 */, E(_7N/* scY9 */), _/* EXTERNAL */));
  return new F(function(){return _7q/* JQuery.disableJq4 */(_76/* Main.lvl3 */, _7g/* Main.lvl4 */, _7O/* scYe */, _/* EXTERNAL */);});
},
_7P/* a18 */ = function(_7Q/* sd39 */, _/* EXTERNAL */){
  while(1){
    var _7R/* sd3b */ = E(_7Q/* sd39 */);
    if(!_7R/* sd3b */._){
      return _0/* GHC.Tuple.() */;
    }else{
      var _7S/* sd3e */ = B(_7K/* Main.a3 */(_7n/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocInst_$couId */, _7R/* sd3b */.a, _/* EXTERNAL */));
      _7Q/* sd39 */ = _7R/* sd3b */.b;
      continue;
    }
  }
},
_7T/* $fOUIdentifiedOUAssocPHInst_$couId */ = function(_7U/* s8Yr */){
  var _7V/* s8Ys */ = E(_7U/* s8Yr */);
  return new F(function(){return _3N/* Metamodel.UfoAInst.$w$couId1 */(_7V/* s8Ys */.b, _7V/* s8Ys */.c);});
},
_7W/* a19 */ = function(_7X/* sd3h */, _/* EXTERNAL */){
  while(1){
    var _7Y/* sd3j */ = E(_7X/* sd3h */);
    if(!_7Y/* sd3j */._){
      return _0/* GHC.Tuple.() */;
    }else{
      var _7Z/* sd3m */ = B(_7B/* Main.a4 */(_7T/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocPHInst_$couId */, _7Y/* sd3j */.a, _/* EXTERNAL */));
      _7X/* sd3h */ = _7Y/* sd3j */.b;
      continue;
    }
  }
},
_80/* a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("story-ufob"));
}),
_81/* a20 */ = function(_82/* sd3p */, _/* EXTERNAL */){
  while(1){
    var _83/* sd3r */ = E(_82/* sd3p */);
    if(!_83/* sd3r */._){
      return _0/* GHC.Tuple.() */;
    }else{
      var _84/* sd3u */ = B(_7K/* Main.a3 */(_7T/* Metamodel.UfoAInst.$fOUIdentifiedOUAssocPHInst_$couId */, _83/* sd3r */.a, _/* EXTERNAL */));
      _82/* sd3p */ = _83/* sd3r */.b;
      continue;
    }
  }
},
_85/* a5 */ = function(_86/*  scYs */, _/* EXTERNAL */){
  while(1){
    var _87/*  a5 */ = B((function(_88/* scYs */, _/* EXTERNAL */){
      var _89/* scYu */ = E(_88/* scYs */);
      if(!_89/* scYu */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _8a/* scYx */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _8b/* scYI */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _8c/* scYC */ = E(_89/* scYu */.a);
          return B(_2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, B(_2K/* Metamodel.UfoAInst.$woueiLabel */(_8c/* scYC */.a, _8c/* scYC */.b))));
        },1))), E(_8a/* scYx */), _/* EXTERNAL */)),
        _8d/* scYN */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _3q/* JQuery.hideJq2 */, E(_8b/* scYI */), _/* EXTERNAL */));
        _86/*  scYs */ = _89/* scYu */.b;
        return __continue/* EXTERNAL */;
      }
    })(_86/*  scYs */, _/* EXTERNAL */));
    if(_87/*  a5 */!=__continue/* EXTERNAL */){
      return _87/*  a5 */;
    }
  }
},
_8e/* getInstanceByName_go */ = function(_8f/* s94j */){
  var _8g/* s94k */ = E(_8f/* s94j */);
  if(!_8g/* s94k */._){
    return __Z/* EXTERNAL */;
  }else{
    var _8h/* s94n */ = E(_8g/* s94k */.a);
    return new T2(1,_8h/* s94n */.b,new T2(1,_8h/* s94n */.c,new T(function(){
      return B(_8e/* Metamodel.UfoAInst.getInstanceByName_go */(_8g/* s94k */.b));
    })));
  }
},
_8i/* elem_by */ = function(_8j/* s1WKd */, _8k/* s1WKe */, _8l/* s1WKf */){
  while(1){
    var _8m/* s1WKg */ = E(_8l/* s1WKf */);
    if(!_8m/* s1WKg */._){
      return false;
    }else{
      if(!B(A2(_8j/* s1WKd */,_8m/* s1WKg */.a, _8k/* s1WKe */))){
        _8l/* s1WKf */ = _8m/* s1WKg */.b;
        continue;
      }else{
        return true;
      }
    }
  }
},
_8n/* nubBy */ = function(_8o/* s1WKk */, _8p/* s1WKl */){
  var _8q/* s1WKm */ = function(_8r/*  s1WKn */, _8s/*  s1WKo */){
    while(1){
      var _8t/*  s1WKm */ = B((function(_8u/* s1WKn */, _8v/* s1WKo */){
        var _8w/* s1WKp */ = E(_8u/* s1WKn */);
        if(!_8w/* s1WKp */._){
          return __Z/* EXTERNAL */;
        }else{
          var _8x/* s1WKq */ = _8w/* s1WKp */.a,
          _8y/* s1WKr */ = _8w/* s1WKp */.b;
          if(!B(_8i/* Data.OldList.elem_by */(_8o/* s1WKk */, _8x/* s1WKq */, _8v/* s1WKo */))){
            return new T2(1,_8x/* s1WKq */,new T(function(){
              return B(_8q/* s1WKm */(_8y/* s1WKr */, new T2(1,_8x/* s1WKq */,_8v/* s1WKo */)));
            }));
          }else{
            var _8z/*   s1WKo */ = _8v/* s1WKo */;
            _8r/*  s1WKn */ = _8y/* s1WKr */;
            _8s/*  s1WKo */ = _8z/*   s1WKo */;
            return __continue/* EXTERNAL */;
          }
        }
      })(_8r/*  s1WKn */, _8s/*  s1WKo */));
      if(_8t/*  s1WKm */!=__continue/* EXTERNAL */){
        return _8t/*  s1WKm */;
      }
    }
  };
  return new F(function(){return _8q/* s1WKm */(_8p/* s1WKl */, _4x/* GHC.Types.[] */);});
},
_8A/* $woumiElements */ = function(_8B/* s94t */, _8C/* s94u */, _8D/* s94v */){
  var _8E/* s94w */ = new T(function(){
    var _8F/* s94x */ = new T(function(){
      return B(_8e/* Metamodel.UfoAInst.getInstanceByName_go */(_8D/* s94v */));
    }),
    _8G/* s94y */ = function(_8H/* s94z */){
      var _8I/* s94A */ = E(_8H/* s94z */);
      if(!_8I/* s94A */._){
        return E(_8F/* s94x */);
      }else{
        var _8J/* s94D */ = E(_8I/* s94A */.a);
        return new T2(1,_8J/* s94D */.b,new T2(1,_8J/* s94D */.c,new T(function(){
          return B(_8G/* s94y */(_8I/* s94A */.b));
        })));
      }
    };
    return B(_8G/* s94y */(_8C/* s94u */));
  }),
  _8K/* s94J */ = function(_8L/* s94K */){
    var _8M/* s94L */ = E(_8L/* s94K */);
    if(!_8M/* s94L */._){
      return E(_8E/* s94w */);
    }else{
      var _8N/* s94O */ = new T(function(){
        return B(_8K/* s94J */(_8M/* s94L */.b));
      }),
      _8O/* s94P */ = function(_8P/* s94Q */){
        var _8Q/* s94R */ = E(_8P/* s94Q */);
        if(!_8Q/* s94R */._){
          return E(_8N/* s94O */);
        }else{
          var _8R/* s94U */ = E(_8Q/* s94R */.a);
          return new T2(1,_8R/* s94U */.b,new T2(1,_8R/* s94U */.c,new T(function(){
            return B(_8O/* s94P */(_8Q/* s94R */.b));
          })));
        }
      };
      return new F(function(){return _8O/* s94P */(_8M/* s94L */.a);});
    }
  };
  return new F(function(){return _8n/* Data.OldList.nubBy */(_q/* Metamodel.UfoAInst.$fEqOUAssocInst_$c==1 */, B(_8K/* s94J */(_8B/* s94t */)));});
},
_8S/* Nothing */ = __Z/* EXTERNAL */,
_8T/* Kind */ = 0,
_8U/* eDonorCentre1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Centre"));
}),
_8V/* eDonorCentre */ = new T2(0,_8T/* Metamodel.UfoA.Kind */,_8U/* Model.Elements.eDonorCentre1 */),
_8W/* iDonorCentre */ = new T2(0,_8V/* Model.Elements.eDonorCentre */,_8S/* GHC.Base.Nothing */),
_8X/* eDonorRegistry1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Registry"));
}),
_8Y/* eDonorRegistry */ = new T2(0,_8T/* Metamodel.UfoA.Kind */,_8X/* Model.Elements.eDonorRegistry1 */),
_8Z/* iDonorRegistry */ = new T2(0,_8Y/* Model.Elements.eDonorRegistry */,_8S/* GHC.Base.Nothing */),
_90/* PlainPH */ = 0,
_91/* a4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("1"));
}),
_92/* a70 */ = new T2(0,_8Y/* Model.Elements.eDonorRegistry */,_91/* Model.Elements.a4 */),
_93/* m6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("10"));
}),
_94/* m4 */ = new T2(0,_8V/* Model.Elements.eDonorCentre */,_93/* Model.Elements.m6 */),
_95/* m8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m1"));
}),
_96/* mkMemberOf1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("M"));
}),
_97/* m1 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_95/* Model.Elements.m8 */,_92/* Model.Elements.a70 */,_94/* Model.Elements.m4 */,_90/* Metamodel.UfoA.PlainPH */),
_98/* mi1 */ = new T3(0,_97/* Model.Elements.m1 */,_8Z/* Model.Elements.iDonorRegistry */,_8W/* Model.Elements.iDonorCentre */),
_99/* eHLALaboratory1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("HLA Laboratory"));
}),
_9a/* eHLALaboratory */ = new T2(0,_8T/* Metamodel.UfoA.Kind */,_99/* Model.Elements.eHLALaboratory1 */),
_9b/* iHLALaboratory */ = new T2(0,_9a/* Model.Elements.eHLALaboratory */,_8S/* GHC.Base.Nothing */),
_9c/* a20 */ = new T2(0,_9a/* Model.Elements.eHLALaboratory */,_91/* Model.Elements.a4 */),
_9d/* m13 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m2"));
}),
_9e/* m2 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_9d/* Model.Elements.m13 */,_92/* Model.Elements.a70 */,_9c/* Model.Elements.a20 */,_90/* Metamodel.UfoA.PlainPH */),
_9f/* mi2 */ = new T3(0,_9e/* Model.Elements.m2 */,_8Z/* Model.Elements.iDonorRegistry */,_9b/* Model.Elements.iHLALaboratory */),
_9g/* Relator */ = 4,
_9h/* eRecruitment1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Recruitment"));
}),
_9i/* eRecruitment */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_9h/* Model.Elements.eRecruitment1 */),
_9j/* iRecruitment */ = new T2(0,_9i/* Model.Elements.eRecruitment */,_8S/* GHC.Base.Nothing */),
_9k/* eRegistration1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Registration"));
}),
_9l/* eRegistration */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_9k/* Model.Elements.eRegistration1 */),
_9m/* iRegistration */ = new T2(0,_9l/* Model.Elements.eRegistration */,_8S/* GHC.Base.Nothing */),
_9n/* EssentialInseparable */ = 3,
_9o/* a80 */ = new T2(0,_9i/* Model.Elements.eRecruitment */,_91/* Model.Elements.a4 */),
_9p/* m14 */ = new T2(0,_9l/* Model.Elements.eRegistration */,_91/* Model.Elements.a4 */),
_9q/* m16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m3"));
}),
_9r/* m3 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_9q/* Model.Elements.m16 */,_9o/* Model.Elements.a80 */,_9p/* Model.Elements.m14 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_9s/* mi3 */ = new T3(0,_9r/* Model.Elements.m3 */,_9j/* Model.Elements.iRecruitment */,_9m/* Model.Elements.iRegistration */),
_9t/* eInitialExamination1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Initial Examination"));
}),
_9u/* eInitialExamination */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_9t/* Model.Elements.eInitialExamination1 */),
_9v/* iInitialExamination */ = new T2(0,_9u/* Model.Elements.eInitialExamination */,_8S/* GHC.Base.Nothing */),
_9w/* m4b1 */ = new T2(0,_9u/* Model.Elements.eInitialExamination */,_91/* Model.Elements.a4 */),
_9x/* m4b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m4b"));
}),
_9y/* m4b */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_9x/* Model.Elements.m4b2 */,_9o/* Model.Elements.a80 */,_9w/* Model.Elements.m4b1 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_9z/* mi4b */ = new T3(0,_9y/* Model.Elements.m4b */,_9j/* Model.Elements.iRecruitment */,_9v/* Model.Elements.iInitialExamination */),
_9A/* eMedicalAssessment5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Initial\nMedical\nAssessment"));
}),
_9B/* eMedicalAssessment1 */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_9A/* Model.Elements.eMedicalAssessment5 */),
_9C/* iMedicalAssessment1 */ = new T2(0,_9B/* Model.Elements.eMedicalAssessment1 */,_8S/* GHC.Base.Nothing */),
_9D/* a32_5 */ = new T2(0,_9B/* Model.Elements.eMedicalAssessment1 */,_91/* Model.Elements.a4 */),
_9E/* m17 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m5"));
}),
_9F/* m5 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_9E/* Model.Elements.m17 */,_9w/* Model.Elements.m4b1 */,_9D/* Model.Elements.a32_5 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_9G/* mi5 */ = new T3(0,_9F/* Model.Elements.m5 */,_9v/* Model.Elements.iInitialExamination */,_9C/* Model.Elements.iMedicalAssessment1 */),
_9H/* eBloodSampleDraw1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Blood Sample Draw"));
}),
_9I/* eBloodSampleDraw */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_9H/* Model.Elements.eBloodSampleDraw1 */),
_9J/* iBloodSampleDraw8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("initial BSD"));
}),
_9K/* iBloodSampleDraw7 */ = new T1(1,_9J/* Model.Elements.iBloodSampleDraw8 */),
_9L/* iBloodSampleDraw1 */ = new T2(0,_9I/* Model.Elements.eBloodSampleDraw */,_9K/* Model.Elements.iBloodSampleDraw7 */),
_9M/* a139 */ = new T2(0,_9I/* Model.Elements.eBloodSampleDraw */,_91/* Model.Elements.a4 */),
_9N/* m6_4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m6_1"));
}),
_9O/* m6_1 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_9N/* Model.Elements.m6_4 */,_9w/* Model.Elements.m4b1 */,_9M/* Model.Elements.a139 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_9P/* mi6_1 */ = new T3(0,_9O/* Model.Elements.m6_1 */,_9v/* Model.Elements.iInitialExamination */,_9L/* Model.Elements.iBloodSampleDraw1 */),
_9Q/* iBloodSampleDraw10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("extended BSD"));
}),
_9R/* iBloodSampleDraw9 */ = new T1(1,_9Q/* Model.Elements.iBloodSampleDraw10 */),
_9S/* iBloodSampleDraw2 */ = new T2(0,_9I/* Model.Elements.eBloodSampleDraw */,_9R/* Model.Elements.iBloodSampleDraw9 */),
_9T/* eExtendedExamination1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended Typing\nwithin New Blood Sample"));
}),
_9U/* eExtendedExamination */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_9T/* Model.Elements.eExtendedExamination1 */),
_9V/* iExtendedExamination */ = new T2(0,_9U/* Model.Elements.eExtendedExamination */,_8S/* GHC.Base.Nothing */),
_9W/* Inseparable */ = 2,
_9X/* a18a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("0..1"));
}),
_9Y/* m6_5 */ = new T2(0,_9I/* Model.Elements.eBloodSampleDraw */,_9X/* Model.Elements.a18a2 */),
_9Z/* m6_6 */ = new T2(0,_9U/* Model.Elements.eExtendedExamination */,_91/* Model.Elements.a4 */),
_a0/* m6_7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m6_2"));
}),
_a1/* m6_2 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_a0/* Model.Elements.m6_7 */,_9Z/* Model.Elements.m6_6 */,_9Y/* Model.Elements.m6_5 */,_9W/* Metamodel.UfoA.Inseparable */),
_a2/* mi6_2 */ = new T3(0,_a1/* Model.Elements.m6_2 */,_9V/* Model.Elements.iExtendedExamination */,_9S/* Model.Elements.iBloodSampleDraw2 */),
_a3/* iBloodSampleDraw12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("verification BSD"));
}),
_a4/* iBloodSampleDraw11 */ = new T1(1,_a3/* Model.Elements.iBloodSampleDraw12 */),
_a5/* iBloodSampleDraw3 */ = new T2(0,_9I/* Model.Elements.eBloodSampleDraw */,_a4/* Model.Elements.iBloodSampleDraw11 */),
_a6/* eVerificationExamination1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Verification Examination"));
}),
_a7/* eVerificationExamination */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_a6/* Model.Elements.eVerificationExamination1 */),
_a8/* iVerificationExamination */ = new T2(0,_a7/* Model.Elements.eVerificationExamination */,_8S/* GHC.Base.Nothing */),
_a9/* m10a1 */ = new T2(0,_a7/* Model.Elements.eVerificationExamination */,_91/* Model.Elements.a4 */),
_aa/* m6_8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m6_3"));
}),
_ab/* m6_3 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_aa/* Model.Elements.m6_8 */,_a9/* Model.Elements.m10a1 */,_9M/* Model.Elements.a139 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_ac/* mi6_3 */ = new T3(0,_ab/* Model.Elements.m6_3 */,_a8/* Model.Elements.iVerificationExamination */,_a5/* Model.Elements.iBloodSampleDraw3 */),
_ad/* Quantity */ = 7,
_ae/* eBloodSample1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Blood Sample"));
}),
_af/* eBloodSample */ = new T2(0,_ad/* Metamodel.UfoA.Quantity */,_ae/* Model.Elements.eBloodSample1 */),
_ag/* iBloodSample */ = new T2(0,_af/* Model.Elements.eBloodSample */,_8S/* GHC.Base.Nothing */),
_ah/* eDNASample1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DNA Sample"));
}),
_ai/* eDNASample */ = new T2(0,_ad/* Metamodel.UfoA.Quantity */,_ah/* Model.Elements.eDNASample1 */),
_aj/* iDNASample */ = new T2(0,_ai/* Model.Elements.eDNASample */,_8S/* GHC.Base.Nothing */),
_ak/* a93 */ = new T2(0,_af/* Model.Elements.eBloodSample */,_91/* Model.Elements.a4 */),
_al/* m18 */ = new T2(0,_ai/* Model.Elements.eDNASample */,_9X/* Model.Elements.a18a2 */),
_am/* m19 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m7"));
}),
_an/* m7 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_am/* Model.Elements.m19 */,_ak/* Model.Elements.a93 */,_al/* Model.Elements.m18 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_ao/* mi7_1 */ = new T3(0,_an/* Model.Elements.m7 */,_ag/* Model.Elements.iBloodSample */,_aj/* Model.Elements.iDNASample */),
_ap/* Role */ = 2,
_aq/* eDonorPotential1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Potential Donor"));
}),
_ar/* eDonorPotential */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_aq/* Model.Elements.eDonorPotential1 */),
_as/* iDonorPotential */ = new T2(0,_ar/* Model.Elements.eDonorPotential */,_8S/* GHC.Base.Nothing */),
_at/* Collective */ = 8,
_au/* eFoundDonors1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Found Donors"));
}),
_av/* eFoundDonors */ = new T2(0,_at/* Metamodel.UfoA.Collective */,_au/* Model.Elements.eFoundDonors1 */),
_aw/* iFoundDonors */ = new T2(0,_av/* Model.Elements.eFoundDonors */,_8S/* GHC.Base.Nothing */),
_ax/* a16a_2a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("0..*"));
}),
_ay/* m20 */ = new T2(0,_ar/* Model.Elements.eDonorPotential */,_ax/* Model.Elements.a16a_2a2 */),
_az/* m21 */ = new T2(0,_av/* Model.Elements.eFoundDonors */,_91/* Model.Elements.a4 */),
_aA/* m22 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m9"));
}),
_aB/* m9 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_aA/* Model.Elements.m22 */,_az/* Model.Elements.m21 */,_ay/* Model.Elements.m20 */,_90/* Metamodel.UfoA.PlainPH */),
_aC/* mi9 */ = new T3(0,_aB/* Model.Elements.m9 */,_aw/* Model.Elements.iFoundDonors */,_as/* Model.Elements.iDonorPotential */),
_aD/* eInfectionTestingV1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Verification\nInfection Markers\nDisease Testing"));
}),
_aE/* eInfectionTestingV */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_aD/* Model.Elements.eInfectionTestingV1 */),
_aF/* iInfectionTestingV */ = new T2(0,_aE/* Model.Elements.eInfectionTestingV */,_8S/* GHC.Base.Nothing */),
_aG/* a30_5 */ = new T2(0,_aE/* Model.Elements.eInfectionTestingV */,_91/* Model.Elements.a4 */),
_aH/* m10a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m10a"));
}),
_aI/* m10a */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_aH/* Model.Elements.m10a2 */,_a9/* Model.Elements.m10a1 */,_aG/* Model.Elements.a30_5 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_aJ/* mi10a */ = new T3(0,_aI/* Model.Elements.m10a */,_a8/* Model.Elements.iVerificationExamination */,_aF/* Model.Elements.iInfectionTestingV */),
_aK/* eInfectionTestingW1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Workup\nInfection Markers\nDisease Testing"));
}),
_aL/* eInfectionTestingW */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_aK/* Model.Elements.eInfectionTestingW1 */),
_aM/* iInfectionTestingW */ = new T2(0,_aL/* Model.Elements.eInfectionTestingW */,_8S/* GHC.Base.Nothing */),
_aN/* eWorkup1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Workup Examination"));
}),
_aO/* eWorkup */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_aN/* Model.Elements.eWorkup1 */),
_aP/* iWorkup */ = new T2(0,_aO/* Model.Elements.eWorkup */,_8S/* GHC.Base.Nothing */),
_aQ/* a30_8 */ = new T2(0,_aL/* Model.Elements.eInfectionTestingW */,_91/* Model.Elements.a4 */),
_aR/* a34_2aa1 */ = new T2(0,_aO/* Model.Elements.eWorkup */,_91/* Model.Elements.a4 */),
_aS/* m10b1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m10b"));
}),
_aT/* m10b */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_aS/* Model.Elements.m10b1 */,_aR/* Model.Elements.a34_2aa1 */,_aQ/* Model.Elements.a30_8 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_aU/* mi10b */ = new T3(0,_aT/* Model.Elements.m10b */,_aP/* Model.Elements.iWorkup */,_aM/* Model.Elements.iInfectionTestingW */),
_aV/* eMedicalAssessment7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Workup\nMedical\nAssessment"));
}),
_aW/* eMedicalAssessment3 */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_aV/* Model.Elements.eMedicalAssessment7 */),
_aX/* iMedicalAssessment3 */ = new T2(0,_aW/* Model.Elements.eMedicalAssessment3 */,_8S/* GHC.Base.Nothing */),
_aY/* a32_11 */ = new T2(0,_aW/* Model.Elements.eMedicalAssessment3 */,_91/* Model.Elements.a4 */),
_aZ/* m10c1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m10c"));
}),
_b0/* m10c */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_aZ/* Model.Elements.m10c1 */,_aR/* Model.Elements.a34_2aa1 */,_aY/* Model.Elements.a32_11 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_b1/* mi10c */ = new T3(0,_b0/* Model.Elements.m10c */,_aP/* Model.Elements.iWorkup */,_aX/* Model.Elements.iMedicalAssessment3 */),
_b2/* iBloodSampleDraw14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("workup BSD"));
}),
_b3/* iBloodSampleDraw13 */ = new T1(1,_b2/* Model.Elements.iBloodSampleDraw14 */),
_b4/* iBloodSampleDraw4 */ = new T2(0,_9I/* Model.Elements.eBloodSampleDraw */,_b3/* Model.Elements.iBloodSampleDraw13 */),
_b5/* m10d1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m10d"));
}),
_b6/* m10d */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_b5/* Model.Elements.m10d1 */,_aR/* Model.Elements.a34_2aa1 */,_9M/* Model.Elements.a139 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_b7/* mi10d */ = new T3(0,_b6/* Model.Elements.m10d */,_aP/* Model.Elements.iWorkup */,_b4/* Model.Elements.iBloodSampleDraw4 */),
_b8/* eMedicalAssessment6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Verification\nMedical\nAssessment"));
}),
_b9/* eMedicalAssessment2 */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_b8/* Model.Elements.eMedicalAssessment6 */),
_ba/* iMedicalAssessment2 */ = new T2(0,_b9/* Model.Elements.eMedicalAssessment2 */,_8S/* GHC.Base.Nothing */),
_bb/* a32_8 */ = new T2(0,_b9/* Model.Elements.eMedicalAssessment2 */,_91/* Model.Elements.a4 */),
_bc/* m10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m11"));
}),
_bd/* m11 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_bc/* Model.Elements.m10 */,_a9/* Model.Elements.m10a1 */,_bb/* Model.Elements.a32_8 */,_90/* Metamodel.UfoA.PlainPH */),
_be/* mi11 */ = new T3(0,_bd/* Model.Elements.m11 */,_a8/* Model.Elements.iVerificationExamination */,_ba/* Model.Elements.iMedicalAssessment2 */),
_bf/* ePBSCDonation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC Donation"));
}),
_bg/* ePBSCDonation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bf/* Model.Elements.ePBSCDonation1 */),
_bh/* iPBSCDonation */ = new T2(0,_bg/* Model.Elements.ePBSCDonation */,_8S/* GHC.Base.Nothing */),
_bi/* ePBSCPreparation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC Donation\nPreparation"));
}),
_bj/* ePBSCPreparation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bi/* Model.Elements.ePBSCPreparation1 */),
_bk/* iPBSCPreparation */ = new T2(0,_bj/* Model.Elements.ePBSCPreparation */,_8S/* GHC.Base.Nothing */),
_bl/* a37a2 */ = new T2(0,_bj/* Model.Elements.ePBSCPreparation */,_91/* Model.Elements.a4 */),
_bm/* m12a1 */ = new T2(0,_bg/* Model.Elements.ePBSCDonation */,_91/* Model.Elements.a4 */),
_bn/* m12a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m12a"));
}),
_bo/* m12a */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_bn/* Model.Elements.m12a2 */,_bm/* Model.Elements.m12a1 */,_bl/* Model.Elements.a37a2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_bp/* mi12a */ = new T3(0,_bo/* Model.Elements.m12a */,_bh/* Model.Elements.iPBSCDonation */,_bk/* Model.Elements.iPBSCPreparation */),
_bq/* eCollection1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection"));
}),
_br/* eCollection */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bq/* Model.Elements.eCollection1 */),
_bs/* iCollection */ = new T2(0,_br/* Model.Elements.eCollection */,_8S/* GHC.Base.Nothing */),
_bt/* eDonation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("HSC Donation"));
}),
_bu/* eDonation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bt/* Model.Elements.eDonation1 */),
_bv/* iDonation */ = new T2(0,_bu/* Model.Elements.eDonation */,_8S/* GHC.Base.Nothing */),
_bw/* a127 */ = new T2(0,_br/* Model.Elements.eCollection */,_91/* Model.Elements.a4 */),
_bx/* a147 */ = new T2(0,_bu/* Model.Elements.eDonation */,_91/* Model.Elements.a4 */),
_by/* m13_4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_1"));
}),
_bz/* m13_1 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_by/* Model.Elements.m13_4 */,_bx/* Model.Elements.a147 */,_bw/* Model.Elements.a127 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_bA/* mi13_1 */ = new T3(0,_bz/* Model.Elements.m13_1 */,_bv/* Model.Elements.iDonation */,_bs/* Model.Elements.iCollection */),
_bB/* ePBSCCollection1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC Collection"));
}),
_bC/* ePBSCCollection */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bB/* Model.Elements.ePBSCCollection1 */),
_bD/* iPBSCCollection */ = new T2(0,_bC/* Model.Elements.ePBSCCollection */,_8S/* GHC.Base.Nothing */),
_bE/* a36a2 */ = new T2(0,_bC/* Model.Elements.ePBSCCollection */,_91/* Model.Elements.a4 */),
_bF/* m13_1a1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_1a"));
}),
_bG/* m13_1a */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_bF/* Model.Elements.m13_1a1 */,_bm/* Model.Elements.m12a1 */,_bE/* Model.Elements.a36a2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_bH/* mi13_1a */ = new T3(0,_bG/* Model.Elements.m13_1a */,_bh/* Model.Elements.iPBSCDonation */,_bD/* Model.Elements.iPBSCCollection */),
_bI/* eBMCollection1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM Collection"));
}),
_bJ/* eBMCollection */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bI/* Model.Elements.eBMCollection1 */),
_bK/* iBMCollection */ = new T2(0,_bJ/* Model.Elements.eBMCollection */,_8S/* GHC.Base.Nothing */),
_bL/* eBMDonation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM Donation"));
}),
_bM/* eBMDonation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bL/* Model.Elements.eBMDonation1 */),
_bN/* iBMDonation */ = new T2(0,_bM/* Model.Elements.eBMDonation */,_8S/* GHC.Base.Nothing */),
_bO/* a36b2 */ = new T2(0,_bJ/* Model.Elements.eBMCollection */,_91/* Model.Elements.a4 */),
_bP/* m13_1b1 */ = new T2(0,_bM/* Model.Elements.eBMDonation */,_91/* Model.Elements.a4 */),
_bQ/* m13_1b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_1b"));
}),
_bR/* m13_1b */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_bQ/* Model.Elements.m13_1b2 */,_bP/* Model.Elements.m13_1b1 */,_bO/* Model.Elements.a36b2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_bS/* mi13_1b */ = new T3(0,_bR/* Model.Elements.m13_1b */,_bN/* Model.Elements.iBMDonation */,_bK/* Model.Elements.iBMCollection */),
_bT/* eDLICollection1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DLI Collection"));
}),
_bU/* eDLICollection */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bT/* Model.Elements.eDLICollection1 */),
_bV/* iDLICollection */ = new T2(0,_bU/* Model.Elements.eDLICollection */,_8S/* GHC.Base.Nothing */),
_bW/* eDLIDonation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DLI Donation"));
}),
_bX/* eDLIDonation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_bW/* Model.Elements.eDLIDonation1 */),
_bY/* iDLIDonation */ = new T2(0,_bX/* Model.Elements.eDLIDonation */,_8S/* GHC.Base.Nothing */),
_bZ/* a36c2 */ = new T2(0,_bU/* Model.Elements.eDLICollection */,_91/* Model.Elements.a4 */),
_c0/* m13_1c1 */ = new T2(0,_bX/* Model.Elements.eDLIDonation */,_91/* Model.Elements.a4 */),
_c1/* m13_1c2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_1c"));
}),
_c2/* m13_1c */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_c1/* Model.Elements.m13_1c2 */,_c0/* Model.Elements.m13_1c1 */,_bZ/* Model.Elements.a36c2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_c3/* mi13_1c */ = new T3(0,_c2/* Model.Elements.m13_1c */,_bY/* Model.Elements.iDLIDonation */,_bV/* Model.Elements.iDLICollection */),
_c4/* eTransport1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transport"));
}),
_c5/* eTransport */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_c4/* Model.Elements.eTransport1 */),
_c6/* iTransportPBSC2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("transport of PBSC"));
}),
_c7/* iTransportPBSC1 */ = new T1(1,_c6/* Model.Elements.iTransportPBSC2 */),
_c8/* iTransportPBSC */ = new T2(0,_c5/* Model.Elements.eTransport */,_c7/* Model.Elements.iTransportPBSC1 */),
_c9/* a59_5 */ = new T2(0,_c5/* Model.Elements.eTransport */,_91/* Model.Elements.a4 */),
_ca/* m13_5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_2"));
}),
_cb/* m13_2 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_ca/* Model.Elements.m13_5 */,_bx/* Model.Elements.a147 */,_c9/* Model.Elements.a59_5 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_cc/* mi13_2a */ = new T3(0,_cb/* Model.Elements.m13_2 */,_bh/* Model.Elements.iPBSCDonation */,_c8/* Model.Elements.iTransportPBSC */),
_cd/* iTransportBM2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("transport of BM"));
}),
_ce/* iTransportBM1 */ = new T1(1,_cd/* Model.Elements.iTransportBM2 */),
_cf/* iTransportBM */ = new T2(0,_c5/* Model.Elements.eTransport */,_ce/* Model.Elements.iTransportBM1 */),
_cg/* mi13_2b */ = new T3(0,_cb/* Model.Elements.m13_2 */,_bN/* Model.Elements.iBMDonation */,_cf/* Model.Elements.iTransportBM */),
_ch/* iTransportDLI2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("transport of DLI"));
}),
_ci/* iTransportDLI1 */ = new T1(1,_ch/* Model.Elements.iTransportDLI2 */),
_cj/* iTransportDLI */ = new T2(0,_c5/* Model.Elements.eTransport */,_ci/* Model.Elements.iTransportDLI1 */),
_ck/* mi13_2c */ = new T3(0,_cb/* Model.Elements.m13_2 */,_bY/* Model.Elements.iDLIDonation */,_cj/* Model.Elements.iTransportDLI */),
_cl/* eTransplantation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplantation"));
}),
_cm/* eTransplantation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_cl/* Model.Elements.eTransplantation1 */),
_cn/* iTransplantation */ = new T2(0,_cm/* Model.Elements.eTransplantation */,_8S/* GHC.Base.Nothing */),
_co/* a42_3c2 */ = new T2(0,_cm/* Model.Elements.eTransplantation */,_91/* Model.Elements.a4 */),
_cp/* m13_6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_3"));
}),
_cq/* m13_3 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_cp/* Model.Elements.m13_6 */,_bx/* Model.Elements.a147 */,_co/* Model.Elements.a42_3c2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_cr/* mi13_3 */ = new T3(0,_cq/* Model.Elements.m13_3 */,_bv/* Model.Elements.iDonation */,_cn/* Model.Elements.iTransplantation */),
_cs/* ePBSCTransplantation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC Transplantation"));
}),
_ct/* ePBSCTransplantation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_cs/* Model.Elements.ePBSCTransplantation1 */),
_cu/* iPBSCTransplantation */ = new T2(0,_ct/* Model.Elements.ePBSCTransplantation */,_8S/* GHC.Base.Nothing */),
_cv/* a42_1a2 */ = new T2(0,_ct/* Model.Elements.ePBSCTransplantation */,_91/* Model.Elements.a4 */),
_cw/* m13_3a1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_3a"));
}),
_cx/* m13_3a */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_cw/* Model.Elements.m13_3a1 */,_bm/* Model.Elements.m12a1 */,_cv/* Model.Elements.a42_1a2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_cy/* mi13_3a */ = new T3(0,_cx/* Model.Elements.m13_3a */,_bh/* Model.Elements.iPBSCDonation */,_cu/* Model.Elements.iPBSCTransplantation */),
_cz/* eBMTransplantation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM Transplantation"));
}),
_cA/* eBMTransplantation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_cz/* Model.Elements.eBMTransplantation1 */),
_cB/* iBMTransplantation */ = new T2(0,_cA/* Model.Elements.eBMTransplantation */,_8S/* GHC.Base.Nothing */),
_cC/* a42_2a2 */ = new T2(0,_cA/* Model.Elements.eBMTransplantation */,_91/* Model.Elements.a4 */),
_cD/* m13_3b1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_3b"));
}),
_cE/* m13_3b */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_cD/* Model.Elements.m13_3b1 */,_bP/* Model.Elements.m13_1b1 */,_cC/* Model.Elements.a42_2a2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_cF/* mi13_3b */ = new T3(0,_cE/* Model.Elements.m13_3b */,_bN/* Model.Elements.iBMDonation */,_cB/* Model.Elements.iBMTransplantation */),
_cG/* eDLITransplantation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DLI Transplantation"));
}),
_cH/* eDLITransplantation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_cG/* Model.Elements.eDLITransplantation1 */),
_cI/* iDLITransplantation */ = new T2(0,_cH/* Model.Elements.eDLITransplantation */,_8S/* GHC.Base.Nothing */),
_cJ/* a42_3a2 */ = new T2(0,_cH/* Model.Elements.eDLITransplantation */,_91/* Model.Elements.a4 */),
_cK/* m13_3c1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m13_3c"));
}),
_cL/* m13_3c */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_cK/* Model.Elements.m13_3c1 */,_c0/* Model.Elements.m13_1c1 */,_cJ/* Model.Elements.a42_3a2 */,_9n/* Metamodel.UfoA.EssentialInseparable */),
_cM/* mi13_3c */ = new T3(0,_cL/* Model.Elements.m13_3c */,_bY/* Model.Elements.iDLIDonation */,_cI/* Model.Elements.iDLITransplantation */),
_cN/* eCollectionCentre1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection\nCentre"));
}),
_cO/* eCollectionCentre */ = new T2(0,_8T/* Metamodel.UfoA.Kind */,_cN/* Model.Elements.eCollectionCentre1 */),
_cP/* iCollectionCentre */ = new T2(0,_cO/* Model.Elements.eCollectionCentre */,_8S/* GHC.Base.Nothing */),
_cQ/* a101 */ = new T2(0,_cO/* Model.Elements.eCollectionCentre */,_91/* Model.Elements.a4 */),
_cR/* m12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("m15"));
}),
_cS/* m15 */ = new T5(0,_96/* Metamodel.UfoA.mkMemberOf1 */,_cR/* Model.Elements.m12 */,_92/* Model.Elements.a70 */,_cQ/* Model.Elements.a101 */,_90/* Metamodel.UfoA.PlainPH */),
_cT/* mi15 */ = new T3(0,_cS/* Model.Elements.m15 */,_8Z/* Model.Elements.iDonorRegistry */,_cP/* Model.Elements.iCollectionCentre */),
_cU/* ouModelInst28 */ = new T2(1,_cT/* Model.Elements.mi15 */,_4x/* GHC.Types.[] */),
_cV/* ouModelInst27 */ = new T2(1,_cM/* Model.Elements.mi13_3c */,_cU/* Model.Models.ouModelInst28 */),
_cW/* ouModelInst26 */ = new T2(1,_cF/* Model.Elements.mi13_3b */,_cV/* Model.Models.ouModelInst27 */),
_cX/* ouModelInst25 */ = new T2(1,_cy/* Model.Elements.mi13_3a */,_cW/* Model.Models.ouModelInst26 */),
_cY/* ouModelInst24 */ = new T2(1,_cr/* Model.Elements.mi13_3 */,_cX/* Model.Models.ouModelInst25 */),
_cZ/* ouModelInst23 */ = new T2(1,_ck/* Model.Elements.mi13_2c */,_cY/* Model.Models.ouModelInst24 */),
_d0/* ouModelInst22 */ = new T2(1,_cg/* Model.Elements.mi13_2b */,_cZ/* Model.Models.ouModelInst23 */),
_d1/* ouModelInst21 */ = new T2(1,_cc/* Model.Elements.mi13_2a */,_d0/* Model.Models.ouModelInst22 */),
_d2/* ouModelInst20 */ = new T2(1,_c3/* Model.Elements.mi13_1c */,_d1/* Model.Models.ouModelInst21 */),
_d3/* ouModelInst19 */ = new T2(1,_bS/* Model.Elements.mi13_1b */,_d2/* Model.Models.ouModelInst20 */),
_d4/* ouModelInst18 */ = new T2(1,_bH/* Model.Elements.mi13_1a */,_d3/* Model.Models.ouModelInst19 */),
_d5/* ouModelInst17 */ = new T2(1,_bA/* Model.Elements.mi13_1 */,_d4/* Model.Models.ouModelInst18 */),
_d6/* ouModelInst16 */ = new T2(1,_bp/* Model.Elements.mi12a */,_d5/* Model.Models.ouModelInst17 */),
_d7/* ouModelInst15 */ = new T2(1,_be/* Model.Elements.mi11 */,_d6/* Model.Models.ouModelInst16 */),
_d8/* ouModelInst14 */ = new T2(1,_b7/* Model.Elements.mi10d */,_d7/* Model.Models.ouModelInst15 */),
_d9/* ouModelInst13 */ = new T2(1,_b1/* Model.Elements.mi10c */,_d8/* Model.Models.ouModelInst14 */),
_da/* ouModelInst12 */ = new T2(1,_aU/* Model.Elements.mi10b */,_d9/* Model.Models.ouModelInst13 */),
_db/* ouModelInst11 */ = new T2(1,_aJ/* Model.Elements.mi10a */,_da/* Model.Models.ouModelInst12 */),
_dc/* ouModelInst10 */ = new T2(1,_aC/* Model.Elements.mi9 */,_db/* Model.Models.ouModelInst11 */),
_dd/* ouModelInst9 */ = new T2(1,_ao/* Model.Elements.mi7_1 */,_dc/* Model.Models.ouModelInst10 */),
_de/* ouModelInst8 */ = new T2(1,_ac/* Model.Elements.mi6_3 */,_dd/* Model.Models.ouModelInst9 */),
_df/* ouModelInst7 */ = new T2(1,_a2/* Model.Elements.mi6_2 */,_de/* Model.Models.ouModelInst8 */),
_dg/* ouModelInst6 */ = new T2(1,_9P/* Model.Elements.mi6_1 */,_df/* Model.Models.ouModelInst7 */),
_dh/* ouModelInst5 */ = new T2(1,_9G/* Model.Elements.mi5 */,_dg/* Model.Models.ouModelInst6 */),
_di/* ouModelInst4 */ = new T2(1,_9z/* Model.Elements.mi4b */,_dh/* Model.Models.ouModelInst5 */),
_dj/* ouModelInst3 */ = new T2(1,_9s/* Model.Elements.mi3 */,_di/* Model.Models.ouModelInst4 */),
_dk/* ouModelInst2 */ = new T2(1,_9f/* Model.Elements.mi2 */,_dj/* Model.Models.ouModelInst3 */),
_dl/* ouModelInst1 */ = new T2(1,_98/* Model.Elements.mi1 */,_dk/* Model.Models.ouModelInst2 */),
_dm/* OUAMediation */ = __Z/* EXTERNAL */,
_dn/* a16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a1"));
}),
_do/* eDonor1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor"));
}),
_dp/* eDonor */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_do/* Model.Elements.eDonor1 */),
_dq/* a2 */ = new T2(0,_dp/* Model.Elements.eDonor */,_91/* Model.Elements.a4 */),
_dr/* eGeneAssignment1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Gene Assignment"));
}),
_ds/* eGeneAssignment */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_dr/* Model.Elements.eGeneAssignment1 */),
_dt/* a5 */ = new T2(0,_ds/* Model.Elements.eGeneAssignment */,_91/* Model.Elements.a4 */),
_du/* a1 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_dn/* Model.Elements.a16 */,_dt/* Model.Elements.a5 */,_dq/* Model.Elements.a2 */),
_dv/* iDonor */ = new T2(0,_dp/* Model.Elements.eDonor */,_8S/* GHC.Base.Nothing */),
_dw/* iGeneAssignment5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("initial GA"));
}),
_dx/* iGeneAssignment4 */ = new T1(1,_dw/* Model.Elements.iGeneAssignment5 */),
_dy/* iGeneAssignment1 */ = new T2(0,_ds/* Model.Elements.eGeneAssignment */,_dx/* Model.Elements.iGeneAssignment4 */),
_dz/* ai1 */ = new T3(0,_du/* Model.Elements.a1 */,_dy/* Model.Elements.iGeneAssignment1 */,_dv/* Model.Elements.iDonor */),
_dA/* a74 */ = new T2(0,_8V/* Model.Elements.eDonorCentre */,_91/* Model.Elements.a4 */),
_dB/* a18 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("1..*"));
}),
_dC/* a96 */ = new T2(0,_9i/* Model.Elements.eRecruitment */,_dB/* Model.Elements.a18 */),
_dD/* a97 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a3"));
}),
_dE/* a3 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_dD/* Model.Elements.a97 */,_dC/* Model.Elements.a96 */,_dA/* Model.Elements.a74 */),
_dF/* ai3 */ = new T3(0,_dE/* Model.Elements.a3 */,_9j/* Model.Elements.iRecruitment */,_8W/* Model.Elements.iDonorCentre */),
_dG/* a140 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a6"));
}),
_dH/* a6 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_dG/* Model.Elements.a140 */,_9M/* Model.Elements.a139 */,_ak/* Model.Elements.a93 */),
_dI/* ai6_1 */ = new T3(0,_dH/* Model.Elements.a6 */,_9L/* Model.Elements.iBloodSampleDraw1 */,_ag/* Model.Elements.iBloodSample */),
_dJ/* ai6_2 */ = new T3(0,_dH/* Model.Elements.a6 */,_9S/* Model.Elements.iBloodSampleDraw2 */,_ag/* Model.Elements.iBloodSample */),
_dK/* ai6_3 */ = new T3(0,_dH/* Model.Elements.a6 */,_a5/* Model.Elements.iBloodSampleDraw3 */,_ag/* Model.Elements.iBloodSample */),
_dL/* ai6_4 */ = new T3(0,_dH/* Model.Elements.a6 */,_b4/* Model.Elements.iBloodSampleDraw4 */,_ag/* Model.Elements.iBloodSample */),
_dM/* eDNAIsolation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DNA Isolation"));
}),
_dN/* eDNAIsolation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_dM/* Model.Elements.eDNAIsolation1 */),
_dO/* a154 */ = new T2(0,_dN/* Model.Elements.eDNAIsolation */,_9X/* Model.Elements.a18a2 */),
_dP/* a155 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a7"));
}),
_dQ/* a7 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_dP/* Model.Elements.a155 */,_ak/* Model.Elements.a93 */,_dO/* Model.Elements.a154 */),
_dR/* iDNAIsolation5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("inital I"));
}),
_dS/* iDNAIsolation4 */ = new T1(1,_dR/* Model.Elements.iDNAIsolation5 */),
_dT/* iDNAIsolation1 */ = new T2(0,_dN/* Model.Elements.eDNAIsolation */,_dS/* Model.Elements.iDNAIsolation4 */),
_dU/* ai7_1 */ = new T3(0,_dQ/* Model.Elements.a7 */,_ag/* Model.Elements.iBloodSample */,_dT/* Model.Elements.iDNAIsolation1 */),
_dV/* iDNAIsolation7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("extended I"));
}),
_dW/* iDNAIsolation6 */ = new T1(1,_dV/* Model.Elements.iDNAIsolation7 */),
_dX/* iDNAIsolation2 */ = new T2(0,_dN/* Model.Elements.eDNAIsolation */,_dW/* Model.Elements.iDNAIsolation6 */),
_dY/* ai7_2 */ = new T3(0,_dQ/* Model.Elements.a7 */,_ag/* Model.Elements.iBloodSample */,_dX/* Model.Elements.iDNAIsolation2 */),
_dZ/* iDNAIsolation9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("verification I"));
}),
_e0/* iDNAIsolation8 */ = new T1(1,_dZ/* Model.Elements.iDNAIsolation9 */),
_e1/* iDNAIsolation3 */ = new T2(0,_dN/* Model.Elements.eDNAIsolation */,_e0/* Model.Elements.iDNAIsolation8 */),
_e2/* ai7_3 */ = new T3(0,_dQ/* Model.Elements.a7 */,_ag/* Model.Elements.iBloodSample */,_e1/* Model.Elements.iDNAIsolation3 */),
_e3/* a156 */ = new T2(0,_dN/* Model.Elements.eDNAIsolation */,_dB/* Model.Elements.a18 */),
_e4/* a157 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a8"));
}),
_e5/* a8 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_e4/* Model.Elements.a157 */,_9c/* Model.Elements.a20 */,_e3/* Model.Elements.a156 */),
_e6/* ai8_1 */ = new T3(0,_e5/* Model.Elements.a8 */,_9b/* Model.Elements.iHLALaboratory */,_dT/* Model.Elements.iDNAIsolation1 */),
_e7/* ai8_2 */ = new T3(0,_e5/* Model.Elements.a8 */,_9b/* Model.Elements.iHLALaboratory */,_dX/* Model.Elements.iDNAIsolation2 */),
_e8/* ai8_3 */ = new T3(0,_e5/* Model.Elements.a8 */,_9b/* Model.Elements.iHLALaboratory */,_e1/* Model.Elements.iDNAIsolation3 */),
_e9/* a158 */ = new T2(0,_dN/* Model.Elements.eDNAIsolation */,_91/* Model.Elements.a4 */),
_ea/* a159 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a9"));
}),
_eb/* a76 */ = new T2(0,_ai/* Model.Elements.eDNASample */,_91/* Model.Elements.a4 */),
_ec/* a9 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ea/* Model.Elements.a159 */,_e9/* Model.Elements.a158 */,_eb/* Model.Elements.a76 */),
_ed/* ai9_1 */ = new T3(0,_ec/* Model.Elements.a9 */,_dT/* Model.Elements.iDNAIsolation1 */,_aj/* Model.Elements.iDNASample */),
_ee/* ai9_2 */ = new T3(0,_ec/* Model.Elements.a9 */,_dX/* Model.Elements.iDNAIsolation2 */,_aj/* Model.Elements.iDNASample */),
_ef/* ai9_3 */ = new T3(0,_ec/* Model.Elements.a9 */,_e1/* Model.Elements.iDNAIsolation3 */,_aj/* Model.Elements.iDNASample */),
_eg/* a17 */ = new T2(0,_ds/* Model.Elements.eGeneAssignment */,_dB/* Model.Elements.a18 */),
_eh/* a31 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a10"));
}),
_ei/* a10 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_eh/* Model.Elements.a31 */,_9c/* Model.Elements.a20 */,_eg/* Model.Elements.a17 */),
_ej/* ai10_1 */ = new T3(0,_ei/* Model.Elements.a10 */,_9b/* Model.Elements.iHLALaboratory */,_dy/* Model.Elements.iGeneAssignment1 */),
_ek/* iGeneAssignment7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("extended GA"));
}),
_el/* iGeneAssignment6 */ = new T1(1,_ek/* Model.Elements.iGeneAssignment7 */),
_em/* iGeneAssignment2 */ = new T2(0,_ds/* Model.Elements.eGeneAssignment */,_el/* Model.Elements.iGeneAssignment6 */),
_en/* ai10_2 */ = new T3(0,_ei/* Model.Elements.a10 */,_9b/* Model.Elements.iHLALaboratory */,_em/* Model.Elements.iGeneAssignment2 */),
_eo/* iGeneAssignment9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("verification GA"));
}),
_ep/* iGeneAssignment8 */ = new T1(1,_eo/* Model.Elements.iGeneAssignment9 */),
_eq/* iGeneAssignment3 */ = new T2(0,_ds/* Model.Elements.eGeneAssignment */,_ep/* Model.Elements.iGeneAssignment8 */),
_er/* ai10_3 */ = new T3(0,_ei/* Model.Elements.a10 */,_9b/* Model.Elements.iHLALaboratory */,_eq/* Model.Elements.iGeneAssignment3 */),
_es/* Mode */ = 5,
_et/* eDonorsTypingResults1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\'s\nTyping Results"));
}),
_eu/* eDonorsTypingResults */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_et/* Model.Elements.eDonorsTypingResults1 */),
_ev/* a33 */ = new T2(0,_eu/* Model.Elements.eDonorsTypingResults */,_91/* Model.Elements.a4 */),
_ew/* a34 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a11"));
}),
_ex/* a11 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ew/* Model.Elements.a34 */,_ev/* Model.Elements.a33 */,_dt/* Model.Elements.a5 */),
_ey/* iDonorsTypingResults5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("initial DTR"));
}),
_ez/* iDonorsTypingResults4 */ = new T1(1,_ey/* Model.Elements.iDonorsTypingResults5 */),
_eA/* iDonorsTypingResults1 */ = new T2(0,_eu/* Model.Elements.eDonorsTypingResults */,_ez/* Model.Elements.iDonorsTypingResults4 */),
_eB/* ai11_1 */ = new T3(0,_ex/* Model.Elements.a11 */,_eA/* Model.Elements.iDonorsTypingResults1 */,_dy/* Model.Elements.iGeneAssignment1 */),
_eC/* iDonorsTypingResults7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("extended DTR"));
}),
_eD/* iDonorsTypingResults6 */ = new T1(1,_eC/* Model.Elements.iDonorsTypingResults7 */),
_eE/* iDonorsTypingResults2 */ = new T2(0,_eu/* Model.Elements.eDonorsTypingResults */,_eD/* Model.Elements.iDonorsTypingResults6 */),
_eF/* ai11_2 */ = new T3(0,_ex/* Model.Elements.a11 */,_eE/* Model.Elements.iDonorsTypingResults2 */,_em/* Model.Elements.iGeneAssignment2 */),
_eG/* ai11_3 */ = new T3(0,_ex/* Model.Elements.a11 */,_eE/* Model.Elements.iDonorsTypingResults2 */,_eq/* Model.Elements.iGeneAssignment3 */),
_eH/* Quality */ = 6,
_eI/* eGenotypeValue1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Genotype value"));
}),
_eJ/* eGenotypeValue */ = new T2(0,_eH/* Metamodel.UfoA.Quality */,_eI/* Model.Elements.eGenotypeValue1 */),
_eK/* a35 */ = new T2(0,_eJ/* Model.Elements.eGenotypeValue */,_91/* Model.Elements.a4 */),
_eL/* a36 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a12"));
}),
_eM/* a12 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_eL/* Model.Elements.a36 */,_eK/* Model.Elements.a35 */,_dt/* Model.Elements.a5 */),
_eN/* iGenotypeValue5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("gv1"));
}),
_eO/* iGenotypeValue4 */ = new T1(1,_eN/* Model.Elements.iGenotypeValue5 */),
_eP/* iGenotypeValue1 */ = new T2(0,_eJ/* Model.Elements.eGenotypeValue */,_eO/* Model.Elements.iGenotypeValue4 */),
_eQ/* ai12_1 */ = new T3(0,_eM/* Model.Elements.a12 */,_eP/* Model.Elements.iGenotypeValue1 */,_dy/* Model.Elements.iGeneAssignment1 */),
_eR/* ai12_2 */ = new T3(0,_eM/* Model.Elements.a12 */,_eP/* Model.Elements.iGenotypeValue1 */,_em/* Model.Elements.iGeneAssignment2 */),
_eS/* ai12_3 */ = new T3(0,_eM/* Model.Elements.a12 */,_eP/* Model.Elements.iGenotypeValue1 */,_eq/* Model.Elements.iGeneAssignment3 */),
_eT/* OUACharacterization */ = new T0(1),
_eU/* a37 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a13"));
}),
_eV/* a13 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_eU/* Model.Elements.a37 */,_dq/* Model.Elements.a2 */,_eK/* Model.Elements.a35 */),
_eW/* ai13 */ = new T3(0,_eV/* Model.Elements.a13 */,_dv/* Model.Elements.iDonor */,_eP/* Model.Elements.iGenotypeValue1 */),
_eX/* eGenotype1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Person\'s Genotype"));
}),
_eY/* eGenotype */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_eX/* Model.Elements.eGenotype1 */),
_eZ/* a38 */ = new T2(0,_eY/* Model.Elements.eGenotype */,_91/* Model.Elements.a4 */),
_f0/* ePerson1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Person"));
}),
_f1/* ePerson */ = new T2(0,_8T/* Metamodel.UfoA.Kind */,_f0/* Model.Elements.ePerson1 */),
_f2/* a42 */ = new T2(0,_f1/* Model.Elements.ePerson */,_91/* Model.Elements.a4 */),
_f3/* a43 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a14"));
}),
_f4/* a14 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_f3/* Model.Elements.a43 */,_f2/* Model.Elements.a42 */,_eZ/* Model.Elements.a38 */),
_f5/* iGenotype5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\'s Genotype"));
}),
_f6/* iGenotype4 */ = new T1(1,_f5/* Model.Elements.iGenotype5 */),
_f7/* iGenotype1 */ = new T2(0,_eY/* Model.Elements.eGenotype */,_f6/* Model.Elements.iGenotype4 */),
_f8/* iPerson5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("person1"));
}),
_f9/* iPerson4 */ = new T1(1,_f8/* Model.Elements.iPerson5 */),
_fa/* iPerson1 */ = new T2(0,_f1/* Model.Elements.ePerson */,_f9/* Model.Elements.iPerson4 */),
_fb/* ai14_1 */ = new T3(0,_f4/* Model.Elements.a14 */,_fa/* Model.Elements.iPerson1 */,_f7/* Model.Elements.iGenotype1 */),
_fc/* iGenotype7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Patient\'s Genotype"));
}),
_fd/* iGenotype6 */ = new T1(1,_fc/* Model.Elements.iGenotype7 */),
_fe/* iGenotype2 */ = new T2(0,_eY/* Model.Elements.eGenotype */,_fd/* Model.Elements.iGenotype6 */),
_ff/* iPerson7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("person2"));
}),
_fg/* iPerson6 */ = new T1(1,_ff/* Model.Elements.iPerson7 */),
_fh/* iPerson2 */ = new T2(0,_f1/* Model.Elements.ePerson */,_fg/* Model.Elements.iPerson6 */),
_fi/* ai14_2 */ = new T3(0,_f4/* Model.Elements.a14 */,_fh/* Model.Elements.iPerson2 */,_fe/* Model.Elements.iGenotype2 */),
_fj/* Phase */ = 3,
_fk/* eGenotypeEvaluated1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Evaluated Genotype"));
}),
_fl/* eGenotypeEvaluated */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_fk/* Model.Elements.eGenotypeEvaluated1 */),
_fm/* a47 */ = new T2(0,_fl/* Model.Elements.eGenotypeEvaluated */,_91/* Model.Elements.a4 */),
_fn/* a69 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a15"));
}),
_fo/* a15 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_fn/* Model.Elements.a69 */,_fm/* Model.Elements.a47 */,_eK/* Model.Elements.a35 */),
_fp/* iGenotypeEvaluated5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ge1"));
}),
_fq/* iGenotypeEvaluated4 */ = new T1(1,_fp/* Model.Elements.iGenotypeEvaluated5 */),
_fr/* iGenotypeEvaluated1 */ = new T2(0,_fl/* Model.Elements.eGenotypeEvaluated */,_fq/* Model.Elements.iGenotypeEvaluated4 */),
_fs/* ai15_1_1 */ = new T3(0,_fo/* Model.Elements.a15 */,_fr/* Model.Elements.iGenotypeEvaluated1 */,_eP/* Model.Elements.iGenotypeValue1 */),
_ft/* a16a_2a1 */ = new T2(0,_9U/* Model.Elements.eExtendedExamination */,_ax/* Model.Elements.a16a_2a2 */),
_fu/* eSelectedET1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Selected and Available\nfor Extended Typing"));
}),
_fv/* eSelectedET */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_fu/* Model.Elements.eSelectedET1 */),
_fw/* a16a_2a3 */ = new T2(0,_fv/* Model.Elements.eSelectedET */,_91/* Model.Elements.a4 */),
_fx/* a16a_2a4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a16a_2a"));
}),
_fy/* a16a_2a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_fx/* Model.Elements.a16a_2a4 */,_fw/* Model.Elements.a16a_2a3 */,_ft/* Model.Elements.a16a_2a1 */),
_fz/* iSelectedET */ = new T2(0,_fv/* Model.Elements.eSelectedET */,_8S/* GHC.Base.Nothing */),
_fA/* ai16a_2a */ = new T3(0,_fy/* Model.Elements.a16a_2a */,_fz/* Model.Elements.iSelectedET */,_9V/* Model.Elements.iExtendedExamination */),
_fB/* a16a_1 */ = new T2(0,_a7/* Model.Elements.eVerificationExamination */,_dB/* Model.Elements.a18 */),
_fC/* eDonorVerified1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Verified\nDonor"));
}),
_fD/* eDonorVerified */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_fC/* Model.Elements.eDonorVerified1 */),
_fE/* a16a_2 */ = new T2(0,_fD/* Model.Elements.eDonorVerified */,_91/* Model.Elements.a4 */),
_fF/* a16a_4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a16a_3"));
}),
_fG/* a16a_3 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_fF/* Model.Elements.a16a_4 */,_fE/* Model.Elements.a16a_2 */,_fB/* Model.Elements.a16a_1 */),
_fH/* iDonorVerified */ = new T2(0,_fD/* Model.Elements.eDonorVerified */,_8S/* GHC.Base.Nothing */),
_fI/* ai16a_3 */ = new T3(0,_fG/* Model.Elements.a16a_3 */,_fH/* Model.Elements.iDonorVerified */,_a8/* Model.Elements.iVerificationExamination */),
_fJ/* ePatientRegistration1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Patient Registration"));
}),
_fK/* ePatientRegistration */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_fJ/* Model.Elements.ePatientRegistration1 */),
_fL/* a17a1 */ = new T2(0,_fK/* Model.Elements.ePatientRegistration */,_91/* Model.Elements.a4 */),
_fM/* ePatient1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Patient"));
}),
_fN/* ePatient */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_fM/* Model.Elements.ePatient1 */),
_fO/* a17a2 */ = new T2(0,_fN/* Model.Elements.ePatient */,_91/* Model.Elements.a4 */),
_fP/* a17a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a17a"));
}),
_fQ/* a17a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_fP/* Model.Elements.a17a3 */,_fO/* Model.Elements.a17a2 */,_fL/* Model.Elements.a17a1 */),
_fR/* iPatient */ = new T2(0,_fN/* Model.Elements.ePatient */,_8S/* GHC.Base.Nothing */),
_fS/* iPatientRegistration */ = new T2(0,_fK/* Model.Elements.ePatientRegistration */,_8S/* GHC.Base.Nothing */),
_fT/* ai17a */ = new T3(0,_fQ/* Model.Elements.a17a */,_fR/* Model.Elements.iPatient */,_fS/* Model.Elements.iPatientRegistration */),
_fU/* eTransplantCentre1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("TransplantCentre"));
}),
_fV/* eTransplantCentre */ = new T2(0,_8T/* Metamodel.UfoA.Kind */,_fU/* Model.Elements.eTransplantCentre1 */),
_fW/* a17b1 */ = new T2(0,_fV/* Model.Elements.eTransplantCentre */,_91/* Model.Elements.a4 */),
_fX/* a17b2 */ = new T2(0,_fK/* Model.Elements.ePatientRegistration */,_dB/* Model.Elements.a18 */),
_fY/* a17b3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a17b"));
}),
_fZ/* a17b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_fY/* Model.Elements.a17b3 */,_fX/* Model.Elements.a17b2 */,_fW/* Model.Elements.a17b1 */),
_g0/* iTransplantCentre */ = new T2(0,_fV/* Model.Elements.eTransplantCentre */,_8S/* GHC.Base.Nothing */),
_g1/* ai17b */ = new T3(0,_fZ/* Model.Elements.a17b */,_fS/* Model.Elements.iPatientRegistration */,_g0/* Model.Elements.iTransplantCentre */),
_g2/* eSearch1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Search"));
}),
_g3/* eSearch */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_g2/* Model.Elements.eSearch1 */),
_g4/* a18a1 */ = new T2(0,_g3/* Model.Elements.eSearch */,_9X/* Model.Elements.a18a2 */),
_g5/* a18a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a18a"));
}),
_g6/* a18a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_g5/* Model.Elements.a18a3 */,_fO/* Model.Elements.a17a2 */,_g4/* Model.Elements.a18a1 */),
_g7/* iSearch */ = new T2(0,_g3/* Model.Elements.eSearch */,_8S/* GHC.Base.Nothing */),
_g8/* ai18a */ = new T3(0,_g6/* Model.Elements.a18a */,_fR/* Model.Elements.iPatient */,_g7/* Model.Elements.iSearch */),
_g9/* a18b1 */ = new T2(0,_g3/* Model.Elements.eSearch */,_91/* Model.Elements.a4 */),
_ga/* a18b2 */ = new T2(0,_av/* Model.Elements.eFoundDonors */,_9X/* Model.Elements.a18a2 */),
_gb/* a18b3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a18b"));
}),
_gc/* a18b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_gb/* Model.Elements.a18b3 */,_ga/* Model.Elements.a18b2 */,_g9/* Model.Elements.a18b1 */),
_gd/* ai18b */ = new T3(0,_gc/* Model.Elements.a18b */,_aw/* Model.Elements.iFoundDonors */,_g7/* Model.Elements.iSearch */),
_ge/* a71 */ = new T2(0,_g3/* Model.Elements.eSearch */,_dB/* Model.Elements.a18 */),
_gf/* a72 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a19"));
}),
_gg/* a19 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_gf/* Model.Elements.a72 */,_ge/* Model.Elements.a71 */,_92/* Model.Elements.a70 */),
_gh/* ai19 */ = new T3(0,_gg/* Model.Elements.a19 */,_g7/* Model.Elements.iSearch */,_8Z/* Model.Elements.iDonorRegistry */),
_gi/* eExamination1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Examination"));
}),
_gj/* eExamination */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_gi/* Model.Elements.eExamination1 */),
_gk/* a73 */ = new T2(0,_gj/* Model.Elements.eExamination */,_dB/* Model.Elements.a18 */),
_gl/* a75 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a21"));
}),
_gm/* a21 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_gl/* Model.Elements.a75 */,_dA/* Model.Elements.a74 */,_gk/* Model.Elements.a73 */),
_gn/* iExamination */ = new T2(0,_gj/* Model.Elements.eExamination */,_8S/* GHC.Base.Nothing */),
_go/* ai21 */ = new T3(0,_gm/* Model.Elements.a21 */,_8W/* Model.Elements.iDonorCentre */,_gn/* Model.Elements.iExamination */),
_gp/* eHLATyping1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("HLA Typing"));
}),
_gq/* eHLATyping */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_gp/* Model.Elements.eHLATyping1 */),
_gr/* a77 */ = new T2(0,_gq/* Model.Elements.eHLATyping */,_dB/* Model.Elements.a18 */),
_gs/* a78 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a22"));
}),
_gt/* a22 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_gs/* Model.Elements.a78 */,_gr/* Model.Elements.a77 */,_eb/* Model.Elements.a76 */),
_gu/* iHLATyping5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("initial T"));
}),
_gv/* iHLATyping4 */ = new T1(1,_gu/* Model.Elements.iHLATyping5 */),
_gw/* iHLATyping1 */ = new T2(0,_gq/* Model.Elements.eHLATyping */,_gv/* Model.Elements.iHLATyping4 */),
_gx/* ai22_1 */ = new T3(0,_gt/* Model.Elements.a22 */,_gw/* Model.Elements.iHLATyping1 */,_aj/* Model.Elements.iDNASample */),
_gy/* iHLATyping7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("extended T"));
}),
_gz/* iHLATyping6 */ = new T1(1,_gy/* Model.Elements.iHLATyping7 */),
_gA/* iHLATyping2 */ = new T2(0,_gq/* Model.Elements.eHLATyping */,_gz/* Model.Elements.iHLATyping6 */),
_gB/* ai22_2 */ = new T3(0,_gt/* Model.Elements.a22 */,_gA/* Model.Elements.iHLATyping2 */,_aj/* Model.Elements.iDNASample */),
_gC/* iHLATyping9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("verification T"));
}),
_gD/* iHLATyping8 */ = new T1(1,_gC/* Model.Elements.iHLATyping9 */),
_gE/* iHLATyping3 */ = new T2(0,_gq/* Model.Elements.eHLATyping */,_gD/* Model.Elements.iHLATyping8 */),
_gF/* ai22_3 */ = new T3(0,_gt/* Model.Elements.a22 */,_gE/* Model.Elements.iHLATyping3 */,_aj/* Model.Elements.iDNASample */),
_gG/* a79 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a23"));
}),
_gH/* a23 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_gG/* Model.Elements.a79 */,_gr/* Model.Elements.a77 */,_ev/* Model.Elements.a33 */),
_gI/* ai23_1 */ = new T3(0,_gH/* Model.Elements.a23 */,_gw/* Model.Elements.iHLATyping1 */,_eA/* Model.Elements.iDonorsTypingResults1 */),
_gJ/* ai23_2 */ = new T3(0,_gH/* Model.Elements.a23 */,_gA/* Model.Elements.iHLATyping2 */,_eE/* Model.Elements.iDonorsTypingResults2 */),
_gK/* iDonorsTypingResults9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("verification DTR"));
}),
_gL/* iDonorsTypingResults8 */ = new T1(1,_gK/* Model.Elements.iDonorsTypingResults9 */),
_gM/* iDonorsTypingResults3 */ = new T2(0,_eu/* Model.Elements.eDonorsTypingResults */,_gL/* Model.Elements.iDonorsTypingResults8 */),
_gN/* ai23_3 */ = new T3(0,_gH/* Model.Elements.a23 */,_gE/* Model.Elements.iHLATyping3 */,_gM/* Model.Elements.iDonorsTypingResults3 */),
_gO/* eDonorAspirant1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Aspirant Donor"));
}),
_gP/* eDonorAspirant */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_gO/* Model.Elements.eDonorAspirant1 */),
_gQ/* a81 */ = new T2(0,_gP/* Model.Elements.eDonorAspirant */,_91/* Model.Elements.a4 */),
_gR/* a82 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a24"));
}),
_gS/* a24 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_gR/* Model.Elements.a82 */,_gQ/* Model.Elements.a81 */,_9o/* Model.Elements.a80 */),
_gT/* iDonorAspirant */ = new T2(0,_gP/* Model.Elements.eDonorAspirant */,_8S/* GHC.Base.Nothing */),
_gU/* ai24 */ = new T3(0,_gS/* Model.Elements.a24 */,_gT/* Model.Elements.iDonorAspirant */,_9j/* Model.Elements.iRecruitment */),
_gV/* a83 */ = new T2(0,_af/* Model.Elements.eBloodSample */,_dB/* Model.Elements.a18 */),
_gW/* a84 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a25"));
}),
_gX/* a86 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("<<material>>\n/belongs"));
}),
_gY/* a85 */ = new T1(1,_gX/* Model.Elements.a86 */),
_gZ/* mkAssoc1 */ = new T1(3,_4x/* GHC.Types.[] */),
_h0/* a25 */ = new T5(0,_gZ/* Metamodel.UfoA.mkAssoc1 */,_gY/* Model.Elements.a85 */,_gW/* Model.Elements.a84 */,_gQ/* Model.Elements.a81 */,_gV/* Model.Elements.a83 */),
_h1/* ai25 */ = new T3(0,_h0/* Model.Elements.a25 */,_gT/* Model.Elements.iDonorAspirant */,_ag/* Model.Elements.iBloodSample */),
_h2/* a87 */ = new T2(0,_ai/* Model.Elements.eDNASample */,_dB/* Model.Elements.a18 */),
_h3/* a26 */ = new T5(0,_gZ/* Metamodel.UfoA.mkAssoc1 */,_gY/* Model.Elements.a85 */,_gW/* Model.Elements.a84 */,_dq/* Model.Elements.a2 */,_h2/* Model.Elements.a87 */),
_h4/* ai26 */ = new T3(0,_h3/* Model.Elements.a26 */,_dv/* Model.Elements.iDonor */,_aj/* Model.Elements.iDNASample */),
_h5/* a88 */ = new T2(0,_gq/* Model.Elements.eHLATyping */,_ax/* Model.Elements.a16a_2a2 */),
_h6/* a89 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a27"));
}),
_h7/* a27 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_h6/* Model.Elements.a89 */,_h5/* Model.Elements.a88 */,_9c/* Model.Elements.a20 */),
_h8/* ai27_1 */ = new T3(0,_h7/* Model.Elements.a27 */,_gw/* Model.Elements.iHLATyping1 */,_9b/* Model.Elements.iHLALaboratory */),
_h9/* ai27_2 */ = new T3(0,_h7/* Model.Elements.a27 */,_gA/* Model.Elements.iHLATyping2 */,_9b/* Model.Elements.iHLALaboratory */),
_ha/* ai27_3 */ = new T3(0,_h7/* Model.Elements.a27 */,_gE/* Model.Elements.iHLATyping3 */,_9b/* Model.Elements.iHLALaboratory */),
_hb/* a90 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a28"));
}),
_hc/* a92 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("<<material>>\nregisters"));
}),
_hd/* a91 */ = new T1(1,_hc/* Model.Elements.a92 */),
_he/* a28 */ = new T5(0,_gZ/* Metamodel.UfoA.mkAssoc1 */,_hd/* Model.Elements.a91 */,_hb/* Model.Elements.a90 */,_dA/* Model.Elements.a74 */,_gV/* Model.Elements.a83 */),
_hf/* ai28 */ = new T3(0,_he/* Model.Elements.a28 */,_8W/* Model.Elements.iDonorCentre */,_ag/* Model.Elements.iBloodSample */),
_hg/* eInfectionTesting1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Infection Markers\nDisease Testing"));
}),
_hh/* eInfectionTesting */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_hg/* Model.Elements.eInfectionTesting1 */),
_hi/* a94 */ = new T2(0,_hh/* Model.Elements.eInfectionTesting */,_91/* Model.Elements.a4 */),
_hj/* a95 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a29"));
}),
_hk/* a29 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_hj/* Model.Elements.a95 */,_hi/* Model.Elements.a94 */,_ak/* Model.Elements.a93 */),
_hl/* iInfectionTesting */ = new T2(0,_hh/* Model.Elements.eInfectionTesting */,_8S/* GHC.Base.Nothing */),
_hm/* ai29 */ = new T3(0,_hk/* Model.Elements.a29 */,_hl/* Model.Elements.iInfectionTesting */,_ag/* Model.Elements.iBloodSample */),
_hn/* eInfectionMarkersV1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\'s Infection\nDisease Markers\nResults\nfor Verification"));
}),
_ho/* eInfectionMarkersV */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_hn/* Model.Elements.eInfectionMarkersV1 */),
_hp/* a30_4 */ = new T2(0,_ho/* Model.Elements.eInfectionMarkersV */,_91/* Model.Elements.a4 */),
_hq/* a30_6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a30_1"));
}),
_hr/* a30_1 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_hq/* Model.Elements.a30_6 */,_aG/* Model.Elements.a30_5 */,_hp/* Model.Elements.a30_4 */),
_hs/* iInfectionMarkersV */ = new T2(0,_ho/* Model.Elements.eInfectionMarkersV */,_8S/* GHC.Base.Nothing */),
_ht/* ai30_1 */ = new T3(0,_hr/* Model.Elements.a30_1 */,_aF/* Model.Elements.iInfectionTestingV */,_hs/* Model.Elements.iInfectionMarkersV */),
_hu/* eInfectionMarkersW1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\'s Infection\nDisease Markers\nResults\nfor Workup"));
}),
_hv/* eInfectionMarkersW */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_hu/* Model.Elements.eInfectionMarkersW1 */),
_hw/* a30_7 */ = new T2(0,_hv/* Model.Elements.eInfectionMarkersW */,_91/* Model.Elements.a4 */),
_hx/* a30_9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a30_2"));
}),
_hy/* a30_2 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_hx/* Model.Elements.a30_9 */,_aQ/* Model.Elements.a30_8 */,_hw/* Model.Elements.a30_7 */),
_hz/* iInfectionMarkersW */ = new T2(0,_hv/* Model.Elements.eInfectionMarkersW */,_8S/* GHC.Base.Nothing */),
_hA/* ai30_2 */ = new T3(0,_hy/* Model.Elements.a30_2 */,_aM/* Model.Elements.iInfectionTestingW */,_hz/* Model.Elements.iInfectionMarkersW */),
_hB/* a31_4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a31_1"));
}),
_hC/* a31_1 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_hB/* Model.Elements.a31_4 */,_fE/* Model.Elements.a16a_2 */,_hp/* Model.Elements.a30_4 */),
_hD/* ai31_1 */ = new T3(0,_hC/* Model.Elements.a31_1 */,_fH/* Model.Elements.iDonorVerified */,_hs/* Model.Elements.iInfectionMarkersV */),
_hE/* eDonorCleared1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Final Cleared\nDonor"));
}),
_hF/* eDonorCleared */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_hE/* Model.Elements.eDonorCleared1 */),
_hG/* a31_5 */ = new T2(0,_hF/* Model.Elements.eDonorCleared */,_91/* Model.Elements.a4 */),
_hH/* a31_6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a31_2"));
}),
_hI/* a31_2 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_hH/* Model.Elements.a31_6 */,_hG/* Model.Elements.a31_5 */,_hw/* Model.Elements.a30_7 */),
_hJ/* iDonorCleared */ = new T2(0,_hF/* Model.Elements.eDonorCleared */,_8S/* GHC.Base.Nothing */),
_hK/* ai31_2 */ = new T3(0,_hI/* Model.Elements.a31_2 */,_hJ/* Model.Elements.iDonorCleared */,_hz/* Model.Elements.iInfectionMarkersW */),
_hL/* a100 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a32"));
}),
_hM/* eMedicalAssessmentResults4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Medical\nAssessment\nResults"));
}),
_hN/* eMedicalAssessmentResults */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_hM/* Model.Elements.eMedicalAssessmentResults4 */),
_hO/* a98 */ = new T2(0,_hN/* Model.Elements.eMedicalAssessmentResults */,_91/* Model.Elements.a4 */),
_hP/* eMedicalAssessment4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Medical\nAssessment"));
}),
_hQ/* eMedicalAssessment */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_hP/* Model.Elements.eMedicalAssessment4 */),
_hR/* a99 */ = new T2(0,_hQ/* Model.Elements.eMedicalAssessment */,_91/* Model.Elements.a4 */),
_hS/* a32 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_hL/* Model.Elements.a100 */,_hR/* Model.Elements.a99 */,_hO/* Model.Elements.a98 */),
_hT/* iMedicalAssessment */ = new T2(0,_hQ/* Model.Elements.eMedicalAssessment */,_8S/* GHC.Base.Nothing */),
_hU/* iMedicalAssessmentResults */ = new T2(0,_hN/* Model.Elements.eMedicalAssessmentResults */,_8S/* GHC.Base.Nothing */),
_hV/* ai32 */ = new T3(0,_hS/* Model.Elements.a32 */,_hT/* Model.Elements.iMedicalAssessment */,_hU/* Model.Elements.iMedicalAssessmentResults */),
_hW/* eMedicalAssessmentResults5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Initial\nMedical\nAssessment\nResults"));
}),
_hX/* eMedicalAssessmentResults1 */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_hW/* Model.Elements.eMedicalAssessmentResults5 */),
_hY/* a32_4 */ = new T2(0,_hX/* Model.Elements.eMedicalAssessmentResults1 */,_91/* Model.Elements.a4 */),
_hZ/* a32_6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a32_1"));
}),
_i0/* a32_1 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_hZ/* Model.Elements.a32_6 */,_9D/* Model.Elements.a32_5 */,_hY/* Model.Elements.a32_4 */),
_i1/* iMedicalAssessmentResults1 */ = new T2(0,_hX/* Model.Elements.eMedicalAssessmentResults1 */,_8S/* GHC.Base.Nothing */),
_i2/* ai32_1 */ = new T3(0,_i0/* Model.Elements.a32_1 */,_9C/* Model.Elements.iMedicalAssessment1 */,_i1/* Model.Elements.iMedicalAssessmentResults1 */),
_i3/* eMedicalAssessmentResults6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Verification\nMedical\nAssessment\nResults"));
}),
_i4/* eMedicalAssessmentResults2 */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_i3/* Model.Elements.eMedicalAssessmentResults6 */),
_i5/* a32_7 */ = new T2(0,_i4/* Model.Elements.eMedicalAssessmentResults2 */,_91/* Model.Elements.a4 */),
_i6/* a32_9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a32_2"));
}),
_i7/* a32_2 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_i6/* Model.Elements.a32_9 */,_bb/* Model.Elements.a32_8 */,_i5/* Model.Elements.a32_7 */),
_i8/* iMedicalAssessmentResults2 */ = new T2(0,_i4/* Model.Elements.eMedicalAssessmentResults2 */,_8S/* GHC.Base.Nothing */),
_i9/* ai32_2 */ = new T3(0,_i7/* Model.Elements.a32_2 */,_ba/* Model.Elements.iMedicalAssessment2 */,_i8/* Model.Elements.iMedicalAssessmentResults2 */),
_ia/* eMedicalAssessmentResults7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Workup\nMedical\nAssessment\nResults"));
}),
_ib/* eMedicalAssessmentResults3 */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_ia/* Model.Elements.eMedicalAssessmentResults7 */),
_ic/* a32_10 */ = new T2(0,_ib/* Model.Elements.eMedicalAssessmentResults3 */,_91/* Model.Elements.a4 */),
_id/* a32_12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a32_3"));
}),
_ie/* a32_3 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_id/* Model.Elements.a32_12 */,_aY/* Model.Elements.a32_11 */,_ic/* Model.Elements.a32_10 */),
_if/* iMedicalAssessmentResults3 */ = new T2(0,_ib/* Model.Elements.eMedicalAssessmentResults3 */,_8S/* GHC.Base.Nothing */),
_ig/* ai32_3 */ = new T3(0,_ie/* Model.Elements.a32_3 */,_aX/* Model.Elements.iMedicalAssessment3 */,_if/* Model.Elements.iMedicalAssessmentResults3 */),
_ih/* a33_4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a33_1"));
}),
_ii/* a33_1 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_ih/* Model.Elements.a33_4 */,_gQ/* Model.Elements.a81 */,_hY/* Model.Elements.a32_4 */),
_ij/* ai33_1 */ = new T3(0,_ii/* Model.Elements.a33_1 */,_gT/* Model.Elements.iDonorAspirant */,_i1/* Model.Elements.iMedicalAssessmentResults1 */),
_ik/* a33_5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a33_2"));
}),
_il/* a33_2 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_ik/* Model.Elements.a33_5 */,_fE/* Model.Elements.a16a_2 */,_i5/* Model.Elements.a32_7 */),
_im/* ai33_2 */ = new T3(0,_il/* Model.Elements.a33_2 */,_fH/* Model.Elements.iDonorVerified */,_i8/* Model.Elements.iMedicalAssessmentResults2 */),
_in/* eDonorSelection1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Selection\nfor Donation"));
}),
_io/* eDonorSelection */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_in/* Model.Elements.eDonorSelection1 */),
_ip/* a33_3a1 */ = new T2(0,_io/* Model.Elements.eDonorSelection */,_91/* Model.Elements.a4 */),
_iq/* eDonorChosen1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Chosen\nfor Donation"));
}),
_ir/* eDonorChosen */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_iq/* Model.Elements.eDonorChosen1 */),
_is/* a33_3a2 */ = new T2(0,_ir/* Model.Elements.eDonorChosen */,_91/* Model.Elements.a4 */),
_it/* a33_3a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a33_3a"));
}),
_iu/* a33_3a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_it/* Model.Elements.a33_3a3 */,_is/* Model.Elements.a33_3a2 */,_ip/* Model.Elements.a33_3a1 */),
_iv/* iDonorChosen */ = new T2(0,_ir/* Model.Elements.eDonorChosen */,_8S/* GHC.Base.Nothing */),
_iw/* iDonorSelection */ = new T2(0,_io/* Model.Elements.eDonorSelection */,_8S/* GHC.Base.Nothing */),
_ix/* ai33_3a */ = new T3(0,_iu/* Model.Elements.a33_3a */,_iv/* Model.Elements.iDonorChosen */,_iw/* Model.Elements.iDonorSelection */),
_iy/* a33_3b1 */ = new T2(0,_io/* Model.Elements.eDonorSelection */,_ax/* Model.Elements.a16a_2a2 */),
_iz/* a33_3b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a33_3b"));
}),
_iA/* a33_3b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_iz/* Model.Elements.a33_3b2 */,_iy/* Model.Elements.a33_3b1 */,_fW/* Model.Elements.a17b1 */),
_iB/* ai33_3b */ = new T3(0,_iA/* Model.Elements.a33_3b */,_iw/* Model.Elements.iDonorSelection */,_g0/* Model.Elements.iTransplantCentre */),
_iC/* ePositiveEvaluation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Positive\nEvaluation"));
}),
_iD/* ePositiveEvaluation */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_iC/* Model.Elements.ePositiveEvaluation1 */),
_iE/* a34a1 */ = new T2(0,_iD/* Model.Elements.ePositiveEvaluation */,_9X/* Model.Elements.a18a2 */),
_iF/* eVerificationEvaluation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Verification\nEvaluation"));
}),
_iG/* eVerificationEvaluation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_iF/* Model.Elements.eVerificationEvaluation1 */),
_iH/* a34a2 */ = new T2(0,_iG/* Model.Elements.eVerificationEvaluation */,_91/* Model.Elements.a4 */),
_iI/* a34a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34a"));
}),
_iJ/* a34a */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_iI/* Model.Elements.a34a3 */,_iH/* Model.Elements.a34a2 */,_iE/* Model.Elements.a34a1 */),
_iK/* iPositiveEvaluation */ = new T2(0,_iD/* Model.Elements.ePositiveEvaluation */,_8S/* GHC.Base.Nothing */),
_iL/* iVerificationEvaluation */ = new T2(0,_iG/* Model.Elements.eVerificationEvaluation */,_8S/* GHC.Base.Nothing */),
_iM/* ai34a */ = new T3(0,_iJ/* Model.Elements.a34a */,_iL/* Model.Elements.iVerificationEvaluation */,_iK/* Model.Elements.iPositiveEvaluation */),
_iN/* eNegativeEvaluation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Negative\nEvaluation"));
}),
_iO/* eNegativeEvaluation */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_iN/* Model.Elements.eNegativeEvaluation1 */),
_iP/* a34b1 */ = new T2(0,_iO/* Model.Elements.eNegativeEvaluation */,_9X/* Model.Elements.a18a2 */),
_iQ/* a34b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34b"));
}),
_iR/* a34b */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_iQ/* Model.Elements.a34b2 */,_iH/* Model.Elements.a34a2 */,_iP/* Model.Elements.a34b1 */),
_iS/* iNegativeEvaluation */ = new T2(0,_iO/* Model.Elements.eNegativeEvaluation */,_8S/* GHC.Base.Nothing */),
_iT/* ai34b */ = new T3(0,_iR/* Model.Elements.a34b */,_iL/* Model.Elements.iVerificationEvaluation */,_iS/* Model.Elements.iNegativeEvaluation */),
_iU/* eDonorReserved1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reserved\nDonor"));
}),
_iV/* eDonorReserved */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_iU/* Model.Elements.eDonorReserved1 */),
_iW/* a34_1a1 */ = new T2(0,_iV/* Model.Elements.eDonorReserved */,_91/* Model.Elements.a4 */),
_iX/* eDonorReservation1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nReservation"));
}),
_iY/* eDonorReservation */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_iX/* Model.Elements.eDonorReservation1 */),
_iZ/* a34_1a2 */ = new T2(0,_iY/* Model.Elements.eDonorReservation */,_91/* Model.Elements.a4 */),
_j0/* a34_1a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_1a"));
}),
_j1/* a34_1a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_j0/* Model.Elements.a34_1a3 */,_iZ/* Model.Elements.a34_1a2 */,_iW/* Model.Elements.a34_1a1 */),
_j2/* iDonorReservation */ = new T2(0,_iY/* Model.Elements.eDonorReservation */,_8S/* GHC.Base.Nothing */),
_j3/* iDonorReserved */ = new T2(0,_iV/* Model.Elements.eDonorReserved */,_8S/* GHC.Base.Nothing */),
_j4/* ai34_1a */ = new T3(0,_j1/* Model.Elements.a34_1a */,_j2/* Model.Elements.iDonorReservation */,_j3/* Model.Elements.iDonorReserved */),
_j5/* a34_1b1 */ = new T2(0,_iY/* Model.Elements.eDonorReservation */,_ax/* Model.Elements.a16a_2a2 */),
_j6/* a34_1b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_1b"));
}),
_j7/* a34_1b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_j6/* Model.Elements.a34_1b2 */,_j5/* Model.Elements.a34_1b1 */,_fW/* Model.Elements.a17b1 */),
_j8/* ai34_1b */ = new T3(0,_j7/* Model.Elements.a34_1b */,_j2/* Model.Elements.iDonorReservation */,_g0/* Model.Elements.iTransplantCentre */),
_j9/* a34_2aa2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_2aa"));
}),
_ja/* a34_2aa */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_j9/* Model.Elements.a34_2aa2 */,_hG/* Model.Elements.a31_5 */,_aR/* Model.Elements.a34_2aa1 */),
_jb/* ai34_2aa */ = new T3(0,_ja/* Model.Elements.a34_2aa */,_hJ/* Model.Elements.iDonorCleared */,_aP/* Model.Elements.iWorkup */),
_jc/* eDonorClearance1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nClearance"));
}),
_jd/* eDonorClearance */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_jc/* Model.Elements.eDonorClearance1 */),
_je/* a34_2ab1 */ = new T2(0,_jd/* Model.Elements.eDonorClearance */,_91/* Model.Elements.a4 */),
_jf/* a34_2ab2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_2ab"));
}),
_jg/* a34_2ab */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_jf/* Model.Elements.a34_2ab2 */,_hG/* Model.Elements.a31_5 */,_je/* Model.Elements.a34_2ab1 */),
_jh/* iDonorClearance */ = new T2(0,_jd/* Model.Elements.eDonorClearance */,_8S/* GHC.Base.Nothing */),
_ji/* ai34_2ab */ = new T3(0,_jg/* Model.Elements.a34_2ab */,_hJ/* Model.Elements.iDonorCleared */,_jh/* Model.Elements.iDonorClearance */),
_jj/* eDonorClearanceAcc1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Clearance\nAccepted"));
}),
_jk/* eDonorClearanceAcc */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_jj/* Model.Elements.eDonorClearanceAcc1 */),
_jl/* a34_2b1 */ = new T2(0,_jk/* Model.Elements.eDonorClearanceAcc */,_91/* Model.Elements.a4 */),
_jm/* eDonorAccepted1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Accepted\nfor Collection"));
}),
_jn/* eDonorAccepted */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_jm/* Model.Elements.eDonorAccepted1 */),
_jo/* a34_2b2 */ = new T2(0,_jn/* Model.Elements.eDonorAccepted */,_91/* Model.Elements.a4 */),
_jp/* a34_2b3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_2b"));
}),
_jq/* a34_2b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_jp/* Model.Elements.a34_2b3 */,_jo/* Model.Elements.a34_2b2 */,_jl/* Model.Elements.a34_2b1 */),
_jr/* iDonorAccepted */ = new T2(0,_jn/* Model.Elements.eDonorAccepted */,_8S/* GHC.Base.Nothing */),
_js/* iDonorClearanceAcc */ = new T2(0,_jk/* Model.Elements.eDonorClearanceAcc */,_8S/* GHC.Base.Nothing */),
_jt/* ai34_2b */ = new T3(0,_jq/* Model.Elements.a34_2b */,_jr/* Model.Elements.iDonorAccepted */,_js/* Model.Elements.iDonorClearanceAcc */),
_ju/* a34_2c1 */ = new T2(0,_jk/* Model.Elements.eDonorClearanceAcc */,_ax/* Model.Elements.a16a_2a2 */),
_jv/* a34_2c2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_2c"));
}),
_jw/* a34_2c */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_jv/* Model.Elements.a34_2c2 */,_ju/* Model.Elements.a34_2c1 */,_fW/* Model.Elements.a17b1 */),
_jx/* ai34_2c */ = new T3(0,_jw/* Model.Elements.a34_2c */,_js/* Model.Elements.iDonorClearanceAcc */,_g0/* Model.Elements.iTransplantCentre */),
_jy/* eCollectionReasoning1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reasoning\nof Collection\nof Noneligible\nDonor"));
}),
_jz/* eCollectionReasoning */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_jy/* Model.Elements.eCollectionReasoning1 */),
_jA/* a34_2d1 */ = new T2(0,_jz/* Model.Elements.eCollectionReasoning */,_9X/* Model.Elements.a18a2 */),
_jB/* a34_2d2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a34_2d"));
}),
_jC/* a34_2d */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_jB/* Model.Elements.a34_2d2 */,_jl/* Model.Elements.a34_2b1 */,_jA/* Model.Elements.a34_2d1 */),
_jD/* iCollectionReasoning */ = new T2(0,_jz/* Model.Elements.eCollectionReasoning */,_8S/* GHC.Base.Nothing */),
_jE/* ai34_2d */ = new T3(0,_jC/* Model.Elements.a34_2d */,_js/* Model.Elements.iDonorClearanceAcc */,_jD/* Model.Elements.iCollectionReasoning */),
_jF/* a35_2 */ = new T2(0,_iG/* Model.Elements.eVerificationEvaluation */,_dB/* Model.Elements.a18 */),
_jG/* a35_3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a35_1"));
}),
_jH/* a35_1 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_jG/* Model.Elements.a35_3 */,_jF/* Model.Elements.a35_2 */,_fW/* Model.Elements.a17b1 */),
_jI/* ai35_1 */ = new T3(0,_jH/* Model.Elements.a35_1 */,_iL/* Model.Elements.iVerificationEvaluation */,_g0/* Model.Elements.iTransplantCentre */),
_jJ/* eCollectedPBSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collected PBSC"));
}),
_jK/* eCollectedPBSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_jJ/* Model.Elements.eCollectedPBSC1 */),
_jL/* a36a1 */ = new T2(0,_jK/* Model.Elements.eCollectedPBSC */,_91/* Model.Elements.a4 */),
_jM/* a36a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a36a"));
}),
_jN/* a36a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_jM/* Model.Elements.a36a3 */,_bE/* Model.Elements.a36a2 */,_jL/* Model.Elements.a36a1 */),
_jO/* iCollectedPBSC */ = new T2(0,_jK/* Model.Elements.eCollectedPBSC */,_8S/* GHC.Base.Nothing */),
_jP/* ai36a */ = new T3(0,_jN/* Model.Elements.a36a */,_bD/* Model.Elements.iPBSCCollection */,_jO/* Model.Elements.iCollectedPBSC */),
_jQ/* eDonorPBSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC Donor"));
}),
_jR/* eDonorPBSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_jQ/* Model.Elements.eDonorPBSC1 */),
_jS/* a36aa1 */ = new T2(0,_jR/* Model.Elements.eDonorPBSC */,_91/* Model.Elements.a4 */),
_jT/* a36aa2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a36aa"));
}),
_jU/* a36aa */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_jT/* Model.Elements.a36aa2 */,_bE/* Model.Elements.a36a2 */,_jS/* Model.Elements.a36aa1 */),
_jV/* iDonorPBSC */ = new T2(0,_jR/* Model.Elements.eDonorPBSC */,_8S/* GHC.Base.Nothing */),
_jW/* ai36aa */ = new T3(0,_jU/* Model.Elements.a36aa */,_bD/* Model.Elements.iPBSCCollection */,_jV/* Model.Elements.iDonorPBSC */),
_jX/* eDonorBM1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nDonor"));
}),
_jY/* eDonorBM */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_jX/* Model.Elements.eDonorBM1 */),
_jZ/* a36ba1 */ = new T2(0,_jY/* Model.Elements.eDonorBM */,_91/* Model.Elements.a4 */),
_k0/* a36ba2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a36ba"));
}),
_k1/* a36ba */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_k0/* Model.Elements.a36ba2 */,_bO/* Model.Elements.a36b2 */,_jZ/* Model.Elements.a36ba1 */),
_k2/* iDonorBM */ = new T2(0,_jY/* Model.Elements.eDonorBM */,_8S/* GHC.Base.Nothing */),
_k3/* ai36ba */ = new T3(0,_k1/* Model.Elements.a36ba */,_bK/* Model.Elements.iBMCollection */,_k2/* Model.Elements.iDonorBM */),
_k4/* eCollectedBM1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collected BM"));
}),
_k5/* eCollectedBM */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_k4/* Model.Elements.eCollectedBM1 */),
_k6/* a36b1 */ = new T2(0,_k5/* Model.Elements.eCollectedBM */,_91/* Model.Elements.a4 */),
_k7/* a36b3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a36b"));
}),
_k8/* a36b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_k7/* Model.Elements.a36b3 */,_bO/* Model.Elements.a36b2 */,_k6/* Model.Elements.a36b1 */),
_k9/* iCollectedBM */ = new T2(0,_k5/* Model.Elements.eCollectedBM */,_8S/* GHC.Base.Nothing */),
_ka/* ai36b */ = new T3(0,_k8/* Model.Elements.a36b */,_bK/* Model.Elements.iBMCollection */,_k9/* Model.Elements.iCollectedBM */),
_kb/* eCollectedDLI1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collected DLI"));
}),
_kc/* eCollectedDLI */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_kb/* Model.Elements.eCollectedDLI1 */),
_kd/* a36c1 */ = new T2(0,_kc/* Model.Elements.eCollectedDLI */,_91/* Model.Elements.a4 */),
_ke/* a36c3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a36c"));
}),
_kf/* a36c */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ke/* Model.Elements.a36c3 */,_bZ/* Model.Elements.a36c2 */,_kd/* Model.Elements.a36c1 */),
_kg/* iCollectedDLI */ = new T2(0,_kc/* Model.Elements.eCollectedDLI */,_8S/* GHC.Base.Nothing */),
_kh/* ai36c */ = new T3(0,_kf/* Model.Elements.a36c */,_bV/* Model.Elements.iDLICollection */,_kg/* Model.Elements.iCollectedDLI */),
_ki/* eDonorPBSCPrepared1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC Prepared\nDonor"));
}),
_kj/* eDonorPBSCPrepared */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_ki/* Model.Elements.eDonorPBSCPrepared1 */),
_kk/* a37a1 */ = new T2(0,_kj/* Model.Elements.eDonorPBSCPrepared */,_91/* Model.Elements.a4 */),
_kl/* a37a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a37a"));
}),
_km/* a37a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_kl/* Model.Elements.a37a3 */,_bl/* Model.Elements.a37a2 */,_kk/* Model.Elements.a37a1 */),
_kn/* iDonorPBSCPrepared */ = new T2(0,_kj/* Model.Elements.eDonorPBSCPrepared */,_8S/* GHC.Base.Nothing */),
_ko/* ai37a */ = new T3(0,_km/* Model.Elements.a37a */,_bk/* Model.Elements.iPBSCPreparation */,_kn/* Model.Elements.iDonorPBSCPrepared */),
_kp/* a37b1 */ = new T2(0,_bj/* Model.Elements.ePBSCPreparation */,_ax/* Model.Elements.a16a_2a2 */),
_kq/* a37b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a37b"));
}),
_kr/* a37b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_kq/* Model.Elements.a37b2 */,_kp/* Model.Elements.a37b1 */,_dA/* Model.Elements.a74 */),
_ks/* ai37b */ = new T3(0,_kr/* Model.Elements.a37b */,_bk/* Model.Elements.iPBSCPreparation */,_8W/* Model.Elements.iDonorCentre */),
_kt/* a102 */ = new T2(0,_br/* Model.Elements.eCollection */,_dB/* Model.Elements.a18 */),
_ku/* a103 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a39"));
}),
_kv/* a39 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ku/* Model.Elements.a103 */,_kt/* Model.Elements.a102 */,_cQ/* Model.Elements.a101 */),
_kw/* ai39 */ = new T3(0,_kv/* Model.Elements.a39 */,_bs/* Model.Elements.iCollection */,_cP/* Model.Elements.iCollectionCentre */),
_kx/* a104 */ = new T2(0,_cm/* Model.Elements.eTransplantation */,_ax/* Model.Elements.a16a_2a2 */),
_ky/* a105 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a40"));
}),
_kz/* a40 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ky/* Model.Elements.a105 */,_kx/* Model.Elements.a104 */,_fW/* Model.Elements.a17b1 */),
_kA/* ai40 */ = new T3(0,_kz/* Model.Elements.a40 */,_cn/* Model.Elements.iTransplantation */,_g0/* Model.Elements.iTransplantCentre */),
_kB/* eTransplantedPBSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplanted PBSC"));
}),
_kC/* eTransplantedPBSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_kB/* Model.Elements.eTransplantedPBSC1 */),
_kD/* a42_1a1 */ = new T2(0,_kC/* Model.Elements.eTransplantedPBSC */,_91/* Model.Elements.a4 */),
_kE/* a42_1a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a42_1a"));
}),
_kF/* a42_1a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_kE/* Model.Elements.a42_1a3 */,_cv/* Model.Elements.a42_1a2 */,_kD/* Model.Elements.a42_1a1 */),
_kG/* iTransplantedPBSC */ = new T2(0,_kC/* Model.Elements.eTransplantedPBSC */,_8S/* GHC.Base.Nothing */),
_kH/* ai42_1a */ = new T3(0,_kF/* Model.Elements.a42_1a */,_cu/* Model.Elements.iPBSCTransplantation */,_kG/* Model.Elements.iTransplantedPBSC */),
_kI/* ePBSCPatient1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nRecipient"));
}),
_kJ/* ePBSCPatient */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_kI/* Model.Elements.ePBSCPatient1 */),
_kK/* a42_1b1 */ = new T2(0,_kJ/* Model.Elements.ePBSCPatient */,_91/* Model.Elements.a4 */),
_kL/* a42_1b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a42_1b"));
}),
_kM/* a42_1b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_kL/* Model.Elements.a42_1b2 */,_cv/* Model.Elements.a42_1a2 */,_kK/* Model.Elements.a42_1b1 */),
_kN/* iPBSCPatient */ = new T2(0,_kJ/* Model.Elements.ePBSCPatient */,_8S/* GHC.Base.Nothing */),
_kO/* ai42_1b */ = new T3(0,_kM/* Model.Elements.a42_1b */,_cu/* Model.Elements.iPBSCTransplantation */,_kN/* Model.Elements.iPBSCPatient */),
_kP/* eTransplantedBM1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplanted BM"));
}),
_kQ/* eTransplantedBM */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_kP/* Model.Elements.eTransplantedBM1 */),
_kR/* a42_2a1 */ = new T2(0,_kQ/* Model.Elements.eTransplantedBM */,_91/* Model.Elements.a4 */),
_kS/* a42_2a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a42_2a"));
}),
_kT/* a42_2a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_kS/* Model.Elements.a42_2a3 */,_cC/* Model.Elements.a42_2a2 */,_kR/* Model.Elements.a42_2a1 */),
_kU/* iTransplantedBM */ = new T2(0,_kQ/* Model.Elements.eTransplantedBM */,_8S/* GHC.Base.Nothing */),
_kV/* ai42_2a */ = new T3(0,_kT/* Model.Elements.a42_2a */,_cB/* Model.Elements.iBMTransplantation */,_kU/* Model.Elements.iTransplantedBM */),
_kW/* eBMPatient1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nRecipient"));
}),
_kX/* eBMPatient */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_kW/* Model.Elements.eBMPatient1 */),
_kY/* a42_2c1 */ = new T2(0,_kX/* Model.Elements.eBMPatient */,_91/* Model.Elements.a4 */),
_kZ/* a42_2c2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a42_2c"));
}),
_l0/* a42_2c */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_kZ/* Model.Elements.a42_2c2 */,_cC/* Model.Elements.a42_2a2 */,_kY/* Model.Elements.a42_2c1 */),
_l1/* iBMPatient */ = new T2(0,_kX/* Model.Elements.eBMPatient */,_8S/* GHC.Base.Nothing */),
_l2/* ai42_2c */ = new T3(0,_l0/* Model.Elements.a42_2c */,_cB/* Model.Elements.iBMTransplantation */,_l1/* Model.Elements.iBMPatient */),
_l3/* eTransplantedDLI1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplanted DLI"));
}),
_l4/* eTransplantedDLI */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_l3/* Model.Elements.eTransplantedDLI1 */),
_l5/* a42_3a1 */ = new T2(0,_l4/* Model.Elements.eTransplantedDLI */,_91/* Model.Elements.a4 */),
_l6/* a42_3a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a42_3a"));
}),
_l7/* a42_3a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_l6/* Model.Elements.a42_3a3 */,_cJ/* Model.Elements.a42_3a2 */,_l5/* Model.Elements.a42_3a1 */),
_l8/* iTransplantedDLI */ = new T2(0,_l4/* Model.Elements.eTransplantedDLI */,_8S/* GHC.Base.Nothing */),
_l9/* ai42_3a */ = new T3(0,_l7/* Model.Elements.a42_3a */,_cI/* Model.Elements.iDLITransplantation */,_l8/* Model.Elements.iTransplantedDLI */),
_la/* eDLIPatient1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent HSC\nRecipient"));
}),
_lb/* eDLIPatient */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_la/* Model.Elements.eDLIPatient1 */),
_lc/* a42_3c1 */ = new T2(0,_lb/* Model.Elements.eDLIPatient */,_91/* Model.Elements.a4 */),
_ld/* a42_3c3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a42_3c"));
}),
_le/* a42_3c */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ld/* Model.Elements.a42_3c3 */,_co/* Model.Elements.a42_3c2 */,_lc/* Model.Elements.a42_3c1 */),
_lf/* iDLIPatient */ = new T2(0,_lb/* Model.Elements.eDLIPatient */,_8S/* GHC.Base.Nothing */),
_lg/* ai42_3c */ = new T3(0,_le/* Model.Elements.a42_3c */,_cn/* Model.Elements.iTransplantation */,_lf/* Model.Elements.iDLIPatient */),
_lh/* eSchedule1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Schedule"));
}),
_li/* eSchedule */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_lh/* Model.Elements.eSchedule1 */),
_lj/* a106 */ = new T2(0,_li/* Model.Elements.eSchedule */,_91/* Model.Elements.a4 */),
_lk/* a107 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a44"));
}),
_ll/* a44 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_lk/* Model.Elements.a107 */,_is/* Model.Elements.a33_3a2 */,_lj/* Model.Elements.a106 */),
_lm/* iSchedule */ = new T2(0,_li/* Model.Elements.eSchedule */,_8S/* GHC.Base.Nothing */),
_ln/* ai44 */ = new T3(0,_ll/* Model.Elements.a44 */,_iv/* Model.Elements.iDonorChosen */,_lm/* Model.Elements.iSchedule */),
_lo/* eSpecification1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Clereance\nSpecification"));
}),
_lp/* eSpecification */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_lo/* Model.Elements.eSpecification1 */),
_lq/* a108 */ = new T2(0,_lp/* Model.Elements.eSpecification */,_91/* Model.Elements.a4 */),
_lr/* a109 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a45"));
}),
_ls/* a45 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_lr/* Model.Elements.a109 */,_is/* Model.Elements.a33_3a2 */,_lq/* Model.Elements.a108 */),
_lt/* iSpecification */ = new T2(0,_lp/* Model.Elements.eSpecification */,_8S/* GHC.Base.Nothing */),
_lu/* ai45 */ = new T3(0,_ls/* Model.Elements.a45 */,_iv/* Model.Elements.iDonorChosen */,_lt/* Model.Elements.iSpecification */),
_lv/* eDonorSubsequentRel1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Selection\nfor Subsequent Donation"));
}),
_lw/* eDonorSubsequentRel */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_lv/* Model.Elements.eDonorSubsequentRel1 */),
_lx/* a110 */ = new T2(0,_lw/* Model.Elements.eDonorSubsequentRel */,_91/* Model.Elements.a4 */),
_ly/* eDonorSubsequent1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Chosen for\nSubsequent Donation"));
}),
_lz/* eDonorSubsequent */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_ly/* Model.Elements.eDonorSubsequent1 */),
_lA/* a111 */ = new T2(0,_lz/* Model.Elements.eDonorSubsequent */,_91/* Model.Elements.a4 */),
_lB/* a112 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a46"));
}),
_lC/* a46 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_lB/* Model.Elements.a112 */,_lA/* Model.Elements.a111 */,_lx/* Model.Elements.a110 */),
_lD/* iDonorSubsequent */ = new T2(0,_lz/* Model.Elements.eDonorSubsequent */,_8S/* GHC.Base.Nothing */),
_lE/* iDonorSubsequentRel */ = new T2(0,_lw/* Model.Elements.eDonorSubsequentRel */,_8S/* GHC.Base.Nothing */),
_lF/* ai46 */ = new T3(0,_lC/* Model.Elements.a46 */,_lD/* Model.Elements.iDonorSubsequent */,_lE/* Model.Elements.iDonorSubsequentRel */),
_lG/* a47a1 */ = new T2(0,_lw/* Model.Elements.eDonorSubsequentRel */,_ax/* Model.Elements.a16a_2a2 */),
_lH/* a47a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a47a"));
}),
_lI/* a47a */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_lH/* Model.Elements.a47a2 */,_lG/* Model.Elements.a47a1 */,_fW/* Model.Elements.a17b1 */),
_lJ/* ai47a */ = new T3(0,_lI/* Model.Elements.a47a */,_lE/* Model.Elements.iDonorSubsequentRel */,_g0/* Model.Elements.iTransplantCentre */),
_lK/* a47b1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a47b"));
}),
_lL/* a47b */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_lK/* Model.Elements.a47b1 */,_lG/* Model.Elements.a47a1 */,_92/* Model.Elements.a70 */),
_lM/* ai47b */ = new T3(0,_lL/* Model.Elements.a47b */,_lE/* Model.Elements.iDonorSubsequentRel */,_8Z/* Model.Elements.iDonorRegistry */),
_lN/* eScheduleSub1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Schedule\nfor Subsequent Donation"));
}),
_lO/* eScheduleSub */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_lN/* Model.Elements.eScheduleSub1 */),
_lP/* a113 */ = new T2(0,_lO/* Model.Elements.eScheduleSub */,_91/* Model.Elements.a4 */),
_lQ/* a114 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a48"));
}),
_lR/* a48 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_lQ/* Model.Elements.a114 */,_lA/* Model.Elements.a111 */,_lP/* Model.Elements.a113 */),
_lS/* iScheduleSub */ = new T2(0,_lO/* Model.Elements.eScheduleSub */,_8S/* GHC.Base.Nothing */),
_lT/* ai48 */ = new T3(0,_lR/* Model.Elements.a48 */,_lD/* Model.Elements.iDonorSubsequent */,_lS/* Model.Elements.iScheduleSub */),
_lU/* eSpecificationSub1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Clereance\nSpecification\nfor Subsequent Donation"));
}),
_lV/* eSpecificationSub */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_lU/* Model.Elements.eSpecificationSub1 */),
_lW/* a115 */ = new T2(0,_lV/* Model.Elements.eSpecificationSub */,_91/* Model.Elements.a4 */),
_lX/* a116 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a49"));
}),
_lY/* a49 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_lX/* Model.Elements.a116 */,_lA/* Model.Elements.a111 */,_lW/* Model.Elements.a115 */),
_lZ/* iSpecificationSub */ = new T2(0,_lV/* Model.Elements.eSpecificationSub */,_8S/* GHC.Base.Nothing */),
_m0/* ai49 */ = new T3(0,_lY/* Model.Elements.a49 */,_lD/* Model.Elements.iDonorSubsequent */,_lZ/* Model.Elements.iSpecificationSub */),
_m1/* eDonorClearedSub1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Final Cleared\nDonor\nfor Subsequent Donation"));
}),
_m2/* eDonorClearedSub */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_m1/* Model.Elements.eDonorClearedSub1 */),
_m3/* a117 */ = new T2(0,_m2/* Model.Elements.eDonorClearedSub */,_dB/* Model.Elements.a18 */),
_m4/* a118 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a50"));
}),
_m5/* a50 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_m4/* Model.Elements.a118 */,_m3/* Model.Elements.a117 */,_aR/* Model.Elements.a34_2aa1 */),
_m6/* iDonorClearedSub */ = new T2(0,_m2/* Model.Elements.eDonorClearedSub */,_8S/* GHC.Base.Nothing */),
_m7/* ai50 */ = new T3(0,_m5/* Model.Elements.a50 */,_m6/* Model.Elements.iDonorClearedSub */,_aP/* Model.Elements.iWorkup */),
_m8/* eDonorClearanceSub1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nClearance\nfor Subsequent Donation"));
}),
_m9/* eDonorClearanceSub */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_m8/* Model.Elements.eDonorClearanceSub1 */),
_ma/* a119 */ = new T2(0,_m9/* Model.Elements.eDonorClearanceSub */,_91/* Model.Elements.a4 */),
_mb/* a120 */ = new T2(0,_m2/* Model.Elements.eDonorClearedSub */,_91/* Model.Elements.a4 */),
_mc/* a121 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a51"));
}),
_md/* a51 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_mc/* Model.Elements.a121 */,_mb/* Model.Elements.a120 */,_ma/* Model.Elements.a119 */),
_me/* ai51 */ = new T3(0,_md/* Model.Elements.a51 */,_m6/* Model.Elements.iDonorClearedSub */,_jh/* Model.Elements.iDonorClearance */),
_mf/* eDonorClearanceSubAcc1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Clearance\nfor Subsequent Donation\nAccepted"));
}),
_mg/* eDonorClearanceSubAcc */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_mf/* Model.Elements.eDonorClearanceSubAcc1 */),
_mh/* a122 */ = new T2(0,_mg/* Model.Elements.eDonorClearanceSubAcc */,_91/* Model.Elements.a4 */),
_mi/* eDonorAcceptedSub1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Accepted\nfor Subsequent Collection"));
}),
_mj/* eDonorAcceptedSub */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_mi/* Model.Elements.eDonorAcceptedSub1 */),
_mk/* a123 */ = new T2(0,_mj/* Model.Elements.eDonorAcceptedSub */,_91/* Model.Elements.a4 */),
_ml/* a124 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a52"));
}),
_mm/* a52 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ml/* Model.Elements.a124 */,_mk/* Model.Elements.a123 */,_mh/* Model.Elements.a122 */),
_mn/* iDonorAcceptedSub */ = new T2(0,_mj/* Model.Elements.eDonorAcceptedSub */,_8S/* GHC.Base.Nothing */),
_mo/* iDonorClearanceSubAcc */ = new T2(0,_mg/* Model.Elements.eDonorClearanceSubAcc */,_8S/* GHC.Base.Nothing */),
_mp/* ai52 */ = new T3(0,_mm/* Model.Elements.a52 */,_mn/* Model.Elements.iDonorAcceptedSub */,_mo/* Model.Elements.iDonorClearanceSubAcc */),
_mq/* a125 */ = new T2(0,_mg/* Model.Elements.eDonorClearanceSubAcc */,_ax/* Model.Elements.a16a_2a2 */),
_mr/* a126 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a53"));
}),
_ms/* a53 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_mr/* Model.Elements.a126 */,_mq/* Model.Elements.a125 */,_fW/* Model.Elements.a17b1 */),
_mt/* ai53 */ = new T3(0,_ms/* Model.Elements.a53 */,_mo/* Model.Elements.iDonorClearanceSubAcc */,_g0/* Model.Elements.iTransplantCentre */),
_mu/* eDonorSubsequentHSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent\nHSC Donor"));
}),
_mv/* eDonorSubsequentHSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_mu/* Model.Elements.eDonorSubsequentHSC1 */),
_mw/* a128 */ = new T2(0,_mv/* Model.Elements.eDonorSubsequentHSC */,_91/* Model.Elements.a4 */),
_mx/* a129 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a54"));
}),
_my/* a54 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_mx/* Model.Elements.a129 */,_mw/* Model.Elements.a128 */,_bw/* Model.Elements.a127 */),
_mz/* iDonorSubsequentHSC */ = new T2(0,_mv/* Model.Elements.eDonorSubsequentHSC */,_8S/* GHC.Base.Nothing */),
_mA/* ai54 */ = new T3(0,_my/* Model.Elements.a54 */,_mz/* Model.Elements.iDonorSubsequentHSC */,_bs/* Model.Elements.iCollection */),
_mB/* eCourierTraining1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Courier Training"));
}),
_mC/* eCourierTraining */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_mB/* Model.Elements.eCourierTraining1 */),
_mD/* a130 */ = new T2(0,_mC/* Model.Elements.eCourierTraining */,_91/* Model.Elements.a4 */),
_mE/* eCourier1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Courier"));
}),
_mF/* eCourier */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_mE/* Model.Elements.eCourier1 */),
_mG/* a131 */ = new T2(0,_mF/* Model.Elements.eCourier */,_91/* Model.Elements.a4 */),
_mH/* a132 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a55"));
}),
_mI/* a55 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_mH/* Model.Elements.a132 */,_mG/* Model.Elements.a131 */,_mD/* Model.Elements.a130 */),
_mJ/* iCourier */ = new T2(0,_mF/* Model.Elements.eCourier */,_8S/* GHC.Base.Nothing */),
_mK/* iCourierTraining */ = new T2(0,_mC/* Model.Elements.eCourierTraining */,_8S/* GHC.Base.Nothing */),
_mL/* ai55 */ = new T3(0,_mI/* Model.Elements.a55 */,_mJ/* Model.Elements.iCourier */,_mK/* Model.Elements.iCourierTraining */),
_mM/* eCourierTrainingRecord1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Courier Training Record"));
}),
_mN/* eCourierTrainingRecord */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_mM/* Model.Elements.eCourierTrainingRecord1 */),
_mO/* a133 */ = new T2(0,_mN/* Model.Elements.eCourierTrainingRecord */,_91/* Model.Elements.a4 */),
_mP/* a134 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a56"));
}),
_mQ/* a56 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_mP/* Model.Elements.a134 */,_mG/* Model.Elements.a131 */,_mO/* Model.Elements.a133 */),
_mR/* iCourierTrainingRecord */ = new T2(0,_mN/* Model.Elements.eCourierTrainingRecord */,_8S/* GHC.Base.Nothing */),
_mS/* ai56 */ = new T3(0,_mQ/* Model.Elements.a56 */,_mJ/* Model.Elements.iCourier */,_mR/* Model.Elements.iCourierTrainingRecord */),
_mT/* a135 */ = new T2(0,_mC/* Model.Elements.eCourierTraining */,_ax/* Model.Elements.a16a_2a2 */),
_mU/* a136 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a57"));
}),
_mV/* a57 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_mU/* Model.Elements.a136 */,_mT/* Model.Elements.a135 */,_92/* Model.Elements.a70 */),
_mW/* ai57 */ = new T3(0,_mV/* Model.Elements.a57 */,_mK/* Model.Elements.iCourierTraining */,_8Z/* Model.Elements.iDonorRegistry */),
_mX/* a137 */ = new T2(0,_c5/* Model.Elements.eTransport */,_ax/* Model.Elements.a16a_2a2 */),
_mY/* a138 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a58"));
}),
_mZ/* a58 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_mY/* Model.Elements.a138 */,_mG/* Model.Elements.a131 */,_mX/* Model.Elements.a137 */),
_n0/* ai58_1 */ = new T3(0,_mZ/* Model.Elements.a58 */,_mJ/* Model.Elements.iCourier */,_c8/* Model.Elements.iTransportPBSC */),
_n1/* ai58_2 */ = new T3(0,_mZ/* Model.Elements.a58 */,_mJ/* Model.Elements.iCourier */,_cf/* Model.Elements.iTransportBM */),
_n2/* ai58_3 */ = new T3(0,_mZ/* Model.Elements.a58 */,_mJ/* Model.Elements.iCourier */,_cj/* Model.Elements.iTransportDLI */),
_n3/* eDeliveredPBSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Delivered PBSC"));
}),
_n4/* eDeliveredPBSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_n3/* Model.Elements.eDeliveredPBSC1 */),
_n5/* a59_4 */ = new T2(0,_n4/* Model.Elements.eDeliveredPBSC */,_9X/* Model.Elements.a18a2 */),
_n6/* a59_6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a59_1"));
}),
_n7/* a59_1 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_n6/* Model.Elements.a59_6 */,_c9/* Model.Elements.a59_5 */,_n5/* Model.Elements.a59_4 */),
_n8/* iDeliveredPBSC */ = new T2(0,_n4/* Model.Elements.eDeliveredPBSC */,_8S/* GHC.Base.Nothing */),
_n9/* ai59_1 */ = new T3(0,_n7/* Model.Elements.a59_1 */,_c8/* Model.Elements.iTransportPBSC */,_n8/* Model.Elements.iDeliveredPBSC */),
_na/* eDeliveredBM1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Delivered BM"));
}),
_nb/* eDeliveredBM */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_na/* Model.Elements.eDeliveredBM1 */),
_nc/* a59_7 */ = new T2(0,_nb/* Model.Elements.eDeliveredBM */,_9X/* Model.Elements.a18a2 */),
_nd/* a59_8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a59_2"));
}),
_ne/* a59_2 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_nd/* Model.Elements.a59_8 */,_c9/* Model.Elements.a59_5 */,_nc/* Model.Elements.a59_7 */),
_nf/* iDeliveredBM */ = new T2(0,_nb/* Model.Elements.eDeliveredBM */,_8S/* GHC.Base.Nothing */),
_ng/* ai59_2 */ = new T3(0,_ne/* Model.Elements.a59_2 */,_cf/* Model.Elements.iTransportBM */,_nf/* Model.Elements.iDeliveredBM */),
_nh/* a59_10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a59_3"));
}),
_ni/* eDeliveredDLI1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Delivered DLI"));
}),
_nj/* eDeliveredDLI */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_ni/* Model.Elements.eDeliveredDLI1 */),
_nk/* a59_9 */ = new T2(0,_nj/* Model.Elements.eDeliveredDLI */,_9X/* Model.Elements.a18a2 */),
_nl/* a59_3 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_nh/* Model.Elements.a59_10 */,_c9/* Model.Elements.a59_5 */,_nk/* Model.Elements.a59_9 */),
_nm/* iDeliveredDLI */ = new T2(0,_nj/* Model.Elements.eDeliveredDLI */,_8S/* GHC.Base.Nothing */),
_nn/* ai59_3 */ = new T3(0,_nl/* Model.Elements.a59_3 */,_cj/* Model.Elements.iTransportDLI */,_nm/* Model.Elements.iDeliveredDLI */),
_no/* eCollectedHSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collected HSC"));
}),
_np/* eCollectedHSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_no/* Model.Elements.eCollectedHSC1 */),
_nq/* a141 */ = new T2(0,_np/* Model.Elements.eCollectedHSC */,_91/* Model.Elements.a4 */),
_nr/* a142 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a60"));
}),
_ns/* a60 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_nr/* Model.Elements.a142 */,_nq/* Model.Elements.a141 */,_bw/* Model.Elements.a127 */),
_nt/* iCollectedHSC */ = new T2(0,_np/* Model.Elements.eCollectedHSC */,_8S/* GHC.Base.Nothing */),
_nu/* ai60 */ = new T3(0,_ns/* Model.Elements.a60 */,_nt/* Model.Elements.iCollectedHSC */,_bs/* Model.Elements.iCollection */),
_nv/* eDeliveredHSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Delivered HSC"));
}),
_nw/* eDeliveredHSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_nv/* Model.Elements.eDeliveredHSC1 */),
_nx/* a143 */ = new T2(0,_nw/* Model.Elements.eDeliveredHSC */,_91/* Model.Elements.a4 */),
_ny/* a144 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a61"));
}),
_nz/* a61 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_ny/* Model.Elements.a144 */,_nx/* Model.Elements.a143 */,_c9/* Model.Elements.a59_5 */),
_nA/* iDeliveredHSC */ = new T2(0,_nw/* Model.Elements.eDeliveredHSC */,_8S/* GHC.Base.Nothing */),
_nB/* ai61 */ = new T3(0,_nz/* Model.Elements.a61 */,_nA/* Model.Elements.iDeliveredHSC */,_cf/* Model.Elements.iTransportBM */),
_nC/* eTransplantedHSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplanted HSC"));
}),
_nD/* eTransplantedHSC */ = new T2(0,_ap/* Metamodel.UfoA.Role */,_nC/* Model.Elements.eTransplantedHSC1 */),
_nE/* a145 */ = new T2(0,_nD/* Model.Elements.eTransplantedHSC */,_91/* Model.Elements.a4 */),
_nF/* a146 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a62"));
}),
_nG/* a62 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_nF/* Model.Elements.a146 */,_nE/* Model.Elements.a145 */,_co/* Model.Elements.a42_3c2 */),
_nH/* iTransplantedHSC */ = new T2(0,_nD/* Model.Elements.eTransplantedHSC */,_8S/* GHC.Base.Nothing */),
_nI/* ai62 */ = new T3(0,_nG/* Model.Elements.a62 */,_nH/* Model.Elements.iTransplantedHSC */,_cn/* Model.Elements.iTransplantation */),
_nJ/* eHSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Hematopoietic\nStem Cells"));
}),
_nK/* eHSC */ = new T2(0,_ad/* Metamodel.UfoA.Quantity */,_nJ/* Model.Elements.eHSC1 */),
_nL/* a148 */ = new T2(0,_nK/* Model.Elements.eHSC */,_91/* Model.Elements.a4 */),
_nM/* a149 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a63"));
}),
_nN/* a63 */ = new T5(0,_dm/* Metamodel.UfoA.OUAMediation */,_8S/* GHC.Base.Nothing */,_nM/* Model.Elements.a149 */,_nL/* Model.Elements.a148 */,_bx/* Model.Elements.a147 */),
_nO/* iHSC */ = new T2(0,_nK/* Model.Elements.eHSC */,_8S/* GHC.Base.Nothing */),
_nP/* ai63 */ = new T3(0,_nN/* Model.Elements.a63 */,_nO/* Model.Elements.iHSC */,_bv/* Model.Elements.iDonation */),
_nQ/* OUContainment */ = new T0(2),
_nR/* a150 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a64"));
}),
_nS/* a64 */ = new T5(0,_nQ/* Metamodel.UfoA.OUContainment */,_8S/* GHC.Base.Nothing */,_nR/* Model.Elements.a150 */,_f2/* Model.Elements.a42 */,_nL/* Model.Elements.a148 */),
_nT/* ai64 */ = new T3(0,_nS/* Model.Elements.a64 */,_fa/* Model.Elements.iPerson1 */,_nO/* Model.Elements.iHSC */),
_nU/* a152 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("2..3"));
}),
_nV/* eExpertStatement1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Expert Statement"));
}),
_nW/* eExpertStatement */ = new T2(0,_es/* Metamodel.UfoA.Mode */,_nV/* Model.Elements.eExpertStatement1 */),
_nX/* a151 */ = new T2(0,_nW/* Model.Elements.eExpertStatement */,_nU/* Model.Elements.a152 */),
_nY/* a153 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("a65"));
}),
_nZ/* a65 */ = new T5(0,_eT/* Metamodel.UfoA.OUACharacterization */,_8S/* GHC.Base.Nothing */,_nY/* Model.Elements.a153 */,_lx/* Model.Elements.a110 */,_nX/* Model.Elements.a151 */),
_o0/* iExpertStatement */ = new T2(0,_nW/* Model.Elements.eExpertStatement */,_8S/* GHC.Base.Nothing */),
_o1/* ai65 */ = new T3(0,_nZ/* Model.Elements.a65 */,_lE/* Model.Elements.iDonorSubsequentRel */,_o0/* Model.Elements.iExpertStatement */),
_o2/* ouModelInst142 */ = new T2(1,_o1/* Model.Elements.ai65 */,_4x/* GHC.Types.[] */),
_o3/* ouModelInst141 */ = new T2(1,_nT/* Model.Elements.ai64 */,_o2/* Model.Models.ouModelInst142 */),
_o4/* ouModelInst140 */ = new T2(1,_nP/* Model.Elements.ai63 */,_o3/* Model.Models.ouModelInst141 */),
_o5/* ouModelInst139 */ = new T2(1,_nI/* Model.Elements.ai62 */,_o4/* Model.Models.ouModelInst140 */),
_o6/* ouModelInst138 */ = new T2(1,_nB/* Model.Elements.ai61 */,_o5/* Model.Models.ouModelInst139 */),
_o7/* ouModelInst137 */ = new T2(1,_nu/* Model.Elements.ai60 */,_o6/* Model.Models.ouModelInst138 */),
_o8/* ouModelInst136 */ = new T2(1,_nn/* Model.Elements.ai59_3 */,_o7/* Model.Models.ouModelInst137 */),
_o9/* ouModelInst135 */ = new T2(1,_ng/* Model.Elements.ai59_2 */,_o8/* Model.Models.ouModelInst136 */),
_oa/* ouModelInst134 */ = new T2(1,_n9/* Model.Elements.ai59_1 */,_o9/* Model.Models.ouModelInst135 */),
_ob/* ouModelInst133 */ = new T2(1,_n2/* Model.Elements.ai58_3 */,_oa/* Model.Models.ouModelInst134 */),
_oc/* ouModelInst132 */ = new T2(1,_n1/* Model.Elements.ai58_2 */,_ob/* Model.Models.ouModelInst133 */),
_od/* ouModelInst131 */ = new T2(1,_n0/* Model.Elements.ai58_1 */,_oc/* Model.Models.ouModelInst132 */),
_oe/* ouModelInst130 */ = new T2(1,_mW/* Model.Elements.ai57 */,_od/* Model.Models.ouModelInst131 */),
_of/* ouModelInst129 */ = new T2(1,_mS/* Model.Elements.ai56 */,_oe/* Model.Models.ouModelInst130 */),
_og/* ouModelInst128 */ = new T2(1,_mL/* Model.Elements.ai55 */,_of/* Model.Models.ouModelInst129 */),
_oh/* ouModelInst127 */ = new T2(1,_mA/* Model.Elements.ai54 */,_og/* Model.Models.ouModelInst128 */),
_oi/* ouModelInst126 */ = new T2(1,_mt/* Model.Elements.ai53 */,_oh/* Model.Models.ouModelInst127 */),
_oj/* ouModelInst125 */ = new T2(1,_mp/* Model.Elements.ai52 */,_oi/* Model.Models.ouModelInst126 */),
_ok/* ouModelInst124 */ = new T2(1,_me/* Model.Elements.ai51 */,_oj/* Model.Models.ouModelInst125 */),
_ol/* ouModelInst123 */ = new T2(1,_m7/* Model.Elements.ai50 */,_ok/* Model.Models.ouModelInst124 */),
_om/* ouModelInst122 */ = new T2(1,_m0/* Model.Elements.ai49 */,_ol/* Model.Models.ouModelInst123 */),
_on/* ouModelInst121 */ = new T2(1,_lT/* Model.Elements.ai48 */,_om/* Model.Models.ouModelInst122 */),
_oo/* ouModelInst120 */ = new T2(1,_lM/* Model.Elements.ai47b */,_on/* Model.Models.ouModelInst121 */),
_op/* ouModelInst119 */ = new T2(1,_lJ/* Model.Elements.ai47a */,_oo/* Model.Models.ouModelInst120 */),
_oq/* ouModelInst118 */ = new T2(1,_lF/* Model.Elements.ai46 */,_op/* Model.Models.ouModelInst119 */),
_or/* ouModelInst117 */ = new T2(1,_lu/* Model.Elements.ai45 */,_oq/* Model.Models.ouModelInst118 */),
_os/* ouModelInst116 */ = new T2(1,_ln/* Model.Elements.ai44 */,_or/* Model.Models.ouModelInst117 */),
_ot/* ouModelInst115 */ = new T2(1,_lg/* Model.Elements.ai42_3c */,_os/* Model.Models.ouModelInst116 */),
_ou/* ouModelInst114 */ = new T2(1,_l9/* Model.Elements.ai42_3a */,_ot/* Model.Models.ouModelInst115 */),
_ov/* ouModelInst113 */ = new T2(1,_l2/* Model.Elements.ai42_2c */,_ou/* Model.Models.ouModelInst114 */),
_ow/* ouModelInst112 */ = new T2(1,_kV/* Model.Elements.ai42_2a */,_ov/* Model.Models.ouModelInst113 */),
_ox/* ouModelInst111 */ = new T2(1,_kO/* Model.Elements.ai42_1b */,_ow/* Model.Models.ouModelInst112 */),
_oy/* ouModelInst110 */ = new T2(1,_kH/* Model.Elements.ai42_1a */,_ox/* Model.Models.ouModelInst111 */),
_oz/* ouModelInst109 */ = new T2(1,_kA/* Model.Elements.ai40 */,_oy/* Model.Models.ouModelInst110 */),
_oA/* ouModelInst108 */ = new T2(1,_kw/* Model.Elements.ai39 */,_oz/* Model.Models.ouModelInst109 */),
_oB/* ouModelInst107 */ = new T2(1,_ks/* Model.Elements.ai37b */,_oA/* Model.Models.ouModelInst108 */),
_oC/* ouModelInst106 */ = new T2(1,_ko/* Model.Elements.ai37a */,_oB/* Model.Models.ouModelInst107 */),
_oD/* ouModelInst105 */ = new T2(1,_kh/* Model.Elements.ai36c */,_oC/* Model.Models.ouModelInst106 */),
_oE/* ouModelInst104 */ = new T2(1,_ka/* Model.Elements.ai36b */,_oD/* Model.Models.ouModelInst105 */),
_oF/* ouModelInst103 */ = new T2(1,_k3/* Model.Elements.ai36ba */,_oE/* Model.Models.ouModelInst104 */),
_oG/* ouModelInst102 */ = new T2(1,_jW/* Model.Elements.ai36aa */,_oF/* Model.Models.ouModelInst103 */),
_oH/* ouModelInst101 */ = new T2(1,_jP/* Model.Elements.ai36a */,_oG/* Model.Models.ouModelInst102 */),
_oI/* ouModelInst100 */ = new T2(1,_jI/* Model.Elements.ai35_1 */,_oH/* Model.Models.ouModelInst101 */),
_oJ/* ouModelInst99 */ = new T2(1,_jE/* Model.Elements.ai34_2d */,_oI/* Model.Models.ouModelInst100 */),
_oK/* ouModelInst98 */ = new T2(1,_jx/* Model.Elements.ai34_2c */,_oJ/* Model.Models.ouModelInst99 */),
_oL/* ouModelInst97 */ = new T2(1,_jt/* Model.Elements.ai34_2b */,_oK/* Model.Models.ouModelInst98 */),
_oM/* ouModelInst96 */ = new T2(1,_ji/* Model.Elements.ai34_2ab */,_oL/* Model.Models.ouModelInst97 */),
_oN/* ouModelInst95 */ = new T2(1,_jb/* Model.Elements.ai34_2aa */,_oM/* Model.Models.ouModelInst96 */),
_oO/* ouModelInst94 */ = new T2(1,_j8/* Model.Elements.ai34_1b */,_oN/* Model.Models.ouModelInst95 */),
_oP/* ouModelInst93 */ = new T2(1,_j4/* Model.Elements.ai34_1a */,_oO/* Model.Models.ouModelInst94 */),
_oQ/* ouModelInst92 */ = new T2(1,_iT/* Model.Elements.ai34b */,_oP/* Model.Models.ouModelInst93 */),
_oR/* ouModelInst91 */ = new T2(1,_iM/* Model.Elements.ai34a */,_oQ/* Model.Models.ouModelInst92 */),
_oS/* ouModelInst90 */ = new T2(1,_iB/* Model.Elements.ai33_3b */,_oR/* Model.Models.ouModelInst91 */),
_oT/* ouModelInst89 */ = new T2(1,_ix/* Model.Elements.ai33_3a */,_oS/* Model.Models.ouModelInst90 */),
_oU/* ouModelInst88 */ = new T2(1,_im/* Model.Elements.ai33_2 */,_oT/* Model.Models.ouModelInst89 */),
_oV/* ouModelInst87 */ = new T2(1,_ij/* Model.Elements.ai33_1 */,_oU/* Model.Models.ouModelInst88 */),
_oW/* ouModelInst86 */ = new T2(1,_ig/* Model.Elements.ai32_3 */,_oV/* Model.Models.ouModelInst87 */),
_oX/* ouModelInst85 */ = new T2(1,_i9/* Model.Elements.ai32_2 */,_oW/* Model.Models.ouModelInst86 */),
_oY/* ouModelInst84 */ = new T2(1,_i2/* Model.Elements.ai32_1 */,_oX/* Model.Models.ouModelInst85 */),
_oZ/* ouModelInst83 */ = new T2(1,_hV/* Model.Elements.ai32 */,_oY/* Model.Models.ouModelInst84 */),
_p0/* ouModelInst82 */ = new T2(1,_hK/* Model.Elements.ai31_2 */,_oZ/* Model.Models.ouModelInst83 */),
_p1/* ouModelInst81 */ = new T2(1,_hD/* Model.Elements.ai31_1 */,_p0/* Model.Models.ouModelInst82 */),
_p2/* ouModelInst80 */ = new T2(1,_hA/* Model.Elements.ai30_2 */,_p1/* Model.Models.ouModelInst81 */),
_p3/* ouModelInst79 */ = new T2(1,_ht/* Model.Elements.ai30_1 */,_p2/* Model.Models.ouModelInst80 */),
_p4/* ouModelInst78 */ = new T2(1,_hm/* Model.Elements.ai29 */,_p3/* Model.Models.ouModelInst79 */),
_p5/* ouModelInst77 */ = new T2(1,_hf/* Model.Elements.ai28 */,_p4/* Model.Models.ouModelInst78 */),
_p6/* ouModelInst76 */ = new T2(1,_ha/* Model.Elements.ai27_3 */,_p5/* Model.Models.ouModelInst77 */),
_p7/* ouModelInst75 */ = new T2(1,_h9/* Model.Elements.ai27_2 */,_p6/* Model.Models.ouModelInst76 */),
_p8/* ouModelInst74 */ = new T2(1,_h8/* Model.Elements.ai27_1 */,_p7/* Model.Models.ouModelInst75 */),
_p9/* ouModelInst73 */ = new T2(1,_h4/* Model.Elements.ai26 */,_p8/* Model.Models.ouModelInst74 */),
_pa/* ouModelInst72 */ = new T2(1,_h1/* Model.Elements.ai25 */,_p9/* Model.Models.ouModelInst73 */),
_pb/* ouModelInst71 */ = new T2(1,_gU/* Model.Elements.ai24 */,_pa/* Model.Models.ouModelInst72 */),
_pc/* ouModelInst70 */ = new T2(1,_gN/* Model.Elements.ai23_3 */,_pb/* Model.Models.ouModelInst71 */),
_pd/* ouModelInst69 */ = new T2(1,_gJ/* Model.Elements.ai23_2 */,_pc/* Model.Models.ouModelInst70 */),
_pe/* ouModelInst68 */ = new T2(1,_gI/* Model.Elements.ai23_1 */,_pd/* Model.Models.ouModelInst69 */),
_pf/* ouModelInst67 */ = new T2(1,_gF/* Model.Elements.ai22_3 */,_pe/* Model.Models.ouModelInst68 */),
_pg/* ouModelInst66 */ = new T2(1,_gB/* Model.Elements.ai22_2 */,_pf/* Model.Models.ouModelInst67 */),
_ph/* ouModelInst65 */ = new T2(1,_gx/* Model.Elements.ai22_1 */,_pg/* Model.Models.ouModelInst66 */),
_pi/* ouModelInst64 */ = new T2(1,_go/* Model.Elements.ai21 */,_ph/* Model.Models.ouModelInst65 */),
_pj/* ouModelInst63 */ = new T2(1,_gh/* Model.Elements.ai19 */,_pi/* Model.Models.ouModelInst64 */),
_pk/* ouModelInst62 */ = new T2(1,_gd/* Model.Elements.ai18b */,_pj/* Model.Models.ouModelInst63 */),
_pl/* ouModelInst61 */ = new T2(1,_g8/* Model.Elements.ai18a */,_pk/* Model.Models.ouModelInst62 */),
_pm/* ouModelInst60 */ = new T2(1,_g1/* Model.Elements.ai17b */,_pl/* Model.Models.ouModelInst61 */),
_pn/* ouModelInst59 */ = new T2(1,_fT/* Model.Elements.ai17a */,_pm/* Model.Models.ouModelInst60 */),
_po/* ouModelInst58 */ = new T2(1,_fI/* Model.Elements.ai16a_3 */,_pn/* Model.Models.ouModelInst59 */),
_pp/* ouModelInst57 */ = new T2(1,_fA/* Model.Elements.ai16a_2a */,_po/* Model.Models.ouModelInst58 */),
_pq/* ouModelInst56 */ = new T2(1,_fs/* Model.Elements.ai15_1_1 */,_pp/* Model.Models.ouModelInst57 */),
_pr/* ouModelInst55 */ = new T2(1,_fi/* Model.Elements.ai14_2 */,_pq/* Model.Models.ouModelInst56 */),
_ps/* ouModelInst54 */ = new T2(1,_fb/* Model.Elements.ai14_1 */,_pr/* Model.Models.ouModelInst55 */),
_pt/* ouModelInst53 */ = new T2(1,_eW/* Model.Elements.ai13 */,_ps/* Model.Models.ouModelInst54 */),
_pu/* ouModelInst52 */ = new T2(1,_eS/* Model.Elements.ai12_3 */,_pt/* Model.Models.ouModelInst53 */),
_pv/* ouModelInst51 */ = new T2(1,_eR/* Model.Elements.ai12_2 */,_pu/* Model.Models.ouModelInst52 */),
_pw/* ouModelInst50 */ = new T2(1,_eQ/* Model.Elements.ai12_1 */,_pv/* Model.Models.ouModelInst51 */),
_px/* ouModelInst49 */ = new T2(1,_eG/* Model.Elements.ai11_3 */,_pw/* Model.Models.ouModelInst50 */),
_py/* ouModelInst48 */ = new T2(1,_eF/* Model.Elements.ai11_2 */,_px/* Model.Models.ouModelInst49 */),
_pz/* ouModelInst47 */ = new T2(1,_eB/* Model.Elements.ai11_1 */,_py/* Model.Models.ouModelInst48 */),
_pA/* ouModelInst46 */ = new T2(1,_er/* Model.Elements.ai10_3 */,_pz/* Model.Models.ouModelInst47 */),
_pB/* ouModelInst45 */ = new T2(1,_en/* Model.Elements.ai10_2 */,_pA/* Model.Models.ouModelInst46 */),
_pC/* ouModelInst44 */ = new T2(1,_ej/* Model.Elements.ai10_1 */,_pB/* Model.Models.ouModelInst45 */),
_pD/* ouModelInst43 */ = new T2(1,_ef/* Model.Elements.ai9_3 */,_pC/* Model.Models.ouModelInst44 */),
_pE/* ouModelInst42 */ = new T2(1,_ee/* Model.Elements.ai9_2 */,_pD/* Model.Models.ouModelInst43 */),
_pF/* ouModelInst41 */ = new T2(1,_ed/* Model.Elements.ai9_1 */,_pE/* Model.Models.ouModelInst42 */),
_pG/* ouModelInst40 */ = new T2(1,_e8/* Model.Elements.ai8_3 */,_pF/* Model.Models.ouModelInst41 */),
_pH/* ouModelInst39 */ = new T2(1,_e7/* Model.Elements.ai8_2 */,_pG/* Model.Models.ouModelInst40 */),
_pI/* ouModelInst38 */ = new T2(1,_e6/* Model.Elements.ai8_1 */,_pH/* Model.Models.ouModelInst39 */),
_pJ/* ouModelInst37 */ = new T2(1,_e2/* Model.Elements.ai7_3 */,_pI/* Model.Models.ouModelInst38 */),
_pK/* ouModelInst36 */ = new T2(1,_dY/* Model.Elements.ai7_2 */,_pJ/* Model.Models.ouModelInst37 */),
_pL/* ouModelInst35 */ = new T2(1,_dU/* Model.Elements.ai7_1 */,_pK/* Model.Models.ouModelInst36 */),
_pM/* ouModelInst34 */ = new T2(1,_dL/* Model.Elements.ai6_4 */,_pL/* Model.Models.ouModelInst35 */),
_pN/* ouModelInst33 */ = new T2(1,_dK/* Model.Elements.ai6_3 */,_pM/* Model.Models.ouModelInst34 */),
_pO/* ouModelInst32 */ = new T2(1,_dJ/* Model.Elements.ai6_2 */,_pN/* Model.Models.ouModelInst33 */),
_pP/* ouModelInst31 */ = new T2(1,_dI/* Model.Elements.ai6_1 */,_pO/* Model.Models.ouModelInst32 */),
_pQ/* ouModelInst30 */ = new T2(1,_dF/* Model.Elements.ai3 */,_pP/* Model.Models.ouModelInst31 */),
_pR/* ouModelInst29 */ = new T2(1,_dz/* Model.Elements.ai1 */,_pQ/* Model.Models.ouModelInst30 */),
_pS/* DisjointComplete */ = 3,
_pT/* eGenotypeUnknown1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Unknown Genotype"));
}),
_pU/* eGenotypeUnknown */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_pT/* Model.Elements.eGenotypeUnknown1 */),
_pV/* g114 */ = new T2(1,_fl/* Model.Elements.eGenotypeEvaluated */,_4x/* GHC.Types.[] */),
_pW/* g113 */ = new T2(1,_pU/* Model.Elements.eGenotypeUnknown */,_pV/* Model.Elements.g114 */),
_pX/* g115 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g5"));
}),
_pY/* g5 */ = new T4(0,_pX/* Model.Elements.g115 */,_eY/* Model.Elements.eGenotype */,_pW/* Model.Elements.g113 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_pZ/* iGenotypeUnknown5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("gup1"));
}),
_q0/* iGenotypeUnknown4 */ = new T1(1,_pZ/* Model.Elements.iGenotypeUnknown5 */),
_q1/* iGenotypeUnknown1 */ = new T2(0,_pU/* Model.Elements.eGenotypeUnknown */,_q0/* Model.Elements.iGenotypeUnknown4 */),
_q2/* gi5_1_1 */ = new T3(0,_pY/* Model.Elements.g5 */,_f7/* Model.Elements.iGenotype1 */,_q1/* Model.Elements.iGenotypeUnknown1 */),
_q3/* gi5_1_2 */ = new T3(0,_pY/* Model.Elements.g5 */,_f7/* Model.Elements.iGenotype1 */,_fr/* Model.Elements.iGenotypeEvaluated1 */),
_q4/* sPermDeferred70 */ = new T2(1,_q3/* Model.Elements.gi5_1_2 */,_4x/* GHC.Types.[] */),
_q5/* sPermDeferred69 */ = new T2(1,_q2/* Model.Elements.gi5_1_1 */,_q4/* Model.Models.sPermDeferred70 */),
_q6/* iGenotypeUnknown7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("gup2"));
}),
_q7/* iGenotypeUnknown6 */ = new T1(1,_q6/* Model.Elements.iGenotypeUnknown7 */),
_q8/* iGenotypeUnknown2 */ = new T2(0,_pU/* Model.Elements.eGenotypeUnknown */,_q7/* Model.Elements.iGenotypeUnknown6 */),
_q9/* gi5_2_1 */ = new T3(0,_pY/* Model.Elements.g5 */,_fe/* Model.Elements.iGenotype2 */,_q8/* Model.Elements.iGenotypeUnknown2 */),
_qa/* iGenotypeEvaluated7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ge2"));
}),
_qb/* iGenotypeEvaluated6 */ = new T1(1,_qa/* Model.Elements.iGenotypeEvaluated7 */),
_qc/* iGenotypeEvaluated2 */ = new T2(0,_fl/* Model.Elements.eGenotypeEvaluated */,_qb/* Model.Elements.iGenotypeEvaluated6 */),
_qd/* gi5_2_2 */ = new T3(0,_pY/* Model.Elements.g5 */,_fe/* Model.Elements.iGenotype2 */,_qc/* Model.Elements.iGenotypeEvaluated2 */),
_qe/* sPermDeferred68 */ = new T2(1,_qd/* Model.Elements.gi5_2_2 */,_4x/* GHC.Types.[] */),
_qf/* sPermDeferred67 */ = new T2(1,_q9/* Model.Elements.gi5_2_1 */,_qe/* Model.Models.sPermDeferred68 */),
_qg/* g111 */ = new T2(1,_aO/* Model.Elements.eWorkup */,_4x/* GHC.Types.[] */),
_qh/* g110 */ = new T2(1,_a7/* Model.Elements.eVerificationExamination */,_qg/* Model.Elements.g111 */),
_qi/* g109 */ = new T2(1,_9U/* Model.Elements.eExtendedExamination */,_qh/* Model.Elements.g110 */),
_qj/* g108 */ = new T2(1,_9u/* Model.Elements.eInitialExamination */,_qi/* Model.Elements.g109 */),
_qk/* g112 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g4"));
}),
_ql/* g4 */ = new T4(0,_qk/* Model.Elements.g112 */,_gj/* Model.Elements.eExamination */,_qj/* Model.Elements.g108 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_qm/* gi4_1 */ = new T3(0,_ql/* Model.Elements.g4 */,_gn/* Model.Elements.iExamination */,_9v/* Model.Elements.iInitialExamination */),
_qn/* gi4_2 */ = new T3(0,_ql/* Model.Elements.g4 */,_gn/* Model.Elements.iExamination */,_9V/* Model.Elements.iExtendedExamination */),
_qo/* gi4_3 */ = new T3(0,_ql/* Model.Elements.g4 */,_gn/* Model.Elements.iExamination */,_a8/* Model.Elements.iVerificationExamination */),
_qp/* gi4_4 */ = new T3(0,_ql/* Model.Elements.g4 */,_gn/* Model.Elements.iExamination */,_aP/* Model.Elements.iWorkup */),
_qq/* sPermDeferred66 */ = new T2(1,_qp/* Model.Elements.gi4_4 */,_4x/* GHC.Types.[] */),
_qr/* sPermDeferred65 */ = new T2(1,_qo/* Model.Elements.gi4_3 */,_qq/* Model.Models.sPermDeferred66 */),
_qs/* sPermDeferred64 */ = new T2(1,_qn/* Model.Elements.gi4_2 */,_qr/* Model.Models.sPermDeferred65 */),
_qt/* sPermDeferred63 */ = new T2(1,_qm/* Model.Elements.gi4_1 */,_qs/* Model.Models.sPermDeferred64 */),
_qu/* PlainGT */ = 0,
_qv/* g97 */ = new T2(1,_np/* Model.Elements.eCollectedHSC */,_4x/* GHC.Types.[] */),
_qw/* g98 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g32"));
}),
_qx/* g32 */ = new T4(0,_qw/* Model.Elements.g98 */,_nK/* Model.Elements.eHSC */,_qv/* Model.Elements.g97 */,_qu/* Metamodel.UfoA.PlainGT */),
_qy/* gi32 */ = new T3(0,_qx/* Model.Elements.g32 */,_nO/* Model.Elements.iHSC */,_nt/* Model.Elements.iCollectedHSC */),
_qz/* g100 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g33"));
}),
_qA/* g99 */ = new T2(1,_nw/* Model.Elements.eDeliveredHSC */,_4x/* GHC.Types.[] */),
_qB/* g33 */ = new T4(0,_qz/* Model.Elements.g100 */,_np/* Model.Elements.eCollectedHSC */,_qA/* Model.Elements.g99 */,_qu/* Metamodel.UfoA.PlainGT */),
_qC/* gi33 */ = new T3(0,_qB/* Model.Elements.g33 */,_nt/* Model.Elements.iCollectedHSC */,_nA/* Model.Elements.iDeliveredHSC */),
_qD/* g101 */ = new T2(1,_nD/* Model.Elements.eTransplantedHSC */,_4x/* GHC.Types.[] */),
_qE/* g102 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g34"));
}),
_qF/* g34 */ = new T4(0,_qE/* Model.Elements.g102 */,_nw/* Model.Elements.eDeliveredHSC */,_qD/* Model.Elements.g101 */,_qu/* Metamodel.UfoA.PlainGT */),
_qG/* gi34 */ = new T3(0,_qF/* Model.Elements.g34 */,_nA/* Model.Elements.iDeliveredHSC */,_nH/* Model.Elements.iTransplantedHSC */),
_qH/* g103 */ = new T2(1,_hv/* Model.Elements.eInfectionMarkersW */,_4x/* GHC.Types.[] */),
_qI/* g104 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g35"));
}),
_qJ/* g35 */ = new T4(0,_qI/* Model.Elements.g104 */,_ho/* Model.Elements.eInfectionMarkersV */,_qH/* Model.Elements.g103 */,_qu/* Metamodel.UfoA.PlainGT */),
_qK/* gi35 */ = new T3(0,_qJ/* Model.Elements.g35 */,_hs/* Model.Elements.iInfectionMarkersV */,_hz/* Model.Elements.iInfectionMarkersW */),
_qL/* eDonorAvailableForWorkup1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Available\nfor Workup"));
}),
_qM/* eDonorAvailableForWorkup */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_qL/* Model.Elements.eDonorAvailableForWorkup1 */),
_qN/* eDonorUnavailableForWorkup1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Unavailable\nfor Workup"));
}),
_qO/* eDonorUnavailableForWorkup */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_qN/* Model.Elements.eDonorUnavailableForWorkup1 */),
_qP/* g106 */ = new T2(1,_qO/* Model.Elements.eDonorUnavailableForWorkup */,_4x/* GHC.Types.[] */),
_qQ/* g105 */ = new T2(1,_qM/* Model.Elements.eDonorAvailableForWorkup */,_qP/* Model.Elements.g106 */),
_qR/* g107 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g36"));
}),
_qS/* g36 */ = new T4(0,_qR/* Model.Elements.g107 */,_ir/* Model.Elements.eDonorChosen */,_qQ/* Model.Elements.g105 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_qT/* iDonorAvailableForWorkup */ = new T2(0,_qM/* Model.Elements.eDonorAvailableForWorkup */,_8S/* GHC.Base.Nothing */),
_qU/* gi36_1 */ = new T3(0,_qS/* Model.Elements.g36 */,_iv/* Model.Elements.iDonorChosen */,_qT/* Model.Elements.iDonorAvailableForWorkup */),
_qV/* iDonorUnavailableForWorkup */ = new T2(0,_qO/* Model.Elements.eDonorUnavailableForWorkup */,_8S/* GHC.Base.Nothing */),
_qW/* gi36_2 */ = new T3(0,_qS/* Model.Elements.g36 */,_iv/* Model.Elements.iDonorChosen */,_qV/* Model.Elements.iDonorUnavailableForWorkup */),
_qX/* sPermDeferred26 */ = new T2(1,_qW/* Model.Elements.gi36_2 */,_4x/* GHC.Types.[] */),
_qY/* sPermDeferred25 */ = new T2(1,_qU/* Model.Elements.gi36_1 */,_qX/* Model.Models.sPermDeferred26 */),
_qZ/* sPermDeferred24 */ = new T2(1,_qK/* Model.Elements.gi35 */,_qY/* Model.Models.sPermDeferred25 */),
_r0/* sPermDeferred23 */ = new T2(1,_qG/* Model.Elements.gi34 */,_qZ/* Model.Models.sPermDeferred24 */),
_r1/* sPermDeferred22 */ = new T2(1,_qC/* Model.Elements.gi33 */,_r0/* Model.Models.sPermDeferred23 */),
_r2/* sPermDeferred21 */ = new T2(1,_qy/* Model.Elements.gi32 */,_r1/* Model.Models.sPermDeferred22 */),
_r3/* sPermDeferred20 */ = new T2(1,_r2/* Model.Models.sPermDeferred21 */,_4x/* GHC.Types.[] */),
_r4/* Disjoint */ = 1,
_r5/* ePBSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC"));
}),
_r6/* ePBSC */ = new T2(0,_ad/* Metamodel.UfoA.Quantity */,_r5/* Model.Elements.ePBSC1 */),
_r7/* eBM1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM"));
}),
_r8/* eBM */ = new T2(0,_ad/* Metamodel.UfoA.Quantity */,_r7/* Model.Elements.eBM1 */),
_r9/* eDLI1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DLI"));
}),
_ra/* eDLI */ = new T2(0,_ad/* Metamodel.UfoA.Quantity */,_r9/* Model.Elements.eDLI1 */),
_rb/* g95 */ = new T2(1,_ra/* Model.Elements.eDLI */,_4x/* GHC.Types.[] */),
_rc/* g94 */ = new T2(1,_r8/* Model.Elements.eBM */,_rb/* Model.Elements.g95 */),
_rd/* g93 */ = new T2(1,_r6/* Model.Elements.ePBSC */,_rc/* Model.Elements.g94 */),
_re/* g96 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g31"));
}),
_rf/* g31 */ = new T4(0,_re/* Model.Elements.g96 */,_nK/* Model.Elements.eHSC */,_rd/* Model.Elements.g93 */,_r4/* Metamodel.UfoA.Disjoint */),
_rg/* iPBSC */ = new T2(0,_r6/* Model.Elements.ePBSC */,_8S/* GHC.Base.Nothing */),
_rh/* gi31_1 */ = new T3(0,_rf/* Model.Elements.g31 */,_nO/* Model.Elements.iHSC */,_rg/* Model.Elements.iPBSC */),
_ri/* iBM */ = new T2(0,_r8/* Model.Elements.eBM */,_8S/* GHC.Base.Nothing */),
_rj/* gi31_2 */ = new T3(0,_rf/* Model.Elements.g31 */,_nO/* Model.Elements.iHSC */,_ri/* Model.Elements.iBM */),
_rk/* iDLI */ = new T2(0,_ra/* Model.Elements.eDLI */,_8S/* GHC.Base.Nothing */),
_rl/* gi31_3 */ = new T3(0,_rf/* Model.Elements.g31 */,_nO/* Model.Elements.iHSC */,_rk/* Model.Elements.iDLI */),
_rm/* sPermDeferred29 */ = new T2(1,_rl/* Model.Elements.gi31_3 */,_4x/* GHC.Types.[] */),
_rn/* sPermDeferred28 */ = new T2(1,_rj/* Model.Elements.gi31_2 */,_rm/* Model.Models.sPermDeferred29 */),
_ro/* sPermDeferred27 */ = new T2(1,_rh/* Model.Elements.gi31_1 */,_rn/* Model.Models.sPermDeferred28 */),
_rp/* sPermDeferred19 */ = new T2(1,_ro/* Model.Models.sPermDeferred27 */,_r3/* Model.Models.sPermDeferred20 */),
_rq/* g28a1 */ = new T2(1,_jK/* Model.Elements.eCollectedPBSC */,_4x/* GHC.Types.[] */),
_rr/* g28a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g28a"));
}),
_rs/* g28a */ = new T4(0,_rr/* Model.Elements.g28a2 */,_r6/* Model.Elements.ePBSC */,_rq/* Model.Elements.g28a1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rt/* gi28a */ = new T3(0,_rs/* Model.Elements.g28a */,_rg/* Model.Elements.iPBSC */,_jO/* Model.Elements.iCollectedPBSC */),
_ru/* g14b_1 */ = new T2(1,_k5/* Model.Elements.eCollectedBM */,_4x/* GHC.Types.[] */),
_rv/* g28b1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g28b"));
}),
_rw/* g28b */ = new T4(0,_rv/* Model.Elements.g28b1 */,_r8/* Model.Elements.eBM */,_ru/* Model.Elements.g14b_1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rx/* gi28b */ = new T3(0,_rw/* Model.Elements.g28b */,_ri/* Model.Elements.iBM */,_k9/* Model.Elements.iCollectedBM */),
_ry/* g14b_6 */ = new T2(1,_kc/* Model.Elements.eCollectedDLI */,_4x/* GHC.Types.[] */),
_rz/* g28c1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g28c"));
}),
_rA/* g28c */ = new T4(0,_rz/* Model.Elements.g28c1 */,_ra/* Model.Elements.eDLI */,_ry/* Model.Elements.g14b_6 */,_qu/* Metamodel.UfoA.PlainGT */),
_rB/* gi28c */ = new T3(0,_rA/* Model.Elements.g28c */,_rk/* Model.Elements.iDLI */,_kg/* Model.Elements.iCollectedDLI */),
_rC/* g29a1 */ = new T2(1,_n4/* Model.Elements.eDeliveredPBSC */,_4x/* GHC.Types.[] */),
_rD/* g29a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g29a"));
}),
_rE/* g29a */ = new T4(0,_rD/* Model.Elements.g29a2 */,_jK/* Model.Elements.eCollectedPBSC */,_rC/* Model.Elements.g29a1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rF/* gi29a */ = new T3(0,_rE/* Model.Elements.g29a */,_jO/* Model.Elements.iCollectedPBSC */,_n8/* Model.Elements.iDeliveredPBSC */),
_rG/* g29b1 */ = new T2(1,_nb/* Model.Elements.eDeliveredBM */,_4x/* GHC.Types.[] */),
_rH/* g29b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g29b"));
}),
_rI/* g29b */ = new T4(0,_rH/* Model.Elements.g29b2 */,_k5/* Model.Elements.eCollectedBM */,_rG/* Model.Elements.g29b1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rJ/* gi29b */ = new T3(0,_rI/* Model.Elements.g29b */,_k9/* Model.Elements.iCollectedBM */,_nf/* Model.Elements.iDeliveredBM */),
_rK/* g29c1 */ = new T2(1,_nj/* Model.Elements.eDeliveredDLI */,_4x/* GHC.Types.[] */),
_rL/* g29c2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g29c"));
}),
_rM/* g29c */ = new T4(0,_rL/* Model.Elements.g29c2 */,_kc/* Model.Elements.eCollectedDLI */,_rK/* Model.Elements.g29c1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rN/* gi29c */ = new T3(0,_rM/* Model.Elements.g29c */,_kg/* Model.Elements.iCollectedDLI */,_nm/* Model.Elements.iDeliveredDLI */),
_rO/* g30a1 */ = new T2(1,_kC/* Model.Elements.eTransplantedPBSC */,_4x/* GHC.Types.[] */),
_rP/* g30a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g30a"));
}),
_rQ/* g30a */ = new T4(0,_rP/* Model.Elements.g30a2 */,_n4/* Model.Elements.eDeliveredPBSC */,_rO/* Model.Elements.g30a1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rR/* gi30a */ = new T3(0,_rQ/* Model.Elements.g30a */,_n8/* Model.Elements.iDeliveredPBSC */,_kG/* Model.Elements.iTransplantedPBSC */),
_rS/* g15b_1 */ = new T2(1,_kQ/* Model.Elements.eTransplantedBM */,_4x/* GHC.Types.[] */),
_rT/* g30b1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g30b"));
}),
_rU/* g30b */ = new T4(0,_rT/* Model.Elements.g30b1 */,_nb/* Model.Elements.eDeliveredBM */,_rS/* Model.Elements.g15b_1 */,_qu/* Metamodel.UfoA.PlainGT */),
_rV/* gi30b */ = new T3(0,_rU/* Model.Elements.g30b */,_nf/* Model.Elements.iDeliveredBM */,_kU/* Model.Elements.iTransplantedBM */),
_rW/* g15b_6 */ = new T2(1,_l4/* Model.Elements.eTransplantedDLI */,_4x/* GHC.Types.[] */),
_rX/* g30c1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g30c"));
}),
_rY/* g30c */ = new T4(0,_rX/* Model.Elements.g30c1 */,_nj/* Model.Elements.eDeliveredDLI */,_rW/* Model.Elements.g15b_6 */,_qu/* Metamodel.UfoA.PlainGT */),
_rZ/* gi30c */ = new T3(0,_rY/* Model.Elements.g30c */,_nm/* Model.Elements.iDeliveredDLI */,_l8/* Model.Elements.iTransplantedDLI */),
_s0/* sPermDeferred38 */ = new T2(1,_rZ/* Model.Elements.gi30c */,_4x/* GHC.Types.[] */),
_s1/* sPermDeferred37 */ = new T2(1,_rV/* Model.Elements.gi30b */,_s0/* Model.Models.sPermDeferred38 */),
_s2/* sPermDeferred36 */ = new T2(1,_rR/* Model.Elements.gi30a */,_s1/* Model.Models.sPermDeferred37 */),
_s3/* sPermDeferred35 */ = new T2(1,_rN/* Model.Elements.gi29c */,_s2/* Model.Models.sPermDeferred36 */),
_s4/* sPermDeferred34 */ = new T2(1,_rJ/* Model.Elements.gi29b */,_s3/* Model.Models.sPermDeferred35 */),
_s5/* sPermDeferred33 */ = new T2(1,_rF/* Model.Elements.gi29a */,_s4/* Model.Models.sPermDeferred34 */),
_s6/* sPermDeferred32 */ = new T2(1,_rB/* Model.Elements.gi28c */,_s5/* Model.Models.sPermDeferred33 */),
_s7/* sPermDeferred31 */ = new T2(1,_rx/* Model.Elements.gi28b */,_s6/* Model.Models.sPermDeferred32 */),
_s8/* sPermDeferred30 */ = new T2(1,_rt/* Model.Elements.gi28a */,_s7/* Model.Models.sPermDeferred31 */),
_s9/* sPermDeferred18 */ = new T2(1,_s8/* Model.Models.sPermDeferred30 */,_rp/* Model.Models.sPermDeferred19 */),
_sa/* eDSFresh1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Fresh DS"));
}),
_sb/* eDSFresh */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_sa/* Model.Elements.eDSFresh1 */),
_sc/* eDSStored1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Stored DS"));
}),
_sd/* eDSStored */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_sc/* Model.Elements.eDSStored1 */),
_se/* eDSDisposed1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Disposed DS"));
}),
_sf/* eDSDisposed */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_se/* Model.Elements.eDSDisposed1 */),
_sg/* g77 */ = new T2(1,_sf/* Model.Elements.eDSDisposed */,_4x/* GHC.Types.[] */),
_sh/* g76 */ = new T2(1,_sd/* Model.Elements.eDSStored */,_sg/* Model.Elements.g77 */),
_si/* g75 */ = new T2(1,_sb/* Model.Elements.eDSFresh */,_sh/* Model.Elements.g76 */),
_sj/* g78 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g23"));
}),
_sk/* g23 */ = new T4(0,_sj/* Model.Elements.g78 */,_ai/* Model.Elements.eDNASample */,_si/* Model.Elements.g75 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_sl/* iDSFresh */ = new T2(0,_sb/* Model.Elements.eDSFresh */,_8S/* GHC.Base.Nothing */),
_sm/* gi23_1_1 */ = new T3(0,_sk/* Model.Elements.g23 */,_aj/* Model.Elements.iDNASample */,_sl/* Model.Elements.iDSFresh */),
_sn/* iDSStored */ = new T2(0,_sd/* Model.Elements.eDSStored */,_8S/* GHC.Base.Nothing */),
_so/* gi23_2_1 */ = new T3(0,_sk/* Model.Elements.g23 */,_aj/* Model.Elements.iDNASample */,_sn/* Model.Elements.iDSStored */),
_sp/* iDSDisposed */ = new T2(0,_sf/* Model.Elements.eDSDisposed */,_8S/* GHC.Base.Nothing */),
_sq/* gi23_3_1 */ = new T3(0,_sk/* Model.Elements.g23 */,_aj/* Model.Elements.iDNASample */,_sp/* Model.Elements.iDSDisposed */),
_sr/* sPermDeferred41 */ = new T2(1,_sq/* Model.Elements.gi23_3_1 */,_4x/* GHC.Types.[] */),
_ss/* sPermDeferred40 */ = new T2(1,_so/* Model.Elements.gi23_2_1 */,_sr/* Model.Models.sPermDeferred41 */),
_st/* sPermDeferred39 */ = new T2(1,_sm/* Model.Elements.gi23_1_1 */,_ss/* Model.Models.sPermDeferred40 */),
_su/* sPermDeferred17 */ = new T2(1,_st/* Model.Models.sPermDeferred39 */,_s9/* Model.Models.sPermDeferred18 */),
_sv/* eBSFresh1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Fresh BS"));
}),
_sw/* eBSFresh */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_sv/* Model.Elements.eBSFresh1 */),
_sx/* eBSStored1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Stored BS"));
}),
_sy/* eBSStored */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_sx/* Model.Elements.eBSStored1 */),
_sz/* eBSDisposed1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Disposed BS"));
}),
_sA/* eBSDisposed */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_sz/* Model.Elements.eBSDisposed1 */),
_sB/* g73 */ = new T2(1,_sA/* Model.Elements.eBSDisposed */,_4x/* GHC.Types.[] */),
_sC/* g72 */ = new T2(1,_sy/* Model.Elements.eBSStored */,_sB/* Model.Elements.g73 */),
_sD/* g71 */ = new T2(1,_sw/* Model.Elements.eBSFresh */,_sC/* Model.Elements.g72 */),
_sE/* g74 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g22"));
}),
_sF/* g22 */ = new T4(0,_sE/* Model.Elements.g74 */,_af/* Model.Elements.eBloodSample */,_sD/* Model.Elements.g71 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_sG/* iBSFresh */ = new T2(0,_sw/* Model.Elements.eBSFresh */,_8S/* GHC.Base.Nothing */),
_sH/* gi22_1_1 */ = new T3(0,_sF/* Model.Elements.g22 */,_ag/* Model.Elements.iBloodSample */,_sG/* Model.Elements.iBSFresh */),
_sI/* iBSStored */ = new T2(0,_sy/* Model.Elements.eBSStored */,_8S/* GHC.Base.Nothing */),
_sJ/* gi22_2_1 */ = new T3(0,_sF/* Model.Elements.g22 */,_ag/* Model.Elements.iBloodSample */,_sI/* Model.Elements.iBSStored */),
_sK/* iBSDisposed */ = new T2(0,_sA/* Model.Elements.eBSDisposed */,_8S/* GHC.Base.Nothing */),
_sL/* gi22_3_1 */ = new T3(0,_sF/* Model.Elements.g22 */,_ag/* Model.Elements.iBloodSample */,_sK/* Model.Elements.iBSDisposed */),
_sM/* sPermDeferred44 */ = new T2(1,_sL/* Model.Elements.gi22_3_1 */,_4x/* GHC.Types.[] */),
_sN/* sPermDeferred43 */ = new T2(1,_sJ/* Model.Elements.gi22_2_1 */,_sM/* Model.Models.sPermDeferred44 */),
_sO/* sPermDeferred42 */ = new T2(1,_sH/* Model.Elements.gi22_1_1 */,_sN/* Model.Models.sPermDeferred43 */),
_sP/* sPermDeferred16 */ = new T2(1,_sO/* Model.Models.sPermDeferred42 */,_su/* Model.Models.sPermDeferred17 */),
_sQ/* g65 */ = new T2(1,_aW/* Model.Elements.eMedicalAssessment3 */,_4x/* GHC.Types.[] */),
_sR/* g64 */ = new T2(1,_b9/* Model.Elements.eMedicalAssessment2 */,_sQ/* Model.Elements.g65 */),
_sS/* g63 */ = new T2(1,_9B/* Model.Elements.eMedicalAssessment1 */,_sR/* Model.Elements.g64 */),
_sT/* g66 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g20"));
}),
_sU/* g20 */ = new T4(0,_sT/* Model.Elements.g66 */,_hQ/* Model.Elements.eMedicalAssessment */,_sS/* Model.Elements.g63 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_sV/* gi20_1 */ = new T3(0,_sU/* Model.Elements.g20 */,_hT/* Model.Elements.iMedicalAssessment */,_9C/* Model.Elements.iMedicalAssessment1 */),
_sW/* gi20_2 */ = new T3(0,_sU/* Model.Elements.g20 */,_hT/* Model.Elements.iMedicalAssessment */,_ba/* Model.Elements.iMedicalAssessment2 */),
_sX/* gi20_3 */ = new T3(0,_sU/* Model.Elements.g20 */,_hT/* Model.Elements.iMedicalAssessment */,_aX/* Model.Elements.iMedicalAssessment3 */),
_sY/* sPermDeferred47 */ = new T2(1,_sX/* Model.Elements.gi20_3 */,_4x/* GHC.Types.[] */),
_sZ/* sPermDeferred46 */ = new T2(1,_sW/* Model.Elements.gi20_2 */,_sY/* Model.Models.sPermDeferred47 */),
_t0/* sPermDeferred45 */ = new T2(1,_sV/* Model.Elements.gi20_1 */,_sZ/* Model.Models.sPermDeferred46 */),
_t1/* sPermDeferred15 */ = new T2(1,_t0/* Model.Models.sPermDeferred45 */,_sP/* Model.Models.sPermDeferred16 */),
_t2/* g61 */ = new T2(1,_aL/* Model.Elements.eInfectionTestingW */,_4x/* GHC.Types.[] */),
_t3/* g60 */ = new T2(1,_aE/* Model.Elements.eInfectionTestingV */,_t2/* Model.Elements.g61 */),
_t4/* g62 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g19"));
}),
_t5/* g19 */ = new T4(0,_t4/* Model.Elements.g62 */,_hh/* Model.Elements.eInfectionTesting */,_t3/* Model.Elements.g60 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_t6/* gi19_1 */ = new T3(0,_t5/* Model.Elements.g19 */,_hl/* Model.Elements.iInfectionTesting */,_aF/* Model.Elements.iInfectionTestingV */),
_t7/* gi19_2 */ = new T3(0,_t5/* Model.Elements.g19 */,_hl/* Model.Elements.iInfectionTesting */,_aM/* Model.Elements.iInfectionTestingW */),
_t8/* sPermDeferred49 */ = new T2(1,_t7/* Model.Elements.gi19_2 */,_4x/* GHC.Types.[] */),
_t9/* sPermDeferred48 */ = new T2(1,_t6/* Model.Elements.gi19_1 */,_t8/* Model.Models.sPermDeferred49 */),
_ta/* sPermDeferred14 */ = new T2(1,_t9/* Model.Models.sPermDeferred48 */,_t1/* Model.Models.sPermDeferred15 */),
_tb/* g54 */ = new T2(1,_cH/* Model.Elements.eDLITransplantation */,_4x/* GHC.Types.[] */),
_tc/* g53 */ = new T2(1,_cA/* Model.Elements.eBMTransplantation */,_tb/* Model.Elements.g54 */),
_td/* g52 */ = new T2(1,_ct/* Model.Elements.ePBSCTransplantation */,_tc/* Model.Elements.g53 */),
_te/* g55 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g13"));
}),
_tf/* g13 */ = new T4(0,_te/* Model.Elements.g55 */,_cm/* Model.Elements.eTransplantation */,_td/* Model.Elements.g52 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_tg/* gi13_1 */ = new T3(0,_tf/* Model.Elements.g13 */,_cn/* Model.Elements.iTransplantation */,_cu/* Model.Elements.iPBSCTransplantation */),
_th/* gi13_2 */ = new T3(0,_tf/* Model.Elements.g13 */,_cn/* Model.Elements.iTransplantation */,_cB/* Model.Elements.iBMTransplantation */),
_ti/* gi13_3 */ = new T3(0,_tf/* Model.Elements.g13 */,_cn/* Model.Elements.iTransplantation */,_cI/* Model.Elements.iDLITransplantation */),
_tj/* sPermDeferred52 */ = new T2(1,_ti/* Model.Elements.gi13_3 */,_4x/* GHC.Types.[] */),
_tk/* sPermDeferred51 */ = new T2(1,_th/* Model.Elements.gi13_2 */,_tj/* Model.Models.sPermDeferred52 */),
_tl/* sPermDeferred50 */ = new T2(1,_tg/* Model.Elements.gi13_1 */,_tk/* Model.Models.sPermDeferred51 */),
_tm/* sPermDeferred13 */ = new T2(1,_tl/* Model.Models.sPermDeferred50 */,_ta/* Model.Models.sPermDeferred14 */),
_tn/* g50 */ = new T2(1,_bU/* Model.Elements.eDLICollection */,_4x/* GHC.Types.[] */),
_to/* g49 */ = new T2(1,_bJ/* Model.Elements.eBMCollection */,_tn/* Model.Elements.g50 */),
_tp/* g48 */ = new T2(1,_bC/* Model.Elements.ePBSCCollection */,_to/* Model.Elements.g49 */),
_tq/* g51 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g12"));
}),
_tr/* g12 */ = new T4(0,_tq/* Model.Elements.g51 */,_br/* Model.Elements.eCollection */,_tp/* Model.Elements.g48 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_ts/* gi12_1 */ = new T3(0,_tr/* Model.Elements.g12 */,_bs/* Model.Elements.iCollection */,_bD/* Model.Elements.iPBSCCollection */),
_tt/* gi12_2 */ = new T3(0,_tr/* Model.Elements.g12 */,_bs/* Model.Elements.iCollection */,_bK/* Model.Elements.iBMCollection */),
_tu/* gi12_3 */ = new T3(0,_tr/* Model.Elements.g12 */,_bs/* Model.Elements.iCollection */,_bV/* Model.Elements.iDLICollection */),
_tv/* sPermDeferred55 */ = new T2(1,_tu/* Model.Elements.gi12_3 */,_4x/* GHC.Types.[] */),
_tw/* sPermDeferred54 */ = new T2(1,_tt/* Model.Elements.gi12_2 */,_tv/* Model.Models.sPermDeferred55 */),
_tx/* sPermDeferred53 */ = new T2(1,_ts/* Model.Elements.gi12_1 */,_tw/* Model.Models.sPermDeferred54 */),
_ty/* sPermDeferred12 */ = new T2(1,_tx/* Model.Models.sPermDeferred53 */,_tm/* Model.Models.sPermDeferred13 */),
_tz/* g43 */ = new T2(1,_bX/* Model.Elements.eDLIDonation */,_4x/* GHC.Types.[] */),
_tA/* g42 */ = new T2(1,_bM/* Model.Elements.eBMDonation */,_tz/* Model.Elements.g43 */),
_tB/* g41 */ = new T2(1,_bg/* Model.Elements.ePBSCDonation */,_tA/* Model.Elements.g42 */),
_tC/* g44 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g10"));
}),
_tD/* g10 */ = new T4(0,_tC/* Model.Elements.g44 */,_bu/* Model.Elements.eDonation */,_tB/* Model.Elements.g41 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_tE/* gi10_1 */ = new T3(0,_tD/* Model.Elements.g10 */,_bv/* Model.Elements.iDonation */,_bh/* Model.Elements.iPBSCDonation */),
_tF/* gi10_2 */ = new T3(0,_tD/* Model.Elements.g10 */,_bv/* Model.Elements.iDonation */,_bN/* Model.Elements.iBMDonation */),
_tG/* gi10_3 */ = new T3(0,_tD/* Model.Elements.g10 */,_bv/* Model.Elements.iDonation */,_bY/* Model.Elements.iDLIDonation */),
_tH/* sPermDeferred58 */ = new T2(1,_tG/* Model.Elements.gi10_3 */,_4x/* GHC.Types.[] */),
_tI/* sPermDeferred57 */ = new T2(1,_tF/* Model.Elements.gi10_2 */,_tH/* Model.Models.sPermDeferred58 */),
_tJ/* sPermDeferred56 */ = new T2(1,_tE/* Model.Elements.gi10_1 */,_tI/* Model.Models.sPermDeferred57 */),
_tK/* sPermDeferred11 */ = new T2(1,_tJ/* Model.Models.sPermDeferred56 */,_ty/* Model.Models.sPermDeferred12 */),
_tL/* g86 */ = new T2(1,_lb/* Model.Elements.eDLIPatient */,_4x/* GHC.Types.[] */),
_tM/* g87 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g26"));
}),
_tN/* g26 */ = new T4(0,_tM/* Model.Elements.g87 */,_kJ/* Model.Elements.ePBSCPatient */,_tL/* Model.Elements.g86 */,_qu/* Metamodel.UfoA.PlainGT */),
_tO/* gi26_1 */ = new T3(0,_tN/* Model.Elements.g26 */,_kN/* Model.Elements.iPBSCPatient */,_lf/* Model.Elements.iDLIPatient */),
_tP/* g88 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g27"));
}),
_tQ/* g27 */ = new T4(0,_tP/* Model.Elements.g88 */,_kX/* Model.Elements.eBMPatient */,_tL/* Model.Elements.g86 */,_qu/* Metamodel.UfoA.PlainGT */),
_tR/* gi27_1 */ = new T3(0,_tQ/* Model.Elements.g27 */,_l1/* Model.Elements.iBMPatient */,_lf/* Model.Elements.iDLIPatient */),
_tS/* sPermDeferred60 */ = new T2(1,_tR/* Model.Elements.gi27_1 */,_4x/* GHC.Types.[] */),
_tT/* sPermDeferred59 */ = new T2(1,_tO/* Model.Elements.gi26_1 */,_tS/* Model.Models.sPermDeferred60 */),
_tU/* sPermDeferred10 */ = new T2(1,_tT/* Model.Models.sPermDeferred59 */,_tK/* Model.Models.sPermDeferred11 */),
_tV/* g9a2 */ = new T2(1,_kX/* Model.Elements.eBMPatient */,_4x/* GHC.Types.[] */),
_tW/* g9a1 */ = new T2(1,_kJ/* Model.Elements.ePBSCPatient */,_tV/* Model.Elements.g9a2 */),
_tX/* g9a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g9a"));
}),
_tY/* g9a */ = new T4(0,_tX/* Model.Elements.g9a3 */,_fN/* Model.Elements.ePatient */,_tW/* Model.Elements.g9a1 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_tZ/* gi9a_1 */ = new T3(0,_tY/* Model.Elements.g9a */,_fR/* Model.Elements.iPatient */,_kN/* Model.Elements.iPBSCPatient */),
_u0/* gi9a_2 */ = new T3(0,_tY/* Model.Elements.g9a */,_fR/* Model.Elements.iPatient */,_l1/* Model.Elements.iBMPatient */),
_u1/* sPermDeferred62 */ = new T2(1,_u0/* Model.Elements.gi9a_2 */,_4x/* GHC.Types.[] */),
_u2/* sPermDeferred61 */ = new T2(1,_tZ/* Model.Elements.gi9a_1 */,_u1/* Model.Models.sPermDeferred62 */),
_u3/* sPermDeferred9 */ = new T2(1,_u2/* Model.Models.sPermDeferred61 */,_tU/* Model.Models.sPermDeferred10 */),
_u4/* sPermDeferred8 */ = new T2(1,_qt/* Model.Models.sPermDeferred63 */,_u3/* Model.Models.sPermDeferred9 */),
_u5/* sPermDeferred7 */ = new T2(1,_qf/* Model.Models.sPermDeferred67 */,_u4/* Model.Models.sPermDeferred8 */),
_u6/* sPermDeferred6 */ = new T2(1,_q5/* Model.Models.sPermDeferred69 */,_u5/* Model.Models.sPermDeferred7 */),
_u7/* g16 */ = new T2(1,_mF/* Model.Elements.eCourier */,_4x/* GHC.Types.[] */),
_u8/* g9 */ = new T2(1,_fN/* Model.Elements.ePatient */,_u7/* Model.Elements.g16 */),
_u9/* g2 */ = new T2(1,_gP/* Model.Elements.eDonorAspirant */,_u8/* Model.Elements.g9 */),
_ua/* g38 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g0"));
}),
_ub/* g0 */ = new T4(0,_ua/* Model.Elements.g38 */,_f1/* Model.Elements.ePerson */,_u9/* Model.Elements.g2 */,_qu/* Metamodel.UfoA.PlainGT */),
_uc/* gi0_1 */ = new T3(0,_ub/* Model.Elements.g0 */,_fa/* Model.Elements.iPerson1 */,_gT/* Model.Elements.iDonorAspirant */),
_ud/* gi0_2 */ = new T3(0,_ub/* Model.Elements.g0 */,_fh/* Model.Elements.iPerson2 */,_fR/* Model.Elements.iPatient */),
_ue/* iPerson9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("person3"));
}),
_uf/* iPerson8 */ = new T1(1,_ue/* Model.Elements.iPerson9 */),
_ug/* iPerson3 */ = new T2(0,_f1/* Model.Elements.ePerson */,_uf/* Model.Elements.iPerson8 */),
_uh/* gi0_3 */ = new T3(0,_ub/* Model.Elements.g0 */,_ug/* Model.Elements.iPerson3 */,_mJ/* Model.Elements.iCourier */),
_ui/* g39 */ = new T2(1,_dp/* Model.Elements.eDonor */,_4x/* GHC.Types.[] */),
_uj/* g40 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g1"));
}),
_uk/* g1 */ = new T4(0,_uj/* Model.Elements.g40 */,_gP/* Model.Elements.eDonorAspirant */,_ui/* Model.Elements.g39 */,_r4/* Metamodel.UfoA.Disjoint */),
_ul/* gi1 */ = new T3(0,_uk/* Model.Elements.g1 */,_gT/* Model.Elements.iDonorAspirant */,_dv/* Model.Elements.iDonor */),
_um/* eDonorAvailable1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Available Donor"));
}),
_un/* eDonorAvailable */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_um/* Model.Elements.eDonorAvailable1 */),
_uo/* eDonorTempDeferred1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Temporary\nDeferred\nDonor"));
}),
_up/* eDonorTempDeferred */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_uo/* Model.Elements.eDonorTempDeferred1 */),
_uq/* eDonorPermDeferred1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Permanently\nDeferred\nDonor"));
}),
_ur/* eDonorPermDeferred */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_uq/* Model.Elements.eDonorPermDeferred1 */),
_us/* g91 */ = new T2(1,_ur/* Model.Elements.eDonorPermDeferred */,_4x/* GHC.Types.[] */),
_ut/* g90 */ = new T2(1,_up/* Model.Elements.eDonorTempDeferred */,_us/* Model.Elements.g91 */),
_uu/* g89 */ = new T2(1,_un/* Model.Elements.eDonorAvailable */,_ut/* Model.Elements.g90 */),
_uv/* g92 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g3"));
}),
_uw/* g3 */ = new T4(0,_uv/* Model.Elements.g92 */,_dp/* Model.Elements.eDonor */,_uu/* Model.Elements.g89 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_ux/* iDonorAvailable */ = new T2(0,_un/* Model.Elements.eDonorAvailable */,_8S/* GHC.Base.Nothing */),
_uy/* gi3_1 */ = new T3(0,_uw/* Model.Elements.g3 */,_dv/* Model.Elements.iDonor */,_ux/* Model.Elements.iDonorAvailable */),
_uz/* iDonorTempDeferred */ = new T2(0,_up/* Model.Elements.eDonorTempDeferred */,_8S/* GHC.Base.Nothing */),
_uA/* gi3_2 */ = new T3(0,_uw/* Model.Elements.g3 */,_dv/* Model.Elements.iDonor */,_uz/* Model.Elements.iDonorTempDeferred */),
_uB/* iDonorPermDeferred */ = new T2(0,_ur/* Model.Elements.eDonorPermDeferred */,_8S/* GHC.Base.Nothing */),
_uC/* gi3_3 */ = new T3(0,_uw/* Model.Elements.g3 */,_dv/* Model.Elements.iDonor */,_uB/* Model.Elements.iDonorPermDeferred */),
_uD/* g116 */ = new T2(1,_ar/* Model.Elements.eDonorPotential */,_4x/* GHC.Types.[] */),
_uE/* g117 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g6"));
}),
_uF/* g6 */ = new T4(0,_uE/* Model.Elements.g117 */,_un/* Model.Elements.eDonorAvailable */,_uD/* Model.Elements.g116 */,_qu/* Metamodel.UfoA.PlainGT */),
_uG/* gi6 */ = new T3(0,_uF/* Model.Elements.g6 */,_ux/* Model.Elements.iDonorAvailable */,_as/* Model.Elements.iDonorPotential */),
_uH/* eSelectedV1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Selected and Available\nfor Verification"));
}),
_uI/* eSelectedV */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_uH/* Model.Elements.eSelectedV1 */),
_uJ/* eNotSelected1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("not Selected\nfor Examination"));
}),
_uK/* eNotSelected */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_uJ/* Model.Elements.eNotSelected1 */),
_uL/* eUnavailable1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Unavailable\nfor Examination"));
}),
_uM/* eUnavailable */ = new T2(0,_fj/* Metamodel.UfoA.Phase */,_uL/* Model.Elements.eUnavailable1 */),
_uN/* g82 */ = new T2(1,_uM/* Model.Elements.eUnavailable */,_4x/* GHC.Types.[] */),
_uO/* g81 */ = new T2(1,_uK/* Model.Elements.eNotSelected */,_uN/* Model.Elements.g82 */),
_uP/* g80 */ = new T2(1,_fv/* Model.Elements.eSelectedET */,_uO/* Model.Elements.g81 */),
_uQ/* g79 */ = new T2(1,_uI/* Model.Elements.eSelectedV */,_uP/* Model.Elements.g80 */),
_uR/* g83 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g24"));
}),
_uS/* g24 */ = new T4(0,_uR/* Model.Elements.g83 */,_ar/* Model.Elements.eDonorPotential */,_uQ/* Model.Elements.g79 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_uT/* iSelectedV */ = new T2(0,_uI/* Model.Elements.eSelectedV */,_8S/* GHC.Base.Nothing */),
_uU/* gi24_1 */ = new T3(0,_uS/* Model.Elements.g24 */,_as/* Model.Elements.iDonorPotential */,_uT/* Model.Elements.iSelectedV */),
_uV/* gi24_2 */ = new T3(0,_uS/* Model.Elements.g24 */,_as/* Model.Elements.iDonorPotential */,_fz/* Model.Elements.iSelectedET */),
_uW/* iNotSelected */ = new T2(0,_uK/* Model.Elements.eNotSelected */,_8S/* GHC.Base.Nothing */),
_uX/* gi24_3 */ = new T3(0,_uS/* Model.Elements.g24 */,_as/* Model.Elements.iDonorPotential */,_uW/* Model.Elements.iNotSelected */),
_uY/* iUnavailable */ = new T2(0,_uM/* Model.Elements.eUnavailable */,_8S/* GHC.Base.Nothing */),
_uZ/* gi24_4 */ = new T3(0,_uS/* Model.Elements.g24 */,_as/* Model.Elements.iDonorPotential */,_uY/* Model.Elements.iUnavailable */),
_v0/* g118 */ = new T2(1,_fD/* Model.Elements.eDonorVerified */,_4x/* GHC.Types.[] */),
_v1/* g119 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g7"));
}),
_v2/* g7 */ = new T4(0,_v1/* Model.Elements.g119 */,_uI/* Model.Elements.eSelectedV */,_v0/* Model.Elements.g118 */,_qu/* Metamodel.UfoA.PlainGT */),
_v3/* gi7 */ = new T3(0,_v2/* Model.Elements.g7 */,_uT/* Model.Elements.iSelectedV */,_fH/* Model.Elements.iDonorVerified */),
_v4/* g8a1 */ = new T2(1,_iV/* Model.Elements.eDonorReserved */,_4x/* GHC.Types.[] */),
_v5/* g8a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g8a"));
}),
_v6/* g8a */ = new T4(0,_v5/* Model.Elements.g8a2 */,_fD/* Model.Elements.eDonorVerified */,_v4/* Model.Elements.g8a1 */,_qu/* Metamodel.UfoA.PlainGT */),
_v7/* gi8a */ = new T3(0,_v6/* Model.Elements.g8a */,_fH/* Model.Elements.iDonorVerified */,_j3/* Model.Elements.iDonorReserved */),
_v8/* g8b1 */ = new T2(1,_ir/* Model.Elements.eDonorChosen */,_4x/* GHC.Types.[] */),
_v9/* g8b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g8b"));
}),
_va/* g8b */ = new T4(0,_v9/* Model.Elements.g8b2 */,_iV/* Model.Elements.eDonorReserved */,_v8/* Model.Elements.g8b1 */,_qu/* Metamodel.UfoA.PlainGT */),
_vb/* gi8b */ = new T3(0,_va/* Model.Elements.g8b */,_j3/* Model.Elements.iDonorReserved */,_iv/* Model.Elements.iDonorChosen */),
_vc/* g8c1 */ = new T2(1,_hF/* Model.Elements.eDonorCleared */,_4x/* GHC.Types.[] */),
_vd/* g8c2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g8c"));
}),
_ve/* g8c */ = new T4(0,_vd/* Model.Elements.g8c2 */,_qM/* Model.Elements.eDonorAvailableForWorkup */,_vc/* Model.Elements.g8c1 */,_qu/* Metamodel.UfoA.PlainGT */),
_vf/* gi8c */ = new T3(0,_ve/* Model.Elements.g8c */,_qT/* Model.Elements.iDonorAvailableForWorkup */,_hJ/* Model.Elements.iDonorCleared */),
_vg/* g8d1 */ = new T2(1,_jn/* Model.Elements.eDonorAccepted */,_4x/* GHC.Types.[] */),
_vh/* g8d2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g8d"));
}),
_vi/* g8d */ = new T4(0,_vh/* Model.Elements.g8d2 */,_hF/* Model.Elements.eDonorCleared */,_vg/* Model.Elements.g8d1 */,_qu/* Metamodel.UfoA.PlainGT */),
_vj/* gi8d */ = new T3(0,_vi/* Model.Elements.g8d */,_hJ/* Model.Elements.iDonorCleared */,_jr/* Model.Elements.iDonorAccepted */),
_vk/* g46 */ = new T2(1,_jY/* Model.Elements.eDonorBM */,_4x/* GHC.Types.[] */),
_vl/* g45 */ = new T2(1,_kj/* Model.Elements.eDonorPBSCPrepared */,_vk/* Model.Elements.g46 */),
_vm/* g47 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g11"));
}),
_vn/* g11 */ = new T4(0,_vm/* Model.Elements.g47 */,_jn/* Model.Elements.eDonorAccepted */,_vl/* Model.Elements.g45 */,_pS/* Metamodel.UfoA.DisjointComplete */),
_vo/* gi11_1 */ = new T3(0,_vn/* Model.Elements.g11 */,_jr/* Model.Elements.iDonorAccepted */,_kn/* Model.Elements.iDonorPBSCPrepared */),
_vp/* gi11_2 */ = new T3(0,_vn/* Model.Elements.g11 */,_jr/* Model.Elements.iDonorAccepted */,_k2/* Model.Elements.iDonorBM */),
_vq/* g11b1 */ = new T2(1,_jR/* Model.Elements.eDonorPBSC */,_4x/* GHC.Types.[] */),
_vr/* g11b2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g11b"));
}),
_vs/* g11b */ = new T4(0,_vr/* Model.Elements.g11b2 */,_kj/* Model.Elements.eDonorPBSCPrepared */,_vq/* Model.Elements.g11b1 */,_qu/* Metamodel.UfoA.PlainGT */),
_vt/* gi11b */ = new T3(0,_vs/* Model.Elements.g11b */,_kn/* Model.Elements.iDonorPBSCPrepared */,_jV/* Model.Elements.iDonorPBSC */),
_vu/* g16_4 */ = new T2(1,_lz/* Model.Elements.eDonorSubsequent */,_4x/* GHC.Types.[] */),
_vv/* g16_5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g16_1"));
}),
_vw/* g16_1 */ = new T4(0,_vv/* Model.Elements.g16_5 */,_jR/* Model.Elements.eDonorPBSC */,_vu/* Model.Elements.g16_4 */,_qu/* Metamodel.UfoA.PlainGT */),
_vx/* gi16_1 */ = new T3(0,_vw/* Model.Elements.g16_1 */,_jV/* Model.Elements.iDonorPBSC */,_lD/* Model.Elements.iDonorSubsequent */),
_vy/* g16_6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g16_2"));
}),
_vz/* g16_2 */ = new T4(0,_vy/* Model.Elements.g16_6 */,_jY/* Model.Elements.eDonorBM */,_vu/* Model.Elements.g16_4 */,_qu/* Metamodel.UfoA.PlainGT */),
_vA/* gi16_2 */ = new T3(0,_vz/* Model.Elements.g16_2 */,_k2/* Model.Elements.iDonorBM */,_lD/* Model.Elements.iDonorSubsequent */),
_vB/* g56 */ = new T2(1,_m2/* Model.Elements.eDonorClearedSub */,_4x/* GHC.Types.[] */),
_vC/* g57 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g17"));
}),
_vD/* g17 */ = new T4(0,_vC/* Model.Elements.g57 */,_lz/* Model.Elements.eDonorSubsequent */,_vB/* Model.Elements.g56 */,_qu/* Metamodel.UfoA.PlainGT */),
_vE/* gi17 */ = new T3(0,_vD/* Model.Elements.g17 */,_lD/* Model.Elements.iDonorSubsequent */,_m6/* Model.Elements.iDonorClearedSub */),
_vF/* g58 */ = new T2(1,_mj/* Model.Elements.eDonorAcceptedSub */,_4x/* GHC.Types.[] */),
_vG/* g59 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g18"));
}),
_vH/* g18 */ = new T4(0,_vG/* Model.Elements.g59 */,_m2/* Model.Elements.eDonorClearedSub */,_vF/* Model.Elements.g58 */,_qu/* Metamodel.UfoA.PlainGT */),
_vI/* gi18 */ = new T3(0,_vH/* Model.Elements.g18 */,_m6/* Model.Elements.iDonorClearedSub */,_mn/* Model.Elements.iDonorAcceptedSub */),
_vJ/* g84 */ = new T2(1,_mv/* Model.Elements.eDonorSubsequentHSC */,_4x/* GHC.Types.[] */),
_vK/* g85 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("g25"));
}),
_vL/* g25 */ = new T4(0,_vK/* Model.Elements.g85 */,_mj/* Model.Elements.eDonorAcceptedSub */,_vJ/* Model.Elements.g84 */,_qu/* Metamodel.UfoA.PlainGT */),
_vM/* gi25_1 */ = new T3(0,_vL/* Model.Elements.g25 */,_mn/* Model.Elements.iDonorAcceptedSub */,_mz/* Model.Elements.iDonorSubsequentHSC */),
_vN/* sPermDeferred95 */ = new T2(1,_vM/* Model.Elements.gi25_1 */,_4x/* GHC.Types.[] */),
_vO/* sPermDeferred94 */ = new T2(1,_vI/* Model.Elements.gi18 */,_vN/* Model.Models.sPermDeferred95 */),
_vP/* sPermDeferred93 */ = new T2(1,_vE/* Model.Elements.gi17 */,_vO/* Model.Models.sPermDeferred94 */),
_vQ/* sPermDeferred92 */ = new T2(1,_vA/* Model.Elements.gi16_2 */,_vP/* Model.Models.sPermDeferred93 */),
_vR/* sPermDeferred91 */ = new T2(1,_vx/* Model.Elements.gi16_1 */,_vQ/* Model.Models.sPermDeferred92 */),
_vS/* sPermDeferred90 */ = new T2(1,_vt/* Model.Elements.gi11b */,_vR/* Model.Models.sPermDeferred91 */),
_vT/* sPermDeferred89 */ = new T2(1,_vp/* Model.Elements.gi11_2 */,_vS/* Model.Models.sPermDeferred90 */),
_vU/* sPermDeferred88 */ = new T2(1,_vo/* Model.Elements.gi11_1 */,_vT/* Model.Models.sPermDeferred89 */),
_vV/* sPermDeferred87 */ = new T2(1,_vj/* Model.Elements.gi8d */,_vU/* Model.Models.sPermDeferred88 */),
_vW/* sPermDeferred86 */ = new T2(1,_vf/* Model.Elements.gi8c */,_vV/* Model.Models.sPermDeferred87 */),
_vX/* sPermDeferred85 */ = new T2(1,_vb/* Model.Elements.gi8b */,_vW/* Model.Models.sPermDeferred86 */),
_vY/* sPermDeferred84 */ = new T2(1,_v7/* Model.Elements.gi8a */,_vX/* Model.Models.sPermDeferred85 */),
_vZ/* sPermDeferred83 */ = new T2(1,_v3/* Model.Elements.gi7 */,_vY/* Model.Models.sPermDeferred84 */),
_w0/* sPermDeferred82 */ = new T2(1,_uZ/* Model.Elements.gi24_4 */,_vZ/* Model.Models.sPermDeferred83 */),
_w1/* sPermDeferred81 */ = new T2(1,_uX/* Model.Elements.gi24_3 */,_w0/* Model.Models.sPermDeferred82 */),
_w2/* sPermDeferred80 */ = new T2(1,_uV/* Model.Elements.gi24_2 */,_w1/* Model.Models.sPermDeferred81 */),
_w3/* sPermDeferred79 */ = new T2(1,_uU/* Model.Elements.gi24_1 */,_w2/* Model.Models.sPermDeferred80 */),
_w4/* sPermDeferred78 */ = new T2(1,_uG/* Model.Elements.gi6 */,_w3/* Model.Models.sPermDeferred79 */),
_w5/* sPermDeferred77 */ = new T2(1,_uC/* Model.Elements.gi3_3 */,_w4/* Model.Models.sPermDeferred78 */),
_w6/* sPermDeferred76 */ = new T2(1,_uA/* Model.Elements.gi3_2 */,_w5/* Model.Models.sPermDeferred77 */),
_w7/* sPermDeferred75 */ = new T2(1,_uy/* Model.Elements.gi3_1 */,_w6/* Model.Models.sPermDeferred76 */),
_w8/* sPermDeferred74 */ = new T2(1,_ul/* Model.Elements.gi1 */,_w7/* Model.Models.sPermDeferred75 */),
_w9/* sPermDeferred73 */ = new T2(1,_uh/* Model.Elements.gi0_3 */,_w8/* Model.Models.sPermDeferred74 */),
_wa/* sPermDeferred72 */ = new T2(1,_ud/* Model.Elements.gi0_2 */,_w9/* Model.Models.sPermDeferred73 */),
_wb/* sPermDeferred71 */ = new T2(1,_uc/* Model.Elements.gi0_1 */,_wa/* Model.Models.sPermDeferred72 */),
_wc/* sPermDeferred5 */ = new T2(1,_wb/* Model.Models.sPermDeferred71 */,_u6/* Model.Models.sPermDeferred6 */),
_wd/* a6 */ = new T(function(){
  return B(_8A/* Metamodel.UfoAInst.$woumiElements */(_wc/* Model.Models.sPermDeferred5 */, _pR/* Model.Models.ouModelInst29 */, _dl/* Model.Models.ouModelInst1 */));
}),
_we/* a7 */ = function(_wf/*  scYQ */, _/* EXTERNAL */){
  while(1){
    var _wg/*  a7 */ = B((function(_wh/* scYQ */, _/* EXTERNAL */){
      var _wi/* scYS */ = E(_wh/* scYQ */);
      if(!_wi/* scYS */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _wj/* scYV */ = B(_3t/* JQuery.selectById1 */(_3o/* Main.diagAJq2 */, _/* EXTERNAL */)),
        _wk/* scZ6 */ = B(_3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
          var _wl/* scZ0 */ = E(_wi/* scYS */.a);
          return B(_6a/* Metamodel.UfoAInst.$w$couId2 */(_wl/* scZ0 */.a, _wl/* scZ0 */.b, _wl/* scZ0 */.c));
        },1))), E(_wj/* scYV */), _/* EXTERNAL */)),
        _wm/* scZb */ = B(_3e/* JQuery.$wa2 */(_3r/* JQuery.hideJq3 */, _3q/* JQuery.hideJq2 */, E(_wk/* scZ6 */), _/* EXTERNAL */));
        _wf/*  scYQ */ = _wi/* scYS */.b;
        return __continue/* EXTERNAL */;
      }
    })(_wf/*  scYQ */, _/* EXTERNAL */));
    if(_wg/*  a7 */!=__continue/* EXTERNAL */){
      return _wg/*  a7 */;
    }
  }
},
_wn/* a8 */ = new T(function(){
  return B(_5r/* Metamodel.UfoAInst.oumiGeneralizations_go */(_wc/* Model.Models.sPermDeferred5 */));
}),
_wo/* go */ = function(_wp/* sd0C */, _wq/* sd0D */){
  while(1){
    var _wr/* sd0E */ = E(_wp/* sd0C */);
    if(!_wr/* sd0E */._){
      return E(_wq/* sd0D */);
    }else{
      _wp/* sd0C */ = _wr/* sd0E */.b;
      _wq/* sd0D */ = _wr/* sd0E */.a;
      continue;
    }
  }
},
_ws/* go1 */ = function(_wt/*  sd15 */, _wu/*  sd16 */){
  while(1){
    var _wv/*  go1 */ = B((function(_ww/* sd15 */, _wx/* sd16 */){
      var _wy/* sd17 */ = E(_ww/* sd15 */);
      if(!_wy/* sd17 */._){
        return E(_wx/* sd16 */);
      }else{
        _wt/*  sd15 */ = _wy/* sd17 */.b;
        _wu/*  sd16 */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_wx/* sd16 */, _wy/* sd17 */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_wt/*  sd15 */, _wu/*  sd16 */));
    if(_wv/*  go1 */!=__continue/* EXTERNAL */){
      return _wv/*  go1 */;
    }
  }
},
_wz/* go2 */ = function(_wA/*  sd2D */, _wB/*  sd2E */){
  while(1){
    var _wC/*  go2 */ = B((function(_wD/* sd2D */, _wE/* sd2E */){
      var _wF/* sd2F */ = E(_wD/* sd2D */);
      if(!_wF/* sd2F */._){
        return E(_wE/* sd2E */);
      }else{
        _wA/*  sd2D */ = _wF/* sd2F */.b;
        _wB/*  sd2E */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_wE/* sd2E */, _wF/* sd2F */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_wA/*  sd2D */, _wB/*  sd2E */));
    if(_wC/*  go2 */!=__continue/* EXTERNAL */){
      return _wC/*  go2 */;
    }
  }
},
_wG/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(": empty list"));
}),
_wH/* prel_list_str */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Prelude."));
}),
_wI/* errorEmptyList */ = function(_wJ/* s9sr */){
  return new F(function(){return err/* EXTERNAL */(B(_2G/* GHC.Base.++ */(_wH/* GHC.List.prel_list_str */, new T(function(){
    return B(_2G/* GHC.Base.++ */(_wJ/* s9sr */, _wG/* GHC.List.lvl */));
  },1))));});
},
_wK/* lvl16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("last"));
}),
_wL/* last1 */ = new T(function(){
  return B(_wI/* GHC.List.errorEmptyList */(_wK/* GHC.List.lvl16 */));
}),
_wM/* oumsElementsNew_go */ = function(_wN/*  s4nd */, _wO/*  s4ne */){
  while(1){
    var _wP/*  oumsElementsNew_go */ = B((function(_wQ/* s4nd */, _wR/* s4ne */){
      var _wS/* s4nf */ = E(_wQ/* s4nd */);
      if(!_wS/* s4nf */._){
        return E(_wR/* s4ne */);
      }else{
        _wN/*  s4nd */ = _wS/* s4nf */.b;
        _wO/*  s4ne */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_wR/* s4ne */, _wS/* s4nf */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_wN/*  s4nd */, _wO/*  s4ne */));
    if(_wP/*  oumsElementsNew_go */!=__continue/* EXTERNAL */){
      return _wP/*  oumsElementsNew_go */;
    }
  }
},
_wT/* oumsElementsNew_go1 */ = function(_wU/*  s4n7 */, _wV/*  s4n8 */){
  while(1){
    var _wW/*  oumsElementsNew_go1 */ = B((function(_wX/* s4n7 */, _wY/* s4n8 */){
      var _wZ/* s4n9 */ = E(_wX/* s4n7 */);
      if(!_wZ/* s4n9 */._){
        return E(_wY/* s4n8 */);
      }else{
        _wU/*  s4n7 */ = _wZ/* s4n9 */.b;
        _wV/*  s4n8 */ = new T(function(){
          return B(_4M/* Metamodel.UfoB.oumsElements1 */(_wY/* s4n8 */, _wZ/* s4n9 */.a));
        });
        return __continue/* EXTERNAL */;
      }
    })(_wU/*  s4n7 */, _wV/*  s4n8 */));
    if(_wW/*  oumsElementsNew_go1 */!=__continue/* EXTERNAL */){
      return _wW/*  oumsElementsNew_go1 */;
    }
  }
},
_x0/* oumsElementsNew */ = function(_x1/* s4nj */, _x2/* s4nk */){
  return new F(function(){return _5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_wT/* Metamodel.UfoB.oumsElementsNew_go1 */(_x2/* s4nk */, _4x/* GHC.Types.[] */)), B(_wM/* Metamodel.UfoB.oumsElementsNew_go */(_x1/* s4nj */, _4x/* GHC.Types.[] */)));});
},
_x3/* $wa */ = function(_x4/* sd3x */, _x5/* sd3y */, _/* EXTERNAL */){
  var _x6/* sd3A */ = rMV/* EXTERNAL */(_x5/* sd3y */),
  _x7/* sd3I */ = E(_x4/* sd3x */);
  if(!B(_7/* GHC.Base.eqString */(B(_wo/* Main.go */(_x6/* sd3A */, _wL/* GHC.List.last1 */)).a, _x7/* sd3I */.a))){
    var _x8/* sd3O */ = B(_85/* Main.a5 */(_wd/* Main.a6 */, _/* EXTERNAL */)),
    _x9/* sd3R */ = B(_we/* Main.a7 */(_wn/* Main.a8 */, _/* EXTERNAL */)),
    _xa/* sd3U */ = B(_3F/* Main.$sa */(_dy/* Model.Elements.iGeneAssignment1 */, _dv/* Model.Elements.iDonor */, _pQ/* Model.Models.ouModelInst30 */, _/* EXTERNAL */)),
    _xb/* sd3X */ = B(_4a/* Main.$sa1 */(_8Z/* Model.Elements.iDonorRegistry */, _8W/* Model.Elements.iDonorCentre */, _dk/* Model.Models.ouModelInst2 */, _/* EXTERNAL */)),
    _xc/* sd40 */ = B(_2G/* GHC.Base.++ */(_x6/* sd3A */, new T2(1,_x7/* sd3I */,_4x/* GHC.Types.[] */))),
    _xd/* sd43 */ = B(_60/* Main.a11 */(B(_ws/* Main.go1 */(_xc/* sd40 */, _4x/* GHC.Types.[] */)), _/* EXTERNAL */)),
    _xe/* sd47 */ = B(_6q/* Main.a12 */(B(_5B/* Metamodel.UfoB.$woumsGeneralizations */(_wc/* Model.Models.sPermDeferred5 */, _xc/* sd40 */)), _/* EXTERNAL */)),
    _xf/* sd4b */ = B(_6z/* Main.a13 */(B(_53/* Metamodel.UfoB.$woumsAssocs */(_pR/* Model.Models.ouModelInst29 */, _xc/* sd40 */)), _/* EXTERNAL */)),
    _xg/* sd4f */ = B(_6I/* Main.a14 */(B(_5m/* Metamodel.UfoB.$woumsAssocsPH */(_dl/* Model.Models.ouModelInst1 */, _xc/* sd40 */)), _/* EXTERNAL */)),
    _xh/* sd4j */ = B(_79/* Main.a15 */(B(_wz/* Main.go2 */(_x6/* sd3A */, _4x/* GHC.Types.[] */)), _/* EXTERNAL */)),
    _xi/* sd4n */ = B(_7h/* Main.a16 */(B(_x0/* Metamodel.UfoB.oumsElementsNew */(_x6/* sd3A */, _xc/* sd40 */)), _/* EXTERNAL */)),
    _xj/* sd4r */ = B(_7G/* Main.a17 */(B(_53/* Metamodel.UfoB.$woumsAssocs */(_pR/* Model.Models.ouModelInst29 */, _x6/* sd3A */)), _/* EXTERNAL */)),
    _xk/* sd4x */ = B(_7P/* Main.a18 */(B(_5Q/* Data.OldList.\\ */(_1y/* Metamodel.UfoAInst.$fEqOUAssocInst */, B(_53/* Metamodel.UfoB.$woumsAssocs */(_pR/* Model.Models.ouModelInst29 */, _xc/* sd40 */)), B(_53/* Metamodel.UfoB.$woumsAssocs */(_pR/* Model.Models.ouModelInst29 */, _x6/* sd3A */)))), _/* EXTERNAL */)),
    _xl/* sd4B */ = B(_7W/* Main.a19 */(B(_5m/* Metamodel.UfoB.$woumsAssocsPH */(_dl/* Model.Models.ouModelInst1 */, _x6/* sd3A */)), _/* EXTERNAL */)),
    _xm/* sd4H */ = B(_81/* Main.a20 */(B(_5Q/* Data.OldList.\\ */(_2v/* Metamodel.UfoAInst.$fEqOUAssocPHInst */, B(_5m/* Metamodel.UfoB.$woumsAssocsPH */(_dl/* Model.Models.ouModelInst1 */, _xc/* sd40 */)), B(_5m/* Metamodel.UfoB.$woumsAssocsPH */(_dl/* Model.Models.ouModelInst1 */, _x6/* sd3A */)))), _/* EXTERNAL */)),
    _xn/* sd4K */ = B(_3t/* JQuery.selectById1 */(_80/* Main.a2 */, _/* EXTERNAL */)),
    _xo/* sd4P */ = B(_4i/* JQuery.$wa28 */(_x7/* sd3I */.d, E(_xn/* sd4K */), _/* EXTERNAL */)),
    _/* EXTERNAL */ = wMV/* EXTERNAL */(_x5/* sd3y */, _xc/* sd40 */);
    return _0/* GHC.Tuple.() */;
  }else{
    return _0/* GHC.Tuple.() */;
  }
},
_xp/* $wlenAcc */ = function(_xq/* s9Bd */, _xr/* s9Be */){
  while(1){
    var _xs/* s9Bf */ = E(_xq/* s9Bd */);
    if(!_xs/* s9Bf */._){
      return E(_xr/* s9Be */);
    }else{
      var _xt/*  s9Be */ = _xr/* s9Be */+1|0;
      _xq/* s9Bd */ = _xs/* s9Bf */.b;
      _xr/* s9Be */ = _xt/*  s9Be */;
      continue;
    }
  }
},
_xu/* diagBJq2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ufob-svg"));
}),
_xv/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */(" > polygon"));
}),
_xw/* getBShape_go */ = function(_xx/*  scWl */){
  while(1){
    var _xy/*  getBShape_go */ = B((function(_xz/* scWl */){
      var _xA/* scWm */ = E(_xz/* scWl */);
      if(!_xA/* scWm */._){
        return E(_xv/* Main.lvl */);
      }else{
        var _xB/* scWo */ = _xA/* scWm */.b,
        _xC/* scWp */ = E(_xA/* scWm */.a);
        if(!B(_2w/* Metamodel.UfoA.$wlvl */(_xC/* scWp */))){
          _xx/*  scWl */ = _xB/* scWo */;
          return __continue/* EXTERNAL */;
        }else{
          return new T2(1,_xC/* scWp */,new T(function(){
            return B(_xw/* Main.getBShape_go */(_xB/* scWo */));
          }));
        }
      }
    })(_xx/*  scWl */));
    if(_xy/*  getBShape_go */!=__continue/* EXTERNAL */){
      return _xy/*  getBShape_go */;
    }
  }
},
_xD/* getBShape1 */ = function(_xE/* scWt */, _/* EXTERNAL */){
  var _xF/* scWv */ = B(_3t/* JQuery.selectById1 */(_xu/* Main.diagBJq2 */, _/* EXTERNAL */));
  return new F(function(){return _3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
    return B(_xw/* Main.getBShape_go */(E(_xE/* scWt */).a));
  },1))), E(_xF/* scWv */), _/* EXTERNAL */);});
},
_xG/* getGroup1 */ = function(_xH/* scW7 */, _/* EXTERNAL */){
  var _xI/* scW9 */ = B(_3t/* JQuery.selectById1 */(_xu/* Main.diagBJq2 */, _/* EXTERNAL */));
  return new F(function(){return _3k/* JQuery.$wa20 */(B(_2G/* GHC.Base.++ */(_3p/* Main.getALine3 */, new T(function(){
    return B(_2P/* GHC.List.filter */(_2C/* Metamodel.UfoA.$fOUIdentifiedOUAssoc1 */, E(_xH/* scW7 */).a));
  },1))), E(_xI/* scW9 */), _/* EXTERNAL */);});
},
_xJ/* go3 */ = function(_xK/* sd4Y */, _xL/* sd4Z */){
  while(1){
    var _xM/* sd50 */ = E(_xK/* sd4Y */);
    if(!_xM/* sd50 */._){
      return E(_xL/* sd4Z */);
    }else{
      _xK/* sd4Y */ = _xM/* sd50 */.b;
      _xL/* sd4Z */ = _xM/* sd50 */.a;
      continue;
    }
  }
},
_xN/* hlCol */ = new T(function(){
  return B(unCStr/* EXTERNAL */("#9DDD17"));
}),
_xO/* lvl10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("pointer"));
}),
_xP/* lvl11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("fill"));
}),
_xQ/* lvl9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("cursor"));
}),
_xR/* normCol */ = new T(function(){
  return B(unCStr/* EXTERNAL */("#000000"));
}),
_xS/* lvl2 */ = function(_/* EXTERNAL */){
  return new F(function(){return __jsNull/* EXTERNAL */();});
},
_xT/* unsafeDupablePerformIO */ = function(_xU/* s2Wdd */){
  var _xV/* s2Wde */ = B(A1(_xU/* s2Wdd */,_/* EXTERNAL */));
  return E(_xV/* s2Wde */);
},
_xW/* nullValue */ = new T(function(){
  return B(_xT/* GHC.IO.unsafeDupablePerformIO */(_xS/* Haste.Prim.Any.lvl2 */));
}),
_xX/* jsNull */ = new T(function(){
  return E(_xW/* Haste.Prim.Any.nullValue */);
}),
_xY/* onClick2 */ = "(function (ev, jq) { jq.click(ev); })",
_xZ/* onClick1 */ = function(_y0/* s9T8 */, _y1/* s9T9 */, _/* EXTERNAL */){
  var _y2/* s9Tl */ = __createJSFunc/* EXTERNAL */(2, function(_y3/* s9Tc */, _/* EXTERNAL */){
    var _y4/* s9Te */ = B(A2(E(_y0/* s9T8 */),_y3/* s9Tc */, _/* EXTERNAL */));
    return _xX/* Haste.Prim.Any.jsNull */;
  }),
  _y5/* s9To */ = E(_y1/* s9T9 */),
  _y6/* s9Tt */ = eval/* EXTERNAL */(E(_xY/* JQuery.onClick2 */)),
  _y7/* s9TB */ = __app2/* EXTERNAL */(E(_y6/* s9Tt */), _y2/* s9Tl */, _y5/* s9To */);
  return _y5/* s9To */;
},
_y8/* onMouseEnter2 */ = "(function (ev, jq) { jq.mouseenter(ev); })",
_y9/* onMouseEnter1 */ = function(_ya/* s9Qy */, _yb/* s9Qz */, _/* EXTERNAL */){
  var _yc/* s9QL */ = __createJSFunc/* EXTERNAL */(2, function(_yd/* s9QC */, _/* EXTERNAL */){
    var _ye/* s9QE */ = B(A2(E(_ya/* s9Qy */),_yd/* s9QC */, _/* EXTERNAL */));
    return _xX/* Haste.Prim.Any.jsNull */;
  }),
  _yf/* s9QO */ = E(_yb/* s9Qz */),
  _yg/* s9QT */ = eval/* EXTERNAL */(E(_y8/* JQuery.onMouseEnter2 */)),
  _yh/* s9R1 */ = __app2/* EXTERNAL */(E(_yg/* s9QT */), _yc/* s9QL */, _yf/* s9QO */);
  return _yf/* s9QO */;
},
_yi/* onMouseLeave2 */ = "(function (ev, jq) { jq.mouseleave(ev); })",
_yj/* onMouseLeave1 */ = function(_yk/* s9Q2 */, _yl/* s9Q3 */, _/* EXTERNAL */){
  var _ym/* s9Qf */ = __createJSFunc/* EXTERNAL */(2, function(_yn/* s9Q6 */, _/* EXTERNAL */){
    var _yo/* s9Q8 */ = B(A2(E(_yk/* s9Q2 */),_yn/* s9Q6 */, _/* EXTERNAL */));
    return _xX/* Haste.Prim.Any.jsNull */;
  }),
  _yp/* s9Qi */ = E(_yl/* s9Q3 */),
  _yq/* s9Qn */ = eval/* EXTERNAL */(E(_yi/* JQuery.onMouseLeave2 */)),
  _yr/* s9Qv */ = __app2/* EXTERNAL */(E(_yq/* s9Qn */), _ym/* s9Qf */, _yp/* s9Qi */);
  return _yp/* s9Qi */;
},
_ys/* visitedCol */ = new T(function(){
  return B(unCStr/* EXTERNAL */("#DAF2A9"));
}),
_yt/* addEvents1 */ = function(_yu/* sd53 */, _yv/* sd54 */, _/* EXTERNAL */){
  var _yw/* sd56 */ = B(_xG/* Main.getGroup1 */(_yv/* sd54 */, _/* EXTERNAL */)),
  _yx/* sd5u */ = B(_y9/* JQuery.onMouseEnter1 */(function(_yy/* sd59 */, _/* EXTERNAL */){
    var _yz/* sd5b */ = B(_xD/* Main.getBShape1 */(_yv/* sd54 */, _/* EXTERNAL */)),
    _yA/* sd5g */ = B(_6V/* JQuery.$wa6 */(_75/* Main.lvl1 */, _xN/* Main.hlCol */, E(_yz/* sd5b */), _/* EXTERNAL */)),
    _yB/* sd5l */ = B(_6V/* JQuery.$wa6 */(_76/* Main.lvl3 */, _7g/* Main.lvl4 */, E(_yA/* sd5g */), _/* EXTERNAL */)),
    _yC/* sd5q */ = B(_3e/* JQuery.$wa2 */(_xQ/* Main.lvl9 */, _xO/* Main.lvl10 */, E(_yB/* sd5l */), _/* EXTERNAL */));
    return _0/* GHC.Tuple.() */;
  }, _yw/* sd56 */, _/* EXTERNAL */)),
  _yD/* sd5N */ = B(_yj/* JQuery.onMouseLeave1 */(function(_yE/* sd5x */, _/* EXTERNAL */){
    var _yF/* sd5z */ = B(_xD/* Main.getBShape1 */(_yv/* sd54 */, _/* EXTERNAL */)),
    _yG/* sd5E */ = B(_6V/* JQuery.$wa6 */(_75/* Main.lvl1 */, _xR/* Main.normCol */, E(_yF/* sd5z */), _/* EXTERNAL */)),
    _yH/* sd5J */ = B(_6V/* JQuery.$wa6 */(_76/* Main.lvl3 */, _7A/* Main.lvl6 */, E(_yG/* sd5E */), _/* EXTERNAL */));
    return _0/* GHC.Tuple.() */;
  }, _yx/* sd5u */, _/* EXTERNAL */)),
  _yI/* sd6k */ = function(_yJ/* sd5Q */, _/* EXTERNAL */){
    var _yK/* sd5S */ = E(_yu/* sd53 */),
    _yL/* sd5U */ = rMV/* EXTERNAL */(_yK/* sd5S */),
    _yM/* sd5X */ = function(_/* EXTERNAL */){
      var _yN/* sd5Z */ = B(_xD/* Main.getBShape1 */(_yv/* sd54 */, _/* EXTERNAL */)),
      _yO/* sd64 */ = B(_6V/* JQuery.$wa6 */(_xP/* Main.lvl11 */, _xN/* Main.hlCol */, E(_yN/* sd5Z */), _/* EXTERNAL */)),
      _yP/* sd67 */ = B(_x3/* Main.$wa */(_yv/* sd54 */, _yK/* sd5S */, _/* EXTERNAL */));
      return _0/* GHC.Tuple.() */;
    };
    if(B(_xp/* GHC.List.$wlenAcc */(_yL/* sd5U */, 0))==1){
      return new F(function(){return _yM/* sd5X */(_/* EXTERNAL */);});
    }else{
      var _yQ/* sd6c */ = B(_xD/* Main.getBShape1 */(new T(function(){
        return B(_xJ/* Main.go3 */(_yL/* sd5U */, _wL/* GHC.List.last1 */));
      },1), _/* EXTERNAL */)),
      _yR/* sd6h */ = B(_6V/* JQuery.$wa6 */(_xP/* Main.lvl11 */, _ys/* Main.visitedCol */, E(_yQ/* sd6c */), _/* EXTERNAL */));
      return new F(function(){return _yM/* sd5X */(_/* EXTERNAL */);});
    }
  },
  _yS/* sd6l */ = B(_xZ/* JQuery.onClick1 */(_yI/* sd6k */, _yD/* sd5N */, _/* EXTERNAL */));
  return _0/* GHC.Tuple.() */;
},
_yT/* getWindow_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function () { return $(window); })");
}),
_yU/* emptySituation */ = new T4(0,_4x/* GHC.Types.[] */,_4x/* GHC.Types.[] */,_4x/* GHC.Types.[] */,_4x/* GHC.Types.[] */),
_yV/* initModelState */ = new T2(1,_yU/* Metamodel.UfoB.emptySituation */,_4x/* GHC.Types.[] */),
_yW/* sMonitored1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nFollow-up"));
}),
_yX/* sMonitored */ = new T4(0,_yW/* Model.Models.sMonitored1 */,_4x/* GHC.Types.[] */,_4x/* GHC.Types.[] */,_4x/* GHC.Types.[] */),
_yY/* ouModelB38 */ = new T2(1,_yX/* Model.Models.sMonitored */,_4x/* GHC.Types.[] */),
_yZ/* lvl32 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nBecomes\nTemporary\nUnavailable"));
}),
_z0/* a3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reasons for making an Available Donor temporary unavailable comprise a performed transplantation, or a travel, medical reasons, gravidity and others."));
}),
_z1/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Just one phase can be in switchPhase"));
}),
_z2/* fail */ = function(_z3/* s4oZ */){
  return new F(function(){return err/* EXTERNAL */(_z1/* Metamodel.UfoB.lvl */);});
},
_z4/* lvl1 */ = new T(function(){
  return B(_z2/* Metamodel.UfoB.fail */(_/* EXTERNAL */));
}),
_z5/* lvl2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("No phase sup found"));
}),
_z6/* lvl3 */ = new T(function(){
  return B(err/* EXTERNAL */(_z5/* Metamodel.UfoB.lvl2 */));
}),
_z7/* $wswitchPhase */ = function(_z8/* s4p0 */, _z9/* s4p1 */, _za/* s4p2 */){
  var _zb/* s4p3 */ = E(_z9/* s4p1 */);
  if(!_zb/* s4p3 */._){
    return E(_z4/* Metamodel.UfoB.lvl1 */);
  }else{
    var _zc/* s4p4 */ = _zb/* s4p3 */.a;
    if(!E(_zb/* s4p3 */.b)._){
      var _zd/* s4p7 */ = function(_ze/* s4p8 */, _zf/* s4p9 */, _zg/* s4pa */){
        var _zh/* s4pb */ = B(_5r/* Metamodel.UfoAInst.oumiGeneralizations_go */(_z8/* s4p0 */));
        if(!_zh/* s4pb */._){
          return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _4x/* GHC.Types.[] */, new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
        }else{
          var _zi/* s4ph */ = _zh/* s4pb */.b,
          _zj/* s4pi */ = E(_zh/* s4pb */.a),
          _zk/* s4pl */ = _zj/* s4pi */.c,
          _zl/* s4pm */ = E(_zf/* s4p9 */),
          _zm/* s4po */ = _zl/* s4pm */.b,
          _zn/* s4pp */ = E(_zl/* s4pm */.a),
          _zo/* s4pq */ = _zn/* s4pp */.a,
          _zp/* s4pr */ = _zn/* s4pp */.b,
          _zq/* s4ps */ = E(_zj/* s4pi */.b),
          _zr/* s4pu */ = _zq/* s4ps */.b,
          _zs/* s4pv */ = E(_zq/* s4ps */.a);
          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _zs/* s4pv */.a, _zs/* s4pv */.b))){
            var _zt/* s4pz */ = function(_zu/*  s4pA */){
              while(1){
                var _zv/*  s4pz */ = B((function(_zw/* s4pA */){
                  var _zx/* s4pB */ = E(_zw/* s4pA */);
                  if(!_zx/* s4pB */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _zy/* s4pD */ = _zx/* s4pB */.b,
                    _zz/* s4pE */ = E(_zx/* s4pB */.a),
                    _zA/* s4pH */ = _zz/* s4pE */.c,
                    _zB/* s4pI */ = E(_zz/* s4pE */.b),
                    _zC/* s4pK */ = _zB/* s4pI */.b,
                    _zD/* s4pL */ = E(_zB/* s4pI */.a);
                    if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _zD/* s4pL */.a, _zD/* s4pL */.b))){
                      _zu/*  s4pA */ = _zy/* s4pD */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _zE/* s4pP */ = E(_zm/* s4po */);
                      if(!_zE/* s4pP */._){
                        if(!E(_zC/* s4pK */)._){
                          return new T2(1,_zA/* s4pH */,new T(function(){
                            return B(_zt/* s4pz */(_zy/* s4pD */));
                          }));
                        }else{
                          _zu/*  s4pA */ = _zy/* s4pD */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        var _zF/* s4pU */ = E(_zC/* s4pK */);
                        if(!_zF/* s4pU */._){
                          _zu/*  s4pA */ = _zy/* s4pD */;
                          return __continue/* EXTERNAL */;
                        }else{
                          if(!B(_7/* GHC.Base.eqString */(_zE/* s4pP */.a, _zF/* s4pU */.a))){
                            _zu/*  s4pA */ = _zy/* s4pD */;
                            return __continue/* EXTERNAL */;
                          }else{
                            return new T2(1,_zA/* s4pH */,new T(function(){
                              return B(_zt/* s4pz */(_zy/* s4pD */));
                            }));
                          }
                        }
                      }
                    }
                  }
                })(_zu/*  s4pA */));
                if(_zv/*  s4pz */!=__continue/* EXTERNAL */){
                  return _zv/*  s4pz */;
                }
              }
            };
            return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_zt/* s4pz */(_zi/* s4ph */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
          }else{
            var _zG/* s4q3 */ = E(_zm/* s4po */);
            if(!_zG/* s4q3 */._){
              if(!E(_zr/* s4pu */)._){
                var _zH/* s4qo */ = new T(function(){
                  var _zI/* s4q5 */ = function(_zJ/*  s4q6 */){
                    while(1){
                      var _zK/*  s4q5 */ = B((function(_zL/* s4q6 */){
                        var _zM/* s4q7 */ = E(_zL/* s4q6 */);
                        if(!_zM/* s4q7 */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _zN/* s4q9 */ = _zM/* s4q7 */.b,
                          _zO/* s4qa */ = E(_zM/* s4q7 */.a),
                          _zP/* s4qe */ = E(_zO/* s4qa */.b),
                          _zQ/* s4qh */ = E(_zP/* s4qe */.a);
                          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _zQ/* s4qh */.a, _zQ/* s4qh */.b))){
                            _zJ/*  s4q6 */ = _zN/* s4q9 */;
                            return __continue/* EXTERNAL */;
                          }else{
                            if(!E(_zP/* s4qe */.b)._){
                              return new T2(1,_zO/* s4qa */.c,new T(function(){
                                return B(_zI/* s4q5 */(_zN/* s4q9 */));
                              }));
                            }else{
                              _zJ/*  s4q6 */ = _zN/* s4q9 */;
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_zJ/*  s4q6 */));
                      if(_zK/*  s4q5 */!=__continue/* EXTERNAL */){
                        return _zK/*  s4q5 */;
                      }
                    }
                  };
                  return B(_zI/* s4q5 */(_zi/* s4ph */));
                });
                return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, new T2(1,_zk/* s4pl */,_zH/* s4qo */), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
              }else{
                var _zR/* s4qv */ = function(_zS/*  s4qw */){
                  while(1){
                    var _zT/*  s4qv */ = B((function(_zU/* s4qw */){
                      var _zV/* s4qx */ = E(_zU/* s4qw */);
                      if(!_zV/* s4qx */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _zW/* s4qz */ = _zV/* s4qx */.b,
                        _zX/* s4qA */ = E(_zV/* s4qx */.a),
                        _zY/* s4qE */ = E(_zX/* s4qA */.b),
                        _zZ/* s4qH */ = E(_zY/* s4qE */.a);
                        if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _zZ/* s4qH */.a, _zZ/* s4qH */.b))){
                          _zS/*  s4qw */ = _zW/* s4qz */;
                          return __continue/* EXTERNAL */;
                        }else{
                          if(!E(_zY/* s4qE */.b)._){
                            return new T2(1,_zX/* s4qA */.c,new T(function(){
                              return B(_zR/* s4qv */(_zW/* s4qz */));
                            }));
                          }else{
                            _zS/*  s4qw */ = _zW/* s4qz */;
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_zS/*  s4qw */));
                    if(_zT/*  s4qv */!=__continue/* EXTERNAL */){
                      return _zT/*  s4qv */;
                    }
                  }
                };
                return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_zR/* s4qv */(_zi/* s4ph */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
              }
            }else{
              var _A0/* s4qT */ = _zG/* s4q3 */.a,
              _A1/* s4qU */ = E(_zr/* s4pu */);
              if(!_A1/* s4qU */._){
                var _A2/* s4qV */ = function(_A3/*  s4qW */){
                  while(1){
                    var _A4/*  s4qV */ = B((function(_A5/* s4qW */){
                      var _A6/* s4qX */ = E(_A5/* s4qW */);
                      if(!_A6/* s4qX */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _A7/* s4qZ */ = _A6/* s4qX */.b,
                        _A8/* s4r0 */ = E(_A6/* s4qX */.a),
                        _A9/* s4r4 */ = E(_A8/* s4r0 */.b),
                        _Aa/* s4r7 */ = E(_A9/* s4r4 */.a);
                        if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _Aa/* s4r7 */.a, _Aa/* s4r7 */.b))){
                          _A3/*  s4qW */ = _A7/* s4qZ */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _Ab/* s4rb */ = E(_A9/* s4r4 */.b);
                          if(!_Ab/* s4rb */._){
                            _A3/*  s4qW */ = _A7/* s4qZ */;
                            return __continue/* EXTERNAL */;
                          }else{
                            if(!B(_7/* GHC.Base.eqString */(_A0/* s4qT */, _Ab/* s4rb */.a))){
                              _A3/*  s4qW */ = _A7/* s4qZ */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,_A8/* s4r0 */.c,new T(function(){
                                return B(_A2/* s4qV */(_A7/* s4qZ */));
                              }));
                            }
                          }
                        }
                      }
                    })(_A3/*  s4qW */));
                    if(_A4/*  s4qV */!=__continue/* EXTERNAL */){
                      return _A4/*  s4qV */;
                    }
                  }
                };
                return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_A2/* s4qV */(_zi/* s4ph */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
              }else{
                if(!B(_7/* GHC.Base.eqString */(_A0/* s4qT */, _A1/* s4qU */.a))){
                  var _Ac/* s4rm */ = function(_Ad/*  s4rn */){
                    while(1){
                      var _Ae/*  s4rm */ = B((function(_Af/* s4rn */){
                        var _Ag/* s4ro */ = E(_Af/* s4rn */);
                        if(!_Ag/* s4ro */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _Ah/* s4rq */ = _Ag/* s4ro */.b,
                          _Ai/* s4rr */ = E(_Ag/* s4ro */.a),
                          _Aj/* s4rv */ = E(_Ai/* s4rr */.b),
                          _Ak/* s4ry */ = E(_Aj/* s4rv */.a);
                          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _Ak/* s4ry */.a, _Ak/* s4ry */.b))){
                            _Ad/*  s4rn */ = _Ah/* s4rq */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _Al/* s4rC */ = E(_Aj/* s4rv */.b);
                            if(!_Al/* s4rC */._){
                              _Ad/*  s4rn */ = _Ah/* s4rq */;
                              return __continue/* EXTERNAL */;
                            }else{
                              if(!B(_7/* GHC.Base.eqString */(_A0/* s4qT */, _Al/* s4rC */.a))){
                                _Ad/*  s4rn */ = _Ah/* s4rq */;
                                return __continue/* EXTERNAL */;
                              }else{
                                return new T2(1,_Ai/* s4rr */.c,new T(function(){
                                  return B(_Ac/* s4rm */(_Ah/* s4rq */));
                                }));
                              }
                            }
                          }
                        }
                      })(_Ad/*  s4rn */));
                      if(_Ae/*  s4rm */!=__continue/* EXTERNAL */){
                        return _Ae/*  s4rm */;
                      }
                    }
                  };
                  return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_Ac/* s4rm */(_zi/* s4ph */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
                }else{
                  var _Am/* s4s5 */ = new T(function(){
                    var _An/* s4rL */ = function(_Ao/*  s4rM */){
                      while(1){
                        var _Ap/*  s4rL */ = B((function(_Aq/* s4rM */){
                          var _Ar/* s4rN */ = E(_Aq/* s4rM */);
                          if(!_Ar/* s4rN */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _As/* s4rP */ = _Ar/* s4rN */.b,
                            _At/* s4rQ */ = E(_Ar/* s4rN */.a),
                            _Au/* s4rU */ = E(_At/* s4rQ */.b),
                            _Av/* s4rX */ = E(_Au/* s4rU */.a);
                            if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_zo/* s4pq */, _zp/* s4pr */, _Av/* s4rX */.a, _Av/* s4rX */.b))){
                              _Ao/*  s4rM */ = _As/* s4rP */;
                              return __continue/* EXTERNAL */;
                            }else{
                              var _Aw/* s4s1 */ = E(_Au/* s4rU */.b);
                              if(!_Aw/* s4s1 */._){
                                _Ao/*  s4rM */ = _As/* s4rP */;
                                return __continue/* EXTERNAL */;
                              }else{
                                if(!B(_7/* GHC.Base.eqString */(_A0/* s4qT */, _Aw/* s4s1 */.a))){
                                  _Ao/*  s4rM */ = _As/* s4rP */;
                                  return __continue/* EXTERNAL */;
                                }else{
                                  return new T2(1,_At/* s4rQ */.c,new T(function(){
                                    return B(_An/* s4rL */(_As/* s4rP */));
                                  }));
                                }
                              }
                            }
                          }
                        })(_Ao/*  s4rM */));
                        if(_Ap/*  s4rL */!=__continue/* EXTERNAL */){
                          return _Ap/*  s4rL */;
                        }
                      }
                    };
                    return B(_An/* s4rL */(_zi/* s4ph */));
                  });
                  return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, new T2(1,_zk/* s4pl */,_Am/* s4s5 */), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
                }
              }
            }
          }
        }
      },
      _Ax/* s4sb */ = function(_Ay/* s4sc */){
        var _Az/* s4sd */ = B(_5r/* Metamodel.UfoAInst.oumiGeneralizations_go */(_z8/* s4p0 */));
        if(!_Az/* s4sd */._){
          return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _4x/* GHC.Types.[] */, new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
        }else{
          var _AA/* s4sj */ = _Az/* s4sd */.b,
          _AB/* s4sk */ = E(_Az/* s4sd */.a),
          _AC/* s4sn */ = _AB/* s4sk */.c,
          _AD/* s4ss */ = E(E(_Ay/* s4sc */).b),
          _AE/* s4su */ = _AD/* s4ss */.b,
          _AF/* s4sv */ = E(_AD/* s4ss */.a),
          _AG/* s4sw */ = _AF/* s4sv */.a,
          _AH/* s4sx */ = _AF/* s4sv */.b,
          _AI/* s4sy */ = E(_AB/* s4sk */.b),
          _AJ/* s4sA */ = _AI/* s4sy */.b,
          _AK/* s4sB */ = E(_AI/* s4sy */.a);
          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _AK/* s4sB */.a, _AK/* s4sB */.b))){
            var _AL/* s4sF */ = function(_AM/*  s4sG */){
              while(1){
                var _AN/*  s4sF */ = B((function(_AO/* s4sG */){
                  var _AP/* s4sH */ = E(_AO/* s4sG */);
                  if(!_AP/* s4sH */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _AQ/* s4sJ */ = _AP/* s4sH */.b,
                    _AR/* s4sK */ = E(_AP/* s4sH */.a),
                    _AS/* s4sN */ = _AR/* s4sK */.c,
                    _AT/* s4sO */ = E(_AR/* s4sK */.b),
                    _AU/* s4sQ */ = _AT/* s4sO */.b,
                    _AV/* s4sR */ = E(_AT/* s4sO */.a);
                    if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _AV/* s4sR */.a, _AV/* s4sR */.b))){
                      _AM/*  s4sG */ = _AQ/* s4sJ */;
                      return __continue/* EXTERNAL */;
                    }else{
                      var _AW/* s4sV */ = E(_AE/* s4su */);
                      if(!_AW/* s4sV */._){
                        if(!E(_AU/* s4sQ */)._){
                          return new T2(1,_AS/* s4sN */,new T(function(){
                            return B(_AL/* s4sF */(_AQ/* s4sJ */));
                          }));
                        }else{
                          _AM/*  s4sG */ = _AQ/* s4sJ */;
                          return __continue/* EXTERNAL */;
                        }
                      }else{
                        var _AX/* s4t0 */ = E(_AU/* s4sQ */);
                        if(!_AX/* s4t0 */._){
                          _AM/*  s4sG */ = _AQ/* s4sJ */;
                          return __continue/* EXTERNAL */;
                        }else{
                          if(!B(_7/* GHC.Base.eqString */(_AW/* s4sV */.a, _AX/* s4t0 */.a))){
                            _AM/*  s4sG */ = _AQ/* s4sJ */;
                            return __continue/* EXTERNAL */;
                          }else{
                            return new T2(1,_AS/* s4sN */,new T(function(){
                              return B(_AL/* s4sF */(_AQ/* s4sJ */));
                            }));
                          }
                        }
                      }
                    }
                  }
                })(_AM/*  s4sG */));
                if(_AN/*  s4sF */!=__continue/* EXTERNAL */){
                  return _AN/*  s4sF */;
                }
              }
            };
            return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_AL/* s4sF */(_AA/* s4sj */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
          }else{
            var _AY/* s4t9 */ = E(_AE/* s4su */);
            if(!_AY/* s4t9 */._){
              if(!E(_AJ/* s4sA */)._){
                var _AZ/* s4tu */ = new T(function(){
                  var _B0/* s4tb */ = function(_B1/*  s4tc */){
                    while(1){
                      var _B2/*  s4tb */ = B((function(_B3/* s4tc */){
                        var _B4/* s4td */ = E(_B3/* s4tc */);
                        if(!_B4/* s4td */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _B5/* s4tf */ = _B4/* s4td */.b,
                          _B6/* s4tg */ = E(_B4/* s4td */.a),
                          _B7/* s4tk */ = E(_B6/* s4tg */.b),
                          _B8/* s4tn */ = E(_B7/* s4tk */.a);
                          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _B8/* s4tn */.a, _B8/* s4tn */.b))){
                            _B1/*  s4tc */ = _B5/* s4tf */;
                            return __continue/* EXTERNAL */;
                          }else{
                            if(!E(_B7/* s4tk */.b)._){
                              return new T2(1,_B6/* s4tg */.c,new T(function(){
                                return B(_B0/* s4tb */(_B5/* s4tf */));
                              }));
                            }else{
                              _B1/*  s4tc */ = _B5/* s4tf */;
                              return __continue/* EXTERNAL */;
                            }
                          }
                        }
                      })(_B1/*  s4tc */));
                      if(_B2/*  s4tb */!=__continue/* EXTERNAL */){
                        return _B2/*  s4tb */;
                      }
                    }
                  };
                  return B(_B0/* s4tb */(_AA/* s4sj */));
                });
                return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, new T2(1,_AC/* s4sn */,_AZ/* s4tu */), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
              }else{
                var _B9/* s4tB */ = function(_Ba/*  s4tC */){
                  while(1){
                    var _Bb/*  s4tB */ = B((function(_Bc/* s4tC */){
                      var _Bd/* s4tD */ = E(_Bc/* s4tC */);
                      if(!_Bd/* s4tD */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _Be/* s4tF */ = _Bd/* s4tD */.b,
                        _Bf/* s4tG */ = E(_Bd/* s4tD */.a),
                        _Bg/* s4tK */ = E(_Bf/* s4tG */.b),
                        _Bh/* s4tN */ = E(_Bg/* s4tK */.a);
                        if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _Bh/* s4tN */.a, _Bh/* s4tN */.b))){
                          _Ba/*  s4tC */ = _Be/* s4tF */;
                          return __continue/* EXTERNAL */;
                        }else{
                          if(!E(_Bg/* s4tK */.b)._){
                            return new T2(1,_Bf/* s4tG */.c,new T(function(){
                              return B(_B9/* s4tB */(_Be/* s4tF */));
                            }));
                          }else{
                            _Ba/*  s4tC */ = _Be/* s4tF */;
                            return __continue/* EXTERNAL */;
                          }
                        }
                      }
                    })(_Ba/*  s4tC */));
                    if(_Bb/*  s4tB */!=__continue/* EXTERNAL */){
                      return _Bb/*  s4tB */;
                    }
                  }
                };
                return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_B9/* s4tB */(_AA/* s4sj */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
              }
            }else{
              var _Bi/* s4tZ */ = _AY/* s4t9 */.a,
              _Bj/* s4u0 */ = E(_AJ/* s4sA */);
              if(!_Bj/* s4u0 */._){
                var _Bk/* s4u1 */ = function(_Bl/*  s4u2 */){
                  while(1){
                    var _Bm/*  s4u1 */ = B((function(_Bn/* s4u2 */){
                      var _Bo/* s4u3 */ = E(_Bn/* s4u2 */);
                      if(!_Bo/* s4u3 */._){
                        return __Z/* EXTERNAL */;
                      }else{
                        var _Bp/* s4u5 */ = _Bo/* s4u3 */.b,
                        _Bq/* s4u6 */ = E(_Bo/* s4u3 */.a),
                        _Br/* s4ua */ = E(_Bq/* s4u6 */.b),
                        _Bs/* s4ud */ = E(_Br/* s4ua */.a);
                        if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _Bs/* s4ud */.a, _Bs/* s4ud */.b))){
                          _Bl/*  s4u2 */ = _Bp/* s4u5 */;
                          return __continue/* EXTERNAL */;
                        }else{
                          var _Bt/* s4uh */ = E(_Br/* s4ua */.b);
                          if(!_Bt/* s4uh */._){
                            _Bl/*  s4u2 */ = _Bp/* s4u5 */;
                            return __continue/* EXTERNAL */;
                          }else{
                            if(!B(_7/* GHC.Base.eqString */(_Bi/* s4tZ */, _Bt/* s4uh */.a))){
                              _Bl/*  s4u2 */ = _Bp/* s4u5 */;
                              return __continue/* EXTERNAL */;
                            }else{
                              return new T2(1,_Bq/* s4u6 */.c,new T(function(){
                                return B(_Bk/* s4u1 */(_Bp/* s4u5 */));
                              }));
                            }
                          }
                        }
                      }
                    })(_Bl/*  s4u2 */));
                    if(_Bm/*  s4u1 */!=__continue/* EXTERNAL */){
                      return _Bm/*  s4u1 */;
                    }
                  }
                };
                return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_Bk/* s4u1 */(_AA/* s4sj */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
              }else{
                if(!B(_7/* GHC.Base.eqString */(_Bi/* s4tZ */, _Bj/* s4u0 */.a))){
                  var _Bu/* s4us */ = function(_Bv/*  s4ut */){
                    while(1){
                      var _Bw/*  s4us */ = B((function(_Bx/* s4ut */){
                        var _By/* s4uu */ = E(_Bx/* s4ut */);
                        if(!_By/* s4uu */._){
                          return __Z/* EXTERNAL */;
                        }else{
                          var _Bz/* s4uw */ = _By/* s4uu */.b,
                          _BA/* s4ux */ = E(_By/* s4uu */.a),
                          _BB/* s4uB */ = E(_BA/* s4ux */.b),
                          _BC/* s4uE */ = E(_BB/* s4uB */.a);
                          if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _BC/* s4uE */.a, _BC/* s4uE */.b))){
                            _Bv/*  s4ut */ = _Bz/* s4uw */;
                            return __continue/* EXTERNAL */;
                          }else{
                            var _BD/* s4uI */ = E(_BB/* s4uB */.b);
                            if(!_BD/* s4uI */._){
                              _Bv/*  s4ut */ = _Bz/* s4uw */;
                              return __continue/* EXTERNAL */;
                            }else{
                              if(!B(_7/* GHC.Base.eqString */(_Bi/* s4tZ */, _BD/* s4uI */.a))){
                                _Bv/*  s4ut */ = _Bz/* s4uw */;
                                return __continue/* EXTERNAL */;
                              }else{
                                return new T2(1,_BA/* s4ux */.c,new T(function(){
                                  return B(_Bu/* s4us */(_Bz/* s4uw */));
                                }));
                              }
                            }
                          }
                        }
                      })(_Bv/*  s4ut */));
                      if(_Bw/*  s4us */!=__continue/* EXTERNAL */){
                        return _Bw/*  s4us */;
                      }
                    }
                  };
                  return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, B(_Bu/* s4us */(_AA/* s4sj */)), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
                }else{
                  var _BE/* s4vb */ = new T(function(){
                    var _BF/* s4uR */ = function(_BG/*  s4uS */){
                      while(1){
                        var _BH/*  s4uR */ = B((function(_BI/* s4uS */){
                          var _BJ/* s4uT */ = E(_BI/* s4uS */);
                          if(!_BJ/* s4uT */._){
                            return __Z/* EXTERNAL */;
                          }else{
                            var _BK/* s4uV */ = _BJ/* s4uT */.b,
                            _BL/* s4uW */ = E(_BJ/* s4uT */.a),
                            _BM/* s4v0 */ = E(_BL/* s4uW */.b),
                            _BN/* s4v3 */ = E(_BM/* s4v0 */.a);
                            if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_AG/* s4sw */, _AH/* s4sx */, _BN/* s4v3 */.a, _BN/* s4v3 */.b))){
                              _BG/*  s4uS */ = _BK/* s4uV */;
                              return __continue/* EXTERNAL */;
                            }else{
                              var _BO/* s4v7 */ = E(_BM/* s4v0 */.b);
                              if(!_BO/* s4v7 */._){
                                _BG/*  s4uS */ = _BK/* s4uV */;
                                return __continue/* EXTERNAL */;
                              }else{
                                if(!B(_7/* GHC.Base.eqString */(_Bi/* s4tZ */, _BO/* s4v7 */.a))){
                                  _BG/*  s4uS */ = _BK/* s4uV */;
                                  return __continue/* EXTERNAL */;
                                }else{
                                  return new T2(1,_BL/* s4uW */.c,new T(function(){
                                    return B(_BF/* s4uR */(_BK/* s4uV */));
                                  }));
                                }
                              }
                            }
                          }
                        })(_BG/*  s4uS */));
                        if(_BH/*  s4uR */!=__continue/* EXTERNAL */){
                          return _BH/*  s4uR */;
                        }
                      }
                    };
                    return B(_BF/* s4uR */(_AA/* s4sj */));
                  });
                  return new F(function(){return _2G/* GHC.Base.++ */(B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, _za/* s4p2 */, B(_5Q/* Data.OldList.\\ */(_4w/* Metamodel.UfoAInst.$fEqOUElementInst */, new T2(1,_AC/* s4sn */,_BE/* s4vb */), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */))))), new T2(1,_zc/* s4p4 */,_4x/* GHC.Types.[] */));});
                }
              }
            }
          }
        }
      },
      _BP/* s4vh */ = B(_5r/* Metamodel.UfoAInst.oumiGeneralizations_go */(_z8/* s4p0 */));
      if(!_BP/* s4vh */._){
        return E(_z6/* Metamodel.UfoB.lvl3 */);
      }else{
        var _BQ/* s4vj */ = _BP/* s4vh */.b,
        _BR/* s4vk */ = E(_BP/* s4vh */.a),
        _BS/* s4vl */ = _BR/* s4vk */.a,
        _BT/* s4vm */ = _BR/* s4vk */.b,
        _BU/* s4vo */ = E(_BR/* s4vk */.c),
        _BV/* s4vr */ = E(_BU/* s4vo */.a),
        _BW/* s4vu */ = E(_zc/* s4p4 */),
        _BX/* s4vw */ = _BW/* s4vu */.b,
        _BY/* s4vx */ = E(_BW/* s4vu */.a),
        _BZ/* s4vy */ = _BY/* s4vx */.a,
        _C0/* s4vz */ = _BY/* s4vx */.b;
        if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_BV/* s4vr */.a, _BV/* s4vr */.b, _BZ/* s4vy */, _C0/* s4vz */))){
          var _C1/* s4vB */ = function(_C2/* s4vC */){
            while(1){
              var _C3/* s4vD */ = E(_C2/* s4vC */);
              if(!_C3/* s4vD */._){
                return __Z/* EXTERNAL */;
              }else{
                var _C4/* s4vF */ = _C3/* s4vD */.b,
                _C5/* s4vG */ = E(_C3/* s4vD */.a),
                _C6/* s4vK */ = E(_C5/* s4vG */.c),
                _C7/* s4vN */ = E(_C6/* s4vK */.a);
                if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_C7/* s4vN */.a, _C7/* s4vN */.b, _BZ/* s4vy */, _C0/* s4vz */))){
                  _C2/* s4vC */ = _C4/* s4vF */;
                  continue;
                }else{
                  var _C8/* s4vR */ = E(_C6/* s4vK */.b);
                  if(!_C8/* s4vR */._){
                    if(!E(_BX/* s4vw */)._){
                      return new T1(1,_C5/* s4vG */);
                    }else{
                      _C2/* s4vC */ = _C4/* s4vF */;
                      continue;
                    }
                  }else{
                    var _C9/* s4vV */ = E(_BX/* s4vw */);
                    if(!_C9/* s4vV */._){
                      _C2/* s4vC */ = _C4/* s4vF */;
                      continue;
                    }else{
                      if(!B(_7/* GHC.Base.eqString */(_C8/* s4vR */.a, _C9/* s4vV */.a))){
                        _C2/* s4vC */ = _C4/* s4vF */;
                        continue;
                      }else{
                        return new T1(1,_C5/* s4vG */);
                      }
                    }
                  }
                }
              }
            }
          },
          _Ca/* s4vY */ = B(_C1/* s4vB */(_BQ/* s4vj */));
          if(!_Ca/* s4vY */._){
            return E(_z6/* Metamodel.UfoB.lvl3 */);
          }else{
            return new F(function(){return _Ax/* s4sb */(_Ca/* s4vY */.a);});
          }
        }else{
          var _Cb/* s4w0 */ = E(_BU/* s4vo */.b);
          if(!_Cb/* s4w0 */._){
            var _Cc/* s4w1 */ = E(_BX/* s4vw */);
            if(!_Cc/* s4w1 */._){
              return new F(function(){return _zd/* s4p7 */(_BS/* s4vl */, _BT/* s4vm */, _BU/* s4vo */);});
            }else{
              var _Cd/* s4w3 */ = function(_Ce/* s4w4 */){
                while(1){
                  var _Cf/* s4w5 */ = E(_Ce/* s4w4 */);
                  if(!_Cf/* s4w5 */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _Cg/* s4w7 */ = _Cf/* s4w5 */.b,
                    _Ch/* s4w8 */ = E(_Cf/* s4w5 */.a),
                    _Ci/* s4wc */ = E(_Ch/* s4w8 */.c),
                    _Cj/* s4wf */ = E(_Ci/* s4wc */.a);
                    if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_Cj/* s4wf */.a, _Cj/* s4wf */.b, _BZ/* s4vy */, _C0/* s4vz */))){
                      _Ce/* s4w4 */ = _Cg/* s4w7 */;
                      continue;
                    }else{
                      var _Ck/* s4wj */ = E(_Ci/* s4wc */.b);
                      if(!_Ck/* s4wj */._){
                        _Ce/* s4w4 */ = _Cg/* s4w7 */;
                        continue;
                      }else{
                        if(!B(_7/* GHC.Base.eqString */(_Ck/* s4wj */.a, _Cc/* s4w1 */.a))){
                          _Ce/* s4w4 */ = _Cg/* s4w7 */;
                          continue;
                        }else{
                          return new T1(1,_Ch/* s4w8 */);
                        }
                      }
                    }
                  }
                }
              },
              _Cl/* s4wm */ = B(_Cd/* s4w3 */(_BQ/* s4vj */));
              if(!_Cl/* s4wm */._){
                return E(_z6/* Metamodel.UfoB.lvl3 */);
              }else{
                return new F(function(){return _Ax/* s4sb */(_Cl/* s4wm */.a);});
              }
            }
          }else{
            var _Cm/* s4wp */ = E(_BX/* s4vw */);
            if(!_Cm/* s4wp */._){
              var _Cn/* s4wq */ = function(_Co/* s4wr */){
                while(1){
                  var _Cp/* s4ws */ = E(_Co/* s4wr */);
                  if(!_Cp/* s4ws */._){
                    return __Z/* EXTERNAL */;
                  }else{
                    var _Cq/* s4wu */ = _Cp/* s4ws */.b,
                    _Cr/* s4wv */ = E(_Cp/* s4ws */.a),
                    _Cs/* s4wz */ = E(_Cr/* s4wv */.c),
                    _Ct/* s4wC */ = E(_Cs/* s4wz */.a);
                    if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_Ct/* s4wC */.a, _Ct/* s4wC */.b, _BZ/* s4vy */, _C0/* s4vz */))){
                      _Co/* s4wr */ = _Cq/* s4wu */;
                      continue;
                    }else{
                      if(!E(_Cs/* s4wz */.b)._){
                        return new T1(1,_Cr/* s4wv */);
                      }else{
                        _Co/* s4wr */ = _Cq/* s4wu */;
                        continue;
                      }
                    }
                  }
                }
              },
              _Cu/* s4wI */ = B(_Cn/* s4wq */(_BQ/* s4vj */));
              if(!_Cu/* s4wI */._){
                return E(_z6/* Metamodel.UfoB.lvl3 */);
              }else{
                return new F(function(){return _Ax/* s4sb */(_Cu/* s4wI */.a);});
              }
            }else{
              var _Cv/* s4wK */ = _Cm/* s4wp */.a;
              if(!B(_7/* GHC.Base.eqString */(_Cb/* s4w0 */.a, _Cv/* s4wK */))){
                var _Cw/* s4wM */ = function(_Cx/* s4wN */){
                  while(1){
                    var _Cy/* s4wO */ = E(_Cx/* s4wN */);
                    if(!_Cy/* s4wO */._){
                      return __Z/* EXTERNAL */;
                    }else{
                      var _Cz/* s4wQ */ = _Cy/* s4wO */.b,
                      _CA/* s4wR */ = E(_Cy/* s4wO */.a),
                      _CB/* s4wV */ = E(_CA/* s4wR */.c),
                      _CC/* s4wY */ = E(_CB/* s4wV */.a);
                      if(!B(_c/* Metamodel.UfoA.$w$c==2 */(_CC/* s4wY */.a, _CC/* s4wY */.b, _BZ/* s4vy */, _C0/* s4vz */))){
                        _Cx/* s4wN */ = _Cz/* s4wQ */;
                        continue;
                      }else{
                        var _CD/* s4x2 */ = E(_CB/* s4wV */.b);
                        if(!_CD/* s4x2 */._){
                          _Cx/* s4wN */ = _Cz/* s4wQ */;
                          continue;
                        }else{
                          if(!B(_7/* GHC.Base.eqString */(_CD/* s4x2 */.a, _Cv/* s4wK */))){
                            _Cx/* s4wN */ = _Cz/* s4wQ */;
                            continue;
                          }else{
                            return new T1(1,_CA/* s4wR */);
                          }
                        }
                      }
                    }
                  }
                },
                _CE/* s4x5 */ = B(_Cw/* s4wM */(_BQ/* s4vj */));
                if(!_CE/* s4x5 */._){
                  return E(_z6/* Metamodel.UfoB.lvl3 */);
                }else{
                  return new F(function(){return _Ax/* s4sb */(_CE/* s4x5 */.a);});
                }
              }else{
                return new F(function(){return _zd/* s4p7 */(_BS/* s4vl */, _BT/* s4vm */, _BU/* s4vo */);});
              }
            }
          }
        }
      }
    }else{
      return E(_z4/* Metamodel.UfoB.lvl1 */);
    }
  }
},
_CF/* lvl12 */ = new T2(1,_uz/* Model.Elements.iDonorTempDeferred */,_4x/* GHC.Types.[] */),
_CG/* lvl13 */ = function(_CH/* s6LT */){
  return new F(function(){return _z7/* Metamodel.UfoB.$wswitchPhase */(_wc/* Model.Models.sPermDeferred5 */, _CF/* Model.Models.lvl12 */, _CH/* s6LT */);});
},
_CI/* a4 */ = new T2(1,_CG/* Model.Models.lvl13 */,_4x/* GHC.Types.[] */),
_CJ/* a5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nTemporary\nDeferred"));
}),
_CK/* lvl1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent\nDonation Required"));
}),
_CL/* lvl2 */ = new T1(1,_CK/* Model.Models.lvl1 */),
_CM/* lvl */ = new T(function(){
  return B(unCStr/* EXTERNAL */("DLI\nExamination"));
}),
_CN/* sChosen6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection Dates Assessment\nfor Subsequent Donation"));
}),
_CO/* sScheduled6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Schedule\nand Specification\nof Donor Clearance"));
}),
_CP/* eFitnessDetermination1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Fitness\nDetermination"));
}),
_CQ/* eFitnessDetermination */ = new T2(0,_9g/* Metamodel.UfoA.Relator */,_CP/* Model.Elements.eFitnessDetermination1 */),
_CR/* iFitnessDetermination */ = new T2(0,_CQ/* Model.Elements.eFitnessDetermination */,_8S/* GHC.Base.Nothing */),
_CS/* sDraw15 */ = new T2(1,_ag/* Model.Elements.iBloodSample */,_4x/* GHC.Types.[] */),
_CT/* lvl15 */ = new T2(1,_b4/* Model.Elements.iBloodSampleDraw4 */,_CS/* Model.Models.sDraw15 */),
_CU/* lvl16 */ = new T2(1,_hz/* Model.Elements.iInfectionMarkersW */,_CT/* Model.Models.lvl15 */),
_CV/* lvl17 */ = new T2(1,_aM/* Model.Elements.iInfectionTestingW */,_CU/* Model.Models.lvl16 */),
_CW/* lvl18 */ = new T2(1,_if/* Model.Elements.iMedicalAssessmentResults3 */,_CV/* Model.Models.lvl17 */),
_CX/* lvl19 */ = new T2(1,_aX/* Model.Elements.iMedicalAssessment3 */,_CW/* Model.Models.lvl18 */),
_CY/* lvl20 */ = new T2(1,_jh/* Model.Elements.iDonorClearance */,_CX/* Model.Models.lvl19 */),
_CZ/* lvl21 */ = new T2(1,_hJ/* Model.Elements.iDonorCleared */,_CY/* Model.Models.lvl20 */),
_D0/* lvl22 */ = new T2(1,_CR/* Model.Elements.iFitnessDetermination */,_CZ/* Model.Models.lvl21 */),
_D1/* lvl23 */ = new T2(1,_aP/* Model.Elements.iWorkup */,_D0/* Model.Models.lvl22 */),
_D2/* lvl24 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_D1/* Model.Models.lvl23 */, _D3/* B1 */);});
},
_D4/* a */ = new T2(1,_D2/* Model.Models.lvl24 */,_4x/* GHC.Types.[] */),
_D5/* a1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Clearance\nfor Subsequent Donation\nProvided"));
}),
_D6/* lvl14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent\nCollection of HSC\nApproval"));
}),
_D7/* sApproved6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent HSC\nCollection"));
}),
_D8/* sDLICollected6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nMonitoring"));
}),
_D9/* sDLICollected5 */ = new T2(0,_D8/* Model.Models.sDLICollected6 */,_yX/* Model.Models.sMonitored */),
_Da/* sDLICollected4 */ = new T2(1,_D9/* Model.Models.sDLICollected5 */,_4x/* GHC.Types.[] */),
_Db/* sDLICollected3 */ = new T2(0,_8S/* GHC.Base.Nothing */,_Da/* Model.Models.sDLICollected4 */),
_Dc/* sDLICollected2 */ = new T2(1,_Db/* Model.Models.sDLICollected3 */,_4x/* GHC.Types.[] */),
_Dd/* sDLICollected10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subseqent HSC\nTransportation"));
}),
_De/* sDLIDelivered5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subseqent HSC\nTransplantation"));
}),
_Df/* sDLIDelivered4 */ = new T(function(){
  return new T2(0,_De/* Model.Models.sDLIDelivered5 */,_Dg/* Model.Models.sDLITransplanted */);
}),
_Dh/* sDLIDelivered3 */ = new T(function(){
  return new T2(1,_Df/* Model.Models.sDLIDelivered4 */,_4x/* GHC.Types.[] */);
}),
_Di/* sDLIDelivered2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_Dh/* Model.Models.sDLIDelivered3 */);
}),
_Dj/* sDLIDelivered1 */ = new T(function(){
  return new T2(1,_Di/* Model.Models.sDLIDelivered2 */,_4x/* GHC.Types.[] */);
}),
_Dk/* sDLIDelivered10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subseqent HSC\nDelivered"));
}),
_Dl/* sDLIDelivered9 */ = new T2(1,_nm/* Model.Elements.iDeliveredDLI */,_4x/* GHC.Types.[] */),
_Dm/* sDLIDelivered8 */ = new T2(1,_cj/* Model.Elements.iTransportDLI */,_Dl/* Model.Models.sDLIDelivered9 */),
_Dn/* sDLIDelivered7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Dm/* Model.Models.sDLIDelivered8 */, _D3/* B1 */);});
},
_Do/* sDLIDelivered6 */ = new T2(1,_Dn/* Model.Models.sDLIDelivered7 */,_4x/* GHC.Types.[] */),
_Dp/* sDLIDelivered */ = new T(function(){
  return new T4(0,_Dk/* Model.Models.sDLIDelivered10 */,_Do/* Model.Models.sDLIDelivered6 */,_Dj/* Model.Models.sDLIDelivered1 */,_4x/* GHC.Types.[] */);
}),
_Dq/* sDLICollected9 */ = new T(function(){
  return new T2(0,_Dd/* Model.Models.sDLICollected10 */,_Dp/* Model.Models.sDLIDelivered */);
}),
_Dr/* sDLICollected8 */ = new T(function(){
  return new T2(1,_Dq/* Model.Models.sDLICollected9 */,_4x/* GHC.Types.[] */);
}),
_Ds/* sDLICollected7 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_Dr/* Model.Models.sDLICollected8 */);
}),
_Dt/* sDLICollected1 */ = new T(function(){
  return new T2(1,_Ds/* Model.Models.sDLICollected7 */,_Dc/* Model.Models.sDLICollected2 */);
}),
_Du/* sDLICollected18 */ = new T2(1,_kg/* Model.Elements.iCollectedDLI */,_4x/* GHC.Types.[] */),
_Dv/* sDLICollected17 */ = new T2(1,_bV/* Model.Elements.iDLICollection */,_Du/* Model.Models.sDLICollected18 */),
_Dw/* sDLICollected16 */ = new T2(1,_bs/* Model.Elements.iCollection */,_Dv/* Model.Models.sDLICollected17 */),
_Dx/* sDLICollected15 */ = new T2(1,_bY/* Model.Elements.iDLIDonation */,_Dw/* Model.Models.sDLICollected16 */),
_Dy/* sDLICollected14 */ = new T2(1,_lD/* Model.Elements.iDonorSubsequent */,_Dx/* Model.Models.sDLICollected15 */),
_Dz/* sDLICollected13 */ = new T2(1,_bv/* Model.Elements.iDonation */,_Dy/* Model.Models.sDLICollected14 */),
_DA/* sDLICollected12 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Dz/* Model.Models.sDLICollected13 */, _D3/* B1 */);});
},
_DB/* sDLICollected11 */ = new T2(1,_DA/* Model.Models.sDLICollected12 */,_4x/* GHC.Types.[] */),
_DC/* sDLICollected19 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subseqent HSC\nCollected"));
}),
_DD/* sDLICollected */ = new T(function(){
  return new T4(0,_DC/* Model.Models.sDLICollected19 */,_DB/* Model.Models.sDLICollected11 */,_Dt/* Model.Models.sDLICollected1 */,_4x/* GHC.Types.[] */);
}),
_DE/* sApproved5 */ = new T(function(){
  return new T2(0,_D7/* Model.Models.sApproved6 */,_DD/* Model.Models.sDLICollected */);
}),
_DF/* sApproved4 */ = new T(function(){
  return new T2(1,_DE/* Model.Models.sApproved5 */,_4x/* GHC.Types.[] */);
}),
_DG/* sApproved3 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_DF/* Model.Models.sApproved4 */);
}),
_DH/* sApproved1 */ = new T(function(){
  return new T2(1,_DG/* Model.Models.sApproved3 */,_4x/* GHC.Types.[] */);
}),
_DI/* sApproved11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent Collection\nof HSC Approved"));
}),
_DJ/* sApproved10 */ = new T2(1,_bY/* Model.Elements.iDLIDonation */,_4x/* GHC.Types.[] */),
_DK/* sApproved9 */ = new T2(1,_lD/* Model.Elements.iDonorSubsequent */,_DJ/* Model.Models.sApproved10 */),
_DL/* sApproved8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_DK/* Model.Models.sApproved9 */, _D3/* B1 */);});
},
_DM/* sApproved7 */ = new T2(1,_DL/* Model.Models.sApproved8 */,_4x/* GHC.Types.[] */),
_DN/* sApproved2 */ = new T(function(){
  return new T4(0,_DI/* Model.Models.sApproved11 */,_DM/* Model.Models.sApproved7 */,_DH/* Model.Models.sApproved1 */,_4x/* GHC.Types.[] */);
}),
_DO/* lvl82 */ = new T(function(){
  return new T2(0,_D6/* Model.Models.lvl14 */,_DN/* Model.Models.sApproved2 */);
}),
_DP/* lvl83 */ = new T(function(){
  return new T2(1,_DO/* Model.Models.lvl82 */,_4x/* GHC.Types.[] */);
}),
_DQ/* lvl84 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_DP/* Model.Models.lvl83 */);
}),
_DR/* a11 */ = new T(function(){
  return new T2(1,_DQ/* Model.Models.lvl84 */,_4x/* GHC.Types.[] */);
}),
_DS/* sWorkup2 */ = new T(function(){
  return new T4(0,_D5/* Model.Models.a1 */,_D4/* Model.Models.a */,_DR/* Model.Models.a11 */,_4x/* GHC.Types.[] */);
}),
_DT/* sScheduled5 */ = new T(function(){
  return new T2(0,_CO/* Model.Models.sScheduled6 */,_DS/* Model.Models.sWorkup2 */);
}),
_DU/* sScheduled4 */ = new T(function(){
  return new T2(1,_DT/* Model.Models.sScheduled5 */,_4x/* GHC.Types.[] */);
}),
_DV/* sScheduled3 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_DU/* Model.Models.sScheduled4 */);
}),
_DW/* sScheduled1 */ = new T(function(){
  return new T2(1,_DV/* Model.Models.sScheduled3 */,_4x/* GHC.Types.[] */);
}),
_DX/* sScheduled11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Schedule and Specification\nof Donor Clearance\nfor Subsequent Donation\nAssigned"));
}),
_DY/* sScheduled10 */ = new T2(1,_lt/* Model.Elements.iSpecification */,_4x/* GHC.Types.[] */),
_DZ/* sScheduled9 */ = new T2(1,_lm/* Model.Elements.iSchedule */,_DY/* Model.Models.sScheduled10 */),
_E0/* sScheduled8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_DZ/* Model.Models.sScheduled9 */, _D3/* B1 */);});
},
_E1/* sScheduled7 */ = new T2(1,_E0/* Model.Models.sScheduled8 */,_4x/* GHC.Types.[] */),
_E2/* sScheduled2 */ = new T(function(){
  return new T4(0,_DX/* Model.Models.sScheduled11 */,_E1/* Model.Models.sScheduled7 */,_DW/* Model.Models.sScheduled1 */,_4x/* GHC.Types.[] */);
}),
_E3/* sChosen5 */ = new T(function(){
  return new T2(0,_CN/* Model.Models.sChosen6 */,_E2/* Model.Models.sScheduled2 */);
}),
_E4/* sChosen4 */ = new T(function(){
  return new T2(1,_E3/* Model.Models.sChosen5 */,_4x/* GHC.Types.[] */);
}),
_E5/* sChosen3 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_E4/* Model.Models.sChosen4 */);
}),
_E6/* sChosen1 */ = new T(function(){
  return new T2(1,_E5/* Model.Models.sChosen3 */,_4x/* GHC.Types.[] */);
}),
_E7/* sChosen11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Chosen\nfor Subsequent Donation"));
}),
_E8/* sChosen10 */ = new T2(1,_iw/* Model.Elements.iDonorSelection */,_4x/* GHC.Types.[] */),
_E9/* sChosen9 */ = new T2(1,_iv/* Model.Elements.iDonorChosen */,_E8/* Model.Models.sChosen10 */),
_Ea/* sChosen8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_E9/* Model.Models.sChosen9 */, _D3/* B1 */);});
},
_Eb/* sChosen7 */ = new T2(1,_Ea/* Model.Models.sChosen8 */,_4x/* GHC.Types.[] */),
_Ec/* sChosen2 */ = new T(function(){
  return new T4(0,_E7/* Model.Models.sChosen11 */,_Eb/* Model.Models.sChosen7 */,_E6/* Model.Models.sChosen1 */,_4x/* GHC.Types.[] */);
}),
_Ed/* sExam6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Performing\na Subsequent\nDonation"));
}),
_Ee/* sExam5 */ = new T(function(){
  return new T2(0,_Ed/* Model.Models.sExam6 */,_Ec/* Model.Models.sChosen2 */);
}),
_Ef/* sExam3 */ = new T(function(){
  return new T2(1,_Ee/* Model.Models.sExam5 */,_4x/* GHC.Types.[] */);
}),
_Eg/* sExam2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_Ef/* Model.Models.sExam3 */);
}),
_Eh/* sExam1 */ = new T(function(){
  return new T2(1,_Eg/* Model.Models.sExam2 */,_4x/* GHC.Types.[] */);
}),
_Ei/* sExam8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_4x/* GHC.Types.[] */, _D3/* B1 */);});
},
_Ej/* sExam7 */ = new T2(1,_Ei/* Model.Models.sExam8 */,_4x/* GHC.Types.[] */),
_Ek/* sExam9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent HSC\nExamination Completed"));
}),
_El/* sExam4 */ = new T(function(){
  return new T4(0,_Ek/* Model.Models.sExam9 */,_Ej/* Model.Models.sExam7 */,_Eh/* Model.Models.sExam1 */,_4x/* GHC.Types.[] */);
}),
_Em/* lvl58 */ = new T(function(){
  return new T2(0,_CM/* Model.Models.lvl */,_El/* Model.Models.sExam4 */);
}),
_En/* lvl59 */ = new T(function(){
  return new T2(1,_Em/* Model.Models.lvl58 */,_4x/* GHC.Types.[] */);
}),
_Eo/* lvl60 */ = new T(function(){
  return new T2(0,_CL/* Model.Models.lvl2 */,_En/* Model.Models.lvl59 */);
}),
_Ep/* lvl61 */ = new T(function(){
  return new T2(1,_Eo/* Model.Models.lvl60 */,_4x/* GHC.Types.[] */);
}),
_Eq/* lvl3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("nDonor\nBecomes\nPermanently\nUnavailable"));
}),
_Er/* sPermDeferred1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reasons for making a Donor Permanently Deferred are a.o. Donor\'s request, age, medical reasons, death or realizing two transplantations."));
}),
_Es/* sPermDeferred4 */ = new T2(1,_uB/* Model.Elements.iDonorPermDeferred */,_4x/* GHC.Types.[] */),
_Et/* sPermDeferred3 */ = function(_Eu/* s6LS */){
  return new F(function(){return _z7/* Metamodel.UfoB.$wswitchPhase */(_wc/* Model.Models.sPermDeferred5 */, _Es/* Model.Models.sPermDeferred4 */, _Eu/* s6LS */);});
},
_Ev/* sPermDeferred2 */ = new T2(1,_Et/* Model.Models.sPermDeferred3 */,_4x/* GHC.Types.[] */),
_Ew/* sPermDeferred96 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nPermanently\nDeferred"));
}),
_Ex/* sPermDeferred */ = new T4(0,_Ew/* Model.Models.sPermDeferred96 */,_Ev/* Model.Models.sPermDeferred2 */,_4x/* GHC.Types.[] */,_Er/* Model.Models.sPermDeferred1 */),
_Ey/* lvl4 */ = new T2(0,_Eq/* Model.Models.lvl3 */,_Ex/* Model.Models.sPermDeferred */),
_Ez/* lvl5 */ = new T2(1,_Ey/* Model.Models.lvl4 */,_4x/* GHC.Types.[] */),
_EA/* lvl6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reasons\nBecome\nPermanent"));
}),
_EB/* lvl7 */ = new T1(1,_EA/* Model.Models.lvl6 */),
_EC/* lvl8 */ = new T2(0,_EB/* Model.Models.lvl7 */,_Ez/* Model.Models.lvl5 */),
_ED/* lvl62 */ = new T(function(){
  return new T2(1,_EC/* Model.Models.lvl8 */,_Ep/* Model.Models.lvl61 */);
}),
_EE/* lvl10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reasons\nGone"));
}),
_EF/* lvl11 */ = new T1(1,_EE/* Model.Models.lvl10 */),
_EG/* lvl9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nBecomes\nAvailable"));
}),
_EH/* lvl35 */ = new T2(1,_ux/* Model.Elements.iDonorAvailable */,_4x/* GHC.Types.[] */),
_EI/* lvl36 */ = function(_EJ/* s6LU */){
  return new F(function(){return _z7/* Metamodel.UfoB.$wswitchPhase */(_wc/* Model.Models.sPermDeferred5 */, _EH/* Model.Models.lvl35 */, _EJ/* s6LU */);});
},
_EK/* lvl37 */ = new T2(1,_EI/* Model.Models.lvl36 */,_4x/* GHC.Types.[] */),
_EL/* lvl38 */ = new T2(1,_fr/* Model.Elements.iGenotypeEvaluated1 */,_4x/* GHC.Types.[] */),
_EM/* lvl39 */ = function(_EN/* s6LV */){
  return new F(function(){return _z7/* Metamodel.UfoB.$wswitchPhase */(_wc/* Model.Models.sPermDeferred5 */, _EL/* Model.Models.lvl38 */, _EN/* s6LV */);});
},
_EO/* lvl40 */ = new T2(1,_EM/* Model.Models.lvl39 */,_EK/* Model.Models.lvl37 */),
_EP/* sHLAVerified14 */ = new T2(1,_eP/* Model.Elements.iGenotypeValue1 */,_4x/* GHC.Types.[] */),
_EQ/* lvl41 */ = new T2(1,_eA/* Model.Elements.iDonorsTypingResults1 */,_EP/* Model.Models.sHLAVerified14 */),
_ER/* lvl42 */ = new T2(1,_dy/* Model.Elements.iGeneAssignment1 */,_EQ/* Model.Models.lvl41 */),
_ES/* lvl43 */ = new T2(1,_gw/* Model.Elements.iHLATyping1 */,_ER/* Model.Models.lvl42 */),
_ET/* lvl44 */ = new T2(1,_aj/* Model.Elements.iDNASample */,_ES/* Model.Models.lvl43 */),
_EU/* lvl45 */ = new T2(1,_dT/* Model.Elements.iDNAIsolation1 */,_ET/* Model.Models.lvl44 */),
_EV/* lvl46 */ = new T2(1,_dv/* Model.Elements.iDonor */,_EU/* Model.Models.lvl45 */),
_EW/* lvl47 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_EV/* Model.Models.lvl46 */, _D3/* B1 */);});
},
_EX/* a6 */ = new T2(1,_EW/* Model.Models.lvl47 */,_EO/* Model.Models.lvl40 */),
_EY/* a7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Available"));
}),
_EZ/* lvl25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nBecomes\nPermanently\nUnavailable"));
}),
_F0/* lvl26 */ = new T2(0,_EZ/* Model.Models.lvl25 */,_Ex/* Model.Models.sPermDeferred */),
_F1/* lvl27 */ = new T2(1,_F0/* Model.Models.lvl26 */,_4x/* GHC.Types.[] */),
_F2/* lvl28 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Permament\nUnavailability\nReasons\nOccured"));
}),
_F3/* lvl29 */ = new T1(1,_F2/* Model.Models.lvl28 */),
_F4/* lvl30 */ = new T2(0,_F3/* Model.Models.lvl29 */,_F1/* Model.Models.lvl27 */),
_F5/* lvl31 */ = new T2(1,_F4/* Model.Models.lvl30 */,_4x/* GHC.Types.[] */),
_F6/* lvl33 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Unavailability\nReasons\nOccured"));
}),
_F7/* lvl34 */ = new T1(1,_F6/* Model.Models.lvl33 */),
_F8/* lvl70 */ = new T(function(){
  return new T2(0,_F7/* Model.Models.lvl34 */,_F9/* Model.Models.sDLITransplanted3 */);
}),
_Fa/* lvl71 */ = new T(function(){
  return new T2(1,_F8/* Model.Models.lvl70 */,_F5/* Model.Models.lvl31 */);
}),
_Fb/* sFound20 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended\nHLA\nTyping\nfrom Stored\nBlood Sample"));
}),
_Fc/* sHLATyped2T */ = new T(function(){
  return B(unCStr/* EXTERNAL */("A not-fully high-resolution-HLA-typed Donor (allele level for all HLA genes) may be asked for an Extended Examination for an Extended Typing.<br/><em>WMDA Definition:</em><br/>Extended Typing - This HLA typing includes the tests carried out on a specific donor/cord blood unit with the purpose of adding additional information (typing of additional loci or further subtyping at a higher resolution) to an existing HLA assignment. The purpose of this typing is to ascertain the level of HLA match between donor and recipient. The additional HLA typing may be performed on a stored sample.<br/><em>WMDA Standard:</em><br/>3.10 Volunteer donors must be counselled when selected for further tests and when selected as a donor for a specific patient.<br/>3.10.1 Counselling for volunteer donors selected for specific patients must include anonymity of the donor and patient, requirement for further blood samples before donation, requirement for infectious disease and other testing, risk of donation, possible duration of loss of time from normal activities, location of the collection, the potential for collection of autologous blood, donor\u2019s right to withdraw and consequences for the patient, details of insurance coverage, possible subsequent donations of HSC or cellular products, alternative collection methods and whether blood or other biological material is reserved for research purposes.<br/>10.03 The registry must have available a clear fee schedule detailing payment terms for extended and verification HLA testing, infectious disease marker testing, procurement and other related services upon request.<br/>10.03.1 The registry should have a procedure to communicate changes in the fee schedule to interested parties thirty (30) days prior to implementation.<br/>10.04 Any cost not standardised or, for any reason, not accessible through such a schedule should be estimated and communicated in advance to the requesting registry and/or transplant centre.<br/>"));
}),
_Fd/* sHLATyped2_15 */ = new T2(1,_em/* Model.Elements.iGeneAssignment2 */,_EP/* Model.Models.sHLAVerified14 */),
_Fe/* sHLATyped2_14 */ = new T2(1,_eE/* Model.Elements.iDonorsTypingResults2 */,_Fd/* Model.Models.sHLATyped2_15 */),
_Ff/* sHLATyped2_13 */ = new T2(1,_gA/* Model.Elements.iHLATyping2 */,_Fe/* Model.Models.sHLATyped2_14 */),
_Fg/* sHLATyped2_12 */ = new T2(1,_dX/* Model.Elements.iDNAIsolation2 */,_Ff/* Model.Models.sHLATyped2_13 */),
_Fh/* sHLATyped2_11 */ = new T2(1,_aj/* Model.Elements.iDNASample */,_Fg/* Model.Models.sHLATyped2_12 */),
_Fi/* sHLATyped2_23 */ = new T2(1,_sI/* Model.Elements.iBSStored */,_Fh/* Model.Models.sHLATyped2_11 */),
_Fj/* sHLATyped2_22 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Fi/* Model.Models.sHLATyped2_23 */, _D3/* B1 */);});
},
_Fk/* sHLATyped2_21 */ = new T2(1,_Fj/* Model.Models.sHLATyped2_22 */,_4x/* GHC.Types.[] */),
_Fl/* sHLATyped2_24 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nExtended\nHLA Typed\n from a Stored\nBlood Sample"));
}),
_Fm/* sHLATyped2_8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donors Selection\nfor Verification"));
}),
_Fn/* sBloodSampleDrawnT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("The Donor Centre also creates a record of the particular Blood Sample and a request for an HLA typing. Donor blood sample is sent to HLA laboratory for histocompatibility testing.<br/><em>WMDA Definition:</em><br/>Testing laboratories - These laboratories perform the histocompatibility, blood group, infectious disease, and other testing of the prospective donors and patients. They may be under the direction of a registry, donor centre or transplant centre or may be separate from these entities.<br/><em>WMDA Standard:</em><br/>3.13 Valid signed informed consent must be obtained if donor blood or other biological material or information is stored and/or used for the purpose of an ethically approved research project.<br/>3.17 Testing must be carried out by laboratories that meet standards established by the government or prevailing in the relevant community for performing these services.<br/>3.17.1 Testing must be carried out in a manner to ensure the accuracy of the data."));
}),
_Fo/* sDraw11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Histocompatibility Confirmation"));
}),
_Fp/* mEventB1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("<<merging>>\n"));
}),
_Fq/* sHLAVerified6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Making Donor Verified"));
}),
_Fr/* sHLAVerified5 */ = new T(function(){
  return B(_2G/* GHC.Base.++ */(_Fp/* Metamodel.UfoB.mEventB1 */, _Fq/* Model.Models.sHLAVerified6 */));
}),
_Fs/* sApproved23 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Selection\nfor Transplantation"));
}),
_Ft/* sApproved19 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection Dates Assessment"));
}),
_Fu/* lvl56 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nEligible"));
}),
_Fv/* lvl57 */ = new T1(1,_Fu/* Model.Models.lvl56 */),
_Fw/* lvl55 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection Approval\nby the Transplant Centre"));
}),
_Fx/* sApproved25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection\nnot Performed"));
}),
_Fy/* sApproved24 */ = new T1(1,_Fx/* Model.Models.sApproved25 */),
_Fz/* sApproved20 */ = new T(function(){
  return new T2(0,_Fy/* Model.Models.sApproved24 */,_FA/* Model.Models.sApproved21 */);
}),
_FB/* sApproved14 */ = new T(function(){
  return new T2(1,_Fz/* Model.Models.sApproved20 */,_FC/* Model.Models.sApproved15 */);
}),
_FD/* sApproved29 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nDonation\nPreparation"));
}),
_FE/* sPBSCPrep1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("The PBSC Donation preparation consists of several hormonal injections."));
}),
_FF/* sPBSCPrep13 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nDonation\nPrepared"));
}),
_FG/* sPBSCCollected5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nTransportaion"));
}),
_FH/* sPBSC1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC is a donation of blood cells, which is performed ambulant. After the donation, the Donor is made Temporary Deferred for the period of one year and is monitored for a period of at least 10 years."));
}),
_FI/* sPBSC7 */ = new T2(1,_kG/* Model.Elements.iTransplantedPBSC */,_4x/* GHC.Types.[] */),
_FJ/* sPBSC6 */ = new T2(1,_cu/* Model.Elements.iPBSCTransplantation */,_FI/* Model.Models.sPBSC7 */),
_FK/* sPBSC5 */ = new T2(1,_cn/* Model.Elements.iTransplantation */,_FJ/* Model.Models.sPBSC6 */),
_FL/* sPBSC4 */ = new T2(1,_kN/* Model.Elements.iPBSCPatient */,_FK/* Model.Models.sPBSC5 */),
_FM/* sPBSC3 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_FL/* Model.Models.sPBSC4 */, _D3/* B1 */);});
},
_FN/* sPBSC2 */ = new T2(1,_FM/* Model.Models.sPBSC3 */,_4x/* GHC.Types.[] */),
_FO/* sPBSC8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nTransplantation\nCompleted"));
}),
_FP/* sPBSC */ = new T(function(){
  return new T4(0,_FO/* Model.Models.sPBSC8 */,_FN/* Model.Models.sPBSC2 */,_FQ/* Model.Models.sDLITransplanted1 */,_FH/* Model.Models.sPBSC1 */);
}),
_FR/* sPBSCDelivered5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nTransplantation"));
}),
_FS/* sPBSCDelivered4 */ = new T(function(){
  return new T2(0,_FR/* Model.Models.sPBSCDelivered5 */,_FP/* Model.Models.sPBSC */);
}),
_FT/* sPBSCDelivered3 */ = new T(function(){
  return new T2(1,_FS/* Model.Models.sPBSCDelivered4 */,_4x/* GHC.Types.[] */);
}),
_FU/* sPBSCDelivered2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_FT/* Model.Models.sPBSCDelivered3 */);
}),
_FV/* sPBSCDelivered1 */ = new T(function(){
  return new T2(1,_FU/* Model.Models.sPBSCDelivered2 */,_4x/* GHC.Types.[] */);
}),
_FW/* sPBSCDelivered10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nDelivered"));
}),
_FX/* sPBSCDelivered9 */ = new T2(1,_n8/* Model.Elements.iDeliveredPBSC */,_4x/* GHC.Types.[] */),
_FY/* sPBSCDelivered8 */ = new T2(1,_c8/* Model.Elements.iTransportPBSC */,_FX/* Model.Models.sPBSCDelivered9 */),
_FZ/* sPBSCDelivered7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_FY/* Model.Models.sPBSCDelivered8 */, _D3/* B1 */);});
},
_G0/* sPBSCDelivered6 */ = new T2(1,_FZ/* Model.Models.sPBSCDelivered7 */,_4x/* GHC.Types.[] */),
_G1/* sPBSCDelivered */ = new T(function(){
  return new T4(0,_FW/* Model.Models.sPBSCDelivered10 */,_G0/* Model.Models.sPBSCDelivered6 */,_FV/* Model.Models.sPBSCDelivered1 */,_4x/* GHC.Types.[] */);
}),
_G2/* sPBSCCollected4 */ = new T(function(){
  return new T2(0,_FG/* Model.Models.sPBSCCollected5 */,_G1/* Model.Models.sPBSCDelivered */);
}),
_G3/* sPBSCCollected3 */ = new T(function(){
  return new T2(1,_G2/* Model.Models.sPBSCCollected4 */,_4x/* GHC.Types.[] */);
}),
_G4/* sPBSCCollected2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_G3/* Model.Models.sPBSCCollected3 */);
}),
_G5/* sPBSCCollected1 */ = new T(function(){
  return new T2(1,_G4/* Model.Models.sPBSCCollected2 */,_Dc/* Model.Models.sDLICollected2 */);
}),
_G6/* sPBSCCollected11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nCollected"));
}),
_G7/* sPBSCCollected10 */ = new T2(1,_jO/* Model.Elements.iCollectedPBSC */,_4x/* GHC.Types.[] */),
_G8/* sPBSCCollected9 */ = new T2(1,_bD/* Model.Elements.iPBSCCollection */,_G7/* Model.Models.sPBSCCollected10 */),
_G9/* sPBSCCollected8 */ = new T2(1,_bs/* Model.Elements.iCollection */,_G8/* Model.Models.sPBSCCollected9 */),
_Ga/* sPBSCCollected7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_G9/* Model.Models.sPBSCCollected8 */, _D3/* B1 */);});
},
_Gb/* sPBSCCollected6 */ = new T2(1,_Ga/* Model.Models.sPBSCCollected7 */,_4x/* GHC.Types.[] */),
_Gc/* sPBSCCollected */ = new T(function(){
  return new T4(0,_G6/* Model.Models.sPBSCCollected11 */,_Gb/* Model.Models.sPBSCCollected6 */,_G5/* Model.Models.sPBSCCollected1 */,_4x/* GHC.Types.[] */);
}),
_Gd/* sPBSCPrep6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("PBSC\nCollection"));
}),
_Ge/* sPBSCPrep5 */ = new T(function(){
  return new T2(0,_Gd/* Model.Models.sPBSCPrep6 */,_Gc/* Model.Models.sPBSCCollected */);
}),
_Gf/* sPBSCPrep4 */ = new T(function(){
  return new T2(1,_Ge/* Model.Models.sPBSCPrep5 */,_4x/* GHC.Types.[] */);
}),
_Gg/* sPBSCPrep3 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_Gf/* Model.Models.sPBSCPrep4 */);
}),
_Gh/* sPBSCPrep2 */ = new T(function(){
  return new T2(1,_Gg/* Model.Models.sPBSCPrep3 */,_4x/* GHC.Types.[] */);
}),
_Gi/* sPBSCPrep12 */ = new T2(1,_bk/* Model.Elements.iPBSCPreparation */,_4x/* GHC.Types.[] */),
_Gj/* sPBSCPrep11 */ = new T2(1,_jV/* Model.Elements.iDonorPBSC */,_Gi/* Model.Models.sPBSCPrep12 */),
_Gk/* sPBSCPrep10 */ = new T2(1,_bh/* Model.Elements.iPBSCDonation */,_Gj/* Model.Models.sPBSCPrep11 */),
_Gl/* sPBSCPrep9 */ = new T2(1,_bv/* Model.Elements.iDonation */,_Gk/* Model.Models.sPBSCPrep10 */),
_Gm/* sPBSCPrep8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Gl/* Model.Models.sPBSCPrep9 */, _D3/* B1 */);});
},
_Gn/* sPBSCPrep7 */ = new T2(1,_Gm/* Model.Models.sPBSCPrep8 */,_4x/* GHC.Types.[] */),
_Go/* sPBSCPrep */ = new T(function(){
  return new T4(0,_FF/* Model.Models.sPBSCPrep13 */,_Gn/* Model.Models.sPBSCPrep7 */,_Gh/* Model.Models.sPBSCPrep2 */,_FE/* Model.Models.sPBSCPrep1 */);
}),
_Gp/* sApproved28 */ = new T(function(){
  return new T2(0,_FD/* Model.Models.sApproved29 */,_Go/* Model.Models.sPBSCPrep */);
}),
_Gq/* sApproved27 */ = new T(function(){
  return new T2(1,_Gp/* Model.Models.sApproved28 */,_4x/* GHC.Types.[] */);
}),
_Gr/* sApproved31 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("The Donor\nWants to Donate\nBlood Cells"));
}),
_Gs/* sApproved30 */ = new T1(1,_Gr/* Model.Models.sApproved31 */),
_Gt/* sApproved26 */ = new T(function(){
  return new T2(0,_Gs/* Model.Models.sApproved30 */,_Gq/* Model.Models.sApproved27 */);
}),
_Gu/* sApproved13 */ = new T(function(){
  return new T2(1,_Gt/* Model.Models.sApproved26 */,_FB/* Model.Models.sApproved14 */);
}),
_Gv/* sApproved35 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nDonation"));
}),
_Gw/* sBMCollected5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nTransportation"));
}),
_Gx/* sBM1 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM Donation is performed in an anesthetic state in a hospital. After the donation, the Donor is made Temporary Deferred for the period of one year and is monitored for a period of at least 10 years."));
}),
_Gy/* sBM7 */ = new T2(1,_kU/* Model.Elements.iTransplantedBM */,_4x/* GHC.Types.[] */),
_Gz/* sBM6 */ = new T2(1,_cB/* Model.Elements.iBMTransplantation */,_Gy/* Model.Models.sBM7 */),
_GA/* sBM5 */ = new T2(1,_cn/* Model.Elements.iTransplantation */,_Gz/* Model.Models.sBM6 */),
_GB/* sBM4 */ = new T2(1,_l1/* Model.Elements.iBMPatient */,_GA/* Model.Models.sBM5 */),
_GC/* sBM3 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_GB/* Model.Models.sBM4 */, _D3/* B1 */);});
},
_GD/* sBM2 */ = new T2(1,_GC/* Model.Models.sBM3 */,_4x/* GHC.Types.[] */),
_GE/* sBM8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nTransplantation\nCompleted"));
}),
_GF/* sBM */ = new T(function(){
  return new T4(0,_GE/* Model.Models.sBM8 */,_GD/* Model.Models.sBM2 */,_FQ/* Model.Models.sDLITransplanted1 */,_Gx/* Model.Models.sBM1 */);
}),
_GG/* sBMDelivered5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nTransplantation"));
}),
_GH/* sBMDelivered4 */ = new T(function(){
  return new T2(0,_GG/* Model.Models.sBMDelivered5 */,_GF/* Model.Models.sBM */);
}),
_GI/* sBMDelivered3 */ = new T(function(){
  return new T2(1,_GH/* Model.Models.sBMDelivered4 */,_4x/* GHC.Types.[] */);
}),
_GJ/* sBMDelivered2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_GI/* Model.Models.sBMDelivered3 */);
}),
_GK/* sBMDelivered1 */ = new T(function(){
  return new T2(1,_GJ/* Model.Models.sBMDelivered2 */,_4x/* GHC.Types.[] */);
}),
_GL/* sBMDelivered10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nDelivered"));
}),
_GM/* sBMDelivered9 */ = new T2(1,_nf/* Model.Elements.iDeliveredBM */,_4x/* GHC.Types.[] */),
_GN/* sBMDelivered8 */ = new T2(1,_cf/* Model.Elements.iTransportBM */,_GM/* Model.Models.sBMDelivered9 */),
_GO/* sBMDelivered7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_GN/* Model.Models.sBMDelivered8 */, _D3/* B1 */);});
},
_GP/* sBMDelivered6 */ = new T2(1,_GO/* Model.Models.sBMDelivered7 */,_4x/* GHC.Types.[] */),
_GQ/* sBMDelivered */ = new T(function(){
  return new T4(0,_GL/* Model.Models.sBMDelivered10 */,_GP/* Model.Models.sBMDelivered6 */,_GK/* Model.Models.sBMDelivered1 */,_4x/* GHC.Types.[] */);
}),
_GR/* sBMCollected4 */ = new T(function(){
  return new T2(0,_Gw/* Model.Models.sBMCollected5 */,_GQ/* Model.Models.sBMDelivered */);
}),
_GS/* sBMCollected3 */ = new T(function(){
  return new T2(1,_GR/* Model.Models.sBMCollected4 */,_4x/* GHC.Types.[] */);
}),
_GT/* sBMCollected2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_GS/* Model.Models.sBMCollected3 */);
}),
_GU/* sBMCollected1 */ = new T(function(){
  return new T2(1,_GT/* Model.Models.sBMCollected2 */,_Dc/* Model.Models.sDLICollected2 */);
}),
_GV/* sBMCollected14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("BM\nCollected"));
}),
_GW/* sBMCollected13 */ = new T2(1,_k9/* Model.Elements.iCollectedBM */,_4x/* GHC.Types.[] */),
_GX/* sBMCollected12 */ = new T2(1,_bK/* Model.Elements.iBMCollection */,_GW/* Model.Models.sBMCollected13 */),
_GY/* sBMCollected11 */ = new T2(1,_bs/* Model.Elements.iCollection */,_GX/* Model.Models.sBMCollected12 */),
_GZ/* sBMCollected10 */ = new T2(1,_bN/* Model.Elements.iBMDonation */,_GY/* Model.Models.sBMCollected11 */),
_H0/* sBMCollected9 */ = new T2(1,_k2/* Model.Elements.iDonorBM */,_GZ/* Model.Models.sBMCollected10 */),
_H1/* sBMCollected8 */ = new T2(1,_bv/* Model.Elements.iDonation */,_H0/* Model.Models.sBMCollected9 */),
_H2/* sBMCollected7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_H1/* Model.Models.sBMCollected8 */, _D3/* B1 */);});
},
_H3/* sBMCollected6 */ = new T2(1,_H2/* Model.Models.sBMCollected7 */,_4x/* GHC.Types.[] */),
_H4/* sBMCollected */ = new T(function(){
  return new T4(0,_GV/* Model.Models.sBMCollected14 */,_H3/* Model.Models.sBMCollected6 */,_GU/* Model.Models.sBMCollected1 */,_4x/* GHC.Types.[] */);
}),
_H5/* sApproved34 */ = new T(function(){
  return new T2(0,_Gv/* Model.Models.sApproved35 */,_H4/* Model.Models.sBMCollected */);
}),
_H6/* sApproved33 */ = new T(function(){
  return new T2(1,_H5/* Model.Models.sApproved34 */,_4x/* GHC.Types.[] */);
}),
_H7/* sApproved37 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("The Donor\nWants to Donate\nBM"));
}),
_H8/* sApproved36 */ = new T1(1,_H7/* Model.Models.sApproved37 */),
_H9/* sApproved32 */ = new T(function(){
  return new T2(0,_H8/* Model.Models.sApproved36 */,_H6/* Model.Models.sApproved33 */);
}),
_Ha/* sApproved12 */ = new T(function(){
  return new T2(1,_H9/* Model.Models.sApproved32 */,_Gu/* Model.Models.sApproved13 */);
}),
_Hb/* sApproved42 */ = new T2(1,_jD/* Model.Elements.iCollectionReasoning */,_4x/* GHC.Types.[] */),
_Hc/* sApproved41 */ = new T2(1,_js/* Model.Elements.iDonorClearanceAcc */,_Hb/* Model.Models.sApproved42 */),
_Hd/* sApproved40 */ = new T2(1,_jr/* Model.Elements.iDonorAccepted */,_Hc/* Model.Models.sApproved41 */),
_He/* sApproved39 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Hd/* Model.Models.sApproved40 */, _D3/* B1 */);});
},
_Hf/* sApproved38 */ = new T2(1,_He/* Model.Models.sApproved39 */,_4x/* GHC.Types.[] */),
_Hg/* sApproved43 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Collection of HSC\nApproved"));
}),
_Hh/* sApprovedT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("The donor is selected for HSC collection in the case that he is eligible after donor clearance (no medical obstacles for collection).  The donor can be also choose if he was not eligible but there is urgent medical need for transplantation and  another donor do not come into consideration (is not available).<br/><em>WMDA Standard:</em><br/>6.05 The registry must make their policy for the minimum criteria needed to allow a specific donor to be available for a specific patient readily accessible to the appropriate parties, such as national/international organisations authorised to provide haematopoietic stem cell treatment.<br/>6.06 Prior to transplantation, the registry must have a process for communicating the volunteer donor\u2019s preference to the appropriate transplant centre in a timely fashion to indicate the type of cells and to communicate any other donor-specific issues) that may impact the transplantation. Nevertheless, the volunteer donor must be free to change their mind at a later date.<br/>6.06.1 The registry must have a process to communicate issues related to donor health and the release of an increased risk product to the transplant centre.<br/>6.06.1.1 An increased risk product should be released by exception only when there is a documented clinical need for the product and when approved by the physician of the transplant centre.<br/>"));
}),
_Hi/* sApproved */ = new T(function(){
  return new T4(0,_Hg/* Model.Models.sApproved43 */,_Hf/* Model.Models.sApproved38 */,_Ha/* Model.Models.sApproved12 */,_Hh/* Texts.sApprovedT */);
}),
_Hj/* lvl75 */ = new T(function(){
  return new T2(0,_Fw/* Model.Models.lvl55 */,_Hi/* Model.Models.sApproved */);
}),
_Hk/* lvl76 */ = new T(function(){
  return new T2(1,_Hj/* Model.Models.lvl75 */,_4x/* GHC.Types.[] */);
}),
_Hl/* lvl77 */ = new T(function(){
  return new T2(0,_Fv/* Model.Models.lvl57 */,_Hk/* Model.Models.lvl76 */);
}),
_Hm/* lvl48 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nNoneligible (Permanently)\nand Another Donors\nAvailable"));
}),
_Hn/* lvl49 */ = new T1(1,_Hm/* Model.Models.lvl48 */),
_Ho/* lvl63 */ = new T(function(){
  return new T2(1,_Ey/* Model.Models.lvl4 */,_FA/* Model.Models.sApproved21 */);
}),
_Hp/* lvl64 */ = new T(function(){
  return new T2(0,_Hn/* Model.Models.lvl49 */,_Ho/* Model.Models.lvl63 */);
}),
_Hq/* lvl65 */ = new T(function(){
  return new T2(1,_Hp/* Model.Models.lvl64 */,_4x/* GHC.Types.[] */);
}),
_Hr/* lvl50 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nNoneligible (Temporary)\nand another Donor\nAvailable"));
}),
_Hs/* lvl51 */ = new T1(1,_Hr/* Model.Models.lvl50 */),
_Ht/* lvl72 */ = new T(function(){
  return new T2(1,_Hu/* Model.Models.lvl69 */,_FA/* Model.Models.sApproved21 */);
}),
_Hv/* lvl73 */ = new T(function(){
  return new T2(0,_Hs/* Model.Models.lvl51 */,_Ht/* Model.Models.lvl72 */);
}),
_Hw/* lvl74 */ = new T(function(){
  return new T2(1,_Hv/* Model.Models.lvl73 */,_Hq/* Model.Models.lvl65 */);
}),
_Hx/* lvl53 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Noneligible\nand another Donor not Available\nand Urgent Medical Need"));
}),
_Hy/* lvl54 */ = new T1(1,_Hx/* Model.Models.lvl53 */),
_Hz/* lvl52 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Reasoning\nof Collection of Noneligible\nDonor"));
}),
_HA/* lvl78 */ = new T(function(){
  return new T2(0,_Hz/* Model.Models.lvl52 */,_Hi/* Model.Models.sApproved */);
}),
_HB/* lvl79 */ = new T(function(){
  return new T2(1,_HA/* Model.Models.lvl78 */,_4x/* GHC.Types.[] */);
}),
_HC/* lvl80 */ = new T(function(){
  return new T2(0,_Hy/* Model.Models.lvl54 */,_HB/* Model.Models.lvl79 */);
}),
_HD/* lvl81 */ = new T(function(){
  return new T2(1,_HC/* Model.Models.lvl80 */,_Hw/* Model.Models.lvl74 */);
}),
_HE/* a10 */ = new T(function(){
  return new T2(1,_Hl/* Model.Models.lvl77 */,_HD/* Model.Models.lvl81 */);
}),
_HF/* a2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Clearance\nProvided"));
}),
_HG/* sWorkupT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor centre provide medical screening (according to Specification of donor clearance) for the eligibility evaluation of a donor for donation of HSC for a specific patient.<br/><em>WMDA Standard:</em><br/>3.08 The volunteer donor has the right to receive the results of their health screening.<br/>3.23 The volunteer donor\'s medical history taken at the time of medical examination for donation must include questions to identify persons at risk of disease transmissible through transplantation (e.g., infectious diseases, genetic defects or disseminated malignancies) according to WMDA recommendations.<br/>3.25 Infectious disease markers must be measured within thirty (30) days of the HSC/cellular product collection and the results must be provided to the transplant centre before commencement of patient conditioning.<br/>3.26 The volunteer donor must be counselled in case of positive disease results."));
}),
_HH/* sWorkup */ = new T(function(){
  return new T4(0,_HF/* Model.Models.a2 */,_D4/* Model.Models.a */,_HE/* Model.Models.a10 */,_HG/* Texts.sWorkupT */);
}),
_HI/* sScheduled15 */ = new T(function(){
  return new T2(0,_CO/* Model.Models.sScheduled6 */,_HH/* Model.Models.sWorkup */);
}),
_HJ/* sScheduled14 */ = new T(function(){
  return new T2(1,_HI/* Model.Models.sScheduled15 */,_4x/* GHC.Types.[] */);
}),
_HK/* sScheduled13 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_HJ/* Model.Models.sScheduled14 */);
}),
_HL/* sScheduled12 */ = new T(function(){
  return new T2(1,_HK/* Model.Models.sScheduled13 */,_4x/* GHC.Types.[] */);
}),
_HM/* sScheduled16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Schedule and Specification\nof Donor Clearance Assigned"));
}),
_HN/* sScheduledT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor centre must contact and informed the specific donor of the proposed date(s) of transplant based on Formal Request for HSC collection by TC. If possible the schedule is proposed, if donor was temporarily unavailable DC informs TC and new schedule can be proposed. Specification of donor clearance is proposed in cooperation of DC and TC (registry policy).<br/><em>WMDA Standard:</em><br/>3.10 Volunteer donors must be counselled when selected for further tests and when selected as a donor for a specific patient.<br/>3.10.1 Counselling for volunteer donors selected for specific patients must include anonymity of the donor and patient, requirement for further blood samples before donation, requirement for infectious disease and other testing, risk of donation, possible duration of loss of time from normal activities, location of the collection, the potential for collection of autologous blood, donor\u2019s right to withdraw and consequences for the patient, details of insurance coverage, possible subsequent donations of HSC or cellular products, alternative collection methods and whether blood or other biological material is reserved for research purposes.<br/>3.11 Valid signed informed consent must be obtained from all volunteer donors at the time of work-up.<br/>3.11.1 Informed consent documents must meet established criteria based at a minimum on WMDA guidelines. In addition to information on the process, risks and benefits, documents must include information on the collection and protection of donor data and the right of the donor to medical confidentiality and to receive medical information. Documents must be clearly written in terms understood by the donor and, at work-up, must include the signature(s) of qualified staff involved in donor counselling.<br/>3.12 The identity of the volunteer donor must be verified, at a minimum, at work-up and at collection, by the qualified staff signing the consent form.<br/>3.22 Donor health requirements affecting the suitability of volunteer donors must be established.<br/>3.22.3 Policies for testing the volunteer donor selected for work-up must be established and must include medical history, physical exam, and laboratory tests in order to determine the volunteer\u2019s fitness to donate.<br/>3.22.3.1 This examination must be performed or supervised by a physician who is not a member of a team who has cared for the patient.<br/>3.22.3.2 Female volunteer donors of childbearing potential must have a pregnancy test and be counselled to avoid pregnancy during the work-up stage before use of mobilising agents, HSC collection or initiation of the recipient\u2019s preparative regimen, whichever occurs earliest.<br/>3.24 Infectious disease testing of volunteer donors selected for specific patients must include testing for diseases thought to be important to consider in HSC transplantation. Testing must monitor infection with human immunodeficiency virus (HIV), Human T-cell Lymphotropic virus I and II, Hepatitis B virus, Hepatitis C virus, Cytomegalovirus (CMV), Treponema pallidum (syphilis) and other infectious agents as defined by national health authorities.<br/>3.24.1 Selected volunteer donors should also be tested for local diseases that are important to consider in transplantation. Donors who have recently travelled outside their country should also be evaluated for infectious agents prevalent in the areas of travel.<br/>6.04 The donor centre/cord blood bank must be informed of the proposed date(s) of transplant at the time a specific donor/cord blood unit is requested for transplantation for a specific patient. If a volunteer donor will be the source of HSC, the donor must also be informed. The transplant centre must specify the latest date by which the donor centre must approve the eligibility of a donor for donation of HSC for a specific patient (i.e., provide donor clearance).<br/><em>WMDA Recommendations:</em><ul><br/><li>Recommended minimum medical assessment at work-up:</li><br/><li>Travel history (Identify travel to areas with endemic malaria, chagas and West Nile virus)</li><br/><li>Sexual history (Identification of high risk sexual behaviour, including within groups associated with a higher prevalence of blood borne viruses)</li><br/><li>Examination (General (including height and weight); cardiovascular (including blood pressure); respiratory; gastrointestinal; neurological)</li><br/><li>Laboratory investigations (see table 4 for infectious disease markers)</li><br/><li>Haematology (Full blood count; coagulation screen (including PT, APTT and fibrinogen); ESR; blood film; haemoglobin electrophoresis or high-pressure liquid chromatography)</li><br/><li>Biochemistry (Urea and electrolytes; liver function tests; LDH; ferritin; random glucose; b-HCG (for females of child-bearing age))</li><br/><li>Other investigations (Chest x-ray; electrocardiogram)</li></ul>"));
}),
_HO/* sScheduled */ = new T(function(){
  return new T4(0,_HM/* Model.Models.sScheduled16 */,_E1/* Model.Models.sScheduled7 */,_HL/* Model.Models.sScheduled12 */,_HN/* Texts.sScheduledT */);
}),
_HP/* sApproved18 */ = new T(function(){
  return new T2(0,_Ft/* Model.Models.sApproved19 */,_HO/* Model.Models.sScheduled */);
}),
_HQ/* sApproved17 */ = new T(function(){
  return new T2(1,_HP/* Model.Models.sApproved18 */,_4x/* GHC.Types.[] */);
}),
_HR/* sApproved16 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_HQ/* Model.Models.sApproved17 */);
}),
_FC/* sApproved15 */ = new T(function(){
  return new T2(1,_HR/* Model.Models.sApproved16 */,_4x/* GHC.Types.[] */);
}),
_HS/* sChosen12 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Chosen\nfor Donation"));
}),
_HT/* sChosenT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplant centre can choose donor with verified concordance of HLA, identity and performed health screening affecting the suitability of donor for work-up.<br/><em>WMDA Definition:</em><br/>Work-up - At this stage, a volunteer donor has been identified as an acceptable match for a patient, agrees to donate HSC after a full donor information and counselling session, and is medically evaluated for their fitness to donate HSC.<br/><em>WMDA Standard:</em><br/>10.04 Any cost not standardised or, for any reason, not accessible through such a schedule should be estimated and communicated in advance to the requesting registry and/or transplant centre.<br/>10.05 If the collection procedure is cancelled after the final donor selection, collection centre and/or donor centre and/or registry are entitled to charge for services performed prior to notice of cancellation. This practice must be noted on the fee schedule."));
}),
_HU/* sChosen */ = new T(function(){
  return new T4(0,_HS/* Model.Models.sChosen12 */,_Eb/* Model.Models.sChosen7 */,_FC/* Model.Models.sApproved15 */,_HT/* Texts.sChosenT */);
}),
_HV/* sApproved22 */ = new T(function(){
  return new T2(0,_Fs/* Model.Models.sApproved23 */,_HU/* Model.Models.sChosen */);
}),
_FA/* sApproved21 */ = new T(function(){
  return new T2(1,_HV/* Model.Models.sApproved22 */,_4x/* GHC.Types.[] */);
}),
_HW/* sDonorReserved2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_FA/* Model.Models.sApproved21 */);
}),
_HX/* sDonorReserved1 */ = new T(function(){
  return new T2(1,_HW/* Model.Models.sDonorReserved2 */,_4x/* GHC.Types.[] */);
}),
_HY/* sDonorReserved6 */ = new T2(1,_j2/* Model.Elements.iDonorReservation */,_4x/* GHC.Types.[] */),
_HZ/* sDonorReserved5 */ = new T2(1,_j3/* Model.Elements.iDonorReserved */,_HY/* Model.Models.sDonorReserved6 */),
_I0/* sDonorReserved4 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_HZ/* Model.Models.sDonorReserved5 */, _D3/* B1 */);});
},
_I1/* sDonorReserved3 */ = new T2(1,_I0/* Model.Models.sDonorReserved4 */,_4x/* GHC.Types.[] */),
_I2/* sDonorReserved7 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Reserved"));
}),
_I3/* sDonorReserved */ = new T(function(){
  return new T4(0,_I2/* Model.Models.sDonorReserved7 */,_I1/* Model.Models.sDonorReserved3 */,_HX/* Model.Models.sDonorReserved1 */,_4x/* GHC.Types.[] */);
}),
_I4/* sVerified5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Reservation"));
}),
_I5/* sVerified4 */ = new T(function(){
  return new T2(0,_I4/* Model.Models.sVerified5 */,_I3/* Model.Models.sDonorReserved */);
}),
_I6/* sVerified3 */ = new T(function(){
  return new T2(1,_I5/* Model.Models.sVerified4 */,_4x/* GHC.Types.[] */);
}),
_I7/* sVerified2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_I6/* Model.Models.sVerified3 */);
}),
_I8/* sVerified1 */ = new T(function(){
  return new T2(1,_I7/* Model.Models.sVerified2 */,_4x/* GHC.Types.[] */);
}),
_I9/* sVerified8 */ = new T2(1,_fH/* Model.Elements.iDonorVerified */,_4x/* GHC.Types.[] */),
_Ia/* sVerified7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_I9/* Model.Models.sVerified8 */, _D3/* B1 */);});
},
_Ib/* sVerified6 */ = new T2(1,_Ia/* Model.Models.sVerified7 */,_4x/* GHC.Types.[] */),
_Ic/* sVerified9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Verified"));
}),
_Id/* sVerified */ = new T(function(){
  return new T4(0,_Ic/* Model.Models.sVerified9 */,_Ib/* Model.Models.sVerified6 */,_I8/* Model.Models.sVerified1 */,_4x/* GHC.Types.[] */);
}),
_Ie/* sHLAVerified4 */ = new T(function(){
  return new T2(0,_Fr/* Model.Models.sHLAVerified5 */,_Id/* Model.Models.sVerified */);
}),
_If/* sHLAVerified3 */ = new T(function(){
  return new T2(1,_Ie/* Model.Models.sHLAVerified4 */,_4x/* GHC.Types.[] */);
}),
_Ig/* sHLAVerified2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_If/* Model.Models.sHLAVerified3 */);
}),
_Ih/* sHLAVerified1 */ = new T(function(){
  return new T2(1,_Ig/* Model.Models.sHLAVerified2 */,_4x/* GHC.Types.[] */);
}),
_Ii/* sHLAVerified15 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor HLA Verified"));
}),
_Ij/* sHLAVerified13 */ = new T2(1,_eq/* Model.Elements.iGeneAssignment3 */,_EP/* Model.Models.sHLAVerified14 */),
_Ik/* sHLAVerified12 */ = new T2(1,_gM/* Model.Elements.iDonorsTypingResults3 */,_Ij/* Model.Models.sHLAVerified13 */),
_Il/* sHLAVerified11 */ = new T2(1,_gE/* Model.Elements.iHLATyping3 */,_Ik/* Model.Models.sHLAVerified12 */),
_Im/* sHLAVerified10 */ = new T2(1,_e1/* Model.Elements.iDNAIsolation3 */,_Il/* Model.Models.sHLAVerified11 */),
_In/* sHLAVerified9 */ = new T2(1,_aj/* Model.Elements.iDNASample */,_Im/* Model.Models.sHLAVerified10 */),
_Io/* sHLAVerified8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_In/* Model.Models.sHLAVerified9 */, _D3/* B1 */);});
},
_Ip/* sHLAVerified7 */ = new T2(1,_Io/* Model.Models.sHLAVerified8 */,_4x/* GHC.Types.[] */),
_Iq/* sHLAVerifiedT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Before possible selection for HSC donation for selected donor must be verified the concordance of an existing HLA assignment.<br/><em>WMDA Definition:</em><br/>ABO - Major human blood group including erythrocyte antigens, A, B and O.<br/>Rh - A specific antigen present on the surface of red blood cells.<br/><em>WMDA Standard:</em><br/>3.21 The ABO blood group and Rh factor testing of volunteer donors must be done at the verification typing stage if the donor\'s blood group has not been previously determined.<br/>6.02 Registries must respond to search requests and to requests for additional information and/or an aliquot of donor (or maternal if cord blood) sample within a time period consistent with WMDA metrics and in a defined manner.<br/>6.02.2 Verification typing of the donor/cord blood unit at a minimum of HLA-A, -B, -DRB1 must be performed prior to donation/shipment for a specific patient.<br/>"));
}),
_Ir/* sHLAVerified */ = new T(function(){
  return new T4(0,_Ii/* Model.Models.sHLAVerified15 */,_Ip/* Model.Models.sHLAVerified7 */,_Ih/* Model.Models.sHLAVerified1 */,_Iq/* Texts.sHLAVerifiedT */);
}),
_Is/* sDraw10 */ = new T(function(){
  return new T2(0,_Fo/* Model.Models.sDraw11 */,_Ir/* Model.Models.sHLAVerified */);
}),
_It/* sDraw9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Medical Assessment"));
}),
_Iu/* sScreened7 */ = new T2(1,_hs/* Model.Elements.iInfectionMarkersV */,_4x/* GHC.Types.[] */),
_Iv/* sScreened6 */ = new T2(1,_aF/* Model.Elements.iInfectionTestingV */,_Iu/* Model.Models.sScreened7 */),
_Iw/* sScreened5 */ = new T2(1,_hl/* Model.Elements.iInfectionTesting */,_Iv/* Model.Models.sScreened6 */),
_Ix/* sScreened4 */ = new T2(1,_i8/* Model.Elements.iMedicalAssessmentResults2 */,_Iw/* Model.Models.sScreened5 */),
_Iy/* sScreened3 */ = new T2(1,_ba/* Model.Elements.iMedicalAssessment2 */,_Ix/* Model.Models.sScreened4 */),
_Iz/* sScreened2 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Iy/* Model.Models.sScreened3 */, _D3/* B1 */);});
},
_IA/* sScreened1 */ = new T2(1,_Iz/* Model.Models.sScreened2 */,_4x/* GHC.Types.[] */),
_IB/* sScreened8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nHealth Screening\nCompleted"));
}),
_IC/* sScreenedT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Before possible selection for HSC donation for selected donor must be verified the donor identity and performed health screening affecting the suitability of donor.<br/><em>WMDA Standard:</em><br/>3.08 The volunteer donor has the right to receive the results of their health screening.<br/>3.22.2 A health screening must be performed at time of verification typing.<br/>3.22.2.1 Information on donor parity and history of other prior sensitizing events such as transfusion must be obtained from volunteer donors during the verification typing stage.<br/>3.23 The volunteer donor\'s medical history taken at the time of medical examination for donation must include questions to identify persons at risk of disease transmissible through transplantation (e.g., infectious diseases, genetic defects or disseminated malignancies) according to WMDA recommendations.<br/><em>WMDA Recommendations:</em><br/>Minimum donor medical and lifestyle information requested at confirmatory/verification typing stage:<ul><br/><li>Cancer</li><br/><li>Autoimmune disease (Ankylosing spondylitis; Crohn\u2019s disease; ulcerative colitis; myasthenia gravis; rheumatoid arthritis; sarcoidosis; SLE; multiple sclerosis; scleroderma/CREST. Any other autoimmune condition)</li><br/><li>Infectious diseases, including being a sexual partner of an infected individual (HIV, Hepatitis B, Hepatitis C, HTLV, syphilis)</li><br/><li>Infectious diseases, others (CJD - including familial and exposure risk, e.g. neurosurgery, use of pituitary hormone, Chagas disease, tuberculosis, malaria)</li><br/><li>Back problems (Any acute or chronic back complaint, including cause, investigations, duration, medication and impact on activities of daily living)</li><br/><li>Hypertension (Most recent blood pressure readings; medications; degree of control)</li><br/><li>Cardiac disease (Coronary artery disease; evidence of valve disease, e.g. murmur; arrhythmia)</li><br/><li>Asthma (Degree of control; medications; use of oral steroids; hospital admissions; intensive care admissions/ventilation)</li><br/><li>Epilepsy (Medications; date of last seizure)</li><br/><li>Pregnancy (Number of pregnancies, including miscarriage; current/recent pregnancies; breastfeeding.)</li><br/><li>Blood transfusion (Receipt of a blood transfusion. Ask year and place of transfusion.)</li><br/><li>Any other medical history (The potential donor should be asked if they have any other past or current medical problems)</li><br/><li>Height and weight</li><br/><li>High risk sexual behaviour (As defined by the registry\u2019s national competent authority)</li><br/><li>Non-prescription parenteral drug use</li><br/><li>Alcohol consumption</li><br/><li>Tattoo, acupuncture or body piercing (When and where. Establish if at an establishment registered according to national regulations)</li><br/><li>Current medications</li><br/><li>Allergies</li></ul>"));
}),
_ID/* sScreened */ = new T(function(){
  return new T4(0,_IB/* Model.Models.sScreened8 */,_IA/* Model.Models.sScreened1 */,_Ih/* Model.Models.sHLAVerified1 */,_IC/* Texts.sScreenedT */);
}),
_IE/* sDraw8 */ = new T(function(){
  return new T2(0,_It/* Model.Models.sDraw9 */,_ID/* Model.Models.sScreened */);
}),
_IF/* sDraw7 */ = new T(function(){
  return new T2(1,_IE/* Model.Models.sDraw8 */,_4x/* GHC.Types.[] */);
}),
_IG/* sDraw6 */ = new T(function(){
  return new T2(1,_Is/* Model.Models.sDraw10 */,_IF/* Model.Models.sDraw7 */);
}),
_IH/* sDraw5 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_IG/* Model.Models.sDraw6 */);
}),
_II/* sDraw1 */ = new T(function(){
  return new T2(1,_IH/* Model.Models.sDraw5 */,_4x/* GHC.Types.[] */);
}),
_IJ/* sDraw14 */ = new T2(1,_a5/* Model.Elements.iBloodSampleDraw3 */,_CS/* Model.Models.sDraw15 */),
_IK/* sDraw13 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_IJ/* Model.Models.sDraw14 */, _D3/* B1 */);});
},
_IL/* sDraw12 */ = new T2(1,_IK/* Model.Models.sDraw13 */,_4x/* GHC.Types.[] */),
_IM/* sDraw16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Blood Sample\nDrawn (3)"));
}),
_IN/* sDraw3 */ = new T(function(){
  return new T4(0,_IM/* Model.Models.sDraw16 */,_IL/* Model.Models.sDraw12 */,_II/* Model.Models.sDraw1 */,_Fn/* Texts.sBloodSampleDrawnT */);
}),
_IO/* sSelection5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Activation\nand Blood Sample Draw (3)"));
}),
_IP/* sSelection4 */ = new T(function(){
  return new T2(0,_IO/* Model.Models.sSelection5 */,_IN/* Model.Models.sDraw3 */);
}),
_IQ/* sSelection3 */ = new T(function(){
  return new T2(1,_IP/* Model.Models.sSelection4 */,_4x/* GHC.Types.[] */);
}),
_IR/* sSelection2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_IQ/* Model.Models.sSelection3 */);
}),
_IS/* sSelection1 */ = new T(function(){
  return new T2(1,_IR/* Model.Models.sSelection2 */,_4x/* GHC.Types.[] */);
}),
_IT/* sSelection8 */ = new T2(1,_uT/* Model.Elements.iSelectedV */,_4x/* GHC.Types.[] */),
_IU/* sSelection7 */ = function(_IV/* s6LW */){
  return new F(function(){return _z7/* Metamodel.UfoB.$wswitchPhase */(_wc/* Model.Models.sPermDeferred5 */, _IT/* Model.Models.sSelection8 */, _IV/* s6LW */);});
},
_IW/* sSelection6 */ = new T2(1,_IU/* Model.Models.sSelection7 */,_4x/* GHC.Types.[] */),
_IX/* sSelection9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donors Selected\nfor Verification"));
}),
_IY/* sSelectionT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Transplant centre can select donor listed on search report for verification.<br/><em>WMDA Definition:</em><br/>Verification Typing - This HLA typing includes the tests carried out on a fresh sample of a specific donor or on an attached-segment of a cord blood unit with the purpose of verifying the identity and concordance of an existing HLA assignment. The purpose of this typing is to ensure that the volunteer/cord blood unit is the same individual/unit whose HLA typing was listed on the search report used to select the donor. This stage used to be referred to as \"confirmatory typing (CT)\".<br/><em>WMDA Standard:</em><br/>2.04 The director or key registry personnel or consultants must have expertise in human histocompatibility and HSC transplantation as documented by the relevant education and experience. At least one of these individuals must be a physician. These individuals must possess a basic understanding of diseases treatable by HSC transplantation, comprehend alternative therapies and donor search problems associated with these diseases, understand HLA specificities antigens/alleles(serologic and DNA-based) and haplotypes, and possess a knowledge of transplant centre, donor centre, collection centre, cord blood bank (if applicable), and registry protocols in their own country and abroad.<br/>2.05 The registry must have a qualified and trained health care professional readily available to assist with routine medical decisions regarding donor selection and donation.<br/>3.10 Volunteer donors must be counselled when selected for further tests and when selected as a donor for a specific patient.<br/>3.10.1 Counselling for volunteer donors selected for specific patients must include anonymity of the donor and patient, requirement for further blood samples before donation, requirement for infectious disease and other testing, risk of donation, possible duration of loss of time from normal activities, location of the collection, the potential for collection of autologous blood, donor\u2019s right to withdraw and consequences for the patient, details of insurance coverage, possible subsequent donations of HSC or cellular products, alternative collection methods and whether blood or other biological material is reserved for research purposes.<br/>3.13 Valid signed informed consent must be obtained if donor blood or other biological material or information is stored and/or used for the purpose of an ethically approved research project.<br/>3.19 The results of the donor assessment including the results of any laboratory tests and medical evaluation must be documented and maintained.<br/>6.03 A donor selected for a specific patient must be placed on a \u201creserved\u201d status from the time of verification typing until the donation date is reached.<br/>6.03.1 If the donation/shipping date is not scheduled or is delayed, a maximum time limit and the procedures for granting exceptions for this status must be set in writing and be readily accessible to health care professionals involved in HSC transplantation.<br/>10.03 The registry must have available a clear fee schedule detailing payment terms for extended and verification HLA testing, infectious disease marker testing, procurement and other related services upon request.<br/>10.03.1 The registry should have a procedure to communicate changes in the fee schedule to interested parties thirty (30) days prior to implementation.<br/>10.04 Any cost not standardised or, for any reason, not accessible through such a schedule should be estimated and communicated in advance to the requesting registry and/or transplant centre.<br/>"));
}),
_IZ/* sSelection */ = new T(function(){
  return new T4(0,_IX/* Model.Models.sSelection9 */,_IW/* Model.Models.sSelection6 */,_IS/* Model.Models.sSelection1 */,_IY/* Texts.sSelectionT */);
}),
_J0/* sHLATyped2_7 */ = new T(function(){
  return new T2(0,_Fm/* Model.Models.sHLATyped2_8 */,_IZ/* Model.Models.sSelection */);
}),
_J1/* sHLATyped2_6 */ = new T(function(){
  return new T2(1,_J0/* Model.Models.sHLATyped2_7 */,_4x/* GHC.Types.[] */);
}),
_J2/* sHLATyped2_5 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_J1/* Model.Models.sHLATyped2_6 */);
}),
_J3/* sHLATyped2_4 */ = new T(function(){
  return new T2(1,_J2/* Model.Models.sHLATyped2_5 */,_4x/* GHC.Types.[] */);
}),
_J4/* sHLATyped2_1 */ = new T(function(){
  return new T4(0,_Fl/* Model.Models.sHLATyped2_24 */,_Fk/* Model.Models.sHLATyped2_21 */,_J3/* Model.Models.sHLATyped2_4 */,_Fc/* Texts.sHLATyped2T */);
}),
_J5/* sFound19 */ = new T(function(){
  return new T2(0,_Fb/* Model.Models.sFound20 */,_J4/* Model.Models.sHLATyped2_1 */);
}),
_J6/* sFound18 */ = new T(function(){
  return new T2(1,_J5/* Model.Models.sFound19 */,_4x/* GHC.Types.[] */);
}),
_J7/* sFound22 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended HLA\nTyping Required\nand\nBlood Sample\nAvailable"));
}),
_J8/* sFound21 */ = new T1(1,_J7/* Model.Models.sFound22 */),
_J9/* sFound17 */ = new T(function(){
  return new T2(0,_J8/* Model.Models.sFound21 */,_J6/* Model.Models.sFound18 */);
}),
_Ja/* sFound14 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended\nHLA\nTyping\nfrom Stored\nDNA Sample"));
}),
_Jb/* sHLATyped2_19 */ = new T2(1,_sn/* Model.Elements.iDSStored */,_Ff/* Model.Models.sHLATyped2_13 */),
_Jc/* sHLATyped2_18 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Jb/* Model.Models.sHLATyped2_19 */, _D3/* B1 */);});
},
_Jd/* sHLATyped2_17 */ = new T2(1,_Jc/* Model.Models.sHLATyped2_18 */,_4x/* GHC.Types.[] */),
_Je/* sHLATyped2_20 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nExtended\nHLA Typed\nfrom a Stored\nDNA Sample"));
}),
_Jf/* sHLATyped2_2 */ = new T(function(){
  return new T4(0,_Je/* Model.Models.sHLATyped2_20 */,_Jd/* Model.Models.sHLATyped2_17 */,_J3/* Model.Models.sHLATyped2_4 */,_Fc/* Texts.sHLATyped2T */);
}),
_Jg/* sFound13 */ = new T(function(){
  return new T2(0,_Ja/* Model.Models.sFound14 */,_Jf/* Model.Models.sHLATyped2_2 */);
}),
_Jh/* sFound12 */ = new T(function(){
  return new T2(1,_Jg/* Model.Models.sFound13 */,_4x/* GHC.Types.[] */);
}),
_Ji/* sFound16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended HLA\nTyping Required\nand\nDNA Sample\nAvailable"));
}),
_Jj/* sFound15 */ = new T1(1,_Ji/* Model.Models.sFound16 */),
_Jk/* sFound11 */ = new T(function(){
  return new T2(0,_Jj/* Model.Models.sFound15 */,_Jh/* Model.Models.sFound12 */);
}),
_Jl/* sDraw21 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended Histocompatibility\nTesting"));
}),
_Jm/* sHLATyped2_16 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nExtended\nHLA Typed\nfrom a Fresh\nBlood Sample"));
}),
_Jn/* sHLATyped2_10 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Fh/* Model.Models.sHLATyped2_11 */, _D3/* B1 */);});
},
_Jo/* sHLATyped2_9 */ = new T2(1,_Jn/* Model.Models.sHLATyped2_10 */,_4x/* GHC.Types.[] */),
_Jp/* sHLATyped2_3 */ = new T(function(){
  return new T4(0,_Jm/* Model.Models.sHLATyped2_16 */,_Jo/* Model.Models.sHLATyped2_9 */,_J3/* Model.Models.sHLATyped2_4 */,_Fc/* Texts.sHLATyped2T */);
}),
_Jq/* sDraw20 */ = new T(function(){
  return new T2(0,_Jl/* Model.Models.sDraw21 */,_Jp/* Model.Models.sHLATyped2_3 */);
}),
_Jr/* sDraw19 */ = new T(function(){
  return new T2(1,_Jq/* Model.Models.sDraw20 */,_4x/* GHC.Types.[] */);
}),
_Js/* sDraw18 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_Jr/* Model.Models.sDraw19 */);
}),
_Jt/* sDraw17 */ = new T(function(){
  return new T2(1,_Js/* Model.Models.sDraw18 */,_4x/* GHC.Types.[] */);
}),
_Ju/* sDraw25 */ = new T2(1,_9S/* Model.Elements.iBloodSampleDraw2 */,_CS/* Model.Models.sDraw15 */),
_Jv/* sDraw24 */ = new T2(1,_9V/* Model.Elements.iExtendedExamination */,_Ju/* Model.Models.sDraw25 */),
_Jw/* sDraw23 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Jv/* Model.Models.sDraw24 */, _D3/* B1 */);});
},
_Jx/* sDraw22 */ = new T2(1,_Jw/* Model.Models.sDraw23 */,_4x/* GHC.Types.[] */),
_Jy/* sDraw26 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Blood Sample\nDrawn (2)"));
}),
_Jz/* sDraw2 */ = new T(function(){
  return new T4(0,_Jy/* Model.Models.sDraw26 */,_Jx/* Model.Models.sDraw22 */,_Jt/* Model.Models.sDraw17 */,_Fn/* Texts.sBloodSampleDrawnT */);
}),
_JA/* sFound8 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Activation\nand Blood Sample Draw (2)"));
}),
_JB/* sFound7 */ = new T(function(){
  return new T2(0,_JA/* Model.Models.sFound8 */,_Jz/* Model.Models.sDraw2 */);
}),
_JC/* sFound6 */ = new T(function(){
  return new T2(1,_JB/* Model.Models.sFound7 */,_4x/* GHC.Types.[] */);
}),
_JD/* sFound10 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended HLA\nTyping Required\nand\nBlood Sample/DNA\nnot Available"));
}),
_JE/* sFound9 */ = new T1(1,_JD/* Model.Models.sFound10 */),
_JF/* sFound5 */ = new T(function(){
  return new T2(0,_JE/* Model.Models.sFound9 */,_JC/* Model.Models.sFound6 */);
}),
_JG/* sFound4 */ = new T(function(){
  return new T2(1,_JF/* Model.Models.sFound5 */,_4x/* GHC.Types.[] */);
}),
_JH/* sFound3 */ = new T(function(){
  return new T2(1,_Jk/* Model.Models.sFound11 */,_JG/* Model.Models.sFound4 */);
}),
_JI/* sFound2 */ = new T(function(){
  return new T2(1,_J9/* Model.Models.sFound17 */,_JH/* Model.Models.sFound3 */);
}),
_JJ/* sFound25 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Extended HLA\nTyping not Required"));
}),
_JK/* sFound24 */ = new T1(1,_JJ/* Model.Models.sFound25 */),
_JL/* sFound23 */ = new T(function(){
  return new T2(0,_JK/* Model.Models.sFound24 */,_J1/* Model.Models.sHLATyped2_6 */);
}),
_JM/* sFound1 */ = new T(function(){
  return new T2(1,_JL/* Model.Models.sFound23 */,_JI/* Model.Models.sFound2 */);
}),
_JN/* sFound30 */ = new T2(1,_aw/* Model.Elements.iFoundDonors */,_4x/* GHC.Types.[] */),
_JO/* sFound29 */ = new T2(1,_uW/* Model.Elements.iNotSelected */,_JN/* Model.Models.sFound30 */),
_JP/* sFound28 */ = new T2(1,_as/* Model.Elements.iDonorPotential */,_JO/* Model.Models.sFound29 */),
_JQ/* sFound27 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_JP/* Model.Models.sFound28 */, _D3/* B1 */);});
},
_JR/* sFound26 */ = new T2(1,_JQ/* Model.Models.sFound27 */,_4x/* GHC.Types.[] */),
_JS/* sFound31 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Potential\nDonor(s)\nFound"));
}),
_JT/* sFoundT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("The result of search is search report, where potential donors are listed. Potential Donors become members of the group Found Donors.<br/><em>WMDA Definition:</em><br/>Search - The process of identifying a suitable donor to donate haematopoietic stem cells for a patient in need of a transplant.<br/><em>WMDA Standard:</em><br/>2.07.1 At least one member of the registry staff must be able to communicate in English and be available as needed to facilitate international searches.<br/>2.10 The registry must have sufficient communication links to facilitate searches.<br/>5.17 Search algorithms must provide lists of suitably matched donors in a reasonable time frame.<br/>5.18 Each printed report must be dated.<br/>5.19 Each step in the search process must be documented with all relevant attributes and a time stamp.<br/>6.01 Critical communications between registries or between a registry and a transplant centre must be in writing clearly readable, or via electronic established system.<br/>6.01.1 These communications should contain a signature of authorisation and be sent by fax or email or should be submitted through authorised access to a communication system.<br/>6.02 Registries must respond to search requests and to requests for additional information and/or an aliquot of donor (or maternal if cord blood) sample within a time period consistent with WMDA metrics and in a defined manner.<br/>6.02.3 The policy of the registry regarding repetition of the database search for a specific patient must be defined and readily accessible to health care professionals involved in HSC transplantation.<br/>6.02.4 The registry must have an effective mechanism to provide access to international patients.<br/>6.07 Donor and patient identity must remain confidential throughout the search process so that only appropriate registry personnel have access to these data.<br/>6.07.1 The registry must have a written policy listing the conditions under which volunteer donors and recipients might be informed of each other\u2019s identity. These policies must comply with governmental laws on disclosure."));
}),
_JU/* sFound */ = new T(function(){
  return new T4(0,_JS/* Model.Models.sFound31 */,_JR/* Model.Models.sFound26 */,_JM/* Model.Models.sFound1 */,_JT/* Texts.sFoundT */);
}),
_JV/* sSearchRequested6 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donors Search"));
}),
_JW/* sSearchRequested5 */ = new T(function(){
  return B(_2G/* GHC.Base.++ */(_Fp/* Metamodel.UfoB.mEventB1 */, _JV/* Model.Models.sSearchRequested6 */));
}),
_JX/* sSearchRequested4 */ = new T(function(){
  return new T2(0,_JW/* Model.Models.sSearchRequested5 */,_JU/* Model.Models.sFound */);
}),
_JY/* sSearchRequested3 */ = new T(function(){
  return new T2(1,_JX/* Model.Models.sSearchRequested4 */,_4x/* GHC.Types.[] */);
}),
_JZ/* sSearchRequested2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_JY/* Model.Models.sSearchRequested3 */);
}),
_K0/* a9 */ = new T(function(){
  return new T2(1,_JZ/* Model.Models.sSearchRequested2 */,_Fa/* Model.Models.lvl71 */);
}),
_K1/* sIssuedRegistrationT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Entities involved in donor recruitment carries out the necessary paperwork (registration form,  valid signed informed consent, medical assessment, necessary information form, etc.).<br/><em><em>WMDA Definition:</em></em><br/>Valid signed informed consent - Signed documentation indicating that a volunteer donor or the maternal donor of umbilical cord blood has been provided with information on the procedure and tests performed, the risks and benefits of the procedure, that they have understood the information provided, have had an opportunity to ask questions, have been provided with satisfactory responses and have confirmed that all information provided is true to the best of their knowledge. The informed consent is valid when it complies with national regulation.<br/><em><em>WMDA Abbreviation:</em></em><br/>HSC - Haematopoietic stem cells are the cells, which give rise to blood and immune system cells. These cells are found in bone marrow, growth factor stimulated peripheral blood, and umbilical cord blood.<br/><em><em>WMDA Standard:</em></em><br/>3.04 Donors must be informed regarding their potential role in the donation of HSC, the risks involved in the donation, and the tests that the donor may undergo. <br/>3.05 Donors must be informed about the use of any medical intervention (e.g., administration of GCSF - Granulocyte colony-stimulating factor) and its known risks and/or side effects.<br/>3.06 A volunteer donor must be free to withdraw at any time. <br/>3.07 To ensure confidentiality, the identity of donors must be protected. Policies and procedures must be in place to ensure donor confidentiality. <br/>3.09 Valid signed informed consent must be obtained initially at the time of recruitment.<br/>3.14 Consent documents signed by volunteer donors must be available for review by individuals designated by the registry or national authorities to evaluate the registry.<br/>3.15 Information on donor age and gender must be collected at the time of recruitment.<br/>3.22 Donor health requirements affecting the suitability of volunteer donors must be established. <br/>3.22.1 An initial health screening should be performed at the time of recruitment.<br/><em>WMDA Recommendations:</em><br/>Recommended minimum donor medical and lifestyle information requested at recruitment:<br/><ul> <li>Cancer</li> <li>Autoimmune disease</li> <li>Infectious diseases, including being a sexual partner of an infected individual(HIV, Hepatitis B, Hepatitis C, HTLV, syphilis)</li> <li>Infectious diseases, others (CJD - including familial risk, Chagas disease, tuberculosis, malaria)</li> <li>Inherited disease (Sickle cell disease, thalassemia, inherited bleeding disorder)</li> <li>Any other medical history (The potential donor should be asked if they have any other past or current medical problems.)</li> <li>High risk sexual behaviour (As defined by the registry\u2019s national competent authority. However, registries should be aware that sexual practices may change with time and are not necessarily criteria for exclusion.)</li> <li>Non-prescription parenteral drug use</li> <li>Current medications</li> <li>Height and weight</li> <li>Allergies</li></ul>"));
}),
_K2/* sAvailable */ = new T(function(){
  return new T4(0,_EY/* Model.Models.a7 */,_EX/* Model.Models.a6 */,_K0/* Model.Models.a9 */,_K1/* Texts.sIssuedRegistrationT */);
}),
_K3/* lvl66 */ = new T(function(){
  return new T2(0,_EG/* Model.Models.lvl9 */,_K2/* Model.Models.sAvailable */);
}),
_K4/* lvl67 */ = new T(function(){
  return new T2(1,_K3/* Model.Models.lvl66 */,_4x/* GHC.Types.[] */);
}),
_K5/* lvl68 */ = new T(function(){
  return new T2(0,_EF/* Model.Models.lvl11 */,_K4/* Model.Models.lvl67 */);
}),
_K6/* a8 */ = new T(function(){
  return new T2(1,_K5/* Model.Models.lvl68 */,_ED/* Model.Models.lvl62 */);
}),
_K7/* sTempDeferred */ = new T(function(){
  return new T4(0,_CJ/* Model.Models.a5 */,_CI/* Model.Models.a4 */,_K6/* Model.Models.a8 */,_z0/* Model.Models.a3 */);
}),
_Hu/* lvl69 */ = new T(function(){
  return new T2(0,_yZ/* Model.Models.lvl32 */,_K7/* Model.Models.sTempDeferred */);
}),
_F9/* sDLITransplanted3 */ = new T(function(){
  return new T2(1,_Hu/* Model.Models.lvl69 */,_4x/* GHC.Types.[] */);
}),
_K8/* sDLITransplanted2 */ = new T(function(){
  return new T2(0,_8S/* GHC.Base.Nothing */,_F9/* Model.Models.sDLITransplanted3 */);
}),
_FQ/* sDLITransplanted1 */ = new T(function(){
  return new T2(1,_K8/* Model.Models.sDLITransplanted2 */,_4x/* GHC.Types.[] */);
}),
_K9/* sDLITransplanted8 */ = new T2(1,_l8/* Model.Elements.iTransplantedDLI */,_4x/* GHC.Types.[] */),
_Ka/* sDLITransplanted7 */ = new T2(1,_cI/* Model.Elements.iDLITransplantation */,_K9/* Model.Models.sDLITransplanted8 */),
_Kb/* sDLITransplanted6 */ = new T2(1,_lf/* Model.Elements.iDLIPatient */,_Ka/* Model.Models.sDLITransplanted7 */),
_Kc/* sDLITransplanted5 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Kb/* Model.Models.sDLITransplanted6 */, _D3/* B1 */);});
},
_Kd/* sDLITransplanted4 */ = new T2(1,_Kc/* Model.Models.sDLITransplanted5 */,_4x/* GHC.Types.[] */),
_Ke/* sDLITransplanted9 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Subsequent HSC\nTransplantation\nCompleted"));
}),
_Dg/* sDLITransplanted */ = new T(function(){
  return new T4(0,_Ke/* Model.Models.sDLITransplanted9 */,_Kd/* Model.Models.sDLITransplanted4 */,_FQ/* Model.Models.sDLITransplanted1 */,_4x/* GHC.Types.[] */);
}),
_Kf/* ouModelB37 */ = new T2(1,_Dg/* Model.Models.sDLITransplanted */,_yY/* Model.Models.ouModelB38 */),
_Kg/* ouModelB36 */ = new T2(1,_Dp/* Model.Models.sDLIDelivered */,_Kf/* Model.Models.ouModelB37 */),
_Kh/* ouModelB35 */ = new T2(1,_DD/* Model.Models.sDLICollected */,_Kg/* Model.Models.ouModelB36 */),
_Ki/* ouModelB34 */ = new T2(1,_DN/* Model.Models.sApproved2 */,_Kh/* Model.Models.ouModelB35 */),
_Kj/* ouModelB33 */ = new T2(1,_DS/* Model.Models.sWorkup2 */,_Ki/* Model.Models.ouModelB34 */),
_Kk/* ouModelB32 */ = new T2(1,_E2/* Model.Models.sScheduled2 */,_Kj/* Model.Models.ouModelB33 */),
_Kl/* ouModelB31 */ = new T2(1,_Ec/* Model.Models.sChosen2 */,_Kk/* Model.Models.ouModelB32 */),
_Km/* ouModelB30 */ = new T2(1,_El/* Model.Models.sExam4 */,_Kl/* Model.Models.ouModelB31 */),
_Kn/* ouModelB29 */ = new T2(1,_GF/* Model.Models.sBM */,_Km/* Model.Models.ouModelB30 */),
_Ko/* ouModelB28 */ = new T2(1,_GQ/* Model.Models.sBMDelivered */,_Kn/* Model.Models.ouModelB29 */),
_Kp/* ouModelB27 */ = new T2(1,_H4/* Model.Models.sBMCollected */,_Ko/* Model.Models.ouModelB28 */),
_Kq/* ouModelB26 */ = new T2(1,_FP/* Model.Models.sPBSC */,_Kp/* Model.Models.ouModelB27 */),
_Kr/* ouModelB25 */ = new T2(1,_G1/* Model.Models.sPBSCDelivered */,_Kq/* Model.Models.ouModelB26 */),
_Ks/* ouModelB24 */ = new T2(1,_Gc/* Model.Models.sPBSCCollected */,_Kr/* Model.Models.ouModelB25 */),
_Kt/* ouModelB23 */ = new T2(1,_Go/* Model.Models.sPBSCPrep */,_Ks/* Model.Models.ouModelB24 */),
_Ku/* ouModelB22 */ = new T2(1,_Hi/* Model.Models.sApproved */,_Kt/* Model.Models.ouModelB23 */),
_Kv/* ouModelB21 */ = new T2(1,_HH/* Model.Models.sWorkup */,_Ku/* Model.Models.ouModelB22 */),
_Kw/* ouModelB20 */ = new T2(1,_Ex/* Model.Models.sPermDeferred */,_Kv/* Model.Models.ouModelB21 */),
_Kx/* ouModelB19 */ = new T2(1,_K7/* Model.Models.sTempDeferred */,_Kw/* Model.Models.ouModelB20 */),
_Ky/* ouModelB18 */ = new T2(1,_HO/* Model.Models.sScheduled */,_Kx/* Model.Models.ouModelB19 */),
_Kz/* ouModelB17 */ = new T2(1,_HU/* Model.Models.sChosen */,_Ky/* Model.Models.ouModelB18 */),
_KA/* ouModelB16 */ = new T2(1,_I3/* Model.Models.sDonorReserved */,_Kz/* Model.Models.ouModelB17 */),
_KB/* ouModelB15 */ = new T2(1,_Id/* Model.Models.sVerified */,_KA/* Model.Models.ouModelB16 */),
_KC/* ouModelB14 */ = new T2(1,_Ir/* Model.Models.sHLAVerified */,_KB/* Model.Models.ouModelB15 */),
_KD/* ouModelB13 */ = new T2(1,_ID/* Model.Models.sScreened */,_KC/* Model.Models.ouModelB14 */),
_KE/* ouModelB12 */ = new T2(1,_IN/* Model.Models.sDraw3 */,_KD/* Model.Models.ouModelB13 */),
_KF/* ouModelB11 */ = new T2(1,_Jp/* Model.Models.sHLATyped2_3 */,_KE/* Model.Models.ouModelB12 */),
_KG/* ouModelB10 */ = new T2(1,_Jf/* Model.Models.sHLATyped2_2 */,_KF/* Model.Models.ouModelB11 */),
_KH/* ouModelB9 */ = new T2(1,_J4/* Model.Models.sHLATyped2_1 */,_KG/* Model.Models.ouModelB10 */),
_KI/* ouModelB8 */ = new T2(1,_Jz/* Model.Models.sDraw2 */,_KH/* Model.Models.ouModelB9 */),
_KJ/* ouModelB7 */ = new T2(1,_IZ/* Model.Models.sSelection */,_KI/* Model.Models.ouModelB8 */),
_KK/* ouModelB6 */ = new T2(1,_JU/* Model.Models.sFound */,_KJ/* Model.Models.ouModelB7 */),
_KL/* sSearchRequested1 */ = new T2(1,_JZ/* Model.Models.sSearchRequested2 */,_4x/* GHC.Types.[] */),
_KM/* sSearchRequested11 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor Search Initiated"));
}),
_KN/* sSearchRequested10 */ = new T2(1,_g7/* Model.Elements.iSearch */,_4x/* GHC.Types.[] */),
_KO/* sSearchRequested9 */ = new T2(1,_fR/* Model.Elements.iPatient */,_KN/* Model.Models.sSearchRequested10 */),
_KP/* sSearchRequested8 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_KO/* Model.Models.sSearchRequested9 */, _D3/* B1 */);});
},
_KQ/* sSearchRequested7 */ = new T2(1,_KP/* Model.Models.sSearchRequested8 */,_4x/* GHC.Types.[] */),
_KR/* sSearchRequested */ = new T4(0,_KM/* Model.Models.sSearchRequested11 */,_KQ/* Model.Models.sSearchRequested7 */,_KL/* Model.Models.sSearchRequested1 */,_4x/* GHC.Types.[] */),
_KS/* ouModelB5 */ = new T2(1,_KR/* Model.Models.sSearchRequested */,_KK/* Model.Models.ouModelB6 */),
_KT/* ouModelB4 */ = new T2(1,_K2/* Model.Models.sAvailable */,_KS/* Model.Models.ouModelB5 */),
_KU/* sAspirantDonor5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Initial Histocompatibility Testing"));
}),
_KV/* sAspirantDonor4 */ = new T2(0,_KU/* Model.Models.sAspirantDonor5 */,_K2/* Model.Models.sAvailable */),
_KW/* sAspirantDonor3 */ = new T2(1,_KV/* Model.Models.sAspirantDonor4 */,_4x/* GHC.Types.[] */),
_KX/* sAspirantDonor2 */ = new T2(0,_8S/* GHC.Base.Nothing */,_KW/* Model.Models.sAspirantDonor3 */),
_KY/* sAspirantDonor1 */ = new T2(1,_KX/* Model.Models.sAspirantDonor2 */,_4x/* GHC.Types.[] */),
_KZ/* sAspirantDonor19 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("A Person becomes\nAspirant Donor"));
}),
_L0/* sAspirantDonor18 */ = new T2(1,_i1/* Model.Elements.iMedicalAssessmentResults1 */,_4x/* GHC.Types.[] */),
_L1/* sAspirantDonor17 */ = new T2(1,_hU/* Model.Elements.iMedicalAssessmentResults */,_L0/* Model.Models.sAspirantDonor18 */),
_L2/* sAspirantDonor16 */ = new T2(1,_9C/* Model.Elements.iMedicalAssessment1 */,_L1/* Model.Models.sAspirantDonor17 */),
_L3/* sAspirantDonor15 */ = new T2(1,_hT/* Model.Elements.iMedicalAssessment */,_L2/* Model.Models.sAspirantDonor16 */),
_L4/* sAspirantDonor14 */ = new T2(1,_ag/* Model.Elements.iBloodSample */,_L3/* Model.Models.sAspirantDonor15 */),
_L5/* sAspirantDonor13 */ = new T2(1,_9L/* Model.Elements.iBloodSampleDraw1 */,_L4/* Model.Models.sAspirantDonor14 */),
_L6/* sAspirantDonor12 */ = new T2(1,_9v/* Model.Elements.iInitialExamination */,_L5/* Model.Models.sAspirantDonor13 */),
_L7/* sAspirantDonor11 */ = new T2(1,_gn/* Model.Elements.iExamination */,_L6/* Model.Models.sAspirantDonor12 */),
_L8/* sAspirantDonor10 */ = new T2(1,_9m/* Model.Elements.iRegistration */,_L7/* Model.Models.sAspirantDonor11 */),
_L9/* sAspirantDonor9 */ = new T2(1,_9j/* Model.Elements.iRecruitment */,_L8/* Model.Models.sAspirantDonor10 */),
_La/* sAspirantDonor8 */ = new T2(1,_gT/* Model.Elements.iDonorAspirant */,_L9/* Model.Models.sAspirantDonor9 */),
_Lb/* sAspirantDonor7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_La/* Model.Models.sAspirantDonor8 */, _D3/* B1 */);});
},
_Lc/* sAspirantDonor6 */ = new T2(1,_Lb/* Model.Models.sAspirantDonor7 */,_4x/* GHC.Types.[] */),
_Ld/* sAspirantDonor */ = new T4(0,_KZ/* Model.Models.sAspirantDonor19 */,_Lc/* Model.Models.sAspirantDonor6 */,_KY/* Model.Models.sAspirantDonor1 */,_K1/* Texts.sIssuedRegistrationT */),
_Le/* ouModelB3 */ = new T2(1,_Ld/* Model.Models.sAspirantDonor */,_KT/* Model.Models.ouModelB4 */),
_Lf/* sNeeds5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Search Initiating"));
}),
_Lg/* sNeeds4 */ = new T2(0,_Lf/* Model.Models.sNeeds5 */,_KR/* Model.Models.sSearchRequested */),
_Lh/* sNeeds3 */ = new T2(1,_Lg/* Model.Models.sNeeds4 */,_4x/* GHC.Types.[] */),
_Li/* sNeeds2 */ = new T2(0,_8S/* GHC.Base.Nothing */,_Lh/* Model.Models.sNeeds3 */),
_Lj/* sNeeds1 */ = new T2(1,_Li/* Model.Models.sNeeds2 */,_4x/* GHC.Types.[] */),
_Lk/* sNeeds13 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("A Person Needs\nUnrelated\nDonor Transplanation"));
}),
_Ll/* sNeeds12 */ = new T2(1,_fS/* Model.Elements.iPatientRegistration */,_4x/* GHC.Types.[] */),
_Lm/* sNeeds11 */ = new T2(1,_qc/* Model.Elements.iGenotypeEvaluated2 */,_Ll/* Model.Models.sNeeds12 */),
_Ln/* sNeeds10 */ = new T2(1,_fe/* Model.Elements.iGenotype2 */,_Lm/* Model.Models.sNeeds11 */),
_Lo/* sNeeds9 */ = new T2(1,_fR/* Model.Elements.iPatient */,_Ln/* Model.Models.sNeeds10 */),
_Lp/* sNeeds8 */ = new T2(1,_fh/* Model.Elements.iPerson2 */,_Lo/* Model.Models.sNeeds9 */),
_Lq/* sNeeds7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_Lp/* Model.Models.sNeeds8 */, _D3/* B1 */);});
},
_Lr/* sNeeds6 */ = new T2(1,_Lq/* Model.Models.sNeeds7 */,_4x/* GHC.Types.[] */),
_Ls/* sNeedsT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("If a Person needs unrelated donor transplantation, via search request he is include in transplant program.<br/><em><em>WMDA Definition:</em></em><br/>Transplant centre (TC) - A medical facility where a patient (recipient) receives a transplant (graft) with HSC from an unrelated donor or from an umbilical cord blood unit. The TC oversees the immediate medical treatment and provides long-term follow-up of the recipient. The search unit undertakes the search for an unrelated donor for specific patients using criteria defined and documented by the TC. This entity may be contained within a TC or may be separate from the TC. If separate, the search unit may coordinate searches for one or several TCs. In the standards, reference to a TC should be interpreted as a TC and/or a search unit as appropriate. Transplant centres/search units seeking an international donor work through the registry in their country.<br/><em><em>WMDA Standard:</em></em><br/>1.07 The registry must ensure that transplant centres affiliated with the registry and requesting a donor from another country meet standards designed to ensure that donation of HSC will only be requested for patients for whom transplantation is a medically acceptable procedure. The nature of these affiliations and the duties and responsibilities of each entity must be documented in a written agreement. <br/>1.07.1 These transplant centre standards should be defined by an appropriate national or international organisation. In absence of such standards, they must be defined by the registry.<br/>1.07.2 The standards for transplant centres must be readily accessible to healthcare professionals involved in HSC transplantation."));
}),
_Lt/* sNeeds */ = new T4(0,_Lk/* Model.Models.sNeeds13 */,_Lr/* Model.Models.sNeeds6 */,_Lj/* Model.Models.sNeeds1 */,_Ls/* Texts.sNeedsT */),
_Lu/* ouModelB2 */ = new T2(1,_Lt/* Model.Models.sNeeds */,_Le/* Model.Models.ouModelB3 */),
_Lv/* sInitial5 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("Donor\nRecruitment"));
}),
_Lw/* sInitial4 */ = new T2(0,_Lv/* Model.Models.sInitial5 */,_Ld/* Model.Models.sAspirantDonor */),
_Lx/* sInitial3 */ = new T2(1,_Lw/* Model.Models.sInitial4 */,_4x/* GHC.Types.[] */),
_Ly/* sInitial2 */ = new T2(0,_8S/* GHC.Base.Nothing */,_Lx/* Model.Models.sInitial3 */),
_Lz/* sInitial1 */ = new T2(1,_Ly/* Model.Models.sInitial2 */,_4x/* GHC.Types.[] */),
_LA/* sInitial20 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("A Person\nwants to\nbecome a Donor"));
}),
_LB/* sInitial19 */ = new T2(1,_g0/* Model.Elements.iTransplantCentre */,_4x/* GHC.Types.[] */),
_LC/* sInitial18 */ = new T2(1,_cP/* Model.Elements.iCollectionCentre */,_LB/* Model.Models.sInitial19 */),
_LD/* sInitial17 */ = new T2(1,_9b/* Model.Elements.iHLALaboratory */,_LC/* Model.Models.sInitial18 */),
_LE/* sInitial16 */ = new T2(1,_8W/* Model.Elements.iDonorCentre */,_LD/* Model.Models.sInitial17 */),
_LF/* sInitial15 */ = new T2(1,_8Z/* Model.Elements.iDonorRegistry */,_LE/* Model.Models.sInitial16 */),
_LG/* sInitial14 */ = new T2(1,_rk/* Model.Elements.iDLI */,_LF/* Model.Models.sInitial15 */),
_LH/* sInitial13 */ = new T2(1,_rg/* Model.Elements.iPBSC */,_LG/* Model.Models.sInitial14 */),
_LI/* sInitial12 */ = new T2(1,_ri/* Model.Elements.iBM */,_LH/* Model.Models.sInitial13 */),
_LJ/* sInitial11 */ = new T2(1,_nO/* Model.Elements.iHSC */,_LI/* Model.Models.sInitial12 */),
_LK/* sInitial10 */ = new T2(1,_q1/* Model.Elements.iGenotypeUnknown1 */,_LJ/* Model.Models.sInitial11 */),
_LL/* sInitial9 */ = new T2(1,_f7/* Model.Elements.iGenotype1 */,_LK/* Model.Models.sInitial10 */),
_LM/* sInitial8 */ = new T2(1,_fa/* Model.Elements.iPerson1 */,_LL/* Model.Models.sInitial9 */),
_LN/* sInitial7 */ = function(_D3/* B1 */){
  return new F(function(){return _2G/* GHC.Base.++ */(_LM/* Model.Models.sInitial8 */, _D3/* B1 */);});
},
_LO/* sInitial6 */ = new T2(1,_LN/* Model.Models.sInitial7 */,_4x/* GHC.Types.[] */),
_LP/* sInitialT */ = new T(function(){
  return B(unCStr/* EXTERNAL */("If a Person wants to become a part of the Registry as a Donor, they must undertake recruitment process.<br/><em><em>WMDA Definition:</em></em><br/>World Marrow Donor Association (WMDA) - A non-profit association that fosters international collaboration to facilitate the exchange of high quality HSC for clinical transplantation worldwide and to promote the interests of donors.<br/>Donor - A person who is the source of cells or tissue for a cellular therapy product. Donors are unrelated to the patient seeking a transplant.<br/>(the model/simulation refer to specific donor type according to the WMDA Standards - volunteer donors of HSC who have passed a minimum age established by national law or their eighteenth (18th) birthday when no regulation exist)<br/>HSC - Haematopoietic stem cells are the cells, which give rise to blood and immune system cells. These cells are found in bone marrow, growth factor stimulated peripheral blood, and umbilical cord blood.<br/>Registry - An organisation responsible for coordination of the search for haematopoietic stem cells from donors (including cord blood) unrelated to the potential recipient.<br/><em><em>WMDA Standard:</em></em><br/>1.06 If a registry relies on an independent donor centre or cord blood bank to recruit and characterise donors/umbilical cord blood units, the registry must ensure that the donor centre/cord blood bank complies with relevant WMDA Standards. The nature of these affiliations and the duties and responsibilities of each entity must be documented in a written agreement.<br/>3.01 Entities involved in donor recruitment must meet any relevant international and national laws and regulations.<br/>3.02 The recruitment of volunteer donors must be performed under the direction of individuals who are experienced in recruitment of donors and in management activities including education, consenting, counselling, confidentiality, and medical screening. These individuals must be appropriately qualified and provided with timely and relevant training. The training and experience of these individuals must be documented.<br/>3.03 The willingness to become a donor must be the individual choice of each donor, that is, donations must be voluntary. Donors must be willing to donate on behalf of any patient being treated in any part of the world. Donors must not be paid for their donation and may be reimbursed for expenses incurred during the donation process."));
}),
_LQ/* sInitial */ = new T4(0,_LA/* Model.Models.sInitial20 */,_LO/* Model.Models.sInitial6 */,_Lz/* Model.Models.sInitial1 */,_LP/* Texts.sInitialT */),
_LR/* initUfoBDiag2 */ = function(_LS/* sd6o */, _/* EXTERNAL */){
  var _LT/* sd6r */ = function(_LU/* sd6y */, _/* EXTERNAL */){
    while(1){
      var _LV/* sd6A */ = E(_LU/* sd6y */);
      if(!_LV/* sd6A */._){
        return _0/* GHC.Tuple.() */;
      }else{
        var _LW/* sd6D */ = B(_yt/* Main.addEvents1 */(_LS/* sd6o */, _LV/* sd6A */.a, _/* EXTERNAL */));
        _LU/* sd6y */ = _LV/* sd6A */.b;
        continue;
      }
    }
  },
  _LX/* sd6G */ = B((function(_LY/* sd6s */, _LZ/* sd6t */, _/* EXTERNAL */){
    var _M0/* sd6v */ = B(_yt/* Main.addEvents1 */(_LS/* sd6o */, _LY/* sd6s */, _/* EXTERNAL */));
    return new F(function(){return _LT/* sd6r */(_LZ/* sd6t */, _/* EXTERNAL */);});
  })(_LQ/* Model.Models.sInitial */, _Lu/* Model.Models.ouModelB2 */, _/* EXTERNAL */));
  return _0/* GHC.Tuple.() */;
},
_M1/* onLoad2 */ = "(function (ev, jq) { jq[0].addEventListener(\'load\', ev); })",
_M2/* onLoad1 */ = function(_M3/* s9NY */, _M4/* s9NZ */, _/* EXTERNAL */){
  var _M5/* s9Ob */ = __createJSFunc/* EXTERNAL */(2, function(_M6/* s9O2 */, _/* EXTERNAL */){
    var _M7/* s9O4 */ = B(A2(E(_M3/* s9NY */),_M6/* s9O2 */, _/* EXTERNAL */));
    return _xX/* Haste.Prim.Any.jsNull */;
  }),
  _M8/* s9Oe */ = E(_M4/* s9NZ */),
  _M9/* s9Oj */ = eval/* EXTERNAL */(E(_M1/* JQuery.onLoad2 */)),
  _Ma/* s9Or */ = __app2/* EXTERNAL */(E(_M9/* s9Oj */), _M5/* s9Ob */, _M8/* s9Oe */);
  return _M8/* s9Oe */;
},
_Mb/* onResize2 */ = "(function (ev, jq) { jq.resize(ev); })",
_Mc/* onResize1 */ = function(_Md/* s9Ou */, _Me/* s9Ov */, _/* EXTERNAL */){
  var _Mf/* s9OH */ = __createJSFunc/* EXTERNAL */(2, function(_Mg/* s9Oy */, _/* EXTERNAL */){
    var _Mh/* s9OA */ = B(A2(E(_Md/* s9Ou */),_Mg/* s9Oy */, _/* EXTERNAL */));
    return _xX/* Haste.Prim.Any.jsNull */;
  }),
  _Mi/* s9OK */ = E(_Me/* s9Ov */),
  _Mj/* s9OP */ = eval/* EXTERNAL */(E(_Mb/* JQuery.onResize2 */)),
  _Mk/* s9OX */ = __app2/* EXTERNAL */(E(_Mj/* s9OP */), _Mf/* s9OH */, _Mi/* s9OK */);
  return _Mi/* s9OK */;
},
_Ml/* ready_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function (f) { jQuery(document).ready(f); })");
}),
_Mm/* getHeight_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function (jq) { return jq.innerHeight(); })");
}),
_Mn/* resizeDiags2 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ufoa-box"));
}),
_Mo/* resizeDiags3 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ufoa-inst-box"));
}),
_Mp/* resizeDiags4 */ = new T(function(){
  return B(unCStr/* EXTERNAL */("ufob-box"));
}),
_Mq/* setHeight_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function (val, jq) { jq.innerHeight(val); return jq; })");
}),
_Mr/* $wa1 */ = function(_/* EXTERNAL */){
  var _Ms/* scWL */ = __app0/* EXTERNAL */(E(_yT/* JQuery.getWindow_f1 */)),
  _Mt/* scWR */ = __app1/* EXTERNAL */(E(_Mm/* JQuery.getHeight_f1 */), _Ms/* scWL */),
  _Mu/* scWU */ = B(_3t/* JQuery.selectById1 */(_Mp/* Main.resizeDiags4 */, _/* EXTERNAL */)),
  _Mv/* scWY */ = Number/* EXTERNAL */(_Mt/* scWR */),
  _Mw/* scX2 */ = jsTrunc/* EXTERNAL */(_Mv/* scWY */),
  _Mx/* scX5 */ = _Mw/* scX2 */-117|0,
  _My/* scXa */ = E(_Mq/* JQuery.setHeight_f1 */),
  _Mz/* scXd */ = __app2/* EXTERNAL */(_My/* scXa */, _Mx/* scX5 */, E(_Mu/* scWU */)),
  _MA/* scXg */ = B(_3t/* JQuery.selectById1 */(_Mo/* Main.resizeDiags3 */, _/* EXTERNAL */)),
  _MB/* scXm */ = __app2/* EXTERNAL */(_My/* scXa */, _Mx/* scX5 */, E(_MA/* scXg */)),
  _MC/* scXp */ = B(_3t/* JQuery.selectById1 */(_Mn/* Main.resizeDiags2 */, _/* EXTERNAL */)),
  _MD/* scXv */ = __app2/* EXTERNAL */(_My/* scXa */, _Mx/* scX5 */, E(_MC/* scXp */));
  return _0/* GHC.Tuple.() */;
},
_ME/* resizeDiags1 */ = function(_MF/* scXy */, _/* EXTERNAL */){
  return new F(function(){return _Mr/* Main.$wa1 */(_/* EXTERNAL */);});
},
_MG/* getWidth_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function (jq) { return jq.width(); })");
}),
_MH/* setWidth_f1 */ = new T(function(){
  return eval/* EXTERNAL */("(function (val, jq) { jq.width(val); return jq; })");
}),
_MI/* resizeLeft1 */ = function(_/* EXTERNAL */){
  var _MJ/* scUP */ = B(_3t/* JQuery.selectById1 */(_Mp/* Main.resizeDiags4 */, _/* EXTERNAL */)),
  _MK/* scUU */ = E(_MG/* JQuery.getWidth_f1 */),
  _ML/* scUX */ = __app1/* EXTERNAL */(_MK/* scUU */, E(_MJ/* scUP */)),
  _MM/* scV0 */ = B(_3t/* JQuery.selectById1 */(_Mo/* Main.resizeDiags3 */, _/* EXTERNAL */)),
  _MN/* scV6 */ = __app1/* EXTERNAL */(_MK/* scUU */, E(_MM/* scV0 */)),
  _MO/* scV9 */ = B(_3t/* JQuery.selectById1 */(_Mp/* Main.resizeDiags4 */, _/* EXTERNAL */)),
  _MP/* scVd */ = Number/* EXTERNAL */(_ML/* scUX */),
  _MQ/* scVh */ = jsTrunc/* EXTERNAL */(_MP/* scVd */),
  _MR/* scVp */ = E(_MH/* JQuery.setWidth_f1 */),
  _MS/* scVs */ = __app2/* EXTERNAL */(_MR/* scVp */, _MQ/* scVh */-100|0, E(_MO/* scV9 */)),
  _MT/* scVv */ = B(_3t/* JQuery.selectById1 */(_Mo/* Main.resizeDiags3 */, _/* EXTERNAL */)),
  _MU/* scVz */ = Number/* EXTERNAL */(_MN/* scV6 */),
  _MV/* scVD */ = jsTrunc/* EXTERNAL */(_MU/* scVz */),
  _MW/* scVM */ = __app2/* EXTERNAL */(_MR/* scVp */, _MV/* scVD */+100|0, E(_MT/* scVv */));
  return _0/* GHC.Tuple.() */;
},
_MX/* resizeRight1 */ = function(_/* EXTERNAL */){
  var _MY/* scTO */ = B(_3t/* JQuery.selectById1 */(_Mp/* Main.resizeDiags4 */, _/* EXTERNAL */)),
  _MZ/* scTT */ = E(_MG/* JQuery.getWidth_f1 */),
  _N0/* scTW */ = __app1/* EXTERNAL */(_MZ/* scTT */, E(_MY/* scTO */)),
  _N1/* scTZ */ = B(_3t/* JQuery.selectById1 */(_Mo/* Main.resizeDiags3 */, _/* EXTERNAL */)),
  _N2/* scU5 */ = __app1/* EXTERNAL */(_MZ/* scTT */, E(_N1/* scTZ */)),
  _N3/* scU8 */ = B(_3t/* JQuery.selectById1 */(_Mp/* Main.resizeDiags4 */, _/* EXTERNAL */)),
  _N4/* scUc */ = Number/* EXTERNAL */(_N0/* scTW */),
  _N5/* scUg */ = jsTrunc/* EXTERNAL */(_N4/* scUc */),
  _N6/* scUo */ = E(_MH/* JQuery.setWidth_f1 */),
  _N7/* scUr */ = __app2/* EXTERNAL */(_N6/* scUo */, _N5/* scUg */+100|0, E(_N3/* scU8 */)),
  _N8/* scUu */ = B(_3t/* JQuery.selectById1 */(_Mo/* Main.resizeDiags3 */, _/* EXTERNAL */)),
  _N9/* scUy */ = Number/* EXTERNAL */(_N2/* scU5 */),
  _Na/* scUC */ = jsTrunc/* EXTERNAL */(_N9/* scUy */),
  _Nb/* scUL */ = __app2/* EXTERNAL */(_N6/* scUo */, _Na/* scUC */-100|0, E(_N8/* scUu */));
  return _0/* GHC.Tuple.() */;
},
_Nc/* main1 */ = function(_/* EXTERNAL */){
  var _Nd/* sd8x */ = function(_/* EXTERNAL */){
    var _Ne/* sd70 */ = __createJSFunc/* EXTERNAL */(0, function(_/* EXTERNAL */){
      var _Nf/* sd6T */ = B(_MI/* Main.resizeLeft1 */(_/* EXTERNAL */));
      return _xX/* Haste.Prim.Any.jsNull */;
    }),
    _Ng/* sd75 */ = "(function(s,f){Haste[s] = f;})",
    _Nh/* sd79 */ = eval/* EXTERNAL */(_Ng/* sd75 */),
    _Ni/* sd7h */ = __app2/* EXTERNAL */(E(_Nh/* sd79 */), "resizeLeft", _Ne/* sd70 */),
    _Nj/* sd7x */ = __createJSFunc/* EXTERNAL */(0, function(_/* EXTERNAL */){
      var _Nk/* sd7q */ = B(_MX/* Main.resizeRight1 */(_/* EXTERNAL */));
      return _xX/* Haste.Prim.Any.jsNull */;
    }),
    _Nl/* sd7B */ = eval/* EXTERNAL */(_Ng/* sd75 */),
    _Nm/* sd7J */ = __app2/* EXTERNAL */(E(_Nl/* sd7B */), "resizeRight", _Nj/* sd7x */),
    _Nn/* sd7P */ = __app0/* EXTERNAL */(E(_yT/* JQuery.getWindow_f1 */)),
    _No/* sd7T */ = B(_Mc/* JQuery.onResize1 */(_ME/* Main.resizeDiags1 */, _Nn/* sd7P */, _/* EXTERNAL */)),
    _Np/* sd7Y */ = B(_3/* JQuery.$wa18 */(E(_No/* sd7T */), _/* EXTERNAL */)),
    _Nq/* sd81 */ = nMV/* EXTERNAL */(_yV/* Metamodel.UfoB.initModelState */),
    _Nr/* sd83 */ = _Nq/* sd81 */,
    _Ns/* sd84 */ = B(_3t/* JQuery.selectById1 */(_xu/* Main.diagBJq2 */, _/* EXTERNAL */)),
    _Nt/* sd8b */ = B(_M2/* JQuery.onLoad1 */(function(_Nu/* sd88 */, _/* EXTERNAL */){
      return new F(function(){return _LR/* Main.initUfoBDiag2 */(_Nr/* sd83 */, _/* EXTERNAL */);});
    }, _Ns/* sd84 */, _/* EXTERNAL */)),
    _Nv/* sd8f */ = function(_Nw/* sd8m */, _/* EXTERNAL */){
      while(1){
        var _Nx/* sd8o */ = E(_Nw/* sd8m */);
        if(!_Nx/* sd8o */._){
          return _0/* GHC.Tuple.() */;
        }else{
          var _Ny/* sd8r */ = B(_yt/* Main.addEvents1 */(_Nr/* sd83 */, _Nx/* sd8o */.a, _/* EXTERNAL */));
          _Nw/* sd8m */ = _Nx/* sd8o */.b;
          continue;
        }
      }
    },
    _Nz/* sd8u */ = B((function(_NA/* sd8g */, _NB/* sd8h */, _/* EXTERNAL */){
      var _NC/* sd8j */ = B(_yt/* Main.addEvents1 */(_Nr/* sd83 */, _NA/* sd8g */, _/* EXTERNAL */));
      return new F(function(){return _Nv/* sd8f */(_NB/* sd8h */, _/* EXTERNAL */);});
    })(_LQ/* Model.Models.sInitial */, _Lu/* Model.Models.ouModelB2 */, _/* EXTERNAL */));
    return _xX/* Haste.Prim.Any.jsNull */;
  },
  _ND/* sd8B */ = __createJSFunc/* EXTERNAL */(0, E(_Nd/* sd8x */)),
  _NE/* sd8H */ = __app1/* EXTERNAL */(E(_Ml/* JQuery.ready_f1 */), _ND/* sd8B */);
  return new F(function(){return _1/* Haste.Prim.Any.$fFromAny()4 */(_/* EXTERNAL */);});
},
_NF/* main */ = function(_/* EXTERNAL */){
  return new F(function(){return _Nc/* Main.main1 */(_/* EXTERNAL */);});
};

var hasteMain = function() {B(A(_NF, [0]));};window.onload = hasteMain;