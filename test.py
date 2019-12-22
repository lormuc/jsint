import traceback
import sys

from jsint import *


class t_tester:
    def __init__(_):
        _.tests = []

    def add_test(_, code, expected_result):
        _.tests.append((code, expected_result))

    def run_test(_, code, expected):
        try:
            value = interpret(code).value
            if value == expected:
                return True
            print(code)
            print("-- expected " + repr(expected) + ", got " + repr(value))
        except:
            print(code)
            traceback_lines = traceback.format_exc().splitlines()
            if len(traceback_lines) > 10:
                traceback_lines = traceback_lines[-9:]
                print("  ...")
            for line in traceback_lines:
                print(line)
        return False

    def run_tests(_, fail_limit=None):
        print("running tests...")
        fail_cnt = 0
        success_cnt = 0
        for i in range(len(_.tests)):
            print("-- test " + str(i))
            success = _.run_test(_.tests[i][0], _.tests[i][1])
            fail_cnt += int(not success)
            success_cnt += int(success)
            if success:
                print("-- success")
            else:
                print("-- failure")
            print("-" * 79)
            if (fail_limit is not None) and (fail_cnt >= fail_limit):
                break
        print()
        print(f"{success_cnt} succeeded, {fail_cnt} failed")
        untested_cnt = len(_.tests) - fail_cnt - success_cnt
        if untested_cnt != 0:
            print(f"{untested_cnt} were untested")


tester = t_tester()

tester.add_test("""
typeof 4.5;
""", "number")

tester.add_test("""
- (10 - 2);
""", -8.0)

tester.add_test("""
3 > 2;
""", True)

tester.add_test("""
3 < 2;
""", False)

tester.add_test("""
'abc' < 'xyz';
""", True)

tester.add_test("""
true && false;
""", False)

tester.add_test("""
true && true;
""", True)

tester.add_test("""
true || false;
""", True)

tester.add_test("""
false || false;
""", False)

tester.add_test("""
!true;
""", False)

tester.add_test("""
!false;
""", True)

tester.add_test("""
1 + 1 == 2 && 10 * 10 > 50;
""", True)

tester.add_test("""
true ? 1 : 2;
""", 1.0)

tester.add_test("""
false ? 1 : 2;
""", 2.0)

tester.add_test("""
8 * null;
""", 0.0)

tester.add_test("""
'5' - 1;
""", 4.0)

tester.add_test("""
'5' + 1;
""", "51")

tester.add_test("""
'five' * 2 == 'five' * 2;
""", False)

tester.add_test("""
false == 0;
""", True)

tester.add_test("""
null == 0;
""", False)

tester.add_test("""
0 / 0 == 0 / 0;
""", False)

tester.add_test("""
null;
""", js_null)

tester.add_test("""
this.a = 3;
this["a"];
""", 3.0)

tester.add_test("""
void (2 * 8);
""", js_undefined)

tester.add_test("""
typeof (void (0));
""", "undefined")

tester.add_test("""
typeof null;
""", "object")

tester.add_test("""
typeof true;
""", "boolean")

tester.add_test("""
typeof "aaaa";
""", "string")

tester.add_test("""
function x() {}
typeof(x);
""", "function")

tester.add_test("""
function x() {}
typeof(this);
""", "object")

tester.add_test("""
var x = 0;
++x;
var y = ++x;
y;
""", 2.0)

tester.add_test("""
var x = 0;
--x;
var y = --x;
y;
""", -2.0)

tester.add_test("""
+true;
""", 1.0)

tester.add_test("""
-0;
""", -0.0)

tester.add_test("""
~1;
""", -2.0)

tester.add_test("""
!0;
""", True)

tester.add_test("""
function fib(x) {
 if (x == 0) {
  return 0;
 }
 if (x == 1) {
  return 1;
 }
 return fib(x-1) + fib(x-2);
}
fib(10);
""", 55.0)

tester.add_test("""
var result = 1;
var counter = 0;
while (counter < 10) {
 result *= 2;
 counter++;
}
result;
""", 1024.0)

tester.add_test("""
var result = 1;
for (var i = 0; i < 12; i++) {
 result *= 2;
}
""", 4096.0)

tester.add_test("""
var result = -1;
for (var i = 0; i < 100; i++) {
 if (i * i == 144) {
  result = i;
  break;
 }
}
i;
""", 12.0)

tester.add_test("""
function f(x) {
 return x + 1;
}
f(0);
""", 1.0)

tester.add_test("""
function fun() {
 var result = "";
 for (var i = 0; i < arguments.length; i++) {
  result += arguments[i];
 }
 return result;
}
fun("a", "bc", "def");
""", "abcdef")

tester.add_test("""
typeof abc;
""", "undefined")

tester.add_test("""
function minus(a, b) {
 if (typeof(b) == "undefined") {
  return -a;
 }
 return a - b;
}
minus(minus(4, 5));
""", 1.0)

tester.add_test("""
function call_n_times(fun, n) {
 if (n == 0) {
  return;
 }
 fun();
 call_n_times(fun, n-1);
}
var x = 0;
function inc() {
 x++;
}
call_n_times(inc, 4);
x;
""", 4.0)

tester.add_test("""
Infinity;
""", math.inf)

tester.add_test("""
-Infinity;
""", -math.inf)

tester.add_test("""
NaN != NaN;
""", True)

tester.add_test("""
eval("4;");
""", 4.0)

tester.add_test("""
eval(3945);
""", 3945.0)

tester.add_test("""
var x = 9, y = 3;
eval("x + y + 1;");
""", 13.0)

tester.add_test("""
var z = '"abc";';
eval(z);
""", "abc")

tester.add_test("""
eval("if (0) {} else { 49; }");
""", 49.0)

tester.add_test("""
eval.length;
""", 1.0)

tester.add_test("""
var o = Object();
o.a = 7;
o.a;
""", 7.0)

tester.add_test("""
var o = Object(null);
o.a = 7;
o.a;
""", 7.0)

tester.add_test("""
Object.length;
""", 1.0)

tester.add_test("""
var x = Object();
x.a = 4;
x.valueOf().a;
""", 4.0)

tester.add_test("""
var f = new Function("x", "x--; return x*x;");
f(9);
""", 64.0)

tester.add_test("""
var f = Function("x, y", "this.x = x; this.y = y;");
var p = new f(1, 2);
p.x + p.y;
""", 3.0)

tester.add_test("""
var f = Function("x, y", "x+1;");
typeof(f);
""", "function")

tester.add_test("""
var f = Function("x, y", "x+1;");
typeof(f);
""", "function")

tester.add_test("""
var f = Function("x, y", "x+1;");
f.length;
""", 2.0)

tester.add_test("""
var f = Function("x, y", "x+1;");
Function.prototype.length;
""", 0.0)

tester.add_test("""
var f = Function("x, y", "x+1;");
Function.prototype(9, 8, "abc");
""", js_undefined)

tester.add_test("""
arr = Array(1, 2, 3);
arr[0] + arr[1] + arr[2] + arr.length;
""", 9)

tester.add_test("""
arr = new Array(9);
arr.length;
""", 9)

tester.add_test("""
delete Array.length;
Array.length;
""", 1)

tester.add_test("""
Array.prototype.constructor == Array;
""", js_true)

tester.add_test("""
a = Array(0, 1, 2);
a[2]++;
a[2];
""", 3)

tester.add_test("""
a = Array(0, 1, 2, 3, 4);
a.length = 2;
a[2];
""", js_undefined)

tester.add_test("""
a = Array(0, 1, 2, 3, 4);
a[99] = 99;
a.length;
""", 100)

tester.add_test("""
a = Array(0, 1, 2, 3, 4);
a.join();
""", "0,1,2,3,4")

tester.add_test("""
a = Array(0, 1, 2, 3, 4);
a.join(" ");
""", "0 1 2 3 4")

tester.add_test("""
a = Array("a", "bc", "def");
a.toString();
""", "a,bc,def")

tester.add_test("""
Array.prototype.join.length;
""", 1)

tester.add_test("""
Array.prototype.toString.length;
""", 0)

tester.add_test("""
Array.prototype.reverse.length;
""", 0)

tester.add_test("""
var a = Array(9, 4, 3);
a.reverse();
a[0] == 3 && a[1] == 4 && a[2] == 9;
""", js_true)

tester.add_test("""
var a = Array(5);
a[0] = 4;
a[1] = 3;
a.reverse();
a[4] == 4 && a[3] == 3 && a[0] == undefined && a[1] == undefined;
""", js_true)

tester.add_test("""
typeof new String();
""", "object")

tester.add_test("""
var x = new String();
x.valueOf();
""", "")

tester.add_test("""
var x = new String("abcdef");
x.length;
""", 6)

tester.add_test("""
String(123);
""", "123")

tester.add_test("""
String();
""", "")

tester.add_test("""
(new String("abc")).toString();
""", "abc")

tester.add_test("""
(new String("abc")).valueOf();
""", "abc")

tester.add_test("""
"abc".charAt(1);
""", "b")

tester.add_test("""
"abc".charAt(1234);
""", "")

tester.add_test("""
"abc".charAt(-1234);
""", "")

tester.add_test("""
"01234567456".indexOf("456");
""", 4)

tester.add_test("""
"01234567456".indexOf("456", 9);
""", -1)

tester.add_test("""
"45674567".lastIndexOf("456");
""", 4)

tester.add_test("""
"45674567".lastIndexOf("456", 3);
""", 0)

tester.add_test("""
"45674567".lastIndexOf("456", 4);
""", 4)

tester.add_test("""
"ab,cd,efgh".split(",").join(" ");
""", "ab cd efgh")

tester.add_test("""
"abcdef".split()[0];
""", "abcdef")

tester.add_test("""
"abcdef".split("").join(" ");
""", "a b c d e f")

tester.add_test("""
"012345678".substring(3, 6);
""", "345")

tester.add_test("""
"012345678".substring(3);
""", "345678")

tester.add_test("""
"012345678".substring(7, 3);
""", "3456")

tester.add_test("""
"012345678".substring(-4, 100);
""", "012345678")

tester.add_test("""
"abcDEF".toUpperCase();
""", "ABCDEF")

tester.add_test("""
"abcDEF".toLowerCase();
""", "abcdef")

tester.add_test("""
(new Boolean()).valueOf();
""", js_false)

tester.add_test("""
Boolean(0);
""", js_false)

tester.add_test("""
false.toString();
""", "false")

tester.add_test("""
true.toString();
""", "true")

tester.add_test("""
true.valueOf();
""", js_true)

tester.add_test("""
Boolean.prototype.constructor.length;
""", 1)

tester.add_test("""
typeof Boolean;
""", "function")

tester.add_test("""
typeof Boolean;
""", "function")

tester.add_test("""
Boolean();
""", js_false)

tester.add_test("""
(new Boolean("")).valueOf();
""", js_false)

tester.add_test("""
Number();
""", t_js_number(0))

tester.add_test("""
Number("123");
""", t_js_number(123))

tester.add_test("""
(new Number("123")).valueOf();
""", t_js_number(123))

tester.add_test("""
(new Number()).valueOf();
""", t_js_number(0))

tester.add_test("""
345.345.toString();
""", "345.345")

tester.add_test("""
345.0.toString(10);
""", "345")

tester.add_test("""
0x345abc.toString(16);
""", "345abc")

tester.add_test("""
(-0xaaaa).toString(2);
""", "-1010101010101010")

tester.add_test("""
Number.length;
""", t_js_number(1))

tester.add_test("""
Number.prototype.valueOf();
""", t_js_number(0))

tester.add_test("""
Number.prototype.constructor == Number;
""", js_true)

tester.add_test("""
Number.MAX_VALUE;
""", t_js_number(sys.float_info.max))

tester.add_test("""
Number.MIN_VALUE / 2;
""", t_js_number(0))

tester.add_test("""
Number.MIN_VALUE.toString();
""", "5e-324")

tester.add_test("""
Number.NaN != Number.NaN;
""", js_true)

tester.add_test("""
Number.NEGATIVE_INFINITY;
""", -math.inf)

tester.add_test("""
Number.POSITIVE_INFINITY;
""", math.inf)

tester.add_test("""
Math.toString();
""", "[object Math]")

tester.add_test("""
Math.E;
""", math.e)

tester.add_test("""
Math.LN10;
""", math.log(10))

tester.add_test("""
Math.LN2;
""", math.log(2))

tester.add_test("""
Math.LOG2E;
""", math.log(math.e, 2))

tester.add_test("""
Math.LOG10E;
""", math.log(math.e, 10))

tester.add_test("""
Math.PI;
""", math.pi)

tester.add_test("""
Math.SQRT1_2;
""", math.sqrt(1/2))

tester.add_test("""
Math.SQRT2;
""", math.sqrt(2))

tester.add_test("""
Math.abs(-0.0);
""", 0.0)

tester.add_test("""
Math.abs(-4.5);
""", 4.5)

tester.add_test("""
Math.abs(-Infinity);
""", math.inf)

tester.add_test("""
Math.acos(0.5);
""", math.acos(0.5))

tester.add_test("""
Math.asin(0.5);
""", math.asin(0.5))

tester.add_test("""
Math.atan2(1, 2);
""", math.atan2(1, 2))

tester.add_test("""
Math.ceil(-1.5);
""", -1.0)

tester.add_test("""
Math.cos(0.5);
""", math.cos(0.5))

tester.add_test("""
Math.exp(0.5);
""", math.exp(0.5))

tester.add_test("""
Math.floor(4.5);
""", 4.0)

tester.add_test("""
Math.log(0.5);
""", math.log(0.5))

tester.add_test("""
Math.max(-4, 9);
""", 9.0)

tester.add_test("""
Math.min(-4, 9);
""", -4.0)

tester.add_test("""
Math.pow(0.5, 2);
""", 0.25)

tester.add_test("""
var x = Math.random();
var y = Math.random();
0 <= x && x < 1 && 0 <= y && y < 1;
""", js_true)

tester.add_test("""
Math.round(3.5);
""", 4.0)

tester.add_test("""
Math.round(-3.5);
""", -3.0)

tester.add_test("""
Math.sin(0.5);
""", math.sin(0.5))

tester.add_test("""
Math.sqrt(0.5);
""", math.sqrt(0.5))

tester.add_test("""
Math.tan(0.5);
""", math.tan(0.5))

tester.add_test("""
parseInt("   -0x0a#$%");
""", -10.0)

tester.add_test("""
parseInt("10", 7);
""", 7.0)

tester.add_test("""
var x = parseInt("abc", 7);
x != x;
""", js_true)

tester.add_test("""
parseInt("+123");
""", 123.0)

tester.add_test("""
parseInt.length;
""", 2.0)

tester.add_test("""
var x = (new Date(2100, 3, 3, 3)).valueOf() / 86400000;
Math.abs(x - 47574) < 2;
""", js_true)

tester.add_test("""
Date.UTC(1970, 0);
""", 0.0)

tester.add_test("""
Date.UTC(2010, 3, 4, 5, 6, 7, 8);
""", 1270357567008.0)

tester.add_test("""
Date.UTC(2010, 3, 4, 5, 6, 7, 8);
""", 1270357567008.0)

tester.add_test("""
Date.parse("1970-01-01 00:00:00 GMT+0000");
""", 0.0)

tester.add_test("""
Date.parse("2001-02-03 12:34:56 GMT+1234");
""", 981158456000.0)

tester.add_test("""
(new Date("2001-02-03 12:34:56 GMT+1234")).valueOf();
""", 981158456000.0)

tester.add_test("""
(new Date(123456789.0)).valueOf();
""", 123456789.0)

tester.add_test("""
Date.length;
""", 7.0)

tester.add_test("""
var now = new Date();
var val = now.valueOf();
Date.parse(now.toString()) == Math.floor(val / 1000) * 1000;
""", js_true)

tester.add_test("""
var now = new Date();
var val = now.valueOf();
Date.parse(now.toUTCString()) == Math.floor(val / 1000) * 1000;
""", js_true)

tester.add_test("""
(new Date(Date.UTC(2009, 10, 1))).getUTCFullYear();
""", 2009.0)

tester.add_test("""
(new Date(Date.UTC(2009, 10, 1))).getUTCMonth();
""", 10.0)

tester.add_test("""
(new Date(Date.UTC(2009, 10, 27, 23))).getUTCHours();
""", 23.0)

tester.add_test("""
(new Date(Date.UTC(2009, 10, 27, 23, 59))).getUTCMinutes();
""", 59.0)

tester.add_test("""
(new Date(Date.UTC(2009, 10, 27, 23, 59, 58))).getUTCSeconds();
""", 58.0)

tester.add_test("""
(new Date(Date.UTC(2009, 10, 27, 23, 59, 58, 999))).getUTCMilliseconds();
""", 999.0)

tester.add_test("""
var x = new Date(0.0);
x.setTime(12345.0);
x.valueOf();
""", 12345.0)

tester.add_test("""
var x = new Date(0.0);
x.setUTCMilliseconds(999);
x.setUTCSeconds(59);
x.setUTCMinutes(59);
x.setUTCHours(23);
x.setUTCDate(9);
x.setUTCMonth(9);
x.setUTCFullYear(2009);
x.valueOf();
""", 1255132799999.0)

tester.add_test("""
var a = Array(7, 8, 9, 3, 4, 0, 3, 5, 6, 1, 4, 5, 3, 9, 7, 9, 5, 0, 3, 1);
a.sort();
a.join(" ");
""", "0 0 1 1 3 3 3 3 4 4 5 5 5 6 7 7 8 9 9 9")

tester.add_test("""
function compare_fn(x, y) {
 return y - x;
}
var a = Array(0, 4, 5, 3, 9, 15, 44, 3, 100, 93, 54, 34);
a.sort(compare_fn);
a.join(" ");
""", "100 93 54 44 34 15 9 5 4 3 3 0")

if __name__ == "__main__":
    fail_limit = int(sys.argv[1]) if len(sys.argv) >= 2 else None
    tester.run_tests(fail_limit)
