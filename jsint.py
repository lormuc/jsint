from collections import namedtuple
import math
import copy
import sys
import traceback
import random
import time
import datetime

from parse import *


whitespace_chars = "\t \f\v\r\n"
nan = float("nan")


class t_runtime_error(Exception):
    pass


class t_unimplemented_property_error(t_runtime_error):
    def __init__(_):
        pass


class t_js_undefined:
    def __str__(_):
        return "undefined"

    def __repr__(_):
        return "t_js_undefined()"


js_undefined = t_js_undefined()


class t_js_null:
    def __str__(_):
        return "null"

    def __repr__(_):
        return "t_js_null()"


js_null = t_js_null()

t_js_boolean = bool
js_false = False
js_true = True

t_js_string = str
t_js_list = list
t_js_number = float
t_js_completion = namedtuple("completion", "kind value")
normal_completion = t_js_completion("normal", None)
t_js_reference = namedtuple("reference", "base property_name")
t_js_property = namedtuple("property",
                           "value read_only dont_enum dont_delete internal")


def is_nan(x):
    return math.isnan(x)


def is_false_number(x):
    return is_nan(x) or x == 0.0


def is_primitive_value(x):
    l = [t_js_undefined, t_js_null, t_js_boolean, t_js_number, t_js_string]
    return type(x) in l


class t_js_object:
    def __init__(_):
        _.props = {}
        _.internal_props = {}

    def has_internal(_, prop_name):
        return _.internal_props.get(prop_name) is not None

    def get_internal(_, prop_name):
        x = _.internal_props.get(prop_name)
        if x is None:
            raise t_runtime_error()
        return x.value

    def put_internal(_, prop_name, value):
        p = t_js_property(value, False, False, False, True)
        _.internal_props[prop_name] = p

    def get(_, prop_name):
        return _.props.get(prop_name)

    def put(_, prop_name, value, attrs=set()):
        read_only = "read_only" in attrs
        dont_enum = "dont_enum" in attrs
        dont_delete = "dont_delete" in attrs
        p = t_js_property(value, read_only, dont_enum, dont_delete, False)
        _.props[prop_name] = p

    def set_value(_, prop_name, value):
        new_prop = _.props[prop_name]._replace(value=value)
        _.props[prop_name] = new_prop

    def delete(_, prop_name):
        del _.props[prop_name]


class t_scope_chain:
    def __init__(_, stack=None):
        if stack is None:
            _.stack = []
        else:
            _.stack = stack

    def push(_, obj):
        _.stack.append(obj)

    def pop(_):
        _.stack.pop()

    def copy(_):
        res = t_scope_chain()
        res.stack = _.stack.copy()
        return res


class t_exec_context:
    def __init__(_, scope_chain, variable_object, this, dont_delete=False):
        _.scope_chain = scope_chain
        _.variable_object = variable_object
        _.this = this
        _.dont_delete = dont_delete

    def put(_, identifier, value):
        if _.dont_delete:
            _.variable_object.put(identifier, value, {"dont_delete"})
        else:
            _.variable_object.put(identifier, value)

    def instantiate_func_decl(_, identifier, func):
        _.put(identifier, func)

    def instantiate_parameter(_, identifier, value):
        _.put(identifier, value)

    def instantiate_var_decl(_, identifier):
        if not _.variable_object.get(identifier):
            _.put(identifier, js_undefined)

    def copy(_):
        return t_exec_context(_.scope_chain.copy(), _.variable_object, _.this)


def parse_code(code):
    return parse(lex(code))


def arg_get(args, idx):
    if idx >= len(args):
        return js_undefined
    return args[idx]


def get_prototype(o):
    return o.get_internal("prototype")


def get_class(o):
    return o.get_internal("class")


def js_get(o, prop_name):
    x = o.get(prop_name)
    if x is not None:
        return x.value
    proto = get_prototype(o)
    if proto is js_null:
        return js_undefined
    return js_get(proto, prop_name)


def is_array_index(_, prop_name):
    i = to_uint_32(_, prop_name)
    return to_string(_, i) == prop_name and i != 2**32 - 1


def array_update_length(_, arr, new_idx):
    if not is_array_index(_, new_idx):
        return
    length = arr.get("length").value
    new_idx = to_uint_32(_, new_idx)
    if new_idx >= length:
        arr.set_value("length", new_idx + 1)


def js_put(_, o, prop_name, value):
    if not js_can_put(o, prop_name):
        return
    if o.get(prop_name) is not None:
        if is_array(o):
            if prop_name == "length":
                new_length = to_uint_32(_, value)
                old_length = o.get("length").value
                for k in range(int(new_length), int(old_length)):
                    idx = to_string(_, t_js_number(k))
                    if o.get(idx) is not None:
                        o.delete(idx)
                value = new_length
        o.set_value(prop_name, value)
        if is_array(o):
            array_update_length(_, o, prop_name)
        return
    o.put(prop_name, value)
    if is_array(o):
        array_update_length(_, o, prop_name)


def js_can_put(o, prop_name):
    x = o.get(prop_name)
    if x is not None:
        return not x.read_only
    proto = get_prototype(o)
    if proto is js_null:
        return True
    return js_can_put(proto, prop_name)


def js_has_property(o, prop_name):
    x = o.get(prop_name)
    if x is not None:
        return True
    proto = get_prototype(o)
    if proto is js_null:
        return False
    return js_has_property(proto, prop_name)


def js_delete(o, prop_name):
    x = o.get(prop_name)
    if x is None:
        return True
    if x.dont_delete:
        return False
    o.delete(prop_name)
    return True


def js_default_value(_, o, hint=None):
    if hint is None:
        if get_class(o) == "Date":
            hint = t_js_string
        else:
            hint = t_js_number

    if hint is t_js_string:
        x = js_get(o, "toString")
        if type(x) is t_js_object:
            y = js_call(_, x, o, [])
            if is_primitive_value(y):
                return y
        z = js_get(o, "valueOf")
        if type(z) is not t_js_object:
            raise t_runtime_error()
        w = js_call(_, z, o, [])
        if not is_primitive_value(w):
            raise t_runtime_error()
        return w

    if hint is t_js_number:
        x = js_get(o, "valueOf")
        if type(x) is t_js_object:
            y = js_call(_, x, o, [])
            if is_primitive_value(y):
                return y
        z = js_get(o, "toString")
        if type(z) is not t_js_object:
            raise t_runtime_error()
        w = js_call(_, z, o, [])
        if not is_primitive_value(w):
            raise t_runtime_error()
        return w


def js_construct(_, obj, args):
    return obj.get_internal("construct")(_, args, obj)


def js_call(_, func, this, args):
    body = func.get_internal("call")
    if type(body) is not t_ast:
        return body(_, this, args)
    params = func.get_internal("function_params")
    _.enter_func_code(this, args, func)
    instantiate_params(_, params, args)
    instantiate_vars(_, body)
    res = normal_completion
    for i in range(len(body.kids)):
        if res.kind != "normal":
            break
        c = js_eval_stmt(_, body.kids[i])
        v = c.value
        if v is None:
            v = res.value
        res = t_js_completion(c.kind, v)
    if res.kind == "return":
        res = res.value
    else:
        res = js_undefined
    _.leave()
    return res


def get_base(v):
    if type(v) is t_js_reference:
        return v.base
    raise t_runtime_error()


def get_property_name(v):
    if type(v) is t_js_reference:
        return v.property_name
    raise t_runtime_error()


def get_value(v):
    if type(v) is not t_js_reference:
        return v
    b = get_base(v)
    if b is js_null:
        raise t_runtime_error()
    return js_get(b, get_property_name(v))


def put_value(_, v, w):
    if type(v) is not t_js_reference:
        raise t_runtime_error()
    b = get_base(v)
    if b is js_null:
        js_put(_, _.global_object, get_property_name(v), w)
    else:
        js_put(_, b, get_property_name(v), w)


def to_primitive(_, value, preferred_type=None):
    l = (t_js_undefined, t_js_null, t_js_boolean, t_js_number, t_js_string)
    if type(value) in l:
        return value
    if type(value) is not t_js_object:
        raise t_runtime_error()
    x = js_default_value(_, value, preferred_type)
    if type(x) in (t_js_object, t_js_reference):
        raise t_runtime_error()
    return x


def to_boolean(value):
    if type(value) in (t_js_undefined, t_js_null):
        return js_false
    if type(value) is t_js_boolean:
        return value
    if type(value) is t_js_number:
        return t_js_boolean(not is_false_number(value))
    if type(value) is t_js_string:
        return t_js_boolean(value != "")
    if type(value) is t_js_object:
        return js_true
    raise t_runtime_error()


def js_string_to_number(value):
    if value == "":
        return 0.0
    try:
        if value.startswith("0x") or value.startswith("0X"):
            return float(int(value, 16))
        return float(value)
    except ValueError:
        return float("nan")


def to_number(_, value):
    if type(value) is t_js_undefined:
        return float("nan")
    if type(value) is t_js_null:
        return 0.0
    if type(value) is t_js_boolean:
        if value:
            return 1.0
        else:
            return 0.0
    if type(value) is t_js_number:
        return value
    if type(value) is t_js_string:
        return js_string_to_number(value.strip("\t\f\r\v \n"))
    if type(value) is t_js_object:
        return to_number(_, to_primitive(_, value, t_js_number))
    raise t_runtime_error()


def sign(x):
    if x > 0.0:
        return 1.0
    else:
        return -1.0


def to_integer(_, value):
    x = to_number(_, value)
    if is_nan(x):
        return 0.0
    if x == 0.0 or x == math.inf or x == -math.inf:
        return x
    return sign(x) * math.floor(abs(x))


def to_int_32(_, value):
    x = to_number(_, value)
    if is_nan(x) or x in (0.0, math.inf, -math.inf):
        return 0.0
    x = (sign(x) * math.floor(abs(x))) % 2**32
    if x >= 2**31:
        x = x - 2**32
    return x


def to_uint_32(_, value):
    x = to_number(_, value)
    if is_nan(x) or x in (0.0, math.inf, -math.inf):
        return 0.0
    return (sign(x) * math.floor(abs(x))) % 2**32


def to_uint_16(_, value):
    x = to_number(_, value)
    if is_nan(x) or x in (0.0, math.inf, -math.inf):
        return 0.0
    return (sign(x) * math.floor(abs(x))) % 2**16


def js_number_to_string(value):
    if is_nan(value):
        return "NaN"
    if value == 0.0:
        return "0"
    if value == math.inf:
        return "Infinity"
    if value == -math.inf:
        return "-Infinity"
    if value.is_integer():
        return t_js_string(str(int(value)))
    return t_js_string(str(value))


def to_string(_, value):
    if type(value) is t_js_undefined:
        return t_js_string("undefined")
    if type(value) is t_js_null:
        return t_js_string("null")
    if type(value) is t_js_boolean:
        if value:
            return "true"
        else:
            return "false"
    if type(value) is t_js_number:
        return js_number_to_string(value)
    if type(value) is t_js_string:
        return value
    if type(value) is t_js_object:
        return to_string(_, to_primitive(_, value, t_js_string))
    raise t_runtime_error()


def to_object(_, value):
    if type(value) is t_js_undefined or type(value) is t_js_null:
        raise t_runtime_error()
    if type(value) is t_js_string:
        return construct_string(_, [value])
    if type(value) is t_js_boolean:
        return construct_boolean(_, [value])
    if type(value) is t_js_number:
        return construct_number(_, [value])
    if type(value) is t_js_object:
        return value


def exp_val(_, x):
    return get_value(js_eval_exp(_, x))


def js_abstract_less_than(_, x, y):
    x = to_primitive(_, x, t_js_number)
    y = to_primitive(_, y, t_js_number)
    if type(x) is t_js_string and type(y) is t_js_string:
        return x < y
    x = to_number(_, x)
    y = to_number(_, y)
    if is_nan(x) or is_nan(y):
        return js_undefined
    if x == math.inf:
        return js_false
    if y == math.inf:
        return js_true
    if y == -math.inf:
        return js_false
    if x == -math.inf:
        return js_true
    if x == y:
        return js_false
    if x < y:
        return js_true
    return js_false


def js_abstract_equals(_, x, y):
    if type(x) is type(y):
        if type(x) is t_js_object:
            return x is y
        return x == y
    if x == js_null and y == js_undefined:
        return js_true
    if x == js_undefined and y == js_null:
        return js_true
    if type(x) is t_js_number and type(y) is t_js_string:
        return x == to_number(_, y)
    if type(x) is t_js_string and type(y) is t_js_number:
        return to_number(_, x) == y
    if type(x) is t_js_boolean:
        return js_abstract_equals(_, to_number(_, x), y)
    if type(y) is t_js_boolean:
        return js_abstract_equals(_, x, to_number(_, y))
    if type(x) in [t_js_string, t_js_number] and type(y) is t_js_object:
        return js_abstract_equals(_, x, to_primitive(_, y))
    if type(y) in [t_js_string, t_js_number] and type(x) is t_js_object:
        return js_abstract_equals(_, y, to_primitive(_, x))
    return js_false


def eval_identifier(_, identifier):
    new_base = js_null
    for obj in reversed(_.exec_context().scope_chain.stack):
        if js_has_property(obj, identifier):
            new_base = obj
            break
    return t_js_reference(base=new_base, property_name=identifier)


def eval_leaf(_, kind, value):
    if kind == "this":
        return _.exec_context().this
    elif kind == "identifier":
        return eval_identifier(_, value)
    elif kind == "null":
        return js_null
    elif kind == "true":
        return js_true
    elif kind == "false":
        return js_false
    elif kind == "octal_literal":
        return t_js_number(int(value, base=8))
    elif kind == "hex_literal":
        return t_js_number(int(value, base=16))
    elif kind == "decimal_literal":
        return t_js_number(value)
    elif kind == "string_literal":
        res = t_js_string("")
        i = 1
        while i != len(value) - 1:
            if value[i] == "\\":
                i += 1
                ch = value[i]
                if ch in ["'", "\"", "\\"]:
                    res += ch
                elif ch == "b":
                    res += "\b"
                elif ch == "f":
                    res += "\f"
                elif ch == "n":
                    res += "\n"
                elif ch == "r":
                    res += "\r"
                elif ch == "t":
                    res += "\t"
                elif is_octal_digit(ch):
                    oct_value = ""
                    for j in range(3 if ch in "0123" else 2):
                        oct_value += value[i]
                        i += 1
                        if not is_octal_digit(value[i]):
                            break
                    i -= 1
                    res += chr(int(oct_value, base=8))
                elif ch == "x":
                    hex_value = ""
                    for j in range(2):
                        i += 1
                        hex_value += value[i]
                    res += chr(int(hex_value, base=16))
                elif ch == "u":
                    hex_value = ""
                    for j in range(4):
                        i += 1
                        hex_value += value[i]
                    res += chr(int(hex_value, base=16))
                else:
                    assert False
            else:
                res += value[i]
            i += 1
        return res
    else:
        assert False


def eval_arguments(_, args):
    res = []
    for arg in args:
        res.append(exp_val(_, arg))
    return res


def is_function(x):
    return get_class(x) == "Function"


def is_array(x):
    return get_class(x) == "Array"


def js_eval(_, this, args):
    x = arg_get(args, 0)
    if type(x) is not str:
        return x
    ast = parse_code(x)
    _.enter_eval_code()
    instantiate_funcs(_, ast)
    instantiate_vars(_, ast)
    res = eval_program(_, ast)
    _.leave()
    if res.kind == "normal" and res.value is not None:
        return res.value
    return js_undefined


def is_radix_digit(char, radix):
    if char == "" or ord(char) >= 128:
        return False
    radix = int(radix)
    assert radix in range(2, 37)
    x = ord(char.lower())
    zero = ord("0")
    if radix <= 10:
        return x in range(zero, zero + radix)
    if x in range(zero, zero + 10):
        return True
    return x in range(ord("a"), ord("a") + radix - 10)


def parse_int(_, this, args):
    string = arg_get(args, 0)
    radix = arg_get(args, 1)
    string = to_string(_, string)
    string = string.lstrip(whitespace_chars)
    sign = 1.0
    if string != "" and string[0] == "-":
        sign = -1.0
    if string != "" and string[0] in ["+", "-"]:
        string = string[1:]
    radix = to_int_32(_, radix)
    if radix == 0:
        radix = 10.0
        if string[:2] in ["0x", "0X"]:
            string = string[2:]
            radix = 16.0
    elif radix < 2 or radix > 36:
        return nan
    elif radix == 16:
        if string[:2] in ["0x", "0X"]:
            string = string[2:]
    k = 0
    while k < len(string) and is_radix_digit(string[k], radix):
        k += 1
    if k == 0:
        return nan
    string = string[:k]
    res = t_js_number(int(string, base=int(radix)))
    res = sign * res
    return res


def empty_function(_, this, args):
    return js_undefined


def create_new_object(_):
    o = t_js_object()
    o.put_internal("prototype", _.object_prototype)
    o.put_internal("class", "Object")
    return o


def call_object_constructor(_, this, args):
    value = arg_get(args, 0)
    if value in [js_null, js_undefined]:
        return create_new_object(_)
    return to_object(_, value)


def construct_object(_, args, unused=None):
    value = arg_get(args, 0)
    if type(value) is t_js_object:
        return value
    if type(value) in [t_js_string, t_js_boolean, t_js_number]:
        return to_object(_, value)
    assert value in [js_null, js_undefined]
    return create_new_object(_)


def object_to_string(_, this, args):
    return "[object " + this.get_internal("class") + "]"


def object_value_of(_, this, args):
    return this


def call_function_constructor(_, this, args):
    return construct_function(_, args)


def construct_function(_, args, unused=None):
    params_string = ""
    body = ""
    if len(args) >= 1:
        body = args[-1]
        for i in range(len(args) - 1):
            if i != 0:
                params_string += ","
            params_string += to_string(_, args[i])
    body = to_string(_, body)
    params_ast = parse(lex(params_string), "formal_params")
    body_ast = parse(lex(body), "func_body")
    params = []
    for kid in params_ast.kids:
        assert kid.kind == "identifier"
        params.append(kid.value)
    return create_function(_, params, body_ast)


length_attrs = {"dont_enum", "dont_delete", "read_only"}
non_length_attrs = {"dont_enum"}

def global_function_init(_):
    function_ctor = t_js_object()
    function_ctor.put_internal("prototype", _.function_prototype)
    function_ctor.put_internal("class", "Function")
    function_ctor.put_internal("call", call_function_constructor)
    function_ctor.put_internal("construct", construct_function)
    function_ctor.put("length", t_js_number(1), length_attrs)
    function_ctor.put("prototype", _.function_prototype, non_length_attrs)

    _.function_prototype.put_internal("prototype", _.object_prototype)
    _.function_prototype.put_internal("class", "Function")
    _.function_prototype.put_internal("call", empty_function)
    _.function_prototype.put("constructor", function_ctor, non_length_attrs)
    _.function_prototype.put("length", t_js_number(0), length_attrs)
    # implement toString
    _.global_object.put("Function", function_ctor)


def global_object_init(_):
    object_ctor = t_js_object()
    object_ctor.put_internal("prototype", _.function_prototype)
    object_ctor.put_internal("class", "Function")
    object_ctor.put_internal("call", call_object_constructor)
    object_ctor.put_internal("construct", construct_object)
    object_ctor.put("length", t_js_number(1), length_attrs)

    _.object_prototype.put_internal("prototype", js_null)
    _.object_prototype.put_internal("class", "Object")
    _.object_prototype.put("constructor", object_ctor, non_length_attrs)
    put_native_method(_, _.object_prototype, "toString", object_to_string)
    put_native_method(_, _.object_prototype, "valueOf", object_value_of)
    object_ctor.put("prototype", _.object_prototype, non_length_attrs)

    _.global_object.put("Object", object_ctor)


def construct_array(_, args, unused=None):
    res = t_js_object()
    res.put_internal("prototype", _.array_prototype)
    res.put_internal("class", "Array")
    if len(args) == 1 and type(args[0]) is t_js_number:
        res.put("length", to_uint_32(_, args[0]), {"dont_enum", "dont_delete"})
    else:
        res.put("length", t_js_number(len(args)), {"dont_enum", "dont_delete"})
        for i in range(len(args)):
            res.put(to_string(_, t_js_number(i)), args[i])
    return res


def call_array_constructor(_, this, args):
    return construct_array(_, args)


def array_prototype_join(_, this, args):
    length = int(to_uint_32(_, js_get(this, "length")))
    separator = arg_get(args, 0)
    if separator == js_undefined:
        separator = ","
    separator = to_string(_, separator)
    if length == 0:
        return ""
    res = ""
    for k in range(length):
        if k != 0:
            res += separator
        x = js_get(this, to_string(_, t_js_number(k)))
        res += "" if x in [js_undefined, js_null] else to_string(_, x)
    return res


def array_swap(_, this, i, j):
    iv = js_get(this, i)
    jv = js_get(this, j)
    if this.get(j) is not None:
        if this.get(i) is not None:
            js_put(_, this, i, jv)
            js_put(_, this, j, iv)
        else:
            js_put(_, this, i, jv)
            js_delete(this, j)
    else:
        if this.get(i) is not None:
            js_put(_, this, j, iv)
            js_delete(this, i)
        else:
            js_delete(this, i)
            js_delete(this, j)


def array_prototype_reverse(_, this, args):
    length = to_uint_32(_, js_get(this, "length"))
    for k in range(int(length // 2)):
        i = to_string(_, t_js_number(k))
        j = to_string(_, length - 1 - k)
        array_swap(_, this, i, j)
    return this


def array_prototype_to_string(_, this, args):
    return array_prototype_join(_, this, [])


def construct_string(_, args, unused=None):
    res = t_js_object()
    res.put_internal("prototype", _.string_prototype)
    res.put_internal("class", "String")
    value = to_string(_, args[0]) if args != [] else ""
    res.put_internal("value", value)
    res.put("length", t_js_number(len(value)), length_attrs)
    return res


def call_string_constructor(_, this, args):
    if args == []:
        return ""
    return to_string(_, args[0])


def string_from_char_code(_, this, args):
    res = ""
    for char in args:
        res += chr(int(to_uint_16(_, char)))
    return res


def string_prototype_to_string(_, this, args):
    return this.get_internal("value")


def string_prototype_value_of(_, this, args):
    return this.get_internal("value")


def string_prototype_char_at(_, this, args):
    string = to_string(_, this)
    pos = to_integer(_, arg_get(args, 0))
    if pos < 0 or pos >= len(string):
        return ""
    return string[int(pos)]


def string_prototype_char_code_at(_, this, args):
    string = to_string(_, this)
    pos = to_integer(_, arg_get(args, 0))
    if pos < 0 or pos >= len(string):
        return t_js_number("nan")
    return t_js_number(ord(string[int(pos)]))


def clamp(x, a, b):
    x = t_js_number(x)
    a = t_js_number(a)
    b = t_js_number(b)
    if x <= a:
        return a
    if x > b:
        return b
    return x


def string_prototype_index_of(_, this, args):
    string = to_string(_, this)
    search_string = to_string(_, arg_get(args, 0))
    pos = to_integer(_, arg_get(args, 1))
    pos = clamp(pos, 0, len(string))
    return t_js_number(string.find(search_string, int(pos)))


def string_prototype_last_index_of(_, this, args):
    string = to_string(_, this)
    search_string = to_string(_, arg_get(args, 0))
    pos = to_number(_, arg_get(args, 1))
    if is_nan(pos):
        pos = math.inf
    else:
        pos = to_integer(_, pos)
    pos = clamp(pos, 0, len(string))
    end = pos + len(search_string)
    return t_js_number(string.rfind(search_string, 0, int(end)))


def append(_, array, new_elt):
    js_put(_, array, to_string(_, js_get(array, "length")), new_elt)


def string_prototype_split(_, this, args):
    string = to_string(_, this)
    separator = arg_get(args, 0)
    res = construct_array(_, [])

    if separator == js_undefined:
        js_put(_, res, "0", string)
        return res
    separator = to_string(_, separator)
    if separator == "":
        for char in string:
            append(_, res, char)
    else:
        for word in string.split(sep=separator):
            append(_, res, word)
    return res


def string_prototype_substring(_, this, args):
    string = to_string(_, this)
    start = to_integer(_, arg_get(args, 0))
    end = arg_get(args, 1)
    end = to_integer(_, end) if end != js_undefined else len(string)
    start = clamp(start, 0, len(string))
    end = clamp(end, 0, len(string))
    if start > end:
        start, end = end, start
    return string[int(start):int(end)]


def string_prototype_to_lower_case(_, this, args):
    return to_string(_, this).lower()


def string_prototype_to_upper_case(_, this, args):
    return to_string(_, this).upper()


def global_string_init(_):
    string_ctor = t_js_object()
    string_ctor.put_internal("prototype", _.function_prototype)
    string_ctor.put_internal("class", "Function")
    string_ctor.put_internal("call", call_string_constructor)
    string_ctor.put_internal("construct", construct_string)
    string_ctor.put("length", t_js_number(1), length_attrs)
    put_native_method(_, string_ctor, "fromCharCode", string_from_char_code, 1)
    string_ctor.put("length", t_js_number(1), length_attrs)

    sp = t_js_object()
    sp.put_internal("prototype", _.object_prototype)
    sp.put_internal("class", "String")
    sp.put("length", t_js_number(0), length_attrs)
    sp.put("constructor", string_ctor, non_length_attrs)
    put_native_method(_, sp, "toString", string_prototype_to_string)
    put_native_method(_, sp, "valueOf", string_prototype_value_of)
    put_native_method(_, sp, "charAt", string_prototype_char_at, 1)
    put_native_method(_, sp, "charCodeAt", string_prototype_char_code_at, 1)
    put_native_method(_, sp, "indexOf", string_prototype_index_of, 1)
    put_native_method(_, sp, "lastIndexOf", string_prototype_last_index_of, 1)
    put_native_method(_, sp, "split", string_prototype_split, 2)
    put_native_method(_, sp, "substring", string_prototype_substring, 2)
    put_native_method(_, sp, "toLowerCase", string_prototype_to_lower_case)
    put_native_method(_, sp, "toUpperCase", string_prototype_to_upper_case)
    _.string_prototype = sp

    attrs = {"dont_enum", "dont_delete", "read_only"}
    string_ctor.put("prototype", _.string_prototype, attrs)

    _.global_object.put("String", string_ctor)


def global_array_init(_):
    array_ctor = t_js_object()
    array_ctor.put_internal("prototype", _.function_prototype)
    array_ctor.put_internal("class", "Function")
    array_ctor.put_internal("call", call_array_constructor)
    array_ctor.put_internal("construct", construct_array)
    array_ctor.put("length", t_js_number(1), length_attrs)

    _.array_prototype = t_js_object()
    _.array_prototype.put_internal("prototype", _.object_prototype)
    _.array_prototype.put_internal("class", "Array")
    _.array_prototype.put("length", t_js_number(0), length_attrs)
    _.array_prototype.put("constructor", array_ctor, non_length_attrs)
    put_native_method(_, _.array_prototype, "join", array_prototype_join, 1)
    put_native_method(_, _.array_prototype, "toString", array_prototype_join)
    put_native_method(_, _.array_prototype, "reverse", array_prototype_reverse)
    _.array_prototype.put("constructor", array_ctor, non_length_attrs)

    array_ctor.put("prototype", _.array_prototype, non_length_attrs)

    _.global_object.put("Array", array_ctor)


def call_boolean_constructor(_, this, args):
    return to_boolean(arg_get(args, 0))


def construct_boolean(_, args, unused=None):
    res = t_js_object()
    res.put_internal("prototype", _.boolean_prototype)
    res.put_internal("class", "Boolean")
    res.put_internal("value", to_boolean(arg_get(args, 0)))
    return res


def boolean_prototype_to_string(_, this, args):
    if this.get_internal("value") == js_true:
        return "true"
    else:
        return "false"


def boolean_prototype_value_of(_, this, args):
    return this.get_internal("value")


def global_boolean_init(_):
    ctor = t_js_object()
    ctor.put_internal("prototype", _.function_prototype)
    ctor.put_internal("class", "Function")
    ctor.put_internal("call", call_boolean_constructor)
    ctor.put_internal("construct", construct_boolean)
    ctor.put("length", t_js_number(1), length_attrs)

    bp = t_js_object()
    bp.put_internal("prototype", _.object_prototype)
    bp.put_internal("class", "Boolean")
    bp.put_internal("value", js_false)
    bp.put("constructor", ctor, non_length_attrs)
    put_native_method(_, bp, "toString", boolean_prototype_to_string)
    put_native_method(_, bp, "valueOf", boolean_prototype_value_of)
    _.boolean_prototype = bp

    attrs = {"dont_enum", "dont_delete", "read_only"}
    ctor.put("prototype", _.boolean_prototype, attrs)

    _.global_object.put("Boolean", ctor, non_length_attrs)


def call_number_constructor(_, this, args):
    if args == []:
        return t_js_number(0)
    return to_number(_, args[0])


def construct_number(_, args, unused=None):
    res = t_js_object()
    res.put_internal("prototype", _.number_prototype)
    res.put_internal("class", "Number")
    value = to_number(_, args[0]) if args != [] else t_js_number(0)
    res.put_internal("value", value)
    return res


def reverse_string(string):
    return string[::-1]


def number_prototype_to_string(_, this, args):
    radix = arg_get(args, 0)
    if radix == t_js_number(10) or radix == js_undefined:
        return to_string(_, this.get_internal("value"))
    if int(radix) in range(2, 37):
        digits = "0123456789abcdefghijklmnopqrstuvwxyz"
        value = this.get_internal("value")
        res = ""
        if value < 0:
            value = -value
            res += "-"
        int_part = int(value)
        frac_part = value - int_part

        int_part_str = ""
        while int_part != 0:
            digit = int(int_part % radix)
            int_part_str += digits[digit]
            int_part //= radix
        res += reverse_string(int_part_str)
        if frac_part != 0:
            res += "."
            while frac_part != 0:
                frac_part *= radix
                digit = int(frac_part)
                res += digits[digit]
                frac_part -= digit
        return res


def number_prototype_value_of(_, this, args):
    return this.get_internal("value")


def global_number_init(_):
    ctor = t_js_object()
    ctor.put_internal("prototype", _.function_prototype)
    ctor.put_internal("class", "Function")
    ctor.put_internal("call", call_number_constructor)
    ctor.put_internal("construct", construct_number)
    ctor.put("length", t_js_number(1), length_attrs)

    proto = t_js_object()
    proto.put_internal("prototype", _.object_prototype)
    proto.put_internal("class", "Number")
    proto.put_internal("value", t_js_number(0))
    proto.put("constructor", ctor, non_length_attrs)
    put_native_method(_, proto, "toString", number_prototype_to_string, 1)
    put_native_method(_, proto, "valueOf", number_prototype_value_of)
    _.number_prototype = proto

    attrs = {"dont_enum", "dont_delete", "read_only"}
    ctor.put("prototype", _.number_prototype, attrs)
    ctor.put("MAX_VALUE", sys.float_info.max, attrs)
    ctor.put("MIN_VALUE", sys.float_info.min * sys.float_info.epsilon, attrs)
    ctor.put("NaN", float("nan"), attrs)
    ctor.put("NEGATIVE_INFINITY", -math.inf, attrs)
    ctor.put("POSITIVE_INFINITY", math.inf, attrs)

    _.global_object.put("Number", ctor, non_length_attrs)


def math_abs(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return abs(x)


def math_acos(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.acos(x)


def math_asin(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.asin(x)


def math_atan(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.atan(x)


def math_atan_2(_, this, args):
    y = to_number(_, arg_get(args, 0))
    x = to_number(_, arg_get(args, 1))
    return math.atan2(y, x)


def math_ceil(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.ceil(x)


def math_cos(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.cos(x)


def math_exp(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.exp(x)


def math_floor(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return t_js_number(math.floor(x))


def math_log(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.log(x)


def math_max(_, this, args):
    x = to_number(_, arg_get(args, 0))
    y = to_number(_, arg_get(args, 1))
    return max(x, y)


def math_min(_, this, args):
    x = to_number(_, arg_get(args, 0))
    y = to_number(_, arg_get(args, 1))
    return min(x, y)


def math_pow(_, this, args):
    x = to_number(_, arg_get(args, 0))
    y = to_number(_, arg_get(args, 1))
    return pow(x, y)


def math_random(_, this, args):
    return random.random()


def math_round(_, this, args):
    x = to_number(_, arg_get(args, 0))
    if x == -0.0 or (x >= -0.5 and x < 0):
        return -0.0
    return t_js_number(math.floor(x + 0.5))


def math_sin(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.sin(x)


def math_sqrt(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.sqrt(x)


def math_tan(_, this, args):
    x = to_number(_, arg_get(args, 0))
    return math.tan(x)


def global_math_init(_):
    obj = t_js_object()
    obj.put_internal("prototype", _.object_prototype)
    obj.put_internal("class", "Math")
    attrs = {"dont_enum", "dont_delete", "read_only"}
    obj.put("E", math.e, attrs)
    obj.put("LN10", math.log(10), attrs)
    obj.put("LN2", math.log(2), attrs)
    obj.put("LOG2E", math.log(math.e, 2), attrs)
    obj.put("LOG10E", math.log(math.e, 10), attrs)
    obj.put("PI", math.pi, attrs)
    obj.put("SQRT1_2", math.sqrt(1/2), attrs)
    obj.put("SQRT2", math.sqrt(2), attrs)
    put_native_method(_, obj, "abs", math_abs, 1)
    put_native_method(_, obj, "acos", math_acos, 1)
    put_native_method(_, obj, "asin", math_asin, 1)
    put_native_method(_, obj, "atan", math_atan, 1)
    put_native_method(_, obj, "atan2", math_atan_2, 2)
    put_native_method(_, obj, "ceil", math_ceil, 1)
    put_native_method(_, obj, "cos", math_cos, 1)
    put_native_method(_, obj, "exp", math_exp, 1)
    put_native_method(_, obj, "floor", math_floor, 1)
    put_native_method(_, obj, "log", math_log, 1)
    put_native_method(_, obj, "max", math_max, 2)
    put_native_method(_, obj, "min", math_min, 2)
    put_native_method(_, obj, "pow", math_pow, 2)
    put_native_method(_, obj, "random", math_random)
    put_native_method(_, obj, "round", math_round, 1)
    put_native_method(_, obj, "sin", math_sin, 1)
    put_native_method(_, obj, "sqrt", math_sqrt, 1)
    put_native_method(_, obj, "tan", math_tan, 1)

    _.global_object.put("Math", obj, attrs)


ms_per_day = 86400000


def day_from_time(t):
    return t // ms_per_day


def time_within_day(t):
    return t % ms_per_day


def days_in_year(year):
    if year % 4 != 0:
        return 365
    if year % 25 != 0:
        return 366
    if year % 16 != 0:
        return 365
    return 366


def day_from_year(y):
    leap_days = (y - 1969) // 4 - (y - 1901) // 100 + (y - 1601) // 400
    return 365 * (y - 1970) + leap_days


def time_from_year(year):
    return ms_per_day * day_from_year(year)


def year_from_time(t):
    p = t // ms_per_day + 719162
    a = p // 146097
    w = p - 146097 * a
    c = min(3, w // 36524)
    u = w - c * 36524
    e = min(24, u // 1461)
    v = u - 1461 * e
    f = min(3, v // 365)
    return 400 * a + 100 * c + 4 * e + f + 1


def in_leap_year(t):
    return days_in_year(year_from_time(t)) == 366


def day_within_year(t):
    return day_from_time(t) - day_from_year(year_from_time(t))


def month_from_time(t):
    leap = in_leap_year(t)
    day = day_within_year(t)
    x = None
    if day in range(0, 31):
        x = 0
    if day in range(31, 59 + leap):
        x = 1
    if day in range(59 + leap, 90 + leap):
        x = 2
    if day in range(90 + leap, 120 + leap):
        x = 3
    if day in range(120 + leap, 151 + leap):
        x = 4
    if day in range(151 + leap, 181 + leap):
        x = 5
    if day in range(181 + leap, 212 + leap):
        x = 6
    if day in range(212 + leap, 243 + leap):
        x = 7
    if day in range(243 + leap, 273 + leap):
        x = 8
    if day in range(273 + leap, 304 + leap):
        x = 9
    if day in range(304 + leap, 334 + leap):
        x = 10
    if day in range(334 + leap, 365 + leap):
        x = 11
    return t_js_number(x)


def day_from_month(month, year):
    leap = (days_in_year(year) == 366)
    table = [
        0,
        31,
        59 + leap,
        90 + leap,
        120 + leap,
        151 + leap,
        181 + leap,
        212 + leap,
        243 + leap,
        273 + leap,
        304 + leap,
        334 + leap
    ]
    return table[int(month)]


def date_from_time(t):
    leap = in_leap_year(t)
    day = day_within_year(t)
    table = [
        day + 1,
        day - 30,
        day - 58 - leap,
        day - 89 - leap,
        day - 119 - leap,
        day - 150 - leap,
        day - 180 - leap,
        day - 211 - leap,
        day - 242 - leap,
        day - 272 - leap,
        day - 303 - leap,
        day - 333 - leap
    ]
    return t_js_number(table[int(month_from_time(t))])


def week_day(t):
    return (day(t) + 4) % 7


def local_time_zone_adjustment():
    return -1 * time.timezone * 1000


def get_local_timezone():
    x = datetime.datetime.now(datetime.timezone(datetime.timedelta(0)))
    return x.astimezone().tzinfo


def summer_time_adjustment(t):
    return 0.0


def local_time(t):
    return t + local_time_zone_adjustment() + summer_time_adjustment(t)


def utc(t):
    tza = local_time_zone_adjustment()
    return t - tza - summer_time_adjustment(t - tza)


hours_per_day = 24
minutes_per_hour = 60
seconds_per_minute = 60
ms_per_second = 1000
ms_per_minute = ms_per_second * seconds_per_minute
ms_per_hour = ms_per_minute * minutes_per_hour


def hour_from_time(t):
    return (t // ms_per_hour) % hours_per_day


def min_from_time(t):
    return (t // ms_per_minute) % minutes_per_hour


def sec_from_time(t):
    return (t // ms_per_second) % seconds_per_minute


def ms_from_time(t):
    return t % ms_per_second


def is_finite(x):
    return not is_nan(x) and x != math.inf and x != -math.inf


def make_time(_, hour, minute, second, millisecond):
    if not all(is_finite(x) for x in (hour, minute, second, millisecond)):
        return nan
    hour = to_integer(_, hour)
    minute = to_integer(_, minute)
    second = to_integer(_, second)
    millisecond = to_integer(_, millisecond)
    res = hour * ms_per_hour + minute * ms_per_minute + second * ms_per_second
    res += millisecond
    return res


def make_day(_, year, month, date):
    if not all(is_finite(x) for x in (year, month, date)):
        return nan
    year = to_integer(_, year)
    month = to_integer(_, month)
    date = to_integer(_, date)
    year += month // 12
    month %= 12
    return day_from_year(year) + day_from_month(month, year) + date - 1


def make_date(day, t):
    if not all(is_finite(x) for x in (day, t)):
        return nan
    return day * ms_per_day + t


def time_clip(_, t):
    if not is_finite(t):
        return nan
    if abs(t) > 8.64e15:
        return nan
    return to_integer(_, t) + (+0.0)


def get_current_utc_time():
    return float(math.floor(time.time() * 1000))


def build_date(_, args):
    if args == []:
        return nan
    year = to_number(_, args[0])
    month = to_number(_, args[1]) if len(args) >= 2 else 0.0
    date = to_number(_, args[2]) if len(args) >= 3 else 1.0
    hours = to_number(_, args[3]) if len(args) >= 4 else 0.0
    minutes = to_number(_, args[4]) if len(args) >= 5 else 0.0
    seconds = to_number(_, args[5]) if len(args) >= 6 else 0.0
    ms = to_number(_, args[6]) if len(args) >= 7 else 0.0
    if not is_nan(year):
        i = to_integer(_, year)
        if int(i) in range(0, 100):
            year = 1900 + i
    day = make_day(_, year, month, date)
    return make_date(day, make_time(_, hours, minutes, seconds, ms))


def date_parse(_, this, args):
    string = to_string(_, arg_get(args, 0))
    x = datetime.datetime.strptime(string, date_format).timestamp()
    return float(math.floor(x * 1000))


def construct_date(_, args, unused=None):
    res = t_js_object()
    res.put_internal("prototype", _.date_prototype)
    res.put_internal("class", "Date")
    value = None
    if len(args) >= 2:
        value = time_clip(_, utc(build_date(_, args)))
    elif len(args) == 1:
        value = to_primitive(_, args[0])
        if type(value) is t_js_string:
            value = date_parse(_, None, [value])
        else:
            value = to_number(_, value)
        value = time_clip(_, value)
    elif len(args) == 0:
        value = get_current_utc_time()
    res.put_internal("value", value)
    return res


def call_date_constructor(_, this, args):
    return date_prototype_to_string(_, construct_date(_, []), [])


def date_prototype_value_of(_, this, args):
    return this.get_internal("value")


def date_utc(_, this, args):
    return build_date(_, args)


def date_prototype_get_time(_, this, args):
    return this.get_internal("value")


def date_prototype_get_full_year(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return year_from_time(local_time(t))


def date_prototype_get_utc_full_year(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return year_from_time(t)


def date_prototype_get_month(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return month_from_time(local_time(t))


def date_prototype_get_utc_month(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return month_from_time(t)


def date_prototype_get_date(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return date_from_time(local_time(t))


def date_prototype_get_utc_date(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return date_from_time(t)


def date_prototype_get_day(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return week_day(local_time(t))


def date_prototype_get_utc_day(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return week_day(t)


def date_prototype_get_hours(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return hour_from_time(local_time(t))


def date_prototype_get_utc_hours(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return hour_from_time(t)


def date_prototype_get_minutes(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return min_from_time(local_time(t))


def date_prototype_get_utc_minutes(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return min_from_time(t)


def date_prototype_get_seconds(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return sec_from_time(local_time(t))


def date_prototype_get_utc_seconds(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return sec_from_time(t)


def date_prototype_get_milliseconds(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return ms_from_time(local_time(t))


def date_prototype_get_utc_milliseconds(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return ms_from_time(t)


def date_prototype_get_timezone_offset(_, this, args):
    t = this.get_internal("value")
    if is_nan(t):
        return nan
    return (t - local_time(t)) / ms_per_minute


def date_prototype_set_time(_, this, args):
    t = arg_get(args, 0)
    this.put_internal("value", time_clip(_, to_number(_, t)))
    return this.get_internal("value")


def date_set(_, this, is_utc, **kwargs):
    t = this.get_internal("value")
    if not is_utc:
        t = local_time(t)
    year = kwargs.get("year")
    month = kwargs.get("month")
    date = kwargs.get("date")
    value_day = None
    if (year is None) and (month is None) and (date is None):
        value_day = day_from_time(t)
    else:
        year = to_number(_, year or year_from_time(t))
        month = to_number(_, month or month_from_time(t))
        date = to_number(_, date or date_from_time(t))
        value_day = make_day(_, year, month, date)

    hour = kwargs.get("hour")
    mins = kwargs.get("mins")
    sec = kwargs.get("sec")
    ms = kwargs.get("ms")
    value_time = None
    if (hour is None) and (mins is None) and (sec is None) and (ms is None):
        value_time = time_within_day(t)
    else:
        hour = to_number(_, hour or hour_from_time(t))
        mins = to_number(_, mins or min_from_time(t))
        sec = to_number(_, sec or sec_from_time(t))
        ms = to_number(_, ms or ms_from_time(t))
        value_time = make_time(_, hour, mins, sec, ms)

    value = make_date(value_day, value_time)
    if not is_utc:
        value = utc(value)
    value = time_clip(_, value)
    this.put_internal("value", value)
    return this.get_internal("value")


def date_prototype_set_milliseconds(_, this, args):
    return date_set(_, this, False, ms=arg_get(args, 0))


def date_prototype_set_utc_milliseconds(_, this, args):
    return date_set(_, this, True, ms=arg_get(args, 0))


def arg_get_or_none(args, i):
    return args[i] if i < len(args) else None


def date_prototype_set_seconds(_, this, args):
    ms = arg_get_or_none(args, 1)
    return date_set(_, this, False, sec=arg_get(args, 0), ms=ms)


def date_prototype_set_utc_seconds(_, this, args):
    ms = arg_get_or_none(args, 1)
    return date_set(_, this, True, sec=arg_get(args, 0), ms=ms)


def date_prototype_set_minutes(_, this, args):
    sec = arg_get_or_none(args, 1)
    ms = arg_get_or_none(args, 2)
    return date_set(_, this, False, mins=arg_get(args, 0), sec=sec, ms=ms)


def date_prototype_set_utc_minutes(_, this, args):
    sec = arg_get_or_none(args, 1)
    ms = arg_get_or_none(args, 2)
    return date_set(_, this, True, mins=arg_get(args, 0), sec=sec, ms=ms)


def date_prototype_set_hours(_, this, args):
    hour = arg_get(args, 0)
    sec = arg_get_or_none(args, 1)
    ms = arg_get_or_none(args, 2)
    mins = arg_get_or_none(args, 3)
    return date_set(_, this, False, hour=hour, mins=mins, sec=sec, ms=ms)


def date_prototype_set_utc_hours(_, this, args):
    hour = arg_get(args, 0)
    sec = arg_get_or_none(args, 1)
    ms = arg_get_or_none(args, 2)
    mins = arg_get_or_none(args, 3)
    return date_set(_, this, True, hour=hour, mins=mins, sec=sec, ms=ms)


def date_prototype_set_date(_, this, args):
    return date_set(_, this, False, date=arg_get(args, 0))


def date_prototype_set_utc_date(_, this, args):
    return date_set(_, this, True, date=arg_get(args, 0))


def date_prototype_set_month(_, this, args):
    date = arg_get_or_none(args, 1)
    return date_set(_, this, False, month=arg_get(args, 0), date=date)


def date_prototype_set_utc_month(_, this, args):
    date = arg_get_or_none(args, 1)
    return date_set(_, this, True, month=arg_get(args, 0), date=date)


def date_prototype_set_full_year(_, this, args):
    year = arg_get(args, 0)
    month = arg_get_or_none(args, 1)
    date = arg_get_or_none(args, 2)
    return date_set(_, this, False, year=year, month=month, date=date)


def date_prototype_set_utc_full_year(_, this, args):
    year = arg_get(args, 0)
    month = arg_get_or_none(args, 1)
    date = arg_get_or_none(args, 2)
    return date_set(_, this, True, year=year, month=month, date=date)


date_format = "%Y-%m-%d %H:%M:%S GMT%z"


def date_prototype_to_string(_, this, args):
    value = this.get_internal("value") / 1000.0
    tz = get_local_timezone()
    return datetime.datetime.fromtimestamp(value, tz=tz).strftime(date_format)


def date_prototype_to_locale_string(_, this, args):
    return date_prototype_to_string(_, this, args)


def date_prototype_to_utc_string(_, this, args):
    return date_prototype_to_string(_, this, args)


def global_date_init(_):
    ctor = t_js_object()
    ctor.put_internal("prototype", _.function_prototype)
    ctor.put_internal("class", "Function")
    ctor.put_internal("call", call_date_constructor)
    ctor.put_internal("construct", construct_date)
    ctor.put("length", t_js_number(7), length_attrs)

    put_native_method(_, ctor, "parse", date_parse, 1)
    put_native_method(_, ctor, "UTC", date_utc, 7)

    proto = t_js_object()
    proto.put_internal("prototype", _.object_prototype)
    proto.put_internal("class", "Date")
    proto.put_internal("value", nan)
    proto.put("constructor", ctor, non_length_attrs)

    def method(name, func, length=0):
        put_native_method(_, proto, name, func, length)
    method("toString", date_prototype_to_string)
    method("toUTCString", date_prototype_to_utc_string)
    method("toLocaleString", date_prototype_to_locale_string)
    method("valueOf", date_prototype_value_of)
    method("getTime", date_prototype_get_time)

    method("getFullYear", date_prototype_get_full_year)
    method("getUTCFullYear", date_prototype_get_utc_full_year)

    method("getMonth", date_prototype_get_month)
    method("getUTCMonth", date_prototype_get_utc_month)

    method("getDate", date_prototype_get_date)
    method("getUTCDate", date_prototype_get_utc_date)

    method("getDay", date_prototype_get_day)
    method("getUTCDay", date_prototype_get_utc_day)

    method("getHours", date_prototype_get_hours)
    method("getUTCHours", date_prototype_get_utc_hours)

    method("getMinutes", date_prototype_get_minutes)
    method("getUTCMinutes", date_prototype_get_utc_minutes)

    method("getSeconds", date_prototype_get_seconds)
    method("getUTCSeconds", date_prototype_get_utc_seconds)

    method("getMilliseconds", date_prototype_get_milliseconds)
    method("getUTCMilliseconds", date_prototype_get_utc_milliseconds)

    method("getTimezoneOffset", date_prototype_get_timezone_offset)
    method("setTime", date_prototype_set_time, 1)

    method("setMilliseconds", date_prototype_set_milliseconds, 1)
    method("setUTCMilliseconds", date_prototype_set_utc_milliseconds, 1)

    method("setSeconds", date_prototype_set_seconds, 2)
    method("setUTCSeconds", date_prototype_set_utc_seconds, 2)

    method("setMinutes", date_prototype_set_minutes, 3)
    method("setUTCMinutes", date_prototype_set_utc_minutes, 3)

    method("setHours", date_prototype_set_hours, 4)
    method("setUTCHours", date_prototype_set_utc_hours, 4)

    method("setDate", date_prototype_set_date, 1)
    method("setUTCDate", date_prototype_set_utc_date, 1)

    method("setMonth", date_prototype_set_month, 2)
    method("setUTCMonth", date_prototype_set_utc_month, 2)

    method("setFullYear", date_prototype_set_full_year, 3)
    method("setUTCFullYear", date_prototype_set_utc_full_year, 3)

    _.date_prototype = proto

    attrs = {"dont_enum", "dont_delete", "read_only"}
    ctor.put("prototype", _.date_prototype, attrs)

    _.global_object.put("Date", ctor, non_length_attrs)


class t_js_state:
    def __init__(_):
        _.global_object = t_js_object()
        _.global_object.put_internal("prototype", js_null)
        _.global_object.put_internal("class", "Object")
        attrs = {"dont_enum", "dont_delete"}
        _.global_object.put("NaN", float("nan"), attrs)
        _.global_object.put("Infinity", math.inf, attrs)
        _.global_object.put("undefined", js_undefined, attrs)

        _.object_prototype = t_js_object()
        _.function_prototype = t_js_object()

        global_function_init(_)
        global_object_init(_)
        global_array_init(_)
        global_string_init(_)
        global_boolean_init(_)
        global_number_init(_)
        global_math_init(_)
        global_date_init(_)

        put_native_method(_, _.global_object, "eval", js_eval, 1)
        put_native_method(_, _.global_object, "parseInt", parse_int, 2)

        _.exec_contexts = []

    def exec_context(_):
        return _.exec_contexts[-1]

    def enter_global_code(_):
        ec = t_exec_context(t_scope_chain([_.global_object]),
                            _.global_object, _.global_object)
        _.exec_contexts.append(ec)

    def enter_eval_code(_):
        if _.exec_contexts == []:
            _.enter_global_code()
            return
        _.exec_contexts.append(_.exec_context().copy())

    def enter_func_code(_, this, params, func_obj):
        if type(this) is not t_js_object:
            this = _.global_object
        activation_object = t_js_object()
        activation_object.put_internal("class", "activation_object")
        activation_object.put_internal("prototype", js_null)
        arguments_object = t_js_object()
        arguments_object.put_internal("prototype", _.object_prototype)
        arguments_object.put("callee", func_obj, {"dont_enum"})
        length = t_js_number(len(params))
        arguments_object.put("length", length, {"dont_enum"})
        # todo
        for i in range(len(params)):
            param = to_string(_, t_js_number(i))
            arguments_object.put(param, params[i], {"dont_enum"})
        activation_object.put("arguments", arguments_object, {"dont_delete"})
        ec = t_exec_context(t_scope_chain([_.global_object,
                                           activation_object]),
                            activation_object, this, dont_delete=True)
        _.exec_contexts.append(ec)

    def leave(_):
        _.exec_contexts.pop()


def is_activation_object(o):
    return get_class(o) == "activation_object"


def eval_op(_, op, args):
    if op == "get_property_dot":
        x = exp_val(_, args[0])
        assert args[1].kind == "identifier"
        return t_js_reference(to_object(_, x), args[1].value)

    if op == "postfix_inc":
        x = js_eval_exp(_, args[0])
        w = to_number(_, get_value(x))
        put_value(_, x, w + 1)
        return w

    if op == "postfix_dec":
        x = js_eval_exp(_, args[0])
        w = to_number(_, get_value(x))
        put_value(_, x, w - 1)
        return w

    if op == "void":
        exp_val(_, args[0])
        return js_undefined

    if op == "delete":
        v = js_eval_exp(_, args[0])
        base = get_base(v)
        prop_name = get_property_name(v)
        if type(base) is not t_js_object:
            return True
        try:
            return js_delete(base, prop_name)
        except t_unimplemented_property_error:
            return not js_has_property(base, prop_name)

    if op == "typeof":
        x = js_eval_exp(_, args[0])
        if type(x) is t_js_reference and get_base(x) is js_null:
            return "undefined"
        x = get_value(x)
        if type(x) is t_js_undefined:
            return t_js_string("undefined")
        if type(x) is t_js_null:
            return t_js_string("object")
        if type(x) is t_js_boolean:
            return t_js_string("boolean")
        if type(x) is t_js_number:
            return t_js_string("number")
        if type(x) is t_js_string:
            return t_js_string("string")
        if type(x) is t_js_object:
            if is_function(x):
                return t_js_string("function")
            return t_js_string("object")

    if op == "prefix_inc":
        x = js_eval_exp(_, args[0])
        w = to_number(_, get_value(x)) + 1
        put_value(_, x, w)
        return w

    if op == "prefix_dec":
        x = js_eval_exp(_, args[0])
        w = to_number(_, get_value(x)) - 1
        put_value(_, x, w)
        return w

    if op == "unary_plus":
        return to_number(_, exp_val(_, args[0]))

    if op == "unary_minus":
        return -to_number(_, exp_val(_, args[0]))

    if op == "bit_not":
        x = to_int_32(_, exp_val(_, args[0]))
        return to_int_32(_, t_js_number(~int(x)))

    if op == "not":
        return not to_boolean(exp_val(_, args[0]))

    if op == "&&":
        x = exp_val(_, args[0])
        if not to_boolean(x):
            return x
        return exp_val(_, args[1])

    if op == "||":
        x = exp_val(_, args[0])
        if to_boolean(x):
            return x
        return exp_val(_, args[1])

    if op == "cond":
        if to_boolean(exp_val(_, args[0])):
            return exp_val(_, args[1])
        return exp_val(_, args[2])

    if op == "=":
        x = js_eval_exp(_, args[0])
        y = js_eval_exp(_, args[1])
        value = get_value(y)
        put_value(_, x, value)
        return value

    if op == "=":
        x = js_eval_exp(_, args[0])
        y = js_eval_exp(_, args[1])
        value = get_value(y)
        put_value(_, x, value)
        return value

    if op[-1] == "=" and op not in ["==", "<=", ">=", "!="]:
        x = js_eval_exp(_, args[0])
        t = get_value(x)
        w = eval_op(_, op[:-1], [t, exp_val(_, args[1])])
        put_value(_, x, w)
        return w

    if op == "new":
        o = exp_val(_, args[0])
        new_args = eval_arguments(_, args[1].kids)
        if type(o) is not t_js_object:
            raise t_runtime_error()
        res = js_construct(_, o, new_args)
        if type(res) is not t_js_object:
            raise t_runtime_error()
        return res

    if op == "arguments":
        func = js_eval_exp(_, args[0])
        func_args = eval_arguments(_, args[1:])
        func_val = get_value(func)
        if type(func_val) is not t_js_object:
            raise t_runtime_error()
        this_val = get_base(func) if type(func) is t_js_reference else js_null
        if is_activation_object(this_val):
            this_val = js_null
        return js_call(_, func_val, this_val, func_args)

    x = exp_val(_, args[0])
    y = exp_val(_, args[1])
    if op == "get_property":
        return t_js_reference(to_object(_, x), to_string(_, y))
    if op == "*":
        x = to_number(_, x)
        y = to_number(_, y)
        return x * y
    if op == "/":
        x = to_number(_, x)
        y = to_number(_, y)
        if y == 0.0:
            if x == 0.0:
                return float("nan")
            if x == math.inf:
                return math.inf
            if x == -math.inf:
                return -math.inf
            else:
                if x < 0:
                    return -math.inf
                if x > 0:
                    return math.inf
        return x / y
    if op == "%":
        x = to_number(_, x)
        y = to_number(_, y)
        try:
            return math.fmod(x, y)
        except ValueError:
            return float("nan")
    if op == "+":
        x = to_primitive(_, x)
        y = to_primitive(_, y)
        if (type(x) is t_js_string) or (type(y) is t_js_string):
            return to_string(_, x) + to_string(_, y)
        return to_number(_, x) + to_number(_, y)
    if op == "-":
        return to_number(_, x) - to_number(_, y)
    if op == "<<":
        x = to_int_32(_, x)
        y = to_uint_32(_, y)
        return to_int_32(_, t_js_number(int(x) << (int(y) % 32)))
    if op == ">>":
        x = to_int_32(_, exp_val(_, arg[0]))
        y = to_uint_32(_, y)
        return t_js_number(int(x) >> (int(y) % 32))
    if op == ">>>":
        x = to_uint_32(_, x)
        y = to_uint_32(_, y)
        return t_js_number(int(x) >> (int(y) % 32))
    if op == "<":
        z = js_abstract_less_than(_, x, y)
        if z == js_undefined:
            return js_false
        return z
    if op == ">":
        z = js_abstract_less_than(_, y, x)
        if z == js_undefined:
            return js_false
        return z
    if op == "<=":
        z = js_abstract_less_than(_, y, x)
        if z in [js_true, js_undefined]:
            return js_false
        return js_true
    if op == ">=":
        z = js_abstract_less_than(_, x, y)
        if z in [js_true, js_undefined]:
            return js_false
        return js_true
    if op == "==":
        return js_abstract_equals(_, x, y)
    if op == "!=":
        return not js_abstract_equals(_, x, y)
    if op == "&":
        return t_js_number(int(to_int_32(_, x)) & int(to_int_32(_, y)))
    if op == "|":
        return t_js_number(int(to_int_32(_, x)) | int(to_int_32(_, y)))
    if op == "^":
        return t_js_number(int(to_int_32(_, x)) ^ int(to_int_32(_, y)))
    if op == ",":
        return y
    assert False, kind


def js_eval_exp(_, x):
    if type(x) is not t_ast:
        return x
    if x.kind == "prim_exp" and x.kids[0].kind != "exp":
        return eval_leaf(_, x.kids[0].kind, x.kids[0].value)
    if not x.kind.endswith("exp"):
        return eval_op(_, x.kind, x.kids)
    res = js_eval_exp(_, x.kids[0])
    for i in range(1, len(x.kids)):
        res = eval_op(_, x.kids[i].kind, [res, *x.kids[i].kids])
    return res


def eval_var_decl(_, decl):
    if len(decl.kids) == 2:
        assert decl.kids[0].kind == "identifier"
        var = eval_identifier(_, decl.kids[0].value)
        value = get_value(js_eval_exp(_, decl.kids[1]))
        put_value(_, var, value)


def js_eval_stmt(_, x):
    if x.kind == "func_decl":
        return normal_completion

    if x.kind == "stmt":
        assert x.kids != []
        x = x.kids[0]

    if x.kind == "block":
        ret = normal_completion
        for i in range(len(x.kids)):
            if ret.kind != "normal":
                break
            c = js_eval_stmt(_, x.kids[i])
            v = c.value
            if v is None:
                v = ret.value
            ret = t_js_completion(c.kind, v)
        return ret

    if x.kind == "var_stmt":
        for decl in x.kids:
            eval_var_decl(_, decl)
        return normal_completion

    if x.kind == "empty_stmt":
        return normal_completion

    if x.kind == "exp_stmt":
        value = get_value(js_eval_exp(_, x.kids[0]))
        return t_js_completion("normal", value)

    if x.kind == "if_stmt":
        if to_boolean(exp_val(_, x.kids[0])):
            return js_eval_stmt(_, x.kids[1])
        else:
            if len(x.kids) == 2:
                return normal_completion
            return js_eval_stmt(_, x.kids[2])

    if x.kind == "while_stmt":
        c = normal_completion
        while True:
            if not to_boolean(exp_val(_, x.kids[0])):
                break
            sc = js_eval_stmt(_, x.kids[1])
            if sc.value is not None:
                c = t_js_completion("normal", sc.value)
            if sc.kind == "break":
                break
            if sc.kind == "return":
                return sc
        return c

    if x.kind == "for_stmt":
        is_for_in = (len(x.kids) == 3)
        if is_for_in:
            is_for_var_in = (x.kids[0].kind == "var_decl_list")
            if is_for_var_in:
                var_decl = x.kids[0].kids[0]
                has_initializer = (len(var_decl.kids) == 2)
                if has_initializer:
                    iter_var = eval_identifier(_, iter_id)
                    value = exp_val(_, var_decl.kids[1])
                    put_value(_, iter_var, value)
            obj = to_object(_, exp_val(_, x.kids[1]))
            c = normal_completion
            visited_props = set()
            while obj is not js_null:
                for name in obj.dict_.copy():
                    prop = obj.find(name)
                    if prop is None or name in visited_props:
                        continue
                    if prop.dont_enum or prop.internal:
                        continue
                    visited_props.add(name)
                    iter_var = None
                    if is_for_var_in:
                        iter_name = var_decl.kids[0].value
                        iter_var = eval_identifier(_, iter_name)
                    else:
                        iter_var = js_eval_exp(_, x.kids[0].kids[0])
                    put_value(_, iter_var, name)
                    sc = js_eval_stmt(_, x.kids[2])
                    if sc.value is not None:
                        c = t_js_completion("normal", sc.value)
                    if sc.kind == "break":
                        return c
                    if sc.kind == "return":
                        return sc
                obj = obj.get_prototype()
            return c
        else:
            if x.kids[0].kind == "opt_exp":
                if not x.kids[0].is_leaf():
                    exp_val(_, x.kids[0].kids[0])
            else:
                for decl in x.kids[0].kids:
                    eval_var_decl(_, decl)
            c = normal_completion
            while True:
                if not x.kids[1].is_leaf():
                    if not to_boolean(exp_val(_, x.kids[1].kids[0])):
                        break
                sc = js_eval_stmt(_, x.kids[3])
                if sc.value is not None:
                    c = t_js_completion("normal", sc.value)
                if sc.kind == "break":
                    break
                if sc.kind == "return":
                    return sc
                if not x.kids[2].is_leaf():
                    exp_val(_, x.kids[2].kids[0])
            return c

    if x.kind == "continue_stmt":
        return t_js_completion("continue", None)

    if x.kind == "break_stmt":
        return t_js_completion("break", None)

    if x.kind == "return_stmt":
        if x.kids != []:
            value = get_value(js_eval_exp(_, x.kids[0]))
            return t_js_completion("return", value)
        return t_js_completion("return", js_undefined)

    if x.kind == "with_stmt":
        obj = to_object(_, exp_val(_, x.kids[0]))
        _.exec_context().scope_chain.push(obj)
        res = js_eval_stmt(_, x.kids[1])
        _.exec_context().scope_chain.pop()
        return res

    assert false, f"bad kind '{x.kind}'"


def instantiate_params(_, params, args):
    for i in range(len(params)):
        val = args[i] if i < len(args) else js_undefined
        _.exec_context().instantiate_parameter(params[i], val)


def instantiate_vars(_, ast):
    if ast.kind == "var_decl":
        assert ast.kids[0].kind == "identifier"
        name = ast.kids[0].value
        _.exec_context().instantiate_var_decl(name)
    else:
        for kid in ast.kids:
            instantiate_vars(_, kid)


def instantiate_funcs(_, ast):
    for kid in ast.kids:
        if kid.kind == "func_decl":
            assert kid.kids[0].kind == "identifier"
            name = kid.kids[0].value
            args = [x.value for x in kid.kids[1].kids]
            func = create_function(_, args, kid.kids[2])
            _.exec_context().instantiate_func_decl(name, func)


def create_native_function(_, func, length):
    res = t_js_object()
    res.put_internal("class", "Function")
    res.put_internal("prototype", _.function_prototype)
    # res.put_internal("function_name", name)
    res.put_internal("call", func)
    res.put("length", t_js_number(length), length_attrs)
    proto = t_js_object()
    proto.put_internal("prototype", _.object_prototype)
    proto.put_internal("class", "Object")
    proto.put("constructor", res, {"dont_enum"})
    res.put("prototype", proto, {"dont_enum"})
    return res


def default_construct(_, args, func):
    obj = t_js_object()
    obj.put_internal("class", "Object")
    prototype = func.get("prototype").value
    if type(prototype) is t_js_object:
        obj.put_internal("prototype", prototype)
    else:
        obj.put_internal("prototype", _.object_prototype)
    x = js_call(_, func, obj, args)
    if type(x) is t_js_object:
        return x
    return obj


def create_function(_, params, body, name="", body_string=""):
    res = t_js_object()
    res.put_internal("class", "Function")
    res.put_internal("prototype", _.function_prototype)
    # res.put_internal("function_name", name)
    res.put_internal("function_params", params)
    # res.put_internal("function_body_string", body_string)
    res.put_internal("call", body)
    res.put_internal("construct", default_construct)
    res.put("length", t_js_number(len(params)), length_attrs)
    prototype = create_new_object(_)
    prototype.put("constructor", res, {"dont_enum"})
    res.put("prototype", prototype, {"dont_delete"})
    return res


def put_native_method(_, o, name, func, length=0):
    o.put(name, create_native_function(_, func, length), {"dont_enum"})


def eval_program(_, ast):
    res = js_eval_stmt(_, ast.kids[0])
    for i in range(1, len(ast.kids)):
        x = js_eval_stmt(_, ast.kids[i])
        if x.value is not None:
            res = x
    return res


def enter_global_code(state, ast):
    state.enter_global_code()
    for kid in ast.kids:
        if kid.kind == "var_stmt":
            identifier = kid.kids[0].kids[0].value
            state.exec_context().instantiate_var_decl(identifier)
    state.leave()


def execute(ast):
    assert ast.kind == "program"
    assert ast.kids != []
    _ = t_js_state()
    _.enter_global_code()
    instantiate_funcs(_, ast)
    instantiate_vars(_, ast)
    res = eval_program(_, ast)
    _.leave()
    assert _.exec_contexts == []
    return res


def interpret(code):
    return execute(parse_code(code))
