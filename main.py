#!/usr/bin/python3

from collections import namedtuple
import math
import copy
import sys
import traceback
import random

class t_loc:
    def __init__(_):
        _.line = 1
        _.column = 0

    def advance(_, on_newline):
        if on_newline:
            _.column = 0
            _.line += 1
        else:
            _.column += 1

    def __repr__(_):
        return f"loc({_.line}, {_.column})"

    def __str__(_):
        return _.__repr__()


class t_runtime_error(Exception):
    pass


class t_unimplemented_property_error(t_runtime_error):
    def __init__(_):
        pass


class t_error(Exception):
    def __init__(_, message, loc):
        _.message = message
        _.loc = loc


class t_syntax_error(t_error):
    def __init__(_, message, loc):
        super().__init__(message, loc)


t_lexeme = namedtuple("lexeme", "kind value loc")

def print_lexemes(l):
    print([(i.value, i.loc) for i in l])

def is_identifier_char(ch):
    return len(ch) == 1 and ((ch.isascii() and ch.isalnum()) or (ch in "$_"))


def is_digit(ch):
    return len(ch) == 1 and ch.isascii() and ch.isdigit()


def is_octal_digit(ch):
    return len(ch) == 1 and ch in "01234567"


def is_hex_digit(ch):
    return len(ch) == 1 and ch in "0123456789abcdefABCDEF"


def char_in(ch, s):
    return len(ch) == 1 and (ch in s)


class t_lexer:
    def __init__(_, string):
        _.string = string
        _.pos = 0
        _.loc = t_loc()
        _.lexeme_loc = copy.copy(_.loc)
        _.result = []

    def peek(_, j=0):
        if _.pos + j >= len(_.string):
            return ""
        return _.string[_.pos + j]

    def advance(_):
        if _.pos >= len(_.string):
            return ""
        ch = _.string[_.pos]
        _.loc.advance(ch == "\n")
        _.pos += 1
        return ch

    def at_end(_):
        return pos >= len(_.string)

    def push(_, kind, value):
        _.result.append(t_lexeme(kind, value, _.lexeme_loc))

    def match(_, s):
        if _.string[_.pos:(_.pos + len(s))] == s:
            for i in range(len(s)):
                _.advance()
            return True
        return False

    def err(_, where=""):
        ch = _.peek()
        if ch == "":
            ch = "eof"
        elif ch == "\n":
            ch = "eol"
        elif not ch.isprintable():
            ch = ""
        msg = "unexpected symbol"
        if ch != "":
            msg += " " + ch
        if where != "":
            msg += " in " + where
        raise t_syntax_error(msg, _.loc)

    def at_end(_):
        return _.peek() == ""

    def whitespace(_):
        w = "\t\v\f "
        if not char_in(_.peek(), w):
            return False
        while char_in(_.peek(), w):
            _.advance()
        return True

    def line_terminator(_, only_check=False):
        if _.peek() not in ["\n", "\r"]:
            return False
        if only_check:
            return True
        s = _.advance()
        # _.push("line_terminator", s)
        return True

    def multiline_comment(_):
        if not _.match("/*"):
            return False
        while not _.match("*/"):
            _.advance()
        return True

    def singleline_comment(_):
        if not _.match("//"):
            return False
        while not _.line_terminator(True):
            _.advance()
        return True

    def comment(_):
        return _.singleline_comment() or _.multiline_comment()

    def identifier(_):
        if not is_identifier_char(_.peek()) or is_digit(_.peek()):
            return False
        s = ""
        while is_identifier_char(_.peek()):
            s += _.advance()
        keywords = ["break", "continue", "delete", "else", "for", "function",
                    "if", "in", "new", "return", "this", "typeof", "var",
                    "void", "while", "with"]
        if (s in keywords) or (s in ["true", "false", "null"]):
            _.push(s, s)
        else:
            _.push("identifier", s)
        return True

    def punctuator(_):
        punctuators = [">>>=", ">>>", ">>=", "<<=", "==", "<=", ">=", "!=",
                       "&&", "||", "++", "--", "<<", ">>", "+=", "-=", "*=",
                       "/=", "&=", "|=", "^=", "%=", "=", ">", "<", ",", "!",
                       "~", "?", ":", ".", "+", "-", "*", "/", "&", "|", "^",
                       "%", "(", ")", "{", "}", "[", "]", ";"]
        for p in punctuators:
            if _.match(p):
                _.push(p, p)
                return True
        return False

    def decimal_literal(_):
        if not (is_digit(_.peek())
                or (_.peek() == "." and is_digit(_.peek(1)))):
            return False
        s = ""
        while is_digit(_.peek()):
            s += _.advance()
        if _.peek() == ".":
            s += _.advance()
            while is_digit(_.peek()):
                s += _.advance()
        if _.peek() in ["e", "E"]:
            s += _.advance()
            if _.peek() in ["+", "-"]:
                s += _.advance()
            u = ""
            while is_digit(_.peek()):
                u += _.advance()
            if u == "":
                _.err("decimal_literal::exponent_part")
            s += u
        _.push("decimal_literal", s)
        return True

    def octal_literal(_):
        if not (_.peek() == "0" and is_octal_digit(_.peek(1))):
            return False
        s = _.advance()
        s += _.advance()
        while is_octal_digit(_.peek()):
            s += _.advance()
        _.push("octal_literal", s)
        return True

    def hex_literal(_):
        if not (_.peek() == "0" and _.peek(1) in ["x", "X"]):
            return False
        s = _.advance()
        s += _.advance()
        while is_hex_digit(_.peek()):
            s += _.advance()
        if len(s) == 2:
            _.err("hex_literal")
        _.push("hex_literal", s)
        return True

    def numeric_literal(_):
        return (_.hex_literal() or _.octal_literal()
                or _.decimal_literal())

    def string_literal(_):
        if _.peek() not in ["'", "\""]:
            return False
        s = _.advance()
        while True:
            if _.at_end() or _.line_terminator(True):
                _.err("string_literal")
            if _.peek() == s[0]:
                s += _.advance()
                break
            if _.peek() == "\\":
                s += _.advance()
                if _.at_end() or _.line_terminator(True):
                    _.err("string_literal::escape_sequence")
                if char_in(_.peek(), "'\"\\bfnrt"):
                    s += _.advance()
                elif is_octal_digit(_.peek()):
                    if char_in(_.peek(), "0123"):
                        s += _.advance()
                        if is_octal_digit(_.peek()):
                            s += _.advance()
                            if is_octal_digit(_.peek()):
                                s += _.advance()
                    else:
                        s += _.advance()
                        if is_octal_digit(_.peek()):
                            s += _.advance()
                elif _.peek() == "x":
                    s += _.advance()
                    for i in range(2):
                        if not is_hex_digit(_.peek()):
                            _.err("string_literal::hex_escape_sequence")
                        s += _.advance()
                elif _.peek() == "u":
                    s += _.advance()
                    for i in range(4):
                        if not is_hex_digit(_.peek()):
                            _.err("string_literal::unicode_escape_sequence")
                        s += _.advance()
                else:
                    s += _.advance()
            else:
                s += _.advance()
        _.push("string_literal", s)
        return True

    def literal(_):
        return _.numeric_literal() or _.string_literal()

    def token(_):
        return _.identifier() or _.literal() or _.punctuator()

    def lex(_):
        while True:
            _.lexeme_loc = copy.copy(_.loc)
            if not (_.comment() or _.whitespace() or _.token()
                    or _.line_terminator()):
                break
        if not _.at_end():
            _.err()
        _.push("eof", "")


def lex(src):
    lexer = t_lexer(src)
    lexer.lex()
    return lexer.result


class t_ast:
    def __init__(_, kind=None, value=None, kids=None, parent=None, loc=None):
        _.kind = kind
        _.value = value
        if kids is None:
            _.kids = []
        else:
            _.kids = kids
        _.parent = parent
        _.loc = loc

    def is_null(_):
        return _.kind is None and _.kids == []

    def is_leaf(_):
        return _.kids == []

    def add_kid(_, x):
        _.kids.append(x)

    def show(_, level=0):
        if _.kind is None:
            if not _.is_leaf():
                _.kids[0].show(level)
            return
        if _.kind.endswith("_exp") and len(_.kids) == 1:
            _.kids[0].show(level)
            return
        ss = 2 * " " * level + _.kind
        if _.is_leaf():
            if _.value is not None:
                print(ss + " " + _.value)
            else:
                print(ss)
        else:
            print(ss)
            for kid in _.kids:
                kid.show(level + 1)


def parse(lexemes, rule_name="program"):
    pos = 0
    result = t_ast()
    cur_node = result
    rule_names = []
    only_check = False
    rules = {}

    def advance():
        nonlocal pos
        if not at_end():
            pos += 1

    def peek():
        return lexemes[pos]

    def at_end():
        return lexemes[pos].kind == "eof"

    def err(message):
        raise t_error(message + ": " + peek().kind, peek().loc)

    def extract(kind):
        if peek().kind != kind:
            m = cur_rule() + ": expected " + kind + ", got " + peek().kind
            raise t_error(m, peek().loc)
        value = peek().value
        advance()
        return value

    def add_leaf(kind, value):
        node = t_ast(kind, value, parent=cur_node, loc=peek().loc)
        cur_node.add_kid(node)

    def enter_rule(name):
        rule_names.append(name)

    def cur_rule():
        return rule_names[-1]

    def leave_rule():
        rule_names.pop()

    def create_node():
        nonlocal cur_node
        loc = peek().loc
        node = t_ast(cur_rule(), parent=cur_node, loc=loc)
        cur_node.add_kid(node)
        cur_node = node

    def replace_node():
        loc = peek().loc
        node = t_ast(cur_rule(), parent=cur_node, loc=loc)
        node.add_kid(last_kid())
        kids[-1] = node
        cur_node = node

    def leave_node():
        nonlocal cur_node
        cur_node = cur_node.parent

    def last_kid():
        return cur_node.kids[-1]

    def check(rule):
        nonlocal only_check
        if only_check:
            return apply(rule)
        only_check = True
        res = apply(rule)
        only_check = False
        return res

    def apply(rule):
        if type(rule) is str:
            if rule[0] != "@":
                if only_check:
                    return peek().kind == rule
                else:
                    if peek().kind == rule:
                        advance()
                        return True
                    else:
                        return False
            name = rule[1:]
            enter_rule(name)
            res = rules[name]()
            leave_rule()
            return res
        else:
            return rule()

    def apply_err(rule):
        if not apply(rule):
            msg = cur_rule() + ": unexpected symbol: " + peek().kind
            raise t_error(msg, peek().loc)

    def plus(symbol):
        def f():
            if not check(symbol):
                return False
            if only_check:
                return True
            while apply(symbol):
                pass
            return True
        return f

    def star(symbol):
        return opt(plus(symbol))

    def seq(*symbols):
        def f():
            if not check(symbols[0]):
                return False
            if only_check:
                return True
            for symbol in symbols:
                apply_err(symbol)
            return True
        return f

    def always(rule):
        def f():
            if only_check:
                return True
            apply(rule)
            return True
        return f

    def build(rule):
        def f():
            if not check(rule):
                return False
            if only_check:
                return True
            create_node()
            apply(rule)
            leave_node()
            return True
        return f

    def bar(*rules):
        def f():
            for rule in rules:
                if apply(rule):
                    return True
            return False
        return f

    def opt(rule):
        def f():
            apply(rule)
            return True
        return f

    def comma_list(rule):
        def f():
            if not check(rule):
                return False
            if only_check:
                return True
            apply(rule)
            while True:
                if not apply(","):
                    break
                apply_err(rule)
            return True
        return f

    def rule(name, *symbols):
        rules[name] = build(seq(*symbols))

    def leaf(name, symbol):
        def f():
            if not check(symbol):
                return False
            if only_check:
                return True
            add_leaf(peek().kind, peek().value)
            advance()
            return True
        rules[name] = f

    leaf("identifier", "identifier")
    leaf("literal", bar("null", "true", "false",
                        "octal_literal", "hex_literal", "decimal_literal",
                        "string_literal"))
    rule("this", "this")

    rule("prim_exp",
         bar("@this",
             "@identifier",
             "@literal",
             seq("(", "@exp", ")")))

    rule("arguments", "(", opt(comma_list("@assign_exp")), ")")
    rule("new", "new", "@member_exp", opt("@arguments"))

    rule("get_property", "[", "@exp", "]")
    rule("get_property_dot", ".", "@identifier")

    rule("member_exp",
         bar("@prim_exp", "@new"),
         star(bar("@get_property",
                  "@get_property_dot")))

    rule("lhs_exp",
         "@member_exp",
         opt(seq("@arguments",
                 star(bar("@arguments",
                          "@get_property",
                          "@get_property_dot")))))

    rule("postfix_inc", "++")
    rule("postfix_dec", "--")
    rule("postfix_exp",
         "@lhs_exp",
         opt(bar("@postfix_inc",
                 "@postfix_dec")))

    rule("delete", "delete", "@unary_exp")
    rule("void", "void", "@unary_exp")
    rule("typeof", "typeof", "@unary_exp")
    rule("prefix_inc", "++", "@unary_exp")
    rule("prefix_dec", "--", "@unary_exp")
    rule("unary_plus", "+", "@unary_exp")
    rule("unary_minus", "-", "@unary_exp")
    rule("bit_not", "~", "@unary_exp")
    rule("not", "!", "@unary_exp")
    rule("unary_exp",
         bar("@delete",
             "@void",
             "@typeof",
             "@prefix_inc",
             "@prefix_dec",
             "@unary_plus",
             "@unary_minus",
             "@bit_not",
             "@not",
             "@postfix_exp"))

    def left_assoc_op(name, subexp, ops):
        for op in ops:
            rule(op, op, subexp)
        rule(name, subexp, star(bar(*[("@" + op) for op in ops])))

    left_assoc_op("mul_exp", "@unary_exp", ["*", "/", "%"])
    left_assoc_op("add_exp", "@mul_exp", ["+", "-"])
    left_assoc_op("shift_exp", "@add_exp", ["<<", ">>", ">>>"])
    left_assoc_op("rel_exp", "@shift_exp", ["<", ">", "<=", ">="])
    left_assoc_op("eq_exp", "@rel_exp", ["==", "!="])
    left_assoc_op("bit_and_exp", "@eq_exp", ["&"])
    left_assoc_op("bit_xor_exp", "@bit_and_exp", ["^"])
    left_assoc_op("bit_or_exp", "@bit_xor_exp", ["|"])
    left_assoc_op("and_exp", "@bit_or_exp", ["&&"])
    left_assoc_op("or_exp", "@and_exp", ["||"])

    rule("cond", "?", "@assign_exp", ":", "@assign_exp")
    rule("cond_exp", "@or_exp", opt("@cond"))

    def assign_exp():
        if not check("@cond_exp"):
            return False
        if only_check:
            return True
        apply_err("@cond_exp")
        if peek().kind in ["=", "*=", "/=", "%=", "+=", "-=",
                           "<<=", ">>=", ">>>=", "&=", "^=", "|="]:
            op = peek().kind
            enter_rule(op)
            advance()
            create_node()
            apply_err("@assign_exp")
            leave_node()
            leave_rule()
        return True
    rule("assign_exp", assign_exp)

    left_assoc_op("exp", "@assign_exp", [","])

    rule("stmt",
         bar("@block",
             "@var_stmt",
             "@empty_stmt",
             "@exp_stmt",
             "@if_stmt",
             "@while_stmt",
             "@for_stmt",
             "@continue_stmt",
             "@break_stmt",
             "@return_stmt",
             "@with_stmt"))

    rule("opt_exp", opt("@exp"))

    rule("block", "{", star("@stmt"), "}")

    rule("var_decl", "@identifier", opt(seq("=", "@assign_exp")))
    rule("var_stmt", "var", comma_list("@var_decl"), ";")

    rule("empty_stmt", ";")
    rule("exp_stmt", "@exp", ";")
    rule("if_stmt",
         "if", "(", "@exp", ")", "@stmt", opt(seq("else", "@stmt")))

    rule("while_stmt", "while", "(", "@exp", ")", "@stmt")

    rule("var_decl_list", comma_list("@var_decl"))
    rule("for_stmt",
         "for", "(",
         bar(seq("var", "@var_decl_list"),
             "@opt_exp"),
         bar(seq(";", "@opt_exp", ";", "@opt_exp"),
             seq("in", "@exp")),
         ")", "@stmt")

    rule("continue_stmt", "continue", ";")
    rule("break_stmt", "break", ";")
    rule("return_stmt", "return", opt("@exp"))
    rule("with_stmt", "with", "(", "@exp", ")", "@stmt")

    rule("formal_params",
         opt(comma_list("@identifier")))
    rule("func_body", star("@stmt"))
    rule("func_decl",
         "function", "@identifier", "(", "@formal_params", ")",
         "{", "@func_body", "}")

    rule("program",
         always(seq(plus(bar("@func_decl", "@stmt")), "eof")))

    apply("@" + rule_name)
    return result.kids[0]


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


def array_prototype_reverse(_, this, args):
    length = to_uint_32(_, js_get(this, "length"))
    for k in range(int(length // 2)):
        i = to_string(_, t_js_number(k))
        j = to_string(_, length - 1 - k)
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

        put_native_method(_, _.global_object, "eval", js_eval, 1)

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


def enter_global_code(state, ast):
    state.enter_global_code()
    for kid in ast.kids:
        if kid.kind == "var_stmt":
            identifier = kid.kids[0].kids[0].value
            state.exec_context().instantiate_var_decl(identifier)
    state.leave()


class t_tester:
    def __init__(_):
        _.tests = []

    def add_test(_, code, expected_result):
        _.tests.append((code, expected_result))

    def run_tests(_):
        print("running tests ...")
        failure_cnt = 0
        for i in range(len(_.tests)):
            code = _.tests[i][0]
            expected = _.tests[i][1]
            print()
            print("*** test " + str(i) + " ***")
            # print(code)
            # parse_code(code).show()
            res = None
            try:
                res = execute(parse_code(code)).value
                if res != expected:
                    print(code)
                    print("expected " + repr(expected) + ", got " + repr(res))
                    print("-" * 79)
                    failure_cnt += 1
            except:
                print(code)
                traceback_lines = traceback.format_exc().splitlines()
                if len(traceback_lines) > 10:
                    traceback_lines = traceback_lines[-9:]
                    print("  ...")
                for line in traceback_lines:
                    print(line)
                print("-" * 79)
                failure_cnt += 1
        success_cnt = len(_.tests) - failure_cnt
        print(f"{success_cnt} successes, {failure_cnt} failures")


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

tester.run_tests()
