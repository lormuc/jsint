#!/usr/bin/python3

from collections import namedtuple
from enum import Enum
import math
import copy
import sys

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


def parse(lexemes):
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
    rule("func_decl",
         "function", "@identifier", "(", "@formal_params", ")", "@block")

    rule("program",
         always(seq(plus(bar("@func_decl", "@stmt")), "eof")))

    apply("@program")
    return result


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

    def put(_, prop_name, value,
            read_only=False, dont_enum=False, dont_delete=False,
            internal=False):
        p = t_js_property(value, read_only, dont_enum, dont_delete, internal)
        _.props[prop_name] = p

    def set_value(_, prop_name, value):
        new_prop = _.props[prop_name]._replace(value=value)
        _.props[prop_name] = new_prop

    def delete(_, prop_name):
        del _.props[prop_name]


class t_scope_chain:
    def __init__(_, stack=[]):
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
        _.variable_object.put(identifier, value, dont_delete=_.dont_delete)

    def instantiate_func_decl(_, identifier, func):
        _.put(identifier, func)

    def instantiate_parameter(_, identifier, value):
        _.put(identifier, value)

    def instantiate_var_decl(_, identifier):
        if not _.variable_object.get(identifier):
            _.put(identifier, js_undefined)

    def copy(_):
        return t_exec_context(_.scope_chain.copy(), _.variable_object, _.this)


class t_js_state:
    def __init__(_):
        _.global_object = t_js_object()
        _.global_object.put_internal("prototype", js_null)
        _.global_object.put_internal("class", "Object")
        _.exec_contexts = []

    def exec_context(_):
        return _.exec_contexts[-1]

    def enter_global_code(_):
        ec = t_exec_context(t_scope_chain([_.global_object]),
                            _.global_object, _.global_object)
        _.exec_contexts.append(ec)

    def enter_eval_code(_):
        if exec_contexts == []:
            _.enter_global_code()
            return
        _.exec_contexts.append(_.exec_context().copy())

    def enter_func_code(_, this, params, func_obj):
        if type(this) is not t_js_object:
            this = _.global_object
        activation_object = t_js_object()
        activation_object.put_internal("class", "activation_object")
        arguments_object = t_js_object() # todo
        arguments_object.put("callee", func_obj, dont_enum=True)
        arguments_object.put("length", len(params), dont_enum=True)
        # todo
        for i in range(len(params)):
            param = to_string(_, params[i])
            arguments_object.put(param, params[i], dont_enum=True)
        activation_object.put("arguments", arguments_object, dont_delete=True)
        ec = t_exec_context(t_scope_chain([_.global_object,
                                           activation_object]),
                            activation_object, this, dont_delete=True)
        _.exec_contexts.append(ec)

    def leave(_):
        _.exec_contexts.pop()


js_state = t_js_state()


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


def js_put(o, prop_name, value):
    if not js_can_put(o, prop_name):
        return
    x = o.get(prop_name)
    if x is not None:
        o.set_value(prop_name, value)
        return
    o.put(prop_name, value)


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


def js_construct(_, o, args):
    pass


def js_call(_, func, this, args):
    body = func.get_internal("function_body")
    params = func.get_internal("function_params")
    _.enter_func_code(this, args, func)
    instantiate_params(_, params, args)
    instantiate_vars(_, body)
    res = js_eval_stmt(_, body)
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
        js_put(_.global_object, get_property_name(v), w)
    else:
        js_put(b, get_property_name(v), w)


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
    # todo
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
            return js_undefined
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
        new_args = eval_arguments(_, args[1:])
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
            func = create_function(name, args, kid.kids[2])
            _.exec_context().instantiate_func_decl(name, func)


#todo
object_prototype = js_null
function_prototype = js_null


def create_object():
    o = t_js_object()
    o.put_internal("prototype", object_prototype)
    o.put_internal("class", "Object")
    return o


def create_function(name, params, body):
    res = t_js_object()
    res.put_internal("class", "Function")
    res.put_internal("prototype", function_prototype)
    res.put_internal("function_name", name)
    res.put_internal("function_params", params)
    res.put_internal("function_body", body)
    l = len(params)
    res.put("length", l, dont_delete=True, dont_enum=True, read_only=True)
    proto = create_object()
    proto.put("constructor", res, dont_enum=True)
    res.put("prototype", proto, dont_enum=True)
    return res


def js_eval(ast):
    assert ast.kind == "program"
    assert ast.kids != []
    _ = t_js_state()
    _.enter_global_code()
    instantiate_funcs(_, ast)
    instantiate_vars(_, ast)
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


def eval_program(code):
    return js_eval(parse(lex(code)).kids[0])


class t_tester:
    def __init__(_):
        _.tests = []

    def add_test(_, code, expected_result):
        _.tests.append((code, expected_result))

    def run_tests(_):
        for test in _.tests:
            code = test[0]
            expected = test[1]
            result = eval_program(code).value
            if result != expected:
                print("-" * 79)
                print(code)
                print("expected " + repr(expected) + ", got " + repr(result))
                print("-" * 79)


tester = t_tester()

tester.add_test("""
function f(x) {
 return x + 1;
}
f(0);
""", 1.0)

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
0 / 0;
""", True)

tester.run_tests()
sys.exit()

src = """
this.a = 3;
this["a"];
"""
ast = parse(lex(src))
ast.show()
print(js_eval(ast.kids[0]))
