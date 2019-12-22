from collections import namedtuple
import copy

class t_error(Exception):
    def __init__(_, message, loc):
        _.message = message
        _.loc = loc


class t_syntax_error(t_error):
    def __init__(_, message, loc):
        super().__init__(message, loc)


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


t_lexeme = namedtuple("lexeme", "kind value loc")


def is_identifier_char(ch):
    return len(ch) == 1 and ((ord(ch) < 128 and ch.isalnum()) or (ch in "$_"))


def is_digit(ch):
    return len(ch) == 1 and ord(ch) < 128 and ch.isdigit()


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
