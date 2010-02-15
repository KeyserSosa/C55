#!/usr/bin/env python
################################################################################# The contents of this file are subject to the Common Public Attribution
# License Version 1.0. (the "License"); you may not use this file except in
# compliance with the License. You may obtain a copy of the License at
# http://code.reddit.com/LICENSE. The License is based on the Mozilla Public
# License Version 1.1, but Sections 14 and 15 have been added to cover use of
# software over a computer network and provide for limited attribution for the
# Original Developer. In addition, Exhibit A has been modified to be consistent
# with Exhibit B.
# 
# Software distributed under the License is distributed on an "AS IS" basis,
# WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
# the specific language governing rights and limitations under the License.
# 
# The Original Code is Reddit.
# 
# The Original Developer is the Initial Developer.  The Initial Developer of the
# Original Code is CondeNet, Inc.
# 
# All portions of the code written by CondeNet are Copyright (c) 2006-2009
# CondeNet, Inc. All Rights Reserved.
################################################################################
"""
  conversts C55 to CSS
"""
from __future__ import with_statement
from ply import lex, yacc
import re
#from cssfilter import validate_css

re_atoms_tup = \
(("h"	    , r"[0-9a-f]"),
 ("nl"	    , r"\n|\r\n|\r|\f"),
 ("nonascii", r"[\200-\377]"),
 ("unicode" , r"\\(%(h)s){1,6}(\r\n|[ \t\r\n\f])?"),
 ("escape"  , r"(%(unicode)s)|\\[^\r\n\f0-9a-f]"),
 ("nmstart" , r"[_a-z]|(%(nonascii)s)|(%(escape)s)"),
 ("nmchar"  , r"[_a-z0-9-]|(%(nonascii)s)|(%(escape)s)"),
 ("string1" , r'"([^\n\r\f\\"]|\\(%(nl)s)|(%(escape)s))*"'), 
 ("string2" , r"'([^\n\r\f\\']|\\(%(nl)s)|(%(escape)s))*'"), 
 ("invalid1", r'"([^\n\r\f"]|\\(%(nl)s)|(%(escape)s))*'),
 ("invalid2", r"'([^\n\r\f']|\\(%(nl)s)|(%(escape)s))*"), 
 ("comment" , r"\/\*[^*]*\*+([^/*][^*]*\*+)*\/"),
 ("ident"   , r"-?(%(nmstart)s)(%(nmchar)s)*"),
 ("name"    , r"(%(nmchar)s)+"),
 ("num"	    , r"([0-9]*\.?[0-9]+)"),
 ("string"  , r"(%(string1)s)|(%(string2)s)"),
 ("invalid" , r"(%(invalid1)s)|(%(invalid2)s)"),
 ("url"	    , r"([!#$%%&*-~]|(%(nonascii)s)|(%(escape)s))*"),
 ("s"	    , r"[ \t\r\n\f]+"),
 ("w"	    , r"(%(s)s)?"),
 ("A"	    , r"a|\\0{0,4}(41|61)(\r\n|[ \t\r\n\f])?"),
 ("C"	    , r"c|\\0{0,4}(43|63)(\r\n|[ \t\r\n\f])?"),
 ("D"	    , r"d|\\0{0,4}(44|64)(\r\n|[ \t\r\n\f])?"),
 ("E"	    , r"e|\\0{0,4}(45|65)(\r\n|[ \t\r\n\f])?"),
 ("G"	    , r"g|\\0{0,4}(47|67)(\r\n|[ \t\r\n\f])?|\\g"),
 ("H"	    , r"h|\\0{0,4}(48|68)(\r\n|[ \t\r\n\f])?|\\h"),
 ("I"	    , r"i|\\0{0,4}(49|69)(\r\n|[ \t\r\n\f])?|\\i"),
 ("K"	    , r"k|\\0{0,4}(4b|6b)(\r\n|[ \t\r\n\f])?|\\k"),
 ("M"	    , r"m|\\0{0,4}(4d|6d)(\r\n|[ \t\r\n\f])?|\\m"),
 ("N"	    , r"n|\\0{0,4}(4e|6e)(\r\n|[ \t\r\n\f])?|\\n"),
 ("O"	    , r"o|\\0{0,4}(51|71)(\r\n|[ \t\r\n\f])?|\\o"),
 ("P"	    , r"p|\\0{0,4}(50|70)(\r\n|[ \t\r\n\f])?|\\p"),
 ("R"	    , r"r|\\0{0,4}(52|72)(\r\n|[ \t\r\n\f])?|\\r"),
 ("S"	    , r"s|\\0{0,4}(53|73)(\r\n|[ \t\r\n\f])?|\\s"),
 ("T"	    , r"t|\\0{0,4}(54|74)(\r\n|[ \t\r\n\f])?|\\t"),
 ("X"	    , r"x|\\0{0,4}(58|78)(\r\n|[ \t\r\n\f])?|\\x"),
 ("Z"	    , r"z|\\0{0,4}(5a|7a)(\r\n|[ \t\r\n\f])?|\\z"))

re_atoms = {}
for k, v in re_atoms_tup:
    re_atoms[k] = v % re_atoms


def _punc(x):
    return (r"%(w)s" + x + r"%(w)s") % re_atoms

#def _num_str(s):
#    if isinstance(s, str):
#        return "(%(num)s)" % re_atoms + _str_to_re(s)
#    return (("(%(num)s)" % re_atoms) +
#            "(%s)" % "|".join(_str_to_re(x) for x in s))

def _num_label(s):
    if isinstance(s, str):
        return  _str_to_re(s)
    return "(%s)" % "|".join(_str_to_re(x) for x in s)

def _str_to_re(s):
    return ''.join("(%%(%s)s)" % x for x in s.upper()) % re_atoms
    

class C55Parser(object):
    tokens  = (
        "S", 
        "CDO",
        "CDC",
        "INCLUDES",
        "DASHMATCH",
        "LBRACE",
        "RBRACE",
        "LBRACKET",
        "RBRACKET",
        "LPAREN",
        "RPAREN",
        "PLUS",
        "MINUS",
        "GREATER",
        "COMMA",
        "STRING",
        "IDENT",
        "HASH",
        
        "IMPORT_SYM",
        "PAGE_SYM",
        "MEDIA_SYM",
        "CHARSET_SYM",
        "IMPORTANT_SYM",
    
#        "EMS",
#        "EXS",
#        "LENGTH",
#        "ANGLE",
#        "TIME",
#        "FREQ",
#        "DIMENSION",
        "PERCENTAGE",
        "NUMBER",
        "URI",
        "FUNCTION",
        "COLON",
        "SEMICOLON",
        "EQUAL",
        "DOT",
        "DOLLAR",
        "EQN",
        "CARET",
        "SLASH",
        "STAR",
        )
    
    t_ignore_COMMENT = "(%(s)s)*%(comment)s" % re_atoms

    t_S         = r"%(s)s" % re_atoms
    t_CDO       = r"<!--"
    t_CDC       = r"-->"
    t_INCLUDES  = _punc(r"~=")
    t_DASHMATCH = _punc(r"\|=")
    
    t_LBRACE    = _punc(r"\{")
    t_RBRACE    = _punc(r"\}")
    t_LBRACKET  = _punc(r"\[")
    t_RBRACKET  = _punc(r"\]")
    t_LPAREN    = _punc(r"\(")
    t_RPAREN    = _punc(r"\)")
    t_PLUS      = _punc(r"\+")
    t_MINUS     = _punc(r"\-")
    t_GREATER   = _punc(r">")
    t_COMMA     = _punc(r",")
    t_IDENT     = "%(ident)s" % re_atoms
    t_STRING    = "%(string)s" % re_atoms
    t_HASH      = r"\#([a-fA-F0-9]{6}|[a-fA-F0-9]{3}|(%(name)s))" % re_atoms
   
    t_IMPORT_SYM = "@" + _str_to_re('import') + t_S
    t_PAGE_SYM   = r"@" + _str_to_re('page') + t_S
    t_MEDIA_SYM = r"@" + _str_to_re("media") + t_S
    t_CHARSET_SYM = r"@" + _str_to_re("charset") + t_S
    
    t_IMPORTANT_SYM = (r"!((%(w)s)|(%(comment)s))*"
                       + _str_to_re("important")) % re_atoms
    
#    t_EMS           = _num_label("em")
#    t_EXS           = _num_label("ex")
#    t_LENGTH        = _num_label(['px', 'cm', 'mm', 'in', 'pt', 'pc'])
#    t_ANGLE         = _num_label(['deg', 'rad', 'grad'])
#    t_TIME          = _num_label(['s', 'ms'])
#    t_FREQ          = _num_label(['hz', 'khz'])
    t_PERCENTAGE    = "%%" % re_atoms
    t_NUMBER        = "%(num)s" % re_atoms
    
    t_URI           = ("url\((%(w)s)(%(string)s)(%(w)s)\)|"
                       "url\((%(w)s)(%(url)s)(%(w)s)\)") % re_atoms
    t_FUNCTION      = "(%(ident)s)\((%(w)s)" % re_atoms
    
    t_COLON         = _punc(":")
    t_SEMICOLON     = _punc(";")
    t_EQUAL         = _punc("=")
    t_SLASH         = _punc("/")
    t_DOT           = "\."
    t_STAR          = r"\*"
    t_DOLLAR        = r"\$"
    t_EQN           = r"\$\("
    t_CARET         = r"\^"
    

    # Error handling rule
    def t_error(self, t):
        print ("Illegal character '%s' at %s (line %s)" %
               (t.value[0], t.lexpos, t.lineno))
        t.lexer.skip(1)

    
    # ignore whitespace before the stylesheet
    def p_stylesheet_whitespace(self, p):
        """stylesheet : S stylesheet
                      | stylesheet S"""
        p[0] = p[2]

    # deal with <!-- and -->, and charset directives
    def p_stylesheet_charset(self, p):
        """stylesheet : CHARSET_SYM STRING SEMICOLON stylesheet
                      | CDO stylesheet
                      | CDC stylesheet"""
        p[0] = StyleSheet(CssThing(" ".join(p[1:len(p)-1])),
                          *p[len(p)-1])

    def p_stylesheet_htmlcomment(self, p):
        """stylesheet : stylesheet CDC
                      | stylesheet CDO"""
        p[0] = StyleSheet(*p[1])
        p[0].append(p[2])

    # @import, media, and page statements
    def p_stylesheet_import(self, p):
        """stylesheet : import stylesheet
                      | media stylesheet
                      | page stylesheet"""
        p[0] = StyleSheet(p[1], *p[2])

    def p_import(self, p):
        """import : IMPORT_SYM STRING SEMICOLON
                  | IMPORT_SYM STRING selector SEMICOLON
                  | IMPORT_SYM URI SEMICOLON
                  | IMPORT_SYM URI selector SEMICOLON"""
        p[0] = CssThingList(*[x.strip(' ') for x in p[1:]])

    def p_media(self, p):
        """media : MEDIA_SYM ruleset"""
        p[0] = CssThingList(p[1], p[2])

    def p_page(self, p):
        """page : PAGE_SYM ruleset"""
        p[0] = CssThingList(p[1], p[2])

    # most important rule: its a bunch of rulesets.  Rulesets include:
    #   selector(s) { ... }
    #   declaration: value(s)
    #   $variable: value(s)
    #   $macro(args,...) { ... }
    def p_stylesheet(self, p):
        """stylesheet : rulesets"""
        p[0] = StyleSheet(CssThing(p[1]) if not isinstance(p[1], CssThing)
                          else p[1])

    # recursive rule: "rulesets" are lists of "ruleset"s
    def p_rulesets_recurse(self, p):
        """rulesets : ruleset rulesets
                    | ruleset rulesets S"""
        p[0] = RuleSets(p[1], *p[2])

    # each rulesets needs at least one ruleset
    def p_rulesets(self, p):
        """rulesets : ruleset"""
        p[0] = RuleSets(p[1])

    def p_rulesets2(self, p):
        """rulesets : declarations"""
        p[0] = RuleSets(p[1])

    def p_rulset_declarations(self, p):
        """rulesets   : declarations rulesets
                      | declarations rulesets S"""
        p[0] = RuleSets(p[1], *p[2])

    def p_rulset_declarations2(self, p):
        """rulesets   : declarations S rulesets
                      | declarations S rulesets S"""
        p[0] = RuleSets(p[1], *p[3])

    def p_rulset_assignment(self, p):
        """rulesets   : assignment rulesets
                      | assignment rulesets S"""
        p[0] = p[2]

    def p_rulset_assignment2(self, p):
        """rulesets   : assignment S rulesets
                      | assignment S rulesets S"""
        p[0] = p[3]

    def p_rulsets_macro(self, p):
        """rulesets : macro rulesets
                    | macro rulesets S"""
        p[0] = p[2]

    # declarations are anything that we can find inside of { } for a
    # CSS rules.  This includes possible assignments as well as
    # declarations, and is recursive
    def p_decl_assignment(self, p):
        """declarations   : assignment SEMICOLON declarations
                          | assignment SEMICOLON declarations S"""
        p[0] = p[3]

    def p_decl_assignment2(self, p):
        """declarations   : assignment SEMICOLON S declarations
                          | assignment SEMICOLON S declarations S"""
        p[0] = p[4]
        
    def p_declarations_semi(self, p):
        """declarations : declaration SEMICOLON declarations
                        | declaration SEMICOLON S declarations"""
        p[0] = Declarations(p[1], *p[len(p) - 1])

    def p_declarations_declaration(self, p):
        """declarations : declaration
                        | declaration SEMICOLON
                        | declaration SEMICOLON S"""
        p[0] = Declarations(p[1])

    def p_declaration(self, p):
        """
        declaration : IDENT COLON terms
        """
        p[0] = Declaration(p[1], p[3])

    def p_macrofn_eval(self, p):
        """declaration : DOLLAR function"""
        m = self.get_var(p[2].name)
        self.new_scope()
        for k, v in zip(m.fn.args, p[2].args):
            self.set_var(str(k), v)
        p[0] = m.body.clone()
        self.old_scope()

    # variables take the form 
    #   $IDENT: terms 
    # we'll define terms later.  
    def p_assignment(self, p):
        """assignment : variable COLON terms SEMICOLON"""
        self.set_var(p[1], p[3])

    def p_variable(self, p):
        """variable : DOLLAR IDENT"""
        p[0] = MacroVariable(p[2], self)
        
    # macros look like a ruleset, except the "selector" is $func()
    def p_macro_decl(self, p):
        """macro : macrofn declarations endscope
                 | macrofn declarations endscope S"""
        self.set_var(p[1].name, Macro(p[1], p[2]))

    def p_macrofn(self, p):
        """macrofn : DOLLAR function beginscope"""
        p[0] = p[2]
        for arg in p[0].args:
            self.set_var(str(arg), MacroVariable(str(arg), self))

    # selector rulesets:  [.selector { [x: y]* } ]*  
    def p_ruleset_empty(self, p):
        """ruleset : selector beginscope endscope
                   | selector beginscope endscope S"""
        p[0] = RuleSet(p[1], Declarations())

    def p_ruleset(self, p):
        """ruleset : selector beginscope rulesets endscope
                   | selector beginscope declarations endscope"""
        p[0] = RuleSet(p[1], p[3])

    # the parser chokes on:
    #  q:before,q:after { a:b }
    # which looks like it might be a valid declaration followed by :after
    def p_selector_special(self, p):
        """ruleset : declaration selector beginscope endscope
                   | declaration selector beginscope rulesets endscope
                   | declaration selector beginscope declarations endscope"""
        selector = self._declaration_to_selector(p[1])
        if isinstance(selector, SelectorList):
            selector[-1] = ChainSelector(selector[-1], p[2])
        else:
            selector = ChainSelector(selector, p[2])
        if len(p) == 5:
            p[0] = RuleSet(selector, Declarations())
        else:
            p[0] = RuleSet(selector, p[4])
        
    # the parser chokes on:
    #  q:before { a:b }
    # which looks like it might be a declaration followed by a brace
    def p_selector_special2(self, p):
        """ruleset : declaration beginscope endscope
                   | declaration beginscope rulesets endscope
                   | declaration beginscope declarations endscope"""
        selector = self._declaration_to_selector(p[1])
        if len(p) == 4:
            p[0] = RuleSet(selector, Declarations())
        else:
            p[0] = RuleSet(selector, p[3])

    def _declaration_to_selector(self, badness):
        selector = ElementSelector(str(badness.prop))
        stuff = badness.val
        more = []
        if isinstance(stuff, Expr):
            stuff = list(stuff)[0]
            more = [ElementSelector(str(s)) for s in list(stuff)[1:]]
        selector = ChainSelector(selector, PseudoSelector(":" + str(stuff)))
        if more:
            more = [selector] + more
            selector = SelectorList(*more)
        return selector

    # right braces and left braces create a new scope
    def p_begin_scope(self, p):
        """beginscope : LBRACE
                      | LBRACE S"""
        self.new_scope()

    def p_end_scope(self, p):
        """endscope : RBRACE
                    | RBRACE S"""
        self.old_scope()
        
    # Selector constructions
    def p_selector_combo(self, p):
        """selector : selector COMMA selector"""
        if isinstance(p[3], SelectorList):
            p[0] = SelectorList(p[1], *p[3])
        else:
            p[0] = SelectorList(p[1], p[3])            

    def p_selector_sselector(self, p):
        """selector : sselector"""
        p[0] = p[1]

    def p_selector_child(self, p):
        """selector : sselector S selector"""
        if isinstance(p[3], (DirectChildSelector, NextToSelector)):
            p[0] = p[3].make_parent(p[1])
        else:
            p[0] = ChildSelector(p[1], p[3])

    def p_sselector_chain(self, p):
        """selector : sselector selector"""
        if isinstance(p[2], (DirectChildSelector, NextToSelector)):
            p[0] = p[2].make_parent(p[1])
        else:
            p[0] = ChainSelector(p[1], p[2])

    # selector "arithmetic" 
    def p_selector_emptygreater(self, p):
        """selector : GREATER selector"""
        p[0] = DirectChildSelector(EmptySelector(), p[2])

    def p_selector_emptyplus(self, p):
        """selector : PLUS selector"""
        p[0] = NextToSelector(EmptySelector(), p[2])

    def p_selector_underscore(self, p):
        """selector : CARET selector
                    | CARET S selector"""
        p[0] = ChainSelector(EmptySelector(), p[len(p) - 1])

    # a s(imple)selector can be the name of a tag or a star
    def p_sselector_elselector(self, p):
        """sselector : IDENT
                     | STAR"""
        p[0] = ElementSelector(p[1])

    # or a class name
    def p_sselector_class(self, p):
        """sselector : DOT IDENT"""
        p[0] = ClassSelector(p[2])


    # or an id
    def p_sselector_id(self, p):
        """sselector : HASH"""
        p[0] = IdSelector(''.join(p[1:]))

    # or a pseudo selector
    def p_sselector_pseudo(self, p):
        """sselector : COLON IDENT
                     | COLON FUNCTION RPAREN
                     | COLON FUNCTION IDENT RPAREN"""
        p[0] = PseudoSelector(''.join(p[1:]))

    # or an attr
    def p_sselector_attrib(self, p):
        """sselector : LBRACKET IDENT RBRACKET
                     | LBRACKET IDENT EQUAL IDENT RBRACKET
                     | LBRACKET IDENT EQUAL STRING RBRACKET
                     | LBRACKET IDENT INCLUDES IDENT RBRACKET
                     | LBRACKET IDENT INCLUDES STRING RBRACKET
                     | LBRACKET IDENT DASHMATCH IDENT RBRACKET
                     | LBRACKET IDENT DASHMATCH STRING RBRACKET"""
        p[0] = AttribSelector(''.join(p[1:]))

    # terms can be space delineated
    def p_terms_list(self, p):
        """terms : term terms
                 | term S terms"""
        p[0] = Terms(p[1].evaluate(), *p[len(p)-1])

    # or delineated by commas or slashes
    def p_cterm(self, p):
        """terms : term COMMA terms
                 | term SLASH terms"""
        p[1] = p[1].evaluate()
        if isinstance(p[3], Expr):
            p[0] = Expr(p[1], *p[3])
        else:
            p[0] = Expr(p[1], p[3])

    # or they can end in !important
    def p_terms_important(self, p):
        """terms : term S IMPORTANT_SYM"""
        p[1] = p[1].evaluate()
        p[0] = Terms(p[1], p[3])

    # or an individual term
    def p_terms(self, p):
        """terms : term"""
        p[0] = Terms(p[1].evaluate())


    def p_term_aterm(self, p):
        """term : aterm"""
        p[0] = p[1]

    def p_term_unary2(self, p):
        """term : MINUS aterm"""
        term = p[2]
        p[0] = term.__class__(-term.num, term.label)

    def p_term(self, p):
        """term : STRING
                | IDENT
                | URI
                | function"""
        p[0] = Term(p[1])

    def p_term_color(self, p):
        """term : HASH"""
        p[0] = Term(p[1])

    def p_term_var(self, p):
        """term : variable"""
        # TODO: change this for symbolic evaluation
        p[0] = p[1]

    def p_term_equation(self, p):
        """term : EQN equation RPAREN"""
        p[0] = p[2]

    def p_equation_term(self, p):
        """equation : term"""
        p[0] = p[1]
    
    def p_equation_precidence(self, p):
        """equation : LPAREN equation RPAREN"""
        p[0] = p[2]

    def p_equation_space(self, p):
        """equation : S equation"""
        p[0] = p[2]

    def p_equation_space2(self, p):
        """equation : equation S"""
        p[0] = p[1]
           
    def p_equation_plus(self, p):
        """equation : equation PLUS equation"""
        p[0] = p[1] + p[3]

    def p_equation_minus(self, p):
        """equation : equation MINUS equation"""
        p[0] = p[1] - p[3]

    def p_equation_times(self, p):
        """equation : equation STAR equation
                    | equation STAR S equation
                    | equation S STAR equation
                    | equation S STAR S equation"""
        p[0] = p[1] * p[len(p)-1]

    def p_equation_quot(self, p):
        """equation : term SLASH equation"""
        p[0] = p[1] / p[3]

    # ignore = for now (treat it as part of the term)
    def p_term_equal(self, p):
        """term : term EQUAL term"""
        p[0] = "".join(str(x).strip(' ') for x in p[1:])

    # same for commas at the end of a long list
    def p_cterm_empty(self, p):
        """term : term COMMA"""
        p[0] = Expr(p[1], "")

#    # numeric terms
#    def p_aterm_measured(self, p):
#        """aterm : NUMBER LENGTH
#                 | NUMBER EMS
#                 | NUMBER EXS
#                 | NUMBER FREQ
#                 | NUMBER TIME
#                 | NUMBER ANGLE"""
#        print "here!"
#        p[0] = ArithmeticTerm(p[1], p[2])

    def p_aterm_percent(self, p):
        """aterm : NUMBER PERCENTAGE"""
        p[0] = Percentage(p[1])
        
    def p_aterm(self, p):
        """aterm : NUMBER IDENT"""
        p[0] = ArithmeticTerm(p[1], p[2])

    def p_aterm_number(self, p):
        """aterm : NUMBER"""
        p[0] = NumberTerm(p[1])

    # functions (TODO)
    def p_function(self, p):
        """function : FUNCTION terms RPAREN"""
        p[0] = Function(p[1].strip("( \n"), p[2])


    # Error rule for syntax errors
    def p_error(self, p):
        print "Syntax error in input! %s" % str(p)

    def __init__(self, debug = True):
        self.lexer = lex.lex(module = self)
        self.parser = yacc.yacc(module = self, debug = debug)
        self.vars = [{}]

    # scoping of variables
    def new_scope(self):
        self.vars.append(self.vars[-1].copy())
        
    def old_scope(self):
        self.vars.pop()

    def get_var(self, x):
        return self.vars[-1][str(x)]

    def set_var(self, x, v):
        if hasattr(v, "__iter__") and len(v) == 1:
            v = v[0]
        if isinstance(x, MacroVariable):
            x = x.name
        self.vars[-1][x] = v

    # parsing and lexing
    def test(self, data):
        self.lexer.input(data)
        return list(self.lexer)

    def parse(self, data):
        return self.parser.parse(data)

    def process(self, data):
        """  c55 -> css """
        return str(self.parse(data).toCss())

    def unprocess(self, data):
        """  css -> c55 """
        return str(self.parse(data).toC55())

class CssThing(object):
    def __init__(self, a):
        self._s = a
    def toString(self, padding = ''):
        return padding + str(self._s)
    def __str__(self):
        return self.toString()

    def __hash__(self):
        return hash(repr(self))
    def __cmp__(self, other):
        return repr(self) == repr(other)
    def __eq__(self, other):
        return repr(self) == repr(other)

    def __repr__(self):
        return "<%s: %s>" % (self.__class__.__name__,
                             repr(str(self)))
    def clone(self):
        return self.__class__(self._s)

class CssThingList(CssThing):
    separator = " "
    def __init__(self, *a):
        self._l = list(a)
        CssThing.__init__(self, "")
    def toString(self, padding = ""):
        if self.separator == '\n':
            return self.separator.join(x.toString(padding = padding) 
                                       for x in self._l)
        else:
            return padding + self.separator.join(x.toString() 
                                             for x in self._l)
    def __iter__(self):
        return iter(self._l)
    def __repr__(self):
        return "<%s: %s>" % (self.__class__.__name__,
                             repr(self._l))
    def append(self, item):
        self._l.append(item)
    def extend(self, items):
        self._l.extend(items)
    def __getitem__(self, indx):
        return self._l[indx]
    def __setitem__(self, indx, val):
        self._l[indx] = val
        
    def get(self, item, default = None):
        return self._d.get(item.sel, default)
    def __contains__(self, item):
        return item.sel in self._d
    def __len__(self):
        return len(self._l)

    def clone(self):
        attrs = []
        for s in self._l:
            attrs.append(s.clone() if isinstance(s, CssThing) else s)
        return self.__class__(*attrs)


class CssThingWAttrs(CssThing):
    __slots__ = []
    def __init__(self, *a):
        _s = []
        for k, v in zip(self.__slots__, a):
            setattr(self, k, v)
            _s.append("%s=%s" % (k, v))
        CssThing.__init__(self, "(%s)" % ",".join(_s))

    def clone(self):
        attrs = []
        for s in self.__slots__:
            s = getattr(self, s)
            attrs.append(s.clone() if isinstance(s, CssThing) else s)
        return self.__class__(*attrs)

# --- used classes below

class Selector(CssThing): 
    def make_parent(self, parent):
        return ChildSelector(parent, self)

class EmptySelector(Selector):
    def __init__(self):
        Selector.__init__(self, "")

class ElementSelector(Selector): pass
class ClassSelector(Selector): 
    def __init__(self, x):
        Selector.__init__(self, "." + x)
        
class IdSelector(Selector): pass
class PseudoSelector(Selector): pass
class AttribSelector(Selector): pass

class ChainSelector(CssThingList):
    separator = ""
    def make_parent(self, parent):
        if isinstance(self._l[0], EmptySelector):
            return self.__class__(parent, *self._l[1:])
        return ChildSelector(parent, self)
    def toString(self, padding = ""):
        s = CssThingList.toString(self, padding)
        if isinstance(self._l[0], EmptySelector) and not self.separator:
            s = "^" + s
        return s.strip(' ')
        
class ChildSelector(ChainSelector):
    separator = " "
class DirectChildSelector(ChainSelector):
    separator = ">"
class NextToSelector(ChainSelector):
    separator = "+"
class SelectorList(ChainSelector):
    separator = ","
    def make_parent(self, parent):
        return SelectorList(*[x.make_parent(parent) for x in self])
        
class Declaration(CssThingWAttrs):
    __slots__ = ['prop', 'val']
    def toString(self, padding = ''):
        val = padding + "%s:%s;" % (str(self.prop), str(self.val))
        #parsed, report = validate_css("* { %s }" % val)
        return val
    def __iter__(self):
        yield self
        
class Declarations(CssThingList):
    separator = "\n"
        
class Term(CssThing): 
    def evaluate(self):
        return self

class Terms(CssThingList):
    separator = " "

class Expr(CssThingList):
    separator = ","
class Function(CssThingWAttrs):
    __slots__ = ["name", "args"]
    def toString(self, padding = ""):
        return padding + "%s(%s)" % (self.name, self.args.toString())

class Macro(CssThingWAttrs):
    __slots__ = ['fn', 'body']
    def toString(self, padding = ""):
        return "MACRO <%s>" % str(self.fn)

class MacroVariable(CssThing):
    def __init__(self, name, parser):
        self.name = name
        self.parser = parser
        CssThing.__init__(self, "$%s$" % name)

    def clone(self):
        return self.evaluate()

    def evaluate(self):
        return self.parser.get_var(self.name)

    def __add__(self, a):
        return make_closure("__add__", self, a)
    def __radd__(self, a):
        return make_closure("__radd__", self, a)

    def __sub__(self, a):
        return make_closure("__sub__", self, a)
    def __rsub__(self, a):
        return make_closure("__rsub__", self, a)

    def __mul__(self, a):
        return make_closure("__mul__", self, a)
    def __rmul__(self, a):
        return make_closure("__rmul__", self, a)

    def __rdiv__(self, a):
        return make_closure("__rdiv__", self, a)

def make_closure(method, x, other):
    class Closure(MacroVariable):
        def evaluate(self):
            e = x.evaluate()
            return getattr(e, method)(other.evaluate())
    return Closure("", None)

class Equation(CssThing):
    def evaluate(self):
        return "equation"

class RuleSet(CssThingWAttrs):
    __slots__ = ['sel', 'decl']
    def __init__(self, *a):
        CssThingWAttrs.__init__(self, *a)
        self.fix_selector()

    def toString(self, padding = ""):
        s = padding + "%s {\n%s\n" + padding + "}"
        return s % (self.sel.toString(), 
                    self.decl.toString(padding + "    "))

    def fix_selector(self):
        """
        to be called on parse completion: the CSS grammar for ","
        versus " " is ambiguous, insofar as "," has lower precidence
        for selectors and higher precidence for terms.  In this way,
        selectors containing ',' need to be reorganized to make the
        outer selector a Selector List
        """
        # count number of commas in the selector
        occur = self.sel.toString().count(',')
        # make sure that count matches the number of items in it or
        # that it is a selectorlist.
        if occur and (not isinstance(self.sel, SelectorList) or
                      len(self.sel) != occur):
            s = SelectorList()
            # peel apart:
            start = self.sel
            current = self.sel
            while True:
                if not isinstance(current, ChainSelector):
                    s.append(start)
                    break
                this, next = list(current)
                if isinstance(next, SelectorList):
                    current[-1] = next[0]
                    s.append(start)
                    start = current = next[1]
                else:
                    current = next
            self.sel = s

            

    def __iter__(self):
        yield self
        
    def toCss(self):
        d = Declarations()
        r = RuleSets()
        for decl in self.decl:
            if isinstance(decl, (Declaration, Declarations)):
                for d1 in decl:
                    d.append(d1)
            elif isinstance(decl, (RuleSet, RuleSets)):
                for rule in decl:
                    for rule2 in rule.toCss():
                        if isinstance(self.sel, SelectorList):
                            new_sel = SelectorList(*[rule2.sel.make_parent(x)
                                                     for x in self.sel])
                        else:
                            new_sel = rule2.sel.make_parent(self.sel)
                        r.append(RuleSet(new_sel, rule2.decl))
        
        if list(d):
            r = RuleSets(RuleSet(self.sel, d), *r)
        return r

class RuleSets(CssThingList):
    separator = "\n"
    
    def __init__(self, *a):
        CssThingList.__init__(self, *a)
        self._d = {}
        for x in a:
            if isinstance(x, RuleSet):
                self._d[x.sel] = x

    def append(self, item):
        CssThingList.append(self, item)
        if isinstance(item, RuleSet):
            self._d[item.sel] = item

    def __setitem__(self, indx, val):
        try:
            existing = self._l[indx]
            if isinstance(existing, RuleSet):
                del self._d[existing.sel]
        except IndexError:
            pass
        self._l[indx] = val
        if isinstance(val, RuleSet):
            self._d[val.sel] = val
        
    def toCss(self):
        r = RuleSets()
        for rule in self:
            for rule2 in rule.toCss():
                r.append(rule2)
        return r
        
    def toC55(self):
        def rechain(sel_list):
            while len(sel_list) > 1:
                sel_list[-2] = sel_list[-1].make_parent(sel_list[-2])
                del sel_list[-1]
            return sel_list[0]
            
        def unchain(selector, wrapper = None):
            if isinstance(selector, SelectorList):
                # this is the tricky one: we only want to disassemble
                # listed selectors if they have common parents
                subsel = map(unchain, selector)
                depth = min(map(len, subsel))
                if depth == 1:
                    # not enough commonality: (a.foo, a) at best
                    return [selector]
                else:
                    for i in range(depth):
                        # check all are equal, break if not
                        current = [x[i] for x in subsel 
                                   if str(x[i]) == str(subsel[0][i])]
                        if len(current) != len(subsel):
                            break
                    # very interesting case this one: (a.foo, a.foo) 
                    # silly user
                    else:
                        return subsel[0]
                    # at this point, i is the first index to not match
                    if i == 0:
                        return [selector]
                    else:
                        childs = SelectorList(*[rechain(x[i:]) for x in subsel])
                        return subsel[0][:i] + [childs]
            elif isinstance(selector, ChainSelector):
                this, next = list(selector)
                if wrapper: 
                    this = wrapper(EmptySelector(), this)
                return [this] + unchain(next, selector.__class__)
            return [wrapper(EmptySelector(), selector) if wrapper else selector]

        sel_tree = [{}, Declarations()]
        for rule in self:
            tree = sel_tree
            for sub in unchain(rule.sel):
                tree = tree[0]
                tree = tree.setdefault(sub, [{}, Declarations()])
            tree[1].extend(list(rule.decl))
        
        def build_rulesets(sel, tree):
            tree, decl = tree
            decl = RuleSets(*decl)
            subrules = [build_rulesets(k, v) for k, v in tree.iteritems()]
            decl.extend(subrules)
            return RuleSet(sel, decl)
        return RuleSets(*[build_rulesets(k, v) 
                          for k, v in sel_tree[0].iteritems()])

class StyleSheet(CssThingList):
    separator = "\n"

    def toCss(self):
        new = StyleSheet()
        for s in self:
            if hasattr(s, "toCss"):
                for x in s.toCss():
                    new.append(x)
            else:
                new.append(s)
        return new
        
    def toC55(self):
        new = StyleSheet()
        for s in self:
            if hasattr(s, "toC55"):
                for x in s.toC55():
                    new.append(x)
            elif hasattr(s, "toCss"):
                for x in s.toCss():
                    new.append(x)
            else:
                new.append(s)
        return new

class ArithmeticTerm(Term):
    def __init__(self, number, label):
        self.num, self.label = float(number), label
        if round(self.num) == self.num:
            self.num = int(self.num)
        
    def toString(self, padding = ""):
        return padding + ("%s%s" % (self.num, self.label))

    def __div__(self, a):
        if isinstance(a, NumberTerm):
            return self.__class__(float(self.num) / a.num, self.label)
        elif isinstance(a, Percentage):
            return self.__class__(float(self.num) / (a.num*.01), self.label)
        elif isinstance(a, (int, float, long)):
            return self.__class__(float(self.num) / a, self.label)
        raise TypeError, "cannot divide '%s' by '%s'" % (repr(self), repr(a))

    def __mul__(self, a):
        if isinstance(a, NumberTerm):
            return self.__class__(float(self.num) * a.num, self.label)
        elif isinstance(a, Percentage):
            return self.__class__(float(self.num) * (a.num*.01), self.label)
        elif isinstance(a, (int, float, long)):
            return self.__class__(float(self.num) * a, self.label)
        raise TypeError, "cannot multiply '%s' by '%s'" % (repr(self), repr(a))

    def __add__(self, a):
        if not isinstance(a, self.__class__) or self.label != a.label:
            raise TypeError, "Cannot add '%s' to '%s'" % (repr(self), repr(a))
        return self.__class__(self.num + a.num, self.label)
            
    def __sub__(self, a):
        if not isinstance(a, self.__class__) or self.label != a.label:
            raise TypeError, "Cannot subtract '%s' from '%s'" % \
                (repr(self), repr(a))
        return self.__class__(self.num - a.num, self.label)
            

class NumberTerm(ArithmeticTerm):
    def __init__(self, number):
        ArithmeticTerm.__init__(self, number, "")
class Percentage(ArithmeticTerm):
    def __init__(self, number):
        ArithmeticTerm.__init__(self, number, "%")

            
if __name__ == "__main__":
    import sys
    for file in sys.argv[1:]:
        with open(file, 'r') as handle:
            if file.endswith(".c55"):
                print C55Parser(debug = False).process(handle.read())
            else:
                print C55Parser(debug = False).unprocess(handle.read())

    
