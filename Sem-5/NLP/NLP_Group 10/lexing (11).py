import ply.lex as lex
import ply.yacc as yacc
 
# List of token names.   This is always required
tokens = [
 'INCLUDE',
 'DEFINE',
 'STDIOLIB',
 'STRINGLIB',
 'STDLIB',
 'MATHLIB',
 'LSQPAREN',
 'RSQPAREN',
 'LEFTBRACE',
 'RIGHTBRACE',
 'LPAREN',
 'RPAREN',
 'DOT',
 'ARROW',
 'POSTFIXPLUS',
 'POSTFIXMINUS',
 'ADDRESS',
 'PLUS',
 'MINUS',
 'MULT',
 'DIVIDE',
 'MODULO',
 'NOT',
 'LEFTSHIFT',
 'RIGHTSHIFT',
 'GREATERTHAN',
 'LESSTHAN',
 'GREATERTHANEQUAL',
 'LESSTHANEQUAL',
 'EQUAL',
 'BINARYOR',
 'BINARYXOR',
 'COMPLEMENT',
 'AND',
 'OR',
 'ADDASSIGN',
 'SUBRACTASSIGN',
 'DIVIDEASSIGN',
 'MODULOASSIGN',
 'MULTASSIGN',
 'LEFTSHIFTASSIGN',
 'RIGHTSHIFTASSIGN',
 'BITWISEANDASSIGN',
 'BITWISEXORASSIGN',
 'BITWISEORASSIGN',
 'COMMA',
 'SEMICOLON',
 'ASSIGN',
 'CONDITIONALOP',
 'SIGNIFY',
 'ID',
 'DIGIT',
 'STRING'
]
 
reserved={
   'auto': 'AUTO',
   'register': 'RESTRICT',
   'int' : 'INT',
   'float' : 'FLOAT',
   'void' : 'VOID',
   'short' : 'SHORT',
   'long' : 'LONG',
   'double' : 'DOUBLE',
   'signed' : 'SIGNED',
   'unsigned' : 'UNSIGNED',
   'double' : 'DOUBLE',
   'continue' : 'CONTINUE',
   'char' : 'CHAR',
   'goto' : 'GOTO',
   'do' : 'DO',
   'extern' : 'EXTERN',
   'static' : 'STATIC',
   'auto' : 'AUTO',
   'register' : 'REGISTER',
   'break': 'BREAK',
   'return' : 'RETURN',
   'case':'CASE',
   'short':'SHORT',
   'volatile': 'VOLATILE',
   'restrict':'RESTRICT',
   'for' : 'FOR',
   'while' : 'WHILE',
   'const' : 'CONST',
   'sizeof' : 'SIZEOF',
   'default': 'DEFAULT',
   'inline':'INLINE',
   'while' : 'WHILE',
   'else' : 'ELSE',
   'if' : 'IF',
   'for' : 'FOR',
   'switch':'SWITCH',
   'case':'CASE',
   'do' : 'DO',
   'break': 'BREAK',
   'return' : 'RETURN',
   'printf':'PRINTF',
   'scanf' : 'SCANF',
   'goto' : 'GOTO',
   'do' : 'DO',
   'main' : 'MAIN'
}
tokens += reserved.values()
# Regular expression rules for simple tokens
 
t_INCLUDE = r'\#include'
t_DEFINE = r'\#define'
t_STDIOLIB = r'<stdlib.h>'
t_STRINGLIB = r'<string.h>'
t_STDLIB = r'<stdio.h>'
t_MATHLIB = r'\<math.h>'
t_LSQPAREN = r'\['
t_RSQPAREN = r'\]'
t_LEFTBRACE = r'\('
t_RIGHTBRACE = r'\)'
t_LPAREN = r'{'
t_RPAREN = r'}'
t_DOT = r'\.'
t_ARROW = r'->'
t_POSTFIXPLUS = r'\++'
t_POSTFIXMINUS = r'\--'
t_ADDRESS = r'&'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULT = r'\*'
t_DIVIDE = r'/'
t_MODULO = r'%'
t_NOT = r'!'
t_LEFTSHIFT = r'<<'
t_RIGHTSHIFT = r'>>'
t_GREATERTHAN = r'>'
t_LESSTHAN = r'<'
t_GREATERTHANEQUAL = r'>='
t_LESSTHANEQUAL = r'<='
t_EQUAL = r'=='
t_BINARYOR = r'\|'
t_BINARYXOR = r'\^'
t_COMPLEMENT = r'~'
t_AND = r'&&'
t_OR = r'\|\|'
t_ADDASSIGN = r'\+='
t_SUBRACTASSIGN = r'\-='
t_DIVIDEASSIGN = r'/='
t_MODULOASSIGN = r'%='
t_MULTASSIGN = r'\*='
t_LEFTSHIFTASSIGN = r'<<='
t_RIGHTSHIFTASSIGN = r'>>='
t_BITWISEANDASSIGN = r'&='
t_BITWISEXORASSIGN = r'\|='
t_BITWISEORASSIGN = r'^='
t_COMMA = r','
t_SEMICOLON = r';'
t_ASSIGN = r'='
t_CONDITIONALOP = r'\?'
t_STRING = r'\".*\n*\"'
t_SIGNIFY = r'\:'

#reserved words
t_AUTO = r'auto'
t_REGISTER = r'register'
t_INT = r'int'
t_FLOAT = r'float'
t_VOID = r'void'
t_SHORT = r'short'
t_LONG = r'long'
t_DOUBLE = r'double'
t_SIGNED = r'signed'
t_UNSIGNED = r'unsigned'
t_CONTINUE = r'continue'
t_CHAR = r'char'
t_GOTO = r'goto'
t_DO = r'do'
t_EXTERN = r'extern'
t_STATIC = r'static'
t_BREAK = r'break'
t_RETURN = r'return'
t_CASE = r'case'
t_VOLATILE = r'volatile'
t_FOR = r'for'
t_WHILE = r'while'
t_CONST = r'const'
t_SIZEOF = r'sizeof'
t_MAIN = r'main'
t_DEFAULT = r'default'
t_INLINE = r'inline'
t_ELSE = r'else'
t_IF = r'if'
t_SWITCH = r'switch'
t_PRINTF = r'printf'
t_SCANF = r'scanf'
t_RESTRICT = r'restrict'
# A regular expression rule with some action code
digit = r'([0-9])'
nondigit = r'([_A-Za-z])'
identifier = r'(' + nondigit + r'(' + digit + r'|' + nondigit + r')*)'
def t_ID(t):
   r'[a-zA-Z_][a-zA-Z_0-9]*'
   t.type = reserved.get(t.value,'ID')    # Check for reserved words
   return t
 
def t_DIGIT(t):
 r'[-+]? (?: (?: \d* \. \d+ ) | (?: \d+ \.? ) )(?: [Ee] [+-]? \d+ ) ?'
 try:
   if(type(t.value)==float):
     t.value = float(t.value)
   else:
     t.value = int(t.value)
 except ValueError:
   print("Line %d: Number %s is too large!" ,(t.lineno,t.value))
   t.value = 0
 return t
 
#Define a rule so we can track line numbers
 
def t_newline(t):
 r'\n+'
 t.lexer.lineno += len(t.value)
 
# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'
 
# Error handling rule
def t_error(t):
 print ("Illegal character '%s'" , t.value[0])
 t.lexer.skip(1)
 
t_ignore_COMMENT = r'//.*'



# No return value. Token discarded
# Build the lexer
lexer = lex.lex(optimize=1)