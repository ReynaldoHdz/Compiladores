import ply.lex as lex
import ply.yacc as yacc

# --------------------- LEXER ---------------------
# Palabras reservadas
reserved = {
    'program': 'PROGRAM',
    'main': 'MAIN',
    'var': 'VAR',
    'int': 'INT',
    'float': 'FLOAT',
    'void': 'VOID',
    'if': 'IF',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'print': 'PRINT',
    'end': 'END'
}

# Lista de nombres de tokens (obligatoria)
tokens = [
    'ID',
    'CTE_INT',
    'CTE_FLOAT',
    'CTE_STRING',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'EQUALS',
    'NOT_EQUALS',
    'GREATER',
    'LESS',
    'SEMICOLON',
    'COLON',
    'COMMA',
    'LPAREN',
    'RPAREN',
    'LBRACE',
    'RBRACE',
    'LBRACKET',
    'RBRACKET'
] + list(reserved.values())

# Reglas de expresiones regulares para tokens simples
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUALS = r'='
t_NOT_EQUALS = r'!='
t_GREATER = r'>'
t_LESS = r'<'
t_SEMICOLON = r';'
t_COLON = r':'
t_COMMA = r','
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'

# Expresiones regulares con acciones
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID') # Checar si es palabra reservada
    return t

def t_CTE_FLOAT(t):
    r'[0-9]+\.[0-9]+'
    t.value = float(t.value)
    return t

def t_CTE_INT(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_CTE_STRING(t):
    r'\"[^"\n]*\"'
    t.value = t.value[1:-1] # Eliminar comillas
    return t

# Ignorar espacios y tabs
t_ignore = ' \t'

# Manejo de saltos de línea
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Manejo de errores
def t_error(t):
    print(f"Carácter ilegal '{t.value[0]}'")
    t.lexer.skip(1)

# Construir el lexer
lexer = lex.lex()

# --------------------- PARSER ---------------------
def p_program(p):
    '''program : PROGRAM ID SEMICOLON prog_vars prog_funcs MAIN body END'''
    p[0] = ('program', p[2], p[4], p[5], p[7])

def p_prog_vars(p):
    '''prog_vars : vars
                 | empty'''
    p[0] = p[1]

def p_prog_funcs(p):
    '''prog_funcs : funcs prog_funcs
                  | empty'''
    if len(p) == 3:
        p[0] = ('prog_funcs', p[1], p[2])
    else:
        p[0] = p[1]

def p_body(p):
    'body : LBRACE body_prime RBRACE'
    p[0] = ('body', p[2])

def p_body_prime(p):
    '''body_prime : statement body_prime
                  | empty'''
    if len(p) == 3:
        p[0] = ('body_prime', p[1], p[2])
    else:
        p[0] = p[1]

def p_assign(p):
    'assign : ID EQUALS expression SEMICOLON'
    p[0] = ('assign', p[1], p[3])

def p_expression(p):
    'expression : exp expression_prime'
    p[0] = ('expression', p[1], p[2])

def p_expression_prime(p):
    '''expression_prime : GREATER exp
                       | LESS exp
                       | NOT_EQUALS exp
                       | empty'''
    if len(p) == 3:  # Casos con operador
        p[0] = ('relop', p[1], p[2])
    else:  # Caso empty
        p[0] = p[1]

def p_cte(p):
    '''cte : CTE_INT
            | CTE_FLOAT'''
    p[0] = ('cte', p[1])

def p_funcs(p):
    'funcs : VOID ID LPAREN funcs_prime RPAREN LBRACKET funcs_vars body RBRACKET SEMICOLON'
    p[0] = ('funcs', p[2], p[4], p[7], p[8])

def p_funcs_prime(p):
    '''funcs_prime : ID COLON type more_funcs
                   | empty'''
    if len(p) == 5:
        p[0] = ('funcs_prime', p[1], p[3], p[4])
    else:
        p[0] = p[1]

def p_more_funcs(p):
    '''more_funcs : COMMA ID COLON type more_funcs
                  | empty'''
    if len(p) == 6:
        p[0] = ('more_funcs', p[2], p[4], p[5])
    else:
        p[0] = p[1]

def p_funcs_vars(p):
    '''funcs_vars : vars
                  | empty'''
    p[0] = p[1]

def p_statement(p):
    '''statement : assign
                 | condition
                 | cycle
                 | f_call
                 | print'''
    p[0] = ('statement', p[1])

def p_exp(p):
    'exp : term exp_prime'
    p[0] = ('exp', p[1], p[2])

def p_exp_prime(p):
    '''exp_prime : PLUS term exp_prime
                 | MINUS term exp_prime
                 | empty'''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = p[1]

def p_term(p):
    'term : factor term_prime'
    p[0] = ('term', p[1], p[2])

def p_term_prime(p):
    '''term_prime : TIMES factor term_prime
                  | DIVIDE factor term_prime
                  | empty'''
    if len(p) == 4:
        p[0] = (p[1], p[2], p[3])
    else:
        p[0] = p[1]

def p_factor(p):
    '''factor : LPAREN expression RPAREN
              | PLUS factor_prime
              | MINUS factor_prime
              | factor_prime'''
    if len(p) == 4:
        p[0] = ('factor_group', p[2])
    elif len(p) == 3:
        p[0] = ('factor_signed', p[1], p[2])
    else:
        p[0] = ('factor', p[1])

def p_factor_prime(p):
    '''factor_prime : ID
                    | cte'''
    p[0] = ('factor_prime', p[1])

def p_vars(p):
    'vars : VAR vars_prime'
    p[0] = ('vars', p[2])

def p_vars_prime(p):
    '''vars_prime : ID id COLON type SEMICOLON vars_prime
                  | empty'''
    if len(p) == 7:  # Caso recursivo
        p[0] = ('vars_prime', p[1], p[2], p[4], p[6])
    else:  # Caso base (empty)
        p[0] = p[1]

def p_id(p):
    '''id : COMMA ID id
           | empty'''
    if len(p) == 4:
        p[0] = ('id_list', p[2], p[3])
    else:
        p[0] = p[1]

def p_type(p):
    '''type : INT
            | FLOAT'''
    p[0] = ('type', p[1])

def p_print(p):
    'print : PRINT LPAREN print_prime RPAREN SEMICOLON'
    p[0] = ('print', p[3])

def p_print_prime(p):
    '''print_prime : expression more_print
                   | CTE_STRING more_print'''
    p[0] = ('print_prime', p[1], p[2])

def p_more_print(p):
    '''more_print : COMMA more_print_prime more_print
                  | empty'''
    if len(p) == 4:
        p[0] = ('more_print', p[2], p[3])
    else:
        p[0] = p[1]

def p_more_print_prime(p):
    '''more_print_prime : expression
                        | CTE_STRING'''
    p[0] = ('more_print_prime', p[1])

def p_cycle(p):
    'cycle : WHILE LPAREN expression RPAREN DO body SEMICOLON'
    p[0] = ('while_loop', p[3], p[6])  # (condición, cuerpo)

def p_condition(p):
    'condition : IF LPAREN expression RPAREN body else_condition SEMICOLON'
    p[0] = ('condition', p[3], p[5], p[6])

def p_else_condition(p):
    '''else_condition : ELSE body
                      | empty'''
    if len(p) == 3:
        p[0] = ('else', p[2])
    else:
        p[0] = p[1]

def p_f_call(p):
    'f_call : ID LPAREN f_call_prime RPAREN SEMICOLON'
    p[0] = ('f_call', p[1], p[3])

def p_f_call_prime(p):
    '''f_call_prime : expression more_f_call
                    | empty'''
    if len(p) == 3:
        p[0] = ('f_call_prime', p[1], p[2])
    else:
        p[0] = p[1]

def p_more_f_call(p):
    '''more_f_call : COMMA expression more_f_call
                   | empty'''
    if len(p) == 4:
        p[0] = ('more_f_call', p[2], p[3])
    else:
        p[0] = p[1]

def p_empty(p):
    'empty :'
    p[0] = ('empty',)

def p_error(p):
    if p:
        print(f"Error de sintaxis antes de '{p.value}' (tipo {p.type}, línea {p.lineno})")
        # Muestra los últimos tokens procesados
        print("Contexto:", parser.symstack[-5:])
    else:
        print("Error de sintaxis al final del input")

parser = yacc.yacc()

# -------------PRUEBAS------------------

# Declaración mínima de programa
test1 = '''
program minimo;
main {
}
end
'''

# Declaración de múltiples variables globales
test2 = '''
program multi_vars;
var 
    x, y, z : int;
    u, v : float;
main {
}
end
'''

# Función simple sin parámetros
test3 = '''
program func_simple;
void saludar() [ { print("Hola"); } ];
main {
    saludar();
}
end
'''

# Función con parámetros y variables locales
test4 = '''
program func_compleja;
void calcular(a : int, b : float) [ 
    var resultado : float; 
    { resultado = a * b; } 
];
main {
    calcular(5, 3.2);
}
end
'''

# Función vacía
test5 = '''
program func_vacia;
void vacia() [ { } ];
main {
    vacia();
}
end
'''

# If-else anidado
test6 = '''
program ifs;
var x : int;
main {
    x = 10;
    if (x > 5) {
        if (x < 15) {
            print("Entre 5 y 15");
        } else {
            print("Mayor a 15");
        };
    };
}
end
'''

# While con condición
test7 = '''
program whiles;
var x, y : int;
main {
    x = 10;
    y = 0;
    while (y != 5) do {
        x = x - 1;
        y = y + 1;
    };
}
end
'''

# Expresiones aritméticas complejas
test8 = '''
program expresiones;
var 
    a, b : int;
    c : float;
main {
    a = 10 * (5 + 3) - 4 / 2;
    b = (a / 3) + 7;
    c = (a + b) * 3.14;
}
end
'''

# Múltiples prints
test9 = '''
program outputs;
var edad : int;
main {
    edad = 25;
    print("Juan", " Edad: ", edad);
}
end
'''

# Anidamiento
test10 = '''
program anidado;
main {
    if (true) {
        while (false) do {
            if (1) {
                print("Anidado");
            };
        };
    };
}
end
'''

# Ejemplo de programa sencillo
test11 = '''
program ejemplo;
var 
    x, y : int;
    z : float;
void funcion(a : int, b : float) [ var z : float; { z = a + b; } ];
main {
    x = 10;
    if (x > 5) {
        print("El valor es: ", x);
    } else {
        y = 20;
    };
    while (x != 0) do {
        x = x - 1;
    };
    print(z);
}
end
'''

# Ejecutar pruebas
tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11]

for i, test in enumerate(tests, 1):
    print(f"Ejecutando prueba {i}:")
    lexer.input(test)
    while True:
        tok = lexer.token()
        if not tok:
            break
        print(tok)
    result = parser.parse(test)
    print("Resultado del análisis:", result)
    print("-" * 40)