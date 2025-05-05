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

# --------------------- SEMANTIC ANALYSIS ---------------------
# Global semantic structures
function_dir = None
semantic_cube = None

def init_semantic_analysis():
    global function_dir, semantic_cube
    function_dir = {
        'functions': {
            'global': {
                'return_type': 'void',
                'vars': {},
                'params': []
            }
        },
        'current_scope': 'global'
    }
    
    semantic_cube = {
        # int operations
        ('int', '+', 'int'): 'int',
        ('int', '-', 'int'): 'int',
        ('int', '*', 'int'): 'int',
        ('int', '/', 'int'): 'float',
        ('int', '>', 'int'): 'bool',
        ('int', '<', 'int'): 'bool',
        ('int', '!=', 'int'): 'bool',
        ('int', '=', 'int'): 'int',
        
        # float operations
        ('float', '+', 'float'): 'float',
        ('float', '-', 'float'): 'float',
        ('float', '*', 'float'): 'float',
        ('float', '/', 'float'): 'float',
        ('float', '>', 'float'): 'bool',
        ('float', '<', 'float'): 'bool',
        ('float', '!=', 'float'): 'bool',
        ('float', '=', 'float'): 'float',
        
        # mixed operations
        ('int', '+', 'float'): 'float',
        ('float', '+', 'int'): 'float',
        ('int', '*', 'float'): 'float',
        ('float', '*', 'int'): 'float',
        ('int', '=', 'float'): 'float',
        ('float', '=', 'int'): 'int'
    }

def check_types(left_type, op, right_type):
    return semantic_cube.get((left_type, op, right_type), 'error')

def add_function(name, return_type):
    if name in function_dir['functions']:
        raise Exception(f"Función '{name}' ya declarada")
    function_dir['functions'][name] = {
        'return_type': return_type,
        'vars': {},
        'params': []
    }

def enter_scope(name):
    function_dir['current_scope'] = name

def exit_scope():
    function_dir['current_scope'] = 'global'

def add_variable(name, var_type):
    scope = function_dir['current_scope']
    if name in function_dir['functions'][scope]['vars']:
        raise Exception(f"Variable '{name}' ya declarada en el ámbito {scope}")
    function_dir['functions'][scope]['vars'][name] = {
        'type': var_type,
        'scope': scope
    }

def get_variable(name):
    # Buscar en el ámbito actual primero
    current_scope = function_dir['current_scope']
    if name in function_dir['functions'][current_scope]['vars']:
        return function_dir['functions'][current_scope]['vars'][name]
    # Buscar en ámbito global
    if name in function_dir['functions']['global']['vars']:
        return function_dir['functions']['global']['vars'][name]
    return None

def add_parameter(func_name, param_name, param_type):
    function_dir['functions'][func_name]['params'].append({
        'name': param_name,
        'type': param_type
    })
    add_variable(param_name, param_type)

# --------------------- PARSER ---------------------
def p_program(p):
    '''program : PROGRAM ID SEMICOLON prog_vars prog_funcs main_declaration END'''
    # Verificar que exista la función main
    if 'main' not in function_dir['functions']:
        raise Exception("Programa debe tener una función main")
    p[0] = ('program', p[2], p[4], p[5], p[7])

def p_main_declaration(p):
    'main_declaration : MAIN body'
    # Registrar la función main cuando aparece en el código
    if 'main' in function_dir['functions']:
        raise Exception("Error semántico: 'main' ya está declarado")
    
    function_dir['functions']['main'] = {
        'return_type': 'void',
        'vars': {},
        'params': []
    }
    function_dir['current_scope'] = 'main'
    p[0] = ('main_declaration', p[2])
    function_dir['current_scope'] = 'global'

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
    var_name = p[1]
    var_info = get_variable(var_name)
    if not var_info:
        raise Exception(f"Variable '{var_name}' no declarada")
    
    expr_type = p[3]['type']
    
    # Permitir asignación int -> float
    if var_info['type'] == 'float' and expr_type == 'int':
        p[0] = ('assign', var_name, p[3])
    else:
        result_type, _ = check_types(var_info['type'], '=', expr_type)
        if result_type == 'error':
            raise Exception(f"No se puede asignar {expr_type} a variable de tipo {var_info['type']}")
    p[0] = ('assign', p[1], p[3])

def p_expression(p):
    'expression : exp expression_prime'
    if p[2][0] == 'empty':  # No hay operación relacional
        p[0] = p[1]
    else:
        left_type = p[1]['type']
        right_type = p[2][2]['type']
        op = p[2][1]
        
        # Manejar conversión implícita int -> float
        if left_type == 'int' and right_type == 'float':
            left_type = 'float'
        elif left_type == 'float' and right_type == 'int':
            right_type = 'float'
        
        result_type, _ = check_types(left_type, op, right_type)
        if result_type == 'error':
            raise Exception(f"Tipos incompatibles: {left_type} {op} {right_type}")
        p[0] = {'type': 'bool', 'value': None}

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
    p[0] = (p.slice[1].type, p[1])  # Devuelve ('CTE_INT', valor) o ('CTE_FLOAT', valor)

def p_funcs(p):
    'funcs : VOID ID LPAREN funcs_prime RPAREN LBRACKET funcs_vars body RBRACKET SEMICOLON'
    # Registrar función
    add_function(p[2], 'void')
    enter_scope(p[2])
    
    # Procesar parámetros
    if p[4][0] != 'empty':
        process_parameters(p[2], p[4])
    
    p[0] = ('funcs', p[2], p[4], p[7], p[8])
    exit_scope()

def process_parameters(func_name, params_node):
    """Process function parameters"""
    # params_node: ('funcs_prime', id, type, more_params)
    param_name = params_node[1]
    param_type = params_node[2][1]  # ('type', actual_type)
    add_parameter(func_name, param_name, param_type)
    
    # Procesar más parámetros si existen
    if params_node[3][0] != 'empty':
        process_parameters(func_name, params_node[3])

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
    if p[2][0] == 'empty':  # No hay operación
        p[0] = p[1]
    else:
        left_type = p[1]['type']
        right_type = p[2][2]['type']
        op = p[2][1]
        
        # Conversión implícita int -> float
        if left_type == 'int' and right_type == 'float':
            left_type = 'float'
        elif left_type == 'float' and right_type == 'int':
            right_type = 'float'
        
        result_type, _ = check_types(left_type, op, right_type)
        if result_type == 'error':
            raise Exception(f"Tipos incompatibles en operación: {left_type} {op} {right_type}")
        p[0] = {'type': result_type, 'value': None}

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
    if isinstance(p[1], tuple):
        if p[1][0] == 'ID':  # Es un identificador
            var_name = p[1][1]
            var_info = get_variable(var_name)
            if not var_info:
                # Verificar si es parámetro de función
                current_func = function_dir['current_scope']
                if current_func != 'global':
                    params = function_dir['functions'][current_func]['params']
                    param_info = next((p for p in params if p['name'] == var_name), None)
                    if param_info:
                        p[0] = {'type': param_info['type'], 'value': var_name}
                        return
            
            if not var_info:
                raise Exception(f"Variable '{var_name}' no declarada")
            p[0] = {'type': var_info['type'], 'value': var_name}
        elif p[1][0] in ('CTE_INT', 'CTE_FLOAT'):
            const_type = 'int' if p[1][0] == 'CTE_INT' else 'float'
            p[0] = {'type': const_type, 'value': p[1][1]}
        else:
            raise Exception(f"Tipo de factor inválido: {p[1][0]}")
    else:
        raise Exception(f"Estructura inválida en factor: {p[1]}")

def p_vars(p):
    'vars : VAR vars_prime'
    p[0] = ('vars', p[2])

def p_vars_prime(p):
    '''vars_prime : ID id COLON type SEMICOLON vars_prime
                  | empty'''
    if len(p) == 7:  # Caso recursivo
        # Añadir variables a la tabla
        current_type = p[4][1]  # ('type', 'int'/'float')
        variable_names = [p[1]] + get_id_list(p[2])
        add_variables(variable_names, current_type)
        p[0] = ('vars_prime', p[1], p[2], p[4], p[6])
    else:  # Caso base (empty)
        p[0] = p[1]

def add_variables(names, var_type):
    """Añade múltiples variables a la tabla de símbolos"""
    for name in names:
        add_variable(name, var_type)

def get_id_list(id_node):
    """Extrae recursivamente la lista de IDs de la regla id"""
    if id_node[0] == 'empty':
        return []
    return [id_node[1]] + get_id_list(id_node[2])

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
    # Verificar que la expresión sea booleana
    if p[3]['type'] != 'bool':
        raise Exception("La condición del while debe ser booleana")
    p[0] = ('while_loop', p[3], p[6])  # (condición, cuerpo)

def p_condition(p):
    'condition : IF LPAREN expression RPAREN body else_condition SEMICOLON'
    # Verificar que la expresión sea booleana
    if p[3]['type'] != 'bool':
        raise Exception("La condición del if debe ser booleana")
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
     # Verificar que la función exista
    if p[1] not in function_dir['functions']:
        raise Exception(f"Función '{p[1]}' no declarada")
    
    # Verificar parámetros
    check_parameters(p[1], p[3])

    p[0] = ('f_call', p[1], p[3])

def check_parameters(func_name, params_node):
    """Check function call parameters match declaration"""
    expected_params = function_dir['functions'][func_name]['params']
    actual_params = get_param_list(params_node)
    
    if len(expected_params) != len(actual_params):
        raise Exception(f"Número incorrecto de parámetros para {func_name}")
    
    for expected, actual in zip(expected_params, actual_params):
        result_type = check_types(expected['type'], '=', actual['type'])
        if result_type == 'error':
            raise Exception(f"Tipo incorrecto para parámetro {expected['name']}")

def get_param_list(params_node):
    """Extract list of parameter types from f_call_prime node"""
    if params_node[0] == 'empty':
        return []
    
    first_param = params_node[1]['type']
    if params_node[2][0] == 'empty':
        return [first_param]
    
    return [first_param] + get_param_list(params_node[2])

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

class SemanticError(Exception):
    pass

def p_error(p):
    if p:
        error_msg = f"Error de sintaxis en línea {p.lineno}: Token inesperado '{p.value}'"
        print(error_msg)
    else:
        print("Error de sintaxis: final de archivo inesperado")

parser = yacc.yacc()

init_semantic_analysis()



# Declaración mínima de programa
data = '''
program ejemplo;
var 
    x, y : int;
    z : float;
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
    
lexer.input(data)
parser.parse(data, lexer=lexer)
print("Análisis semántico completado")