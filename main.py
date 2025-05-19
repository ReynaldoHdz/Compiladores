import ply.lex as lex
import ply.yacc as yacc

class SymbolTable:
    def __init__(self):
        self.scopes = [{}]  # Stack of scopes (global scope by default)
        self.functions = {}  # Function directory
        self.current_scope = 0  # Points to current scope in stack

    def enter_scope(self):
        """Create a new scope level"""
        self.scopes.append({})
        self.current_scope += 1

    def exit_scope(self):
        """Leave current scope"""
        if self.current_scope > 0:  # Don't pop global scope
            self.scopes.pop()
            self.current_scope -= 1

    def add_variable(self, name, var_type, size=1, address=None):
        """Add variable to current scope with memory address"""
        if name in self.scopes[self.current_scope]:
            raise ValueError(f"Variable '{name}' already declared in this scope")
        self.scopes[self.current_scope][name] = {
            'type': var_type,
            'size': size,
            'address': address
        }

    def lookup_variable(self, name):
        """Find variable in current or enclosing scopes"""
        for scope in reversed(self.scopes[:self.current_scope + 1]):
            if name in scope:
                return scope[name]
        return None

    def add_function(self, name, return_type, parameters=None):
        """Add function to function directory"""
        if name in self.functions:
            raise ValueError(f"Function '{name}' already declared")
        self.functions[name] = {
            'return_type': return_type,
            'parameters': parameters or [],
            'variables': {},  # Will store function's local variables
            'address': None   # Will be filled during code generation
        }
        return self.functions[name]

    def get_function(self, name):
        """Retrieve function details"""
        return self.functions.get(name)

    def current_scope_variables(self):
        """Get variables in current scope"""
        return self.scopes[self.current_scope]
    

class MemoryManager:
    def __init__(self):
        # Memory block definitions
        self.blocks = {
            'global_int': {'start': 1, 'end': 20, 'current': 1},
            'global_float': {'start': 21, 'end': 40, 'current': 21},
            'global_bool': {'start': 41, 'end': 60, 'current': 41},
            'global_string': {'start': 61, 'end': 80, 'current': 61},
            
            'local_int': {'start': 81, 'end': 100, 'current': 81},
            'local_float': {'start': 101, 'end': 120, 'current': 101},
            'local_bool': {'start': 121, 'end': 140, 'current': 121},
            'local_string': {'start': 141, 'end': 160, 'current': 141},
            
            'temp_int': {'start': 161, 'end': 180, 'current': 161},
            'temp_float': {'start': 181, 'end': 200, 'current': 181},
            'temp_bool': {'start': 201, 'end': 220, 'current': 201},
            'temp_string': {'start': 221, 'end': 240, 'current': 221},
            
            'const_int': {'start': 241, 'end': 260, 'current': 241},
            'const_float': {'start': 261, 'end': 280, 'current': 261},
            'const_bool': {'start': 281, 'end': 300, 'current': 281},
            'const_string': {'start': 301, 'end': 320, 'current': 301}
        }
        
        # Operation codes
        self.operations = {
            '+': 1,
            '-': 2,
            '*': 3,
            '/': 4,
            '<': 5,
            '>': 6,
            '=': 7,
            '!=': 8,
            'int_to_float': 9
        }
        
        # Constants storage
        self.constants = {
            'int': {},
            'float': {},
            'bool': {},
            'string': {}
        }
    
    def get_address(self, var_type, scope='global', is_temp=False, is_const=False):
        """Allocate memory address based on variable type and scope"""
        if is_const:
            block_type = f'const_{var_type}'
        elif is_temp:
            block_type = f'temp_{var_type}'
        else:
            block_type = f'{scope}_{var_type}'
            
        block = self.blocks[block_type]
        if block['current'] > block['end']:
            raise MemoryError(f"Out of memory in {block_type} block")
            
        address = block['current']
        block['current'] += 1
        return address
    
    def get_operation_code(self, op):
        """Get numeric operation code"""
        return self.operations.get(op, 0)
    
    def add_constant(self, const_type, value):
        """Add constant to memory and return its address"""
        if value not in self.constants[const_type]:
            address = self.get_address(const_type, is_const=True)
            self.constants[const_type][value] = address
        return self.constants[const_type][value]
    


class Compiler:
    # --------------------- Lexer Definitions ---------------------
    # Reserved words (class variable)
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

    # Tokens list (class variable)
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

    # Simple token regex rules (methods)
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

    # Ignored characters
    t_ignore = ' \t'

    def __init__(self):
        # Initialize compiler state
        self.symbol_table = SymbolTable()
        self.quadruples = []
        self.operand_stack = []
        self.operator_stack = []
        self.type_stack = []
        self.temp_counter = 0
        self.jump_stack = []
        self.memory = MemoryManager()
        
        # Build the lexer and parser
        self.lexer = lex.lex(module=self)
        self.parser = yacc.yacc(module=self)
    
    def reset(self):
        """Reset the compiler state for new compilation"""
        self.quadruples = []
        self.operand_stack = []
        self.operator_stack = []
        self.type_stack = []
        self.temp_counter = 0
        self.jump_stack = []
        self.memory = MemoryManager()
        self.symbol_table = SymbolTable()
        self.symbol_table.enter_scope()  # Global scope
    
    def generate_temp(self, var_type):
        """Generate a new temporary variable with memory address"""
        temp_name = f"t{self.temp_counter}"
        self.temp_counter += 1
        
        # Allocate memory for the temporary
        address = self.memory.get_address(var_type, is_temp=True)
        
        # Add to symbol table
        self.symbol_table.add_variable(temp_name, var_type, address=address)
        return temp_name
    
    def emit_quad(self, op, arg1, arg2, result):
        """Generate quadruple with memory addresses"""
        # Get operation code
        op_code = self.memory.get_operation_code(op)
        
        # Get memory addresses for operands
        def get_address(operand):
            if operand is None:
                return 0
            if isinstance(operand, (int, float, str, bool)):
                # Handle constants
                const_type = 'int' if isinstance(operand, int) else \
                           'float' if isinstance(operand, float) else \
                           'bool' if isinstance(operand, bool) else 'string'
                return self.memory.add_constant(const_type, operand)
            else:
                # Lookup variable address
                var_info = self.symbol_table.lookup_variable(operand)
                return var_info['address'] if var_info else 0
        
        arg1_addr = get_address(arg1)
        arg2_addr = get_address(arg2) if arg2 is not None else 0
        result_addr = get_address(result)
        
        quad = (op_code, arg1_addr, arg2_addr, result_addr)
        self.quadruples.append(quad)
        return quad

    # --------------------- Lexer Methods ---------------------
    def t_ID(self, t):
        r'[a-zA-Z_][a-zA-Z0-9_]*'
        t.type = self.reserved.get(t.value, 'ID')  # Check for reserved words
        return t

    def t_CTE_FLOAT(self, t):
        r'[0-9]+\.[0-9]+'
        t.value = float(t.value)
        return t

    def t_CTE_INT(self, t):
        r'[0-9]+'
        t.value = int(t.value)
        return t

    def t_CTE_STRING(self, t):
        r'\"[^"\n]*\"'
        t.value = t.value[1:-1]  # Remove quotes
        return t

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    def t_error(self, t):
        print(f"Illegal character '{t.value[0]}'")
        t.lexer.skip(1)

    # --------------------- Parser Methods ---------------------
    def p_program(self, p):
        '''program : PROGRAM ID SEMICOLON prog_vars prog_funcs MAIN body END'''
        self.symbol_table.enter_scope()  # Global scope
        p[0] = ('program', p[2], p[4], p[5], p[7])

    def p_prog_vars(self, p):
        '''prog_vars : vars
                    | empty'''
        p[0] = p[1]

    def p_prog_funcs(self, p):
        '''prog_funcs : funcs prog_funcs
                    | empty'''
        if len(p) == 3:
            p[0] = ('prog_funcs', p[1], p[2])
        else:
            p[0] = p[1]

    def p_body(self, p):
        'body : LBRACE body_prime RBRACE'
        p[0] = ('body', p[2])

    def p_body_prime(self, p):
        '''body_prime : statement body_prime
                    | empty'''
        if len(p) == 3:
            p[0] = ('body_prime', p[1], p[2])
        else:
            p[0] = p[1]

    def p_assign(self, p):
        'assign : ID EQUALS expression SEMICOLON'
        result = self.operand_stack.pop()
        result_type = self.type_stack.pop()
        var_name = p[1]
        
        var_info = self.symbol_table.lookup_variable(var_name)
        if not var_info:
            raise ValueError(f"Undeclared variable {var_name}")
        
        # Type checking
        if var_info['type'] != result_type:
            if var_info['type'] == 'float' and result_type == 'int':
                # Convert int to float
                temp = self.generate_temp('float')
                self.emit_quad('int_to_float', result, None, temp)
                result = temp
            else:
                raise ValueError(f"Type mismatch in assignment to {var_name}")
        
        self.emit_quad('=', result, None, var_name)
        p[0] = ('assign', var_name, p[3])

    def p_expression(self, p):
        'expression : exp expression_prime'
        if p[2][0] != 'empty':  # If there's a relational operator
            op = p[2][1]
            right_operand = self.operand_stack.pop()
            right_type = self.type_stack.pop()
            left_operand = self.operand_stack.pop()
            left_type = self.type_stack.pop()
            
            # Type conversion if needed
            if left_type != right_type:
                if {left_type, right_type} == {'int', 'float'}:
                    if left_type == 'int':
                        temp = self.generate_temp('float')
                        self.emit_quad('int_to_float', left_operand, None, temp)
                        left_operand = temp
                        left_type = 'float'
                    else:
                        temp = self.generate_temp('float')
                        self.emit_quad('int_to_float', right_operand, None, temp)
                        right_operand = temp
                        right_type = 'float'
            
            temp = self.generate_temp('bool')
            self.emit_quad(op, left_operand, right_operand, temp)
            self.operand_stack.append(temp)
            self.type_stack.append('bool')
        p[0] = ('expression', p[1], p[2])

    def p_expression_prime(self, p):
        '''expression_prime : GREATER exp
                        | LESS exp
                        | NOT_EQUALS exp
                        | empty'''
        if len(p) == 3:  # Casos con operador
            p[0] = ('relop', p[1], p[2])
        else:  # Caso empty
            p[0] = p[1]

    def p_cte(self, p):
        '''cte : CTE_INT
                | CTE_FLOAT'''
        p[0] = ('cte', p[1])

    def p_funcs(self, p):
        'funcs : VOID ID LPAREN funcs_prime RPAREN LBRACKET funcs_vars body RBRACKET SEMICOLON'
        func_name = p[2]
        return_type = p[1]
        
        # Add function to symbol table
        self.symbol_table.add_function(func_name, return_type)
        self.symbol_table.enter_scope()  # Function scope
        
        # Process parameters (from funcs_prime) and variables (from funcs_vars)
        p[0] = ('funcs', func_name, p[4], p[7], p[8])
        
        self.symbol_table.exit_scope()  # Exit function scope

    def p_funcs_prime(self, p):
        '''funcs_prime : ID COLON type more_funcs
                    | empty'''
        if len(p) == 5:
            p[0] = ('funcs_prime', p[1], p[3], p[4])
        else:
            p[0] = p[1]

    def p_more_funcs(self, p):
        '''more_funcs : COMMA ID COLON type more_funcs
                    | empty'''
        if len(p) == 6:
            p[0] = ('more_funcs', p[2], p[4], p[5])
        else:
            p[0] = p[1]

    def p_funcs_vars(self, p):
        '''funcs_vars : vars
                    | empty'''
        p[0] = p[1]

    def p_statement(self, p):
        '''statement : assign
                    | condition
                    | cycle
                    | f_call
                    | print'''
        p[0] = ('statement', p[1])

    def p_exp(self, p):
        'exp : term exp_prime'
        if p[2][0] != 'empty':  # If there's + or -
            op = p[2][0]
            right_operand = self.operand_stack.pop()
            left_operand = self.operand_stack.pop()
            temp = self.generate_temp()
            
            self.emit_quad(op, left_operand, right_operand, temp)
            self.operand_stack.append(temp)
        p[0] = ('exp', p[1], p[2])

    def p_exp_prime(self, p):
        '''exp_prime : PLUS term exp_prime
                    | MINUS term exp_prime
                    | empty'''
        if len(p) == 4:
            p[0] = (p[1], p[2], p[3])
        else:
            p[0] = p[1]

    def p_term(self, p):
        'term : factor term_prime'
        if p[2][0] != 'empty':  # If there's * or /
            op = p[2][0]
            right_operand = self.operand_stack.pop()
            left_operand = self.operand_stack.pop()
            temp = self.generate_temp()
            
            self.emit_quad(op, left_operand, right_operand, temp)
            self.operand_stack.append(temp)
        p[0] = ('term', p[1], p[2])

    def p_term_prime(self, p):
        '''term_prime : TIMES factor term_prime
                    | DIVIDE factor term_prime
                    | empty'''
        if len(p) == 4:
            p[0] = (p[1], p[2], p[3])
        else:
            p[0] = p[1]

    def p_factor(self, p):
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

    def p_factor_prime(self, p):
        '''factor_prime : ID
                        | cte'''
        if p[1][0] == 'ID':
            # Verify variable exists in symbol table
            var_info = self.symbol_table.lookup_variable(p[1])
            if not var_info:
                raise ValueError(f"Undeclared variable {p[1]}")
            self.operand_stack.append(p[1])
        else:  # Constant
            self.operand_stack.append(p[1])
        p[0] = ('factor_prime', p[1])

    def p_vars(self, p):
        'vars : VAR vars_prime'
        p[0] = ('vars', p[2])

    def p_vars_prime(self, p):
        '''vars_prime : ID id COLON type SEMICOLON vars_prime
                    | empty'''
        if len(p) == 7:  # Variable declaration
            var_type = p[4][1]  # Extract type from ('type', 'int/float')
            # Add all variables in declaration list
            variables = [p[1]] + self._flatten_id_list(p[2])
            for var_name in variables:
                self.symbol_table.add_variable(var_name, var_type)
            p[0] = ('vars_prime', variables, var_type, p[6])
        else:
            p[0] = p[1]

    def _flatten_id_list(self, id_node):
        """Helper to convert ('id', 'a', ('id', 'b', ...)) to ['a', 'b', ...]"""
        result = []
        while id_node[0] == 'id':
            result.append(id_node[1])
            id_node = id_node[2]
        return result

    def p_id(self, p):
        '''id : COMMA ID id
            | empty'''
        if len(p) == 4:
            p[0] = ('id', p[2], p[3])
        else:
            p[0] = p[1]

    def p_type(self, p):
        '''type : INT
                | FLOAT'''
        p[0] = ('type', p[1])

    def p_print(self, p):
        'print : PRINT LPAREN print_prime RPAREN SEMICOLON'
        p[0] = ('print', p[3])

    def p_print_prime(self, p):
        '''print_prime : expression more_print
                    | CTE_STRING more_print'''
        p[0] = ('print_prime', p[1], p[2])

    def p_more_print(self, p):
        '''more_print : COMMA more_print_prime more_print
                    | empty'''
        if len(p) == 4:
            p[0] = ('more_print', p[2], p[3])
        else:
            p[0] = p[1]

    def p_more_print_prime(self, p):
        '''more_print_prime : expression
                            | CTE_STRING'''
        p[0] = ('more_print_prime', p[1])

    def p_cycle(self, p):
        'cycle : WHILE LPAREN expression RPAREN DO body SEMICOLON'
        p[0] = ('while_loop', p[3], p[6])

    def p_condition(self, p):
        'condition : IF LPAREN expression RPAREN body else_condition SEMICOLON'
        p[0] = ('condition', p[3], p[5], p[6])

    def p_else_condition(self, p):
        '''else_condition : ELSE body
                        | empty'''
        if len(p) == 3:
            p[0] = ('else', p[2])
        else:
            p[0] = p[1]

    def p_f_call(self, p):
        'f_call : ID LPAREN f_call_prime RPAREN SEMICOLON'
        p[0] = ('f_call', p[1], p[3])

    def p_f_call_prime(self, p):
        '''f_call_prime : expression more_f_call
                        | empty'''
        if len(p) == 3:
            p[0] = ('f_call_prime', p[1], p[2])
        else:
            p[0] = p[1]

    def p_more_f_call(self, p):
        '''more_f_call : COMMA expression more_f_call
                    | empty'''
        if len(p) == 4:
            p[0] = ('more_f_call', p[2], p[3])
        else:
            p[0] = p[1]

    def p_empty(self, p):
        'empty :'
        p[0] = ('empty',)

    def p_error(self, p):
        if p:
            print(f"Error de sintaxis antes de '{p.value}' (tipo {p.type}, línea {p.lineno})")
            print("Contexto:", self.parser.symstack[-5:])
        else:
            print("Error de sintaxis al final del input")

    # --------------------- Public Interface ---------------------
    def compile(self, source_code):
        self.lexer.input(source_code)
        return self.parser.parse(lexer=self.lexer)
    
    def reset(self):
        """Reset the compiler state for new compilation"""
        self.quadruples = []
        self.operand_stack = []
        self.operator_stack = []
        self.temp_counter = 0  # Reset temporary counter
        # Reset symbol table if needed
        self.symbol_table = SymbolTable()
        self.symbol_table.enter_scope()  # Global scope
    

def run_test_case(compiler, source_code, expected_quads):
    print(f"\nTesting: {source_code.strip()}")
    try:
        compiler.reset()  # Reset compiler state before each test
        
        compiler.compile(source_code)
        
        print("Generated Quadruples:")
        for i, quad in enumerate(compiler.quadruples):
            print(f"{i}: {quad}")
            
        # Normalize temporary variables in expected quads for comparison
        normalized_quads = []
        temp_map = {}
        next_temp = 0
        
        for quad in compiler.quadruples:
            normalized = list(quad)
            for i in range(4):
                if isinstance(normalized[i], str) and normalized[i].startswith('t'):
                    if normalized[i] not in temp_map:
                        temp_map[normalized[i]] = f"t{next_temp}"
                        next_temp += 1
                    normalized[i] = temp_map[normalized[i]]
            normalized_quads.append(tuple(normalized))
        
        # Do the same normalization for expected quads
        expected_normalized = []
        temp_map = {}
        next_temp = 0
        
        for quad in expected_quads:
            normalized = list(quad)
            for i in range(4):
                if isinstance(normalized[i], str) and normalized[i].startswith('t'):
                    if normalized[i] not in temp_map:
                        temp_map[normalized[i]] = f"t{next_temp}"
                        next_temp += 1
                    normalized[i] = temp_map[normalized[i]]
            expected_normalized.append(tuple(normalized))
            
        assert len(normalized_quads) == len(expected_normalized), \
            f"Expected {len(expected_normalized)} quads, got {len(normalized_quads)}"
            
        for i, (generated, expected) in enumerate(zip(normalized_quads, expected_normalized)):
            assert generated == expected, f"Quad {i} mismatch:\nGenerated: {generated}\nExpected: {expected}"
            
        print("✓ Test passed")
    except Exception as e:
        print(f"✗ Test failed: {str(e)}")
        raise

# Initialize compiler once
compiler = Compiler()

def test_arithmetic_with_memory():
    compiler = Compiler()
    source_code = """
    program test;
    var a,b,c,x:int;
    main {
        x = a + b * c;
    }
    end
    """
    
    # Expected memory addresses might be:
    # a: 1 (global_int)
    # b: 2 (global_int)
    # c: 3 (global_int)
    # x: 4 (global_int)
    # t0: 161 (temp_int)
    # t1: 162 (temp_int)
    
    expected_quads = [
        (3, 2, 3, 161),  # * b(2) c(3) -> t0(161)
        (1, 1, 161, 162), # + a(1) t0(161) -> t1(162)
        (7, 162, 0, 4)    # = t1(162) -> x(4)
    ]
    
    run_test_case(compiler, source_code, expected_quads)

test_arithmetic_with_memory()