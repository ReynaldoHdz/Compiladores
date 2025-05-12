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

    def add_variable(self, name, var_type, size=1):
        """Add variable to current scope"""
        if name in self.scopes[self.current_scope]:
            raise ValueError(f"Variable '{name}' already declared in this scope")
        self.scopes[self.current_scope][name] = {
            'type': var_type,
            'size': size,
            'address': None  # Will be filled during memory allocation
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
        self.temp_counter = 0
        
        # Build the lexer and parser
        self.lexer = lex.lex(module=self)
        self.parser = yacc.yacc(module=self)

    def generate_temp(self):
        """Generate a new temporary variable"""
        temp_name = f"t{self.temp_counter}"
        self.temp_counter += 1
        return temp_name

    def emit_quad(self, op, arg1, arg2, result):
        """Generate quadruple with type checking"""
        type1 = self.get_operand_type(arg1)
        type2 = self.get_operand_type(arg2) if arg2 else None
        
        # Handle type conversions if needed
        if type1 != type2 and arg2:
            if {type1, type2} == {'int', 'float'}:
                # Convert int to float
                if type1 == 'int':
                    conv_temp = self.generate_temp()
                    self.quadruples.append(('int_to_float', arg1, None, conv_temp))
                    arg1 = conv_temp
                else:
                    conv_temp = self.generate_temp()
                    self.quadruples.append(('int_to_float', arg2, None, conv_temp))
                    arg2 = conv_temp
        
        quad = (op, arg1, arg2, result)
        self.quadruples.append(quad)
        return quad
        
    def get_operand_type(self, operand):
        """Determine type of an operand (variable or constant)"""
        if isinstance(operand, int):
            return 'int'
        elif isinstance(operand, float):
            return 'float'
        else:  # Variable name
            var_info = self.symbol_table.lookup_variable(operand)
            return var_info['type'] if var_info else None

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
    # (All your existing p_* methods go here)
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
        # The expression result should be on top of operand_stack
        result = self.operand_stack.pop()
        var_name = p[1]
        
        # Verify variable exists
        if not self.symbol_table.lookup_variable(var_name):
            raise ValueError(f"Undeclared variable {var_name}")
        
        self.emit_quad('=', result, None, var_name)
        p[0] = ('assign', var_name, p[3])

    def p_expression(self, p):
        'expression : exp expression_prime'
        if p[2][0] != 'empty':  # If there's a relational operator
            op = p[2][1]
            right_operand = self.operand_stack.pop()
            left_operand = self.operand_stack.pop()
            temp = self.generate_temp()
        
            # Add type checking here using symbol_table if needed
            self.emit_quad(op, left_operand, right_operand, temp)
            self.operand_stack.append(temp)
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
        p[0] = ('while_loop', p[3], p[6])  # (condición, cuerpo)

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
            # Muestra los últimos tokens procesados
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

def test_arithmetic():
    # Simple expression
    run_test_case(compiler, """
    program test;
    var a,b,c,x:int;
    main {
        x = a + b * c;
    }
    end
    """, [
        ('*', 'b', 'c', 't0'),
        ('+', 'a', 't0', 't1'),
        ('=', 't1', None, 'x')
    ])

    # With parentheses
    run_test_case(compiler, """
    program test;
    var a,b,c,x:int;
    main {
        x = (a + b) * c;
    }
    end
    """, [
        ('+', 'a', 'b', 't0'),
        ('*', 't0', 'c', 't1'),
        ('=', 't1', None, 'x')
    ])

def test_type_conversion():
    # Mixed int/float operations
    run_test_case(compiler, """
    program test;
    var a:int; b,x:float;
    main {
        x = a + b;
    }
    end
    """, [
        ('int_to_float', 'a', None, 't0'),
        ('+', 't0', 'b', 't1'),
        ('=', 't1', None, 'x')
    ])

def test_assignment():
    run_test_case(compiler, """
    program test;
    var a,b,c: int;
    main {
        a = 1;
        b = 2;
        c = a + b;
    }
    end
    """, [
        ('=', ('cte', 1), None, 'a'),
        ('=', ('cte', 2), None, 'b'),
        ('+', 'a', 'b', 't0'),
        ('=', 't0', None, 'c')
    ])

def test_var_and_const():
    run_test_case(compiler, """
    program test;
    var a,b,c: int;
    main {
        a = 1;
        b = 2;
        c = a + b * 3;
    }
    end
    """, [
        ('=', ('cte', 1), None, 'a'),
        ('=', ('cte', 2), None, 'b'),
        ('*', 'b', ('cte', 3), 't0'),
        ('+', 'a', 't0', 't1'),
        ('=', 't1', None, 'c')
    ])

def test_error_cases():
    # Undeclared variable
    try:
        compiler.compile("""
        program test;
        main {
            x = 5;
        }
        end
        """)
        assert False, "Should have raised undeclared variable error"
    except ValueError as e:
        assert "Undeclared variable" in str(e)

    # Type mismatch
    try:
        compiler.compile("""
        program test;
        var a:int; b:float;
        main {
            a = b;
        }
        end
        """)
        assert False, "Should have raised type error"
    except ValueError as e:
        assert "Type mismatch" in str(e)

def run_full_test_suite():
    print("Running Arithmetic Tests...")
    test_arithmetic()
    
    print("\nRunning Type Conversion Test...")
    test_type_conversion()

    print("\Running Variable Assignment Test...")
    test_assignment()
    
    print("\Running Var and Const Test...")
    test_var_and_const()
    
    """ print("\nRunning Error Cases...")
    test_error_cases() """
    
    print("\nAll tests completed!")

if __name__ == "__main__":
    run_full_test_suite()