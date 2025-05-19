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

class MemoryManager:
    def __init__(self):
        # Define memory ranges (20 addresses per type)
        self.memory_ranges = {
            # Global variables
            'global_int': (1000, 1019),
            'global_float': (1020, 1039),
            'global_bool': (1040, 1059),
            'global_string': (1060, 1079),
            
            # Local variables
            'local_int': (2000, 2019),
            'local_float': (2020, 2039),
            'local_bool': (2040, 2059),
            'local_string': (2060, 2079),
            
            # Temporary variables
            'temp_int': (3000, 3019),
            'temp_float': (3020, 3039),
            'temp_bool': (3040, 3059),
            'temp_string': (3060, 3079),
            
            # Constants
            'const_int': (4000, 4019),
            'const_float': (4020, 4039),
            'const_bool': (4040, 4059),
            'const_string': (4060, 4079),
        }
        
        # Current pointers for each memory range
        self.current_pointers = {key: start for key, (start, end) in self.memory_ranges.items()}
        
        # Dictionary to store constant values
        self.constants = {
            'int': {},
            'float': {},
            'bool': {},
            'string': {}
        }
        
        # Operation codes
        self.operation_codes = {
            '+': 1,
            '-': 2,
            '*': 3,
            '/': 4,
            '<': 5,
            '>': 6,
            '=': 7,  # Assignment
            '!=': 8,
        }

    def allocate(self, var_type, scope='global', is_temp=False, is_const=False):
        """Allocate memory address based on type and scope"""
        if is_const:
            prefix = 'const'
        elif is_temp:
            prefix = 'temp'
        else:
            prefix = scope
        
        memory_key = f"{prefix}_{var_type}"
        
        if memory_key not in self.memory_ranges:
            raise ValueError(f"Invalid memory type: {memory_key}")
            
        current = self.current_pointers[memory_key]
        end = self.memory_ranges[memory_key][1]
        
        if current > end:
            raise MemoryError(f"Out of memory for {memory_key}")
            
        self.current_pointers[memory_key] += 1
        return current
    
    def get_constant_address(self, value, var_type):
        """Get or create address for a constant value"""
        if value not in self.constants[var_type]:
            address = self.allocate(var_type, is_const=True)
            self.constants[var_type][value] = address
        return self.constants[var_type][value]
    
    def get_operation_code(self, op):
        """Get numeric operation code"""
        return self.operation_codes.get(op, 0)

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
        self.memory_manager = MemoryManager()
        self.quadruples = []
        self.operand_stack = []
        self.operator_stack = []
        self.temp_counter = 0
        
        # Build the lexer and parser
        self.lexer = lex.lex(module=self)
        self.parser = yacc.yacc(module=self)

    def generate_temp(self, var_type=None):
        """Generate a new temporary variable with memory address"""
        temp_name = f"t{self.temp_counter}"
        self.temp_counter += 1
        
        # If type not provided, try to infer from operand stack
        if var_type is None and self.operand_stack:
            # Get types of top two operands (if available)
            op1_type = self.get_operand_type(self.operand_stack[-1])
            if len(self.operand_stack) > 1:
                op2_type = self.get_operand_type(self.operand_stack[-2])
                # Use the "higher" type (float > int)
                var_type = 'float' if 'float' in {op1_type, op2_type} else op1_type
            else:
                var_type = op1_type
        
        # Default to int if type still can't be determined
        if var_type is None:
            var_type = 'int'
        
        # Allocate memory for the temporary
        address = self.memory_manager.allocate(var_type, is_temp=True)
        
        # Add to symbol table
        self.symbol_table.add_variable(temp_name, var_type)
        self.symbol_table.current_scope_variables()[temp_name]['address'] = address
        
        return temp_name

    def emit_quad(self, op, arg1, arg2, result):
        """Generate quadruple with memory addresses"""
        # Get types
        type1 = self.get_operand_type(arg1)
        type2 = self.get_operand_type(arg2) if arg2 else None
        
        # Get addresses
        addr1 = self.get_operand_address(arg1)
        addr2 = self.get_operand_address(arg2) if arg2 else None
        result_addr = self.get_operand_address(result)
        
        # Handle type conversions if needed
        if type1 != type2 and arg2:
            if {type1, type2} == {'int', 'float'}:
                # Convert int to float
                if type1 == 'int':
                    conv_temp = self.generate_temp('float')
                    self.quadruples.append(('int_to_float', addr1, None, self.get_operand_address(conv_temp)))
                    addr1 = self.get_operand_address(conv_temp)
                else:
                    conv_temp = self.generate_temp('float')
                    self.quadruples.append(('int_to_float', addr2, None, self.get_operand_address(conv_temp)))
                    addr2 = self.get_operand_address(conv_temp)
        
        # Get operation code
        op_code = self.memory_manager.get_operation_code(op)
        
        # Create both versions of the quadruple
        name_quad = (op, arg1, arg2, result)
        addr_quad = (op_code, addr1, addr2, result_addr)
        
        self.quadruples.append({
            'name_quad': name_quad,
            'addr_quad': addr_quad
        })
        
        return name_quad
    
    
    def get_operand_address(self, operand):
        """Get memory address of an operand"""
        if operand is None:
            return None
            
        # Handle constants
        if isinstance(operand, int):
            return self.memory_manager.get_constant_address(operand, 'int')
        elif isinstance(operand, float):
            return self.memory_manager.get_constant_address(operand, 'float')
        elif isinstance(operand, bool):
            return self.memory_manager.get_constant_address(operand, 'bool')
        elif isinstance(operand, str) and operand.startswith('"'):
            return self.memory_manager.get_constant_address(operand, 'string')
        
        # Handle variables
        var_info = self.symbol_table.lookup_variable(operand)
        if var_info:
            return var_info['address']
        
        return None
    
    def get_operand_type(self, operand):
        """Determine type of an operand (variable or constant)"""
        if operand is None:
            return None
        if isinstance(operand, int):
            return 'int'
        elif isinstance(operand, float):
            return 'float'
        elif isinstance(operand, bool):
            return 'bool'
        elif isinstance(operand, str):
            if operand.startswith('"'):
                return 'string'
            # It's a variable name
            var_info = self.symbol_table.lookup_variable(operand)
            return var_info['type'] if var_info else None
        return None

    # Modify add_variable to allocate memory
    def add_variable(self, name, var_type, size=1):
        """Add variable to current scope with memory allocation"""
        if name in self.symbol_table.current_scope_variables():
            raise ValueError(f"Variable '{name}' already declared in this scope")
        
        # Determine scope ('global' or 'local')
        scope = 'global' if self.symbol_table.current_scope == 0 else 'local'
        
        # Allocate memory
        address = self.memory_manager.allocate(var_type, scope)
        
        # Add to symbol table
        self.symbol_table.current_scope_variables()[name] = {
            'type': var_type,
            'size': size,
            'address': address
        }

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
            temp = self.generate_temp('bool')  # Explicit bool for comparisons
            
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
    

def run_test_case(compiler, source_code):
    print(f"\nTesting: {source_code.strip()}")
    compiler.reset()  # Reset compiler state before each test
    
    compiler.compile(source_code)
        
    print("Generated Quadruples:") 
    for i, quad in enumerate(compiler.quadruples):
        print(f"{i}: {quad}")

# Initialize compiler once
compiler = Compiler()

run_test_case(compiler, '''
program pelos;
var x, y, z: int;
main {
    z = x + y * 2;
}
end
''')