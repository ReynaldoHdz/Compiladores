import ply.lex as lex
import ply.yacc as yacc
from SymbolTable import SymbolTable
from MemoryManager import MemoryManager
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
        self.memory = MemoryManager()
        self.symbol_table = SymbolTable()
        self.quadruples = []
        self.operand_stack = []
        self.operator_stack = []
        self.temp_counter = 0
        
        # Build the lexer and parser
        self.lexer = lex.lex(module=self)
        self.parser = yacc.yacc(module=self)

        # Initialize global scope
        self.current_scope = "global"
        self.symbol_table.enter_scope()

    def generate_temp(self):
        """Generate a new temporary variable"""
        temp_name = f"t{self.temp_counter}"
        self.temp_counter += 1
        return temp_name
        
    def get_operand_type(self, operand):
        """Determine type of an operand (variable or constant)"""
        if isinstance(operand, int):
            return 'int'
        elif isinstance(operand, float):
            return 'float'
        else:  # Variable name
            var_info = self.symbol_table.lookup_variable(operand)
            return var_info['type'] if var_info else None
        
    def get_address(self, identifier):
        """Get memory address for a variable or constant"""
        if isinstance(identifier, int):
            # Integer constant
            if identifier not in self.memory.constants['int'].values():
                addr = self.memory.allocate('const_int', identifier)
            else:
                addr = next(k for k,v in self.memory.constants['int'].items() if v == identifier)
            return addr
        
        elif isinstance(identifier, float):
            # Float constant
            if identifier not in self.memory.constants['float'].values():
                addr = self.memory.allocate('const_float', identifier)
            else:
                addr = next(k for k,v in self.memory.constants['float'].items() if v == identifier)
            return addr
        
        else:
            # Variable
            var_info = self.symbol_table.lookup_variable(identifier)
            if not var_info:
                raise ValueError(f"Undeclared variable {identifier}")
            
            segment = f"{'local' if self.current_scope != 'global' else 'global'}_{var_info['type']}"
            if not hasattr(var_info, 'address'):
                var_info['address'] = self.memory.allocate(segment)
            return var_info['address']

    def emit_quad(self, op, arg1=None, arg2=None, result=None):
        """Generate memory-based quadruple"""
        op_code = self.memory.get_op_code(op)
        if op_code == -1:
            raise ValueError(f"Invalid operation {op}")
        
        self.quadruples.append((op_code, arg1, arg2, result))

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
        var_name = p[1]
        var_info = self.symbol_table.lookup_variable(var_name)
        if not var_info:
            raise ValueError(f"Undeclared variable {var_name}")
        
        result = self.operand_stack.pop()
        
        # Get the actual value if it's a constant
        source_value = result.get('value')
        if isinstance(source_value, str) and source_value.startswith('temp'):
            source_value = None  # Don't check conversion for temps
        
        # Type checking
        if not self.memory.validate_assignment(var_info['type'], result['type'], source_value):
            raise TypeError(
                f"Cannot assign {result['type']} ({source_value if source_value else 'expression'}) "
                f"to {var_info['type']} variable {var_name}"
            )
        
        # Generate conversion if needed
        if var_info['type'] == 'float' and result['type'] == 'int':
            temp_addr = self.memory.allocate('float', is_temp=True)
            self.emit_quad('int_to_float', result['address'], None, temp_addr)
            result = {'address': temp_addr, 'type': 'float'}
        
        # Generate the assignment quadruple
        self.emit_quad('=', result['address'], None, var_info['address'])
        p[0] = ('assign', var_name, p[3])

    def p_expression(self, p):
        'expression : exp expression_prime'
        if p[2][0] != 'empty':  # If there's a relational operator
            op = p[2][1]
            right = self.operand_stack.pop()
            left = self.operand_stack.pop()
            
            # Allocate temp result (always boolean)
            temp_addr = self.memory.allocate('int', is_temp=True)
            self.emit_quad(op, left['address'], right['address'], temp_addr)
            self.operand_stack.append({
                'address': temp_addr,
                'type': 'int',
                'value': f'temp_{temp_addr}'
            })
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
        if p[2][0] != 'empty':  # + or -
            op = p[2][0]
            right = self.operand_stack.pop()
            left = self.operand_stack.pop()
            
            # Determine result type
            result_type = 'float' if 'float' in (left['type'], right['type']) else 'int'
            
            # Allocate temp
            temp_addr = self.memory.allocate(result_type, is_temp=True)
            self.emit_quad(op, left['address'], right['address'], temp_addr)
            self.operand_stack.append({
                'address': temp_addr,
                'type': result_type,
                'value': f'temp_{temp_addr}'
            })
        p[0] = ('exp', p[1], p[2])

    def get_type(self, address):
        """Determine type from memory address"""
        for segment, (start, end) in self.memory.memory_map.items():
            if start <= address <= end:
                return segment.split('_')[-1]  # Returns 'int', 'float', etc.
        raise ValueError(f"Invalid memory address {address}")

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
            right = self.operand_stack.pop()
            left = self.operand_stack.pop()
            
            # Special handling for division
            if op == '/':
                # Division always produces float
                result_type = 'float'
                
                # Convert left operand if needed
                if left['type'] == 'int':
                    temp_addr = self.memory.allocate('float', is_temp=True)
                    self.emit_quad('int_to_float', left['address'], None, temp_addr)
                    left = {'address': temp_addr, 'type': 'float'}
                
                # Convert right operand if needed
                if right['type'] == 'int':
                    temp_addr = self.memory.allocate('float', is_temp=True)
                    self.emit_quad('int_to_float', right['address'], None, temp_addr)
                    right = {'address': temp_addr, 'type': 'float'}
            else:
                # For other operations, normal type promotion
                result_type = 'float' if 'float' in (left['type'], right['type']) else 'int'
            
            # Allocate temp
            temp_addr = self.memory.allocate(result_type, is_temp=True)
            self.emit_quad(op, left['address'], right['address'], temp_addr)
            self.operand_stack.append({
                'address': temp_addr,
                'type': result_type,
                'value': f'temp_{temp_addr}'
            })
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
        if isinstance(p[1], tuple) and p[1][0] == 'ID':
            # Variable case
            var_name = p[1][1]
            var_info = self.symbol_table.lookup_variable(var_name)
            if not var_info:
                raise ValueError(f"Undeclared variable {var_name}")
            self.operand_stack.append({
                'address': var_info['address'],
                'type': var_info['type'],
                'value': var_name
            })
        else:
            # Constant case
            const_value = p[1][1] if isinstance(p[1], tuple) else p[1]
            const_type = 'float' if isinstance(const_value, float) else 'int'
            const_addr = self.memory.allocate(const_type, value=const_value)
            self.operand_stack.append({
                'address': const_addr,
                'type': const_type,
                'value': const_value
            })
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
                # Allocate memory and add to symbol table
                address = self.memory.allocate(var_type)
                self.symbol_table.add_variable(var_name, var_type)
                var_info = self.symbol_table.lookup_variable(var_name)
                var_info['address'] = address
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
    compiler.reset()
    
    # Parse the source code
    compiler.compile(source_code)
    
    # Print symbol table
    print("\nSymbol Table:")
    for scope in compiler.symbol_table.scopes:
        for name, info in scope.items():
            print(f"{name}: type={info['type']}, address={info['address']}")
    
    # Print quadruples
    print("\nGenerated Quadruples:")
    for i, (op, arg1, arg2, res) in enumerate(compiler.quadruples):
        print(f"{i:2d}: [{op}] {arg1 or '-':3} {arg2 or '-':3} -> {res}")

# Initialize compiler once
compiler = Compiler()

run_test_case(compiler, '''
program test;
var a,b,c,x:int;
main {
    x = a + b / c;
}
end
''')
