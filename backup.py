import ply.lex as lex
import ply.yacc as yacc

class SymbolTable:
    def __init__(self, memory_manager):
        self.scopes = [{}]  # Stack of scopes (global scope by default)
        self.functions = {}  # Function directory
        self.current_scope = 0  # Points to current scope in stack
        self.memory_manager = memory_manager  # Reference to memory manager

    def enter_scope(self):
        """Create a new scope level"""
        self.scopes.append({})
        self.current_scope += 1
        if self.current_scope > 1:  # Skip for global scope
            self.memory_manager.push_context()

    def exit_scope(self):
        """Leave current scope"""
        if self.current_scope > 0:  # Don't pop global scope
            self.scopes.pop()
            self.current_scope -= 1
            if self.current_scope > 0:  # Skip for global scope
                self.memory_manager.pop_context()

    def add_variable(self, name, var_type, size=1):
        """Add variable to current scope with memory allocation"""
        if name in self.scopes[self.current_scope]:
            raise ValueError(f"Variable '{name}' already declared in this scope")
        
        # Determine memory segment (global or local)
        segment = 'global' if self.current_scope == 0 else 'local'
        
        # Get memory address for this variable
        address = self.memory_manager.get_address(segment, var_type)
        
        self.scopes[self.current_scope][name] = {
            'type': var_type,
            'size': size,
            'address': address
        }
        return address

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
    # Memory segment boundaries
    MEMORY_MAP = {
        'global': {
            'int': (1, 50),
            'float': (51, 100)
        },
        'local': {
            'int': (101, 150),
            'float': (151, 200)
        },
        'temp': {
            'int': (201, 250),
            'float': (251, 300),
            'bool': (301, 350)
        },
        'constant': {
            'int': (351, 400),
            'float': (401, 450),
            'string': (451, 500)
        }
    }
    
    # Operation codes
    OPERATIONS = {
        '+': 1,
        '-': 2,
        '*': 3,
        '/': 4,
        '<': 5,
        '>': 6,
        '=': 7,
        '!=': 8,
        'GOTO': 9,
        'GOTOF': 10,
        'PRINT': 11,
        'ERA': 12,
        'PARAM': 13,
        'GOSUB': 14,
        'ENDF': 15,
        'RETURN': 16
    }
    
    def __init__(self):
        # Initialize memory counters for each segment
        self.counters = {}
        for segment, types in self.MEMORY_MAP.items():
            self.counters[segment] = {}
            for type_name, (start, _) in types.items():
                self.counters[segment][type_name] = start
        
        # Constant value to address mapping for reuse
        self.constant_map = {
            'int': {},
            'float': {},
            'string': {}
        }
        
        # Stack for local memory contexts (for function calls)
        self.context_stack = []
        
        # Virtual memory storage (address -> value)
        self.memory = {}
    
    def get_address(self, segment, data_type, value=None):
        """Get a memory address for a variable or constant."""
        if segment == 'constant':
            # For constants, check if we've already assigned an address
            if value in self.constant_map[data_type]:
                return self.constant_map[data_type][value]
            
        # Get the current counter for this segment/type
        counter = self.counters[segment][data_type]
        
        # Check if we've reached the limit for this segment
        start, end = self.MEMORY_MAP[segment][data_type]
        if counter > end:
            raise MemoryError(f"Out of memory in {segment} {data_type} segment")
        
        # Increment the counter for next allocation
        self.counters[segment][data_type] += 1
        
        if segment == 'constant':
            # Store the mapping from value to address for constants
            self.constant_map[data_type][value] = counter
        
        return counter
    
    def get_operation_code(self, operation):
        """Get the numeric code for an operation."""
        return self.OPERATIONS.get(operation, 0)
    
    def reset(self):
        """Reset memory manager state."""
        # Reset counters to initial values
        for segment, types in self.MEMORY_MAP.items():
            for type_name, (start, _) in types.items():
                self.counters[segment][type_name] = start
        
        # Clear constant mappings
        self.constant_map = {
            'int': {},
            'float': {},
            'string': {}
        }
        
        # Clear context stack
        self.context_stack = []
        
        # Clear memory
        self.memory = {}
    
    def push_context(self):
        """Push current local counters to stack (for function calls)."""
        self.context_stack.append(self.counters['local'].copy())
        
        # Reset local counters to initial values
        for type_name, (start, _) in self.MEMORY_MAP['local'].items():
            self.counters['local'][type_name] = start
    
    def pop_context(self):
        """Restore previous local counters (when returning from function)."""
        if self.context_stack:
            self.counters['local'] = self.context_stack.pop()
            
    def get_address_type(self, address):
        """Get the segment and type for a given address."""
        for segment, types in self.MEMORY_MAP.items():
            for type_name, (start, end) in types.items():
                if start <= address <= end:
                    return segment, type_name
        return None, None
    
    def store_value(self, address, value):
        """Store a value at a memory address."""
        self.memory[address] = value
    
    def get_value(self, address):
        """Get the value at a memory address."""
        return self.memory.get(address)

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
        # Initialize memory manager
        self.memory_manager = MemoryManager()
        
        # Initialize compiler state
        self.symbol_table = SymbolTable(self.memory_manager)
        self.quadruples = []
        self.memory_quadruples = []  # New list for memory-based quadruples
        self.operand_stack = []
        self.type_stack = []         # Stack to track operand types
        self.address_stack = []      # Stack to track operand addresses
        self.operator_stack = []
        self.temp_counter = 0
        self.jump_stack = []
        
        # Build the lexer and parser
        self.lexer = lex.lex(module=self)
        self.parser = yacc.yacc(module=self)

    def generate_temp(self, result_type):
        """Generate a new temporary variable with memory allocation"""
        temp_name = f"t{self.temp_counter}"
        self.temp_counter += 1
        
        # Allocate memory for this temporary
        temp_address = self.memory_manager.get_address('temp', result_type)
        
        # Add to symbol table for reference
        self.symbol_table.scopes[self.symbol_table.current_scope][temp_name] = {
            'type': result_type,
            'size': 1,
            'address': temp_address
        }
        
        return temp_name, temp_address
    
    def get_result_type(self, left_type, right_type, operator):
        """Determine the result type of an operation based on operand types"""
        # Simplified type checking rules
        if operator in ['<', '>', '!=']:
            return 'bool'
        
        if left_type == 'float' or right_type == 'float':
            return 'float'
        
        return 'int'

    def emit_quad(self, op, arg1, arg2, result):
        """Generate both standard and memory-based quadruples"""
        # Standard quadruple (using variable names/values)
        quad = (op, arg1, arg2, result)
        self.quadruples.append(quad)
        
        # Memory-based quadruple (using memory addresses)
        op_code = self.memory_manager.get_operation_code(op)
        
        # Helper function to get address for an argument (arg1, arg2, or result)
        def get_operand_address(operand, is_result_operand=False):
            if operand is None:
                return 0

            # 1. Handle special integer cases (e.g., jump addresses for GOTO/GOTOF)
            # This is only relevant for the 'result' operand in specific operations.
            if is_result_operand and isinstance(operand, int) and op in ['GOTO', 'GOTOF']:
                return operand
            
            # 2. Handle string operands (could be variable names or string literals)
            if isinstance(operand, str):
                var_info = self.symbol_table.lookup_variable(operand)
                if var_info:
                    # It's a variable or temporary variable
                    return var_info['address']
                else:
                    # It's a string literal (e.g., "hello")
                    addr = self.memory_manager.get_address('constant', 'string', operand)
                    self.memory_manager.store_value(addr, operand)
                    return addr
            
            # 3. Handle numeric constants (int or float literals)
            elif isinstance(operand, (int, float)) and not isinstance(operand, bool):
                if isinstance(operand, int):
                    addr = self.memory_manager.get_address('constant', 'int', operand)
                    self.memory_manager.store_value(addr, operand)
                    return addr
                elif isinstance(operand, float):
                    addr = self.memory_manager.get_address('constant', 'float', operand)
                    self.memory_manager.store_value(addr, operand)
                    return addr
            
            # Default for unhandled types or errors
            # print(f"Warning: Unhandled operand type for '{operand}' ({type(operand)})")
            return 0

        # Get addresses for all operands using the helper function
        arg1_addr = get_operand_address(arg1)
        arg2_addr = get_operand_address(arg2)
        result_addr = get_operand_address(result, is_result_operand=True) # Pass flag for special result handling
        
        # Create memory quadruple
        memory_quad = (op_code, arg1_addr, arg2_addr, result_addr)
        self.memory_quadruples.append(memory_quad)
        
        return len(self.quadruples) - 1
        
    def get_operand_type(self, operand):
        """Determine type of an operand (variable or constant)"""
        if operand is None:
            return None
        if isinstance(operand, int):
            return 'int'
        elif isinstance(operand, float):
            return 'float'
        else:  # Variable name
            var_info = self.symbol_table.lookup_variable(operand)
            return var_info['type'] if var_info else None

    def fill_quad(self, quad_index, value):
        """Fill a previously generated quadruple with a jump address"""
        if 0 <= quad_index < len(self.quadruples):
            op, arg1, arg2, _ = self.quadruples[quad_index]
            self.quadruples[quad_index] = (op, arg1, arg2, value)
        else:
            raise IndexError(f"Quadruple index {quad_index} out of range.")

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
        # Get expression result info from stacks
        result = self.operand_stack.pop()
        result_type = self.type_stack.pop()
        result_addr = self.address_stack.pop()
        
        var_name = p[1]
        
        # Verify variable exists
        var_info = self.symbol_table.lookup_variable(var_name)
        if not var_info:
            raise ValueError(f"Undeclared variable {var_name}")
        
        # Check type compatibility for assignment
        if var_info['type'] == 'int' and result_type == 'float':
            print(f"Warning: Possible loss of precision assigning float to int in {var_name}")
        
        self.emit_quad('=', result, None, var_name)
        p[0] = ('assign', var_name, p[3])

    def p_expression(self, p):
        'expression : exp expression_prime'
        if p[2][0] != 'empty':  # If there's a relational operator
            op = p[2][1]
            right_operand = self.operand_stack.pop()
            right_type = self.type_stack.pop()
            right_addr = self.address_stack.pop()
            
            left_operand = self.operand_stack.pop()
            left_type = self.type_stack.pop()
            left_addr = self.address_stack.pop()
            
            # Determine result type for comparison operations
            result_type = 'bool'  # Comparisons always return boolean
            
            # Generate temporary variable with correct type
            temp_name, temp_addr = self.generate_temp(result_type)
            
            # Generate the comparison quadruple
            self.emit_quad(op, left_operand, right_operand, temp_name)
            
            # Push the result onto the stacks
            self.operand_stack.append(temp_name)
            self.type_stack.append(result_type)
            self.address_stack.append(temp_addr)
        p[0] = ('expression', p[1], p[2])

    def p_expression_prime(self, p):
        '''expression_prime : GREATER exp
                        | LESS exp
                        | NOT_EQUALS exp
                        | empty'''
        if len(p) == 3:  # Case with operator
            # Get the right operand info from stacks (from exp)
            # We don't pop here, as that's done in p_expression
            p[0] = ('relop', p[1], p[2])
        else:  # Empty case
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
        p[0] = ('exp', p[1], p[2])

    def p_exp_prime(self, p):
        '''exp_prime : PLUS save_operator term process_operation exp_prime
                    | MINUS save_operator term process_operation exp_prime
                    | empty'''
        if len(p) == 6:
            p[0] = (p[1], p[3], p[5])
        else:
            p[0] = p[1]

    # Add embedded action helpers for arithmetic operations
    def p_save_operator(self, p):
        'save_operator :'
        # Save the operator to operator stack
        self.operator_stack.append(p[-1])
        p[0] = ('save_operator',)

    def p_process_operation(self, p):
        'process_operation :'
        if self.operator_stack:
            operator = self.operator_stack.pop()
            
            # Get right operand information
            right_operand = self.operand_stack.pop()
            right_type = self.type_stack.pop()
            right_addr = self.address_stack.pop()
            
            # Get left operand information
            left_operand = self.operand_stack.pop()
            left_type = self.type_stack.pop()
            left_addr = self.address_stack.pop()
            
            # Determine result type
            result_type = self.get_result_type(left_type, right_type, operator)
            
            # Generate temporary variable with proper type
            temp_name, temp_addr = self.generate_temp(result_type)
            
            # Generate quadruple
            self.emit_quad(operator, left_operand, right_operand, temp_name)
            
            # Push result back onto stacks
            self.operand_stack.append(temp_name)
            self.type_stack.append(result_type)
            self.address_stack.append(temp_addr)
        
        p[0] = ('process_operation',)

    def p_term(self, p):
        'term : factor term_prime'
        p[0] = ('term', p[1], p[2])

    
    def p_term_prime(self, p):
        '''term_prime : TIMES save_operator factor process_operation term_prime
                    | DIVIDE save_operator factor process_operation term_prime
                    | empty'''
        if len(p) == 6:
            p[0] = (p[1], p[3], p[5])
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

    # Fix for factor handling
    def p_factor_prime(self, p):
        '''factor_prime : ID
                        | cte'''
        if isinstance(p[1], tuple) and p[1][0] == 'cte':
            # Handle constant
            value = p[1][1]
            
            # Determine type and get memory address
            if isinstance(value, int):
                addr = self.memory_manager.get_address('constant', 'int', value)
                self.memory_manager.store_value(addr, value)
                data_type = 'int'
            elif isinstance(value, float):
                addr = self.memory_manager.get_address('constant', 'float', value)
                self.memory_manager.store_value(addr, value)
                data_type = 'float'
            
            # Push to stacks
            self.operand_stack.append(value)
            self.type_stack.append(data_type)
            self.address_stack.append(addr)
            
        else:  # ID
            # Verify variable exists in symbol table
            var_name = p[1]
            var_info = self.symbol_table.lookup_variable(var_name)
            
            if not var_info:
                raise ValueError(f"Undeclared variable {var_name}")
            
            # Push to stacks
            self.operand_stack.append(var_name)
            self.type_stack.append(var_info['type'])
            self.address_stack.append(var_info['address'])
            
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
        '''print_prime : expression
                        | CTE_STRING'''
        if p[1] == ('empty',):  # Handle the empty case for print_prime if it was allowed
            p[0] = ('print_prime', p[1])
            return

        if isinstance(p[1], tuple) and p[1][0] == 'expression':
            # This means an expression was just parsed, its result is on the stack
            operand = self.operand_stack.pop()
            self.type_stack.pop() # Pop the type as well
            self.address_stack.pop() # Pop the address as well
            self.emit_quad('PRINT', None, None, operand)
        else:
            # It's a CTE_STRING
            self.emit_quad('PRINT', None, None, p[1]) # Pass the string literal directly

        p[0] = ('print_prime', p[1])

    def p_more_print(self, p):
        '''more_print : COMMA print_item more_print
                    | empty'''
        # The logic for emitting quadruples for subsequent print items needs to be here.
        # When `print_item` is processed, it will put the operand on the stack.
        # This rule then needs to consume it and generate the PRINT quad.
        if len(p) == 4:
            # 'print_item' will have already pushed its result to the stacks
            # So, we just need to generate the PRINT quad for it
            operand = self.operand_stack.pop()
            self.type_stack.pop()
            self.address_stack.pop()
            self.emit_quad('PRINT', None, None, operand)
            p[0] = ('more_print', p[2], p[3])
        else:
            p[0] = p[1] # 'empty'

    def p_print_item(self, p):
        '''print_item : expression
                      | CTE_STRING'''
        # This new rule will handle both expression and string for print_prime and more_print
        # It's responsible for pushing the operand onto the stack for expressions,
        # or for handling the string literal directly if it's not an expression.
        if isinstance(p[1], tuple) and p[1][0] == 'expression':
            # The 'expression' rule itself already pushes the result onto the stacks.
            # So, we don't need to do anything further here for expressions, just pass it up.
            p[0] = ('print_item', p[1])
        else:
            # It's a CTE_STRING. Push its value (the string content) onto the operand stack
            # and provide a dummy type/address if needed for consistency with other operations.
            # For a print string, we directly use the string value as the operand.
            # No need to allocate constant memory for print string in this simplified example
            # but in a real compiler, you might. For now, just pass the string.
            self.operand_stack.append(p[1])
            self.type_stack.append('string') # Assuming 'string' type for string literals
            string_addr = self.memory_manager.get_address('constant', 'string', p[1])
            self.memory_manager.store_value(string_addr, p[1])
            self.address_stack.append(string_addr)
            p[0] = ('print_item', p[1])

    # -------------------- New embedded actions for control flow --------------------
    
    # Action after evaluating the condition in if statement
    def p_if_condition(self, p):
        'if_condition :'
        # Get condition result from stacks
        result = self.operand_stack.pop()
        result_type = self.type_stack.pop()
        result_addr = self.address_stack.pop()
        
        # Verify result is boolean
        if result_type != 'bool':
            print(f"Warning: Non-boolean condition in if statement")
        
        # Generate quad to jump if false - use the address directly
        goto_f_index = self.emit_quad('GOTOF', result, None, None)
        
        # Push jump index to jump stack
        self.jump_stack.append(goto_f_index)
        
        p[0] = ('if_condition',)

    # Action at the end of the 'then' block in if statement
    def p_if_end(self, p):
        'if_end :'
        # For if-else, generate jump to skip else block
        goto_index = self.emit_quad('GOTO', None, None, None)
        
        # Get the GOTOF index to fill
        false_jump = self.jump_stack.pop()
        
        # Fill the false jump to point to next quad (else or after if)
        self.fill_quad(false_jump, len(self.quadruples))
        
        # Push the GOTO index for later filling (after else block)
        self.jump_stack.append(goto_index)
        
        p[0] = ('if_end',)

    # Action at the very end of if-else statement
    def p_if_else_end(self, p):
        'if_else_end :'
        # Get the GOTO index from the end of 'then' block
        end_jump = self.jump_stack.pop()
        
        # Fill it with current quad position (after the else block)
        self.fill_quad(end_jump, len(self.quadruples))
        
        p[0] = ('if_else_end',)

    # Action at the beginning of a while loop
    def p_while_start(self, p):
        'while_start :'
        # Push current quad position to mark loop start
        self.jump_stack.append(len(self.quadruples))
        
        p[0] = ('while_start',)

    # Action after evaluating the condition in while loop
    def p_while_condition(self, p):
        'while_condition :'
        # Get condition result from stacks
        result = self.operand_stack.pop()
        result_type = self.type_stack.pop()
        result_addr = self.address_stack.pop()
        
        # Verify result is boolean
        if result_type != 'bool':
            print(f"Warning: Non-boolean condition in while loop")
        
        # Generate quad to jump if false - use the address directly
        goto_f_index = self.emit_quad('GOTOF', result, None, None)
        
        # Push jump index to jump stack
        self.jump_stack.append(goto_f_index)
        
        p[0] = ('while_condition',)

    # Action at the end of while loop body
    def p_while_end(self, p):
        'while_end :'
        # Get the GOTOF index
        false_jump = self.jump_stack.pop()
        
        # Get the loop start position
        start_position = self.jump_stack.pop()
        
        # Generate jump back to loop start
        self.emit_quad('GOTO', None, None, start_position)
        
        # Fill the false jump to point after the loop
        self.fill_quad(false_jump, len(self.quadruples))
        
        p[0] = ('while_end',)

    # -------------------- Update control flow rules with embedded actions --------------------
    
    def p_cycle(self, p):
        'cycle : WHILE while_start LPAREN expression RPAREN while_condition DO body while_end SEMICOLON'
        p[0] = ('while_loop', p[4], p[8])

    def p_condition(self, p):
        '''condition : IF LPAREN expression RPAREN if_condition body if_end else_condition if_else_end SEMICOLON'''
        p[0] = ('condition', p[3], p[6], p[8])

    def p_else_condition(self, p):
        '''else_condition : ELSE body
                        | empty'''
        if len(p) == 3:
            p[0] = ('else', p[2])
        else:
            p[0] = p[1]
    
    # -------------------- End of control flow updates --------------------

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
        self.memory_manager.reset()
        self.quadruples = []
        self.memory_quadruples = []
        self.operand_stack = []
        self.type_stack = []
        self.address_stack = []
        self.operator_stack = []
        self.jump_stack = []
        self.temp_counter = 0
        
        # Reset symbol table
        self.symbol_table = SymbolTable(self.memory_manager)
    

def run_test_case(compiler, source_code, case_name=""):
    print(f"\nTesting: {case_name}")
    print(f"Source code: {source_code.strip()}")
    compiler.reset()  # Reset compiler state before each test
    
    try:
        compiler.compile(source_code)
        
        print("\nStandard Quadruples:") 
        for i, quad in enumerate(compiler.quadruples):
            print(f"{i}: {quad}")
            
        print("\nMemory-Based Quadruples:")
        for i, quad in enumerate(compiler.memory_quadruples):
            print(f"{i}: {quad}")
    except Exception as e:
        print(f"Compilation error: {e}")

compiler = Compiler()

run_test_case(compiler, '''
program printarg;
void function (a:int) [
    var b:int;      
    { 
        b = 2;
        print(a+b);   
    }]
main {
    function(1);
}
end
''')