class SymbolTable:
    def __init__(self, memory_manager):
        self.scopes = [{}]  # Stack of scopes (global scope by default)
        self.functions = {}  # Function directory
        self.current_scope = 0  # Points to current scope in stack
        self.memory_manager = memory_manager  # Reference to memory manager
        self.function_start = 0

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
            'address': None,  # Will be filled during code generation
            'quad_start': None  # Starting quadruple for this function
        }
        return self.functions[name]
    
    def set_function_start(self, name, quad_index):
        """Set the starting quadruple index for a function"""
        if name in self.functions:
            self.functions[name]['quad_start'] = quad_index

    def get_function(self, name):
        """Retrieve function details"""
        return self.functions.get(name)

    def current_scope_variables(self):
        """Get variables in current scope"""
        return self.scopes[self.current_scope]