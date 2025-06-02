class ActivationRecord:
    def __init__(self, function_name):
        self.function_name = function_name
        self.local_memory = {}  # address -> value mapping for local variables
        self.return_address = None
    
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

        # Add activation record stack
        self.activation_stack = []
        self.global_memory = {}  # Global memory separate from activation records
    
    def push_activation_record(self, function_name):
        """Push a new activation record for function call"""
        record = ActivationRecord(function_name)
        self.activation_stack.append(record)
        return record
    
    def pop_activation_record(self):
        """Pop the current activation record when function returns"""
        if self.activation_stack:
            return self.activation_stack.pop()
        return None
    
    def get_current_activation_record(self):
        """Get the current activation record (top of stack)"""
        return self.activation_stack[-1] if self.activation_stack else None
    
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
        """Store a value at a memory address with scope awareness"""
        segment, data_type = self.get_address_type(address)
        
        if segment == 'global':
            self.global_memory[address] = value
        elif segment in ['local', 'temp']:
            # Store in current activation record if it exists, otherwise global
            current_record = self.get_current_activation_record()
            if current_record:
                current_record.local_memory[address] = value
            else:
                self.global_memory[address] = value
        else:  # constant
            self.memory[address] = value
    
    def get_value(self, address):
        """Get the value at a memory address with scope awareness"""
        segment, data_type = self.get_address_type(address)
        
        if segment == 'global':
            return self.global_memory.get(address)
        elif segment in ['local', 'temp']:
            # Check current activation record first, then global
            current_record = self.get_current_activation_record()
            if current_record and address in current_record.local_memory:
                return current_record.local_memory[address]
            return self.global_memory.get(address)
        else:  # constant
            return self.memory.get(address)