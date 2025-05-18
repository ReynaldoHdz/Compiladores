class MemoryManager:
    def __init__(self):
        # Simplified memory ranges (20 blocks per type)
        self.memory_ranges = {
            'global_int': range(0, 20),
            'global_float': range(20, 40),
            'temp_int': range(40, 60),
            'temp_float': range(60, 80),
            'const_int': range(80, 100),
            'const_float': range(100, 120)
        }
        
        self.next_address = {k: v.start for k, v in self.memory_ranges.items()}
        self.constants = {}  # {value: address} mapping
        
        # Operation codes
        self.op_codes = {
            '+': 1, '-': 2, '*': 3, '/': 4,
            '<': 5, '>': 6, '=': 7, '!=': 8,
            'int_to_float': 9
        }

    def allocate(self, var_type, is_temp=False, value=None):
        """Allocate memory for a variable or constant"""
        if value is not None:
            # Handle constants
            if value in self.constants:
                return self.constants[value]
            
            segment = f'const_{var_type}'
            addr = self.next_address[segment]
            self.constants[value] = addr
            self.next_address[segment] += 1
            return addr
        
        # Handle variables and temporaries
        segment = f"{'temp' if is_temp else 'global'}_{var_type}"
        addr = self.next_address[segment]
        self.next_address[segment] += 1
        return addr

    def get_op_code(self, operator):
        return self.op_codes.get(operator, -1)
    
    def validate_assignment(self, target_type, source_type, source_value=None):
        """Check if assignment is valid with possible conversion"""
        if target_type == source_type:
            return True
        elif target_type == 'float' and source_type == 'int':
            return True  # Implicit int-to-float conversion always allowed
        elif target_type == 'int' and source_type == 'float':
            # Only allow if it's a constant float with integer value
            if isinstance(source_value, float):
                return source_value.is_integer()
            return False  # Disallow float variables being assigned to int
        return False