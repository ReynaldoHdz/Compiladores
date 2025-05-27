# --- 1. Provided Data Structures ---

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
    'PRINT': 11
}

# Invert OPERATIONS for easier lookup by code (e.g., 1 -> '+')
OP_CODES_MAP = {v: k for k, v in OPERATIONS.items()}


# Your Memory-Based Quadruples
quadruples = [
    (OPERATIONS['<'], 1, 351, 301),  # ('<', 'a', 8, 't0')
    (OPERATIONS['GOTOF'], 301, 0, 5),   # ('GOTOF', 't0', None, 5) - target is quad index 5
    (OPERATIONS['+'], 1, 352, 201),  # ('+', 'a', 1, 't1')
    (OPERATIONS['='], 201, 0, 1),    # ('=', 't1', None, 'a') - 'a' is at address 1
    (OPERATIONS['GOTO'], 0, 0, 6),      # ('GOTO', None, None, 6) - target is quad index 6 (end of if-else logic)
    (OPERATIONS['='], 353, 0, 3),    # ('=', 3, None, 'c') - 'c' is at address 3
]

# Constants Table: Maps constant addresses to their values
# Based on your addresses and MEMORY_MAP:
# 351 (int): 8
# 352 (int): 1
# 353 (int): 3
constants_table = {
    351: 8,
    352: 1,
    353: 3
}

# --- 2. Virtual Machine Memory ---

# The main memory for the VM.
# This will store all variable values (global, local, temporaries).
vm_memory = {}

# Initialize global variables based on your mapping.
# 'a' is global int, address 1. 'c' is global int, address 3.
# In a real compiler, these would either be initialized to a default value (like 0)
# or your language might have an explicit initialization mechanism.
vm_memory[1] = 0  # Initialize 'a'
vm_memory[3] = 0  # Initialize 'c'


# --- 3. The Virtual Machine Interpreter ---

def run_virtual_machine(quads, constants, memory, op_codes_map, memory_map):
    instruction_pointer = 0
    
    # Helper to determine the segment and type of an address
    def get_address_info(addr):
        if addr is None:
            return None, None # No info for None address

        for segment, types in memory_map.items():
            for data_type, (start, end) in types.items():
                if start <= addr <= end:
                    return segment, data_type
        return None, None # Address not found in map

    # Helper function to get the value from an address
    def get_value(addr):
        if addr is None:
            return None # Handle None operand addresses gracefully

        segment, data_type = get_address_info(addr)
        
        if segment == 'constant':
            if addr not in constants:
                raise ValueError(f"Constant address {addr} not found in constants table.")
            return constants[addr]
        elif segment in ['global', 'local', 'temp']:
            if addr not in memory:
                # For temporaries, they might not exist until assigned.
                # For variables, this might indicate an uninitialized variable.
                print(f"  Warning: Accessing uninitialized address {addr} (segment: {segment}, type: {data_type})")
                # Depending on language semantics, this could be an error or return a default.
                # For now, we'll let it potentially raise a KeyError if not handled.
            return memory.get(addr) # Use .get() to avoid KeyError for uninitialized, returns None by default
        else:
            raise ValueError(f"Address {addr} does not belong to any defined memory segment.")

    # Helper function to set the value at an address
    def set_value(addr, value):
        if addr is None:
            print("  Warning: Attempted to set value to None address. Skipping.")
            return

        segment, data_type = get_address_info(addr)
        
        if segment == 'constant':
            raise ValueError(f"Attempted to write to constant address {addr}.")
        elif segment in ['global', 'local', 'temp']:
            # Basic type checking (can be expanded)
            if data_type == 'int' and not isinstance(value, int):
                if isinstance(value, float): # Allow float to int conversion (truncation)
                    value = int(value)
                else:
                    raise TypeError(f"Type mismatch: Expected int at address {addr}, got {type(value).__name__}")
            elif data_type == 'float' and not isinstance(value, (int, float)):
                 if isinstance(value, int): # Allow int to float conversion
                    value = float(value)
                 else:
                    raise TypeError(f"Type mismatch: Expected float at address {addr}, got {type(value).__name__}")
            elif data_type == 'bool' and not isinstance(value, bool):
                raise TypeError(f"Type mismatch: Expected bool at address {addr}, got {type(value).__name__}")

            memory[addr] = value
            print(f"  --> Set memory[{addr}] (segment: {segment}, type: {data_type}) = {value}")
        else:
            raise ValueError(f"Cannot set value for address {addr} in unknown segment.")

    print("--- Starting Virtual Machine Execution ---")
    
    while instruction_pointer < len(quads):
        quad = quads[instruction_pointer]
        op_code = quad[0]
        operand1_addr = quad[1]
        operand2_addr = quad[2]
        result_addr = quad[3]

        operator = op_codes_map.get(op_code, f"UNKNOWN_OP_{op_code}")

        print(f"\nExecuting Quad {instruction_pointer}: {quad} (Op: {operator})")

        # --- The Core Operation Logic ---
        match operator:
            case '<':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                # Ensure types are comparable if needed, e.g., both int or both float
                result = val1 < val2
                set_value(result_addr, result)
                print(f"  Comparison: {val1} < {val2} = {result}")

            case 'GOTOF':
                condition = get_value(operand1_addr)
                target_quad_index = result_addr
                print(f"  GOTOF: Condition ({operand1_addr}) = {condition}, Target = {target_quad_index}")
                if not condition:
                    instruction_pointer = target_quad_index
                    print(f"  Condition is FALSE, jumping to Quad {instruction_pointer}")
                    continue # Skip instruction_pointer increment

            case '+':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                result = val1 + val2
                set_value(result_addr, result)
                print(f"  Addition: {val1} + {val2} = {result}")
            
            case '=': # ASSIGN
                val_to_assign = get_value(operand1_addr)
                set_value(result_addr, val_to_assign)
                print(f"  Assignment: Value {val_to_assign} assigned to address {result_addr}")
            
            case 'GOTO':
                target_quad_index = result_addr
                print(f"  GOTO: Jumping to Quad {target_quad_index}")
                instruction_pointer = target_quad_index
                continue # Skip instruction_pointer increment
            
            case 'PRINT':
                val_to_print = get_value(operand1_addr)
                print(f"  >>> PRINT: {val_to_print}")

            case '-':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                result = val1 - val2
                set_value(result_addr, result)
                print(f"  Subtraction: {val1} - {val2} = {result}")

            case '*':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                result = val1 * val2
                set_value(result_addr, result)
                print(f"  Multiplication: {val1} * {val2} = {result}")

            case '/':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                if val2 == 0:
                    raise ZeroDivisionError(f"Division by zero at quad {instruction_pointer}!")
                result = val1 / val2
                set_value(result_addr, result)
                print(f"  Division: {val1} / {val2} = {result}")
            
            case '>':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                result = val1 > val2
                set_value(result_addr, result)
                print(f"  Comparison: {val1} > {val2} = {result}")
            
            case '!=':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                result = val1 != val2
                set_value(result_addr, result)
                print(f"  Comparison: {val1} != {val2} = {result}")

            case _:
                raise NotImplementedError(f"Operator '{operator}' (Code: {op_code}) not implemented yet.")

        instruction_pointer += 1 # Move to the next quadruple

    print("\n--- Execution Complete ---")
    print("Final Memory State:", vm_memory)
    print(f"Value of 'a' (address 1): {vm_memory.get(1, 'Not set')}")
    print(f"Value of 'c' (address 3): {vm_memory.get(3, 'Not set')}")


# --- 4. Run the Virtual Machine ---
print("Initializing VM...")
run_virtual_machine(quadruples, constants_table, vm_memory, OP_CODES_MAP, MEMORY_MAP)