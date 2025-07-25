class ActivationRecord:
    """Represents a function's activation record on the call stack"""
    def __init__(self, function_name, return_address):
        self.function_name = function_name
        self.return_address = return_address
        self.local_memory = {}  # Local variables and temporaries
        self.param_values = []  # Parameters passed to this function

class VirtualMachine:
    def __init__(self):
        # Memory segment boundaries
        self.MEMORY_MAP = {
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
        self.OPERATIONS = {
            '+': 1,
            '-': 2,
            '*': 3,
            '/': 4,
            '<': 5,
            '>': 6,
            '=': 7,
            '!=': 8,
            '==': 9,
            'GOTO': 10,
            'GOTOF': 11,
            'PRINT': 12,
            'ERA': 13,
            'PARAM': 14,
            'GOSUB': 15,
            'ENDF': 16,
            'RETURN': 17
        }

        # Invert OPERATIONS for easier lookup
        self.OP_CODES_MAP = {v: k for k, v in self.OPERATIONS.items()}
        
        # Initialize VM memory
        self.vm_memory = {}
        self.function_directory = {}
        self.constants_table = {}
        self.quadruples = []

        self.activation_stack = []
        self.current_activation = None
        self.param_stack = []

        self.debug_mode = False

    def load_compilation_data(self, filename):
        """Load function directory, constants and quadruples from compilation data file"""
        current_section = None
        current_func = None
        
        with open(filename, 'r') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                    
                if line == "FUNCTION_DIRECTORY":
                    current_section = "FUNC"
                elif line == "CONSTANTS":
                    current_section = "CONST"
                elif line == "QUADRUPLES":
                    current_section = "QUAD"
                elif line in ["=================", "=========", "=========="]:
                    continue
                elif current_section == "FUNC":
                    if line.startswith("FUNC"):
                        current_func = line.split()[1]
                        self.function_directory[current_func] = {}
                    elif line.startswith("type:"):
                        self.function_directory[current_func]['type'] = line.split()[1]
                    elif line.startswith("start:"):
                        self.function_directory[current_func]['start'] = int(line.split()[1])
                    elif line.startswith("params:"):
                        self.function_directory[current_func]['params'] = int(line.split()[1])
                    elif line.startswith("vars:"):
                        self.function_directory[current_func]['vars'] = int(line.split()[1])
                    elif line == "END_FUNC":
                        current_func = None
                elif current_section == "CONST" and line != "END_CONSTANTS":
                    parts = line.split(',')
                    type_name = parts[0]
                    if type_name == 'string':
                        # String values might contain commas, so rejoin the rest
                        value = ','.join(parts[1:-1])
                        address = int(parts[-1])
                    else:
                        value = int(parts[1]) if type_name == 'int' else float(parts[1])
                        address = int(parts[2])
                    self.constants_table[address] = value
                elif current_section == "QUAD" and line != "END_QUADRUPLES":
                    parts = line.split(',', 4)  # Split into max 5 parts
                    idx, op, arg1, arg2, result = parts
                    
                    # Convert numeric strings to integers, keep function names as strings
                    arg1 = int(arg1) if arg1.isdigit() or (arg1.startswith('-') and arg1[1:].isdigit()) else arg1
                    arg2 = int(arg2) if arg2.isdigit() or (arg2.startswith('-') and arg2[1:].isdigit()) else arg2
                    result = int(result) if result.isdigit() or (result.startswith('-') and result[1:].isdigit()) else result
                    
                    self.quadruples.append((int(op), arg1, arg2, result))

        if self.debug_mode:
            print("\nFunction Directory:", self.function_directory)
            print("Constants Table:", self.constants_table)
            print("Quadruples:", self.quadruples)

    def debug_print(self, message):
        """Print debug message only if debug mode is on"""
        if self.debug_mode:
            print(message)

    def get_address_info(self, addr):
        if addr is None:
            return None, None
        for segment, types in self.MEMORY_MAP.items():
            for data_type, (start, end) in types.items():
                if start <= addr <= end:
                    return segment, data_type
        return None, None

    def get_value(self, addr):
        """Get value with activation record awareness"""
        if addr is None:
            return None

        segment, data_type = self.get_address_info(addr)
        
        if segment == 'constant':
            if addr not in self.constants_table:
                raise ValueError(f"Constant address {addr} not found in constants table.")
            return self.constants_table[addr]
        elif segment == 'global':
            # Global variables are always in vm_memory
            if addr not in self.vm_memory:
                self.debug_print(f"  Warning: Accessing uninitialized global address {addr}")
            return self.vm_memory.get(addr)
        elif segment in ['local', 'temp']:
            # Local and temp variables check current activation record first
            if self.current_activation and addr in self.current_activation.local_memory:
                return self.current_activation.local_memory[addr]
            # Fallback to global memory (for main function)
            if addr not in self.vm_memory:
                self.debug_print(f"  Warning: Accessing uninitialized address {addr}")
            return self.vm_memory.get(addr)
        else:
            raise ValueError(f"Address {addr} does not belong to any defined memory segment.")

    def set_value(self, addr, value):
        """Set value with activation record awareness"""
        if addr is None:
            self.debug_print("  Warning: Attempted to set value to None address. Skipping.")
            return

        segment, data_type = self.get_address_info(addr)
        
        if segment == 'constant':
            raise ValueError(f"Attempted to write to constant address {addr}.")
        elif segment == 'global':
            # Global variables always go to vm_memory
            self.vm_memory[addr] = value
            self.debug_print(f"  --> Set memory[{addr}] (segment: {segment}, type: {data_type}) = {value}")
        elif segment in ['local', 'temp']:
            # Local and temp variables go to current activation record if it exists
            if self.current_activation:
                self.current_activation.local_memory[addr] = value
                self.debug_print(f"  --> Set activation[{self.current_activation.function_name}].memory[{addr}] (segment: {segment}, type: {data_type}) = {value}")
            else:
                # Fallback to global memory (for main function)
                self.vm_memory[addr] = value
                self.debug_print(f"  --> Set memory[{addr}] (segment: {segment}, type: {data_type}) = {value}")
        else:
            raise ValueError(f"Cannot set value for address {addr} in unknown segment.")

    def execute(self):
        """Execute the loaded quadruples"""
        self.debug_print("--- Starting Virtual Machine Execution ---")
        instruction_pointer = 0
        
        while instruction_pointer < len(self.quadruples):
            quad = self.quadruples[instruction_pointer]
            op_code = quad[0]
            operand1_addr = quad[1]
            operand2_addr = quad[2]
            result_addr = quad[3]

            operator = self.OP_CODES_MAP.get(op_code, f"UNKNOWN_OP_{op_code}")
            self.debug_print(f"\nExecuting Quad {instruction_pointer}: {quad} (Op: {operator})")

            match operator:
                case 'ERA':
                    func_name = operand1_addr
                    self.debug_print(f"  ERA: Preparing activation record for function '{func_name}'")
                    self.param_stack = []
                
                case 'PARAM':
                    param_value = self.get_value(operand1_addr)
                    param_index = result_addr
                    self.debug_print(f"  PARAM: Pushing parameter {param_index} with value {param_value}")
                    self.param_stack.append(param_value)
                
                case 'GOSUB':
                    func_name = operand1_addr
                    self.debug_print(f"  GOSUB: Calling function '{func_name}'")
                    
                    if func_name not in self.function_directory:
                        raise ValueError(f"Function '{func_name}' not found in function directory")
                    
                    func_info = self.function_directory[func_name]
                    
                    new_activation = ActivationRecord(func_name, instruction_pointer + 1)
                    new_activation.param_values = self.param_stack.copy()
                    
                    if self.current_activation:
                        self.activation_stack.append(self.current_activation)
                    
                    self.current_activation = new_activation
                    
                    param_count = func_info.get('params', 0)
                    for i in range(param_count):
                        if i < len(self.param_stack):
                            param_addr = 101 + i
                            self.current_activation.local_memory[param_addr] = self.param_stack[i]
                            self.debug_print(f"  --> Assigned parameter {i} (value: {self.param_stack[i]}) to address {param_addr}")

                    instruction_pointer = func_info['start']
                    self.debug_print(f"  --> Jumping to function start at quad {instruction_pointer}")
                    self.debug_print(f"  --> Activation stack depth: {len(self.activation_stack) + 1}")
                    continue
                
                case 'ENDF':
                    self.debug_print(f"  ENDF: Returning from function")
                    
                    if self.current_activation:
                        return_address = self.current_activation.return_address
                        self.debug_print(f"  --> Returning to quad {return_address}")
                        
                        if self.activation_stack:
                            self.current_activation = self.activation_stack.pop()
                            self.debug_print(f"  --> Restored activation for '{self.current_activation.function_name}'")
                        else:
                            self.current_activation = None
                            self.debug_print(f"  --> Back to main program")
                        
                        instruction_pointer = return_address
                        continue
                    else:
                        self.debug_print("  --> End of main program")
                        break
                
                case 'GOTO':
                    if operand1_addr == 'main':
                        target_quad_index = self.function_directory['main']['start']
                        self.debug_print(f"  GOTO: Jumping to main at quad {target_quad_index}")
                    else:
                        target_quad_index = result_addr
                        self.debug_print(f"  GOTO: Jumping to quad {target_quad_index}")
                    instruction_pointer = target_quad_index
                    continue
    
                case '<':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 < val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Comparison: {val1} < {val2} = {result}")
                
                case '==':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 == val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Comparison: {val1} == {val2} = {result}")

                case 'GOTOF':
                    condition = self.get_value(operand1_addr)
                    target_quad_index = result_addr
                    self.debug_print(f"  GOTOF: Condition ({operand1_addr}) = {condition}, Target = {target_quad_index}")
                    if not condition:
                        instruction_pointer = target_quad_index
                        self.debug_print(f"  Condition is FALSE, jumping to Quad {instruction_pointer}")
                        continue

                case '+':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 + val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Addition: {val1} + {val2} = {result}")
                
                case '=':
                    val_to_assign = self.get_value(operand1_addr)
                    self.set_value(result_addr, val_to_assign)
                    self.debug_print(f"  Assignment: Value {val_to_assign} assigned to address {result_addr}")
                
                case 'PRINT':
                    val_to_print = self.get_value(result_addr)
                    # Always print the output, regardless of debug mode
                    print(f"{val_to_print}")

                case '-':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 - val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Subtraction: {val1} - {val2} = {result}")

                case '*':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 * val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Multiplication: {val1} * {val2} = {result}")

                case '/':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    if val2 == 0:
                        raise ZeroDivisionError(f"Division by zero at quad {instruction_pointer}!")
                    result = val1 / val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Division: {val1} / {val2} = {result}")
                
                case '>':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 > val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Comparison: {val1} > {val2} = {result}")
                
                case '!=':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 != val2
                    self.set_value(result_addr, result)
                    self.debug_print(f"  Comparison: {val1} != {val2} = {result}")

                case _:
                    raise NotImplementedError(f"Operator '{operator}' (Code: {op_code}) not implemented yet.")

            instruction_pointer += 1

        self.debug_print("\n--- Execution Complete ---")
        if self.debug_mode:
            print("Final Memory State:", self.vm_memory)
            if self.current_activation:
                print("Current Activation Memory:", self.current_activation.local_memory)

    def reset(self):
        """Reset the virtual machine state"""
        # Clear all memory
        self.vm_memory = {}
        self.function_directory = {}
        self.constants_table = {}
        self.quadruples = []
        self.activation_stack = []
        self.current_activation = None
        self.param_stack = []
        
        # Clear any activation records or other execution state
        self.activation_stack = []
        self.global_memory = {}

# Example usage:
if __name__ == "__main__":
    vm = VirtualMachine()
    vm.load_compilation_data("test1_data.txt")
    vm.execute()