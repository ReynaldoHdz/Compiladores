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
            '+': 1, '-': 2, '*': 3, '/': 4,
            '<': 5, '>': 6, '=': 7, '!=': 8,
            'GOTO': 9, 'GOTOF': 10, 'PRINT': 11,
            'ERA': 12, 'PARAM': 13, 'GOSUB': 14,
            'ENDF': 15, 'RETURN': 16
        }

        # Invert OPERATIONS for easier lookup
        self.OP_CODES_MAP = {v: k for k, v in self.OPERATIONS.items()}
        
        # Initialize VM memory
        self.vm_memory = {}
        self.function_directory = {}
        self.constants_table = {}
        self.quadruples = []

    def load_compilation_data(self, filename):
        """Load function directory, constants and quadruples from compilation data file"""
        current_section = None
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
                elif current_section == "CONST" and line != "END_CONSTANTS":
                    type_name, value, address = line.split(',')
                    value = int(value) if type_name == 'int' else float(value)
                    self.constants_table[int(address)] = value
                elif current_section == "QUAD" and line != "END_QUADRUPLES":
                    idx, op, arg1, arg2, result = line.split(',')
                    arg1 = int(arg1) if arg1.isdigit() else arg1
                    arg2 = int(arg2) if arg2.isdigit() else arg2
                    result = int(result) if result.isdigit() else result
                    self.quadruples.append((int(op), arg1, arg2, result))

        print("\nFunction Directory:", self.function_directory)
        print("Constants Table:", self.constants_table)
        print("Quadruples:", self.quadruples)

    def get_address_info(self, addr):
        if addr is None:
            return None, None
        for segment, types in self.MEMORY_MAP.items():
            for data_type, (start, end) in types.items():
                if start <= addr <= end:
                    return segment, data_type
        return None, None

    def get_value(self, addr):
        if addr is None:
            return None

        segment, data_type = self.get_address_info(addr)
        
        if segment == 'constant':
            if addr not in self.constants_table:
                raise ValueError(f"Constant address {addr} not found in constants table.")
            return self.constants_table[addr]
        elif segment in ['global', 'local', 'temp']:
            if addr not in self.vm_memory:
                print(f"  Warning: Accessing uninitialized address {addr}")
            return self.vm_memory.get(addr)
        else:
            raise ValueError(f"Address {addr} does not belong to any defined memory segment.")

    def set_value(self, addr, value):
        if addr is None:
            print("  Warning: Attempted to set value to None address. Skipping.")
            return

        segment, data_type = self.get_address_info(addr)
        
        if segment == 'constant':
            raise ValueError(f"Attempted to write to constant address {addr}.")
        elif segment in ['global', 'local', 'temp']:
            if data_type == 'int' and not isinstance(value, int):
                value = int(value) if isinstance(value, float) else value
            elif data_type == 'float' and not isinstance(value, (int, float)):
                value = float(value) if isinstance(value, int) else value
            elif data_type == 'bool' and not isinstance(value, bool):
                raise TypeError(f"Type mismatch: Expected bool at address {addr}")

            self.vm_memory[addr] = value
            print(f"  --> Set memory[{addr}] (segment: {segment}, type: {data_type}) = {value}")
        else:
            raise ValueError(f"Cannot set value for address {addr} in unknown segment.")

    def execute(self):
        """Execute the loaded quadruples"""
        print("--- Starting Virtual Machine Execution ---")
        instruction_pointer = 0
        
        while instruction_pointer < len(self.quadruples):
            quad = self.quadruples[instruction_pointer]
            op_code = quad[0]
            operand1_addr = quad[1]
            operand2_addr = quad[2]
            result_addr = quad[3]

            operator = self.OP_CODES_MAP.get(op_code, f"UNKNOWN_OP_{op_code}")
            print(f"\nExecuting Quad {instruction_pointer}: {quad} (Op: {operator})")

            # Your existing match-case block here
            match operator:
                case '<':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    # Ensure types are comparable if needed, e.g., both int or both float
                    result = val1 < val2
                    self.set_value(result_addr, result)
                    print(f"  Comparison: {val1} < {val2} = {result}")

                case 'GOTOF':
                    condition = self.get_value(operand1_addr)
                    target_quad_index = result_addr
                    print(f"  GOTOF: Condition ({operand1_addr}) = {condition}, Target = {target_quad_index}")
                    if not condition:
                        instruction_pointer = target_quad_index
                        print(f"  Condition is FALSE, jumping to Quad {instruction_pointer}")
                        continue # Skip instruction_pointer increment

                case '+':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 + val2
                    self.set_value(result_addr, result)
                    print(f"  Addition: {val1} + {val2} = {result}")
                
                case '=': # ASSIGN
                    val_to_assign = self.get_value(operand1_addr)
                    self.set_value(result_addr, val_to_assign)
                    print(f"  Assignment: Value {val_to_assign} assigned to address {result_addr}")
                
                case 'GOTO':
                    target_quad_index = result_addr
                    print(f"  GOTO: Jumping to Quad {target_quad_index}")
                    instruction_pointer = target_quad_index
                    continue # Skip instruction_pointer increment
                
                case 'PRINT':
                    val_to_print = self.get_value(result_addr)
                    print(f"  >>> PRINT: {val_to_print}")

                case '-':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 - val2
                    self.set_value(result_addr, result)
                    print(f"  Subtraction: {val1} - {val2} = {result}")

                case '*':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 * val2
                    self.set_value(result_addr, result)
                    print(f"  Multiplication: {val1} * {val2} = {result}")

                case '/':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    if val2 == 0:
                        raise ZeroDivisionError(f"Division by zero at quad {instruction_pointer}!")
                    result = val1 / val2
                    self.set_value(result_addr, result)
                    print(f"  Division: {val1} / {val2} = {result}")
                
                case '>':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 > val2
                    self.set_value(result_addr, result)
                    print(f"  Comparison: {val1} > {val2} = {result}")
                
                case '!=':
                    val1 = self.get_value(operand1_addr)
                    val2 = self.get_value(operand2_addr)
                    result = val1 != val2
                    self.set_value(result_addr, result)
                    print(f"  Comparison: {val1} != {val2} = {result}")

                case _:
                    raise NotImplementedError(f"Operator '{operator}' (Code: {op_code}) not implemented yet.")

            instruction_pointer += 1

        print("\n--- Execution Complete ---")
        print("Final Memory State:", self.vm_memory)

    def reset(self):
        """Reset the virtual machine state"""
        # Clear all memory
        self.vm_memory = {}
        self.function_directory = {}
        self.constants_table = {}
        self.quadruples = []
        
        # Clear any activation records or other execution state
        self.activation_stack = []
        self.global_memory = {}

# Example usage:
if __name__ == "__main__":
    vm = VirtualMachine()
    vm.load_compilation_data("test1_data.txt")
    vm.execute()