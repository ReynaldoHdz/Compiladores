# --- 1. Data Structures (Simulating your .txt file content) ---

# Example Quadruples: (operator, operand1_addr, operand2_addr, result_addr)
# We'll use simple integer addresses for now.
# Quadruple 0: ('+', 100, 101, 200)  -- Adds content at address 100 and 101, stores result at 200
# Quadruple 1: ('ASSIGN', 200, None, 300) -- Assigns content at address 200 to address 300 (which represents 'x')
quadruples = [
    ('+', 100, 101, 200),
    ('ASSIGN', 200, None, 300)
]

# Constants Table: Maps constant addresses to their values
# In a real compiler, these addresses would be assigned during compilation.
constants_table = {
    100: 5,  # Constant 5
    101: 3   # Constant 3
}

# --- 2. Virtual Machine Memory ---

# A simple dictionary to simulate memory for variables and temporaries.
# We'll use addresses for keys and values for content.
# Addresses 200 and higher will be for temporary results and variables.
vm_memory = {}

# --- 3. The Virtual Machine Interpreter ---

def run_virtual_machine(quads, constants, memory):
    instruction_pointer = 0  # Points to the current quadruple to execute
    
    # Simulate a global scope for variables
    # In a real VM, you'd have activation records for functions
    global_variables = {} 

    while instruction_pointer < len(quads):
        quad = quads[instruction_pointer]
        operator = quad[0]
        operand1_addr = quad[1]
        operand2_addr = quad[2]
        result_addr = quad[3]

        print(f"Executing Quadruple {instruction_pointer}: {quad}")

        # Helper function to get the value from an address
        def get_value(addr):
            if addr in constants:
                return constants[addr]
            elif addr in memory:
                return memory[addr]
            elif addr in global_variables: # Check global variables for program variables
                return global_variables[addr]
            else:
                # This indicates an error in address resolution or uninitialized memory
                raise ValueError(f"Address {addr} not found in constants, memory, or global variables.")

        # Helper function to set the value at an address
        def set_value(addr, value):
            if addr >= 300: # Simple heuristic: addresses 300+ are 'program variables'
                global_variables[addr] = value
            else: # Otherwise, it's a temporary result or a constant (though constants shouldn't be written to)
                memory[addr] = value

        # --- The Core Switch-Case Logic (if-elif-else in Python) ---
        match operator:
            case '+':
                val1 = get_value(operand1_addr)
                val2 = get_value(operand2_addr)
                result = val1 + val2
                set_value(result_addr, result)
                print(f"  Result of +: {val1} + {val2} = {result} stored at {result_addr}")
            case 'ASSIGN':
                val = get_value(operand1_addr)
                set_value(result_addr, val)
                print(f"  Result of ASSIGN: {val} assigned to {result_addr}")
            case _:
                raise NotImplementedError(f"Operator '{operator}' not implemented yet.")
            
        # Add more operators here as your language grows!
        # elif operator == '-':
        #     ...
        # elif operator == '*':
        #     ...
        # elif operator == '/':
        #     ...
        # elif operator == 'GOTO': # For control flow
        #     instruction_pointer = result_addr
        #     continue # Skip incrementing IP
        # elif operator == 'GOTOF': # Conditional GOTO
        #     condition = get_value(operand1_addr)
        #     if not condition:
        #         instruction_pointer = result_addr
        #         continue
        # elif operator == 'ERA': # For function calls - activate record
        #     ...
        # elif operator == 'GOSUB': # For function calls - actual call
        #     ...
        # elif operator == 'RETURN':
        #     ...
        # elif operator == 'PARAM':
        #     ...
            

        instruction_pointer += 1 # Move to the next quadruple unless a GOTO was executed

    print("\n--- Execution Complete ---")
    print("Final Global Variables:", global_variables)
    print("Final VM Memory (Temporaries):", vm_memory)

# --- 4. Run the Virtual Machine ---
print("Starting Virtual Machine...")
run_virtual_machine(quadruples, constants_table, vm_memory)

# Expected output:
# Final Global Variables: {300: 8}