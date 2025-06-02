from compiler import Compiler
from vm import VirtualMachine

def run_test_case(compiler, source_code, case_name="", export=False, run=False, debug=False):
    print(f"\nTesting: {case_name}")
    print(f"\n{source_code.strip()}")
    compiler.reset()
    vm.reset()
    
    # Set debug mode on VM
    vm.debug_mode = debug
   
    try:
        compiler.compile(source_code)
       
        if debug:
            print("\nStandard Quadruples:")
            for i, quad in enumerate(compiler.quadruples):
                print(f"{i}: {quad}")
               
            print("\nMemory-Based Quadruples:")
            for i, quad in enumerate(compiler.memory_quadruples):
                print(f"{i}: {quad}")
           
        # Export compilation data
        if export:
            compiler.export_compilation_data(f"{case_name.replace(' ', '_')}_data.txt")
            
        if run:
            if not debug:
                print("\nOutput:")
            vm.load_compilation_data(f"{case_name.replace(' ', '_')}_data.txt")
            vm.execute()
       
    except Exception as e:
        print(f"Compilation error: {e}")

compiler = Compiler()
vm = VirtualMachine()

""" run_test_case(compiler, '''
program math;
var a, b : int;
main {
    a = 1;
    b = 2;
              
    print(a + b);
}
end
''', "addition", export=True, run=False)

run_test_case(compiler, '''
program math;
var a, b, c : int;
main {
    a = 1;
    b = 2;
    c = b - a;
              
    print(c);
}
end
''', "subtraction", export=True, run=False)

run_test_case(compiler, '''
program math;
var a, b, c : int;
main {
    a = 1;
    b = 2;
    c = a - b;
              
    print(c);
}
end
''', "subtraction2", export=True, run=False)

run_test_case(compiler, '''
program math;
var a, b, c : int;
main {
    a = 2;
    b = 3;
    c = a * b;
              
    print(c);
}
end
''', "multiplication", export=True, run=False)

run_test_case(compiler, '''
program math;
var a, b : int;
    c : float;
main {
    a = 6;
    b = 3;
    c = a / b;
              
    print(c);
}
end
''', "division", export=True, run=False)

run_test_case(compiler, '''
program math;
var a, b, c : int;
main {
    a = 2;
    b = 3;
    c = 4 + a * b;
              
    print(c);
}
end
''', "precedence1", export=True, run=False)

run_test_case(compiler, '''
program math;
var a, b : int;
    c  : float;
main {
    a = 6;
    b = 3;
    c = a / b + 2;
              
    print(c);
}
end
''', "precedence2", export=True, run=False) """

""" run_test_case(compiler, '''
program twofuncs;
void one() [
    {
        print(1);
    }];
void two() [
    {
        print(2);
    }];
main {
    one();
    two();
}
end
''', "multi_func", export=True, run=False) """

""" run_test_case(compiler, '''
program testwhile;
var a : int;
main {
    a = 6;
    while (a > 0) do {
        a = a-1;
        print(a);
    };
}
end
''', "while_loop", export=True, run=False) """

""" run_test_case(compiler, '''
program testif;
var a : int;
main {
    a = 2;
    if (a != 2) {
        print("If you see this there's an issue");
    };
    if (a > 0) {
        print("Inside second if");
    };
}
end
''', "if_statement", export=True, run=False) """

""" run_test_case(compiler, '''
program testifelse;
var a : int;
main {
    a = 2;
    if (a != 2) {
        print("If you see this there's an issue");
    } else {
        print("Inside else statement");
    };
    print("Back in main");
}
end
''', "if_else", export=True, run=False) """


""" run_test_case(compiler, '''
program parameters;
void add_one(a:int) [
    {
        print(a+1);
    }];
main {
    add_one(1);
}
end
''', "parameters", export=True, run=True) """

""" run_test_case(compiler, '''
program testglobal;
var a : int;
void add_subtract(a:int) [
    var b : int;
    {
        a = a + 2;
        b = 1;
        print(a-b);
    }];
main {
    a = 2;
    add_subtract(a);
}
end
''', "global_and_local", export=True, run=True) """

""" run_test_case(compiler, '''
program equivalence;
var a : int;
main {
    a = 2;
    if (a == 2) {
        print("Hello World!");
    };
}
end
''', "test_equivalence", export=True, run=True) """

""" run_test_case(compiler, '''
program recursion;
void countdown(n : int) [{
    if (n == 0) {
        print("Done!");
    } else {
        print(n);
        countdown(n-1);
    };
}];
main {
    countdown(5);
}
end
''', "recursion", export=True, run=True) """

run_test_case(compiler, '''
program returns;
int adds_one(n : int) [{
    return n+1;
}];
main {
    print(adds_one(3));
}
end
''', "factorial", export=True, run=True)