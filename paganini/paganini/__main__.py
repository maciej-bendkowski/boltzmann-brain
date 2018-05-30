def main(args=None):

    __author__    = "Sergey Dovgal and Maciej Bendkowski"
    __copyright__ = "Copyright (C) 2017 Sergey Dovgal and Maciej Bendkowski"
    __license__   = "Public Domain"
    __version__   = "0.29559774252"

    flag_debug = False

    #
    ##
    ### IMPORTS
    ##
    #

    #-- Stdio manipulations, read, write and stderr
    try:
        import sys
        import os
        import os.path
        import argparse
        from argparse import RawTextHelpFormatter

    except:
        print ("Something went wrong, cannot import package 'sys' or 'os' or 'argparse'")
        exit(1)

    if sys.version_info.major > 2:
        sys.stderr.write('You are using Python 3. Please use Python 2.\n')
        exit(1)

    #-- Better hints at non-installed packages
    list_of_noninstalled_packages = []

    # --- cvxopt is truly necessary
    try:
        import cvxpy
    except:
        list_of_noninstalled_packages += ['cvxpy']

    # --- sympy is an insurance from exponent overflow
    # -- It will be removed for practical purposes and replaced by bounded region
    # feasible subset.
    try:
        import sympy
    except:
        list_of_noninstalled_packages += ['sympy']

    try:
        import numpy as np
    except:
        list_of_noninstalled_packages += ['numpy']

    if len(list_of_noninstalled_packages) > 0:
        sys.stderr.write("""It seems that you need to install some packages.
    Please be patient and type into your command line
        pip2 install """ + ' '.join(list_of_noninstalled_packages) + """
    If you have only Python2 installed, you can also try
        pip install """ + ' '.join(list_of_noninstalled_packages) + """
    Good luck!
    """)
        sys.exit(1)

    ### PRECISION

    np.set_printoptions(precision=14)


    #
    ##
    ###  PARSING COMMAND LINE ARGUMENTS
    ##
    #


    class bcolors:
        HEADER = '\033[95m'
        OKBLUE = '\033[94m'
        OKGREEN = '\033[92m'
        WARNING = '\033[93m'
        FAIL = '\033[91m'
        ENDC = '\033[0m'
        BOLD = '\033[1m'
        UNDERLINE = '\033[4m'

    example_string = bcolors.BOLD + """Example """ + bcolors.ENDC + """

    Consider a system for marking abstractions in lambda-terms:

    L = z L^2 + u z L + D
    D = z + z D

    We want to have 40% of abstractions, so we encode all the variables and
    functions into a single vector [z, u, L, D] and construct input file
    2 1
    0.4
    3
    1 1 1 0
    1 0 2 0
    0 0 0 1
    2
    1 0 0 0
    1 0 0 1
    """

    parser = argparse.ArgumentParser(
            description= bcolors.BOLD + """Welcome to paganini.py! """ +
            bcolors.ENDC,
            epilog = example_string,
            formatter_class=RawTextHelpFormatter)

    parser.add_argument('-i', '--input', dest='input', nargs=1,
            required=False, help="Name of the input file")
    parser.add_argument('-s', '--solver', dest='solver', nargs=1,
            required=False, help="Solver: [CVXOPT, SCS, ECOS]. Default is ECOS.")
    parser.add_argument('-p', '--precision', dest='precision', nargs=1,
            type=float,
            required=False, help="Precision. Defaults to 1e-20.")
    parser.add_argument('-m', '--max-iters', dest='maxiters', nargs=1, type=int,
            required=False, help="Maximum number of iterations.")
    parser.add_argument('-t', '--type', dest='type',
            required=False, help="Type of the grammar: [rational, algebraic]")

    arguments = parser.parse_args()
    sys.stderr.write("Started concerto...\n")

    filename = arguments.input[0] if arguments.input else None
    precision = 1e-20
    if arguments.precision != None:
        try:
            precision = float(arguments.precision[0])
        except:
            raise Exception("Precision should be a float!")
    is_rational = False
    if arguments.type == 'rational':
        sys.stderr.write("System is of rational type\n")
        is_rational = True
    elif arguments.type == 'algebraic':
        is_rational = False
    elif arguments.type != None:
        sys.stderr.write("Type of the grammar not recognized, using algebraic.\n")

    solver = cvxpy.ECOS
    maxiters = 20

    if arguments.solver != None:
        if (arguments.solver[0] == 'CVXOPT'):
            solver = cvxpy.CVXOPT
            maxiters = 20
        elif (arguments.solver[0] == 'SCS'):
            solver = cvxpy.SCS
            maxiters = 2500
        elif (arguments.solver[0] == 'ECOS'):
            solver = cvxpy.ECOS
            maxiters = 20
        else:
            sys.stderr.write("Solver not recognized. Using ECOS by default.\n")

    if arguments.maxiters != None:
        maxiters = int(arguments.maxiters[0])

    if filename and not os.path.isfile(filename):
        raise Exception("File doesn't exist!", filename)

    #
    ##
    ### MAIN CODE
    ##
    #

    # set default error file name
    err_filename = filename if filename else "stdin"

    input_error_string = """
    Input format error in '""" + err_filename + """'!
    Expected:
    <n_fun> <n_var>
    <freq1> <freq2> ... <freq n_var>
    <number of monomials in first definition>
    <exponent1> <exponent2> ... <exponent n_var> - for the first monomial
    <exponent1> <exponent2> ... <exponent n_var> - for the second
    ...
    <number of monomials in second definition>
    ...

    Type python2 paganini.py for help and examples.
    """

    FILE = open(filename,'r') if filename else sys.stdin

    # Read the number of variables and functions

    vec = FILE.next().split()
    assert np.size(vec) >= 2, input_error_string
    try:
        number_of_functions = int(vec[0])
        number_of_variables = int(vec[1])
        total_number_of_variables = number_of_functions + number_of_variables + 1
    except:
        raise Exception(input_error_string)
    assert number_of_functions > 0, '\nThe number of functions should be a positive integer.\n'
    assert number_of_variables >= 0, '\nThe number of functions should be nonnegative integer.\n'

    # Read the frequences
    vec = FILE.next().split()
    assert np.size(vec) >= number_of_variables,\
                '\nThe number of frequences should be equal to number of variables.\n'
    if number_of_variables > 0:
        try:
            freq = map(float, vec)
        except:
            raise Exception(input_error_string)
    else:
        freq = []

    sys.stderr.write("Reading the coefficients... ")
    # Read the coefficients of equations
    coeff_array = []
    for n_equation in xrange(number_of_functions):
        coeff_array += [[]]
        vec = FILE.next().split()
        assert np.size(vec) >= 1,\
                'What is the number of monomials in equation '+ n_equation + '?\n'
        n_monomials = int(vec[0])
        for monomial in xrange(n_monomials):
            vec = FILE.next().split()
            coeff_array[-1] += [map(int, vec)]

    sys.stderr.write("done!\n")

    sys.stderr.write("Composing optimization problem... ")

    z = cvxpy.Variable((number_of_variables + number_of_functions + 1))

    obj = np.array( [[1.0] + freq + [0.0] *  number_of_functions])

    constraints = [
                z[idx + number_of_variables + 1] >=
                cvxpy.log_sum_exp(np.array(coeff_array[idx]) * z)
                for idx in range(number_of_functions)
            ]
    if is_rational:
        constraints += [
            cvxpy.norm(z, 2) <= 40
        ]

    objective = cvxpy.Maximize( obj * z)

    prob = cvxpy.Problem(objective, constraints)

    sys.stderr.write("Done!\n")

    sys.stderr.write("Solving the problem.\n")

    old_stdout = sys.stdout
    sys.stdout = sys.stderr

    if solver != cvxpy.SCS:
        try:
            result = prob.solve(solver=solver, verbose=True, feastol=precision,
                    max_iters=maxiters)
        except prob.status == cvxpy.UNBOUNDED:
            sys.stderr.write("Problem is unbounded\n")
            sys.stderr.write("Probably the set of weights is incorrect")
            exit(1)
        except:
            sys.stderr.write("Solver " + str(solver) + " failed :(")
            exit(1)
    else:
        try:
            result = prob.solve(solver=solver, verbose=True, eps=precision,
                    max_iters=maxiters)
        except:
            sys.stderr.write("Solver " + str(solver) + " failed :(")
            exit(1)

    sys.stderr.write("Solved.\n")

    sys.stdout = old_stdout

    print ('\n'.join([
            str(sympy.exp(expr).evalf())
            for expr in z.value
          ]))

if __name__ == "__main__":
    main()
