def main(args=None):

    __author__    = "Sergey Dovgal"
    __copyright__ = "Copyright (C) 2017 Sergey Dovgal"
    __license__   = "Public Domain"
    __version__   = "0.2955977424"

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
    except:
        print ("Something went wrong, cannot import package 'sys' or 'os'")
        exit(1)

    """The main routine."""
    if args is None:
        args = sys.argv[1:]
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

    # --- technical inclusions
    from cvxopt import matrix
    from numpy import log, exp
    np.set_printoptions(precision=14)

    sys.stderr.write("Started concerto...\n")

    #
    ## ERROR MESSAGES
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

    welcome_message = bcolors.BOLD + """Welcome to paganini.py!
    """ + bcolors.ENDC

    usage_message = bcolors.UNDERLINE + """
    usage:""" + bcolors.ENDC + """ python2 paganini.py input.txt 1e-6
    """ + bcolors.UNDERLINE + """usage:""" + bcolors.ENDC + """ python2 paganini.py input.txt CVXOPT
    """ + bcolors.UNDERLINE + """usage:""" + bcolors.ENDC + """ python2 paganini.py input.txt SCS
    """ + bcolors.UNDERLINE + """usage:""" + bcolors.ENDC + """ python2 paganini.py input.txt ECOS

    [*] input.txt is the name of the input file
    with coefficients of algebraic
    specifications
    [*] 1e-6 is a float number corresponding to precision
    [*] [CVXOPT, SCS, ECOS] stand for different convex optimization solvers.
    ECOS is more preferrable for algebraic systems, SCS for rational.

    =======
    Example

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

    #
    ##
    ###  PARSING COMMAND LINE ARGUMENTS
    ##
    #

    if not flag_debug:
        if len(sys.argv) < 3:
            print (welcome_message + usage_message)
            quit()

        filename = sys.argv[1]

        try:
            precision = float(sys.argv[2])
        except:
            raise Exception("Precision should be a float!")

        import os.path
        if not os.path.isfile(filename):
            raise Exception("File doesn't exist!", filename)

        solver = cvxpy.ECOS
        second_solver = cvxpy.SCS
        iters = 5000

        if len(sys.argv) >= 4:
            if (sys.argv[3] == 'CVXOPT'):
                solver = cvxpy.CVXOPT
            elif (sys.argv[3] == 'SCS'):
                solver = cvxpy.SCS
                second_solver = cvxpy.ECOS
            elif (sys.argv[3] == 'ECOS'):
                solver = cvxpy.ECOS
            else:
                sys.stderr.write("Solver not recognized. Using ECOS by default.\n")
    else:
        # I specify filename by hand in debug regime
        filename = 'input.txt'
        precision = 1e-10

    #
    ##
    ### MAIN CODE
    ##
    #

    input_error_string = """
    Input format error in '""" + filename + """'!
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

    FILE = open(filename,'r')

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
            ] + [
                cvxpy.norm(z, 2) <= 60
            ]

    objective = cvxpy.Maximize( obj * z)

    prob = cvxpy.Problem(objective, constraints)

    sys.stderr.write("Done!\n")

    sys.stderr.write("Solving the problem.\n")

    old_stdout = sys.stdout
    sys.stdout = sys.stderr

    if solver == cvxpy.SCS:
        try:
            result = prob.solve(solver=solver, verbose=True, eps=precision, max_iters = iters)
        except:
            result = prob.solve(solver=second_solver, verbose=True, feastol=precision)
    else:
        try:
            result = prob.solve(solver=solver, verbose=True, feastol=precision)
        except:
            result = prob.solve(solver=solver, verbose=True, eps=precision)

    sys.stderr.write("Solved.\n")

    sys.stdout = old_stdout

    print ('\n'.join([
            str(sympy.exp(expr).evalf())
            for expr in z.value
          ]))

if __name__ == "__main__":
    main()
