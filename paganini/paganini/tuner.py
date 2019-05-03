import cvxpy
import sympy
import numpy as np

from scipy import sparse

from enum import Enum
from collections import Counter
from collections import deque

class Exp:
    """ Class of algebraic expressions used in an algebraic equation.  An
    expression is represented using a multiplicative coefficient (constant) and
    a map of variables to their respective non-negative exponents."""

    def __init__(self, mul_coeff = 1, variables = {}):
        self._mul_coeff = mul_coeff
        self._variables = variables

    def __mul__(self, other):
        """ Multiplication of algebraic expressions."""
        if isinstance(other, int):
            # multiplication with a constant.
            return Exp(self._mul_coeff * other, self._variables)
        else:
            assert (isinstance(other, Exp))
            # multiplication with another expression.
            x, y = Counter(self._variables), Counter(other._variables)
            return Exp(self._mul_coeff * other._mul_coeff, dict(x + y))

    __rmul__ = __mul__ # make multiplication commute again

    def spec(self):
        """ Returns a deque of pairs (partially) describing the expression. The
        first component denotes a variable, whereas the second one denotes its
        respective exponent. The represented expression (in fact monomial) can
        be recovered by multiplying all of the variables with their exponents
        and the multiplicative coefficient."""
        assert len(self._variables) > 0, "Expression without variables."

        data = deque()
        for var, e in self._variables.items():
            data.append((var, e))
        return data

class Variable(Exp):
    """ Class of variables (i.e. also expressions)."""

    def __init__(self, idx):
        self._idx = idx
        super(Variable,self).__init__(1,{})
        self._variables[idx] = 1

        # tuning value
        self.value = None

    def __pow__(self, n):
        """ Exponentiation of variables."""

        assert (isinstance(n, int))
        xs = dict(self._variables)
        xs[self._idx] = n
        return Exp(self._mul_coeff, xs)

class Eq:

    def __init__(self, variable, monomials):
        self._variable  = variable
        self._monomials = monomials

class Type(Enum):
    """ Enumeration of supported system types."""
    ALGEBRAIC = 1
    RATIONAL  = 2

class Params:
    """ CVXPY solver parameters, initalised with some defaults."""

    def __init__(self, sys_type):
        self.verbose   = True
        if sys_type == Type.RATIONAL:
            self.sys_type  = Type.RATIONAL
            self.solver    = cvxpy.SCS
            self.max_iters = 2500
            self.eps       = 1.e-20
            self.norm      = 40
        else:
            self.sys_type  = Type.ALGEBRAIC
            self.solver    = cvxpy.ECOS
            self.max_iters = 25
            self.feastol   = 1.e-20

class Specification:
    """ Class representing algebraic combinatorial systems."""

    def __init__(self):
        self._counter          = 0
        self._equations        = deque()
        self._tuning_variables = deque()
        self._all_variables    = deque()

    def variable(self, x = None):
        """ Discharges a new, fresh variable. If 'x' is given, the returned
        variable is also marked, see Specification.tune."""

        idx = self._counter
        self._counter += 1

        var = Variable(idx) # safe for future reference
        self._all_variables.append(var)

        if x is not None:
            self.tune(var, x)

        return var

    def variables(self, xs = None):
        """ Returns an infinite variable generator. Given a non-empty list of
        marking values, decorates the initial variable prefix with respective
        values of xs, see Specification.tune."""

        if xs is not None:
            for x in xs:
                yield self.variable(x)

        while True:
             yield self.variable()

    def add(self, variable, expressions):
        """ Includes the given equation in the system. Note that each equation
        consists of a left-hand side variable and a corresponding right-hand
        side expression list (i.e. a list a monomials comprising the right-hand
        side sum. Each expression should be either an instance of 'Exp' or be a
        positive integer."""

        self._equations.append((variable,expressions))

    def Seq(self, expressions):
        """ Given a list of expressions X (or single expression), introduces to
        the system a new equation which defines a sequence of structures from X.
        The resulting variable corresponding to that class is then returned."""

        if isinstance(expressions, Exp):
            expressions = [expressions]

        seq = self.variable()
        exprs = list(map(lambda expr: expr * seq, expressions))
        self.add(seq, [1] + exprs)
        return seq

    def tune(self, variable, x):
        """ Marks the given variable with the given value."""
        self._tuning_variables.append((variable, x))

    def _total_variables(self):
        """ Returns the total number of discharged variables."""
        return self._counter

    def specs(self):
        """ Computes the sparse matrix specifications corresponding to each of
        the system equations."""
        matrices = deque()
        for (_, expressions) in self._equations:

            rows = 0 # row counter
            row, col, data = deque(), deque(), deque()
            for exp in expressions:

                if isinstance(exp, Exp):
                    for _ in range(exp._mul_coeff):
                        for (v, e) in exp.spec():
                            row.append(rows)
                            col.append(v)
                            data.append(e)
                        rows += 1
                else:
                    rows += exp

            # create a sparse representation of the equation
            matrix = sparse.csr_matrix((np.array(data),
                (np.array(row),np.array(col))), shape=(rows,
                self._total_variables()))

            matrices.append(matrix)
        return matrices

    def _type_variables(self):
        return [v[0]._idx for v in self._equations]

    def check_type(self):
        """ Checks if the system is algebraic or rational."""

        ts = self._type_variables()
        for (_, expressions) in self._equations:
            for exp in expressions:
                if isinstance(exp, Exp):
                    for v, e in exp._variables.items():
                        if v in ts and e > 1:
                            return Type.ALGEBRAIC

        return Type.RATIONAL

    def _init_params(self, params = None):
        if params is None:
            # some defaults
            sys_type = self.check_type()
            return Params(sys_type)
        else:
            return params

    def _compose_constraints(self, var):
        assert len(self._equations) > 0, "System without equations."
        matrices = self.specs()
        constraints = deque()

        for idx, (eq_variable, _) in enumerate(self._equations):
            log_exp = matrices[idx]
            tidx = eq_variable._idx
            constraints.append(var[tidx] >= cvxpy.log_sum_exp(log_exp * var))

        return constraints

    def _run_solver(self, var, problem, params):

        if params.sys_type == Type.RATIONAL:
            solution = problem.solve(solver = params.solver, verbose =
                    params.verbose, eps = params.eps, max_iters =
                    params.max_iters)
        else:
            solution = problem.solve(solver = params.solver, verbose =
                    params.verbose, feastol = params.feastol, max_iters =
                    params.max_iters)

        # decorate system variables
        for idx, expr in enumerate(var.value):
            self._all_variables[idx].value = sympy.exp(expr).evalf()

        return solution

    def run_tuner(self, t, params = None):
        """ Given the type variable and a set of tuning parameters, composes a
        (tuning) optimisation problem corresponding to an approximate sampler
        meant for structures of the given type. Variables are tuned so to
        achieve (in expectation) the marked variable values.  Consider the
        following example:

        sp = Specification()
        z, u, M = sp.variable(1000), sp.variable(200), sp.variable()
        sp.add(M, [z, u * z * M, z * M **2])

        params = Params(Type.ALGEBRAIC)
        sp.run_tuner(M, params)

        Here, the variables z and u are marked with *absolute* values 1000 and
        200, respectively. The input type represents the type of Motzkin trees,
        i.e. unary-binary plane trees. Variable z marks their size, whereas u
        marks the occurrences of unary nodes. The tuning goal is to obtain
        specific values of z, u, and M, such that the induced branching
        probabilities lead to a sampler which generates Motzkin trees of size
        1000 with around 200 unary nodes (both in expectation).

        Respective variables (including type variables) are decorated with a
        proper 'value'. The method returns the CVXPY solution (i.e. the optimal
        value for the problem, or a string indicating why the problem could not
        be solved)."""

        params = self._init_params(params)
        n = self._total_variables()

        assert n > 0, "System without variables."
        assert len(self._equations) > 0, "System without equations."

        var = cvxpy.Variable(n)

        # compose the constraints
        constraints = self._compose_constraints(var)

        # compose the objective
        obj = np.zeros(n)
        obj[t._idx] = 1.0
        for (v, x) in self._tuning_variables:
            obj[v._idx] = -x

        objective = cvxpy.Minimize(obj * var)
        problem   = cvxpy.Problem(objective, constraints)

        return self._run_solver(var, problem, params)

    def run_singular_tuner(self, z, params = None):
        """ Given a (size) variable and a set of tuning parameters, composes an
        optimisation problem corresponding to an approximate sampler meant for
        structures of the given type. Variables are tuned so to achieve (in
        expectation) the marked variable frequencies.

        Consider the following example:

        sp = Specification()
        z, u, M = sp.variable(), sp.variable(0.4), sp.variable()
        sp.add(M, [z, u * z * M, z * M **2])

        params = Params(Type.ALGEBRAIC)
        sp.run_singular_tuner(z, params)

        Here, the variable u is marked with a *frequency* 0.4.  The type M
        represents the type of Motzkin trees, i.e. unary-binary plane trees.
        Variable z marks their size, whereas u marks the occurrences of unary
        nodes. The tuning goal is to obtain specific values of z, u, and M, such
        that the induced branching probabilities lead to a sampler which
        generates, in expectation, Motzkin trees of infinite (i.e. unbounded)
        size and around 40% of unary nodes.

        Respective variables (including type variables) are decorated with a
        proper 'value'. The method returns the CVXPY solution (i.e. the optimal
        value for the problem, or a string indicating why the problem could not
        be solved)."""

        params = self._init_params(params)
        n = self._total_variables()

        assert n > 0, "System without variables."
        assert len(self._equations) > 0, "System without equations."

        var = cvxpy.Variable(n)

        # compose the constraints
        constraints = self._compose_constraints(var)

        if params.sys_type == Type.RATIONAL:
            # for rational systems the optimisation problem becomes unbounded,
            # hence we need to artificially bound the vector norm.
            constraints.append(cvxpy.norm(var,2) <= params.norm)

        # compose the objective
        obj = np.zeros(n)
        obj[z._idx] = 1.0

        for (v, x) in self._tuning_variables:
            obj[v._idx] = x

        objective = cvxpy.Maximize(obj * var)
        problem   = cvxpy.Problem(objective, constraints)

        return self._run_solver(var, problem, params)

# if __name__ == "__main__":

    # spec = Specification()
    # z, T = spec.variable(), spec.variable()
    # Ts   = spec.Seq(T)

    # spec.add(T, [z * Ts])

    # spec.run_singular_tuner(z)
    # print(z.value, T.value)
