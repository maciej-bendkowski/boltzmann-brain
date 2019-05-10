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

    def __pow__(self, n):
        """ Expression exponentiation."""
        assert (isinstance(n, int))

        xs = dict(self._variables)
        for v in self._variables:
            xs[v] *= n

        return Exp(self._mul_coeff, xs)

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

    def __add__(self, other):
        """ Expression addition."""
        return Polynomial(self) + other

    __radd__ = __add__ # make addition commute again

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

class Polynomial:
    """ Class of polonomials of algebraic expressions."""

    def __init__(self, monomials):
        if isinstance(monomials, Exp):
            monomials = [monomials]

        self._monomials = monomials

    def __mul__(self, other):
        " Polynomial multiplication."""
        if isinstance(other, Exp):
            other = Polynomial([other])

        assert isinstance(other, Polynomial)

        outcome = deque()
        for a in self._monomials:
            for b in other._monomials:
                outcome.append(a * b)

        return Polynomial(list(outcome))

    __rmul__ = __mul__ # make multiplication commute again

    def __pow__(self, n):
        """ Naive exponentiation of polynomials."""

        assert isinstance(n, int)
        assert n > 0, "Positive exponent required."

        if n == 1:
            return Polynomial(self._monomials)

        if n % 2 == 1:
            return self * self ** (n - 1)
        else:
            other = self ** (n >> 1)
            return other * other

    def __add__(self, other):
        """ Polynomial addition."""
        if isinstance(other, Exp) or isinstance(other, int):
            other = Polynomial([other])

        assert isinstance(other, Polynomial)
        # FIXME: Consider a representation without monomial duplicates.
        return Polynomial(self._monomials + other._monomials)

    __radd__ = __add__ # make addition commute again

    def __iter__(self):
        return iter(self._monomials)

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
            self.max_iters = 50
            self.feastol   = 1.e-20

class Operator(Enum):
    """ Enumeration of supported constraint signs."""
    LEQ       = 1 # less or equal
    GEQ       = 3 # greater or equal
    UNBOUNDED = 4 # unbounded operator

class Constraint:
    """ Supported constraints for classes such as SEQ or MSET."""
    def __init__(self, operator, value):
        self.operator = operator
        self.value    = value

    @staticmethod
    def normalise(constraint = None):

        if constraint is None:
            return Constraint(Operator.UNBOUNDED, 0)
        else:
            return constraint

def leq(n):
    """ Creates a less or equal constraint for the given input."""
    assert n >= 0, "Negative constraints are not supported."
    return Constraint(Operator.LEQ, n)

def geq(n):
    """ Creates a greater or equal constraint for the given input."""
    assert n >= 0, "Negative constraints are not supported."
    return Constraint(Operator.GEQ, n)

class Specification:
    """ Class representing algebraic combinatorial systems."""

    def __init__(self, truncate = 10):

        self._counter            = 0
        self._equations          = {}

        self._tuning_variables   = {}
        self._type_variable_idxs = set()

        self._msets              = {} # (truncated) MSet equations.
        self._mset_defs          = {} # original MSet expressions.
        self._powers             = {} # accounts for expressions like T(Z^i).

        self.truncate            = truncate
        self._all_variables      = deque()

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
        side expression list (i.e. a list a monomials) or a single expression
        defining the right-hand side sum. Each expression should be either an
        instance of 'Exp' or be a positive integer."""

        self._equations[variable] = Polynomial(expressions)
        self._type_variable(variable)

    def Seq(self, expressions, constraint = None):
        """ Given a list of expressions X or single monomial, introduces to
        the system a new equation which defines a sequence of structures from X.
        The resulting variable corresponding to that class is then returned."""

        expressions = Polynomial(expressions)
        constraint = Constraint.normalise(constraint)

        if constraint.operator == Operator.UNBOUNDED:
            # note: Seq(expr) = 1 + expr * Seq(expr).

            seq = self.variable()
            self.add(seq, 1 + expressions * seq)
            return seq

        if constraint.operator == Operator.LEQ:
            # note: Seq(expr)_{<= k} = 1 + expr + expr^2 + ... + expr^k.
            v = self.variable()
            self.add(v, expressions)

            seq = self.variable()
            xs = list(range(1, constraint.value + 1))
            self.add(seq, 1 + Polynomial(list(map(lambda k: v ** k, xs))))
            return seq

        # constraint.operator == Operator.GEQ
        # note: Seq(expr)_{>= k} = expr^k + expr^{k+1} + ...
        #                        = expr^k (1 + expr^2 + expr^3 + ...)
        #                        = expr^k Seq(expr).

        seq = self.variable() #FIXME
        p = Polynomial(expressions) ** constraint.value
        self.add(seq, p * self.Seq(expressions)) # unbounded
        return seq

    def MSet(self, expressions):
        """ Given a list of expressions X or single monomial, introduces to
        the system a new equation which defines a multiset of structures from X.
        The resulting variable corresponding to that class is then returned."""

        # Note: at the time of definition, not all right-hand side's of
        # corresponding expressions might be present. Therefore we postpone the
        # series composition. For future reference, we safe the original
        # definition at specification level.

        mset = self.variable()
        self._type_variable(mset)
        self._mset_defs[mset] = Polynomial(expressions)
        return mset

    def _power_variable(self, var, d = 1):
        """ Given a variable, say, t = T(Z_1,...,Z_k) outputs a new variable
        representing the dth power of t, i.e. t_i = T(Z_1^d,...,Z_k^d). The
        variable t_i is cached within the specification and its defining
        equation saved."""

        assert d > 0, "Invalid degree parameter d."
        assert self._is_type_variable(var), "Non-type variable."

        if var not in self._powers:
            self._powers[var] = {}

        # check the variable cache.
        if d in self._powers[var]:
            return self._powers[var][d]

        if d == 1 and var in self._equations.keys(): # special case
            self._powers[var][d] = var
            return var

        var_d = self.variable() if d > 1 else var

        self._type_variable(var_d)
        self._powers[var][d] = var_d # memorise var[d]

        if var in self._equations.keys():
            # create respective rhs monomials.
            monomials = self._equations[var]
            exprs = list(map(lambda e : self._power_expr(e, d), monomials))
            self.add(var_d, Polynomial(exprs))
            return var_d
        else:
            # iterate and create respective rhs monomials.
            exprs = self._mset_defs[var]
            self._msets[var_d] = []
            for k in range(d, self.truncate + 1, d):
                self._msets[var_d].append(list(map(lambda e :
                    self._power_expr(e, k), exprs))) # increase d

            return var_d

    def _power_expr(self, expr, d = 1):
        """ Extends _power_variable to expressions."""

        variables = {}
        for idx in expr._variables:
            v = self._all_variables[idx]
            if self._is_type_variable(v):
                # substitute the power variable.
                x = self._power_variable(v, d)
                variables[x._idx] = expr._variables[v._idx]
            else:
                # increase the exponent.
                variables[v._idx] = d * expr._variables[v._idx]

        return Exp(expr._mul_coeff, variables)

    def tune(self, variable, x):
        """ Marks the given variable with the given value."""
        self._tuning_variables[variable] = x

    def _total_variables(self):
        """ Returns the total number of discharged variables."""
        return self._counter

    def _expr_specs(self, expressions):
        """ Given a list (i.e. series) of monomial expressions, creates a
        corresponding sparse matrix thereof."""

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

        # create a sparse representation of the series.
        return sparse.csr_matrix((np.array(data),
            (np.array(row),np.array(col))), shape=(rows,
            self._total_variables()))

    def specs(self):
        """ Computes the sparse matrix specifications
            corresponding to each of the system equations."""

        matrices = deque()
        for expressions in self._equations.values():
            matrix = self._expr_specs(expressions)
            matrices.append(matrix)
        return matrices

    def _type_variable(self, variable):
        """ Marks a type variable."""
        self._type_variable_idxs.add(variable._idx)

    def _is_type_variable(self, variable):
        return variable._idx in self._type_variable_idxs

    def check_type(self):
        """ Checks if the system is algebraic or rational."""

        if len(self._mset_defs) > 0:
            return Type.ALGEBRAIC

        for expressions in self._equations.values():
            for exp in expressions:
                if isinstance(exp, Exp):
                    for v, e in exp._variables.items():
                        if v in self._type_variable_idxs and e > 1: #FIXME
                            return Type.ALGEBRAIC

        return Type.RATIONAL

    def _init_params(self, params = None):
        if params is None:
            # some defaults
            sys_type = self.check_type()
            return Params(sys_type)
        else:
            return params

    def _construct_truncated_msets(self):
        """ Assuming that the system is already defined, constructs a truncated
        series representation for each of the MSet variables in the
        specification."""

        for mset in self._mset_defs:
            self._power_variable(mset,1)

    def _compose_constraints(self, var):
        assert len(self._equations) > 0, "System without equations."
        matrices = self.specs()
        constraints = deque()

        # compose regular type variable constraints.
        for idx, eq_variable in enumerate(self._equations):
            log_exp = matrices[idx]
            tidx = eq_variable._idx
            constraints.append(var[tidx] >= cvxpy.log_sum_exp(log_exp * var))

        # compose MSet variable constraints.
        for v in self._msets:
            expressions = self._msets[v]
            xs = [1/(i+1) * cvxpy.exp(cvxpy.sum(self._expr_specs(e) * var))
                    for i, e in enumerate(expressions)]

            constraints.append(var[v._idx] >= cvxpy.sum(xs))

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

        assert self._total_variables() > 0, "System without variables."
        assert len(self._equations) > 0, "System without equations."
        assert len(self._tuning_variables) > 0,\
            "The given system has no tuned variables."

        self._construct_truncated_msets() # note: might generate variables.
        n = self._total_variables()
        var = cvxpy.Variable(n)

        # compose the constraints
        constraints = self._compose_constraints(var)

        n = self._total_variables()

        # compose the objective
        obj = np.zeros(n)
        obj[t._idx] = 1.0
        for (v, x) in self._tuning_variables.items():
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

        assert self._total_variables() > 0, "System without variables."
        assert len(self._equations) > 0, "System without equations."

        self._construct_truncated_msets() # note: might generate variables.
        n = self._total_variables()
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

        for (v, x) in self._tuning_variables.items():
            obj[v._idx] = x

        objective = cvxpy.Maximize(obj * var)
        problem   = cvxpy.Problem(objective, constraints)

        return self._run_solver(var, problem, params)
