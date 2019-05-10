import unittest
from tuner import *

class TestTuner(unittest.TestCase):

    def test_singular_btrees(self):
        """ Singular tuning of binary trees
            B = 1 + Z * B^2."""

        spec = Specification()
        z, B = spec.variable(), spec.variable()
        spec.add(B, 1 + z * B ** 2)

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.25)
        self.assertAlmostEqual(B.value, 2)

    def test_singular_motzkin_trees(self):
        """ Singular tuning of Motzkin trees
            M = Z * SEQ<=2(M). """

        spec = Specification()
        z, M = spec.variable(), spec.variable()
        spec.add(M, z * spec.Seq(M, leq(2)))

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.333333333333334)
        self.assertAlmostEqual(M.value, 1.0)

    def test_singular_motzkin_trees2(self):
        """ Singular tuning of Motzkin trees
            M = Z + Z * M + Z * M^2. """

        spec = Specification()
        z, M = spec.variable(), spec.variable()
        spec.add(M, z + z * M + z * M ** 2)

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.333333333333334)
        self.assertAlmostEqual(M.value, 1.0)

    def test_singular_trees(self):
        """ Singular tuning of plane trees
            T = 1 + Z * SEQ(T)."""

        spec = Specification()
        z, T = spec.variable(), spec.variable()
        spec.add(T, z * spec.Seq(T))

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.25)
        self.assertAlmostEqual(T.value, 0.5)

    def test_singular_lambda_terms(self):
        """ Singular tuning of plain lambda terms
            L = Z * SEQ(Z) + Z * L + Z * L^2."""

        spec = Specification()
        z, L, D = spec.variable(), spec.variable(), spec.variable()
        spec.add(L, D + z * L + z * L ** 2)
        spec.add(D, z + z * D)

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.295597742522085)
        self.assertAlmostEqual(L.value, 1.19148788395312)

    def test_singular_lambda_terms2(self):
        """ Singular tuning of plain lambda terms
            L = Z * SEQ(Z) + Z * L + Z * L^2."""

        spec = Specification()
        z, L = spec.variable(), spec.variable()
        spec.add(L, z * spec.Seq(z) + z * L + z * L ** 2)

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.295597742522085)
        self.assertAlmostEqual(L.value, 1.19148788395312)

    def test_singular_polya_trees(self):
        """ Singular tuning of Polya trees
            T = 1 + Z * SEQ(T)."""

        spec = Specification()
        z, T = spec.variable(), spec.variable()
        spec.add(T, z * spec.MSet(T))

        spec.run_singular_tuner(z)

        self.assertAlmostEqual(z.value, 0.338322112871298)
        self.assertAlmostEqual(T.value, 1)

    def test_singular_custom_trees(self):
        """ Singular tuning of some custom trees defined by
            T = 1 + Z * SEQ_>=2(T)."""

        params = Params(Type.ALGEBRAIC)
        params.max_iters = 100 # required

        spec = Specification()
        z, T = spec.variable(), spec.variable()
        spec.add(T, z + z * spec.Seq(T, geq(2)))

        spec.run_singular_tuner(z, params)

        self.assertAlmostEqual(z.value, 0.333333333333335)
        self.assertAlmostEqual(T.value, 0.499999999999993)

if __name__ == '__main__':
    unittest.main()
