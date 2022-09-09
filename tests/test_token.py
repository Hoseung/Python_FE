from tokens import NewEnum
import pytest # more convenient than python.unittest

"""
pytest.mark : cateogirze tests.
pytest.mark.parameterize: run a test with set of parameters
"""

E = NewEnum('Enum', ["A", "B", "C"])

class TestNewEnum:

    @pytest.mark.parametrize("value1, value2", [
        (E.A, E.A), 
        (E.B, E.B),
        (E.C, E.C),
    ])
    def test_should_be_eqaul(self, value1, value2):
        assert value1 == value2

    @pytest.mark.parametrize("value1, value2", [
        (E.A, E.B),
        (E.A, E.C),
        (E.B, E.C),
        ])
    def test_should_not_be_equal(self, value1, value2):
        assert value1 != value2