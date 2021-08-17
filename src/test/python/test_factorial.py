from factorial import factorial

class TestFactorial:
    def test_low(self):
        assert factorial(0) == 1
        assert factorial(5) == 120

    def test_high(self):
        assert factorial(10) == 3628800
