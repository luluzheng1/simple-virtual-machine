# ported from Ruby code by Mert Erden and Lulu Zheng
# from the Are We Fast Yet benchmarks suite
# see file sieve.rb
class Sieve():
    @classmethod
    def benchmark(cls):
        flags = [True for _ in range(5000)]
        result = cls.sieve(flags, 5000)
        assert cls.verify_result(result)

    @classmethod
    def verify_result(cls, result):
        return result == 669

    @classmethod
    def sieve(cls, flags, size):
        prime_count = 0
        for i in range(2, size):
            if flags[i - 1]:
                prime_count += 1
                k = i + i
                while k <= size:
                    flags[k - 1] = False
                    k += i
        return prime_count
