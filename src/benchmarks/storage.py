# ported from Ruby code by Mert Erden and Lulu Zheng
# from the Are We Fast Yet benchmarks suite
# see file storage.rb

class Random():
    def __init__(self):
        self.seed = 74755

    def next(self):
        self.seed = ((self.seed * 1309) + 13849) & 65535
        return self.seed


class Storage():
    def __init__(self):
        self.count = 0

    def benchmark(self):
        self.count = 0
        random = Random()
        self.build_tree_depth(7, random)
        return self.count

    def verify_result(self):
        assert 5461 == self.count

    def build_tree_depth(self, depth, random):
        self.count += 1
        if depth == 1:
            return [None for _ in range(random.next() % 10 + 1)]
        else:
            [self.build_tree_depth(depth - 1, random) for _ in range(4)]


test = Storage()
test.benchmark()
test.verify_result()
