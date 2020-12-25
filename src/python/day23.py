import functools
from operator import mul


class Node:
    def __init__(self, value):
        self.value = value
        self.next = None

    def __repr__(self):
        return str(self.value)

    def __eq__(self, other):
        return self.value == other


class CircularLinkedList:
    def __init__(self):
        self.head = None
        self.last = None
        self.mapping = {}

    def from_list(self, lst):
        prev = None
        for x in reversed(lst):
            node = Node(x)
            node.next = prev
            self.mapping[x] = node
            if prev is None:
                self.last = node
            prev = node
        self.head = node
        self.last.next = self.head

    def find(self, value):
        return self.mapping[value]

    def get_nodes(self, start, count):
        current = self.find(start)
        values = []
        for _ in range(count):
            values.append(current.value)
            current = current.next
        return values


def parse_input(num):
    return [int(x) for x in str(num)]


def solve_part2(num):
    cups = parse_input(num) + list(range(10, 1_000_001))
    max_cup = max(cups)
    iterations = 10_000_000
    lst = CircularLinkedList()
    lst.from_list(cups)

    current = lst.head
    for _ in range(iterations):
        pickup1 = current.next
        pickup2 = pickup1.next
        pickup3 = pickup2.next
        needle = current.value - 1
        while True:
            if needle == 0:
                needle = max_cup
                continue
            if needle in [pickup1, pickup2, pickup3]:
                needle -= 1
                continue
            destination = lst.find(needle)
            break
        current.next = pickup3.next
        pickup3.next, destination.next = destination.next, pickup1
        current = current.next
    return functools.reduce(mul, lst.get_nodes(1, 3), 1)


if __name__ == "__main__":
    print(solve_part2(158937462))
