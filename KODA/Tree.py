import os
import heapq


class HeapNode:
    def __init__(self, char, freq):
        self.char = char
        self.freq = freq
        self.left = None
        self.right = None

    def __cmp__(self, other):
        if other is None:
            return -1
        if not isinstance(other, HeapNode):
            return -1
        return self.freq > other.freq


class Tree:
    def __init__(self, codes={}, compressed_codes={}):
        self.heap = []
        self.codes = codes
        self.compressed_codes = compressed_codes

    def make_heap(self, dict_frequencies):
        for code in dict_frequencies:
            new_node = HeapNode(code, dict_frequencies[code])
            heapq.heappush(self.heap, new_node)

    def merge_nodes(self):
        while len(self.heap) > 1:
            left_node = heapq.heappop(self.heap)
            right_node = heapq.heappop(self.heap)

            merged_node = HeapNode(None, left_node.freq + right_node.freq)
            merged_node.left = left_node
            merged_node.right = right_node

            heapq.heappush(self.heap, merged_node)

    def create_code_recursive(self, parent, current_code):
        if parent is None:
            return

        # lisc
        if parent.char is not None:
            self.codes[parent.char] = current_code
            self.compressed_codes[current_code] = parent.char
            return

        self.create_code_recursive(parent.left, current_code + "0")
        self.create_code_recursive(parent.right, current_code + "1")

    def start_create_codes(self):
        if len(self.heap) > 1:
            print("Something is wrong")

        root = heapq.heappop(self.heap)
        code = ""
        self.create_code_recursive(root, code)

    def create_codes(self, dict_freqencies):
        self.make_heap(dict_freqencies)
        self.merge_nodes()
        self.start_create_codes()

    def print_codes(self):
        for code in self.compressed_codes:
            print("Compressed code {} Normal code {}\n".format(code, self.compressed_codes[code]))



if __name__ == '__main__':
    HuffmanTree = Tree()
    freqencies = {}
    freqencies["000"] = 50
    freqencies["001"] = 40
    freqencies["010"] = 20
    freqencies["011"] = 10
    freqencies["100"] = 6
    freqencies["101"] = 5
    freqencies["110"] = 2
    freqencies["111"] = 1
    HuffmanTree.create_codes(freqencies)
    HuffmanTree.print_codes()



