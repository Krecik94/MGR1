import os
import heapq
import numpy
import cv2
from functools import total_ordering

@total_ordering
class HeapNode:
    def __init__(self, char, freq):
        self.char = char
        self.freq = freq
        self.left = None
        self.right = None

    def __eq__(self, other):
        if other is None:
            return -1
        if not isinstance(other, HeapNode):
            return -1
        return self.freq == other.freq

    def __gt__(self, other):
        if other is None:
            return -1
        if not isinstance(other, HeapNode):
            return -1
        return self.freq > other.freq


class Tree:
    def __init__(self, decompress_dictionary={}, compress_dictionary={}):
        self.heap = []
        self.decompress_dictionary = decompress_dictionary
        self.compress_dictionary = compress_dictionary

    def _make_heap(self, dict_frequencies):
        for code in dict_frequencies:
            new_node = HeapNode(code, dict_frequencies[code])
            heapq.heappush(self.heap, new_node)

    def _merge_nodes(self):
        while len(self.heap) > 1:
            left_node = heapq.heappop(self.heap)
            right_node = heapq.heappop(self.heap)

            merged_node = HeapNode(None, left_node.freq + right_node.freq)
            merged_node.left = left_node
            merged_node.right = right_node

            heapq.heappush(self.heap, merged_node)

    def _create_code_recursive(self, parent, current_code):
        if parent is None:
            return

        # leaf
        if parent.char is not None:
            self.decompress_dictionary[current_code] = parent.char
            self.compress_dictionary[parent.char] = current_code
            return

        self._create_code_recursive(parent.left, current_code + "0")
        self._create_code_recursive(parent.right, current_code + "1")

    def _start_create_codes(self):
        if len(self.heap) > 1:
            print("Something is wrong")

        root = heapq.heappop(self.heap)
        code = ""
        self._create_code_recursive(root, code)

    def create_codes(self, dict_freqencies):
        """
        Returns Tuple with 2 dictionaries. First
        :param dict_freqencies:
        :return:
        """
        self._make_heap(dict_freqencies)
        self._merge_nodes()
        self._start_create_codes()

        return (self.compress_dictionary, self.decompress_dictionary)

    def print_codes(self):
        for code in self.compress_dictionary:
            print("Compressed code {} Normal code {}\n".format(self.compress_dictionary[code], code))



if __name__ == '__main__':
    HuffmanTree = Tree()
    freqencies = {}
    freqencies["00000000"] = 50
    freqencies["00000001"] = 40
    freqencies["00000010"] = 20
    freqencies["00000011"] = 10
    freqencies["00000100"] = 6
    freqencies["00000101"] = 5
    freqencies["00000110"] = 2
    freqencies["00000111"] = 1
    HuffmanTree.create_codes(freqencies)
    HuffmanTree.print_codes()
    to_code = "000000000000000100000111"
    encoded = HuffmanTree.encode_bytes_1_value(to_code)
    print("Encoded:")
    print(encoded)
    print("----")
    decoded = HuffmanTree.decode_bytes_1_value(encoded)
    print("Decoded (original):")
    print(decoded)

