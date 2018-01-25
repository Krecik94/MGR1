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
    def __init__(self):
        self.heap = []
        self.decompress_dictionary = {}
        self.compress_dictionary = {}

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

    def make_Markov(self, frequency_dict):
        dict_of_dicts = {}
        output_compress_dict = {}
        output_decompress_dict = {}
        for key in frequency_dict:
            dict_of_dicts[key[0]] = {}

        for key in frequency_dict:
            dict_of_dicts[key[0]][key[1]] = frequency_dict[key]

        for key in dict_of_dicts:
            HuffmanTree = Tree()
            (compressDictionary, decompressDictionary) = HuffmanTree.create_codes(dict_of_dicts[key], '1-value')
            for second_key in compressDictionary:
                output_compress_dict[(key, second_key)] = compressDictionary[second_key]
                output_decompress_dict[(key, compressDictionary[second_key])] = second_key

        return (output_compress_dict, output_decompress_dict)

    def create_codes(self, dict_freqencies, mode):
        """
        Returns Tuple with 2 dictionaries. First
        :param dict_freqencies:
        :return:
        """
        if mode in ["1-value", "2-value"]:
            self._make_heap(dict_freqencies)
            self._merge_nodes()
            self._start_create_codes()

            return (self.compress_dictionary, self.decompress_dictionary)
        if mode == "Markov":
            return self.make_Markov(dict_freqencies)

    def print_codes(self):
        for code in self.compress_dictionary:
            print("Compressed code {} Normal code {}\n".format(self.compress_dictionary[code], code))


if __name__ == '__main__':
    HuffmanTree = Tree()
    freqencies = {}
    freqencies[1] = 50
    freqencies[23] = 40
    freqencies[44] = 20
    freqencies[15] = 10
    freqencies[90] = 6
    freqencies[244] = 5
    freqencies[234] = 2
    freqencies[255] = 1
    HuffmanTree.create_codes(freqencies)
    HuffmanTree.print_codes()
