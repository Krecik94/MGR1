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

        # leaf
        if parent.char is not None:
            self.codes[current_code] = parent.char
            self.compressed_codes[parent.char] = current_code
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

    # def encode_text_1_value(self, text):
    #     encoded_text = ""
    #     byte_text = ""
    #     for char in text:
    #         byte_text += char
    #         if len(byte_text) is 8:
    #             encoded_text += self.compressed_codes[byte_text]
    #             byte_text = ""
    #     return encoded_text
    #
    # def decode_text_1_value(self, encoded_text):
    #     text = ""
    #     code = ""
    #     for char in encoded_text:
    #         code += char
    #         if code in self.codes:
    #             text += self.codes[code]
    #             code = ""
    #     return text

    def encode_bytes_1_value(self, bytes):
        encoded_text = ""
        for val in bytes:
            encoded_text += self.compressed_codes[val]
        return encoded_text

    def decode_bytes_1_value(self, encoded_text):
        decoded = []
        code = ""
        for char in encoded_text:
            code += char
            if code in self.codes:
                decoded.append(self.codes[code])
                code = ""
        return decoded

    def print_codes(self):
        for code in self.compressed_codes:
            print("Compressed code {} Normal code {}\n".format(self.compressed_codes[code], code))



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
    encoded = HuffmanTree.encode_text_1_value(to_code)
    print("Encoded:")
    print(encoded)
    print("----")
    decoded = HuffmanTree.decode_text_1_value(encoded)
    print("Decoded (original):")
    print(decoded)

