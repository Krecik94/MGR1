import time
import glob
from Encoder import Encoder
from Tree import Tree
import cv2
import numpy as np

for path in glob.glob("test_data\\*.pgm"):
    modes = ["1-value", "2-value", "Markov"]
    for mode in modes:
        print('\nFile: {0}, Mode: {1}'.format(path, mode))
        start = time.time()
        input_data = Encoder.read_data(path)
        print('Input size (bits): {0}'.format(len(input_data) * 8))

        print('Creating frequency dict')

        frequency_dict = Encoder.calculate_frequencies(input_data, mode)
        print('Symbols in dict: {0}'.format(len(frequency_dict.keys())))
        entropy = Encoder.calculate_entropy(frequency_dict)
        print('Entropy: {0}'.format(entropy))

        HuffmanTree = Tree()
        (compressDictionary, decompressDictionary) = HuffmanTree.create_codes(frequency_dict, mode)
        to_code = input_data
        encoded = Encoder.encode(to_code, compressDictionary, mode)
        print('Encoded size (bits): {0}'.format(len(encoded)))
        average_code_length = Encoder.calculate_average_code_length(frequency_dict,compressDictionary)
        print('Average code length: {0}'.format(average_code_length))
        print('Compression ratio: {0}%'.format(100 * len(encoded)/(len(input_data) * 8)))
        # print('Decoding')
        # start = time.time()
        # decoded = Encoder.decode(encoded, decompressDictionary, mode)
        # end = time.time()
        # print('Time spent decoding: {0}'.format(end - start))
        #
        # print("Is input and output the same?: {0}".format(input_data == decoded))
        # print()
