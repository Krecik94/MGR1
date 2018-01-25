import time
import glob
from Encoder import Encoder
from Tree import Tree
import cv2
import numpy as np
import os

for path in glob.glob("test_data\\*.pgm"):
    modes = ["1-value", "2-value", "Markov"]
    for mode in modes:
        print('\nFile: {0}, Mode: {1}'.format(path, mode))
        input_data = Encoder.read_data(path)
        print('Input size (bits): {0}'.format(len(input_data) * 8))
        start = time.time()
        frequency_dict = Encoder.calculate_frequencies(input_data, mode)
        print('Symbols in dict: {0}'.format(len(frequency_dict.keys())))
        entropy = Encoder.calculate_entropy(frequency_dict, mode)
        print('Entropy: {0:01.2f}'.format(entropy))
        HuffmanTree = Tree()
        (compressDictionary, decompressDictionary) = HuffmanTree.create_codes(frequency_dict, mode)
        to_code = input_data
        encoded = Encoder.encode(to_code, compressDictionary, mode)
        print('Encoded size (bits): {0}'.format(len(encoded)))
        average_code_length = Encoder.calculate_average_code_length(frequency_dict,compressDictionary)
        print('Average code length: {0:01.2f}'.format(average_code_length))
        print('Compression ratio: {0:01.1f}%'.format(100 * len(encoded)/(len(input_data) * 8)))
        decoded = Encoder.decode(encoded, decompressDictionary, mode)
        end = time.time()
        print("Is input and output the same?: {0}".format(input_data == decoded))
        print('Time spent: {0:01.1f}s'.format(end - start))
        os.chdir(os.path.join(os.getcwd(),'output'))
        arr = np.reshape(decoded, (512, 512))
        cv2.imwrite('{0}'.format(os.path.split(path)[1]), arr)
        os.chdir(os.path.abspath(os.path.join(os.getcwd(), os.pardir)))
