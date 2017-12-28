import time
import glob
from Encoder import Encoder
from Tree import Tree
import cv2
import numpy as np

for path in glob.glob("test_data\\*.pgm"):
    modes = ["1-value", "2-value", "Markov"]
    for mode in modes:
        print('Reading data from path: {0}, Using mode: {1}'.format(path, mode))
        start = time.time()
        input_data = Encoder.read_data(path)
        end = time.time()
        print('Time spent reading data: {0}'.format(end - start))

        print('Creating frequency dict')
        start = time.time()
        frequency_dict = Encoder.calculate_frequencies(input_data, mode)
        end = time.time()
        print('Time spent creating frequency dict: {0}'.format(end - start))

        print('Creating code dicts')
        start = time.time()
        HuffmanTree = Tree()
        (compressDictionary, decompressDictionary) = HuffmanTree.create_codes(frequency_dict)
        end = time.time()
        print('Time spent creating code dicts: {0}'.format(end - start))

        print('Encoding')
        start = time.time()
        to_code = input_data
        encoded = Encoder.encode(to_code, compressDictionary, mode)
        end = time.time()
        print('Time spent encoding: {0}'.format(end - start))

        print('Decoding')
        start = time.time()
        decoded = Encoder.decode(encoded, decompressDictionary, mode)
        end = time.time()
        print('Time spent decoding: {0}'.format(end - start))

        print("Is input and output the same?: {0}".format(input_data == decoded))
        print()