from Encoder import Encoder
from Tree import Tree
import cv2
import numpy as np

path = 'test_data\\uniform.pgm'
print('Reading data from path: {0}'.format(path))
# Reading data. input_data will be a list of values
input_data = Encoder.read_data(path)

# Creating frequency dict of symbols in input data
frequency_dict = Encoder.calculate_frequencies(input_data, "1-value")
print(frequency_dict)

HuffmanTree = Tree()

(compressDictionary, decompressDictionary) = HuffmanTree.create_codes(frequency_dict)
HuffmanTree.print_codes()
to_code = input_data
encoded = Encoder.encode(to_code, compressDictionary, '1-value')
print("Encoded:")
print(encoded)
print("----")
decoded = Encoder.decode(encoded, decompressDictionary, '1-value')
print("Decoded (original):")
print(decoded)

arr = np.reshape(decoded, (512, 512))

cv2.imwrite("decoded.pgm", arr)