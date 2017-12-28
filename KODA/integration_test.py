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

HuffmanTree.create_codes(frequency_dict)
HuffmanTree.print_codes()
to_code = input_data
encoded = HuffmanTree.encode_bytes_1_value(to_code)
print("Encoded:")
print(encoded)
print("----")
decoded = HuffmanTree.decode_bytes_1_value(encoded)
print("Decoded (original):")
print(decoded)

arr = np.reshape(decoded, (512, 512))

cv2.imwrite("decoded.pgm", arr)