import numpy
import math
import cv2


class Encoder:
    @staticmethod
    def read_data(path):
        """
        Reads data and converts it into convenient format for further processing.
        :param path: path to data file
        :return: TODO: think what format would be best(list of elements?)
        """
        # Image is of numpy.ndarray type
        image = cv2.imread(path, cv2.IMREAD_GRAYSCALE)

        # print('Size of input image: {0}'.format(image.size))

        # List comprehension. Gathers all pixels from input data and forms a list.
        input_data = [x for x in image.flat]

        # print('Size of data in list format: {0}'.format(len(input_data)))

        return input_data

    @staticmethod
    def calculate_entropy(frequency_dict, mode):
        if mode != "Markov":
            total_symbols_sum = 0
            for key in frequency_dict.keys():
                total_symbols_sum += frequency_dict[key]

            entropy = 0

            for key in frequency_dict.keys():
                probability = frequency_dict[key] / total_symbols_sum
                entropy += probability * math.log(probability, 2)
            return -entropy
        else:
            dict_of_dicts = {}
            entropy = 0
            outer_dict_of_probabilities = {}
            inner_dict_of_probabilities = {}
            for key in frequency_dict:
                dict_of_dicts[key[0]] = {}
                inner_dict_of_probabilities[key[0]] = {}

            for key in frequency_dict:
                dict_of_dicts[key[0]][key[1]] = frequency_dict[key]
                inner_dict_of_probabilities[key[0]][key[1]] = 0

            for key in dict_of_dicts:
                sum_of_symbols = 0
                for inner_key in dict_of_dicts[key]:
                    sum_of_symbols += dict_of_dicts[key][inner_key]

                for inner_key in dict_of_dicts[key]:
                    inner_dict_of_probabilities[key][inner_key] = dict_of_dicts[key][inner_key] / sum_of_symbols

            total_symbols_sum = 0
            for key in frequency_dict.keys():
                total_symbols_sum += frequency_dict[key]

            for key in frequency_dict.keys():
                outer_dict_of_probabilities[key] = frequency_dict[key]/total_symbols_sum

            for key in frequency_dict.keys():
                entropy += outer_dict_of_probabilities[key] * math.log(inner_dict_of_probabilities[key[0]][key[1]], 2)

            return -entropy

    @staticmethod
    def calculate_average_code_length(frequency_dict, compress_dict):
        total_symbols_sum = 0
        for key in frequency_dict.keys():
            total_symbols_sum += frequency_dict[key]

        average_code_length = 0

        for key in frequency_dict.keys():
            probability = frequency_dict[key] / total_symbols_sum
            average_code_length += probability * len(compress_dict[key])
        return average_code_length

    @staticmethod
    def calculate_frequencies(input_data, mode):
        """
        Creates frequency dictionary based on input data and selected mode
        :param input_data:
        :param mode: 1-value 2-values or Markov
        :return: dict of frequencies (symbol -> frequency)
        """
        if mode == '1-value':
            frequency_dict = {}
            unique_values = set(input_data)
            for value in unique_values:
                frequency_dict[value] = input_data.count(value)
            return frequency_dict

        elif mode == '2-value':
            frequency_dict = {}
            # Iterating over input data elements 2 at a time, creating tuples.
            # '//' is floor division
            for i in range(0, len(input_data) // 2):
                data_tuple = (input_data[2 * i], input_data[2 * i + 1])
                # Counting occurrences
                if data_tuple in frequency_dict:
                    frequency_dict[data_tuple] += 1
                else:
                    frequency_dict[data_tuple] = 1
            # Case when number of data elements in input data is odd
            if len(input_data) % 2 == 1:
                # [-1] takes last element of list
                frequency_dict[input_data[-1], None] = 1
            return frequency_dict

        elif mode == 'Markov':
            frequency_dict = {}
            # 1st element has nothing before it
            frequency_dict[(None, input_data[0])] = 1
            for i in range(1, len(input_data)):
                data_tuple = (input_data[i - 1], input_data[i])
                # Counting occurrences
                if data_tuple in frequency_dict:
                    frequency_dict[data_tuple] += 1
                else:
                    frequency_dict[data_tuple] = 1
            return frequency_dict

        else:
            print('Wrong mode.')

    @staticmethod
    def encode(input_data, compress_dictionary, mode):
        """
        Encodes data according to selected mode
        :param mode: 1-value 2-values of Markov
        :param input_data: the data to compress
        :param compress_dictionary: dictionary containing codes
        :return: encoded data: string containing encoded data, None if invalid mode
        """
        if mode == '1-value':
            encoded_text = ""
            for val in input_data:
                encoded_text += compress_dictionary[val]
            return encoded_text
        elif mode == '2-value':
            encoded_text = ""
            for iter in range(0, len(input_data), 2):
                encoded_text += compress_dictionary[(input_data[iter], input_data[iter + 1])]
            return encoded_text
        elif mode == 'Markov':
            encoded_text = ""
            for iter in range(len(input_data)):
                if iter == 0:
                    encoded_text += compress_dictionary[(None, input_data[iter])]
                else:
                    encoded_text += compress_dictionary[(input_data[iter - 1], input_data[iter])]
            return encoded_text
        return

    @staticmethod
    def decode(input_data, decompress_dictionary, mode):
        """
        Decodes data according to selected mode
        :param input_data: the data to decompress
        :param decompress_dictionary: dictionary containing codes
        :param mode: 1-value, 2-value, Markov
        :return:
        """

        if mode == '1-value':
            decoded = []
            code = ""
            for char in input_data:
                code += char
                if code in decompress_dictionary:
                    decoded.append(decompress_dictionary[code])
                    code = ""
            return decoded
        elif mode == '2-value':
            decoded = []
            code = ""
            for char in input_data:
                code += char
                if code in decompress_dictionary:
                    decoded.append(decompress_dictionary[code][0])
                    decoded.append(decompress_dictionary[code][1])
                    code = ""
            return decoded
        elif mode == 'Markov':
            decoded = []
            decoded.append(decompress_dictionary[(None, '')])
            previous_symbol = decompress_dictionary[(None, '')]
            code = ""
            for char in input_data:
                while True:
                    if (previous_symbol, '') in decompress_dictionary:
                        decoded.append(decompress_dictionary[(previous_symbol, '')])
                        previous_symbol = decompress_dictionary[(previous_symbol, '')]
                    else:
                        break
                code += char
                if (previous_symbol, code) in decompress_dictionary:
                    new_symbol = decompress_dictionary[(previous_symbol, code)]
                    decoded.append(new_symbol)
                    previous_symbol = new_symbol
                    code = ""
            return decoded
        return


# This test passes if module is called directly. If module is imported elsewhere this code won't be executed.
if __name__ == '__main__':
    path = 'test_data\\uniform.pgm'
    print('Reading data from path: {0}'.format(path))
    # Reading data. input_data will be a list of values
    input_data = Encoder.read_data(path)

    # Creating frequency dict of symbols in input data
    frequency_dict = Encoder.calculate_frequencies(input_data, "Markov")
    print(frequency_dict)

    # encoded_data = Encoder.encode(input_data, frequency_dict, "Markov")

    # if Encoder.decode(encoded_data, frequency_dict, "Markov") == input_data:
    #    print('Success')
    # else:
    #    print('No success')
