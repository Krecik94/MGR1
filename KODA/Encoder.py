import numpy
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

        print('Size of input image: {0}'.format(image.size))

        # List comprehension. Gathers all pixels from input data and forms a list.
        input_data = [x for x in image.flat]

        print('Size of data in list format: {0}'.format(len(input_data)))

        return input_data

    @staticmethod
    def calculate_frequencies(input_data, mode):
        """
        Creates frequency dictionary base on input data and selected mode
        :param input_data:
        :param mode: 1-value 2-values of Markov
        :return: dict of frequencies (symbol -> frequency)
        """
        if mode == '1-value':
            frequency_dict = {}
            unique_values = set(input_data)
            for value in unique_values:
                frequency_dict[value] = input_data.count(value)
            return frequency_dict

        elif mode == '2-value':
            pass
        elif mode == 'Markov':
            pass
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
        return

    @staticmethod
    def decode(input_data, decompress_dictionary, mode):
        """
        Decodes data acording to selected mode
        :param input_data: the data to decompress
        :param decompress_dictionary: dictionary containing codes
        :param mode: 1-value, 2-value, Markov
        :return:
        """
        decoded = []
        code = ""
        for char in input_data:
            code += char
            if code in decompress_dictionary:
                decoded.append(decompress_dictionary[code])
                code = ""
        return decoded

# This test passes if module is called directly. If module is imported elsewhere this code won't be executed.
if __name__ == '__main__':

    path = 'test_data\\uniform.pgm'
    print('Reading data from path: {0}'.format(path))
    # Reading data. input_data will be a list of values
    input_data = Encoder.read_data(path)

    # Creating frequency dict of symbols in input data
    frequency_dict = Encoder.calculate_frequencies(input_data, "1-value")
    print (frequency_dict)

    encoded_data = Encoder.encode(input_data, frequency_dict, "1-value")

    if Encoder.decode(encoded_data, frequency_dict, "1-value") == input_data:
        print('Success')
    else:
        print('No success')
