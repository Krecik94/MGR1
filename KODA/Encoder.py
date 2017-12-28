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
            pass
        else:
            print('Wrong mode.')

    @staticmethod
    def encode(input_data, frequency_dictionary, mode):
        """
        Encodes data according to selected mode
        :param mode: 1-value 2-values of Markov
        :param input_data:
        :param frequency_dictionary:
        :return: encoded data
        """
        pass

    @staticmethod
    def decode(input_data, frequency_dictionary, mode):
        # Same as encode(), but other way around
        pass


# This test passes if module is called directly. If module is imported elsewhere this code won't be executed.
if __name__ == '__main__':
    path = 'test_data\\uniform.pgm'
    print('Reading data from path: {0}'.format(path))
    # Reading data. input_data will be a list of values
    input_data = Encoder.read_data(path)

    # Creating frequency dict of symbols in input data
    frequency_dict = Encoder.calculate_frequencies(input_data, "2-value")
    print(frequency_dict)

    encoded_data = Encoder.encode(input_data, frequency_dict, "1-value")

    if Encoder.decode(encoded_data, frequency_dict, "1-value") == input_data:
        print('Success')
    else:
        print('No success')
