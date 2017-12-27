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
        pass

    @staticmethod
    def calculate_frequencies(input_data, mode):
        """
        Creates frequency dictionary base on input data and selected mode
        :param input_data:
        :param mode: 1-value 2-values of Markov
        :return: dict of frequencies (symbol -> frequency)
        """
        pass

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
    print('main')

    input_data = Encoder.read_data("MGR1/KODA/test_data/geometr_05.pgm")
    frequency_dict = Encoder.calculate_frequencies(input_data, "1-value")
    encoded_data = Encoder.encode(input_data, frequency_dict, "1-value")

    if Encoder.decode(encoded_data, frequency_dict, "1-value") == input_data:
        print('Success')
    else:
        print('No success')
