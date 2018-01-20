import re

import numpy as np


class Alignment:
    def __init__(self, first_phrase, second_phrase, label, match, left_description, right_description):
        self.first_phrase = first_phrase
        self.second_phrase = second_phrase
        self.label = label
        self.match = match
        self.left_description = left_description
        self.right_description = right_description


class SentencePair:
    def __init__(self, ID):
        self.ID = ID
        self.sentence_1 = None
        self.sentence_2 = None

        # Sentence 1 divided into words
        self.source = {}

        # Sentence 2 divided into words
        self.translation = {}

        # List of phrase alignments
        self.alignment = []


def parse_input(path):
    """
    Function to parse input data into format that is used by further functions
    :param path: path to input data file
    :return: list of Sentence_pair objects
    """
    sentence_pairs = []

    sentence_id_pattern = re.compile('sentence id="(\d*)')
    sentence_pattern = re.compile('//\s(.*)')
    index_pattern = re.compile('(\d*) (.*) :')
    alignment_pattern = re.compile('(.*) <==> (.*) \/\/ (.*) \/\/ (.*) \/\/ (.*) <==> (.*)')
    end_of_source = re.compile('</source>')
    end_of_translation = re.compile('</translation>')
    end_of_alignment = re.compile('</alignment>')

    with open(path, 'r') as f:
        while True:
            line = f.readline()
            # Detect end of file
            if line is '':
                break
            match = sentence_id_pattern.search(line)
            # Case of new sentence pair
            if match is not None:
                sentence_pair = SentencePair(match.group(1))

                line = f.readline()
                match = sentence_pattern.search(line)
                sentence_pair.sentence_1 = match.group(1)

                line = f.readline()
                match = sentence_pattern.search(line)
                sentence_pair.sentence_2 = match.group(1)

                line = f.readline()

                # Reading words of 1st sentence
                while True:
                    line = f.readline()
                    if end_of_source.match(line) is not None:
                        break
                    match = index_pattern.match(line)
                    sentence_pair.source[match.group(1)] = match.group(2)

                line = f.readline()

                # Reading words of 2st sentence
                while True:
                    line = f.readline()
                    if end_of_translation.match(line) is not None:
                        break
                    match = index_pattern.match(line)
                    sentence_pair.translation[match.group(1)] = match.group(2)

                line = f.readline()

                # Reading alignment
                while True:
                    line = f.readline()
                    if end_of_alignment.match(line) is not None:
                        break
                    match = alignment_pattern.match(line)
                    sentence_pair.alignment.append(Alignment(first_phrase=match.group(1),
                                                             second_phrase=match.group(2),
                                                             label=match.group(3),
                                                             match=match.group(4),
                                                             left_description=match.group(5),
                                                             right_description=match.group(6)))
                sentence_pairs.append(sentence_pair)
                line = f.readline()

    return sentence_pairs


def create_input_output_data_scores(sentence_pairs, model):
    """
    Creates input and output data sets of proper dimensions to be fed into the network.
    Output data consists of scores assigned to phrase pairs
    :param sentence_pairs: list of sentence pair objects to be converted into proper format
    :return: tuple of (input_data, output_data) where input data is a (*,200) matrix and output data is a (*,7) matrix
    """
    input_data = None
    output_data = None

    for sentence_pair in sentence_pairs:
        for single_alignment in sentence_pair.alignment:
            first_index_list = single_alignment.first_phrase.split(' ')
            first_input = np.zeros(100)
            if first_index_list[0] != '0':
                first_word_list = [sentence_pair.source[x] for x in first_index_list]
                # print(first_word_list)
                for word in first_word_list:
                    first_input = np.add(first_input, model.wv[word.lower()])
                # print(first_input)

            second_index_list = single_alignment.second_phrase.split(' ')
            second_input = np.zeros(100)
            if second_index_list[0] != '0':
                second_word_list = [sentence_pair.translation[x] for x in second_index_list]
                # print(second_word_list)
                for word in second_word_list:
                    second_input = np.add(second_input, model.wv[word.lower()])
                # print(second_input)

            to_append = np.concatenate((first_input, second_input))
            if input_data is None:
                input_data = np.array([to_append], ndmin=2)
            else:
                input_data = np.append(input_data, [to_append], axis=0)

            output = np.zeros(7)
            if single_alignment.match == 'NIL':
                output[0] = 1
            elif single_alignment.match == '0':
                output[1] = 1
            elif single_alignment.match == '1':
                output[2] = 1
            elif single_alignment.match == '2':
                output[3] = 1
            elif single_alignment.match == '3':
                output[4] = 1
            elif single_alignment.match == '4':
                output[5] = 1
            elif single_alignment.match == '5':
                output[6] = 1

            if output_data is None:
                output_data = np.array([output], ndmin=2)
            else:
                output_data = np.append(output_data, [output], axis=0)

    return (input_data,output_data)


if __name__ == "__main__":
    parse_input()
