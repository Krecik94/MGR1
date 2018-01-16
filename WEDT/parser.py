import os
import re


class Alignment:
    def __init__(self, first_phrase, second_phrase, label, match, left_description, right_description):
        self.first_phrase = first_phrase
        self.second_phrase = second_phrase
        self.label = label
        self.match = match
        self.left_description = left_description
        self.right_description = right_description


class Sentence_pair:
    def __init__(self, ID):
        self.ID = ID
        self.sentence_1 = None
        self.sentence_2 = None
        self.source = {}
        self.translation = {}
        self.alignment = []


def parse_input():
    sentence_pairs = []

    sentence_id_pattern = re.compile('sentence id="(\d*)')
    sentence_pattern = re.compile('//\s(.*)')
    index_pattern = re.compile('(\d*) (.*) :')
    alignment_pattern = re.compile('(.*) <==> (.*) \/\/ (.*) \/\/ (.*) \/\/ (.*) <==> (.*)')
    end_of_source = re.compile('</source>')
    end_of_translation = re.compile('</translation>')
    end_of_alignment = re.compile('</alignment>')

    with open('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.images.wa', 'r') as f:
        while True:

            line = f.readline()
            if line is '':
                break
            match = sentence_id_pattern.search(line)
            if match is not None:
                # print(match.group(1))
                sentence_pair = Sentence_pair(match.group(1))

                line = f.readline()
                match = sentence_pattern.search(line)
                sentence_pair.sentence_1 = match.group(1)

                line = f.readline()
                match = sentence_pattern.search(line)
                sentence_pair.sentence_2 = match.group(1)

                line = f.readline()

                while (True):
                    line = f.readline()
                    if end_of_source.match(line) is not None:
                        break
                    match = index_pattern.match(line)
                    sentence_pair.source[match.group(1)] = match.group(2)
                    # print(match.group(1))
                    # print(match.group(2))

                # print(sentence_pair.source)

                line = f.readline()

                while (True):
                    line = f.readline()
                    if end_of_translation.match(line) is not None:
                        break
                    match = index_pattern.match(line)
                    sentence_pair.translation[match.group(1)] = match.group(2)
                    # print(match.group(1))
                    # print(match.group(2))

                # print(sentence_pair.translation)

                line = f.readline()

                while (True):
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
                # print(sentence_pair.alignment)
                sentence_pairs.append(sentence_pair)
                line = f.readline()

    return sentence_pairs


if __name__ == "__main__":
    parse_input()
