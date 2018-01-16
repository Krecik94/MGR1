from gensim.models import word2vec
import re


def create_m2v_model(pathList):

    newLineRegex = re.compile('\n')

    sentences = []

    if len(pathList) > 0:
        for path in pathList:
            for line in open(path, 'r'):
                sentence = line.lower().split(' ')
                sentence[len(sentence)-1] = newLineRegex.sub('', sentence[len(sentence)-1])
                sentences.append(sentence)

        w2v = word2vec.Word2Vec(sentences, min_count=1)

        return w2v


if __name__ == '__main__':
    create_m2v_model(['Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent1.txt',
                     'Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent2.txt'])
