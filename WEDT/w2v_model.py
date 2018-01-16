from gensim.models import word2vec
import re


def create_model(path1, path2):

    newLineRegex = re.compile('\n')

    sentences = []

    for line in open(path1, 'r'):
        sentence = line.split(' ')
        sentence[len(sentence)-1] = newLineRegex.sub('', sentence[len(sentence)-1])
        sentences.append(sentence)

    for line in open(path2, 'r'):
        sentence = line.split(' ')
        sentence[len(sentence)-1] = newLineRegex.sub('', sentence[len(sentence)-1])
        sentences.append(sentence)

    w2v = word2vec.Word2Vec(sentences)

    return w2v


if __name__ == '__main__':
    create_model('Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent1.txt',
                 'Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent2.txt')
