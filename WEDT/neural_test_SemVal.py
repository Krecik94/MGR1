import tensorflow as tf
import re
from gensim.models import word2vec
import numpy as np

input_data1 = []
input_data2 = []
gs = []
input_file = open("Data/STS/Train_data/STS.input.headlines.txt", "r")
gs_file = open("Data/STS/Train_data/STS.gs.headlines.txt", "r")

# sentences = word2vec.Text8Corpus('text8')
w2v = word2vec.KeyedVectors.load_word2vec_format('deps.words')  # word2vec.Word2Vec(sentences, size=200)#

# w2v.wv.save_word2vec_format('test')

# Regex for taking out special characters
wordRegex = re.compile('[^a-zA-Z]')

numRegex = re.compile('\n')

for line in input_file:
    if line != "\n":
        val = line.split("\t")
        tmp1 = np.array([])
        tmp2 = np.array([])
        split = re.split('\s|-', val[1])
        for x in re.split('\s|-', val[0]):  # .split(" "):
            if wordRegex.sub('', x.lower()) != '':
                tmp1 = np.append(tmp1, w2v.wv[wordRegex.sub('', x.lower())])
                # if tmp1.size:
                #     tmp1 = np.concatenate((tmp1, w2v.wv[wordRegex.sub('', x.lower())]))
                # else:
                #     tmp1 = w2v.wv[wordRegex.sub('', x.lower())]
        for x in re.split('\s|-', val[1]):  # .split(" "):
            if wordRegex.sub('', x.lower()) != '':
                tmp2 = np.append(tmp2, w2v.wv[wordRegex.sub('', x.lower())])
                # if tmp1.size:
                #     tmp2 = np.concatenate((tmp2, w2v.wv[wordRegex.sub('', x.lower())]))
                # else:
                #     tmp2 = w2v.wv[wordRegex.sub('', x.lower())]
        input_data1.append(tmp1)
        input_data2.append(tmp2)
for line in gs_file:
    if line != "\n":
        gs.append(float(numRegex.sub('', line)))

input_data = []
for x in range(len(input_data1)):
    # input_data1[x].extend([0] * (30 - len(input_data1[x])))
    # input_data2[x].extend([0] * (30 - len(input_data2[x])))
    input_data.append((input_data1[x], input_data2[x], gs[x]))


n_nodes_hl1 = 500
n_nodes_hl2 = 500
n_nodes_hl3 = 500
n_classes = 1
batch_size = 100


x = tf.placeholder(tf.float32, [None, None])  # first sentence
x2 = tf.placeholder(tf.float32, [None, None])  # second sentence
y = tf.placeholder(tf.float32)  # target value

def neural_network_model(data, data2):
    """
    Fuction creating neural network model
    :param data: neural network input data
    :return: neural network output
    """

    """
    Definition of weights and biases in each node in layer.
    At start they are defined as random
    """
    hidden_1_layer_1 = {'weights': tf.Variable(tf.random_normal([9000, n_nodes_hl1])),
                        'biases': tf.Variable(tf.random_normal([n_nodes_hl1]))}

    hidden_1_layer_2 = {'weights': tf.Variable(tf.random_normal([9000, n_nodes_hl1])),
                        'biases': tf.Variable(tf.random_normal([n_nodes_hl1]))}

    hidden_2_layer = {'weights1': tf.Variable(tf.random_normal([n_nodes_hl1, n_nodes_hl2])),
                      'weights2': tf.Variable(tf.random_normal([n_nodes_hl1, n_nodes_hl2])),
                      'biases': tf.Variable(tf.random_normal([n_nodes_hl2]))}

    output_layer = {'weights': tf.Variable(tf.random_normal([n_nodes_hl2, n_classes])),
                    'biases': tf.Variable(tf.random_normal([n_classes]))}

    """
    Definition of nodes in layers
    """
    l1_1 = tf.add(tf.matmul(data, hidden_1_layer_1['weights']), hidden_1_layer_1['biases'])
    l1_1 = tf.nn.relu(l1_1)

    l1_2 = tf.add(tf.matmul(data2, hidden_1_layer_2['weights']), hidden_1_layer_2['biases'])
    l1_2 = tf.nn.relu(l1_2)

    l2 = tf.add(tf.add(tf.matmul(l1_1, hidden_2_layer['weights1']), tf.matmul(l1_2, hidden_2_layer['weights2'])), hidden_2_layer['biases'])
    l2 = tf.nn.relu(l2)

    output = tf.matmul(l2, output_layer['weights']) + output_layer['biases']

    return output

def train_neural_network(x, x2):
    """
    Training neural network
    :param x: Placeholder for input data (???)
    :return: none
    """
    prediction = neural_network_model(x, x2)
    cost = tf.reduce_mean(tf.abs(tf.subtract(prediction, y)))  # tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=prediction, labels=y)) TODO
    optimizer = tf.train.GradientDescentOptimizer(1.0).minimize(cost)

    hm_epochs = 20
    with tf.Session() as sess:
        sess.run(tf.global_variables_initializer())
        for epoch in range(hm_epochs):
            epoch_loss = 0
            for fst, scnd, thrd in input_data:
                fst_res = np.pad(fst, (0, 9000 - fst.shape[0]), 'constant', constant_values=0.0)
                scnd_res = np.pad(scnd, (0, 9000 - scnd.shape[0]), 'constant', constant_values=0.0)
                fst_res.shape = (1, fst_res.shape[0])
                scnd_res.shape = (1, scnd_res.shape[0])
                _, c = sess.run([optimizer, cost], feed_dict={x: fst_res, x2: scnd_res, y: thrd})
                epoch_loss += c

            print('Epoch', epoch, 'completed out of', hm_epochs, 'loss:', epoch_loss)
        correct = tf.equal(prediction, y)
        accuracy = tf.reduce_mean(tf.cast(correct, 'float'))
        fst_test = np.pad(input_data[0][0], (0, 9000 - input_data[0][0].shape[0]), 'constant', constant_values=0.0)
        scnd_test = np.pad(input_data[0][1],(0, 9000 - input_data[0][1].shape[0]), 'constant', constant_values=0.0)
        fst_test.shape = (1, fst_test.shape[0])
        scnd_test.shape = (1, scnd_test.shape[0])
        print('Accuracy:', accuracy.eval({x: fst_test, x2: scnd_test, y: input_data[0][2]}))

train_neural_network(x, x2)
