import tensorflow as tf
import re
from gensim.models import word2vec
import numpy as np

input_data1 = []
input_data2 = []
gs = []
input_file = open("Data/STS/Train_data/STS.input.images.txt", "r")
gs_file = open("Data/STS/Train_data/STS.gs.images.txt", "r")

#sentences = word2vec.Text8Corpus('text8')
w2v = word2vec.KeyedVectors.load_word2vec_format('deps.words')#word2vec.Word2Vec(sentences, size=200)#

#w2v.wv.save_word2vec_format('test')

#Regex for taking out special characters
regex = re.compile('[^a-zA-Z]')

for line in input_file:
    if line != "\n":
        val = line.split("\t")
        tmp1 = []
        tmp2 = []
        split = re.split('\s|-', val[1])
        for x in re.split('\s|-', val[0]):  #.split(" "):
            if regex.sub('', x.lower()) != '':
                tmp1.append(w2v.wv[regex.sub('', x.lower())])
        for x in re.split('\s|-', val[1]):  #.split(" "):
            if regex.sub('', x.lower()) != '':
                tmp2.append(w2v.wv[regex.sub('', x.lower())])
        input_data1.append(tmp1)
        input_data2.append(tmp2)
for line in gs_file:
    if line != "\n":
        gs.append(line)

input_data = []
for x in range(len(input_data1)):
    input_data1[x].extend([0] * (30 - len(input_data1[x])))
    input_data2[x].extend([0] * (30 - len(input_data2[x])))
    input_data.append((input_data1[x], input_data2[x]))


n_nodes_hl1 = 500
n_nodes_hl2 = 500
n_nodes_hl3 = 500
n_classes = 1
batch_size = 100


x = tf.placeholder(tf.float32, [None, 30])
x2 = tf.placeholder(tf.float32, [None, 30])
y = tf.placeholder(tf.float32)

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
    hidden_1_layer_1 = {'weights': tf.Variable(tf.random_normal([30, n_nodes_hl1])),
                        'biases': tf.Variable(tf.random_normal([n_nodes_hl1]))}

    hidden_1_layer_2 = {'weights': tf.Variable(tf.random_normal([30, n_nodes_hl1])),
                        'biases': tf.Variable(tf.random_normal([n_nodes_hl1]))}

    hidden_2_layer = {'weights1': tf.Variable(tf.random_normal([n_nodes_hl1, n_nodes_hl2])),
                      'weights2': tf.Variable(tf.random_normal([n_nodes_hl1, n_nodes_hl2])),
                      'biases': tf.Variable(tf.random_normal([n_nodes_hl2]))}

    output_layer = {'weights': tf.Variable(tf.random_normal([n_nodes_hl2, n_classes])),
                    'biases': tf.Variable(tf.random_normal([n_classes]))}

    """
    Definition of nodes in layers
    """
    print(data[2:-2])
    l1_1 = tf.add(tf.matmul(data, hidden_1_layer_1['weights']), hidden_1_layer_1['biases'])
    l1_1 = tf.nn.relu(l1_1)

    l1_2 = tf.add(tf.matmul(data2, hidden_1_layer_2['weights']), hidden_1_layer_2['biases'])
    l1_2 = tf.nn.relu(l1_2)

    l2 = tf.add(tf.add(tf.matmul(l1_1, hidden_2_layer['weights1']), tf.matmul(l1_2, hidden_2_layer['weights2'])), hidden_2_layer['biases'])
    l2 = tf.nn.relu(l2)

    output = tf.matrix_transpose(tf.matmul(l2, output_layer['weights']) + output_layer['biases'])

    return output

def train_neural_network(x, x2):
    """
    Training neural network
    :param x: Placeholder for input data (???)
    :return: none
    """
    prediction = neural_network_model(x, x2)
    cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=prediction, labels=y))
    optimizer = tf.train.AdamOptimizer().minimize(cost)

    hm_epochs = 20
    with tf.Session() as sess:
        sess.run(tf.global_variables_initializer())
        for epoch in range(hm_epochs):
            epoch_loss = 0
            _, c = sess.run([optimizer, cost], feed_dict={x: input_data1, x2: input_data2, y: gs})
            epoch_loss += c

            print('Epoch', epoch, 'completed out of', hm_epochs, 'loss:', epoch_loss)
        correct = tf.equal(tf.argmax(prediction), tf.argmax(y))
        accuracy = tf.reduce_mean(tf.cast(correct, 'float'))
        print('Accuracy:', accuracy.eval({x: input_data1,x2: input_data2, y: gs}))

train_neural_network(x, x2)
