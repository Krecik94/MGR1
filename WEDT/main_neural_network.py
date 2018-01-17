import tensorflow as tf
import random
from gensim.models import word2vec
import numpy as np
import input_parser
import w2v_model

sentence_1 = tf.placeholder(tf.float32, [None, 100])  # first sentence
sentence_2 = tf.placeholder(tf.float32, [None, 100])  # second sentence
result = tf.placeholder(tf.float32)  # target values

model = w2v_model.create_m2v_model(['Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent1.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent2.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.images.sent1.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.images.sent2.txt'])


def main():
    sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.images.wa')
    print(len(sentence_pairs))

    train_neural_network(sentence_1, sentence_2)


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
    n_nodes_hl1 = 1000
    n_nodes_hl2 = 1000
    n_nodes_hl3 = 500
    n_classes = 7

    hidden_1_layer_1 = {'weights': tf.Variable(tf.truncated_normal([100, n_nodes_hl1],
                                                                   stddev=1.0 / np.sqrt(100)),
                                               name='weights11'),
                        'biases': tf.zeros([n_nodes_hl1], name='biasesl1')}  # tf.Variable(tf.random_normal([n_nodes_hl1]), name='biases11')}

    hidden_1_layer_2 = {'weights': tf.Variable(tf.truncated_normal([100, n_nodes_hl1],
                                                                   stddev=1.0 / np.sqrt(100)),
                                               name='weights12'),
                        'biases': tf.zeros([n_nodes_hl1], name='biases12')}  # tf.Variable(tf.random_normal([n_nodes_hl1]), name='biases12')}

    hidden_2_layer = {'weights1': tf.Variable(tf.truncated_normal([n_nodes_hl1, n_nodes_hl2],
                                                                  stddev=1.0 / np.sqrt(100)),
                                              name='weights21'),
                      'weights2': tf.Variable(tf.truncated_normal([n_nodes_hl1, n_nodes_hl2],
                                                                  stddev=1.0 / np.sqrt(100)),
                                              name='weights22'),
                      'biases': tf.zeros([n_nodes_hl2], name='biases2')}  # tf.Variable(tf.random_normal([n_nodes_hl2]), name='biases2')}

    output_layer = {'weights': tf.Variable(tf.truncated_normal([n_nodes_hl2, n_classes],
                                                               stddev=1.0 / np.sqrt(100)),
                                           name='weights_out'),
                    'biases': tf.zeros([n_classes], name='biases_out')}  # tf.Variable(tf.random_normal([n_classes]), name='biases_out')}

    """
    Definition of nodes in layers
    """
    l1_1 = tf.add(tf.matmul(data, hidden_1_layer_1['weights']), hidden_1_layer_1['biases'])

    l1_2 = tf.add(tf.matmul(data2, hidden_1_layer_2['weights']), hidden_1_layer_2['biases'])
    #l1_2 = tf.nn.relu(l1_2)

    l2 = tf.add(tf.add(tf.matmul(l1_1, hidden_2_layer['weights1']), tf.matmul(l1_2, hidden_2_layer['weights2'])),
                hidden_2_layer['biases'])
    #l2 = tf.nn.relu(l2)

    output = tf.matmul(l2, output_layer['weights']) + output_layer['biases']
    #output = tf.nn.relu(output)

    return output

def test_cost(first, second):

    #first_max = np.max(first)


    return tf.nn.softmax_cross_entropy_with_logits(logits=first,labels=second)

def train_neural_network(sentence_1, sentence_2):
    """
    Training neural network
    :param sentence_1: Placeholder for input data (???)
    :return: none
    """
    prediction = neural_network_model(sentence_1, sentence_2)

    cost = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=prediction, labels=result))
    #cost = tf.reduce_mean(-tf.reduce_sum(tf.multiply(result, tf.log(tf.nn.softmax(logits=prediction) + 1e-10))))
    #cost = test_cost(prediction,result)
    optimizer = tf.train.GradientDescentOptimizer(0.05).minimize(cost)  # GradientDescentOptimizer(1.0).minimize(cost)

    sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.images.wa')

    hm_epochs = 40
    with tf.Session() as sess:
        sess.run(tf.global_variables_initializer())

        first_test_input = np.zeros(100)
        second_test_input = np.zeros(100)
        result_test_output = np.zeros(7)



        first_test_input.shape = (1, first_test_input.shape[0])
        second_test_input.shape = (1, second_test_input.shape[0])
        result_test_output.shape = (1, result_test_output.shape[0])

        #print(sess.run(cost, feed_dict={sentence_1: first_test_input, sentence_2: second_test_input, result: result_test_output}))

        #print(prediction.eval({sentence_1: first_test_input, sentence_2: second_test_input}))

        for epoch in range(hm_epochs):
            epoch_loss = 0
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


                    # print(output)
                    # print(first_input)
                    first_input.shape = (1, first_input.shape[0])
                    second_input.shape = (1, second_input.shape[0])

                    _, c = sess.run([optimizer, cost],
                                    feed_dict={sentence_1: first_input, sentence_2: second_input, result: output})

                    #print(sess.run(tf.log(tf.nn.softmax(logits=prediction)), feed_dict={sentence_1: first_input, sentence_2: second_input}))

                    if epoch > 38:
                        print('prediction: ', sess.run(prediction,
                                                      feed_dict={sentence_1: first_input, sentence_2: second_input}),
                              output)

                    #print(sess.run(cost,feed_dict={sentence_1: first_input, sentence_2: second_input, result: output}))


                    #print(prediction.eval({sentence_1: first_input, sentence_2: second_input}))
                    epoch_loss += c

            print('Epoch', epoch, 'completed out of', hm_epochs, 'loss:', epoch_loss)
        correct = tf.equal(prediction, result)
        accuracy = tf.reduce_mean(tf.cast(correct, 'float'))

        first_test_input = np.zeros(100)
        second_test_input = np.zeros(100)

        first_test_input.shape = (1, first_test_input.shape[0])
        second_test_input.shape = (1, second_test_input.shape[0])

        print('Out: ', sess.run(prediction,  feed_dict={sentence_1: first_test_input, sentence_2: second_test_input}))

        '''for i in range(5):
            
            print('Accuracy:', prediction.eval({sentence_1: fst_test, sentence_2: scnd_test}), input_data[i][2],
                  prediction.eval({sentence_1: fst_test, sentence_2: scnd_test}) / input_data[i][
                      2])  # , y: input_data[0][2]}))
        omg = sess.run([v for v in tf.trainable_variables() if v.name == "weights_out:0"])
        print(omg)'''


if __name__ == "__main__":
    main()
