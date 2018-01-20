from keras.models import Sequential
from sklearn.metrics import confusion_matrix, precision_score, recall_score, f1_score, cohen_kappa_score

from keras.layers import LSTM, Dense
import numpy as np
import input_parser
import w2v_model

model = w2v_model.create_m2v_model(['Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent1.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent2.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.images.sent1.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.images.sent2.txt',
                                    'Data/ISTS/Train_data/STSint.input.headlines.sent1.txt',
                                    'Data/ISTS/Train_data/STSint.input.headlines.sent2.txt',
                                    'Data/ISTS/Train_data/STSint.input.images.sent1.txt',
                                    'Data/ISTS/Train_data/STSint.input.images.sent2.txt'])

sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.images.wa')
print(len(sentence_pairs))

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

print(input_data.shape)
print(output_data.shape)

data_dim = 200
timesteps = 8
num_classes = 7

# # expected input data shape: (batch_size, timesteps, data_dim)
# model = Sequential()
# model.add(LSTM(32, return_sequences=True,
#                input_shape=(timesteps, data_dim)))  # returns a sequence of vectors of dimension 32
# model.add(LSTM(32, return_sequences=True))  # returns a sequence of vectors of dimension 32
# model.add(LSTM(32))  # return a single vector of dimension 32
# model.add(Dense(7, activation='softmax'))

# Initialize the constructor
network_model = Sequential()

# Add an input layer
network_model.add(Dense(200, activation='relu', input_shape=(200,)))

# Add one hidden layer
network_model.add(Dense(100, activation='relu'))

# Add an output layer
network_model.add(Dense(7, activation='softmax'))

# network_model output shape
network_model.output_shape

# network_model summary
network_model.summary()

# network_model config
network_model.get_config()

# List all weight tensors
network_model.get_weights()

network_model.compile(loss='categorical_crossentropy',
              optimizer='rmsprop',
              metrics=['accuracy'])

# # Generate dummy training data
# x_train = np.random.random((1000, timesteps, data_dim))
# y_train = np.random.random((1000, num_classes))
#
# # Generate dummy validation data
# x_val = np.random.random((100, timesteps, data_dim))
# y_val = np.random.random((100, num_classes))
#
network_model.fit(input_data, output_data,
          batch_size=64, epochs=100)


sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.headlines.wa')
print(len(sentence_pairs))

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

print(input_data.shape)
print(output_data.shape)

y_pred = network_model.predict(input_data)

print(y_pred)
print(input_data)
#confusion_matrix(output_data, y_pred)

score = network_model.evaluate(input_data, output_data,verbose=1)

print(score)
# test_input = np.array([np.concatenate((model.wv['dog'], model.wv['dog']))], ndmin=2)
# pred_test = network_model.predict(test_input)
# print (pred_test)
#     #,
#      #     validation_data=(x_val, y_val))