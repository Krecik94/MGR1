from keras.models import Sequential
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

input_data = np.array([])
output_data = np.array([])

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

        input_data = np.append(input_data, np.concatenate(first_input, second_input))

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

        output_data = np.append(output_data, output)

print(input_data.shape)
print(output_data.shape)

data_dim = 200
timesteps = 8
num_classes = 7

# expected input data shape: (batch_size, timesteps, data_dim)
model = Sequential()
model.add(LSTM(32, return_sequences=True,
               input_shape=(timesteps, data_dim)))  # returns a sequence of vectors of dimension 32
model.add(LSTM(32, return_sequences=True))  # returns a sequence of vectors of dimension 32
model.add(LSTM(32))  # return a single vector of dimension 32
model.add(Dense(7, activation='softmax'))

model.compile(loss='categorical_crossentropy',
              optimizer='rmsprop',
              metrics=['accuracy'])

# Generate dummy training data
x_train = np.random.random((1000, timesteps, data_dim))
y_train = np.random.random((1000, num_classes))

# Generate dummy validation data
x_val = np.random.random((100, timesteps, data_dim))
y_val = np.random.random((100, num_classes))

model.fit(x_train, y_train,
          batch_size=64, epochs=5)
    #,
     #     validation_data=(x_val, y_val))