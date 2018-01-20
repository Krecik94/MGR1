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

# Parsing input to create proper list of sentence pairs
sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.images.wa')

# Creating input and output data sets to be fed into neural network
input_data, output_data = input_parser.create_input_output_data_scores(sentence_pairs,model)


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
network_model.add(Dense(300, activation='relu', input_shape=(200,)))

# Add one hidden layer

network_model.add(Dense(50, activation='relu'))

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

# Creating neural network
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

# Training neural network
network_model.fit(input_data, output_data,
          batch_size=64, epochs=100)


sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\STSint.gs.headlines.wa')
print(len(sentence_pairs))

input_data = None
output_data = None

input_data, output_data = input_parser.create_input_output_data_scores(sentence_pairs,model)

print(input_data.shape)
print(output_data.shape)

# Testing neural network
y_pred = network_model.predict(input_data)
print(network_model.evaluate(input_data,output_data, batch_size=20, verbose=1))

print(confusion_matrix(output_data.argmax(axis=1),y_pred.argmax(axis=1)))

# Precision
print(precision_score(output_data.argmax(axis=1), y_pred.argmax(axis=1),average='micro'))

# # Recall
# print(recall_score(output_data.argmax(axis=1), y_pred.argmax(axis=1),average='micro'))
#
# # F1 score
# print(f1_score(output_data.argmax(axis=1), y_pred.argmax(axis=1),average='micro'))


#score = network_model.evaluate(input_data, output_data,verbose=1)

#print(score)
# test_input = np.array([np.concatenate((model.wv['dog'], model.wv['dog']))], ndmin=2)
# pred_test = network_model.predict(test_input)
# print (pred_test)
#     #,
#      #     validation_data=(x_val, y_val))