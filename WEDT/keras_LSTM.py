from keras.models import Sequential
from sklearn.metrics import confusion_matrix, precision_score, recall_score, f1_score, cohen_kappa_score

from keras.layers import LSTM, Dense
import numpy as np
import input_parser
import w2v_model

# .wa file with training data in Data/ISTS/Train_data folder
FILE_TRAIN = 'STSint.gs.images.wa'

# .wa file with testing data in Data/ISTS/test_data_w_fold folder
FILE_TEST = 'STSint.gs.images.wa'

# Variable describing if neural network should check for scores or etiquettes
# Vaules: 'scores', 'etiquettes'
MODE = 'scores'

model = w2v_model.create_m2v_model(['Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent1.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.headlines.sent2.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.images.sent1.txt',
                                    'Data/ISTS/test_data_w_fold_standard/STSint.input.images.sent2.txt',
                                    'Data/ISTS/Train_data/STSint.input.headlines.sent1.txt',
                                    'Data/ISTS/Train_data/STSint.input.headlines.sent2.txt',
                                    'Data/ISTS/Train_data/STSint.input.images.sent1.txt',
                                    'Data/ISTS/Train_data/STSint.input.images.sent2.txt'])

# Parsing input to create proper list of sentence pairs
sentence_pairs = input_parser.parse_input('Data\\ISTS\\Train_data\\' + FILE_TRAIN)

# Creating input and output data sets to be fed into neural network
if MODE == 'scores':
    input_data, output_data = input_parser.create_input_output_data_scores(sentence_pairs, model)
elif MODE == 'etiquettes':
    input_data, output_data = input_parser.create_input_output_data_etiquettes(sentence_pairs, model)

data_dim = 100
timesteps = 2
if MODE == 'scores':
    num_classes = 7
elif MODE == 'etiquettes':
    num_classes = 8

# expected input data shape: (batch_size, timesteps, data_dim)
network_model = Sequential()
network_model.add(LSTM(32, return_sequences=True,
               input_shape=(timesteps, data_dim)))  # returns a sequence of vectors of dimension 32
network_model.add(LSTM(32, return_sequences=True))  # returns a sequence of vectors of dimension 32
network_model.add(LSTM(32))  # return a single vector of dimension 32
network_model.add(Dense(num_classes, activation='softmax'))

# Creating neural network
network_model.compile(loss='categorical_crossentropy',
              optimizer='rmsprop',
              metrics=['accuracy'])

input_data = np.reshape(input_data, (len(input_data), timesteps, data_dim))

# Training neural network
network_model.fit(input_data, output_data,
          batch_size=64, epochs=20)

sentence_pairs = input_parser.parse_input('Data\\ISTS\\test_data_w_fold_standard\\' + FILE_TEST)

input_data = None
output_data = None

if MODE == 'scores':
    input_data, output_data = input_parser.create_input_output_data_scores(sentence_pairs, model)
elif MODE == 'etiquettes':
    input_data, output_data = input_parser.create_input_output_data_etiquettes(sentence_pairs, model)

input_data = np.reshape(input_data, (len(input_data), timesteps, data_dim))

# Testing neural network
y_pred = network_model.predict(input_data)
print(network_model.evaluate(input_data,output_data, batch_size=20, verbose=1))

# Confusion matrix
print("Confusion matrix :\n", confusion_matrix(output_data.argmax(axis=1),y_pred.argmax(axis=1)))

# F1 score
print("F1 :", f1_score(output_data.argmax(axis=1), y_pred.argmax(axis=1),average='micro'))
