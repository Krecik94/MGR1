import random

import keras
from keras.layers import Dense
import pandas as pd
from keras.models import Sequential
from sklearn.metrics import confusion_matrix, f1_score
import functools

top3_acc = functools.partial(keras.metrics.top_k_categorical_accuracy, k=3)
top3_acc.__name__ = 'top3_acc'

top2_acc = functools.partial(keras.metrics.top_k_categorical_accuracy, k=2)
top2_acc.__name__ = 'top2_acc'

top1_acc = functools.partial(keras.metrics.top_k_categorical_accuracy, k=1)
top1_acc.__name__ = 'top1_acc'

def main():
    n = 45171  # number of records in file
    s = 5000  # desired sample size
    skip = sorted(random.sample(range(n), n - s))
    # Data used for training
    test_input_data = pd.read_csv('obrazki\\Test_all.csv', header=None, skiprows=skip)
    print('loaded test_input_data')
    test_output_data = pd.read_csv('obrazki\\Test_output_all.csv', header=None, skiprows=skip)
    print('loaded test_output_data')

    training_input_data = pd.read_csv('obrazki\\Training_all.csv', header=None, skiprows=skip)
    print('loaded training_input_data')
    training_output_data = pd.read_csv('obrazki\\Training_output_all.csv', header=None, skiprows=skip)
    print('loaded training_output_data')

    print(test_input_data)
    print(test_output_data)
    print('test')

    # creating model
    network_model = Sequential()
    network_model.add(Dense(1000, activation='relu', input_shape=(10000,)))

    # Add one hidden layer
    #network_model.add(Dense(1000, activation='relu'))
    # Add one hidden layer

    network_model.add(Dense(200, activation='relu'))
    # Add an output layer
    network_model.add(Dense(87, activation='softmax'))

    # Creating neural network
    network_model.compile(loss='categorical_crossentropy',
                          optimizer='adadelta',
                          metrics=['accuracy', 'top_k_categorical_accuracy', top1_acc, top2_acc, top3_acc])

    print('starting training')
    # Training neural network
    network_model.fit(training_input_data, training_output_data,
                      batch_size=1000, epochs=20)
    print('finished training')

    y_pred = network_model.predict(test_input_data)
    print('evaluating')
    print(network_model.evaluate(test_input_data, test_output_data, batch_size=20, verbose=1))
    print('evaluating finished')
    print(network_model.metrics_names)
    # Confusion matrix
    #print("Confusion matrix :\n", confusion_matrix(test_output_data.argmax(axis=1), y_pred.argmax(axis=1)))

    # F1 score
    #print("F1 :", f1_score(test_output_data.argmax(axis=1), y_pred.argmax(axis=1), average='micro'))


if __name__ == '__main__':
    main()
