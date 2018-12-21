import random

import keras
from keras.layers import Dense
import pandas as pd
from keras.models import Sequential
import functools

# Creating additional measures
top3_acc = functools.partial(keras.metrics.top_k_categorical_accuracy, k=3)
top3_acc.__name__ = 'top3_acc'

top2_acc = functools.partial(keras.metrics.top_k_categorical_accuracy, k=2)
top2_acc.__name__ = 'top2_acc'


def main():
    # picking ranodm number of rows from the data file
    n = 45171  # number of records in file
    s = 5000  # desired sample size
    skip = sorted(random.sample(range(n), n - s))

    # Data used for testing
    test_input_data = pd.read_csv('obrazki\\Test_all.csv', header=None, skiprows=skip)
    print('loaded test_input_data')
    test_output_data = pd.read_csv('obrazki\\Test_output_all.csv', header=None, skiprows=skip)
    print('loaded test_output_data')

    # Data used for training
    training_input_data = pd.read_csv('obrazki\\Training_all.csv', header=None, skiprows=skip)
    print('loaded training_input_data')
    training_output_data = pd.read_csv('obrazki\\Training_output_all.csv', header=None, skiprows=skip)
    print('loaded training_output_data')

    # creating model
    network_model = Sequential()
    # input layer
    network_model.add(Dense(10000, activation='relu', input_shape=(10000,)))
    # hidden layers
    # output layer
    network_model.add(Dense(87, activation='softmax'))

    # Compiling neural network
    network_model.compile(loss='categorical_crossentropy',
                          optimizer='adam',
                          metrics=['accuracy', 'top_k_categorical_accuracy', top2_acc, top3_acc])

    print('starting training')
    # Training neural network
    network_model.fit(training_input_data, training_output_data,
                      batch_size=1000, epochs=20)
    print('finished training')

    # calculating prediction for debugging purposes
    y_pred = network_model.predict(test_input_data)
    print('evaluating')
    print(network_model.evaluate(test_input_data, test_output_data, batch_size=20, verbose=1))
    print('evaluating finished')
    print(network_model.metrics_names)

if __name__ == '__main__':
    main()
