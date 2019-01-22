import random

import keras
from keras.layers import Dense
import pandas as pd
from keras.models import Sequential
import functools
from keras.preprocessing.image import load_img
from keras.preprocessing.image import img_to_array
from keras.applications.vgg16 import preprocess_input
from keras.applications.vgg16 import decode_predictions
from keras.applications.vgg16 import VGG16

def main():

    # load the model
    model = VGG16()
    # load an image from file
    image = load_img('obrazki//fruits-360//Training//Apple Braeburn//r_115_100.jpg', target_size=(224, 224))
    # convert the image pixels to a numpy array
    image = img_to_array(image)
    # reshape data for the model
    image = image.reshape((1, image.shape[0], image.shape[1], image.shape[2]))
    # prepare the image for the VGG model
    image = preprocess_input(image)
    # predict the probability across all output classes
    yhat = model.predict(image)
    # convert the probabilities to class labels
    label = decode_predictions(yhat)
    # retrieve the most likely result, e.g. highest probability
    label = label[0][0]
    # print the classification
    print('%s (%.2f%%)' % (label[1], label[2] * 100))








    '''
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
    network_model.add(Dense(2000, activation='relu', input_shape=(10000,)))
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
'''


if __name__ == '__main__':
    main()
