import random

import keras
from keras.layers import Dense
import pandas as pd
from keras.models import Sequential
import functools
from keras.preprocessing.image import load_img, ImageDataGenerator
from keras.preprocessing.image import img_to_array
from keras.applications.vgg16 import preprocess_input
from keras.applications.vgg16 import decode_predictions
from keras.applications.vgg16 import VGG16
from keras import models
from keras import layers
from keras import optimizers

def main():

    # load the model
    pretrained_model = VGG16(weights='imagenet', include_top=False, input_shape=(224, 224, 3))


    for layer in pretrained_model.layers[:-4]:
        layer.trainable = False

    model = models.Sequential()

    # Add the vgg convolutional base model
    model.add(pretrained_model)

    # Add new layers
    model.add(layers.Flatten())
    model.add(layers.Dense(1024, activation='relu'))
    model.add(layers.Dropout(0.5))
    model.add(layers.Dense(87, activation='softmax'))

    train_datagen = ImageDataGenerator(
        rescale=1. / 255,
        rotation_range=20,
        width_shift_range=0.2,
        height_shift_range=0.2,
        horizontal_flip=True,
        fill_mode='nearest')

    train_batchsize = 100
    val_batchsize = 10

    train_generator = train_datagen.flow_from_directory(
        'obrazki//fruits-360//Training//',
        target_size=(224, 224),
        batch_size=train_batchsize,
        class_mode='categorical')

    # Compile the model
    model.compile(loss='categorical_crossentropy',
                  optimizer=optimizers.RMSprop(lr=1e-4),
                  metrics=['acc'])
    # Train the model
    history = model.fit_generator(
        train_generator,
        steps_per_epoch=train_generator.samples / train_generator.batch_size,
        epochs=30,
        verbose=1)

    '''
    # load an image from file
    image = load_img('obrazki//fruits-360//Training//Huckleberry//3_100.jpg', target_size=(224, 224))
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
