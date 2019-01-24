import random

import numpy as np
import keras
from keras.layers import Dense
import pandas as pd
from keras.models import Sequential, model_from_json
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
    # load json and create model
    json_file = open('model_w_o_aug.json', 'r')
    loaded_model_json = json_file.read()
    json_file.close()
    loaded_model = model_from_json(loaded_model_json)
    # load weights into new model
    loaded_model.load_weights("augmentation_trained_5_epochs_100x100.h5")
    print("Loaded model from disk")

    # load an image from file
    image = load_img('obrazki//fruits-360//Training//Tomato Maroon//2_100.jpg', target_size=(100, 100))
    # convert the image pixels to a numpy array
    image = img_to_array(image)
    # reshape data for the model
    image = image.reshape((1, image.shape[0], image.shape[1], image.shape[2]))
    # prepare the image for the VGG model

    img_datagen = ImageDataGenerator(rescale=1. / 255)
    img_generator = img_datagen.flow(image)

    predictions = loaded_model.predict_generator(img_generator, verbose=1)

    # predict the probability across all output classes
    print(predictions[0])
    print(np.unravel_index(np.argmax(predictions[0]), predictions[0].shape))


if __name__ == '__main__':
    main()