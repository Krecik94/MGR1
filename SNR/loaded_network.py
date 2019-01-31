import random

import numpy as np
from sklearn.metrics import roc_curve, auc
import keras
from keras.layers import Dense
import matplotlib as plt
import pandas as pd
from keras.models import Sequential, model_from_json
import functools
from keras.preprocessing.image import load_img, ImageDataGenerator
from keras.preprocessing.image import img_to_array


def main():
    # load json and create model
    json_file = open('model.json', 'r')
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

    img_generator = img_datagen.flow_from_directory(
        'obrazki//fruits-360//Test//',
        target_size=(100, 100),
        batch_size=50,
        shuffle=False)

    test_output_data = pd.read_csv('obrazki\\Test_output_all.csv', header=None)

    predictions = loaded_model.predict_generator(img_generator, verbose=1)
    hits = 0
    cat_count = [0 for row in range(95)]
    hit_count = [0 for row in range(95)]
    for idx, val in enumerate(predictions):
        print(np.argmax(val))
        cat_count[int(test_output_data[idx])] = 1 + cat_count[int(test_output_data[idx])]
        if np.argmax(val) == int(test_output_data[idx]):
            hits += 1
            hit_count[int(test_output_data[idx])] = 1 + hit_count[int(test_output_data[idx])]


    # predict the probability across all output classes
    # print(predictions[0])
    # print(np.unravel_index(np.argmax(predictions[0]), predictions[0].shape))
    print(str(hits) + " / " + str(len(predictions)))
    print(float(hits) / len(predictions))
    for i, val in enumerate(cat_count):
        print(str(i) + " " + str(float(hit_count[i]) / float(cat_count[i])))

    fpr_keras, tpr_keras, thresholds_keras = roc_curve(test_output_data, predictions.ravel())
    auc_keras = auc(fpr_keras, tpr_keras)


if __name__ == '__main__':
    main()