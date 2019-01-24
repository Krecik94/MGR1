import pandas as pd
import numpy as np

from joblib import dump, load
from sklearn import svm
from keras.preprocessing.image import ImageDataGenerator, load_img, img_to_array
from keras.applications.vgg16 import VGG16

def main():

    # load the model
    model = VGG16(weights='imagenet', include_top=False, input_shape=(100, 100, 3))

    train_datagen = ImageDataGenerator(
        rescale=1. / 255,
        rotation_range=20,
        width_shift_range=0.2,
        height_shift_range=0.2,
        horizontal_flip=True,
        fill_mode='nearest')

    validation_datagen = ImageDataGenerator(rescale=1. / 255)

    train_batchsize = 50
    val_batchsize = 50

    train_generator = train_datagen.flow_from_directory(
        'obrazki//fruits-360//Training//',
        target_size=(100, 100),
        batch_size=train_batchsize,
        class_mode='categorical')

    test_generator = validation_datagen.flow_from_directory(
        'obrazki//fruits-360//Test//',
        target_size=(100, 100),
        batch_size=val_batchsize,
        class_mode='categorical',
        shuffle=False)

    features = model.predict_generator(train_generator, verbose=1)

    features_list = []

    for feature in features:
        feature_np = np.array(feature)
        features_list.append(feature_np.flatten())

    feature_list_np = np.array(features_list)

    training_output_data = pd.read_csv('obrazki\\Training_output_all.csv', header=None)

    training_output_np = np.array(training_output_data).flatten()

    clf = svm.SVC(gamma='scale', decision_function_shape='ovo')
    clf.fit(feature_list_np, training_output_np)

    dump(clf, 'svm_model.joblib')

    # load an image from file
    image = load_img('obrazki//fruits-360//Training//Tomato Maroon//2_100.jpg', target_size=(100, 100))
    # convert the image pixels to a numpy array
    image = img_to_array(image)
    # reshape data for the model
    image = image.reshape((1, image.shape[0], image.shape[1], image.shape[2]))
    # prepare the image for the VGG model

    img_datagen = ImageDataGenerator(rescale=1. / 255)
    img_generator = img_datagen.flow(image)

    feature1 = model.predict_generator(img_generator, verbose=1)
    feature1_np = np.array(feature1).flatten()

    print(clf.predict(feature1_np[0]))


if __name__ == '__main__':
    main()