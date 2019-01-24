from joblib import dump, load
import numpy as np
from keras.applications import VGG16
from keras.preprocessing.image import ImageDataGenerator, load_img, img_to_array


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

    features = model.predict_generator(test_generator, verbose=1)

    features_list = []

    for feature in features:
        feature_np = np.array(feature)
        features_list.append(feature_np.flatten())

    feature_list_np = np.array(features_list)

    clf = load("svm_model.joblib")

    print(clf.predict(feature_list_np))



if __name__ == '__main__':
    main()