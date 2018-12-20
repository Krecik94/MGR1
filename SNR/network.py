from keras.layers import Dense
import pandas as pd
from keras.models import Sequential
from sklearn.metrics import confusion_matrix, f1_score


def main():
    # Data used for training
    test_input_data = pd.read_csv('obrazki\\Test_all.csv', header=None, nrows=1000)
    print('loaded test_input_data')
    test_output_data = pd.read_csv('obrazki\\Test_output_all.csv', header=None, nrows=1000)
    print('loaded test_output_data')

    training_input_data = pd.read_csv('obrazki\\Test_all.csv', header=None, nrows=1000)
    print('loaded training_input_data')
    training_output_data = pd.read_csv('obrazki\\Test_output_all.csv', header=None, nrows=1000)
    print('loaded training_output_data')

    print(test_input_data)
    print(test_output_data)
    print('test')

    # creating model
    network_model = Sequential()
    network_model.add(Dense(100, activation='relu', input_shape=(10000,)))

    # Add one hidden layer
    #network_model.add(Dense(500, activation='relu'))
    # Add one hidden layer
    network_model.add(Dense(100, activation='relu'))

    # Add an output layer
    network_model.add(Dense(87, activation='softmax'))

    # Creating neural network
    network_model.compile(loss='categorical_crossentropy',
                          optimizer='rmsprop',
                          metrics=['accuracy'])

    print('starting training')
    # Training neural network
    network_model.fit(training_input_data, training_output_data,
                      batch_size=64, epochs=20)
    print('finished training')

    y_pred = network_model.predict(test_input_data)
    print(network_model.evaluate(test_input_data, test_output_data, batch_size=20, verbose=1))
    print(network_model.metrics_names)
    # Confusion matrix
    print("Confusion matrix :\n", confusion_matrix(test_output_data.argmax(axis=1), y_pred.argmax(axis=1)))

    # F1 score
    print("F1 :", f1_score(test_output_data.argmax(axis=1), y_pred.argmax(axis=1), average='micro'))


if __name__ == '__main__':
    main()
