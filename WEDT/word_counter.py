with open('Data\\ISTS\\test_data_w_fold_standard\\STSint.input.images.sent1.txt', 'r') as f:
    word_dict = {}
    while(True):
        line = f.readline()
        if line == '':
            break
        word_list = line.split(' ')
        for word in word_list:
            word_dict[word] = ""
        print (word_dict.keys())

