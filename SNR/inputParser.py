import os


def delete_jpgs():
    files = folders = 0

    for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Training'):
        files += len(filenames)
        folders += len(dirnames)
        print(dirpath)
        for f in filenames:
            if f.endswith('.jpg'):
                os.remove(os.path.join(dirpath, f))

    print("{:,} files, {:,} folders".format(files, folders))


def check_csvs():
    files = folders = 0
    count_dict = {}

    for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Test'):
        files += len(filenames)
        folders += len(dirnames)
        print(dirpath)
        for f in filenames:
            if f.endswith('.csv'):
                with open(os.path.join(dirpath, f), 'r') as file:
                    content = file.read()
                    count = content.count(',')
                    content_one_line = content.replace('\n', ',')
                    if count in count_dict:
                        count_dict[count] += 1
                    else:
                        count_dict[count] = 1

    print("{:,} files, {:,} folders".format(files, folders))
    print(count_dict)


def make_csvs_one_line():
    for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Training'):

        print(dirpath)
        for f in filenames:
            if f.endswith('.csv'):
                with open(os.path.join(dirpath, f), 'r') as file:
                    content = file.read()
                    content_one_line = content.replace('\n', ',')
                with open(os.path.join(dirpath, f), 'w') as file:
                    file.write(content_one_line)


def make_one_big_file():
    with open(os.path.join('obrazki', 'Training_all.csv'), 'w') as big_file:
        for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Training'):
            print(dirpath)
            for f in filenames:
                if f.endswith('.csv'):
                    with open(os.path.join(dirpath, f), 'r') as file:
                        content = file.read()
                        content_one_line = content.replace('\n', ',')
                        content_one_line = content_one_line[:-1]
                        big_file.write(content_one_line)
                        big_file.write('\n')


def make_one_big_output_file():
    dirname_set = set()
    for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Training'):
        for dirname in dirnames:
            dirname_set.add(dirname)
    dirname_list = list(dirname_set)
    dirname_list = sorted(dirname_list)

    with open(os.path.join('obrazki', 'Training_output_all.csv'), 'w') as big_file:
        for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Training'):
            print(dirpath)
            for f in filenames:
                string_to_write = ''
                if f.endswith('.csv'):
                    for i in range(len(dirname_list)):
                        if i == dirname_list.index(os.path.basename(dirpath)):
                            string_to_write+='1'
                        else:
                            string_to_write+='0'
                        string_to_write+=','
                    big_file.write(string_to_write[:-1])
                    big_file.write('\n')


def make_one_big_output_file_svm():
    dirname_set = set()
    for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Training'):
        for dirname in dirnames:
            dirname_set.add(dirname)
    dirname_list = list(dirname_set)
    dirname_list = sorted(dirname_list)

    with open(os.path.join('obrazki', 'Test_output_all.csv'), 'w') as big_file:
        for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Test'):
            print(dirpath)
            string_to_write = ''
            for f in filenames:
                if f.endswith('.jpg'):
                    string_to_write+=str(dirname_list.index(os.path.basename(dirpath)))
                    string_to_write+=','
            big_file.write(string_to_write)


if __name__ == '__main__':
    make_one_big_output_file_svm()
