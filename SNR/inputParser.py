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
    with open(os.path.join('obrazki', 'Test_all.csv'), 'w') as big_file:
        for dirpath, dirnames, filenames in os.walk('obrazki\\fruits-360\\Test'):
                print(dirpath)
                for f in filenames:
                    if f.endswith('.csv'):
                        with open(os.path.join(dirpath, f), 'r') as file:
                            content = file.read()
                            content_one_line = content.replace('\n', ',')
                            content_one_line = content_one_line[:-1]
                            big_file.write(content_one_line)
                            big_file.write('\n')

if __name__ == '__main__':
    make_one_big_file()
