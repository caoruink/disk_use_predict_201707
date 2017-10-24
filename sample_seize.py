# -*- coding: utf-8 -*-
import os
from os.path import join
# seize feature from a file contain 7 days' covariate values and 4 response values(marked A or B, 1h, 6h, 12h, 24h)


class sample(object):
    def __init__(self, file_route, host_name):
        # read file
        self.file_sample = open(file_route, 'rb')
        self.out_file_length = 7 * 24 * 6
        self.out_sample = []
        self.out_sample.append(host_name)

    def construct_sample_label(self):
        self.file_sample.seek(0)
        line = self.file_sample.readlines()[2]
        line = line.decode('utf-8').strip("\00")
        line = line.split(",")
        index = 0
        while index < self.out_file_length:
            self.out_sample.append(line[2 + index])
            index += 1
        self.out_sample.append(self.label_sample(line[(2 + self.out_file_length):(2 + self.out_file_length + 6)]))
        self.out_sample.append(self.label_sample(line[(2 + self.out_file_length):(2 + self.out_file_length + 36)]))
        self.out_sample.append(self.label_sample(line[(2 + self.out_file_length):(2 + self.out_file_length + 72)]))
        self.out_sample.append(self.label_sample(line[(2 + self.out_file_length):(2 + self.out_file_length + 144)]))
        self.out_sample.append(self.label_sample(line[(2 + self.out_file_length):(2 + self.out_file_length + 144 * 2)]))
        self.out_sample.append(self.label_sample(line[(2 + self.out_file_length):(2 + self.out_file_length + 144 * 3)]))

    def construct_sample_target(self):
        self.file_sample.seek(0)
        line = self.file_sample.readlines()[2]
        line = line.decode('utf-8').strip("\00")
        line = line.split(",")
        index = 0
        while index < self.out_file_length:
            self.out_sample.append(line[2 + index])
            index += 1

        self.out_sample.append(line[2 + self.out_file_length + 6])
        self.out_sample.append(line[2 + self.out_file_length + 36])
        self.out_sample.append(line[2 + self.out_file_length + 72])
        self.out_sample.append(line[2 + self.out_file_length + 144])
        self.out_sample.append(line[2 + self.out_file_length + 144 * 2])
        self.out_sample.append(line[2 + self.out_file_length + 144 * 3])

    def label_sample(self, sam_array):
        for each in sam_array:
            if float(each.split("\"")[1]) >= 75:
                return "B"
        return "A"

    def get_array(self):
        return self.out_sample

    def __del__(self):
        self.file_sample.close()


if __name__ == '__main__':
    outfile_label = open("data\\sample\\labeled\\sample_7days_labeled.csv", 'w', newline='')
    outfile_unlabel = open("data\\sample\\unlabeled\\sample_7days_unlabeled.csv", 'w', newline='')
    source = 'data\\solved_data\\600'
    for root, dirs, files in os.walk(source):
        for one_file in files:
            print(one_file)
            onefullfilename = join(root, one_file)
            sam = sample(onefullfilename, one_file.split('.')[0])
            sam.construct_sample_target()
            for each in sam.get_array():
                outfile_unlabel.write(each + ',')
            outfile_unlabel.write('\n')
            del sam
            sam = sample(onefullfilename, one_file.split('.')[0])
            sam.construct_sample_label()
            for each in sam.get_array():
                outfile_label.write(each + ',')
            outfile_label.write('\n')
            del sam
    outfile_unlabel.close()
    outfile_label.close()
