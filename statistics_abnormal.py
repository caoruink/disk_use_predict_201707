# -*- coding: utf-8 -*-
import csv
import datetime
from datetime import datetime, timedelta
import os
from os.path import join
# the bucket to record the times of abnomal for each ten minute or one day


class bucket(object):
    def __init__(self, character_array):
        # initialization the bucket
        self.length = len(character_array)
        self.time_bucket = dict([(k, 0) for k in character_array])
    def add_bucket(self, file_abnormal):
        # add times of the dict of one file which contains the abnormal information
        # read the file
        information = open(file_abnormal, 'rb')
        information.seek(0)
        line = information.readline()
        # The second line of information is what we need, so readline twice
        line = information.readline().decode('utf-8').strip("\00")
        # the csv file split by ','
        line = line.split(",")
        if len(line) == 2:
            return
        # remove the first unit
        line = line[1 : ]
        for each in line:
            # date and time are split by ' '
            each_split = each.split(" ")
            if self.length == 144:
                self.time_bucket[each_split[1].split('"')[0]] += 1
            else:
                self.time_bucket[each_split[0].split('"')[1]] += 1
    def write_bucket(self, filename):
        # write the bucket information
        outfile = open(filename, 'w', newline='')
        writer = csv.writer(outfile)
        for key in self.time_bucket:
            writer.writerow([key, self.time_bucket[key]])
        outfile.close()


def oneday_timestamp():
    time_start = datetime(2000, 1, 1, 0, 0, 0)
    t = []
    array_index = []
    i = 0
    while i < 144:
        array_index.append(i)
        i += 1
    for i in array_index:
        t.append(time_start + timedelta(minutes=10 * i))
    return t


def week_timestamp():
    time_start = datetime(2017, 1, 31, 0, 0, 0)
    t = []
    array_index = []
    i = 0
    while i < 24:
        array_index.append(i)
        i += 1
    for i in array_index:
        t.append(time_start + timedelta(days=1 * i))
    return t

oneday_array = []
for each in oneday_timestamp():
    oneday_array.append(each.strftime("%Y-%m-%d %H:%M:%S").split(' ')[1])
# print(oneday_array)

week_array = []
for each in week_timestamp():
    week_array.append(each.strftime("%Y-%m-%d %H:%M:%S").split(' ')[0])
# print(week_array)
bucket_day = bucket(oneday_array)
bucket_week = bucket(week_array)

source = 'data\\abnormal_time\\'
for root, dirs, files in os.walk(source):
    for onefile in files:
        print(onefile)
        onefullfilename = join(root, onefile)
        bucket_day.add_bucket(onefullfilename)
        bucket_week.add_bucket(onefullfilename)

bucket_day.write_bucket("data\\abnormal_day.csv")
bucket_week.write_bucket("data\\abnormal_week.csv")
