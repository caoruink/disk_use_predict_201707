# -*- coding: utf-8 -*-
from statistics_abnormal import bucket
import datetime
from datetime import datetime, timedelta
import os
from os.path import join

# Statistics of each host of the abnormal situation, then add up them by the cluster distribution.


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


if __name__ == '__main__':
    oneday_array = []
    for each in oneday_timestamp():
        oneday_array.append(each.strftime("%Y-%m-%d %H:%M:%S").split(' ')[1])
    # print(oneday_array)

    week_array = []
    for each in week_timestamp():
        week_array.append(each.strftime("%Y-%m-%d %H:%M:%S").split(' ')[0])
    # print(week_array)
    out_day = []
    out_week = []
    source = 'data\\abnormal_time\\'
    # for each host, collect abnormal information, creat buckte for each one
    for root, dirs, files in os.walk(source):
        for onefile in files:
            print(onefile)
            bucket_day = bucket(oneday_array)
            bucket_week = bucket(week_array)
            onefullfilename = join(root, onefile)
            bucket_day.add_bucket(onefullfilename)
            bucket_week.add_bucket(onefullfilename)
            temp_day = bucket_day.get_time_bucket()
            temp_day["filename"] = onefile.split(".csv")[0]
            temp_week = bucket_week.get_time_bucket()
            temp_week["filename"] = onefile.split(".csv")[0]
            out_day.append(temp_day)
            out_week.append(temp_week)
            del bucket_day, bucket_week

    oneday_array.insert(0, "filename")
    week_array.insert(0, "filename")
    outfile_day = open("data\\day_abnormal_eachhost.csv", 'w', newline='')
    outfile_week = open("data\\week_abnormal_eachhost.csv", 'w', newline='')
    for each_keyname in oneday_array:
        outfile_day.write(each_keyname + ',')
    outfile_day.write('\n')
    for each_keyname in week_array:
        outfile_week.write(each_keyname + ',')
    outfile_week.write('\n')

    for each_host in out_day:
        for key in oneday_array:
            outfile_day.write(str(each_host[key]) + ',')
        outfile_day.write('\n')
    outfile_day.close()
    for each_host in out_week:
        for key in week_array:
            outfile_week.write(str(each_host[key]) + ',')
        outfile_week.write('\n')
    outfile_week.close()

    del outfile_day, out_day
    del outfile_week, out_week
