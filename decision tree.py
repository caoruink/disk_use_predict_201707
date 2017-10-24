# -*- coding: utf-8 -*-
import numpy as np
from sklearn.tree import DecisionTreeRegressor
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import KFold
# from sklearn.ensemble import AdaBoostClassifier
import time


class SampleFile(object):
    """
    针对一个数据文件进行预测，首先按照target的个数区分X和Y，然后预测并计算结果
    """
    def __init__(self, file_route, filename, num_target):
        """
        初始化一些参数
        :param file_route:文件的路径
        :param filename: 文件的名字
        :param num_target: target的数量
        """
        self.__file_route = file_route
        self.__filename = filename
        sample_file = open(self.__file_route + "\\" + self.__filename, 'r')
        self.__sample_x = None
        self.__sample_y = None
        self.__hostname = []
        self.__num_feature = 0
        self.__num_target = num_target
        self.__sample_content = sample_file.readlines()
        sample_file.close()
        del sample_file
        # 日志文件
        self.outfile = open(self.__file_route + "\\result" + self.__filename.split(".")[0] + ".txt", 'w')
        self.outfile.write("Begin " + self.__filename + " at " + time.strftime('%Y-%m-%d %H:%M:%S',
                                                                               time.localtime(time.time())) + "\n")
        self.error = []
        self.__num_split = 10

    def separate_XY(self):
        """
        区分feature和target
        :return:
        """
        self.outfile.write(
            "separate X and Y at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
        all_features = []
        for each in self.__sample_content:
            line = each.split(",")
            self.__hostname.append(line[0])
            line = line[1:len(line)]
            new_line = []
            for every in line:
                new_line.append(float(every))
            all_features.append(new_line)
        self.__sample_content = np.array(all_features)
        del all_features
        self.__num_feature = len(self.__sample_content[0, ]) - self.__num_target
        self.__hostname = np.array(self.__hostname)
        self.__sample_x = self.__sample_content[:, 0: self.__num_feature]
        self.__sample_y = self.__sample_content[:, self.__num_feature: self.__num_feature + self.__num_target]
        self.outfile.write(
            "end separate X and Y at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
        return

    @staticmethod
    def cal_error_class(y_test, y_predict):
        """
        计算预警准确的和误报率
        :param y_test: 原始值
        :param y_predict: 与测试
        :return: 预警准确率和误报率
        """
        TP = 0  # BOTH 0,
        FP = 0  # PRE 0, ACT 1
        FN = 0  # PRE 1, ACT 0
        TN = 0  # BOTH 1
        for index in np.arange(0, len(y_test)):
            if y_test[index] == 1:
                if y_predict[index] == 1:
                    TN += 1
                else:
                    FP += 1
            else:
                if y_predict[index] == 1:
                    FN += 1
                else:
                    TP += 1
        # print(TN, TN + FP)
        alarm_rate = TN / (TN + FP)
        error_rate = (FN + FP) / (TP + FN + TN + FP)
        # print(alarm_rate, error_rate)
        return alarm_rate, error_rate

    @staticmethod
    def cal_error_reg(y_test, y_predict):
        """
        计算RMSE
        :param y_test: 原始值
        :param y_predict: 预测值
        :return: 误差
        """
        residual = 0
        for index in np.arange(0, len(y_test)):
            residual += np.square(y_test - y_predict)
        return np.sqrt(residual/len(y_test))

    def fit_class(self, x_train, y_train, x_test, y_test):
        """
        做分类预测
        :param x_train:
        :param y_train:
        :param x_test:
        :param y_test:
        :return:
        """
        self.outfile.write(
            "fit class model at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
        # rng = np.random.RandomState(1)
        clf = DecisionTreeClassifier(random_state=1, min_samples_split=2, min_samples_leaf=5, max_depth=7,
                                     class_weight="balanced")
        # clf = AdaBoostClassifier(dt, n_estimators=700, random_state=rng, learning_rate=1.3)
        # print(x_train.shape)
        # print(y_train.shape)
        clf.fit(x_train, y_train)
        y_predict = clf.predict(x_test)
        if self.__num_target == 1:
            alarm_rate, error_rate = self.cal_error_class(y_test, y_predict)
            self.error.append(alarm_rate)
            self.error.append(error_rate)
            self.outfile.write("alarm_rate:" + str(alarm_rate) + "    error_rate:" + str(error_rate) + "\n")
            self.outfile.write(
                "end fit class model at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
            return
        for i in np.arange(0, self.__num_target):
            alarm_rate, error_rate = self.cal_error_class(y_test[:, i], y_predict[:, i])
            self.error.append(alarm_rate)
            self.error.append(error_rate)
            self.outfile.write("alarm_rate:" + str(alarm_rate) + "    error_rate:" + str(error_rate) + "\n")
            self.outfile.write(
                "end fit class model at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
        return

    def fit_reg(self, x_train, y_train, x_test, y_test):
        """
        做回归预测
        :param x_train:
        :param y_train:
        :param x_test:
        :param y_test:
        :return:
        """
        self.outfile.write(
            "fit regression model at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
        clf = DecisionTreeRegressor(random_state=0)
        clf.fit(x_train, y_train)
        y_predict = clf.predict(x_test)
        if self.__num_target == 1:
            rmse = self.cal_error_reg(y_test, y_predict)
            self.error.append(rmse)
            self.outfile.write("RMSE:" + str(rmse) + "\n")
            self.outfile.write(
                "end fit regression model at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")
            return
        for i in np.arange(0, self.__num_target):
            rmse = self.cal_error_reg(y_test[:, i], y_predict[:, i])
            self.outfile.write("RMSE: " + str(rmse) + "\n")
            self.outfile.write(
                "end fit regression model at " + time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())) + "\n")

    def predict_class(self):
        """
        串起预测的整个流程
        :return:
        """
        self.separate_XY()
        kf = KFold(n_splits=self.__num_split, shuffle=True)
        for train_index, test_index in kf.split(self.__sample_x):
            x_train, x_test = self.__sample_x[train_index], self.__sample_x[test_index]
            y_train, y_test = self.__sample_y[train_index], self.__sample_y[test_index]
            self.fit_class(x_train, y_train, x_test, y_test)
            self.outfile.write("\n")
        return

    def predict_reg(self):
        """
        串起预测的整个流程
        :return:
        """
        self.separate_XY()
        kf = KFold(n_splits=self.__num_split, shuffle=True)
        for train_index, test_index in kf.split(self.__sample_x):
            x_train, x_test = self.__sample_x[train_index], self.__sample_x[test_index]
            y_train, y_test = self.__sample_y[train_index], self.__sample_y[test_index]
            self.fit_reg(x_train, y_train, x_test, y_test)
        return

    def solve_error_class(self):
        new_error = []
        for i in np.arange(0, self.__num_split):
            new_error.append(self.error[i * 2 * self.__num_target: (i + 1) * 2 * self.__num_target])
        self.outfile.write(
            "mean alarm rate and mean error rate of " + self.__filename + "is" + str(np.mean(new_error, 0)) + "\n")
        print("mean alarm rate and mean error rate of " + self.__filename + "is" + str(np.mean(new_error, 0)) + "\n")
        return

    def solve_error_reg(self):
        new_error = []
        for i in np.arange(0, self.__num_split):
            new_error.append(self.error[i * self.__num_target: (i + 1) * self.__num_target])
        self.outfile.write("mean RMSE of " + self.__filename + "is" + str(np.mean(new_error, 0)) + "\n")
        print("mean RMSE of " + self.__filename + "is" + str(np.mean(new_error, 0)) + "\n")
        return


if __name__ == '__main__':
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_votesample_2days_labeled.csv", 6)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))

    vote_2days = SampleFile("data\\sample\\labeled", "sample_2days_labeled.csv", 6)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))

    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_0sample_2days_labeled.csv", 1)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_1sample_2days_labeled.csv", 1)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_2sample_2days_labeled.csv", 1)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_3sample_2days_labeled.csv", 1)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_4sample_2days_labeled.csv", 1)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    vote_2days = SampleFile("data\\sample\\labeled", "GBDT_5sample_2days_labeled.csv", 1)
    vote_2days.predict_class()
    vote_2days.solve_error_class()
    vote_2days.outfile.close()
    del vote_2days
    print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))

    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_votesample_7days_labeled.csv", 6)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    #
    # vote_2days = SampleFile("data\\sample\\labeled", "sample_7days_labeled.csv", 6)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    #
    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_0sample_7days_labeled.csv", 1)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_1sample_7days_labeled.csv", 1)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_2sample_7days_labeled.csv", 1)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_3sample_7days_labeled.csv", 1)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_4sample_7days_labeled.csv", 1)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
    # vote_2days = SampleFile("data\\sample\\labeled", "GBDT_5sample_7days_labeled.csv", 1)
    # vote_2days.predict_class()
    # vote_2days.solve_error_class()
    # vote_2days.outfile.close()
    # del vote_2days
    # print(time.strftime('%Y-%m-%d %H:%M:%S', time.localtime(time.time())))
