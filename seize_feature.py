# -*- coding: utf-8 -*-
from sklearn.preprocessing import MinMaxScaler
from sklearn.feature_selection import SelectKBest, chi2, f_regression, RFE
from sklearn.linear_model import LogisticRegression
from sklearn.feature_selection import SelectFromModel
from sklearn.ensemble import GradientBoostingClassifier, GradientBoostingRegressor
import numpy as np
# seize feature


class seize_feature(object):
    def __init__(self, file_route, file_name, num_feature, num_target):
        # 读入文件
        self.file_route = file_route
        self.file_name = file_name
        self.num_feature = num_feature
        self.num_target = num_target
        sample_file = open(self.file_route + "\\" + self.file_name, 'r')
        self.sample_x = None
        self.sample_y = None
        self.hostname = []
        self.sample_content = sample_file.readlines()
        sample_file.close()

    def construct_file_content(self):
        # 处理文件，截取需要的部分
        all_features = []
        for each in self.sample_content:
            line = each.split(",")
            self.hostname.append(line[0])
            line = line[1:len(line)]
            new_line = []
            for every in line:
                new_line.append(float(every))
            all_features.append(new_line)
        all_features = np.array(all_features)
        self.hostname = np.array(self.hostname)
        return all_features

    def standard_scaler(self):
        # 数据标准化
        all_feature = self.construct_file_content()
        self.sample_content = MinMaxScaler().fit_transform(all_feature)
        self.sample_x = self.sample_content[:, 0: self.num_feature]
        self.sample_y = self.sample_content[:, self.num_feature: self.num_feature + self.num_target]
        return

    def where_equal(self, select_result):
        # 找出编号
        num_out = len(select_result[0, :])
        index_feature = np.arange(num_out)
        index_all = 0
        out_index = []
        for index in index_feature:
            while index_all < self.num_feature:
                if (self.sample_x[:, index_all] == select_result[:, index]).all():
                    out_index.append(index_all)
                    break
                else:
                    index_all += 1
        return out_index

    def write_select_index(self, mean_name, select_index):
        # 将找出的编号写入文件
        outfile = open(self.file_route + "\\" + mean_name + self.file_name.split(".")[0] + ".txt", "w+")
        for each in select_index:
            outfile.write(str(each) + '\n')
        outfile.close()

    def check_greater(self, num):
        if num >= 2:
            return True
        else:
            return False

    def vote_select_index(self, indexes):
        # 投票得到最终的特征
        out = []
        indexes = np.array(indexes)
        no_feature = set(indexes[0, ]) | set(indexes[1, ]) | set(indexes[2, ]) | set(indexes[3, ]) | \
                     set(indexes[4, ]) | set(indexes[5, ])
        for each in no_feature:
            i = 0
            if each in indexes[0, ]:
                i += 1
            if each in indexes[1, ]:
                i += 1
            if each in indexes[2, ]:
                i += 1
            if each in indexes[3, ]:
                i += 1
            if each in indexes[4, ]:
                i += 1
            if each in indexes[5, ]:
                i += 1
            if self.check_greater(i):
                out.append(each)
        content = []
        print(len(out))
        for each in out:
            content.append(self.sample_x[:, each])
        return np.array(content).T

    def write_select_value(self, mean_name, select_result, y):
        feature_file = open(self.file_route + "\\" + mean_name + self.file_name, "w+", newline='')
        for eachline in np.column_stack((self.hostname, select_result, y)):
            for each in eachline[:-1]:
                feature_file.write(str(each) + ',')
            feature_file.write(str(eachline[-1]) + '\n')
        feature_file.close()
        return

    def chi2_test(self):
        select_indexes = []
        for index in np.arange(self.num_target):
            y = self.sample_y[:, index]
            select_result = SelectKBest(chi2, k=50).fit_transform(self.sample_x, y)
            select_index = self.where_equal(select_result)
            select_indexes.append(select_index)
            self.write_select_index("chi2_" + str(index + 1), select_index)
            if index == 0 or index == 3:
                self.write_select_value("chi2_" + str(index), select_result, y)
        self.write_select_value("chi2_vote", self.vote_select_index(select_indexes), self.sample_y)
        return

    # def chi2_test(self):
    #     # 卡方检验
    #     select_result = SelectKBest(chi2, k=50).fit_transform(self.sample_x, self.sample_y)
    #     outfile = open(self.file_route + "//feature_chi2" + self.file_name.split(".")[0] + ".txt", "w+")
    #     for each in self.where_equal(select_result, 50):
    #         outfile.write(str(each) + '\n')
    #     outfile.close()
    #     feature_file = open(self.file_route + "//feature_chi2" + self.file_name, "w+", newline='')
    #     for eachline in np.column_stack((self.hostname, select_result, self.sample_y)):
    #         for eachnum in eachline:
    #             feature_file.write(str(eachnum) + ',')
    #         feature_file.write('\n')
    #     feature_file.close()
    #     return

    def f_regression(self):
        # 用于回归的计算方差的方法
        select_indexes = []
        for index in np.arange(self.num_target):
            y = self.sample_y[:, index]
            select_result = SelectKBest(f_regression, k=50).fit_transform(self.sample_x, y)
            select_index = self.where_equal(select_result)
            select_indexes.append(select_index)
            self.write_select_index("f_regression_" + str(index + 1), select_index)
            if index == 0 or index == 3:
                self.write_select_value("f_regression_" + str(index), select_result, y)
        self.write_select_value("f_regression_vote", self.vote_select_index(select_indexes), self.sample_y)
        return

    def rfe(self):
        # 递归特征消除法
        select_indexes = []
        for index in np.arange(self.num_target):
            y = self.sample_y[:, index]
            select_result = RFE(estimator=LogisticRegression(), n_features_to_select=50).fit_transform(self.sample_x, y)
            select_index = self.where_equal(select_result)
            select_indexes.append(select_index)
            self.write_select_index("RFE_" + str(index + 1), select_index)
            if index == 0 or index == 3:
                self.write_select_value("RFE_" + str(index), select_result, y)
        self.write_select_value("RFE_vote", self.vote_select_index(select_indexes), self.sample_y)
        return

    def penalty(self):
        # L1 惩罚项
        select_indexes = []
        for index in np.arange(self.num_target):
            y = self.sample_y[:, index]
            select_result = SelectFromModel(LogisticRegression()).fit_transform(self.sample_x, y)
            select_index = self.where_equal(select_result)
            select_indexes.append(select_index)
            self.write_select_index("L2_" + str(index + 1), select_index)
            if index == 0 or index == 3:
                self.write_select_value("L2_" + str(index), select_result, y)
        self.write_select_value("L2_vote", self.vote_select_index(select_indexes), self.sample_y)
        return

    def gbrt(self):
        select_indexes = []
        for index in np.arange(self.num_target):
            y = self.sample_y[:, index]
            select_result = SelectFromModel(GradientBoostingRegressor()).fit_transform(self.sample_x, y)
            select_index = self.where_equal(select_result)
            select_indexes.append(select_index)
            self.write_select_index("GBRT_" + str(index + 1), select_index)
            if index == 0 or index == 3:
                self.write_select_value("GBRT_" + str(index), select_result, y)
        self.write_select_value("GBRT_vote", self.vote_select_index(select_indexes), self.sample_y)
        return

    def gbct(self):
        select_indexes = []
        for index in np.arange(self.num_target):
            y = self.sample_y[:, index]
            select_result = SelectFromModel(GradientBoostingClassifier()).fit_transform(self.sample_x, y)
            select_index = self.where_equal(select_result)
            select_indexes.append(select_index)
            self.write_select_index("GBDT_" + str(index + 1), select_index)
            # if index == 0 or index == 3:
            #     self.write_select_value("GBDT_" + str(index), select_result, y)
            self.write_select_value("GBDT_" + str(index), select_result, y)
        self.write_select_value("GBDT_vote", self.vote_select_index(select_indexes), self.sample_y)
        return


if __name__ == '__main__':
    # file_2days_unlabeled = seize_feature("data\\sample\\unlabeled", "sample_7days_unlabeled.csv", 144 * 2, 6)
    file_2days_labeled = seize_feature("data\\sample\\labeled", "sample_7days_labeled.csv", 144 * 7, 6)

    # file_2days_unlabeled.standard_scaler()
    # file_2days_unlabeled.f_regression()
    # file_2days_unlabeled.gbrt()

    file_2days_labeled.standard_scaler()
    # file_2days_labeled.f_regression()
    # file_2days_labeled.chi2_test()
    # file_2days_labeled.rfe()
    # file_2days_labeled.penalty()
    file_2days_labeled.gbct()



    # clf = ExtraTreesClassifier()
    # clf = clf.fit(sample_x, sample_y)
    # clf.feature_importances_
    # model = SelectFromModel(clf, prefit=True)
    # X_new = model.transform(sample_x)

    # clf = LinearSVC(C=0.01, penalty="l1", dual=False).fit(sample_x, sample_y)
    # model = SelectFromModel(clf, prefit=True)
    # X_new = model.transform(sample_x)
    #
    # print(X_new.shape)
    # i = 0
    # j = 0
    # outfile = open("importance.txt", 'w')
    # for each in clf.coef_:
    # # for each in clf.feature_importances_:
    #     for every in each:
    #         outfile.write(str(float(every)) + '\n')
    # outfile.close()
    # while i < len(sample_x[:, 0]) and j < len(X_new[:, 0]):
    #     if j < len(X_new[:, 0]) and i < len(sample_x[:, 0]) and (sample_x[:, i] == X_new[:, j]).all():
    #         print(i)
    #         j += 1
    #     i += 1

