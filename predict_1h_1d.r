setwd("E:\\disk_use_201707")

# 10 crossing validation, randomly create train and test samples for each TS file
rand <- function(filename)
{
  print(filename)
  cat("start read file at", as.character(Sys.time()))
  # read TS file from floder
  data_in <- read.csv(paste(flodername, "\\", filename, ".csv", sep = ""), header = T, as.is = T)
    cat("end read file at", as.character(Sys.time()))
  # remove the first two line
  data <- data_in[, 3 : ncol(data_in)]
  # the number of the target value and the attributes(includes target value)
  num_target <- 6
  num_attr <- ncol(data)
  # the number of the sample
  num_sample <- nrow(data)
  # an array of the sanmple index
  sample_index <- c(1 : num_sample)
  # 10 crossing validation, to create 10 random train samples and test samples
  folds <- createFolds(y=sample_index, k=10)
  ###error_DNN <- NULL
  error_DT <- NULL
  for(folds_index in 1 : 10)
  {
    # one of the 10 random train samples and test samples
    fold <- folds[[folds_index]]		
    #preset test sample and train sample
    test_file <- NULL
    train_file <- NULL
    for(sam in sample_index)
    {
      if(is.element(sam, fold))
      {
        test_file <- rbind(test_file, data[sam, ])
      }
      else{
        train_file <- rbind(train_file, data[sam, ])
      }
    }
    cat("end seperating test and train at", as.character(Sys.time()), "for the", folds_index, "seperate")
    # seperate x and y of test and train sample
    test_y <- test_file[, c((ncol(test_file) - num_target + 1), (ncol(test_file) - 2))]
    test_x <- test_file[, 1 : (ncol(test_file) - num_target)]
    train_y <- train_file[, c((ncol(test_file) - num_target + 1), (ncol(test_file) - 2))]
    train_x <- train_file[, 1 : (ncol(train_file) - num_target)]
    # predict
    # error_DNN <- rbind(error_DNN, deeplearning(train_x, train_y, test_x, test_y))
    error_DT <- rbind(error_DT, decision_tree(train_x, train_y, test_x, test_y))
    cat("end", folds_index, "th train of", filename, "at", as.character(Sys.time()))
  }
  # print(error_DNN)
  write.csv(error_DT, paste(flodername, "\\", filename, "_error_dt.csv", sep = ""))
  # write.csv(error_DNN, paste(flodername, "\\", filename, "_error_dnn.csv", sep = ""))
  print(error_DT)
  print("end random!")
}

wavelet <- function(datas, fil)#小波分解
{	
  feature = NULL
  for(i in 1 : nrow(datas))
  {
    eachrow <- t(datas[i,])
    wt <- dwt(eachrow, filter=fil, boundary="reflection")
    feature <- rbind(feature, unlist(c(wt@W, wt@V[[wt@level]])))
  }
  feature <- as.data.frame(feature)
  return(feature)
}

deeplearning<-function(train_x, train_y, test_x, test_y)#DNN
{
  data_train_x <- as.h2o(train_x)
  data_test_x <- as.h2o(test_x)
  data_train_y <- as.h2o(train_y)
  #data_test_y <- as.h2o(test_y)
  error <- NULL
  i = 1
  while(i <= length(test_y))
  {
    datas <- h2o.cbind(data_train_x, data_train_y[i])
    # print(datas[1,])
    # fit <- h2o.deeplearning(x = colnames(data_train_x), y = colnames(data_train_y[i]), training_frame = datas, activation = "TanhWithDropout", hidden = hidden_array, epochs = epochs_num, stopping_metric  = "RMSE")
    fit <- h2o.deeplearning(x = colnames(data_train_x), y = colnames(data_train_y[i]), training_frame = datas, activation = "Tanh", hidden = c(128, 64, 32), epochs = 10000, stopping_metric  = "RMSE")
    predictions <- predict(object = fit, newdata = data_test_x)
    predicted <- as.data.frame(predictions$predict)
    #print(predicted)
    num_nomal = 0#原样本中正样本的个数
    num_pre_nomal = 0#正样本预测的结果依旧为正???
    num_pre_alarm = 0#负样本预测的结果为负???
    index = 1
    for(index in 1 : length(test_y[, i]))
    {
      if(test_y[index, i] < 75)
      {
        num_nomal = num_nomal + 1
        if(predicted[index, 1] < 75)
        {
          num_pre_nomal = num_pre_nomal + 1
        }
      }
      else
      {
        if(predicted[index, 1] > 75)
        {
          num_pre_alarm = num_pre_alarm + 1
        }
      }
    }
    error_ops = num_pre_nomal / num_nomal
    error_neg = num_pre_alarm / (length(test_y[, i]) - num_nomal)
    error_all = c(error_ops, error_neg)
    error <- rbind(error, error_all)
    print(error)
    i = i + 1
  }
  print("deep learning predict end!")
  return(error)
}

decision_tree<-function(train_x, train_y, test_x, test_y)#决策???
{
  error <- NULL
  i = 1
  while(i <= length(test_y))
  {
    y <- train_y[i]
    colnames(y) = "y"
    train <- data.frame(cbind(y, train_x))
    ct <- ctree(y~., data = train, controls = ctree_control(minsplit = 30, minbucket = 10, maxdepth = 5))
    predicted <- predict(ct, test_x)
    TP = 0 #BOTH > 75%
    FP = 0 #PRE > 75%, ACT <75%
    FN = 0 #PRE < 75%, ACT >75%
    TN = 0 #BOTH < 75%
    for(index in 1 : length(test_y[, i]))
    {
      if(test_y[index, i] < 75)
      {
        if(predicted[index, 1] < 75)
        {
          TP = TP + 1
        }
        else
        {
          FP = FP+1
        }
      }
      else
      {
        if(predicted[index, 1] < 75)
        {
          FN = FN + 1
        }
        else
        {
          TN = TN+1
        }
      }
    }
    error_ops = TP / (TP + FN)
    error_neg = FP / (FP+TN)
    error_all = c(error_ops, error_neg)
    error <- rbind(error, error_all)
    i = i + 1
  }
  print("decision tree predict end!")
  return(error)
}
# library(wavelets)
library(h2o)
h2o.init()
library(party)
library(caret)

flodername <- "data\\solved_data\\TSs"
filenames <- c("TS1")
#filenames <- c("TS1", "TS1_2", "TS1_3", "TS2", "TS2_2", "TS3")

# 预设小波分解的一些参???
# fil <- wt.filter(filter = "bl14", modwt = FALSE, level = 2)

for(files in filenames)
{
  rand(files)
}
