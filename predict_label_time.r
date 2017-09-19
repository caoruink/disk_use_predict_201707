setwd("E:\\disk_use_201707")
library(party)
library(caret)
library(forecast)
library(e1071)  # svm
library(tseries)
label_sample <- fuction(samples)
{
  num_sample <- nrow(samples)
  outsample <- NULL
  for(index in (1 : num_sample))
  {
    temp <- samples[index,]
    fail <- F
    for(each in temp[2 : length(temp)])
    {
      if(each >= 75)
      {
        fail <- T
        print(each)
        break
      }
    }
    newtemp <- NULL
    if(fail)
    {
      newtemp <- c(temp[1:145], 1)
    }
    else
    {
      newtemp <- c(temp[1:145], 0)
    }
    outsample <- rbind(outsample, newtemp)
  }
  cat("the total number of sample is", nrow(outsample), "\n")
  write.csv(outsample, "labeled_7days.csv")
}

# 10 crossing validation, randomly create train and test samples for each TS file
rand <- function(filename)
{
  print(filename)
  cat("start read file at", as.character(Sys.time()))
  # read TS file from floder
  data_in <- read.csv(filename, header = T, as.is = T)
  cat("end read file at", as.character(Sys.time()))
  # remove the first line
  data <- data_in[, 2 : ncol(data_in)]
  # the number of the target value and the attributes(includes target value)
  num_target <- 1
  num_attr <- ncol(data) - 1
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
    trian_hostname <- train_file[, 1]
    test_hostname <- test_file[, 1]
    train_file <- train_file[, 2 : ncol(train_file)]
    test_file <- test_file[, 2 : ncol(test_file)]
    cat("end seperating test and train at", as.character(Sys.time()), "for the", folds_index, "seperate\n")
    # seperate x and y of test and train sample
    test_y <- test_file[, ncol(test_file)]
    test_x <- test_file[, 1 : (ncol(test_file) - num_target)]
    train_y <- train_file[, ncol(test_file)]
    train_x <- train_file[, 1 : (ncol(train_file) - num_target)]
    # predict
    predict_result <- decision_tree(train_x, train_y, test_x)
    # analysis of result and find out original series
    error_DT <- rbind(error_DT, analysis_result(predict_result, test_y))
    cat("end", folds_index, "th train of", filename, "at", as.character(Sys.time()), "\n")
    # find the original file
    total_test <- length(test_hostname)
    error <- NULL
    i = 1
    while(i <= total_test)
    {
      if(predict_result[i] == 'B')
      {
        print(i)
        filename_overd = test_hostname[i]
        orignal_file <- read.csv(paste("data\\solved_data\\600\\", filename_overd, ".csv", sep = ""), header = T, as.is = T)
        orignal_file <- orignal_file[2, 3 : ncol(orignal_file)]
        cat("************", filename_overd, " ************\n")
        # ARIMA predict
        error_line <- ARIMA_svm(orignal_file, 7*144, 15*144)
        error <- rbind(error, error_line)
        cat("###########end", filename_overd, "at", as.character(Sys.time()), "###################\n")
      }
      i = i + 1
    }
    print(error)
    View(error)
    cat("abnomal value predict\n")
    
    # calculate error rate
    # calculate RMSE
  }
  # print(error_DNN)
  # write.csv(error_DT, paste(flodername, "\\", filename, "_error_dt.csv", sep = ""))
  # write.csv(error_DNN, paste(flodername, "\\", filename, "_error_dnn.csv", sep = ""))
  print(error_DT)
  print("end random!")
}

decision_tree <- function(train_x, train_y, test_x)
{
  train <- data.frame(cbind(y = train_y, train_x))
  ct <- ctree(y~., data = train, controls = ctree_control(minsplit = 10, minbucket = 5, maxdepth = 7, testtype = "MonteCarlo"))
  predicted <- predict(ct, test_x)
  return(predicted)
}

analysis_result <- function(predicted, test_y)
{
  a<-cbind(predicted, test_y)
  TP = 0 #BOTH B
  FP = 0 #PRE B, ACT A
  FN = 0 #PRE A, ACT B
  TN = 0 #BOTH A
  for(index in 1 : length(test_y))
  {
    if(test_y[index] == "B")
    { 
      if(predicted[index] == "B")
      {
        TP = TP + 1
      }
      else
      {
        FN = FN + 1
      }
    }
    else
    {
      if(predicted[index] == "B")
      {
        FP = FP + 1
      }
      else
      {
        TN = TN+1
      }
    }
  }
  error_ops = TP / (TP + FN)
  error_neg = FP / (FP + TN)
  error_all = c(error_ops, error_neg)
  return(error_all)
}

ARIMA <- function(ts, length_in, length_pre, hostname)
{
  ts<-as.numeric(ts)
  # plot(ts,type='l')
  ts_in <- ts[1 : length_in]
  ts_out <- ts[length_in + 1 : length(ts)]
  arima_ts <- auto.arima(ts_in)
  ts_forecast <- forecast(arima_ts, h = length_pre, level = c(99.5))
  plot(ts_forecast, ylim=c(70,80))
  
  result_pre <- as.numeric(ts_forecast['mean']$mean)
  total_length_ts_out <- length(ts_out)
  i = 1
  flag_o = F
  flag_p = F
  while(i <= min(total_length_ts_out, length_pre))
  {
    if(ts_out[i] >= 75 && flag_o == F)
    {
      cat("original overd is", i, "\n")
      flag_o = T
    }
    if(result_pre[i] >= 75 && flag_p == F)
    {
      cat("prediction overd is", i, "\n")
      flag_p = T
    }
    if(flag_p == T && flag_o == T)
    {
      return()
    }
    i = i + 1
  }
}

calculate_error_rate <- function(pre, row)
{
  return(mean(abs(pre - row)))
}

ARIMA_svm <- function(ts, length_in, length_pre)
{
  # 前五天预测，后两天做残差预测，预测之后十四天的使用度
  ts<-as.numeric(ts)
  # plot(ts,type='l')
  ts_in <- ts[1 : (5 * 144)]
  ts_svm_test <- ts[(5 * 144 + 1) : length_in]
  ts_out <- ts[(length_in + 1) : length(ts)]
  # 先预测两天的做残差建模
  arima_ts <- auto.arima(ts_in)
  ts_forecast <- forecast(arima_ts, h = 2 * 144, level = c(99.5))

  result_pre <- as.numeric(ts_forecast['mean']$mean)
  residuals_pre_ori <- result_pre - ts_svm_test
  residuals_pre_ori <- as.matrix(residuals_pre_ori)
  newmatrix <- matrix(NA, 4, length(residuals_pre_ori) - 6)
  for(i in 1 : ncol(newmatrix))
  {
    newmatrix[, i] = c(residuals_pre_ori[i], residuals_pre_ori[i + 1], residuals_pre_ori[i] + 2, residuals_pre_ori[i + 3])
  }
  inputdata<-cbind(t(newmatrix[, 1 : ncol(newmatrix)]), residuals_pre_ori[5 : length(residuals_pre_ori) - 3])
  svmfit <- svm (V5~., data = inputData, type="eps-regression", kernel = "radial", cost = 1000, gamma=0.0001, scale = FALSE)

  # 新的一轮预测，预测十四天
  ts_in <- c(ts_in, ts_svm_test)
  arima_ts <- auto.arima(ts_in)
  ts_forecast <- forecast(arima_ts, h = length_pre, level = c(99.5))

  result_pre <- as.numeric(ts_forecast['mean']$mean)  
  total_length_ts_out <- length(ts_out)  
  
  # 预测残差
  res_index = 1
  pre_res_new = NULL
  while(res_index <= length_pre)
  {
    test_svm = residuals_pre_ori[(length(residuals_pre_ori) - 3) : length(residuals_pre_ori)]
    test_svm = as.matrix(test_svm)
    row.names(test_svm) <- c("V1", "V2", "V3", "V4")
    new_res = predict(svmfit, t(test_svm))
    pre_res_new = c(pre_res_new, new_res)
    residuals_pre_ori = c(residuals_pre_ori, new_res)
    res_index = res_index + 1
  }
  
  result_pre <- as.numeric(result_pre) + pre_res_new
  
  error1 <- calculate_error_rate(result_pre[1 : min(total_length_ts_out, length_pre)], ts_out[1 : min(total_length_ts_out, length_pre)])
  error2 <- calculate_error_rate(as.numeric(ts_forecast['mean']$mean)[1 : min(total_length_ts_out, length_pre)], ts_out[1 : min(total_length_ts_out, length_pre)])

  # i = 1
  # flag_o = F
  # flag_p = F
  # while(i <= min(total_length_ts_out, length_pre))
  # {
  #   if(ts_out[i] >= 75 && flag_o == F)
  #   {
  #     cat("original overd is", i, "\n")
  #     flag_o = T
  #   }
  #   if(result_pre[i] >= 75 && flag_p == F)
  #   {
  #     cat("prediction overd is", i, "\n")
  #     flag_p = T
  #   }
  #   if(flag_p == T && flag_o == T)
  #   {
  #     return()
  #   }
  #   i = i + 1
  # }
  return(c(error1, error2))
}


filename <- c("labeled_7days.csv")


for(files in filenames)
{
  rand(files)
}
