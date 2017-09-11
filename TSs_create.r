setwd("F:\\disk_use_201707")
# 创造时间序列
# target value 1h,6h,12h,24h,48h,72h. 
#   时间序列1：	    2days(10min)
# 			        288+6=294
# 	时间序列1_2：	1day(1h)+1day(10min)
# 			        24+144+6=174
# 	时间序列1_3：	2days(30min)
# 			        96+6=102
# 	时间序列2：	    1day(4h)+1day(2h)+2day(1h)+1day(10min)
# 			        216
# 	时间序列2_2：	3days(1h)+1day(30min)+1day(10min)
# 			        270
# 	时间序列3：	    1day(6h)+2days(4h)+2days(2h)+1day(1h)+1day(10min)
# 			        214
# caorui@nbjl.nankai.edu.cn
# start 2017/07/14 15:24 v1.0.0
# end 2017/07/13 21:40
ts1_create <- function(content)
{
    index <- c((5 * 24 * 6 + 1) : (7 * 24 * 6), 7 * 24 * 6 + 6,  7 * 24 * 6 + 36, 7 * 24 * 6+ 72, 8 * 24 * 6, 9 * 24 * 6, 10 * 24 * 6)
    return(content[2, index])
}
ts12_create <- function(content)
{
    # 取第六天的每一小时的平均值
    temp <- content[, c((5 * 24 * 6 + 1) : (6 * 24 * 6))]
    new_value <- NULL
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 5))])))
        i <- i + 6
    }
    index <- c((6 * 24 * 6 + 1) : (7 * 24 * 6), 7 * 24 * 6 + 6,  7 * 24 * 6 + 36, 7 * 24 * 6+ 72, 8 * 24 * 6, 9 * 24 * 6, 10 * 24 * 6)
    return(c(new_value, content[2, index]))
}
ts13_create <- function(content)
{
    # 取第六、七天的每半小时的平均值
    temp <- content[, c((5 * 24 * 6 + 1) : (7 * 24 * 6))]
    new_value <- NULL
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 2))])))
        i <- i + 3
    }
    index <- c(7 * 24 * 6 + 6,  7 * 24 * 6 + 36, 7 * 24 * 6+ 72, 8 * 24 * 6, 9 * 24 * 6, 10 * 24 * 6)
    return(c(new_value, content[2, index]))
}
ts2_create <- function(content)
{
    new_value <- NULL
    # 取第三天的每4小时的平均值
    temp <- content[, c((2 * 24 * 6 + 1) : (3 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 23))])))
        i <- i + 24
    }
    # 取第四天的每2小时的平均值
    temp <- content[, c((3 * 24 * 6 + 1) : (4 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 11))])))
        i <- i + 12
    }
    # 取第五、六天的每1小时的平均值
    temp <- content[, c((4 * 24 * 6 + 1) : (6 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 5))])))
        i <- i + 6
    }
    # 取第七天的每10min的值&target
    index <- c((6 * 24 * 6 + 1) : (7 * 24 * 6), 7 * 24 * 6 + 6,  7 * 24 * 6 + 36, 7 * 24 * 6+ 72, 8 * 24 * 6, 9 * 24 * 6, 10 * 24 * 6)
    return(c(new_value, content[2, index]))
}
ts22_create <- function(content)
{
    new_value <- NULL
    # 取第三、四、五天的每1小时的平均值
    temp <- content[, c((2 * 24 * 6 + 1) : (5 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 5))])))
        i <- i + 6
    }
    # 取第六天的每半小时的平均值
    temp <- content[, c((5 * 24 * 6 + 1) : (6 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 2))])))
        i <- i + 3
    }
    # 取第七天的每10min的值&target
    index <- c((6 * 24 * 6 + 1) : (7 * 24 * 6), 7 * 24 * 6 + 6,  7 * 24 * 6 + 36, 7 * 24 * 6+ 72, 8 * 24 * 6, 9 * 24 * 6, 10 * 24 * 6)
    return(c(new_value, content[2, index]))
}
ts3_create <- function(content)
{
    new_value <- NULL
    # 取第一天的每6小时的平均值
    temp <- content[, c((0 * 24 * 6 + 1) : (1 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 35))])))
        i <- i + 36
    }
    # 取第二、三天的每4小时的平均值
    temp <- content[, c((1 * 24 * 6 + 1) : (3 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 23))])))
        i <- i + 24
    }
    # 取第四、五天的每2小时的平均值
    temp <- content[, c((3 * 24 * 6 + 1) : (5 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 11))])))
        i <- i + 12
    }
    # 取第六天的每1小时的平均值
    temp <- content[, c((5 * 24 * 6 + 1) : (6 * 24 * 6))]
    i <- 1
    while(i <= ncol(temp))
    {
        new_value <- c(new_value, mean(as.numeric(content[2, (i : (i + 5))])))
        i <- i + 6
    }
    # 取第七天的每10min的值&target
    index <- c((6 * 24 * 6 + 1) : (7 * 24 * 6), 7 * 24 * 6 + 6,  7 * 24 * 6 + 36, 7 * 24 * 6+ 72, 8 * 24 * 6, 9 * 24 * 6, 10 * 24 * 6)
    return(c(new_value, content[2, index]))
}
flodernames = "data\\solved_data\\600"
out_dir = "data\\solved_data\\TSs"
filenames <- dir(flodernames)
TSs_1 <- NULL
TSs_1_2 <- NULL
TSs_1_3 <- NULL
TSs_2 <- NULL
TSs_2_1 <- NULL
TSs_3 <- NULL
for(filename in filenames)
{
    print(filename)
    file_content <- read.csv(paste(flodernames, "\\", filename, sep = ""), header = T, as.is = T)
    if(ncol(file_content) < 1500)
    {
        print("error")
        next
    }
    file_content <- file_content [, 3:ncol(file_content)]
    TSs_1 <- rbind(TSs_1, c(strsplit(filename, ".csv")[[1]], ts1_create(file_content)))
    TSs_1_2 <- rbind(TSs_1_2, c(strsplit(filename, ".csv")[[1]], ts12_create(file_content)))
    TSs_1_3 <- rbind(TSs_1_3, c(strsplit(filename, ".csv")[[1]], ts13_create(file_content)))
    TSs_2 <- rbind(TSs_2, c(strsplit(filename, ".csv")[[1]], ts2_create(file_content)))
    TSs_2_1 <- rbind(TSs_2_1, c(strsplit(filename, ".csv")[[1]], ts22_create(file_content)))
    TSs_3 <- rbind(TSs_3, c(strsplit(filename, ".csv")[[1]], ts3_create(file_content)))
}
# TSs_1 <- as.character(TSs_1)
# TSs_1_2 <- as.character(TSs_1_2)
# TSs_1_3 <- as.character(TSs_1_3)
# TSs_2 <- as.character(TSs_2)
# TSs_2_1 <- as.character(TSs_2_1)
# TSs_3 <- as.character(TSs_3)
write.csv(TSs_1, paste(out_dir, "\\TS1.csv",  sep = ""))
write.csv(TSs_1_2, paste(out_dir, "\\TS1_2.csv", sep = ""))
write.csv(TSs_1_3, paste(out_dir, "\\TS1_3.csv", sep = ""))
write.csv(TSs_2, paste(out_dir, "\\TS2.csv", sep = ""))
write.csv(TSs_2_1, paste(out_dir, "\\TS2_2.csv", sep = ""))
write.csv(TSs_3, paste(out_dir, "\\TS3.csv", sep = ""))
