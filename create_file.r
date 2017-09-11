setwd("F:\\disk_use_201707")
# 不创建基准时间轴而按照文件本身的数据合并文件
# caorui@nbjl.nankai.edu.cn
# start 2017/05/15 11:00 v1.0.0
# end 3017/05/15 15:50 
# 创建基准时间轴baseline_time
create_times <- function(start_time, end_time, interval)
{
    length_time <- as.numeric(difftime(end_time, start_time, units="secs")/interval)
    baseline_time = start_time + interval * 0 : length_time
    return(baseline_time)
}
solve_files <- function(file1, file2, file3, interval)
{
    nr = nrow(file1)
    nc1 = ncol(file1)
    nc2 = ncol(file2)
    nc3 = ncol(file3)
    # 过短的文件不进行处理
    if((nc1 < 100) || (nc2 < 100) || (nc3 < 100))
    {
        return("error")
    }

    # 确定开始时间和结束时间
    time1 = file1[1, ]
    start_time = time1[grep("00:00:00", time1)[1]]
    time3 = file3[1, ]
    index_test_time = grep("00:00:00", time3)
    end_time = time3[index_test_time[length(index_test_time)]]
    start_time = strptime(as.character(start_time), "%Y-%m-%d %H:%M:%S")
    end_time = strptime(as.character(end_time), "%Y-%m-%d %H:%M:%S")
    print(start_time)
    print(end_time)

    # 确定baseline_time
    baseline_time <- create_times(start_time, end_time, interval)
    out = NULL
    out <- rbind(out, c("time", as.character(baseline_time)))
    # 检测属性有15个
    num_attributes = 15
    index_attr = 0
    while(index_attr < num_attributes)
    {        
        index_baseline_time = 1
        line = NULL
        time_tmp_1 = file1[index_attr * 2 + 1, ]
        time_tmp_2 = file2[index_attr * 2 + 1, ]
        time_tmp_3 = file3[index_attr * 2 + 1, ]
        content_1 = file1[index_attr * 2 + 2, ]
        content_2 = file2[index_attr * 2 + 2, ]
        content_3 = file3[index_attr * 2 + 2, ]
        line <- c(line, content_1[1])
        index_content = 1
        length_file = length(time_tmp_1)
        while(index_content < length_file)
        {
            each_time = time_tmp_1[index_content]
            if(index_baseline_time > length(baseline_time))
            {
                break
            }
            if(each_time == "time" || each_time == "NA" || each_time == "")
            {
                index_content = index_content + 1
                next
            }
            each_time = strptime(each_time, "%Y-%m-%d %H:%M:%S")
            if(each_time < baseline_time[index_baseline_time])
            {
                index_content = index_content + 1
                next
            }
            else if(each_time == baseline_time[index_baseline_time])
            {
                line <- c(line, content_1[index_content])
                index_content = index_content + 1
                index_baseline_time = index_baseline_time + 1
            }
            else if(each_time > baseline_time[index_baseline_time])
            {
                line <- c(line, round((as.numeric(content_1[index_content - 1]) + as.numeric(content_1[index_content]))/2, 2))
                index_baseline_time = index_baseline_time + 1
            }
        }      
        index_content = 1
        length_file = length(time_tmp_2)
        while(index_content < length_file)
        {
            each_time = time_tmp_2[index_content]
            if(index_baseline_time > length(baseline_time))
            {
                break
            }
            if(each_time == "time" || each_time == "NA" || each_time == "")
            {
                index_content = index_content + 1
                next
            }
            each_time = strptime(each_time, "%Y-%m-%d %H:%M:%S")
            if(each_time < baseline_time[index_baseline_time])
            {
                index_content = index_content + 1
                next
            }
            else if(each_time == baseline_time[index_baseline_time])
            {
                line <- c(line, content_2[index_content])
                index_content = index_content + 1
                index_baseline_time = index_baseline_time + 1
            }
            else if(each_time > baseline_time[index_baseline_time])
            {
                line <- c(line, round((as.numeric(content_2[index_content - 1]) + as.numeric(content_2[index_content]))/2, 2))
                index_baseline_time = index_baseline_time + 1
            }
        } 
        index_content = 1
        length_file = length(time_tmp_3)
        while(index_content < length_file)
        {
            each_time = time_tmp_3[index_content]
            if(index_baseline_time > length(baseline_time))
            {
                break
            }
            if(each_time == "time" || each_time == "NA" || each_time == "")
            {
                index_content = index_content + 1
                next
            }
            each_time = strptime(each_time, "%Y-%m-%d %H:%M:%S")
            if(each_time < baseline_time[index_baseline_time])
            {
                index_content = index_content + 1
                next
            }
            else if(each_time == baseline_time[index_baseline_time])
            {
                line <- c(line, content_3[index_content])
                index_content = index_content + 1
                index_baseline_time = index_baseline_time + 1
            }
            else if(each_time > baseline_time[index_baseline_time])
            {
                line <- c(line, round((as.numeric(content_3[index_content - 1]) + as.numeric(content_3[index_content]))/2, 2))
                index_baseline_time = index_baseline_time + 1
            }
        }
        index_attr = index_attr + 1
        out <- rbind(out, line)
    }
    return(out)
}
combine_file <- function(second_dirname, flodernames, interval)
{
    filenames <- dir(paste(flodernames[1], "\\", second_dirname, sep = ""))
    filenames <- filenames[1:1999]
    filenames_2 <- dir(paste(flodernames[2], "\\", second_dirname, sep = ""))
    filenames_3 <- dir(paste(flodernames[3], "\\", second_dirname, sep = ""))
    filenames_new <- dir(paste("newdata", "\\", second_dirname, sep = ""))
    for(filename in filenames)
    {
        print(filename)
        if(filename %in% filenames_new)
        {
            print("THERE HAS BEING")
            next
        }
        if((!(filename %in% filenames_2)) || (!(filename %in% filenames_3)))
        {
            print("error")
            next
        }
        file1 <- read.csv(paste(flodernames[1], "\\", second_dirname, "\\", filename, sep = ""), header = F, as.is = T)
        file2 <- read.csv(paste(flodernames[2], "\\", second_dirname, "\\", filename, sep = ""), header = F, as.is = T)
        file3 <- read.csv(paste(flodernames[3], "\\", second_dirname, "\\", filename, sep = ""), header = F, as.is = T)
        print("end read!")
        outfile_content <- solve_files(file1, file2, file3, interval)
        if(length(outfile_content) != 5)
        {
            write.csv(outfile_content, paste("newdata", "\\", second_dirname, "\\", filename, sep = ""))
        }
    }
}

#需要合并的文件夹名称
flodernames = c("TS_data_all_feature0214", "TS_data_all_feature0217", "TS_data_all_feature0223")
#需要合并的时间间隔文件夹名称
second_dir = c("600", "3600")
# 分别合并十分钟级和一小时级的数据
combine_file(second_dir[1], flodernames, 600)
# combine_file(second_dir[2], flodernames, 3600)
