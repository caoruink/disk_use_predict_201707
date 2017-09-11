setwd("F:\\disk_use_201707")
# 提取出磁盘使用度数据
# caorui@nbjl.nankai.edu.cn
# start 2017/07/13 21:34 v1.0.0
# end 

#需要提取的文件夹名称
flodernames = "data\\TS_data_nomal\\600"
#输出的文件夹名称
out_dir = "data\\solved_data\\600"
filenames <- dir(flodernames)
for(filename in filenames)
{
    print(filename)
    file_content <- read.csv(paste(flodernames, "\\", filename, sep = ""), header = T, as.is = T)
    disk_content <- file_content[c(1, 6), 2 : ncol(file_content)]
    write.csv(disk_content, paste(out_dir, "\\", filename, sep = ""))
}
