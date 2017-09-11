setwd("E:\\disk_use_201707")
# 用stl找出异常变化的时间点
flodername <- "data\\solved_data\\600"
filenames <- dir(flodername)
for(filename in filenames)
{
    data_in <- read.csv(paste(flodername, "\\", filename, sep = ""), header = T, as.is = TRUE)
    data_in <- data_in[ , 3 : ncol(data_in)]
    line_disk <- data_in[2, ]
    line_time <- data_in[1, ]
    line_disk=as.numeric(line_disk)
    sdata <- ts(line_disk, frequency = 3, start = c(2000, 1))
    fit <- stl(sdata, s.window = 'periodic', robust = T)
    out <- which(fit$weights <1e-8)
    out_time <- line_time[out]
    write.csv(out_time, paste("data\\abnormal_time\\", filename, sep = ''))
}
# a<-read.csv("host1011.csv", header = T, as.is = T)
# a<-a[2, 3:length(a)]
# a<-as.numeric(a)
# sdata=ts(a,frequency=10,start=c(2008,1))
# fit=stl(sdata,s.window='periodic',robust=T)
# op <- par(mar=c(0,4,0,3),oma=c(5,0,4,0),mfcol=c(4,1))
# plot(fit)
# plot(sdata)
# out<-which(fit$weights < 1e-8)
# plot(fit)
# sts<-fit$time.series
# points(time(sts)[out],0.8*sts[,"remainder"][out],pch="x",col="red")
# par(op)
# plot(a,type="l")
# points(time(a)[out],1.0*a[out],pch="x",col="red")

