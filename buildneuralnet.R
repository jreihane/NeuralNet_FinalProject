setwd("G:\\Education\\Amirkabir University\\Neural Network\\projects\\4\\data set")
library(devtools)
library(nnet)
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
# library(caret)
# require(RCurl)

# root.url<-'https://gist.github.com/fawda123'
# raw.fun<-paste(
#         root.url,
#         '5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8b19/nnet_plot_fun.r',
#         sep='/'
# )
# script<-getURL(raw.fun, ssl.verifypeer = FALSE)
# eval(parse(text = script))
# rm('script','raw.fun')


# source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
data_csv <- read.csv("coc81.csv")

test1_data <- subset(data_csv,project_id %in% c(1,7,13,19,25,31,37,43,49,55,61))
train1_data <- subset(data_csv,!(project_id %in% c(1,7,13,19,25,31,37,43,49,55,61)))

test2_data <- subset(data_csv,project_id %in% c(2,8,14,20,26,32,38,44,50,56,62))
train2_data <- subset(data_csv,!(project_id %in% c(2,8,14,20,26,32,38,44,50,56,62)))

test3_data <- subset(data_csv,project_id %in% c(3,9,15,21,27,33,39,45,51,57,63))
train3_data <- subset(data_csv,!(project_id %in% c(3,9,15,21,27,33,39,45,51,57,63)))

test4_data <- subset(data_csv,project_id %in% c(4,10,16,22,28,34,40,46,52,58))
train4_data <- subset(data_csv,!(project_id %in% c(4,10,16,22,28,34,40,46,52,58)))

test5_data <- subset(data_csv,project_id %in% c(5,11,17,23,29,35,41,47,53,59))
train5_data <- subset(data_csv,!(project_id %in% c(5,11,17,23,29,35,41,47,53,59)))

test6_data <- subset(data_csv,project_id %in% c(6,12,18,24,30,36,42,48,54,60))
train6_data <- subset(data_csv,!(project_id %in% c(6,12,18,24,30,36,42,48,54,60)))

# t <- train1_data
# net <- with(train1_data,nnet(formula=actual ~ time_factor+rely_factor+data_factor+cplx_factor+stor_factor+virt_factor+turn_factor+acap_factor+aexp_factor+pcap_factor+vexp_factor+lexp_factor+modp_factor+tool_factor+sced_factor+dev_mode_factor,size=23))#,x=loc,y=loc
net <- nnet(formula=actual_log ~ time_factor+rely_factor+data_factor+
                    cplx_factor+stor_factor+virt_factor+turn_factor+
                    acap_factor+aexp_factor+pcap_factor+vexp_factor+
                    lexp_factor+modp_factor+tool_factor+sced_factor+
                    loc_log+dev_mode_factor,size=23, data = train1_data, linout=T)#,x=loc,y=loc

# net <- nnet(formula=actual_log ~ time+rely+data+
#                     cplx+stor+virt+turn+
#                     acap+aexp+pcap+vexp+
#                     lexp+modp+tool+sced+
#                     loc_log+dev_mode,size=23, data = train1_data, linout=T)#,x=loc,y=loc

#x=train1_data[,-18],y=train1_data[,18]
print(net)
#vars <- c("time_factor","rely_factor","data_factor","acap_factor","aexp_factor","pcap_factor","vexp_factor","lexp_factor","modp_factor","tool_factor","sced_factor","dev_mode_factor","actual_log")
# nas <- is.na(test1_data)
# print(table(nas))
#net <- with(train1_data,nnet(formula=actual ~ time+rely+data+cplx+stor+virt+turn+acap+aexp+pcap+vexp+lexp+modp+tool+sced+dev_mode,size=23))#,x=loc,y=loc
#net <- nnet(formula=test1_data$actual ~ test1_data$time_factor+test1_data$rely_factor+test1_data$data_factor,size=23)#,x=loc,y=loc
# print(names(train1_data))
# print(names(test1_data))
print("------------------------------------------")
pr <- predict(net,test1_data,type="raw")
test1_data$estimated <- pr
test1_data$estimated2 <- 10 ^ pr
# p <- pr
# p[floor(p) > 0] <- 1
# p[floor(p) <= 0] <- 0

#plot.nnet(net)
# plot.nnet(net)
plot(pr)
print(pr)
# print(p)

#print(net)