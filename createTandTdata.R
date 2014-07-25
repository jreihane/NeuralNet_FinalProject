setwd("G:\\Education\\Amirkabir University\\Neural Network\\projects\\4\\data set")
library(neuralnet)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')

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

net <- with(train1_data,neuralnet(formula=actual ~ time_factor+rely_factor+data_factor,data=train1_data,hidden=c(23),rep = 6))#,x=loc,y=loc
plot.nnet(net)