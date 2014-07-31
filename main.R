source("buildneuralnet.R")
source("buildregression.R")
source("createsampledata.R")
source("calculateerror.R")


# remove(nnet_result,net_MMRE_results,net_list)

data_csv <- read.csv("coc81.csv")


train_data <- create_train_data(data_csv)
test_data <- create_test_data(data_csv)

reg_results <- doRegression(train_data,test_data)
reg_MMRE_results <- calculateRegError(reg_results)
reg_r2 <- getRegR2(reg_results)

reg_result <- data.frame(reg_MMRE_results,reg_r2, row.names=NULL)

# nnet_result <- doNNCal(train_data,test_data)
# net_MMRE_results <- calculateNNError(nnet_result)
i <- 0
j <- 1
# while(net_MMRE_results[[1]] > 100 && i < 40){
# # while((net_r2[[1]] < .8 || net_r2[[2]] < .8 || net_r2[[3]] < .8 || net_r2[[4]] < .8 || net_r2[[5]] < .8 || net_r2[[6]] < .8) && i < 40){
#         nnet_result <- doNNCal(train_data,test_data)
#         net_MMRE_results <- calculateNNError(nnet_result)
#         
#         
#         
#         i <- i + 1
#         
#         print(i)
# }


# net_r2 <- getNNR2(nnet_result)
# print(net_MMRE_results)
# print(net_r2)

# nn_result <- data.frame(net_MMRE_results,net_r2, row.names=NULL)

finale_result <- data.frame(nn_result,reg_result, row.names=NULL)
pdf("results\\final_result9.pdf", height=6, width=8)
grid.table(finale_result)
dev.off()