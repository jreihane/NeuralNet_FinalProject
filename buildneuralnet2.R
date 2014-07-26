setwd("G:\\Education\\Amirkabir University\\Neural Network\\projects\\4\\data set")
library(devtools)
library(nnet)
source("createsampledata.R")
library(gridExtra)
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
#source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
# set.seed(3374)
data_csv <- read.csv("coc81.csv")

# net <- with(train1_data,nnet(formula=actual ~ time_factor+rely_factor+data_factor+cplx_factor+stor_factor+virt_factor+turn_factor+acap_factor+aexp_factor+pcap_factor+vexp_factor+lexp_factor+modp_factor+tool_factor+sced_factor+dev_mode_factor,size=23))#,x=loc,y=loc

train_data <- create_train_data(data_csv)
test_data <- create_test_data(data_csv)

results <- matrix(nrow = 11,ncol=0)
results2 <- matrix(nrow = 10,ncol=0)

applyNet <- function(train_data_item,i){
        
        estimated_values <- matrix(ncol=0,nrow=nrow(test_data[[i]]))
#         print(nrow(train_data_item))
        net <- NULL
        pr <- NULL

#         for(j in 1:5){
                net <- nnet(formula=actual_log ~ time_factor+rely_factor+data_factor+
                                    cplx_factor+stor_factor+virt_factor+turn_factor+
                                    acap_factor+aexp_factor+pcap_factor+vexp_factor+
                                    lexp_factor+modp_factor+tool_factor+sced_factor+
                                    loc_log+dev_mode_factor,size=23,
                            data = train_data_item, linout=T, maxit = 300,trace=F)
                
                pr <- predict(net,test_data[[i]],type="raw")
                
#                 estimated_values$Est_log <- pr
#                 estimated_values$Est <- 10 ^ pr
                #         print(nrow(estimated_values))
                #                 print("--------------")
                # print(nrow(estimated_values))
                # print("--------------")
                plot(pr, pch = 19)
                
                dir_name <- paste("figures\\",i,sep = "")
                
                if(!file.exists(dir_name)) dir.create(dir_name)
                
                plot_name <- paste(i, ".png")
#                 plot_name <- paste(j, ".png")
                plot_name <- paste(dir_name,"\\", plot_name,sep = "")
                dev.copy(png,plot_name,width=480,height=480)
                dev.off()
                
#         }

        Est_log <- pr
        estimated_values <- cbind(estimated_values, Est_log)
        Est <- 10 ^ pr
        estimated_values <- cbind(estimated_values, Est)
#         act_log <- test_data[[i]]$actual_log
#         act <- test_data[[i]]$actual
#         if(nrow(act) < 11)
#                 pr <- rbind(NA)
#         if(nrow(act_log) < 11)
#                 pr <- rbind(NA)
# print(nrow(estimated_values))
# print(nrow(test_data[[i]]$actual))
# print(nrow(test_data[i]$actual_log))
        estimated_values <- cbind(estimated_values, test_data[[i]]$actual_log)
        estimated_values <- cbind(estimated_values, test_data[[i]]$actual)
#         estimated_values$actual <- test_data[[i]]$actual
# estimated_values <- data.frame(estimated_values,row.names=NULL)
        if(nrow(estimated_values) == 10){
                print("----------")
                estimated_values <- rbind(estimated_values,NA)
        }
#         print(estimated_values$Est)
        
        results <<- cbind(results,estimated_values[,4])
        results <<- cbind(results,estimated_values[,2])

#         results <<- cbind(results,test_data[[i]]$actual_log)
#         results <<- cbind(results,estimated_values$Est_log)

        
}


x <- mapply(applyNet,train_data,seq_along(train_data),SIMPLIFY = F)
results <- data.frame(results,row.names=NULL)
colnames(results) <- c("Conj1_Obs","Conj1_Est","Conj2_Obs","Conj2_Est","Conj3_Obs","Conj3_Est","Conj4_Obs","Conj4_Est","Conj5_Obs","Conj5_Est","Conj6_Obs","Conj6_Est")

pdf("data_output.pdf", height=5, width=20)
grid.table(results)
dev.off()