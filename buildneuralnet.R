setwd("G:\\Education\\Amirkabir University\\Neural Network\\projects\\4\\data set")
library(devtools)
library(nnet)
source("createsampledata.R")
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
#source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')

data_csv <- read.csv("coc81.csv")

# net <- with(train1_data,nnet(formula=actual ~ time_factor+rely_factor+data_factor+cplx_factor+stor_factor+virt_factor+turn_factor+acap_factor+aexp_factor+pcap_factor+vexp_factor+lexp_factor+modp_factor+tool_factor+sced_factor+dev_mode_factor,size=23))#,x=loc,y=loc

train_data <- create_train_data(data_csv)
test_data <- create_test_data(data_csv)

results <- matrix(ncol=6)

applyNet <- function(train_data_item,i){
#         estimated_values0 <- data.frame(dimnames = (list(c(),c("pr1","pr2","pr3","pr4","pr5"))), stringsAsFactors=F)
        estimated_values0 <- data.frame(pr1=character(),pr2=character(),pr3=character(),pr4=character(),pr5=character(), stringsAsFactors=F)
#         names(estimated_values0) <- c("pr1","pr2","pr3","pr4","pr5")
        estimated_values <- matrix(ncol=0,nrow=nrow(test_data[[i]]))
        estimated_values2 <- list()
        
        for(j in 1:5){
                net <- nnet(formula=actual_log ~ time_factor+rely_factor+data_factor+
                                    cplx_factor+stor_factor+virt_factor+turn_factor+
                                    acap_factor+aexp_factor+pcap_factor+vexp_factor+
                                    lexp_factor+modp_factor+tool_factor+sced_factor+
                                    loc_log+dev_mode_factor,size=23,
                                        data = train_data_item, linout=T, maxit = 300,trace=F)
                
                # net <- nnet(formula=actual_log ~ time+rely+data+
                #                     cplx+stor+virt+turn+
                #                     acap+aexp+pcap+vexp+
                #                     lexp+modp+tool+sced+
                #                     loc_log+dev_mode,size=23, data = train1_data, linout=T)#,x=loc,y=loc
                
                #         print(net)
#                 print(i)
#                 print("------------------------------------------")
                pr <- predict(net,test_data[i],type="raw")
#                 print(nrow(pr))
# print(ncol(pr))
                #         print(test_data[i])
                #         print(i)
                #         f <- test_data[[1]]
                #         print(f)
                #         print(test_data[[1]]$project_id)
                col_name <- paste("pr", j,sep="")
#                 col_name2 <- paste("estimated2", j,sep="")
#                 if(nrow(estimated_values) == 0) estimated_values <- data.frame(pr)
#                 else
estimated_values <- cbind(pr,estimated_values)

#                 estimated_values0$"pr2" <- pr

#                 results$
#                 rbind(estimated_values2, 10 ^ pr)
                

#                 cbind(test_data[[i]],col_name)
#                 cbind(test_data[[i]],col_name2)
#                 test_data[[i]][,col_name] <- pr
#                 test_data[[i]][,col_name2] <- 10 ^ pr
#                 test_data[[i]]$col_name <- pr
                plot(pr, pch = 19)
                
                dir_name <- paste("figures\\",i,sep = "")
                
                if(!file.exists(dir_name)) dir.create(dir_name)
                
                plot_name <- paste(j, ".png")
                plot_name <- paste(dir_name,"\\", plot_name,sep = "")
                dev.copy(png,plot_name,width=480,height=480)
                dev.off()
        }
# estimated_values0[,] <- estimated_values
estimated_values <- cbind(estimated_values, test_data[[i]]$actual_log)
#         print(estimated_values)
# print(results)
#         data.frame(estimated_values)
#         colnames(estimated_values) <- c("pr1","pr2","pr3","pr4","pr5","actual_log")
# print(estimated_values)
# results <<- estimated_values
results <<- rbind(results,estimated_values)

# estimated_values
#         test_data[[i]] <- c(test_data[i], estimated_values)
# cbind(test_data[[i]], estimated_values)
# print(nrow(results))
#         if(nrow(results) == 0){
#                 print("e")
                
#                 results <<- test_data[i]
#                 rbind(test_data[i],results)
                
#         }
#         else{
#                 rbind(test_data[i],results)
#         }
# cbind(test_data[[i]],estimated_values2)
}


x <- mapply(applyNet,train_data,seq_along(train_data),SIMPLIFY = F)
results <- data.frame(results,row.names=NULL)
colnames(results) <- c("pr1","pr2","pr3","pr4","pr5","actual_log")
# print(x[1])
# df <- data.frame(x[1:3],stringsAsFactors = F)