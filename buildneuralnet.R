setwd("G:\\Education\\Amirkabir University\\Neural Network\\projects\\4\\data set")
library(devtools)
library(nnet)
source("createsampledata.R")
library(gridExtra)
source("calculateerror.R")
#source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
#source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
# set.seed(3374)

# net <- with(train1_data,nnet(formula=actual ~ time_factor+rely_factor+data_factor+cplx_factor+stor_factor+virt_factor+turn_factor+acap_factor+aexp_factor+pcap_factor+vexp_factor+lexp_factor+modp_factor+tool_factor+sced_factor+dev_mode_factor,size=23))#,x=loc,y=loc

net_list <- list()
results <- matrix(nrow = 11,ncol=0)
results2 <- matrix(nrow = 10,ncol=0)

applyNet <- function(train_data_item,i){
        
#         estimated_values <- matrix(ncol=0,nrow=nrow(test_data[[i]]))
#         net <- NULL
#         pr <- NULL

        net <- nnet(formula=actual_log ~ time_factor+rely_factor+data_factor+
                            cplx_factor+stor_factor+virt_factor+turn_factor+
                            acap_factor+aexp_factor+pcap_factor+vexp_factor+
                            lexp_factor+modp_factor+tool_factor+sced_factor+
                            loc_log+dev_mode_factor,size=23,
                            data = train_data_item, linout=T, maxit = 300,trace=F)
#         print(net)
        net_list[[i]] <<- net
}

testNet <- function(test_data_item,i){
        estimated_values <- matrix(ncol=0,nrow=nrow(test_data_item))
        t <- data.frame(test_data_item, row.names = NULL)

        pr <- predict(net_list[[i]],t,type="raw")
        
        plot(pr, pch = 19)
        
        dir_name <- paste("figures\\",i,sep = "")
        
        if(!file.exists(dir_name)) dir.create(dir_name)
        
        plot_name <- paste(i, ".png")
        #                 plot_name <- paste(j, ".png")
        plot_name <- paste(dir_name,"\\", plot_name,sep = "")
        dev.copy(png,plot_name,width=480,height=480)
        dev.off()
        
        Est_log <- pr
        estimated_values <- cbind(estimated_values, Est_log)
        Est <- 10 ^ pr
        estimated_values <- cbind(estimated_values, Est)
        estimated_values <- cbind(estimated_values, t$actual_log)
        estimated_values <- cbind(estimated_values, t$actual)
        
        if(nrow(estimated_values) == 10){
                #                 print("----------")
                estimated_values <- rbind(estimated_values,NA)
        }
        
        results <<- cbind(results,estimated_values[,4])
        results <<- cbind(results,estimated_values[,2])
        
        #         results <<- cbind(results,test_data[[i]]$actual_log)
        #         results <<- cbind(results,estimated_values$Est_log)
}

calculateNNError <- function(result_data){
        error_rate <- calculateMMRE(result_data)
        error_rate
}

doNNCal <- function(train_data,test_data){
        x1 <- mapply(applyNet,train_data,seq_along(train_data),SIMPLIFY = F)
        x2 <- mapply(testNet,test_data,seq_along(test_data),SIMPLIFY = F)
        
        results <- data.frame(results,row.names=NULL)
        colnames(results) <- c("Conj1_Obs","Conj1_Est","Conj2_Obs","Conj2_Est","Conj3_Obs","Conj3_Est","Conj4_Obs","Conj4_Est","Conj5_Obs","Conj5_Est","Conj6_Obs","Conj6_Est")
        
        pdf("results\\nn\\data_output.pdf", height=5, width=20)
        grid.table(results)
        dev.off()
        
        results
}

getNNR2 <- function(data){
        r2_list <- list()
        for(data_set in 1:6){
                var_obs_name <- paste('Conj',data_set,'_Obs',sep='')
                var_est_name <- paste('Conj',data_set,'_Est',sep='')
                
                fit <- lm(data[,var_obs_name]~data[,var_est_name])
                
                r2 <- summary(fit)$r.squared
                r2_list <- rbind(r2_list,r2)
        }
        r2_list
}


# calculateError(results)