

# The formula of MMRE is:
#     for each row in dataset:
#               e1 <- |M(est) - M(act)| / M(act)
#               add e1 to total sum of other rows
#     divide the sum by number of rows

calculateMMRE <- function(data){
        
        result <- list()
        
        for(data_set in 1:6){
                var_obs_name <- paste('Conj',data_set,'_Obs',sep='')
                var_est_name <- paste('Conj',data_set,'_Est',sep='')
                sum <- 0
                for(row in 1:nrow(data)){
#                         print('------------------------------------')
#                         print(data[row,var_obs_name])
                        if(!is.na(data[row,var_obs_name]) && !is.na(data[row,var_est_name])){
#                                 print(data[row,var_obs_name])
#                                 print(data[row,var_est_name])
                                for_sum <- abs(data[row,var_obs_name] - data[row,var_est_name]) / data[row,var_obs_name]
                                sum <- sum + for_sum
#                                 print('------------------------------------')
#                                 print(for_sum)
#                                 print(sum)
                        }
                        
                        
                }
                
                res <- (sum * 100) / nrow(data)
#                 print(res)
                result <- rbind(result,res)
        }

        result
}