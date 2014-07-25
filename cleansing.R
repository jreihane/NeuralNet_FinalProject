setwd("G:\\Education\\Amirkabir University\\Neural Network\\projects\\4\\data set")

library(foreign)

data <- read.arff("cocomo 81_CORRECTED.arff")

# Time factor
data[which(data$time == 1.0),"time_factor"] <- 1
data[which(data$time %in% c(1.06,1.07,1.08)),"time_factor"] <- 2
data[which(data$time %in% c(1.11,1.15)),"time_factor"] <- 3
data[which(data$time %in% c(1.27,1.30,1.35)),"time_factor"] <- 4
data[which(data$time %in% c(1.46,1.66,1.48)),"time_factor"] <- 5

# Rely factor
data[which(data$rely == 0.75),"rely_factor"] <- 1
data[which(data$rely %in% c(0.88,0.94)),"rely_factor"] <- 2
data[which(data$rely == 1),"rely_factor"] <- 3
data[which(data$rely == 1.15),"rely_factor"] <- 4
data[which(data$rely == 1.4),"rely_factor"] <- 5

#data[which(data$rely_factor == 1.4),"rely_factor"] <- 5

# Data factor
data[which(data$data == 0.94),"data_factor"] <- 1
data[which(data$data == 1),"data_factor"] <- 2
data[which(data$data == 1.04),"data_factor"] <- 3
data[which(data$data == 1.08),"data_factor"] <- 4
data[which(data$data == 1.16),"data_factor"] <- 5

# cplx factor
data[which(data$cplx == 0.7),"cplx_factor"] <- 1
data[which(data$cplx == 0.85),"cplx_factor"] <- 2
data[which(data$cplx == 1),"cplx_factor"] <- 3
data[which(data$cplx == 1.07),"cplx_factor"] <- 4
data[which(data$cplx == 1.15),"cplx_factor"] <- 5
data[which(data$cplx %in% c(1.30,1.65)),"cplx_factor"] <- 6

# stor factor
data[which(data$stor == 1),"stor_factor"] <- 1
data[which(data$stor == 1.06),"stor_factor"] <- 2
data[which(data$stor == 1.14),"stor_factor"] <- 3
data[which(data$stor == 1.21),"stor_factor"] <- 4
data[which(data$stor == 1.56),"stor_factor"] <- 5

# virt factor
data[which(data$virt == 0.87),"virt_factor"] <- 1
data[which(data$virt == 1),"virt_factor"] <- 2
data[which(data$virt == 1.15),"virt_factor"] <- 3
data[which(data$virt == 1.30),"virt_factor"] <- 4

# turn factor
data[which(data$turn == 0.87),"turn_factor"] <- 1
data[which(data$turn == 0.94),"turn_factor"] <- 2
data[which(data$turn == 1),"turn_factor"] <- 3
data[which(data$turn == 1.07),"turn_factor"] <- 4
data[which(data$turn == 1.15),"turn_factor"] <- 5

# acap factor
data[which(data$acap == 0.71),"acap_factor"] <- 1
data[which(data$acap %in% c(0.78,0.86)),"acap_factor"] <- 2
data[which(data$acap == 1),"acap_factor"] <- 3
data[which(data$acap %in% c(1.10,1.19)),"acap_factor"] <- 4
data[which(data$acap == 1.46),"acap_factor"] <- 5

# aexp factor
data[which(data$aexp == 0.82),"aexp_factor"] <- 1
data[which(data$aexp == 0.91),"aexp_factor"] <- 2
data[which(data$aexp == 1),"aexp_factor"] <- 3
data[which(data$aexp == 1.13),"aexp_factor"] <- 4
data[which(data$aexp == 1.29),"aexp_factor"] <- 5

# pcap factor
data[which(data$pcap == 0.7),"pcap_factor"] <- 1
data[which(data$pcap %in% c(0.86,0.93)),"pcap_factor"] <- 2
data[which(data$pcap == 1),"pcap_factor"] <- 3
data[which(data$pcap %in% c(1.08,1.17,1.42)),"pcap_factor"] <- 4

# vexp factor
data[which(data$vexp == 0.9),"vexp_factor"] <- 1
data[which(data$vexp == 1),"vexp_factor"] <- 2
data[which(data$vexp == 1.1),"vexp_factor"] <- 3
data[which(data$vexp == 1.21),"vexp_factor"] <- 4

# lexp factor
data[which(data$lexp == 0.95),"lexp_factor"] <- 1
data[which(data$lexp == 1),"lexp_factor"] <- 2
data[which(data$lexp == 1.07),"lexp_factor"] <- 3
data[which(data$lexp == 1.14),"lexp_factor"] <- 4

# modp factor
data[which(data$modp == 0.82),"modp_factor"] <- 1
data[which(data$modp %in% c(0.91,0.95)),"modp_factor"] <- 2
data[which(data$modp == 1),"modp_factor"] <- 3
data[which(data$modp == 1.1),"modp_factor"] <- 4
data[which(data$modp == 1.24),"modp_factor"] <- 5

# tool factor
data[which(data$tool == 0.83),"tool_factor"] <- 1
data[which(data$tool %in% c(0.91,0.95)),"tool_factor"] <- 2
data[which(data$tool == 1),"tool_factor"] <- 3
data[which(data$tool == 1.1),"tool_factor"] <- 4
data[which(data$tool == 1.24),"tool_factor"] <- 5

# sced factor
data[which(data$sced == 1),"sced_factor"] <- 1
data[which(data$sced == 1.04),"sced_factor"] <- 2
data[which(data$sced == 1.08),"sced_factor"] <- 3
data[which(data$sced == 1.23),"sced_factor"] <- 4

# dev_mode factor
data[which(data$dev_mode == "organic"),"dev_mode_factor"] <- 1
data[which(data$dev_mode == "semidetached"),"dev_mode_factor"] <- 2
data[which(data$dev_mode == "embedded"),"dev_mode_factor"] <- 3


# logarithmic change of big numbers
data[,"actual_log"] <- log10(data[,"actual"])
data[,"loc_log"] <- log10(data[,"loc"])


write.csv(data, "coc81.csv",row.names=FALSE,sep=",")
