library(nlme)
library(gridExtra)
source("createsampledata.R")

data_csv <- read.csv("coc81.csv")

train_data <- create_train_data(data_csv)
test_data <- create_test_data(data_csv)
result_coeffs <- list()

test_normality <- function(t){
        t2 <- data.frame(t)
        
        qqnorm(t2$actual,datax = T)
        qqline(t2$actual)
        dev.copy(png,paste("figures\\regression\\","MMACT_PPlot.png"),width=360,height=360)
        dev.off()
        

        lnt2 <- log(t2$actual,exp(2))
        qqnorm(lnt2,datax = T)
        qqline(lnt2)
        dev.copy(png,paste("figures\\regression\\","LN_MMACT_PPlot.png"),width=360,height=360)
        dev.off()
}

applyRegression <- function(train_data_item,i){
        t <- data.frame(train_data_item)
        
#         par(mfrow=c(1,1))
#         x <- qqnorm(t$actual_ln,datax = T)
#         qqline(t$actual_ln,col=1,c(.25,.95))
#         dev.copy(png,paste("figures\\regression\\","actual_ln_PPlot_",i,".png",sep=""),width=480,height=480)
#         dev.off()

#         fit <- lm(t$actual_ln ~ t$loc_ln,data = t)
#         print(fit)
#         par(mfrow = c(2,2))
#         plot(fit)
#         dev.copy(png,paste("figures\\regression\\","lm_results",i,".png",sep=""),width=480,height=480)
#         dev.off()

#         multi_fit <- lm(t$actual_ln ~ t$time_factor+t$rely_factor+t$data_factor+
#                                 t$cplx_factor+t$stor_factor+t$virt_factor+t$turn_factor+
#                                 t$acap_factor+t$aexp_factor+t$pcap_factor+t$vexp_factor+
#                                 t$lexp_factor+t$modp_factor+t$tool_factor+t$sced_factor+
#                                 t$loc_ln+t$dev_mode_factor)
        if(i == 1)
                formula <- t$actual_ln ~ t$rely_factor+t$loc_ln
        if(i == 2)
                formula <- t$actual_ln ~ t$time_factor+t$rely_factor+t$data_factor+
                                t$stor_factor+t$virt_factor+t$acap_factor+t$pcap_factor+
                                t$modp_factor+t$loc_ln+t$dev_mode_factor
        if(i == 3)
                formula <- t$actual_ln ~ +t$rely_factor+t$acap_factor+t$loc_ln
        if(i == 4)
                formula <- t$actual_ln ~ t$time_factor+t$rely_factor+t$acap_factor+
                                t$lexp_factor+t$modp_factor+t$loc_ln
        if(i == 5)
                formula <- t$actual_ln ~ t$acap_factor+t$dev_mode_factor+t$loc_ln
        if(i == 6)
                formula <- t$actual_ln ~ t$time_factor+t$rely_factor+
                t$acap_factor+t$modp_factor+t$dev_mode_factor+t$loc_ln

        multi_fit <- lm(formula)
        multi_fit_sum <- summary(multi_fit)

        coeffs <- t(multi_fit_sum$coefficients[,1])
        coeff_frame <- data.frame(coeffs,row.names = NULL)
# print(colnames(coeffs))
#         colnames(coeff_frame) <- row.names(coeffs)
#         if(is.null(result_coeffs)) result_coeffs <<- c(coeff_frame)
#         else result_coeffs <<- c(result_coeffs,coeff_frame)
        result_coeffs <<- c(result_coeffs,coeff_frame)
#         result_coeffs[i] <<- coeff_frame
#         print(result_coeffs[[1]]$loc_ln)
#         print(names(multi_fit_sum))
#         print((multi_fit_sum$coefficients))
        
        pdf(paste("results\\regression\\coefficients_dataset_",i,".pdf",sep=""), height=nrow(coeffs)/2, width=10)
        grid.table(coeffs)
        dev.off()
}

# tt <- train_data[2]
# applyRegression(train_data[3],3)
x <- mapply(applyRegression,train_data,seq_along(train_data),SIMPLIFY = F)

print(result_coeffs[1]$loc_ln)
# print(data.frame(result_coeffs[1]))
