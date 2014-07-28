library(nlme)
library(gridExtra)
source("createsampledata.R")
source("calculateerror.R")


# result_coeffs1 <- NULL
# result_coeffs2 <- NULL
# result_coeffs3 <- NULL
# result_coeffs4 <- NULL
# result_coeffs5 <- NULL
# result_coeffs6 <- NULL
result_coeffs <- list()#c(result_coeffs1,result_coeffs2,result_coeffs3,result_coeffs4,result_coeffs5,result_coeffs6)
# result_coeffs <- vector("frame")
lm_list <- list()
final_result_pdf <- matrix(nrow = 11,ncol=0)
r2 <- list()

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
        t <- data.frame(train_data_item, row.names = NULL)
        
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
                formula <- actual_ln ~ rely_factor+loc_ln
#                 formula <- t$actual_ln ~ t$rely_factor+t$loc_ln
        if(i == 2)
                formula <- actual_ln ~ time_factor+rely_factor+data_factor+
                                stor_factor+virt_factor+acap_factor+pcap_factor+
                                modp_factor+loc_ln+dev_mode_factor
        if(i == 3)
                formula <- actual_ln ~ +rely_factor+acap_factor+loc_ln
        if(i == 4)
                formula <- actual_ln ~ time_factor+rely_factor+acap_factor+
                                lexp_factor+modp_factor+loc_ln
        if(i == 5)
                formula <- actual_ln ~ acap_factor+dev_mode_factor+loc_ln
        if(i == 6)
                formula <- actual_ln ~ time_factor+rely_factor+acap_factor+
                                modp_factor+dev_mode_factor+loc_ln

        multi_fit <- lm(formula, data = t)

        lm_list[[i]] <<- multi_fit

#         coeff_frame <- data.frame(coeffs,row.names = names(coeffs))
#         row.names(coeff_frame) <- names(coeffs)
#         result_coeffs[[i]] <<- list(coeff_frame)
        
        #         result_coeffs[i] <<- coeff_frame
        multi_fit_sum <- summary(multi_fit)

        r2 <<- rbind(r2,multi_fit_sum$r.squared)

        coeffs <- multi_fit_sum$coefficients[,1]
        h <- nrow(t)/2
        pdf(paste("results\\regression\\coefficients_dataset_",i,".pdf",sep=""), height=h, width=10)
        grid.table(coeffs)
        dev.off()

}

testRegression <- function(test_data_item,i){
        t <- data.frame(test_data_item, row.names = NULL)
#         tt2 <- tt[,c('rely_factor','loc_ln','actual_ln')]
        predicted_ln <- predict(object = lm_list[[i]],newdata = t,interval = "prediction")
        predicted <- exp(predicted_ln[,'fit'])
        actual <- t[,'actual']
        com_res <- data.frame(actual, predicted)

        if(nrow(com_res) < 11) com_res <- rbind(com_res,NA)

        final_result_pdf <<- cbind(final_result_pdf, com_res)
        

        h <- nrow(t)/2
        pdf(paste("results\\regression\\predicted_dataset_",i,".pdf",sep=""), height=h, width=7)
        grid.table(com_res)
        dev.off()
}

doRegression <- function(train_data,test_data){
        x1 <- mapply(applyRegression,train_data,seq_along(train_data),SIMPLIFY = F)
        x2 <- mapply(testRegression,test_data,seq_along(test_data),SIMPLIFY = F)
        
        results <- data.frame(final_result_pdf,row.names=NULL)
        colnames(results) <- c("Conj1_Obs","Conj1_Est","Conj2_Obs","Conj2_Est","Conj3_Obs","Conj3_Est","Conj4_Obs","Conj4_Est","Conj5_Obs","Conj5_Est","Conj6_Obs","Conj6_Est")
        
        pdf("results\\regression\\data_output.pdf", height=5, width=17)
        grid.table(results)
        dev.off()
        
        results
#         calculateError(results)
}

calculateRegError <- function(result_data){
        error_rate <- calculateMMRE(result_data)
        error_rate
}
getRegR2 <- function(){
        r2
}

# doRegression(train_data,test_data)
# applyRegression(train_data[1],1)
# testRegression(test_data[1],1)

# print(r2)