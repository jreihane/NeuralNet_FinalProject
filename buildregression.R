library(nlme)

data_csv <- read.csv("coc81.csv")

train_data <- create_train_data(data_csv)
test_data <- create_test_data(data_csv)
r_squared <- list()

dependent_variable_name <- "actual_ln"
independent_variable_names <- c("loc_ln","time_factor","rely_factor","data_factor",
                                        "cplx_factor","stor_factor","virt_factor","turn_factor",
                                        "acap_factor","aexp_factor","pcap_factor","vexp_factor",
                                        "lexp_factor","modp_factor","tool_factor","sced_factor",
                                        "dev_mode_factor")

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

        multi_fit <- lm(t$actual_ln ~ t$time_factor+t$rely_factor+t$data_factor+
                        t$cplx_factor+t$stor_factor+t$virt_factor+t$turn_factor+
                        t$acap_factor+t$aexp_factor+t$pcap_factor+t$vexp_factor+
                        t$lexp_factor+t$modp_factor+t$tool_factor+t$sced_factor+
                        t$loc_ln+t$dev_mode_factor)

        multi_fit_sum <- summary(multi_fit)
        r_squared <<- c(r_squared,(multi_fit_sum$r.squared))
print(names(multi_fit_sum))
print(multi_fit_sum$coefficients)
}

# tt <- train_data[2]
applyRegression(train_data[1],1)
# x <- mapply(applyRegression,train_data,seq_along(train_data),SIMPLIFY = F)


