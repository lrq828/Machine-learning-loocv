Enet <- function(est_dd,val_dd_list,alpha){
  fit = cv.glmnet(as.matrix(est_dd[,3:ncol(est_dd)]),
                as.matrix(Surv(est_dd$OS.time,est_dd$OS)),
                family = "cox",
                alpha = alpha,
                nfolds = 10)
  RS = as.numeric(predict(fit,type = 'link', newx = as.matrix(val_dd_list[,-c(1, 2)]),s = fit$lambda.min))
  
  rid <- rownames(coef(fit,s = "lambda.min"))[which(coef(fit,s = "lambda.min")!=0)]
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,RS,est_dd2,val_dd_list2))
}
