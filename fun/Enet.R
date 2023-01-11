Enet <- function(est_dd,val_dd_list,alpha){
  fit = cv.glmnet(as.matrix(est_dd[,3:ncol(est_dd)]),
                as.matrix(Surv(est_dd$OS.time,est_dd$OS)),
                family = "cox",
                alpha = alpha,
                nfolds = 10)
  
  train_RS <- as.numeric(predict(fit,type = 'link', newx = as.matrix(est_dd[,-c(1, 2)]),s = fit$lambda.min))
  train_cindex=cindex(est_dd,train_RS)
  
  test_RS = as.numeric(predict(fit,type = 'link', newx = as.matrix(val_dd_list[,-c(1, 2)]),s = fit$lambda.min))
  test_cindex=cindex(val_dd_list,test_RS)
  

  rid <- rownames(coef(fit,s = "lambda.min"))[which(coef(fit,s = "lambda.min")!=0)]
  if (length(rid) <= 1){
    
  }
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,est_dd2,val_dd_list2,train_cindex,test_cindex))
}
