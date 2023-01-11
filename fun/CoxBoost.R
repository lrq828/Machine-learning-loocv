myCoxBoost <- function(est_dd,val_dd_list){
  pen <- optimCoxBoostPenalty(est_dd[,'OS.time'],est_dd[,'OS'],as.matrix(est_dd[,-c(1,2)]),
                              trace=TRUE,start.penalty=500,parallel = T)
  
  cv.res <- cv.CoxBoost(est_dd[,'OS.time'],est_dd[,'OS'],as.matrix(est_dd[,-c(1,2)]),
                        maxstepno=500,K=10,type="verweij",penalty=pen$penalty)
  
  fit <- CoxBoost(est_dd[,'OS.time'],est_dd[,'OS'],as.matrix(est_dd[,-c(1,2)]),
                  stepno=cv.res$optimal.step,penalty=pen$penalty)
  
  train_RS <- fit$linear.predictor[nrow(fit$linear.predictor),]
  train_cindex=cindex(est_dd,train_RS)
  
  test_RS=as.numeric(predict(fit,newdata=val_dd_list[,-c(1,2)], newtime=val_dd_list[,1], newstatus=val_dd_list[,2], type="lp"))
  test_cindex=cindex(val_dd_list,test_RS)
  
  rid<-names(coef(fit)[coef(fit)!=0])
  est_dd2 <- est_dd[,c('OS.time','OS',rid)]
  val_dd_list2 <- val_dd_list[,c('OS.time','OS',rid)]
  return(list(fit,est_dd2,val_dd_list2,train_cindex,test_cindex))
}