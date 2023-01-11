# 只能放后面
superpc <- function(est_dd,val_dd_list){
  data <- list(x=t(est_dd[,-c(1,2)]),
               y=est_dd$OS.time,
               censoring.status=est_dd$OS,
               featurenames=colnames(est_dd)[-c(1,2)])
  fit <- superpc.train(data = data,type = 'survival',s0.perc = 0.5) #default
  cv.fit <- superpc.cv(fit,data,n.threshold = 20,#default 
                       n.fold = 10,
                       n.components=3,
                       min.features=2,
                       max.features=nrow(data$x),
                       compute.fullcv= TRUE,
                       compute.preval=TRUE)
  
  test <- list(x = t(val_dd_list[, -c(1, 2)]),
               y = val_dd_list$OS.time,
               censoring.status = val_dd_list$OS,
               featurenames = colnames(val_dd_list)[-c(1, 2)])
  
  train_ff <- superpc.predict(fit,
                              data,
                              data,
                              threshold = cv.fit$thresholds[which.max(cv.fit[["scor"]][1, ])],
                              n.components = 1)
  
  train_RS <- as.numeric(train_ff$v.pred)
  train_cindex=cindex(est_dd,train_RS)
  
  test_ff <- superpc.predict(fit,
                        data,
                        test,
                        threshold = cv.fit$thresholds[which.max(cv.fit[["scor"]][1, ])],
                        n.components = 1)
  
  test_RS <- as.numeric(test_ff$v.pred)
  test_cindex=cindex(val_dd_list,test_RS)

  return(list(fit,train_cindex,test_cindex))
}


