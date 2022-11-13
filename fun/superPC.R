# 只能放后面
superpc <- function(est_dd,val_dd_list){
  data <- list(x=t(est_dd[,-c(1,2)]),y=est_dd$OS.time,censoring.status=est_dd$OS,
               featurenames=colnames(est_dd)[-c(1,2)])
  fit <- superpc.train(data = data,type = 'survival',s0.perc = 0.5) #default
  cv.fit <- superpc.cv(fit,data,n.threshold = 20,#default 
                       n.fold = 10,
                       n.components=3,
                       min.features=5,
                       max.features=nrow(data$x),
                       compute.fullcv= TRUE,
                       compute.preval=TRUE)
  test <-
    list(
      x = t(val_dd_list[, -c(1, 2)]),
      y = val_dd_list$OS.time,
      censoring.status = val_dd_list$OS,
      featurenames = colnames(val_dd_list)[-c(1, 2)]
    )
  ff <-
    superpc.predict(fit,
                    data,
                    test,
                    threshold = cv.fit$thresholds[which.max(cv.fit[["scor"]][1, ])],
                    n.components = 1)
  
  RS <- as.numeric(ff$v.pred)
  return(list(fit,RS))
}