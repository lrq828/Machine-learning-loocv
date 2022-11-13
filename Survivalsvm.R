# 只能放后面
mySurvivalsvm <- function(est_dd,val_dd_list){
  fit = survivalsvm(Surv(OS.time,OS)~., data= est_dd, gamma.mu = 1)
  RS=as.numeric(predict(fit, val_dd_list)$predicted)
  return(list(fit,RS))
  }