# 只能放后面
GBM <- function(est_dd,val_dd_list){
  fit <- gbm(formula = Surv(OS.time,OS)~.,data = est_dd,distribution = 'coxph',
             n.trees = 10000,
             interaction.depth = 3,
             n.minobsinnode = 10,
             shrinkage = 0.001,
             cv.folds = 10,n.cores = 6)
  # find index for number trees with minimum CV error
  best <- which.min(fit$cv.error)
  set.seed(seed)
  fit <- gbm(formula = Surv(OS.time,OS)~.,data = est_dd,distribution = 'coxph',
             n.trees = best,
             interaction.depth = 3,
             n.minobsinnode = 10,
             shrinkage = 0.001,
             cv.folds = 10,n.cores = 8)
  RS=as.numeric(predict(fit,val_dd_list,n.trees = best,type = 'link'))
  return(list(fit,RS))
}