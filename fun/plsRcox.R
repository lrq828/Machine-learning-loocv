# 只能放后面

myplsRcox <- function(est_dd,val_dd_list){
  pdf("plsRcox_plot/plsRcox.pdf")
  cv.plsRcox.res=cv.plsRcox(list(x=est_dd[,3:ncol(est_dd)],time=est_dd$OS.time,status=est_dd$OS),nt=10,verbose = FALSE)
  dev.off()
  fit <- plsRcox(est_dd[,3:ncol(est_dd)],time=est_dd$OS.time,event=est_dd$OS,nt=as.numeric(cv.plsRcox.res[5]))
  
  RS=as.numeric(predict(fit,type="lp",newdata=val_dd_list[,-c(1,2)]))
  return(list(fit,RS))
}

