cindex <- function(data,RS){
  rs=as.data.frame(cbind(data[,1:2],RS))
  aa=coxph(Surv(OS.time,OS)~RS,rs)
  cindex=as.numeric(summary(aa)$concordance[1])
  return(cindex)
}