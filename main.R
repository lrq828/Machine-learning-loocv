rm(list = ls())
{
  library(survival)
  library(randomForestSRC)
  library(glmnet)
  library(plsRcox)
  library(superpc)
  library(gbm)
  library(CoxBoost)
  library(survivalsvm)
  library(dplyr)
  library(tibble)
  library(BART)
  library(survival)
  library(survminer)
  library(timeROC)
  library(data.table)
  library(tidyr)
  library(dplyr)
  library(tibble)
  library(caret)
  library(ggRandomForests)
  source("fun/CoxBoost.R");source("fun/Enet.R");source("fun/GBM.R");source("fun/plsRcox.R")
  source("fun/RSF.R");source("fun/StepCox.R");source("fun/superPC.R");source("fun/Survivalsvm.R")
  source("cindex.R")
  
  suppressWarnings(suppressMessages(future::plan("multiprocess",workers = 5)))
}
##################################
#### 准备工作 ####
##################################
{
  suppressWarnings(suppressMessages(future::plan("multiprocess",workers = 6)))
  setwd("E:\\CA\\machine learning\\circle")
  
  result <- data.frame();cc <- data.frame()
  #读取数据
  data <- read.delim("test.txt")
  rownames(data)=data[,1]
  data=data[,-1]

  #划分训练集和测试集
  size <- createDataPartition(data$OS.time,p=0.7,list = F)  # 随机选择70%的数据作为Train data
  est_dd <- data[size,]
  val_dd_list <- data[-size,]
  # est_dd <- read.delim("matrix.txt")
  # val_dd_list <- read.delim("test.txt")
  
  rf_nodesize <- 15
  seed <- 2022
  set.seed(seed)
}

funlists <- c(StepCox,Enet,myCoxBoost,RSF,myplsRcox,mySurvivalsvm,GBM,superpc)
names <- c("StepCox","Enet","CoxBoost","RSF","plsRcox","Survivalsvm","GBM","superpc")
dirnames <- c("both","backward","forward")
alphalist <- c(seq(0,1,0.1))

for (i in c(seq(1,8,1))) {
  # StepCox
  if(i==1){ 
    firstfun <- funlists[[i]]
    for (j in c(seq(1,2,1))) {
      ll <- firstfun(est_dd,val_dd_list,dirnames[j])
      est_dd2 <- as.data.frame(ll[2]);val_dd_list2 <- as.data.frame(ll[3])
      ############## 第二层算法########################
      for (k in c(seq(1,8,1))) {
        # Enet
        if(k==2){ 
          secondfun <- funlists[[k]]
          for (m in c(seq(1,11,1))) {
            lll <- secondfun(est_dd2,val_dd_list2,alphalist[m])
            train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
            cc <- as.data.frame(cbind(train_cindex,test_cindex))
            #模型结果
            cc$Model <- paste0(names[i],'(',dirnames[j],')','+',names[k],'(alpha=',alphalist[m],')')
            result <- rbind(result,cc)
          }
        }
        # myCoxBoost,RSF
        else if(k %in% c(3,4)){
          secondfun <- funlists[[k]]
          lll <- secondfun(est_dd2,val_dd_list2)
          train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'(',dirnames[j],')','+',names[k])
          result <- rbind(result,cc)
        }
        # myplsRcox,mySurvivalsvm,GBM,superpc
        else if(k %in% c(seq(5,8,1))){ 
          secondfun <- funlists[[k]]
          lll <- secondfun(est_dd2,val_dd_list2)
          train_cindex <- unlist(lll[2]);test_cindex <- unlist(lll[3])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'(',dirnames[j],')','+',names[k])
          result <- rbind(result,cc)
        }
        #null
        else if(k==i){
          train_cindex <- unlist(ll[4]);test_cindex <- unlist(ll[5])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'(',dirnames[j],')')
          result <- rbind(result,cc)
        }
      }
    }
  }
  # Enet
  else if(i==2){ 
    firstfun <- funlists[[i]]
    for (j in c(seq(1,11,1))) {
      ll <- firstfun(est_dd,val_dd_list,alphalist[j])
      est_dd2 <- as.data.frame(ll[2]);val_dd_list2 <- as.data.frame(ll[3])
      ############### 第二层算法##################
      for (k in c(seq(1,8,1))) {
        # StepCox
        if(k==1){ 
          secondfun <- funlists[[k]]
          for (m in c(seq(1,3,1))) {
            lll <- secondfun(est_dd2,val_dd_list2,dirnames[m])
            train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
            cc <- as.data.frame(cbind(train_cindex,test_cindex))
            #模型结果
            cc$Model <- paste0(names[i],'(alpha=',alphalist[j],')','+',names[k],'(',dirnames[m],')')
            result <- rbind(result,cc)
          }
        }
        # myCoxBoost,RSF
        else if(k %in% c(3,4)){
          secondfun <- funlists[[k]]
          lll <- secondfun(est_dd2,val_dd_list2)
          train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'(alpha=',alphalist[j],')','+',names[k])
          result <- rbind(result,cc)
        }
        # myplsRcox,mySurvivalsvm,GBM,superpc
        else if(k %in% c(seq(5,8,1))){ 
          secondfun <- funlists[[k]]
          lll <- secondfun(est_dd2,val_dd_list2)
          train_cindex <- unlist(lll[2]);test_cindex <- unlist(lll[3])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'(alpha=',alphalist[j],')','+',names[k])
          result <- rbind(result,cc)
        }
        #null
        else if(k==i){
          train_cindex <- unlist(ll[4]);test_cindex <- unlist(ll[5])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'(alpha=',alphalist[j],')')
          result <- rbind(result,cc)
        }
      }
    }
  }
  # myCoxBoost,RSF
  else if(i %in% c(3,4)){ 
    firstfun <- funlists[[i]]
    ll <- firstfun(est_dd,val_dd_list)
    est_dd2 <- as.data.frame(ll[2]);val_dd_list2 <- as.data.frame(ll[3])
    ##########第二层算法#########
    for (k in c(seq(1,8,1))) {
      # StepCox
      if(k==1){ 
        secondfun <- funlists[[k]]
        for (m in c(seq(1,3,1))) {
          lll <- secondfun(est_dd2,val_dd_list2,dirnames[m])
          train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'+',names[k],'(',dirnames[m],')')
          result <- rbind(result,cc)
        }
      }
      # Enet
      else if(k==2){ 
        secondfun <- funlists[[k]]
        for (m in c(seq(1,11,1))) {
          lll <- secondfun(est_dd2,val_dd_list2,alphalist[m])
          train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
          cc <- as.data.frame(cbind(train_cindex,test_cindex))
          #模型结果
          cc$Model <- paste0(names[i],'+',names[k],'(alpha=',alphalist[m],')')
          result <- rbind(result,cc)
        }
      }
      # myplsRcox,mySurvivalsvm,GBM,superpc
      else if(k %in% c(seq(5,8,1))){ 
        secondfun <- funlists[[k]]
        lll <- secondfun(est_dd2,val_dd_list2)
        train_cindex <- unlist(lll[2]);test_cindex <- unlist(lll[3])
        cc <- as.data.frame(cbind(train_cindex,test_cindex))
        #模型结果
        cc$Model <- paste0(names[i],'+',names[k])
        result <- rbind(result,cc)
      }
      # RSF
      else if(k == 3 && i != 3){
        secondfun <- funlists[[k]]
        lll <- secondfun(est_dd2,val_dd_list2)
        train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
        cc <- as.data.frame(cbind(train_cindex,test_cindex))
        #模型结果
        cc$Model <- paste0(names[i],'(alpha=',alphalist[j],')','+',names[k])
        result <- rbind(result,cc)
      }
      # myCoxBoost
      else if(k == 4 && i != 4){
        secondfun <- funlists[[k]]
        lll <- secondfun(est_dd2,val_dd_list2)
        train_cindex <- unlist(lll[4]);test_cindex <- unlist(lll[5])
        cc <- as.data.frame(cbind(train_cindex,test_cindex))
        #模型结果
        cc$Model <- paste0(names[i],'(alpha=',alphalist[j],')','+',names[k])
        result <- rbind(result,cc)
      }
      else if(k==i){
        train_cindex <- unlist(ll[4]);test_cindex <- unlist(ll[5])
        cc <- as.data.frame(cbind(train_cindex,test_cindex))
        #模型结果
        cc$Model <- paste0(names[i])
        result <- rbind(result,cc)
      }
    }
  }
  # myplsRcox,mySurvivalsvm,GBM,superpc
  else if(i %in% c(seq(5,8,1))){
    firstfun <- funlists[[i]]
    ll <- firstfun(est_dd,val_dd_list)
    train_cindex <- unlist(ll[2]);test_cindex <- unlist(ll[3])
    cc <- as.data.frame(cbind(train_cindex,test_cindex))
    #模型结果
    cc$Model <- paste0(names[i])
    result <- rbind(result,cc)
  }
}

write.csv(result,"result.csv")
