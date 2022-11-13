# Machine-learning-combination
CoxBoost、Enet(alpha=0.1-0.9)、Ridge(Enet(alpha=0))、Lassos(Enet(alpha=1))、GBM、plsRcox、RSF、StepCox、SuperPC、SurvivalSVM
十种算法相互结合
如StepCox(both)+Enet(alpha=0.2)、StepCox(both)+plsRcox、StepCox(backward)+plsRcox、plsRcox、Enet(alpha=0)+StepCox(forward)、Enet(alpha=0)、RSF+StepCox(both)、Enet(alpha=0)+plsRcox、RSF+StepCox(forward)、RSF+plsRcox、Enet(alpha=0.1)+superpc、Enet(alpha=0.2)+superpc、StepCox(both)+superpc、StepCox(backward)+superpc等组合，共188种详见combination.txt。
创建CoxBoost、Enet、GBM、plsRcox、RSF、StepCox、SuperPC、SurvivalSVM函数文件(xxx.R)
在mian.R中加载所有函数文件(xxx.R)，通过for循环把算法结合起来，计算每种组合C-index
只需运行main.R文件即可


待改进：
函数文件(xxx.R)和mian.R的代码写得有点乱(但不影响运行)，待改进
如：main.R中的C-index计算可以创建一个函数，那样就不用在每个for下面写三到四行代码来计算，这样代码应该会简洁不少;
部分library的R包可能会用不到

