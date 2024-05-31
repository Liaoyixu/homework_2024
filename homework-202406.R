##HEADER --------------------------------------------------------------------
#
#Scrip name:   "data_exploration.R"
#Purpose:      "Exploratory data analysis"
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/4/16        
#
#SETUP ---------------------------------------------------------------------

###############################################################################
# 下载相关的包
install.packages("corrplot")
install.packages("car")
install.packages("carData")
# 加载必要的包
library(ade4)
library(corrplot)  # 用于相关性矩阵的可视化
library(carData)
library(car)       # 用于计算 VIF
# 加载 Doubs 数据集
data(doubs)
doubs
###############################################################################
# 第一部分：删除 Doubs 数据集中数据缺失的采样点
# 鱼类多度
spe <- doubs$fish
range(spe) #整个多度数据值的范围
ab <- table(unlist(spe)) # 计算没种多度值的数量
# 鱼类多度分级
barplot(ab,las = 1,xlab = "abund degree",ylab = "frequency",col = gray(5:0/5))
sum(spe == 0) # 多度数据中0值所占比例

# 鱼类分布情况
spa <- doubs$xy
plot(spa,asp = 1,type = "n",xlab = "x(km)",ylab = "y(km)") # 设置图面宽度
lines(spa,col="light blue")
text(spa,row.names(spa),cex = 0.5,col = "red") # 用行号标记取样点
text(70,10,"shangyou",cex = 0.8,col = "red" ) # 标记上游
text(20,120,"xiayou",cex = 0.8,col="red") # 标记下游

# 探索鱼类分布与环境之间的关系
par(mfrow = c(2,2)) # 将绘图窗口分为四块
# 查看四种鱼多度沿河的分布情况
plot(spa,asp=1,col="brown",cex=spe$Cogo,xlab="x(km)",ylab="y(km)")
lines(spa,col="lightblue")
plot(spa,asp=1,col="brown",cex=spe$Satr,xlab="x(km)",ylab="y(km)")
lines(spa,col="lightblue")
plot(spa,asp=1,col="brown",cex=spe$Phph,xlab="x(km)",ylab="y(km)")
lines(spa,col="lightblue")
plot(spa,asp=1,col="brown",cex=spe$Thth,xlab="x(km)",ylab="y(km)")
lines(spa,col="lightblue")

# 分析鱼类在不同采样点的存在情况
# 计算不同采样点鱼类数量
spe.pres <- apply(spe > 0, 2 ,sum) # 2表示按列来执行函数的算法
sort(spe.pres) # 从小到大排序
spe.relf <- 100*spe.pres/nrow(spe) # 计算相对频次
round(sort(spe.relf), 1 ) # 排序并保留1位小数
par(mfrow=c(1,2)) # 绘制窗口
# 绘制鱼类存在数量直方图
hist(spe.pres,right = FALSE,las=1,xlab = "site",ylab = "species",
     breaks = seq(0,30,by=5),col = "yellowgreen")
# 绘制鱼类存在相对频次直方图
hist(spe.relf,right = FALSE,las=1,xlab = "site(%)",ylab = "species",
     breaks = seq(0,100,by=10),col = "orange")

# 分析鱼类物种丰度
sit.pres <- apply(spe > 0, 1, sum)
sort(sit.pres)
par(mfrow = c(1,2))
plot(sit.pres,type = "s",las=1,col="gray",xlab = "site",
     ylab = "species abundance") # type=s在图形中数据显示为阶梯图
text(sit.pres,row.names(spe),cex = .5,col = "orange")
plot(spa,asp=1,pch=21,col="white",bg="brown",cex=5*sit.pres/max(sit.pres),
     xlab = "x(km)",ylab = "y(km)")
lines(spa,col="yellowgreen")
# 其中8号采样点无物种数据，应将其删除
spe <- spe[-8,]
spa <- spa[-8,]
Env <- Env[-8,]

###############################################################################
# 第二部分：检测环境因素是否存在共线性
# 载入环境因素
Env <- doubs$env
# 绘制环境因子的空间分布图
par(mfrow = c(2,2))
plot(spa,asp = 1,pch = 21,col = "white",bg = "yellow",
     cex = 5*Env$alt/max(Env$alt),xlab = "x",ylab = "y")
lines(spa,col = "lightblue")
plot(spa,asp = 1,pch = 21,col = "white",bg = "yellowgreen",
     cex = 5*Env$deb/max(Env$deb),xlab = "x",ylab = "y")
lines(spa,col = "lightblue")
plot(spa,asp = 1,pch = 21,col = "white",bg = "green",
     cex = 5*Env$oxy/max(Env$oxy),xlab = "x",ylab = "y")
lines(spa,col = "lightblue")
plot(spa,asp = 1,pch = 21,col = "white",bg = "lightgreen",
     cex = 5*Env$nit/max(Env$nit),xlab = "x",ylab = "y")
lines(spa,col = "lightblue")
# 绘制环境因子随距源头距离的变化图
par(mfrow = c(2,2))
plot(Env$dfs,Env$alt,type = "l",xlab = "dfs(km)",ylab = "alt(km)",col = "yellow")
plot(Env$dfs,Env$deb,type = "l",xlab = "dfs(km)",ylab = "flow(m3/s)",col = "yellowgreen")
plot(Env$dfs,Env$oxy,type = "l",xlab = "dfs(km)",ylab = "oxy(mg/L)",col = "green")
plot(Env$dfs,Env$nit,type = "l",xlab = "dfs(km)",ylab = "nit(mg/L)",col = "lightgreen")
# 分析环境变量之间的关系
# 带频度分布的柱状图和光滑拟合曲线的双变量散点图
download.file("panelutils.R")
source("panelutils.R")
pairs(Env, panel = panel.smooth, diag.panel = panel.hist,
      main="双变量散点图（带频度分布图和平滑曲线）")
# 查看是否存在共线性

###############################################################################
# 第三部分：分析鱼类与环境因素之间的关系并可视化
# 查看Axis lengths选择限制性排序还是非限制性排序
vegan::decorana(spe)
vegan::decorana(Env) #DCA值都小于4 选择非限制性排序，分析生物与环境的关系选择RDA
# 标准化
spe.hel <- vegan::decostand(spe,method = "hellinger")
env.z <- vegan::decostand(Env,method = "standardize")
# 运行 RDA 并提取关键结果
spe.rda <- rda(spe.hel ~ .,data = env.z)
# 选择重要变量
summary(spe.rda) # 73%可以用环境因素解释
vif.cca(spe.rda)
# 向前选择
fwd.sel <- ordiR2step(rda(spe.hel ~ 1,data = env.z),
                      formula(spe.rda),direction="forward",R2scope = TRUE,
                      pstep = 1000,trace = FALSE)
fwd.sel$call
spe.rda.signif <- rda(formula = spe.hel ~ dfs + oxy + bdo, data = env.z)
RsquareAdj(spe.rda.signif)

# 可视化
# scaling = 1
ordiplot(spe.rda.signif,scaling = 1,type = "text")
# scaling = 2
ordiplot(spe.rda.signif,scaling = 2,type = "text")
