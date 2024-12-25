#1.卡方检验  独立性检验
#卡方检验可用来评估两个分类变量变量之间的相关性【分类变量！！！】，分类变量之间是否存在统计学上的关联性
library(vcd)
#创建列联表 这个是卡方
mytable <- table(Arthritis$Treatment,Arthritis$Improved)
#mytable <- xtabs(~Treatment+Improved, data=Arthritis)
mytable
chisq.test(mytable)
#算出来的卡方的p value 为0.001463。所以拒绝原假设。认为治疗方法与治疗结果之间存在显著的关联。

mytable1 <- table(Arthritis$Sex,Arthritis$Improved)
chisq.test(mytable1)
#p value 0.088 ，说明 Sex 和 Improved 这两个变量之间没有显著的统计关联。
#这里有警告了 ：Warning message:In chisq.test(mytable1) : Chi-squared approximation may be incorrect

#2.Fisher 精确检验
mytable1 <- table(Arthritis$Sex,Arthritis$Improved)
fisher.test(mytable1)
#p value: 0.1094

#3-1.相关性度量
mytable2 <- xtabs(~Treatment+Improved, data=Arthritis)
assocstats(mytable2)

#3-2.相关分析 相关系数可以用来描述定量变量之间的关系
#协方差 和 相关系数
##•	协方差：只是告诉你两个变量是“同涨同跌”还是“反着涨跌”，但数值大小不太好理解，因为单位不一样（比如温度和体重，怎么比较？）。
#•	相关系数：告诉你两个变量的相关性强弱，数值清楚直观，适合用来分析变量之间的关系。
states <- state.x77[,c(1:6)]
cov(states)
cor(states)

#相关性的显著性检验
cor.test(states[,3],states[,5])
library(psych)
corr.test(states,use="complete")

#4.t检验
#独立样本t检验
library(MASS)
attach(UScrime)
class(UScrime$So)
#用来比较 UScrime 数据集中 Prob 变量在 So 分类下两个组别的均值是否存在显著差异。
t.test(Prob ~ So, data=UScrime) 

#非独立样本t检验
t.test(UScrime$U1,UScrime$U2, paired = TRUE)
