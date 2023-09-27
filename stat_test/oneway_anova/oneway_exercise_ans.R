# R statistics exercise 
# 2021/5/12
# Naoto Yamashita

#Q1. PlantGrowthというR中のデータをロードする
data("PlantGrowth")

#Q2. データの先頭を表示して，各種統計量を一応チェックする
head(PlantGrowth)
library(psych)
describe(PlantGrowth)
describeBy(PlantGrowth$weight, PlantGrowth$group)
plot(PlantGrowth)

#Q3. groupを要因とした，weightに対する1要因分散分析を行う
aov_res <- aov(weight ~ group, PlantGrowth)
anova(aov_res)

#Q4. 多重比較を行う必要があれば，行う
#Bonferroni法
pairwise.t.test(PlantGrowth$weight, #差を比較したい変数
                PlantGrowth$group,  #群を表す変数
                p.adjust.method = "bonferroni"#ボンフェローニ法による多重比較
)

#TukeyのHSD法
TukeyHSD(aov_res, "group")

#Dunnett法
library(multcomp)
summary(glht(aov_res,linfct=mcp(group="Dunnett")))

#Games-Howell法
library(rstatix)
games_howell_test(PlantGrowth, weight~group)

#Q5. 結果を報告する文章を書く
############################################################################
# 植物の生育重量に対して，グループを要因とした一要因分散分析を行った．
# なお，有意水準は5%とした．その結果，グループの有意な主効果が認めら
# れた(F(2, 27)=4.846, p < 0.05)．その上で，TukeyのHSD法による多重比較検定
# を，有意水準5%で行ったところ，trt1（平均値は4.661）とtrt2（平均値は5.526）
# の間に，重量の有意な差（p = 0.012）が認められた．
############################################################################