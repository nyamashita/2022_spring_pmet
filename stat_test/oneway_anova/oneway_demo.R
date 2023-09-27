# R statistics
# 2021/5/12
# Naoto Yamashita

#事前にirisデータを読み込みましょう
data(iris) #irisという変数に格納される

#1要因分散分析
##一旦結果をaov_resに格納する
aov_res <- aov(Sepal.Length ~ Species, data = iris)
#格納した結果を，anova関数に与えて分散分析表を作る
anova(aov_res)

#全体平方和 = データから平均値を引いて二乗して和をとる
## 分散分析表の「合計」と一致するかチェック
mean_all <- mean(iris$Sepal.Length)
ss_all <- sum((iris$Sepal.Length - mean_all)^2)
ss_all

#要因の平方和
##speciesごとにデータ抽出する
data_setosa     <- iris[iris$Species=="setosa",     "Sepal.Length"]
data_versicolor <- iris[iris$Species=="versicolor", "Sepal.Length"]
data_virginica  <- iris[iris$Species=="virginica",  "Sepal.Length"]
#speciesごとの平均値を計算
mean_setosa     <- mean(data_setosa)
mean_versicolor <- mean(data_versicolor)
mean_virginica  <- mean(data_virginica)
##要因平方和を求める
ss_setosa     <- (mean_setosa     - mean_all)^2 * 50
ss_versicolor <- (mean_versicolor - mean_all)^2 * 50
ss_virginica  <- (mean_virginica  - mean_all)^2 * 50
ss_species    <- ss_setosa + ss_virginica + ss_versicolor
ss_species

#残差平方和
ss_residual <- ss_all - ss_species
ss_residual


#平均平方和＝平方和/自由度
mss_species <- ss_species/2
mss_species
mss_resaiual <- ss_residual/147
mss_resaiual

#検定統計量 F値
Fval <- mss_species/mss_resaiual
Fval

#p値，F値に対応する確率＝ありえる度を計算
df(Fval, 2, 147)

#多重比較検定
pairwise.t.test(iris$Sepal.Length, #差を比較したい変数
                iris$Species,  #群を表す変数
                p.adjust.method = "bonferroni"#ボンフェローニ法による多重比較
                )

#結果の書き方の棒グラフ
barplot(c(mean_setosa, mean_versicolor, mean_virginica),
        names.arg = c("setosa", "versicolor", "virginica"))

#さまざまな多重比較
#TukeyのHSD法
TukeyHSD(aov_res, "Species")

#Dunnett法
#multcompというライブラリをインストールしておく
library(multcomp)
summary(glht(aov_res,linfct=mcp(Species="Dunnett")))

#Games-Howell法
#rstatixというライブラリをインストールしておく
library(rstatix)
games_howell_test(iris, Sepal.Length~Species)
