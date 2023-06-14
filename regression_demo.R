## 心理測定論
##  回帰分析
##  Naoto Yamashita

#準備
#tshirt.csvを読み込もう

head(tshirt)

#重回帰分析
res_lm <- lm(sales ~ material + price + design, tshirt)
res_lm$coefficients #結果の表示; 偏回帰係数

#手作業で計算する場合
X <- as.matrix(cbind(1, tshirt[,1:3]))
y <- as.matrix(tshirt$sales)
w <- solve(t(X) %*% X) %*% t(X) %*% y
e <- y - X %*% w
J <- diag(nrow(X)) - rep(1,nrow(X))%*%t(rep(1,nrow(X)))/nrow(X) #中心化行列

#従属変数と予測
plot(tshirt$sales, res_lm$fitted.values)
cor(tshirt$sales, res_lm$fitted.values)

#誤差の性質
## 平均
residual_lm <- res_lm$residuals
mean(residual_lm)

## 独立変数との相関係数
cor(residual_lm, tshirt$material)
cor(residual_lm, tshirt$price)
cor(residual_lm, tshirt$design)

## 予測値との相関係数
cor(residual_lm, res_lm$fitted.values)

## 従属変数の分散 = 予測値の分散 + 残差の分散
var(tshirt$sales) - var(res_lm$fitted.values) - var(residual_lm)
sum((y)^2) - sum((X %*% w)^2) - sum((e)^2) #平方和の分解

#分散説明率と一致するいろいろ
## そのまま
var(res_lm$fitted.values)/var(tshirt$sales)
1 - var(res_lm$residuals)/var(tshirt$sales)
sum((J %*% X %*% w)^2)/sum((J%*%y)^2)

## 重相関係数の二乗
cor(tshirt$sales, res_lm$fitted.values)^2

## 決定係数: 下の方を見る
summary(res_lm)

#データを標準化したとき: 標準解
tshirt_scale <- scale(tshirt)
res_lm_std <- lm(sales ~ material + price + design, as.data.frame(tshirt_scale))
res_lm_std$coefficients

#相関係数と比較する
res_lm$coefficients
cor(tshirt$material, tshirt$sales)
cor(tshirt$price, tshirt$sales)
cor(tshirt$design, tshirt$sales)

#独立変数よりも個体が少ない場合 -> 偏回帰係数がNAになる
lm(sales ~ material + price + design, tshirt[c(1:2),])

#多重共線性
# materialと非常に相関が高いmaterial2という独立変数を追加
tshirt_mult <- data.frame(tshirt,
                          material2 = tshirt$material+0.01*rnorm(50))
res_lm_mult <- lm(sales ~ ., tshirt_mult)
res_lm_mult$coefficients #大きな偏回帰係数が含まれてる

##########################################
# 適用例; 自動車の燃費データ
data("mtcars")
mtcars <- mtcars[,c("mpg", "disp", "hp", "wt")]

#多重共線性をチェック; おそらく問題ない
cor(mtcars)

#個体数と変数数をチェック
dim(mtcars)#3独立変数に対して32個体，おそらく大丈夫

#多くの単位が混在しているため，標準解を採用
res_lm_cars <- lm(mpg ~ ., as.data.frame(scale(mtcars)))

#決定係数をチェック; 0.8376で問題なし
summary(res_lm_cars)

#解釈
res_lm_cars$coefficients
# disp(排気量): ほとんど影響なし
# hp(馬力)：馬力が上がると燃費が悪くなる
# wt(重量)：車が重いと燃費が悪くなる