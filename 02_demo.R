## 心理測定論 第2回
## yyyy/mm/dd

head(tshirt)

#重回帰分析
res_lm <- lm(sales ~ material + price + design, tshirt)
res_lm$coefficients

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


#分散説明率と一致するいろいろ
## そのまま
var(res_lm$fitted.values)/var(tshirt$sales)
1 - var(res_lm$residuals)/var(tshirt$sales)

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
