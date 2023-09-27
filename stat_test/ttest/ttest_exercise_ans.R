# R statistics exercise (ANS)
# 2021/4/28
# Naoto Yamashita

#Q0: d02_itudatsu.csvデータを読み込む

#Q1: 中2の男女の逸脱行動得点が異なるかどうかを検定
## このデータは対応のないデータ
## 等分散性の検定
var.test(data[data$sex=="M", "chu2"], data[data$sex=="F", "chu2"])
## 帰無仮説を棄却できない = 等分散性の仮定が成り立たないので，Welchの検定
t.test(data[data$sex=="M", "chu2"], data[data$sex=="F", "chu2"], var.equal = FALSE)

#Q2: 小6と中2の逸脱行動得点が異なるかを検定
## このデータは対応のあるデータなので，対応のあるt検定
t.test(data$syo6, data$chu2, paired = TRUE)
