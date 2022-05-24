## 心理測定論
##  確認的因子分析
##  Naoto Yamashita

#personal.csvを読み込む

#lavaanによる確認的因子分析
library(lavaan)#事前にインストールしておこう

## モデル式の作成
model <-
"
  katsudo =~ sekkyoku + sendo + yaruki + tyutyo
  syako =~ youki + buaiso + hanashi + 1*ninki
"
## モデルのフィットとパラメータ推定
res_cfa <- cfa(model, personal, std.lv = TRUE)

## 推定されたパラメータ値を確認
summary(res_cfa, standardized=TRUE)

## 適合度指標の計算
fitMeasures(res_cfa)


#識別されないモデル
model_unidentified <- "
  katsudo =~ sekkyoku + sendo + yaruki + tyutyo + youki + buaiso + hanashi + ninki
  syako =~ youki + buaiso + hanashi + 1*ninki
"
res_unidentified <- cfa(model_unidentified, personal)

#不適解を起こす
model_improper <- "
  katsudo =~ sekkyoku + sendo + yaruki + tyutyo
  syako =~ youki + buaiso + hanashi + 1*ninki
"
##データを減らしてみた場合
res_improper <- cfa(model_improper, personal[c(1:10),])
