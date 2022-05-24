## 心理測定論
##  探索的因子分析
##  Naoto Yamashita

#personal.csvを読み込む

#psychパッケージ
library(psych)

#######################
## 初期解(直交解)
#######################
init_res <- fa(personal, nfactors = 2, rotate = "none", fm = "ml")
init_res #summaryを使わないほうが素直に結果が出る

#相関係数の固有値プロット
plot(eigen(cor(personal))$values, type="o", xlab = "factors", ylab = "eigen value")
abline(h = 1, lty = 2)

#因子負荷量, 因子パターン
init_res$loadings

#直交解の性質
#共通性
init_res$communalities
apply(init_res$loadings^2, 1, sum) #横方向の2乗和とおなじ
#寄与率
init_res$loadings#この下の方 4.062/8=Proportion varに一致
#因子構造
init_res$Structure #因子パターンと一致
#因子間相関
#回転法=noneの場合だと出力されない

#######################
## 直交回転した解
#######################
#varimax回転
orth_res <- fa(personal, nfactors = 2, rotate = "varimax", fm = "ml")
#目的関数の値を比較する
init_res$objective
orth_res$objective
#回転後の因子パターン
unclass(orth_res$loadings)#unclassに入れてやると，全部の負荷量を表示してくれる

#######################
## 斜交回転した解
#######################
#promax回転
oblq_res <- fa(personal, nfactors = 2, rotate = "promax", fm = "ml")
#目的関数の値を比較する
oblq_res$objective
oblq_res$Phi #因子間相関が出ている
#回転後の因子パターン
unclass(oblq_res$loadings)


#因子得点の推定
#デフォルトの設定では，regressionで推定している．
head(oblq_res$scores)


#並行分析
fa.parallel(scale(personal), fa = "fa")
