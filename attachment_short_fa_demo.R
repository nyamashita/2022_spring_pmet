## 心理測定論
##  愛着スタイルの短縮版尺度
##  Naoto Yamashita
library(psych)

#attachment_short.csvを読み込む

#探索的因子分析
fa_res <- fa(attachment_short, nfactors = 2, rotate = "promax", fm = "ml")

#因子負荷量を確認
fa_res$loadings
write.csv(unclass(fa_res$loadings), "loadings.csv")

#推定した因子得点
fa_res$scores
write.csv(unclass(fa_res$scores), "loadings.csv")
