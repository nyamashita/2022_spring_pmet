## 心理測定論
##  主成分分析
##  Naoto Yamashita

library(psych)

#自作の主成分分析関数
svd_pca <- function(X, r){
  X <- as.matrix(X)
  svd_res <- svd(X)
  n <- nrow(X)
  
  #loadings
  A <- svd_res$v[,1:r]%*%diag(svd_res$d[1:r])/sqrt(n)
  #score
  F <- sqrt(n)*svd_res$u[,1:r]
  #weight
  W <- sqrt(n)*svd_res$v[,1:r]%*%diag(svd_res$d[1:r]^(-1))
  
  list(
    A=A,
    F=F,
    W=W,
    vexp=n*sum(diag(A%*%t(A)))/sum(diag(t(X)%*%X))
  )
}

#事前にpilot.csvを読み込む
head(pilot)
pairs.panels(pilot)

#標準化したデータ
pilot_scale <- scale(pilot)
#中心化したデータ
pilot_center <- scale(pilot, scale = FALSE)

#個体数
n <- nrow(pilot)

#主成分分析
res_pca_scale <- svd_pca(pilot_scale, 2) #標準解
res_pca_center <- svd_pca(pilot_center, 2) #非標準解

#主成分負荷量
res_pca_scale$A

#主成分得点
res_pca_scale$F

#重み行列で主成分得点を計算する
pilot_scale%*%res_pca_scale$W #標準解
pilot_center%*%res_pca_center$W #非標準解

#重み行列と主成分負荷量の関係
res_pca_scale$W / res_pca_scale$A #標準解：各列が定数倍になっている
1/eigen(t(pilot_scale)%*%pilot_scale/n)$values[1:2] #共分散行列の固有値の逆数
res_pca_center$W / res_pca_center$A #非標準解：各列が定数倍になっている
1/eigen(t(pilot_center)%*%pilot_center/n)$values[1:2] #共分散行列の固有値の逆数

#主成分負荷量はもとの変数と主成分の相関係数行列
cor(pilot_scale, res_pca_scale$F) #標準解のときに成り立つ．主成分負荷量と同じ．
cor(pilot_center, res_pca_center$F) #非標準解のときには成り立たない！

#SPSSの結果を再現する
## 相関行列に基づく出力=標準解
res_pca_scale$A * sqrt(n)/sqrt(n-1) #「成分行列」は主成分負荷量を微調整したもの
res_pca_scale$F * sqrt(n-1)/sqrt(n) #「主成分得点」は主成分得点を微調整したもの
#重み行列は出力されない
## 分散共分散行列に基づく出力=非標準解
res_pca_center$A * sqrt(n)/sqrt(n-1) #「成分行列（元データ）」は主成分負荷量を微調整したもの
res_pca_center$F * sqrt(n-1)/sqrt(n) #「主成分得点」は主成分得点を微調整したもの
#重み行列は出力されない

#Rのprcomp関数の結果を再現する
res_prcomp<- prcomp(pilot, scale.=TRUE)#関数中で自動的に標準化される
res_prcomp$rotation #主成分負荷量
res_pca_scale$A %*% diag(svd(pilot_scale)$d[1:2]^(-1)) * sqrt(n) #主成分負荷量を特異値で定数倍したもの
res_prcomp$x #主成分得点
res_pca_scale$F %*% diag(svd(pilot_scale)$d[1:2]) /sqrt(n) #主成分負荷量を特異値で定数倍したもの
#重み行列は出力されない

#バイプロット
biplot(res_prcomp, cex=1.5)

#分散説明率と主成分数
var_data <- diag(cov(pilot)) #元データの分散
var_scores <- diag(cov(res_pca$x)) #主成分得点の分散
vexp <- var_scores/sum(var_data) #分散説明率を計算
cum_vexp <- cumsum(vexp) #累積分散説明率を計算
plot(vexp,xlab="components", ylab="explained variance", type="o") #分散説明率のグラフ
barplot(cum_vexp, ylab="cumulative explained variance") #累積分散説明の棒グラフ
