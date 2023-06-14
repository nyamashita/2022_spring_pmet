# 心理測定論
# 線形代数入門
# 2022/4/5
# Naoto Yamashita

#データ行列 X
X <- matrix(c(90, 70, 20,
              90, 40, 50,
              20, 90, 80,
              10, 60, 90), byrow = TRUE, 4, 3)

#行数
n <- nrow(X)

#中心化
J <- diag(1,n) - rep(1,4) %*% t(rep(1,4))/n
X_centered <- J %*% X
colMeans(X_centered) #各列の平均を計算

#分散と標準偏差と共分散
C <- t(X_centered) %*% X_centered / (n-1)
D <- diag(diag(C)^(-1/2))

#標準化
X_stdized <- J %*% X %*% D
colMeans(X_stdized)#各列の平均は0
cov(X_stdized)#分散共分散行列の対角要素＝分散は1
t(X_stdized) %*% X_stdized / (n-1)#相関係数行列

#線形代数を使わずに計算する場合
X_stdized2 <- X
for(col in 1:3){
  for(row in 1:4){
    X_stdized2[row, col] <- X[row,col] - mean(X[,col])/sqrt(var(X[,col]))
  }
}

#連立方程式
A <- matrix(c(5, -4,  6,
              7, -6, 10,
              4,  9,  7), byrow = TRUE, 3, 3)
b <- c(8, 14, 74)
solve(A) %*% A #逆行列
A %*% solve(A) #逆行列
solve(A)%*%b
