# 心理測定論
# Rのデモ
# 2022/4/5
# Naoto Yamashita

#電卓として使う
3 + 5
10 - 3
2 * 3
20 / 4
sqrt(2)
2^2
date()

#変数を使う
a <- 2
b <- 3
c = 4 #イコールでもいい
a + b + c #合計
mean(a, b ,c) #平均
A + b + c #大文字と小文字は区別される

#csvデータを読み込む
hawks <- read.csv("")#パスを設定
d01_hawks #データを表示
head(d01_hawks, 5) #先頭5行を表示
?head() #head関数のヘルプを表示

#統計っぽいことをする
mean(d01_hawks$height) #heightの平均
var(d01_hawks$height) #heightの分散
summary(d01_hawks) #各列の色々な統計量を出す

#グラフを書く
plot(d01_hawks)

#データを抜き出す
hawks[1,] #一行目を抜き出す
hawks[,2] #二行目を抜き出す
hawks[hawks$salary>1000,] #salaryが1000より大きい行を抜き出す