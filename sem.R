## 心理測定論
##  構造方程式モデリング
##  Naoto Yamashita

#多変量データ解析法の教科書と同じ出力を得るための設定

# SEMに必要なパッケージを読み込む
library(lavaan)
library(semPlot)

#モデルを定義する
model.text <- "
  #測定方程式
  soyo =~ gogaku + kiso + senmon + sotsuron
  tekio =~ shido + funiki + katsuyo + fuben
  syujuku =~ kougi + ensyu + syuron + hyotei
  manzoku =~ zyuzitu + tanoshisa + kokai + kibou
  #構造方程式
  syujuku ~ soyo + tekio
  manzoku ~ syujuku
  #潜在変数間の共分散
  soyo ~~ tekio
"
#SEMの実行
res <- sem(model.text, #モデル
           master_course, #データ
           std.lv=TRUE #従属変数になっている潜在変数の分散を1に固定するオプション
           )
#結果の要約，Extimateの列に非標準解，Std.allの列に標準解が表示される
summary(res, standardized = TRUE)
#適合度指標
fitMeasures(res)
#結果の図示
semPaths(res, "est")#非標準解
semPaths(res, "std")#標準解
