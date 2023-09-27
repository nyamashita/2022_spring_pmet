# R statistics
# 2021/4/28
# Naoto Yamashita

#事前にd02_itsudatsu.csvをdataという変数で読み込みましょう

#ヒストグラムと平均
hist(data[data$sex=="M", "syo6"])
hist(data[data$sex=="F", "syo6"])
mean(data[data$sex=="M", "syo6"])
mean(data[data$sex=="F", "syo6"])

#箱ひげ図
boxplot(data[data$sex=="M", "syo6"], data[data$sex=="F", "syo6"], names = c("M", "F"))

#t検定
t.test(data[data$sex=="M", "syo6"], #1個目のデータ
       data[data$sex=="F", "syo6"], #2個目のデータ
       var.equal = TRUE #おまじないだと思ってください
       )


#等分散性の検定
var.test(data[data$sex=="M", "syo6"], #1個目のデータ
         data[data$sex=="F", "syo6"], #2個目のデータ
         )

#Welchの検定
t.test(data[data$sex=="M", "syo6"], #1個目のデータ
       data[data$sex=="F", "syo6"], #2個目のデータ
       var.equal = FALSE #これでWelchの検定
)

#自動で選択
t.test(data[data$sex=="M", "syo6"], #1個目のデータ
       data[data$sex=="F", "syo6"] #2個目のデータ
)

#対応のあるt検定
t.test(data[data$sex=="M", "syo6"], #1個目のデータ
       data[data$sex=="F", "syo6"], #2個目のデータ
       paired = TRUE #このオプションで対応の有無を指定
)
