# R statistics
# 2021/5/19
# Naoto Yamashita


# 05_envy.csvをenvy_dataとしてロードしましょう

#簡単に視覚化 -> 外れ値はなさそう
plot(envy_data$status, envy_data$envy)
plot(envy_data$attitude, envy_data$envy)

#2要因分散分析；その1
aov_res_1 <- aov(envy ~ attitude + status, data = envy_data)
anova(aov_res_1)
##多重比較
TukeyHSD(aov_res_1)
plot(TukeyHSD(aov_res_1))#面白いプロット

#2要因分散分析をやるときに便利なプロット
interaction.plot(envy_data$attitude, envy_data$status, envy_data$envy)

#2要因分散分析；その2　交互作用入り
aov_res_2 <- aov(envy ~ attitude*status, data = envy_data)
aov_res_2_1 <- aov(envy ~ attitude + status + attitude:status, data = envy_data)
anova(aov_res_2)

#単純主効果の検定
aov_res_friendly <- aov(envy ~ status,
                        data = envy_data[envy_data$attitude=="friendly",])
anova(aov_res_friendly)

aov_res_hostile <- aov(envy ~ status,
                       data = envy_data[envy_data$attitude=="hostile",])
anova(aov_res_hostile)

aov_res_neutral <- aov(envy ~ status,
                       data = envy_data[envy_data$attitude=="neutral",])
anova(aov_res_neutral)

##有意差が見られた主効果に対して多重比較
TukeyHSD(aov_res_hostile)


#反復測定分散分析
aov_res_rep <- aov(envy ~ status + Error(sbj_rep/status), data = envy_data)
summary(aov_res_rep)#summary関数を使うことに注意
