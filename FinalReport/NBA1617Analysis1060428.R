#讀取檔案
NBA1617Analysis = read.csv("NBA1617TeamPerGameStats.csv",header=T)
#列出聯盟16強檔案資訊
head(NBA1617Analysis,16)
#隨機抽出10%觀察值當作保留的測試樣本，剩下做為Tree模型所用的訓練樣本
np = ceiling(0.1*nrow(NBA1617Analysis))
np#3個測試樣本
test.index = sample(1:nrow(NBA1617Analysis),np)
#測試樣本
NBA1617Analysis.test=NBA1617Analysis[test.index,]
#訓練樣本
NBA1617Analysis.train=NBA1617Analysis[-test.index]
#install.packages("tree")
library(tree);library(CHAID)
#建立決策樹的模型
NBA1617Analysis.tree = tree(Team~. , data=NBA1617Analysis.train)
plot(NBA1617Analysis.tree);text(NBA1617Analysis.tree)
plot(NBA1617Analysis$Team,NBA1617Analysis$PTS)
# slm.model = lm()
