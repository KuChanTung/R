#讀取2008年球員數據：
nba  =read.csv("http://datasets.flowingdata.com/ppg2008.csv", sep=",")

#Step 2. Sort data
#按照球員得分，將球員從小到大排序：

nba <- nba[order(nba$PTS),]

#Step 3. Prepare data
#把行號換成行名（球球員名稱）：
row.names(nba) <- nba$Name
#去掉第一列行號：
nba <- nba[,2:20] # or nba <- nba[,-1]
#Step 4. Prepare data, again
#把 data frame 轉化為我們需要的矩陣格式：
nba_matrix <- data.matrix(nba)
#Step 5. Make a heatmap
# R 的默认还会在图的左边和上边绘制 dendrogram，使用Rowv=NA, Colv=NA去掉
heatmap(nba_matrix, Rowv=NA, Colv=NA, col=cm.colors(256), revC=FALSE, scale='column')
#这样就得到了上面的那张热图。
#Step 6. Color selection
#或者想把热图中的颜色换一下：
heatmap(nba_matrix, Rowv=NA, Colv=NA, col=heat.colors(256), revC=FALSE, scale="column", margins=c(5,10))