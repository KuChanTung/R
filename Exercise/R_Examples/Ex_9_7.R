# Example 9-7: points, lines, text

xp = c(160,165,170,175)    # 各附加點的 X 座標值
yp = c(80,90,80,90)         # 各附加點的 Y 座標值
plot(height,score)
points(xp,yp,col=2,pch=".")
points(xp,yp,col=2,pch=19) # 加上紅色的點，使用第 19 號符號
lines(xp,yp,col=3)           # 加上綠色的線段
# 加上藍色文字
text(xp,yp+10,labels=c("點1","點2","點3","點4"),col=4) 

