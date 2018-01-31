# Example 13-1: xtabs example

# 請修改資料檔路徑 
#
(  tw = read.table("d:/r/tw_marriage.txt",header=T)  )


xtabs( widowed ~ area  , data=tw)	# 喪偶者依照區域分類計算人數

xtabs( married ~ area + gender, data=tw) #已婚者依區域與性別分組
( tw.xtabs = xtabs( married ~ gender + area, data=tw)  )

margin.table(tw.xtabs,margin=1)				# 橫列邊際總和
margin.table(tw.xtabs,margin=2)				# 直行邊際總和

rowSums(xtabs( married ~ gender + area, data=tw))   # 橫列總和
colMeans(xtabs( married ~ gender + area, data=tw))
colSums(xtabs( married ~ gender + area, data=tw))   # 直行總和

tapply(tw$unmarried, list(tw$gender,tw$area), FUN = mean) 
tapply(tw$unmarried, list(tw$gender,tw$area),FUN=max)

aggregate(tw$unmarried,by=list(tw$gender,tw$area),FUN=mean)

round(prop.table(xtabs( married ~ gender + area,data=tw)),2)

round(prop.table(xtabs( married ~ gender + area, data=tw),
                     margin=1),2)	

round(prop.table(xtabs( married ~ gender + area, data=tw),
                     margin=2),2)

xtabs( married ~ city + gender , data=tw)

