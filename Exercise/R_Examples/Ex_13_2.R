# Example 13-2 : table example

#20 個信用卡持有者的卡別、居住區域、以及性別資料：
cards = c(
"金卡"  , "白金卡", "普通卡", "普通卡", "普通卡", "金卡"   ,"普通卡", "普通卡",
"普通卡", "普通卡", "普通卡", "金卡"  , "普通卡", "白金卡" ,"金卡"  , "普通卡",
"普通卡", "普通卡", "白金卡", "普通卡"
)

area = c(
"南區", "北區", "南區", "南區", "北區", "南區", "南區", "南區", "南區", "南區",
"北區", "南區", "南區", "南區", "南區", "北區", "北區", "北區", "北區", "南區"
)

gender = c(
"女", "女", "女", "女", "女", "男", "女", "男", "男", "男", "男", "女", "女", "男",
"女", "女", "男", "男", "男", "男"
)

tabulate(factor(cards,levels=c("普通卡","金卡","白金卡")))

table(cards,dnn="會員分類")

members.table = table(cards,gender,dnn=c("會員分類","性別"))
members.table

margin.table(members.table,margin=1)

margin.table(members.table,margin=2)

rowSums(members.table)
colSums(members.table)

prop.table(members.table)		# 每一格的比例
prop.table(members.table,margin=1)	# 橫列邊際比例
prop.table(members.table,margin=2)	# 直行邊際比例

table(cards,gender,area)

rowSums(table(cards,gender,area))	# 所有橫列相加
rowSums(table(cards,gender,area),dims=2) # 視區域為橫列
colSums(table(cards,gender,area))	# 所有直行(cards)相加
colSums(colSums(table(cards,gender,area)))
rowSums(colSums(table(cards,gender,area)))
colSums(table(cards,gender,area),dims=2) # 將 area 當直行

prop.table(table(cards,gender,area))     # 每個格子所佔的比例
prop.table(table(cards,gender,area),margin=1)
prop.table(table(cards,gender,area),margin=2)
prop.table(table(cards,gender,area),margin=3)

