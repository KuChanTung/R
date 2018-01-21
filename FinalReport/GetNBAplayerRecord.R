#抓NBA球員資料

library(XML)
library(RCurl)

url = "http://www.basketball-reference.com/players/"
get_url = getURL(url, encoding = "UTF-8")
get_url_parse = htmlParse(get_url, encoding = "UTF-8")
url_player_initial = xpathSApply(get_url_parse, "//tr/td[1]/a", function(x) c(xmlAttrs(x)[["href"]]))
url_modify = "http://www.basketball-reference.com"
url_player_all = sapply(url_player_initial, function(x) paste(url_modify,x,sep=""))

data_player_all = sapply(url_player_all, function(x) readHTMLTable(x))





data_player_all_full = as.data.frame(data_player_all[1])
for(i in 2:length(data_player_all)){
  tmp = as.data.frame(data_player_all[i])
  names(data_player_all_full) = names(tmp)
  data_player_all_full = rbind(data_player_all_full, tmp)
}

names(data_player_all_full) = c("player","from","to","position","height","weight","birthdate","college")
data_player_all_full$from = as.numeric(as.character(data_player_all_full$from))
data_player_all_full$to = as.numeric(as.character(data_player_all_full$to))
data_player_all_full$weight = as.numeric(as.character(data_player_all_full$weight))
data_player_all_full$height = sapply(data_player_all_full$height, FUN=function(x) sub("-",".",x))
data_player_all_full$height = as.numeric(as.character(data_player_all_full$height))