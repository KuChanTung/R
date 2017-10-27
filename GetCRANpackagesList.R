library(XML)
library(RCurl)

urlp="http://cran.r-project.org/web/views/"
urlp
urlpall="http://cran.r-project.org/web/packages/available_packages_by_name.html"

doc <- htmlParse(getURL(urlp,encoding='utf-8'))
doc
tbls <-readHTMLTable(doc,header = FALSE,StringaAsFactors=FALSE)

str(tbls)
tbls
class(tbls)

PackagesClass <- as.data.frame(tbls)
PackagesClass
PackagesClass$NULL.V2

docall <- htmlParse(getURL(urlpall,encoding='UTF-8'))
docall
tblsall <-readHTMLTable(docall,StringsAsFactors=FALSE)
tblsall

str(tblsall)
class(tblsall)

tblsall[[1]]
head(tblsall[[1]][1])
head(tblsall[[1]][2])

PackagesClassAll <- as.data.frame(tblsall)
PackagesClassAll