library(rvest)
library(ggplot2)
library(tidyr)

vlaverturl <- "https://www.vlaamsparlement.be/vlaamse-volksvertegenwoordigers"
vlaverthtml <- read_html(vlaverturl)

##Get the names, parties and the urls of the profile pages
vlavert <- vlaverthtml %>% html_nodes(".field--name-volledige-naam") %>% html_text()
vlapart <- vlaverthtml %>% html_nodes(".field--name-huidigefractie") %>% html_text()
vlaverturls <- vlaverthtml %>% html_nodes("span a") %>% html_attr("href")

rawdata <- data.frame()
index <- 0

##Go to the profile pages of all the MP's and collect the data
base_verturl <- "https://www.vlaamsparlement.be"

for(verturlid in vlaverturls){
  print(index)
  index <- index + 1
  
  verturl <- paste(base_verturl,verturlid,sep="")
  verthtml <- read_html(verturl)
  
  ##Initiatieven
  vertiniturl <- verthtml %>% html_node(".field--name-recent-documents-link .field__items .field__item a") %>% html_attr("href")
  vertiniturl <- sub("publicatiedatum[van][date]=all","publicatiedatum[van][date]=current_legislature", vertiniturl, fixed = TRUE)
  initiatieven <- read_html(paste(base_verturl, vertiniturl, sep="")) %>% html_node("h1.page-title") %>% html_text()
  
  ##Vragen
  vertvragenurl <- verthtml %>% html_node(".field--name-recent-questions-link .field__items .field__item a") %>% html_attr("href")
  vertvragenurl <- sub("publicatiedatum[van][date]=all","publicatiedatum[van][date]=current_legislature", vertvragenurl, fixed = TRUE)
  vragen <- read_html(paste(base_verturl, vertvragenurl, sep="")) %>% html_node("h1.page-title") %>% html_text()
  
  ##Tussenkomsten
  verttussenkurl <- verthtml %>% html_node(".field--name-recent-interventions-link .field__items .field__item a") %>% html_attr("href")
  verttussenkurl <- sub("publicatiedatum[van][date]=all","publicatiedatum[van][date]=current_legislature", verttussenkurl, fixed = TRUE)
  tussenkomsten <- read_html(paste(base_verturl, verttussenkurl, sep="")) %>% html_node("h1.page-title") %>% html_text()
  
  vertdata <- data.frame()
  vertdata <- data.frame(vlavert[index], vlapart[index], initiatieven, vragen, tussenkomsten, vlaverturls[index])
  
  rawdata <- rbind(rawdata, vertdata)
}

colnames(rawdata) <- c("naam", "partij", "initiatieven", "vragen", "tussenkomsten", "url")

finaldata <- rawdata
##Remove text we don't need
finaldata$initiatieven <- sub("Ongeveer ", "", finaldata$initiatieven, fixed=TRUE)
finaldata$initiatieven <- sub(" zoekresultaten in de huidige zittingsperiode", "", finaldata$initiatieven, fixed=TRUE)
finaldata$vragen <- sub("Ongeveer ", "", finaldata$vragen, fixed=TRUE)
finaldata$vragen <- sub(" zoekresultaten in de huidige zittingsperiode", "", finaldata$vragen, fixed=TRUE)
finaldata$tussenkomsten <- sub("Ongeveer ", "", finaldata$tussenkomsten, fixed=TRUE)
finaldata$tussenkomsten <- sub(" zoekresultaten in de huidige zittingsperiode", "", finaldata$tussenkomsten, fixed=TRUE)

##Convert to numbers
finaldata$initiatieven <- as.integer(finaldata$initiatieven)
finaldata$vragen <- as.integer(finaldata$vragen)
finaldata$tussenkomsten <- as.integer(finaldata$tussenkomsten)

##Format names and ad questions and initiatives
finaldata <- finaldata %>% separate(naam, c("voornaam", "achternaam"), " ", extra = "merge")
finaldata$initiaal <- paste(substr(finaldata$voornaam, 1, 1), ".", sep="")
finaldata$initnaam <- paste(finaldata$initiaal, finaldata$achternaam, sep=" ")
finaldata$vrageninitiatieven <- finaldata$vragen + finaldata$initiatieven

median.tussenkomsten <- median(finaldata$tussenkomsten)
median.vrageninitiatieven <- median(finaldata$vrageninitiatieven)

finaldata <- select(finaldata, voornaam, achternaam, initiaal, initnaam, partij, initiatieven, vragen, tussenkomsten, vrageninitiatieven, profiel, url)

write.csv(finaldata, file="finaldata_30-09.csv", row.names = FALSE)

scatter <- ggplot(finaldata, aes(x = tussenkomsten, y = vrageninitiatieven, col = partij)) + geom_point( alpha = 0.7, size = 3) + theme_minimal() + geom_text(aes(label = initnaam), nudge_y = 10) + scale_colour_manual(values = c("#83de62","#ffac12", "#003d6d", "#f5822a", "#e23a3f", "#5a5101", "#000000", "#cccccc")) + labs(x = "Aantal tussenkomsten", y = "Aantal vragen en initiatieven") + theme(legend.position="none") + geom_hline(aes(yintercept=median.vrageninitiatieven)) + geom_vline(aes(xintercept=median.tussenkomsten)) 
##+ scale_x_continuous(limit = c(0, 300)) + scale_y_continuous(limit = c(0, 700))

scattergrid <- ggplot(finaldata, aes(x = tussenkomsten, y = vrageninitiatieven, col = partij)) + geom_point( alpha = 0.2, size = 3) + theme_minimal() + scale_colour_manual(values = c("#83de62","#ffac12", "#003d6d", "#f5822a", "#e23a3f", "#ffe500", "#000000", "#cccccc")) + labs(x = "Aantal tussenkomsten", y = "Aantal vragen en initiatieven") + theme(legend.position="none") + geom_hline(aes(yintercept=median.vrageninitiatieven)) + geom_vline(aes(xintercept=median.tussenkomsten)) + facet_grid(. ~ partij) + theme(panel.background = element_rect(fill = '#fef7ea', colour = '#fef7ea'), plot.background = element_rect(fill = '#fef7ea', colour = '#fef7ea'))