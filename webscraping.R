library(stringi)
library(rvest)
library(httr)

#Strona gumtree
gumtreeurl = 'https://www.gumtree.pl/s-mieszkania-i-domy-sprzedam-i-kupie/krakow/v1c9073l3200208p1'
gumtreesource <- GET(gumtreeurl)
gumtreehtml <- read_html(gumtreesource)

gumtreehtml %>%
  html_nodes('.href-link')%>%
  html_text() -> title

# data preprocesing
title<-gsub("\n","",title)
title<-gsub("#","",title)
title<-gsub("^\\s+|\\s+$",'',title)  #usuwamy spacje na końcu i na początku

head(title,4)

length((title))

#pobieramy opisy z usunięciem znaczników tab i konica linii
gumtreehtml %>%
  html_nodes('div.tileV1')%>%
  html_nodes('div.description')%>%
  html_text() %>%
  gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')%>%
  gsub(x=.,pattern="^\\s+|\\s+$",replacement='')-> description_first

head(description_first,2) 

# pobieramy ceny
gumtreehtml %>%
  html_nodes('.ad-price')%>%
  html_text() -> amount
# preprocesing amount
amount<-gsub(" zł","",amount)
amount<-gsub("[[:space:]]","",amount)   # wszystkie typy spacji !
amount <- as.numeric(amount)
head(amount)

length(amount)

gumtreehtml %>%
  html_nodes('.price-text')%>%
  html_text()%>%
  gsub("zł",'' , .) %>%
  gsub("[[:space:]]","", .)%>%
  gsub("\\n","", .)%>%
  as.numeric( .)-> price

head(price)

length(price)

# ściągamy datę utworzenia
gumtreehtml %>%
  html_nodes('.creation-date')%>%
  html_text() %>%
  gsub('[[:space:]]','', .)-> creation.date

head(creation.date)

length(creation.date)

# mamy dane zestawiamy w ramkę danych
n<-length(price)-length(amount)
amount<-c(amount,rep(NA,n))

dane <-data.frame(title=title,description=description_first,amount=amount,price=price)
head(dane,2)

# obrazy ze strony

gumtreehtml %>%
  html_nodes("img") -> link.titles
# preprocesing data
head(link.titles,3)

tail(link.titles,3)

# zapis na dysku
for(i in 2:24){
  img.url <- link.titles[i] %>% html_attr("data-src")
  download.file(img.url , paste0("k",i,".png"), mode = "wb")
}

url.link <- read_html('https://www.gumtree.pl/a-mieszkania-i-domy-sprzedam-i-kupie/krakow/penthouse-4+pok-0-prowizji-+-gorka-narodowa/1006214436020911356673609')
url.link%>%
  html_nodes('.selMenu .name')%>%
  html_text() %>%
  gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')->name
url.link%>%
  html_nodes('.selMenu .value')%>%
  html_text() %>%
  gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')->value 

dane.szczegolowe<-data.frame(name=name,value=value)
View(dane.szczegolowe)

url.link <- read_html('https://www.gumtree.pl/a-mieszkania-i-domy-sprzedam-i-kupie/krakow/penthouse-4+pok-0-prowizji-+-gorka-narodowa/1006214436020911356673609')
# pobranie pełnych opisów
url.link%>%
  html_nodes('.pre')%>%
  html_text() %>%
  gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')->full_descr

# wejście na podstrony 
gumtreehtml %>%
  html_nodes('.href-link')%>%
  html_attr('href') ->  links

head(links,3)

# ustalam listę do której dopisuję data framy
wynik <-list()
opis<-c("")
n<-length(links)
#tworzę petlę pobierającą dane
for(i in 1:n){
  url.link <- paste0('https://www.gumtree.pl',links[i])
  url.link1 <- read_html(url.link)
  url.link1%>%
    html_nodes('.selMenu .name')%>%
    html_text() %>%
    gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')->name
  url.link1%>%
    html_nodes('.selMenu .value')%>%
    html_text() %>%
    gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')->value  
  
  dane.wynik<-data.frame(name=name,value=value)
  wynik[[i]] <- dane.wynik
  
  url.link1%>%
    html_nodes('.pre')%>%
    html_text() %>%
    gsub(x = ., pattern = '[\a\r\t\n]', replacement = '')->full_descr
  opis<-c(opis,full_descr)
}
