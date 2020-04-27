#set up
cbind.fill <- function(...){
  nm <- list(...) 
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow)) 
  do.call(cbind, lapply(nm, function (x) 
    rbind(x, matrix(, n-nrow(x), ncol(x))))) 
}
dt<-data.frame(matrix(nrow=0,ncol=10))
pagelink <- "http://www.imdb.com/search/title?at=0&sort=boxoffice_gross_us&start="

#grab movie links
for (i in 0:240){
page=as.numeric(i*50+1)
url<- paste0(pagelink,"",page)
html <- htmlTreeParse(url,useInternalNodes=T)
title<- data.frame(xpathSApply(html,"//*[@class='title']/a",xmlValue))
movieurl<- data.frame(xpathSApply(html,"//*[@class='title']/a",xmlAttrs))
year<- data.frame(xpathSApply(html,"//*[@class='year_type']",xmlValue))
rating<- data.frame(xpathSApply(html,"//*[@class='rating-rating']/span[@class='value']",xmlValue))
description<- data.frame(xpathSApply(html,"//*[@class='outline']",xmlValue))
people<- data.frame(xpathSApply(html,"//*[@class='credit']",xmlValue))
genre<- data.frame(xpathSApply(html,"//*[@class='genre']",xmlValue))
sales<- data.frame(xpathSApply(html,"//*[@class='sort_col']",xmlValue))
certificateA<- t(data.frame(xpathSApply(html,"//*[@class='certificate']/span",xmlAttrs)))
certificate <- certificateA[,1]
runtime<- data.frame(xpathSApply(html,"//*[@class='runtime']",xmlValue))
movies<- cbind.fill(title,movieurl,year,rating,description,people,genre,sales,certificate,runtime)
dt<-rbind(dt,movies)
print(paste0( "working on the page ",i))
}


#assign the movieurl to real movie url and change column name
dt$movieurl<-paste0("http://www.imdb.com",dt[,2])
colnames(dt) <- c("title","movieid","year","rating","description","people","genre","sales","certificate","runtime","movieurl")

#save the movieurl in a seperate file
write.xlsx(dt, "dt.xlsx")


###################################
########################################

#use the movieurl to get to movie detailed information on movie page
dt <- read.table("~/Documents/R-data/Movie/dt.txt", header=TRUE, quote="\"")
urldt<-data.frame(dt[1:11849,11])
View(urldt)
count <- as.numeric(nrow(urldt))
dt2<-data.frame(matrix(nrow=0,ncol=34))

for (i in 1:2) {
  url<-urldt[i,]
  html <- htmlTreeParse(url,useInternalNodes=T)
  genre<- c(xpathSApply(html,"//*[@class='infobar']/a[1]",xmlValue))
  date<- c(xpathSApply(html,"//*[@class='infobar']/span/a",xmlValue))
  rating<- c(xpathSApply(html,"//span[@itemprop='ratingValue']",xmlValue))
  ratingCount<- c(xpathSApply(html,"//span[@itemprop='ratingCount']",xmlValue))
  MetaScoreCount<- c(xpathSApply(html,"//*[@href='criticreviews?ref_=tt_ov_rt']",xmlValue))
  reviewCount<- c(xpathSApply(html,"//*[@href='reviews?ref_=tt_ov_rt']/span[@itemprop='reviewCount']",xmlValue))
  criticCount<- c(xpathSApply(html,"//*[@href='externalreviews?ref_=tt_ov_rt']/span[@itemprop='reviewCount']",xmlValue))
  description<- c(xpathSApply(html,"//p[@itemprop='description']",xmlValue))
  ############starts&director
  director<- c(xpathSApply(html,"//div[@itemprop='director']/a[@itemprop='url']",xmlValue))
  directorURL<- c(xpathSApply(html,"//div[@itemprop='director']/a[@itemprop='url']",xmlGetAttr,"href"))
  writer<- c(xpathSApply(html,"//div[@itemprop='creator']/a[@itemprop='url']",xmlValue))
  writerURL<- c(xpathSApply(html,"//div[@itemprop='creator']/a[@itemprop='url']",xmlGetAttr,"href"))
  stars<- c(xpathSApply(html,"//div[@itemprop='actors']/a[@itemprop='url']",xmlValue))
  starsURL<- c(xpathSApply(html,"//div[@itemprop='actors']/a[@itemprop='url']",xmlGetAttr,"href"))
#############otherinfo
  amazonprice <-c(xpathSApply(html,"//*[@class='info table-cell']/p",xmlValue))
  oscars<- c(xpathSApply(html,"//*[@itemprop='awards']/b",xmlValue))
  otherAwards<- c(xpathSApply(html,"//*[@itemprop='awards']",xmlValue))[2]
############
  castlist<- c(xpathSApply(html,"//td[@itemprop='actor']/a/span[@itemprop='name']",xmlValue))
  characterlist<- c(xpathSApply(html,"//td[@class='character']/div/a",xmlValue))
##########
  storyline<- c(xpathSApply(html,"//div[@id='titleStoryLine']/div[@class='inline canwrap']/p",xmlValue))
  plotmainkeywords<- c(xpathSApply(html,"//span[@itemprop='keywords']",xmlValue))
  fullgenre<- c(xpathSApply(html,"//div[@itemprop='genre']/a",xmlValue))
###details
 country<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[2]
 language<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[3]
  filminglocation<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[6]
  AKA<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[5]
 fulltime<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[12]
 Soundmix<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[13]
  openweeksales<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[8]
  growsssales<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[9]
  budget<- c(xpathSApply(html,"//div[@id='titleDetails']/div[@class='txt-block']",xmlValue))[7]
  company<- c(xpathSApply(html,"//span[@itemprop='creator']",xmlValue))
  companyURL<- c(xpathSApply(html,"//span[@itemprop='creator']/a",xmlGetAttr,"href"))

detail<- cbind.fill(url,genre,fullgenre,date,rating,ratingCount,MetaScoreCount,reviewCount,criticCount,description,director,directorURL,writer,writerURL,stars,starsURL,amazonprice,oscars,
otherAwards,castlist,characterlist,storyline,plotmainkeywords,country,language,filminglocation,AKA,fulltime,Soundmix,openweeksales,growsssales,budget,company,companyURL)

dt2<-rbind(dt2,detail)
print(paste0( "working on the page ",i)) }

write.xlsx(dt2, "dt2.xlsx")


######################################
########################################


plotkeyword <- http://www.imdb.com/title/tt0201485/keywords?ref_=tt_stry_kw
reviewpage <- http://www.imdb.com/title/tt0201485/reviews?count=71&start=0
//*[@id="overview-top"]/div[2]/meta
//*[@id="overview-top"]/div[2]/meta[itemprop='contentRating']
