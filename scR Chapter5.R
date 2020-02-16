rm(list = ls())
library(httr)
appId <- "dd065fa516cd8e64919b93e1b04c75833673625b"
res <- GET(
  url = "http://api.e-stat.go.jp/rest/2.1/app/json/getStatsData",
  query = list(
    appId = appId,
    statsDataId = "0003104180" 
  )
)
result <- content(res)
result #結果の取得

str(result,max.level = 4,list.len = 4) #リストの様子のチェック
statistical_data <- result$GET_STATS_DATA$STATISTICAL_DATA
str(statistical_data$DATA_INF$VALUE[[1]])
str(statistical_data$CLASS_INF$CLASS_OBJ[[2]])

do.call(rbind,statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)
#同じ形のデータフレームに統一するために、対象がネストしたリストかどうかを調べ、ネストされたリストの場合だけ、
#do.callを適用するような処理を行う
do.cal
force_rbind <- function(x){
  x_are_list <-as.logical(lapply(x,is.list))
  if(all(x_are_list)){
    do.call(rbind,x)
  }else{
    rbind(x)
  }
}
force_rbind(statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)
force_rbind(statistical_data$CLASS_INF$CLASS_OBJ[[1]]$CLASS)
#dplyrパッケージでうまくリストでもネストしたリストでも扱うことができる
library(dplyr)
bind_rows(statistical_data$CLASS_INF$CLASS_OBJ[[2]]$CLASS)
bind_rows(statistical_data$CLASS_INF$CLASS_OBJ[[1]]$CLASS)

#時計データの取り出し
data_df <- bind_rows(statistical_data$DATA_INF$VALUE)
data_df

#メタ情報の取り出し、メタ情報は少し複雑な構造をしているため、リストのまま取り出し
meta_info <- statistical_data$CLASS_INF$CLASS_OBJ %>%
  lapply(function(i){
    i$CLASS <- bind_rows(i$CLASS)
    i
  })
meta_info[[1]]
meta_info[[2]]
#メタ情報を統計データに紐づける際には、各カテゴリのメタ情報を統計データの列名と同じ名前で取り出す

mids <- sapply(meta_info,function(x) x$'@id')
mids
?paste0
names(meta_info) <- paste0("@",mids)

meta_info_cat01 <- meta_info[["@cat01"]]
meta_info_cat01
#コードの羅列とCLASS要素の対応情報を結びつける
cat01_factor <- factor(data_df[["@cat01"]],
                       levels = meta_info_cat01$CLASS$'@code',
                       labels = meta_info_cat01$CLASS$'@name')
head(cat01_factor)

data_df[,"@cat01"] <- cat01_factor

meta_info_wo_cat01 <- meta_info[names(meta_info) != "@cat01"]
for(colname in names(meta_info_wo_cat01)){
  m <- meta_info[[colname]]
  data_df[,colname] <- factor(data_df[[colname]],
                              levels = m$CLASS$'@code',
                              labels = m$CLASS$'@name')
}

#メタ情報の＠nameに置き換える
mids <- names(meta_info)
mnames <- sapply(meta_info,function(x) x$'@name') 
mnames

names(data_df)[match(mids,names(data_df))] <-mnames
data_df
#それぞれのカテゴリにどのような値が含まれているかどうかのチェック levels
levels(data_df$`時間軸（月）`)
levels(data_df$男女別)
levels(data_df$人口)
levels(data_df$年齢5歳階級)
levels(data_df$全国)

data_H26_Oct <- data_df %>%
  filter(
    `時間軸（月）` == "平成26年10月",
    男女別      == "男女計",
    人口        == "総人口",
    grepl(pattern = "^¥¥d+", x = 年齢5歳階級)
  ) 
data_H26_Oct

library(magrittr)
library(httr)
library(dplyr)
#reserve the API end point in obeject
base.url <- "http://earthquake.usgs.gov/fdsnws/event/1/"
#scraping the API information by the GET method
req <- paste0(base.url,'version') %>% GET()
req %>% status_code()
#the returning back the API version 
req %>% content(encoding = "UTF-8")

params <- list(format = "csv",
               starttime = "2020-01-01",
               endtime = "2020-01-31")
params
#specifying the foramat to obtain data and period getting them by using parameter
req <- paste0(base.url,"query") %>% GET(query = params)

res <- req %>% content(encoding = 'UTF-8')

res %>% glimpse() #take a look at the size of dataframe and name of variables, and also the contents
res %>% count(type,sort = TRUE)#assign the descending order by introducing the code of sort

nrow(res)
# deleting the variables expept for the earthquake
res %<>% filter(type == "earthquake") 
nrow(res)
# the selecting the some variable we want to use in analysis 
res %<>% select(id,time,lat = latitude,lon = longitude,depth,mag,place)

# detecting the depth and mag value of dataframe by summarize()
res %>%
  summarize_at(vars(depth,mag),c("min","max","mean"),na.rm = TRUE)
#putting to use the geojson on the API request
res <- paste0(base.url,"query") %>%
  GET(query = list(format = "geojson",
                   starttime = "2020-01-01",
                   endtime = "2020-01-31",
                   limit = "100")) %>%
  content(as = "text",encoding = "UTF-8")
res %>% substr(1,100)
# tramsforming the geojson into the object involved with geometrial space
library(rgdal)
df.sp <- readOGR(res,"OGRGeoJSON",stringsAsFactors = FALSE) # reading the SpatialPointsdataFrame
isS4(df.sp) #confirmation if it has the S4 formation of data?
slotNames(df.sp)
df.sp@data %>% glimpse()

library(ggplot2)
library(maps)

world <- map_data("world") #ggplot2:map_data
worldmap <- ggplot(world,aes(x = long,y = lat,group = group)) +
  geom_path() + 
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

# pointing out
worldmap +
  geom_polygon(fill = "lightgreen",colour = "grey")+
  geom_point(data = as.data.frame(df.sp),
             aes(x = coords.x1,y = coords.x2,
                 group = magType,
                 shape = magType,
                 colour = "red",
                 size = (mag / 2)))

# requesting for API number
library(httr)
res_no_auth <- GET(url = "https://api.github.com/",
                   path = "rate_limit")
content_no_auth <- content(res_no_auth)
str(content_no_auth$resources)
as.POSIXct(content_no_auth$resources$core$reset,origin = "1970-01-01",tz = "Asia/Tokyo")

#requesting API
res_authorized <- GET("https://api.github.com/",
                      path = "rate_limit",
                      add_headers('Authorization' = 
                                    "token cc630e35d82a27da8355895c8c83e63cfbeacb01"))
content_authorized <- content(res_authorized)
str(content_authorized$resources)

base.url <- "https://api.github.com/"
token <- "cc630e35d82a27da8355895c8c83e63cfbeacb01"

#Obtaining Events API
library(magrittr)
res <- GET(base.url,
           path = "/users/hadley/events",
           query = list(access_token = token)) %>%
  content()
#the recognizing the number of event, which turns to be 30s
length(res)
# Obtaining the name of elements, after extracting only one data 
res %>% extract2(1) %>%
  str(max.level = 1) %>% names()

#Obtaining the values from the deep floor like this, which is the list contaning the another list in row.
library(purrr)
res %>% map_chr("type")#recognizing the type of event

#all the events are the public attributes.
res %>% map_lgl("public")
res %>% map("actor") %>%
  extract2(1)
# the number of objects to obetain are set to be 100, Repository API
res <- GET(base.url,
           path = "/users/hadley/repos",
           query = list(access_token = token,per_page = 100)
           ) %>%
  content()
#recognizing the name of elements
res %>% extract2(1) %>% names()
# according to the name of elements, allowing the bunch of objects to be dataframe for using more usefully and conveniently.
df.repo <- res %>% map_df(~ .[c("name","created_at","size","stargazers_count")])

library(dplyr)
df.repo %>% glimpse()
# transforming the value obtained by the code created_at into the date or time form from chr
# extracting the things that is above 80 on the number of stargazers evaluation number
df.repo %<>% mutate(created_at = as.POSIXct(created_at))  %>%
  filter(stargazers_count >= 80)
?geom_text_repel
library(ggplot2)
library(ggrepel)
# sort the name of repositories based on the order of evaluation number
sort.name <- with(df.repo,reorder(name,stargazers_count,median))
df.repo %>% ggplot(aes(sort.name,stargazers_count)) +
  geom_point(aes(size = size)) +
  geom_text_repel(aes(label = sort.name)) +
  theme(axis.text.x = element_blank()) +
  xlab("repository")

#標準の高階関数
l <- list(x = 1:3,y = 5:7,z = c(TRUE,FALSE,NA))
lapply(l,FUN = mean)
Map(mean,l)

#Purrrを利用したもの
library(purrr)
l %>% map(mean)
l %>% map_dbl(mean)
l %>% map_chr(mean)
l %>% map_dbl(mean,na.rm = TRUE)

#通常の関数のようには利用できない
l %>% map_dbl(mean(na.rm = TRUE)) #Error

#リストに含まれる要素の文字数を数えるという簡単な処理。
name.holder <- list(list(Last = c("Motohiro","Daisuke","HIroki","Shiinya"),
                         First= c("Ishida","Ichikawa","YUtani","Uryu")))
name.holder %>% map(nchar,type = "width") 
#要素名を参照し、特定の要素について実行する
name.holder %>% map("Last") %>%
  map(nchar,type = "width")

name.holder %>% flatten() %>%
  map(nchar,type = "width")
#リストの要素を参照し、データフレームとして値を格納する。
name.holder %>% 
  map_df(~ .[c("First","Last")])

q()
