rm(list = ls())
library(rvest)
library(dplyr)
library(magrittr)
library(stringr)

x <- read_html("https://en.wikipedia.org/wiki/Shikoku_Pilgrimage")
#表からデータを読み取る
tab <- x %>% html_table(header = TRUE)#表のヘッダーを含めて抽出する header()
class(tab)
NROW(tab)#要素数の確認 表の要素が4つあることが分かる
pilgr <- tab %>% extract2(2) #表内のデータフレームの抽出
pilgr %>% glimpse()#glimpse データの各変数の中身とそのデータ型を確認することができる
#位置情報データの加工
pilgr$Coordinates[1] 

library(magrittr)
library(stringr)
coord <- pilgr %>%
  use_series(Coordinates) %>%
  str_extract_all(pattern = "[1-9][1-9]¥¥.[0-9]+|[1-9][1-9][1-9]¥¥.[0-9]+",
                  simplify = TRUE) %>%
  extract(,1:2) %>%
  as_data_frame() %>%
  mutate_if(is.character(),as.numeric) %>%
  set_colnames(c('lat','lon'))

head(pilgr$Coordinates)
length(pilgr$Coordinates)
head(coord)
library(rvest)
#アメリカ合衆国州人口密度の塗り分け図
x <- read_html("https://ja.wikipedia.org/wiki/アメリカ合衆国の州")
tabs <- x %>% html_table(header = TRUE, fill = TRUE)
NROW(tabs)
states <- tabs %>% extract2(1)
glimpse(states) #データを一見確認 dplyr

#dplyr中の関数を用いて、データを処理。必要な列のみを選択し、変数名を変更
states %<>% select(region = 州名,pop.dens = 人口密度) %>%
  mutate(region = gsub("[ア-ン].+州","",region)) #関数名を変更、特に州名の日本語部分をmutate及びgsub関数で削除

head(states)
?tolower
#データの地図上に反映させる
library(ggplot2)
library(viridis)
map.states <- map_data("state") #マップデータ化
ggplot() + 
  geom_polygon(data = map.states, aes(x = long, y = lat, group = group)) + 
  geom_map(data = states, map = map.states,
           aes(fill = pop.dens, map_id = tolower(region))) + 
  scale_fill_viridis(alpha = 0.8)

dev.off()
traceback()

#ウェブAPIサーピスを用いたデータ抽出
app_id_facebook <- "794003277734108"
app_secret_facebook <- "a2344135450dec22bc1636a5c5bc68e6"

library(httr)
#endpoint
facebook <- oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth",
                           access = "https://graph.facebook.com/oauth/access_token")
#information of application
myapp <- oauth_app(appname = 'facebook',key = app_id_facebook,secret = app_secret_facebook)

scope <- "public_profile"
token_facebook <- oauth2.0_token(endpoint = facebook,
                                 app = myapp,
                                 scope = scope,
                                 type = "application/x-www-form-urlencoded", # 取得したい情報に応じてスコープを指定する
                                 #大体このWebAPIを求める場合が多い
                                 cache = FALSE
                                 )

#自身のアカウントの友達の数を取得
url <- "https://graph.facebook.com/v2.7/me/friends?fields=app_id_facebook,Keita Saita"
res <- GET(url, config = token_facebook)
res_parsed <- content(res,as = "parsed")
res_parsed$summary$total_count

# Twitter API and tweets tweeted recently
install.packages("twitteR")
library(twitteR)
setup_twitter_oauth(consumer_key = ,
                    consumer_secret = )
searchTwitter('#rstats',n = 50)
