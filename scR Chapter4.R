rm(list = ls())
library(rvest)
library(magrittr)
x <- read_html("https://www.r-bloggers.com")
x %>% html_nodes(css = "body") %>% #body要素を指定
  html_children() #子要素を取得

#子コンテンツ部分から子要素を取り出す
x %>% html_nodes(css = "body > div#mainwrapper") %>%
  html_children()
x.nodes <- x %>% html_nodes(css = "body > div#mainwrapper > div#leftcontent > div > div")
x.nodes[3] %>% html_structure() #HTMLドキュメントの構造表示

x.nodes[3] %>% html_structure()
articles <- x.nodes %>% html_nodes("h2") %>% html_text()
links <- x.nodes %>% html_nodes("h2 > a") %>% html_attr(name = "href")
  
length(articles)
articles[1:3]
links[1]
articles[20]
links[20]
x <- read_html(links[20])
x %>% html_nodes("div.entry") %>% html_children()

# trim & substrにより、本文中の余計な改行や空白を取り除き、かつ本文の一部だけを出力している。
x %>% html_nodes("div.entry") %>% html_text(trim = TRUE) %>% substr(1,500)

#ログインが必要なページのウェブスクレイピング
session <- html_session("https://qiita.com/login?redirect_to=%2F")#ログイン実行
is.session(session)
# HTML要素の情報を取得するための函数html_form()
forms <- html_form(session)
forms %>% extract2(1)
input.values <- forms %>%
  extract2(1) %>% 
  set_values(identity = "avant.garde.sk.jrc717@gmail.com", password = "skjrc8717")
#ログインする
(login.session <- submit_form(session,input.values))

login.session %>% html_nodes(css = "body > div >div >div >div > div > div ") %>%
  html_children()

login.session %>% html_nodes(css = "div.tl-UserInfo_statsItem") %>%
  html_children()

login.session %>% 
  html_nodes("div.tl-UserInfo_statsItem > div > div") %>%
  html_text()

login.session %>%
  jump_to("notifications") %>%
  html_nodes(xpath = '//[@id = "main"]/div/div/div/ul/li/a/div[2]') %>%
  html_text()

q()
