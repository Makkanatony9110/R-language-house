rm(list = ls())
?do.call
library(magrittr)
library(rvest)
#DOM Document Object Model
simple <- read_html('https://IshidaMotohiro.github.io/sample_check/simple.html')
simple

#node
simple %>% html_nodes('h1')
simple %>% html_nodes('h1') %>% html_text()
simple %>% html_nodes('a') %>% html_attrs()
simple %>% html_nodes('a') %>% html_attr('href')

#extract !!!!!!!!!
library(magrittr)
simple %>% html_nodes('a') %>% html_attr('href') %>%extract(2)

simple2 <- read_html('https://IshidaMotohiro.github.io/sample_check/simple2.html')
simple2
#class = . 
simple2 %>% html_nodes(css = '.green')
#id = #
simple2 %>% html_nodes(css = '#red')

simple2 %>% html_nodes('body #red') %>% html_text()
simple2 %>% html_nodes(xpath = '/html/body/p')
simple2 %>% html_nodes(xpath = '/html/body/div/p')
simple2 %>% html_nodes(xpath = '//p')
#階層が異なる要素全てを抽出するための記述方法
simple2 %>% html_nodes(xpath = '//p') %>% html_text()
simple2 %>% html_nodes(xpath = '//body/p') 
simple2 %>% html_nodes(xpath = '//body//p')
simple2 %>% html_nodes(xpath = '//body/p')
simple2 %>% html_nodes(xpath = '//body/*/p')

simple2 %>% html_nodes(xpath = 'body/p')
simple2 %>% html_nodes(xpath = 'body/*/p')
simple2 %>% html_nodes(css = 'body > p' )
simple2 %>% html_nodes(css = 'body p')

simple3 <- read_html('https://IshidaMotohiro.github.io/sample_check/attrs.html')
simple3 %>% html_nodes(xpath = '//a[@target]')
simple3 %>% html_nodes(css = 'a[target]')
simple3 %>% html_nodes(xpath = "//p[@class = 'green']")
simple3 %>% html_nodes(css = "p[class = 'green']")
simple3 %>% html_nodes(css = 'p.green')
simple3 %>% html_nodes(xpath = "//p[@id = 'red']")
simple3 %>% html_nodes(css = "p[id = 'red']")
simple3 %>% html_nodes(css = "p#red")

dat <- read_html('https://IshidaMotohiro.github.io/sample_check/xpath.html')
tbls <- dat %>% html_table()
tbls

tbls[[1]]
library(magrittr)
tbls %>% extract2(1)

# read up all the body parts
simple3 %>% html_nodes(xpath = '//body/descendant::p')
simple3 %>% html_nodes(css = 'body p')

#the case when some element contain perticular words 
simple3 %>% html_nodes(xpath = "//p[contains(text(),'使った例')]")
simple3 %>% html_nodes(css = "p:contains('使った例')")

simple3 %>% html_nodes(xpath = "//a[starts-with(@href,'https')]")
simple3 %>% html_nodes(css = "a[href^='https']")

x <- read_html("https://ishidamotohiro.github.io/sample_check/simple.html")
html_nodes(x,css = "body > a:nth-child(2)")

charToRaw("あ")
a <- "あ"
a <- iconv(a,to = "UTF-8")
charToRaw(a) 文字変換コード
a
a <- iconv(a,to = "CP932")
a
a1 <- "あ"
charToRaw(a1)

a2 <- iconv('あ',to = "CP932")
charToRaw(a2)
a1 == a2

library(rvest)
library(dplyr)

#対象とする文字コードの確認
x<-read_html("https://IshidaMotohiro.github.io/sample_check/simple3.html")
x %>% html_nodes(xpath = "//meta[@content | @charset]") 
#the case when the character code is not the UTF-8
y <- read_html("http://rmecab.jp/R/sjis.html",encoding = 'CP932') 
guess_encoding(y)

library(rvest)
authors <- read_xml("https://ishidamotohiro.github.io/sample_check/authors.xml")
authors %>% xml_nodes(xpath = "//lastname") 
authors %>% xml_nodes(xpath = "//lastname") %>% xml_text()

ns <- read_html("https://IshidaMotohiro.github.io/sample_check/namespace.xml")
ns %>% xml_nodes(xpath = "//lastname")
xml_ns(ns)

install.packages("jsonlite")
library(jsonlite)
member <- fromJSON("https://IshidaMotohiro.github.io/sample_check/authors.json")
member[[1]]
member$authors

library(httr)
res <- GET("https://IshidaMotohiro.github.io/sample_check./xpath.html")
res
# the exploitation of content
res %>% content()
res %>% headers() %>% head()

# XMl packages
install.packages('XML')
library(XML)

#the case when we treat XML (htmlParse)
res1 <- htmlParse("http://rmecab.jp/R/test.html")
res1 %>% class()
# the way to preserve the data as a frame of list (htmlTreeParse)
res2 <- htmlTreeParse("http://rmecab.jp/R/test.html")
res2 %>% class()

# the need to use xmlRoot code mainly in order to correct the dataframe
res1R <- xmlRoot(res1)
res2R <- xmlRoot(res2)

res1R %>% getNodeSet("//a") #要素全体の取得
res1R %>% xpathApply("//a",xmlValue) #要素の値のみ取得
res2R[[2]]['a', all = TRUE] %>% sapply(xmlValue)

#リンク先のURLを文字列として取得
res1 %>% getHTMLLinks()
res1 %>% xpathApply("//a",xmlAttrs)
res1 %>% xpathApply("//a",xmlGetAttr,'href')

res2R[[2]]["a",all = TRUE] %>% sapply(xmlGetAttr,'href')

# readHTMLTable, th function which can directly scrap the graph or figure from document in website
tab <- readHTMLTable("http://rmecab.jp/R/test.html")
str(tab)
tab[[2]]
class(tab[[2]])

library(magrittr)
tab %>% extract2(2)
res3 <- xmlParse("http://rmecab.jp/R/authors.xml")
res3 %>% xpathApply("//lastname",xmlValue)
xmlToDataFrame("http://rmecab.jp/R/authors.xml")

#the creation of dataframe of XMl document,,,,,,xmlTodataFrame 
library(RCurl)
res1 <- htmlParse(getURL('https://IshidaMotohiro.github.io/sample_check/xpath.html'))
res1 %>% class()

#文字列操作関数 grep
bun <- c('今年は2017年です','これは平成29年にあたります','干支は酉です。すなわちとりです.')
class(bun)
grep('1',bun)
grep('1',bun,value = TRUE)
grepl('1',bun)

gsub('.','。',bun)

#stringr
install.packages("stringr")
library(stringr)
str_replace_all(bun,".","。")

#文字列検索
str_detect(bun,"1")
#文字列を含む要素を取り出す
str_subset(bun,"1")

#正規表現
grep("7|9",bun,value = TRUE)
grep('[0-9]',bun,value = TRUE)
grep('[0-9][0-9][0-9][0-9]',bun,value = TRUE)

grep("。",bun,value = TRUE)
grep("。$",bun,value = TRUE)

str_replace_all(bun,"¥¥d{4}",'----')

#文字列の中からURLを取り出す
sentences <- "URL is http://rmecab.jp" 
str_extract(sentences,"https?://[^[:space:]]*")

#特殊文字
kawabata <- "トンネルを抜けると     そこは
雪国だった。"
# //s 空白文字を指定するメタ文字 スペースだけでなく、改行記号も該当する
str_replace_all(kawabata,'¥¥s','')

#HTTPリクエスト
req <- c(
  "GET /index.html HTTP/1.1",
  "Host: example.com",
  ""
)
#コネクションの作成
con <- socketConnection("example.com",port = 80)
writeLines(req,con)
#コネクションからHTTPレスポンスを読み取る
res <- readLines(con)
close(con)

cat(res,sep = '¥n')

library(httr)
#URLをひとまとまりの文字列として
GET("http://example.com/index.html")
#URLを要素に分けた文字列として
GET(url = "", scheme = "http",hostname = "example.com",path = "/index.html")

POST("http://example.com",add_headers('Content-Type' = "text/plain;charset = UTF-8"),
     body = enc2utf8("テスト"))

POST("http://httpbin.org/post",body = "test")
POST("http://httpbin.org/post",body = "test",content_type("text/plain"))

POST("http://httpbin.org/post",body = list(attachment = upload_file("/path/to/file")))

#JSON形式でHTTPリクエストを送る
POST("http://httpbin.org/post",body = list(keyword = 'test'), encode = 'json')
VERB("GET","http://httpbin.org/get",body = "test")

##HTTPレスポンスの構造
res <- GET("http://example.com")
is(res) #the representation of response object
status_code(res)

# ステータスコードが400以上かどうかチェックすればリクエストが成功か失敗かどうかを判定することができる
res <- GET("http://httpbin.org/status/500")
if (400 <= status_code(res)) {
  stop("リクエストが失敗しています。")
}#400以上の場合、リクエストが失敗している。

#ステータスコードによりリクエストが成功かどうかを自動でチェックしてくれる関数
message_for_status(res)
warn_for_status(res)
stop_for_status(res)

res <- GET("http://httpbin.org/get")
names(headers(res))
headers(res)$'content-type'
headers(res)$'Content-type'
# レスポンスボディ
res <- GET("http://httpbin.org/robots.txt") #ボディを読み取りして、リストやデータフレームに変換する関数 
content(res)

res <-GET("http://httpbin.org/user-agent")
headers(res)$"content-type"
content(res)

# JSONの配列をリストに変換するかベクトルにベクトルに変換するかの選択
library(jsonlite)
fromJSON('[1,2,3]',simplifyVector = FALSE) #リスト型
fromJSON('[1,2,3]',simplifyVector = TRUE) #ベクトル型

#レスポンスボディを一旦文字列として取り出してからパースする関数に渡す
body_raw <- content(res, as = "text")
body_raw

fromJSON(body_raw,simplifyVector = TRUE)
content(res,simplifyVector = TRUE)

#Basic認証/Digest認証 ユーザー名とパスワードを一定の形式に変換してAuthorizationヘッダに指定する方法
library(openssl)
#ユーザー名とパスワードを：で繋ぐ
base64_encode("user:passwd")

#デフォルトはBasic認証
GET("http://httpbin.org/basic-auth/user/passwd",
    authenticate("user","passwd"))
#他の認証方式はtype引数を指定
GET("http://httpbin.org/basic-auth/user/passwd",
    authenticate("user","passwd",type = "digest"))

#OAuth認証
#httpパッケージでのOAuth認証

#endpointの指定
github_endpoint <- oauth_endpoint(
  authorize = "https://github.com/login/oauth/authorize",
  access = "https://github.com/login/oauth/access_token"
)
#appの指定
github_app <- oauth_app(
  appname = "my_dummy_application",
  key = "abcdefgh",
  secret = "12345678"
)

github_token <- oauth2.0_token(github_endpoint,github_app)
GET("https://api.github.com/rate_limit",config = config(token = github_token))
