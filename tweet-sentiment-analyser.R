install.packages(c("devtools", "rjson", "bit64", "httr", "plyr", "ggplot2", "doBy", "XML"))

library(devtools)

install_github("geoffjentry/twitteR")
install_github('R-package','quandl')

library(plyr)
library(doBy)
library(Quandl)
library(twitteR)

api_key <- "vCMhVOsJeyL0ReBeMtPzAVMtG"
api_secret <- "7atxb4Unr9SgzeT0w6m3JQAHWXotToBoBcjVnAc1xFmj7AkJnE"
access_token <- "1577934554-wmzI5GbiaRjKsCS4p3LIhlRukRyb8fmEQoFsC6s"
access_token_secret <- "gkgrD0j9JVMB88SvnI9NgPEdvqHZUrxB8r4UuO2r1xVxy"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

hu.liu.pos = scan('/Users/joanhealy/Downloads/opinion-lexicon-English/positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('/Users/joanhealy/Downloads/opinion-lexicon-English/negative-words.txt', what='character', comment.char=';')

pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting', 'epicfail')

score.sentence <- function(sentence, pos.words, neg.words) {
  sentence = gsub('[[:punct:]]', '', sentence)
  sentence = gsub('[[:cntrl::]]', '', sentence)
  sentence = gsub('\\d+', '', sentence)
  sentence = tolower(sentence)
  
  word.list = str_split(sentence, '\\s+')  #split at white space
  words = unlist(word.list)   
  
  pos.matches = match(words, pos.words)
  neg.matches = match(words, neg.words)
  
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  score = sum(pos.matches) - sum(neg.matches)
  
  return(score)
}

score.sentiment <- function(sentences, pos.words, neg.words) {
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {	#laply For each element of a list, apply function then combine results into an array
    tryCatch(score.sentence(sentence, pos.words, neg.words ), error=function(e) 0)
  }, pos.words, neg.words)
  
  #now we construct a data frame - a table
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
}

collect.and.score <- function (handle, code, airline, pos.words, neg.words) {
  
  tweets = searchTwitter(handle, n=1500)
  text = laply(tweets, function(t) t$getText()) #extract the text content of the all the tweets
  
  score = score.sentiment(text, pos.words, neg.words)
  score$airline = airline
  score$code = code
  
  return (score)  
}

delta.scores = collect.and.score("@delta", "DL", "delta", pos.words, neg.words)
united.scores = collect.and.score("@united", "UN", "united", pos.words, neg.words)
jetblue.scores = collect.and.score("@JetBlue", "JB", "JetBlue", pos.words, neg.words)
us.scores = collect.and.score("@USAirways", "US", "USAirways", pos.words, neg.words)
southwest.scores = collect.and.score("@SouthwestAir", "SW", "southwest airlines", pos.words, neg.words)
american.scores = collect.and.score("@AmericanAir", "AA", "american airlines", pos.words, neg.words)

all.scores = rbind(american.scores, delta.scores, jetblue.scores, southwest.scores, united.scores, us.scores)

all.scores$very.pos = as.numeric( all.scores$score >= 2)
all.scores$very.neg = as.numeric( all.scores$score <= -2)

twitter.df = ddply(all.scores, c('airline', 'code'), summarise, pos.count = sum (very.pos), neg.count = sum(very.neg))

twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count

twitter.df$score = round (100 * twitter.df$pos.count / twitter.df$all.count)

orderBy(~-score, twitter.df)

install.packages("XML")
library(XML)

acsi.url = 'http://www.theacsi.org/index.php?option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'

acsi.df = readHTMLTable(acsi.url, header=T, which=1, stringAsFactors=F)

acsi.df = acsi.df[, c(1,22)]

head(acsi.df, 10)

colnames(acsi.df) = c('airline', 'score')

acsi.df$code = c('JB', 'SW', 'DL', NA, NA, 'AA', 'US', 'UN', NA, NA)

compare.df = merge(twitter.df, acsi.df, by='code', suffixes=c('.twitter', '.acsi'))

print(compare.df)

Quandl.auth("MD5w72ZwJAf1n5YV-ZFu")

stocks.aal = Quandl("YAHOO/AAL", collapse="annual", start_date="2014-01-01", type="ts")
stocks.dal = Quandl("YAHOO/DAL", collapse="quarterly", start_date="2014-01-01", type="ts")
stocks.jaw = Quandl("YAHOO/F_JAW", collapse="quarterly",  start_date="2014-01-01", type="ts")

compareALL.df = merge(compare.df, stocks.aal,  by='code')

print(stocks.aal)

install.packages("RMySQL")
library(RMySQL)

m <- dbDriver("MySQL", max.con = 25)

con <- dbConnect(m, host='localhost', user="root", password = "root", dbname="sentimentTweets", unix.socket='/Applications/MAMP/tmp/mysql/mysql.sock')

rs <- dbSendQuery(con, "select * from news")
df <- fetch(rs, n = -1) # -1 means fetch all
print(df)

dbWriteTable(con, "RtestTable2", compare.df, row.names = F, append=TRUE) 