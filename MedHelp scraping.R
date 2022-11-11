# MedHelp scraping

## Required packages
library(RSelenium)
library(rvest)
library(tidyverse)
library(stringr)
library(magrittr)

## Gathering links for each page of the subcommunity of *Coronavirus*

### We create a loop to scrape the link for each thread/post from page 1 to 33 (as there were only 33 pages when we collected the data on October 16, 2021)
for(i in 1:33) {
  url <- getURL(paste0("https://www.medhelp.org/forums/Coronavirus/show/2203?page=", i))
  parser<-htmlParse(url)
  links <- xpathSApply(parser, "//a[@href]", xmlGetAttr, "href")
  pages <- as.data.frame(str_subset(links, "posts/Coronavirus"))
  medhelp_page <- rbind(medhelp_page, pages)
  pages <- NULL
}
names(medhelp_page)[[1]]<-"links"
medlinks <- medhelp_page$links
medhelp_page<-NULL


## Extracing posts (question and answers) from each page

### Based on the links collected in the last step, we create s loop to opened each link/page each time. 
remDr$open()
remDr$navigate("https://www.medhelp.org/posts/Coronavirus/Coronavirus-Variant/show/3066295")

for(k in 1:length(medlinks)){
  remDr$navigate(medlinks[k])
  
  title <- remDr$findElement(using = "class", value = "subj_title")
  more <- remDr$findElements(using = "css selector", value = ".trunc_comment_icon")
  if(length(more)!=0){
    for(l in 1:length(more)){
      more[[l]]$clickElement()
    }
    Sys.sleep(1)
  }
  ### Within each post, we create another loop and only scrape the question and all the answers and store them into a csv file.
  response <- remDr$findElements(using = "class", value = "resp_body")
  response_count <- remDr$findElement(using = "css", value = ".read_responses")$getElementText()
  response_num <- as.numeric(gsub(" Responses", "", response_count[[1]]))
  comment <- remDr$findElements(using = "class", value = "comment_body")
  comment_num <- as.numeric(length(remDr$findElements(using = "class", value = "comment_body")))
  subject <- remDr$findElement(using = "class", value = "subj_body")$getElementText()
  subject_text <- data.frame(subject[[1]])
  subject_text[[2]] <- paste0(title$getElementText())
  names(subject_text)[[1]] <- "posts"
  names(subject_text)[[2]] <- "question"
  
  if(comment_num!=0){
    for(i in 1:response_num){
      response_raw <- c(response_raw,
                        response[[i]]$getElementText())
      response_text <- data.frame(matrix(unlist(response_raw), byrow = T))
      response_text[[2]] <- paste0(title$getElementText())
      names(response_text)[[1]] <- "posts"
      names(response_text)[[2]] <- "question"
      all_responses <- rbind(all_responses, response_text)
      response_raw<-NULL
      response_text<-NULL
    }
    for(j in 1:comment_num){
      comment_raw <- c(comment_raw,
                       comment[[j]]$getElementText())
      comment_text <- data.frame(matrix(unlist(comment_raw), byrow = T))
      comment_text[[2]] <- paste0(title$getElementText())
      names(comment_text)[[1]] <- "posts"
      names(comment_text)[[2]] <- "question"
      all_comments <- rbind(all_comments, comment_text)
      comment_raw<-NULL
      comment_text<-NULL
    }
    
  } else {
    for(i in 1:response_num){
      response_raw <- c(response_raw,
                        response[[i]]$getElementText())
      response_text <- data.frame(matrix(unlist(response_raw), byrow = T))
      response_text[[2]] <- paste0(title$getElementText())
      names(response_text)[[1]] <- "posts"
      names(response_text)[[2]] <- "question"
      all_responses <- rbind(all_responses, response_text)
      response_raw<-NULL
      response_text<-NULL
    }
    
  }
  post_sum <- rbind(subject_text, all_responses, all_comments)
  all_post <- rbind(all_post, post_sum)
  all_responses<-NULL
  all_comments<-NULL
  subject_text <- NULL
  
}

all_post<-NULL
post_sum <- NULL
response_raw<-NULL
response_text<-NULL
all_responses<-NULL
all_comments<-NULL
comment_raw<-NULL
comment_text<-NULL
subject_text <- NULL

MedHelp_new <- all_post
write.csv(MedHelp_new, "Data/MedHelp_new.csv", row.names = F)
remDr$close()
remDr$closeServer()
