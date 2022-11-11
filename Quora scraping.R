# Quora scraping

## Required packages
library(RSelenium)
library(rvest)
library(tidyverse)
library(stringr)
library(magrittr)


## Gathering links for each page of the topic of *COVID-19 (2019-2020)*

### We open the homepage of Quora and log in with our account and password.
remDr$open()
remDr$navigate("https://www.quora.com/topic/COVID-19/")
links <- NULL
element <- remDr$findElement(using = "class", value = "puppeteer_test_question_title")
flag <- TRUE
counter <- 0
n <- 5

### Since Quora is a more dynamic website compared to MedHelp, we let scraper automatically scroll down and render the new content. After experimentation, we decide compare the pagesource every n(n=5) time, since sometimes one scroll down doesn't render new content
while(flag){
  for (z in 1:length(remDr$findElements(using = "class", 
                                        value = "puppeteer_test_question_title"))) {
    ### Here we get the links for each question
    remDr$findElements(using = "class", value = "puppeteer_test_question_title")[[z]] ->clickthis
    clickthis$clickElement()
    ### Then we extract hyperlinks
    links <- c(links, 
               remDr$getPageSource() %>%
                 magrittr::extract2(1) %>%
                 read_html() %>%
                 html_nodes('a') %>%
                 html_attr("href")
    )
  }
  counter <- counter + 1
  
  for(i in 1:n){
    element$sendKeysToElement(list("key"="end"))
    Sys.sleep(2)
  }
  if(exists("pagesource")){
    if(pagesource == remDr$getPageSource()[[1]]){
      flag <- FALSE
      writeLines(paste0("Scrolled down ",n*counter," times.\n"))
    } else {
      pagesource <- remDr$getPageSource()[[1]]
    }
  } else {
    pagesource <- remDr$getPageSource()[[1]]
  }
  
}



## Filtering out not related links

### Unlike the scraper on MedHelp, the scraper on Quora would extract a lot of unrelated hyperlinks on the page. So we need to filter those out based on the their urls.
df <- data.frame(matrix(unlist(links), byrow=TRUE))
names(df)[1] <- "links"
alllinks <- df %>% distinct(links, .keep_all = T)
alllinks <- alllinks %>%
  filter(!str_detect(links, paste(sprintf("\\b%s\\b", "https://www.quora.com/following"), 
                                  collapse = '|')))
alllinks <- alllinks %>%
  filter(!str_detect(links, paste(sprintf("\\b%s\\b", "https://www.quora.com/answer"), 
                                  collapse = '|')))
alllinks <- alllinks %>%
  filter(!str_detect(links, paste(sprintf("\\b%s\\b", "https://www.quora.com/notifications"), 
                                  collapse = '|')))
alllinks <- alllinks %>%
  filter(!str_detect(links, paste(sprintf("\\b%s", "https://www.quora.com/spaces"), 
                                  collapse = '|')))
alllinks <- alllinks %>%
  filter(!str_detect(links, paste(sprintf("\\b%s", "https://www.quora.com/topic/"), 
                                  collapse = '|')))
alllinks <- alllinks %>%
  filter(!str_detect(links, paste(sprintf("\\b%s", "https://www.quora.com/profile/"), 
                                  collapse = '|')))
alllinks <- alllinks %>% filter(!str_detect(links, paste(sprintf("\\b%s\\b", "/answer/"), 
                                                         collapse = '|')))
alllinks <- alllinks %>% filter(!str_detect(links, paste(sprintf("\\b%s\\b", "/answers/"), 
                                                         collapse = '|')))
alllinks <- distinct(alllinks, links)
alllinks <- alllinks$links


## Extracing posts (question and answers) from each page

### Similarly, based on the links collected in the last step, we create s loop to opened each link/page each time and make the scraper automatically scroll down and render new content. 
options(width=80)
for(i in 1:length(alllinks)){
  remDr$navigate(alllinks[i])
  ### Again within each post, we create another loop and only scrape the question and all the answers and store them into a csv file. The scraper also needs to figure out whether there is any "more" (to expand the answers) on that page. If we do not distinguish these two situations, there could be an error messages. Also, unlike the static webpage like MedHelp, we set an artificial number (50) which is large enough to let the scraper capture all the new content after scrolling down.
  if(length(remDr$findElements(using = "css", value = ".qu-fontFamily--sans"))!=0){
    more <- remDr$findElements(using = "css", value = ".qu-fontFamily--sans")
    for(k in 1:length(more)){
      more[[k]]$executeScript("arguments[0].scrollIntoView(false);", args = list(more[[k]]))
      more[[k]]$executeScript('window.scrollBy(0,300)')
      more[[k]]$clickElement()
    }
    Sys.sleep(1)
    
    for(l in 1:50){
      last_height = remDr$executeScript("return document.body.scrollHeight")[[1]]
      remDr$executeScript('window.scrollTo(0,1000000)')
      Sys.sleep(1)    
      new_height = remDr$executeScript("return document.body.scrollHeight")[[1]]
      if(last_height == new_height){
        print(paste("Done"))
        break
      }
    }
    
    for(j in 1:length(remDr$findElements(using = "class",
                                         value = "puppeteer_test_answer_content"))){
      answer_raw <- c(answer_raw,
                      remDr$findElements(using = "class", 
                                         value = "puppeteer_test_answer_content")[[j]]$getElementText())
      answer_text <- data.frame(matrix(unlist(answer_raw), byrow=TRUE))
      answer_text[[2]] <- paste0(remDr$findElement(using = "class", 
                                                   value = "puppeteer_test_question_title")$getElementText())
      names(answer_text)[[1]] <- "answers"
      names(answer_text)[[2]] <- "question"
    }
    
  } else {
    for(l in 1:50){
      last_height = remDr$executeScript("return document.body.scrollHeight")[[1]]
      remDr$executeScript('window.scrollTo(0,1000000)')
      Sys.sleep(1)    
      new_height = remDr$executeScript("return document.body.scrollHeight")[[1]]
      if(last_height == new_height){
        print(paste("Done"))
        break
      }
    }
    
    for(j in 1:length(remDr$findElements(using = "class", 
                                         value = "puppeteer_test_answer_content"))){
      answer_raw <- c(answer_raw,
                      remDr$findElements(using = "class", 
                                         value = "puppeteer_test_answer_content")[[j]]$getElementText())
      answer_text <- data.frame(matrix(unlist(answer_raw), byrow=TRUE))
      answer_text[[2]] <- paste0(remDr$findElement(using = "class", 
                                                   value = "puppeteer_test_question_title")$getElementText())
      names(answer_text)[[1]] <- "answers"
      names(answer_text)[[2]] <- "question"
    }
  }
  all_answers <- rbind(all_answers, answer_text)
  answer_raw <- NULL
  answer_text<-NULL
  Sys.sleep(2)
}


write.csv(all_answers, "Data/Quora new.csv", row.names = F)
remDr$close()
remDr$closeServer()
