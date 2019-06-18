library(rvest)
library(xml2)
library(tidyverse)
library(magrittr)
library(scales)
library(knitr)
library(lubridate)
library(V8)



scrape <- function(x){
  page <- x
  js <- page %>%  html_nodes('li') %>% html_nodes('script') %>% html_text()
  ct <- v8()
  content <- read_html(ct$eval(gsub('document.write','', js))) %>% 
    html_text()
  return(content)
}

scrape('https://joautok.hu')
