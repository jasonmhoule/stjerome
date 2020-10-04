require(tidyverse, xml2, rvest)
library(tidyverse)
library(xml2)
library(rvest)

book <- read_html("https://bible.usccb.org/bible") %>% 
  html_nodes("#block-usccb-readings-content .content a") %>% 
  html_text()

link <- read_html("https://bible.usccb.org/bible") %>% 
  html_nodes("#block-usccb-readings-content .content a") %>% 
  html_attr("href")

mn_tbl <- tibble(book, link)

##

scrape_ch_links <- function(book_scr, link_scr) {
  
  print(book_scr)
  
  newurl <- paste0("https://bible.usccb.org",link_scr)
  
  rd_html <- read_html(newurl)
  
  intro <- rd_html %>% 
    html_nodes("#scribeI") %>% 
    toString()
  
  ch_links <- rd_html %>% 
    html_nodes(".field-content a")
  
  chapter <- ch_links %>% 
    html_text()
  
  link <- ch_links %>% 
    html_attr("href")
  
  # booklist <- list(intro, chapter, link)
  booklist <- tibble(intro = list(intro), content = list(tibble(chapter, link)))
  
  tibble(book = book_scr, booklist = list(booklist))
  
}

ch_tbl <- map2_dfr(mn_tbl$book, mn_tbl$link, scrape_ch_links) %>% 
  unnest(booklist)

full_ch_tbl <- mn_tbl %>% 
  left_join(ch_tbl, by = "book") %>% 
  filter(book != "Preface")

saveRDS(full_ch_tbl, "full_ch_tbl.rds")
# full_ch_tbl <- readRDS("full_ch_tbl.rds")

scrape_ch_html <- function(chapter_scr, link_scr) {
  
  newurl <- paste0("https://bible.usccb.org",link_scr)
  
  print(chapter_scr)
  
  rd_html <- read_html(newurl)
  
  html_body <- rd_html %>% 
    html_nodes("#scribeI") %>% 
    toString()
  
  tibble(chapter = chapter_scr, ch_content = list(html_body))
  
}

scrape_bk_html <- function(book_scr, content_scr) {
  
  print(book_scr)
  
  if(nrow(content_scr) == 0) {
    return(content = tibble(chapter = NA_character_, link = NA_character_, ch_content = list(NULL)))
  }
  
  oo <- map2_dfr(content_scr$chapter, content_scr$link, scrape_ch_html)
  
  left_join(content_scr, oo, by = "chapter")
  
}

oo2 <- map2(full_ch_tbl$book, full_ch_tbl$content, scrape_bk_html)

full_bible <- full_ch_tbl %>% 
  select(-content) %>% 
  tibble(content = oo2)

saveRDS(full_bible, "full_bible.rds")
# full_bible <- readRDS("full_bible.rds")
