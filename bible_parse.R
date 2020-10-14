require(tidyverse)
require(rvest)

full_bible <- readRDS("full_bible.rds")
full_bible

# https://bible.usccb.org/bible/genesis/1

# full_bible[1,]$intro[[1]] %>% 
#   read_html() %>% 
#   html_text() %>% 
#   write_lines("gen_intro.txt")

# full_bible[1,]$content[[1]]$ch_content[[1]] %>% 
#   read_html() %>% 
#   html_text() %>% 
#   write_lines("gen_ch1.txt")

build_ch_tbl <- function(bk, ch) {
  
  book <- full_bible[bk,]
  
  if(bk == 0) {
    
    ch_content <- book$intro[[1]]
    
  } else {
    
    ch_content <- book$content[[1]]$ch_content[[ch]]
    
  }

  verses <- ch_content %>% 
    read_html() %>% 
    html_nodes(".verse, .wv, .nv, .bksect, .chsect, table")
  
  verse_classes <- verses %>% 
    map(html_attr, "class")
  
  verse_callbacks <- verses %>% 
    map(html_nodes, "a")
  
  verse_b <- verses %>% 
    map(html_nodes, "b") %>% 
    map(html_text) %>% 
    as.character()
  
  verse_nums <- verses %>% 
    map(html_nodes, ".bcv") %>% 
    map(html_text) %>% 
    as.character()
  
  verse_links <- verses %>% 
    map(html_nodes, ".enref") %>% 
    map(html_attr, "href") %>% 
    as.character()
  
  verse_annotations <- verses %>% 
    map(html_nodes, ".fnref") %>% 
    map(html_attr, "href") %>% 
    as.character()
  
  ids <- verses %>% 
    map(html_attr, "id") %>% 
    as.character()
  
  verse_node_txt <- verses
  
  for (i in 1:length(verse_node_txt)) {
    xml_remove(xml_node(verse_node_txt[[i]], "a"))
    xml_remove(xml_node(verse_node_txt[[i]], ".bcv"))
  }
  
  verse_text <- verse_node_txt %>% 
    map(html_text) %>% 
    as.character()
  
  tibble(bk = bk, ch = ch, verse_nums, verse_text, verse_links, verse_annotations, ids, verse_classes, verse_b)
  
}

bk <- 73
ch <- 5
build_ch_tbl(bk, ch) %>% View()

# Still need to handle annotations and links

annotations <- ch_content %>% 
  read_html() %>% 
  html_nodes("td, .fn, .fncon") %>% 
  html_text()

links <- ch_content %>% 
  read_html() %>% 
  html_nodes(".en")

# Pull intro + all narrative into one long thread, to break out by sections

# Stitch back together the other references
