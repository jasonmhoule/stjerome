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
  
  if(ch == 0) {
    
    ch_content <- book$intro[[1]]
    
  } else {
    
    ch_content <- book$content[[1]]$ch_content[[ch]]
    
  }

  verses <- ch_content %>% 
    read_html() %>% 
    html_nodes(".verse, .wv, .nv, .bksect, .chsect, table")
  
  if(length(verses) == 0) {
    return(NULL)
  }
  
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
  
  verse_html <- verses %>% 
    map(as.character) %>% 
    unlist()

  verse_node_txt <- verses
  
  for (i in 1:length(verse_node_txt)) {
    xml_remove(xml_node(verse_node_txt[[i]], "a"))
    xml_remove(xml_node(verse_node_txt[[i]], ".bcv"))
  }
  
  verse_text <- verse_node_txt %>% 
    map(html_text) %>% 
    as.character()
  
  tibble(bk = bk, ch = ch, verse_nums, verse_html, verse_text, verse_links, verse_annotations, ids, verse_classes, verse_b)
  
}

# Pull intro + all narrative into one long thread, to break out by sections

assemble_rough_book <- function(bk) {
  
  chs <- nrow(full_bible[bk,]$content[[1]])
  
  buch <- map_dfr(0:chs, build_ch_tbl, bk = bk)
  
  buch %>% 
    filter(!grepl("-",ids)) %>% 
    filter(!grepl("fn|en",verse_classes)) %>% 
    filter(trimws(verse_text) != "")
  
}

links_to_tw <- function(book) {
  
  book <- book %>% 
    mutate(verse_html_tw = gsub('<a class=\"fnref\" href=\"(.*?)\".*?</sup></a>',"<sup>[[*|\\1]]</sup>",verse_html))
  
  verse_node_txt <- book$verse_html_tw %>% 
    paste(collapse = "") %>% 
    read_html() %>% 
    html_nodes(".verse, .wv, .nv, .bksect, .chsect, table")
  
  for (i in 1:length(verse_node_txt)) {
    xml_remove(xml_node(verse_node_txt[[i]], "a"))
    xml_remove(xml_node(verse_node_txt[[i]], ".bcv"))
  }
  
  verse_text <- verse_node_txt %>% 
    map(html_text) %>% 
    as.character()
  
  book$verse_text_tw <- verse_text
  
  return(book)
}

