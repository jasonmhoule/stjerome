require(tidyverse)
require(rvest)
# library(tidyverse)
# library(rvest)

full_bible <- readRDS("full_bible.rds")

full_bible

gen <- full_bible[1,]
# https://bible.usccb.org/bible/genesis/0

gen$intro[[1]] %>% 
  read_html() %>% 
  html_text() %>% 
  write_lines("gen_intro.txt")

gen$intro[[1]] %>% 
  read_html() %>% 
  html_nodes("a")

gen$content[[1]]$ch_content[[1]] %>% 
  read_html() %>% 
  html_nodes("a")

# Excise bottom annotations and link references
# Pull intro + all narrative into one long thread, to break out by sections
# Stitch back together the other references