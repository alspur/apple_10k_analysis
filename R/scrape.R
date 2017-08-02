# scrape.R

# laod ----------

# load packages
library(tidyverse)
library(tidytext)
library(rvest)
library(stringr)
library(lubridate)

# scrape page 1 of apple 10k's ---------------
apple <- read_html("http://investor.apple.com/sec.cfm?DocType=Annual&ndq_keyword=")

# parse html links to 10k's
apple_links <- apple %>% 
  html_nodes("a.button.button-compact[href^='secfiling.cfm']") %>% 
  xml_attr("href") %>% 
  unique() %>% 
  str_c("http://investor.apple.com/", .)

# parse 10k information
apple_info <- apple %>% 
  html_nodes(".column.large-2.small-12") %>%
  html_text() %>%
  str_trim()

# parse date of 10k
apple_dates <- apple_info[seq(4,22,2)]

# parse type of 10k
apple_type <- apple_info[seq(3,21,2)]

# get 10k text
apple_text <- apple_links %>%
  map_chr(~ read_html(.) %>% 
  html_node("efx_business") %>%
  html_text())

# combine info into df
apple_df <- tibble(date = mdy(apple_dates),
                   type = apple_type, 
                   text = apple_text)

# scrape page 2 of apple 10k's ---------------
apple2 <- read_html("http://investor.apple.com/sec.cfm?DocType=Annual&DocTypeExclude=&SortOrder=FilingDate%20Descending&Year=&PageNum=2&FormatFilter=&CIK=&NumberPerPage=10")


# parse html links to 10k's
apple_links2 <- apple2 %>% 
  html_nodes("a.button.button-compact[href^='secfiling.cfm']") %>% 
  xml_attr("href") %>% 
  unique() %>% 
  str_c("http://investor.apple.com/", .)

# parse 10k information
apple_info2 <- apple2 %>% 
  html_nodes(".column.large-2.small-12") %>%
  html_text() %>%
  str_trim()

# parse date of 10k
apple_dates2 <- apple_info2[seq(4,22,2)]

# parse type of 10k
apple_type2 <- apple_info2[seq(3,21,2)]

# get 10k text
apple_text2 <- apple_links2 %>%
  map_chr(~ read_html(.) %>% 
            html_node("efx_business") %>%
            html_text())

# combine info into df
apple_df2 <- tibble(date = mdy(apple_dates2),
                   type = apple_type2, 
                   text = apple_text2)



# scrape page 3 of apple 10k's ---------------
apple3 <- read_html("http://investor.apple.com/sec.cfm?DocType=Annual&DocTypeExclude=&SortOrder=FilingDate%20Descending&Year=&PageNum=3&FormatFilter=&CIK=&NumberPerPage=10")


# parse html links to 10k's
apple_links3 <- apple3 %>% 
  html_nodes("a.button.button-compact[href^='secfiling.cfm']") %>% 
  xml_attr("href") %>% 
  unique() %>% 
  str_c("http://investor.apple.com/", .)

# parse 10k information
apple_info3 <- apple3 %>% 
  html_nodes(".column.large-2.small-12") %>%
  html_text() %>%
  str_trim()

# parse date of 10k
apple_dates3 <- apple_info3[seq(4,12,2)]

# parse type of 10k
apple_type3 <- apple_info3[seq(3,11,2)]

# get 10k text
apple_text3 <- apple_links3 %>%
  map_chr(~ read_html(.) %>% 
            html_node("efx_business") %>%
            html_text())

# combine info into df
apple_df3 <- tibble(date = mdy(apple_dates3),
                    type = apple_type3, 
                    text = apple_text3)

# tidy the text -----------

apple_raw_10k <- bind_rows(apple_df, apple_df2, apple_df3) %>%
  filter(!is.na(text)) %>%
  select(-type)

apple_tidy_10k <- apple_raw_10k %>%
  unnest_tokens(word, text) %>%
  add_count(date) %>%
  rename(doc_words = n) 

# save data ----------------

saveRDS(apple_tidy_10k, "data/apple_tidy_10k.rda")
