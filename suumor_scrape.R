library(rvest)
library(stringr)
library(tidyverse)

##
result <- list()


##
search_url_base <-
  "http://suumo.jp/jj/common/ichiran/JJ901FC004/?initFlg=1&seniFlg=1&pc=50&pj=1&po=0&ar=030&ra=030012&rnekTmp=050011570&newflg=0&km=1&rnek=050011570&ts=1&ts=2&ts=3"
page_num <- 109

search_url <- paste0(search_url_base, "&pn=", seq_len(page_num))

for (ii in seq_len(page_num)) {
  search_page <- read_html(search_url[ii])
  
  ## shosai and shuhen urls
  search_page %>%
    html_nodes("ul a") %>%
    html_text("href") %>%
    str_detect("詳細表示") -> shosai
  
  search_page %>%
    html_nodes("ul a") %>%
    html_attr("href") %>%
    .[shosai] -> shosai_url
  
  
  shuhen_url <- paste0(shosai_url, "kankyo/")
  
  ## read html sourse
  
  shosai_page <- map(shosai_url, read_html)
  shuhen_page <- map(shuhen_url, read_html)
  
  ## scrape
  map3(
    shosai_page,
    shuhen_page,
    shosai_url,
    ~ list(
      ## shosai
      ## info
      "info " =
        .x %>%
        html_nodes("div div h1") %>%
        html_text(),
      
      ## detail, yachin etc.
      "detail" =
        .x %>% 
        html_nodes("div .detailinfo") %>% 
        html_node("table") %>% 
        html_table(),
      
      ## minites to station
      "m_2_station" =
        .x %>% 
        html_nodes("div .detailnote-value-list") %>% 
        html_text(),
      ## gaiyo table
      "gaiyo" =
        .x %>%
        html_nodes(xpath = '//table[@class="data_table table_gaiyou"]') %>%
        html_table(),
      
      ## shuhen
      "lnglat" =
        .y %>%
        html_nodes("script") %>%
        purrr::map_chr(as.character) %>%
        str_subset("lng"),
      
      ## url
      "url" = .z
    )
  ) -> temp_result
  
  ## splice
  temp_result_nested <- list(temp_result)
  names(temp_result_nested) <- search_url[ii]
  result <- splice(result, temp_result_nested)
  
  message(round(100*ii/page_num))
}


###
at_depth(temp_result, 2, ~ length(.x) == 0) %>% 
  map(~ reduce(.x, all)) %>% any


