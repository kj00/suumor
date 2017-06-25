library(magrittr)
library(stringr)
library(tidyverse)

#
result <- read_rds("result.rds")


map2(result, names(result), function(result, name) {
  tibble(
    info = result %>% map_chr( ~ .x$info),
    detail = result %>% map( ~ .x$detail[[1]]),
    gaiyo = result %>% map( ~ .x$gaiyo[[1]]),
    m_2_station = result %>% map( ~ .x$m_2_station),
    lnglat = result %>% map_chr( ~ .x$lnglat %>%
                                   replace(., length(.) == 0, NA)),
    search_url = name
  )
}) %>% reduce(bind_rows) -> df


## bukken, gositsu
df$info %>%
  str_replace("の賃貸.+", "") %>%
  {
    if_else(
      str_detect(., "\\("),
      str_extract(., "^.+\\(") %>%
        str_replace("\\(", ""),
      as.character(NA)
    )
  } %>%
  {
    list(
      "bukken" = str_replace(., " [0-9]+号室", ""),
      "goshitsu" = str_extract(., " [0-9]+号室") %>%
        str_trim("both")
    )
  }

## room tyoe, floor, m2
df$info %>%
  str_replace("の賃貸.+", "") %>%
  {
    if_else(
      str_detect(., "\\("),
      str_extract(., "\\(.+\\)") %>%
        str_replace_all("\\(|\\)", ""),
      as.character(NA)
    )
  } %>%
  str_split("/") %>%
  {
    list(
      "room_type" = map_chr(.,  ~ .x[1]),
      "floor" = map_chr(., ~ .x[2]),
      "m2" = map_chr(., ~ .x[3] %>%
                       str_replace("m2", "")) %>%
        as.numeric
    )
  } -> cl_info


##

df$detail %>% {
  list(
    "yachin" = map_dbl(
      .,
      ~ .x$X1 %>%
        str_extract("^.+万円") %>%
        str_replace("万円$", "") %>%
        {as.numeric(.) * 10000}
    ),
    "kanri_kyoei_hi" =  map_dbl(
      .,
      ~ .x$X1 %>% str_extract("管理費(?s).*") %>%
        str_extract("[0-9].+円") %>% 
        str_replace("円", "") %>% 
        as.numeric()
    ),
    "facing_to" = map_chr(., ~ .x$X3 %>% str_extract(".+$")),
    "building_type" = map_chr(., ~ .x$X4 %>% str_extract("^.+")),
    "building_age" = map_int(., ~ .x$X4 %>% str_extract(".+$") %>% 
                               str_extract("[0-9]+") %>% 
                               as.integer()),
    "address" = map_chr(., ~.x$X5 %>% 
                          str_extract("^.+"))
    
  )} -> cl_gaiyo


# df$detail[[1]]$X1 %>% str_extract("^.+万円")
# df$detail[[1]]$X1 %>%
#   str_extract("管理費(?s).*") %>%
#   str_extract("[0-9].+円")
# 
# df$detail[[1]]$X3 %>% str_extract("^.+")
# df$detail[[1]]$X3 %>% str_extract(".+$")
# df$detail[[1]]$X4 %>% str_extract("^.+")
# df$detail[[1]]$X4 %>% str_extract(".+$")
# df$detail[[1]]$X5 %>% str_extract("^.+")
# df$gaiyo[[1]] %>% View
# df$gaiyo %>% map( ~ .x$X1) %>% unlist %>% unique
# df$gaiyo %>% map( ~ .x$X3) %>% unlist %>% unique

####

df$m_2_station[[1]]

df$m_2_station %>%
  map(~ .x %>% 
        str_extract(".+") %>% 
        {.[!is.na(.)]} %>% 
        str_split(" ")) %>% 
        {
          list("dest_1" = map())
        }

  


df$m_2_station %>% 
  map_int(length) %>% 
  {. == 3} %>% all


####
df$lnglat %>% {list(
    "lng" = str_extract(., "lng.+") %>% 
            str_extract("\\d+\\.*\\d*"),
    "lat" = str_extract(., "lat.+") %>% 
            str_extract("\\d+\\.*\\d*")
    )} -> cl_lnglat








