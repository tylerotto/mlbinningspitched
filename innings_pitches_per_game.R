library(tidyverse)
library(sqldf)
library(rvest)
library(glue)

###############


# define time range of years to pull and the website to scrape
jump <- seq(1960, 2003, by = 1)
site <- paste('https://www.baseball-almanac.com/players/baseball_births.php?y=', jump, sep="")


# create a list of data frames of players by birth year
dfList <- lapply(site, function(i) {
  webpage <- read_html(i)
  draft_table <- html_nodes(webpage, 'table')
  draft <- html_table(draft_table)[[1]] %>% 
    as.data.frame() %>% 
    as_tibble() %>% 
    janitor::clean_names() %>% 
    filter(str_length(x4)==4) %>% 
    rename(player_name = x1,
           birth_year = x2) %>% 
    select(1,2) 
})


# combine the list of data frames in one data frame
finaldf <- do.call(rbind, dfList)  %>% 
  mutate(player_name = str_squish(player_name))

############################



game_log_web <- read_html('https://www.baseball-reference.com/leagues/daily.fcgi?request=1&type=p&dates=lastndays&lastndays=100000')

draft_table <- html_nodes(game_log_web, 'table')
draft <- html_table(draft_table)[[1]] %>% 
  janitor::clean_names() %>% 
  mutate(name = str_squish(name))
  

# select(name, birth_year, gs, ip) %>% 
library(lubridate)

check <- inner_join(draft, finaldf, by = c('name'='player_name')) %>% 
  mutate(across(8:so_w, as.numeric),
         player_birth_year= year(mdy(birth_year))) %>% 
  filter(gs > 50) %>% 
  group_by(player_birth_year) %>% 
  summarise(innings_pitched_per_game = sum(ip)/sum(gs),
            triples = sum(x3b)
            )%>% 
  arrange(desc(player_birth_year))

check %>% 
  ggplot(aes(x=player_birth_year, y =innings_pitched_per_game )) +
  geom_line()



###############



inner_join(draft, finaldf, by = c('name'='player_name')) %>% 
  select(birth_year) %>% 
  mutate(date = year(mdy(birth_year)))


finaldf %>% 
  distinct(player_name) %>% 
  arrange(player_name)

draft %>% 
  distinct(name)%>% 
  arrange(name)



