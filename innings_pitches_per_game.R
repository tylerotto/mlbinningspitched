library(tidyverse)
library(sqldf)
library(rvest)
library(glue)
library(hrbrthemes)
library(lubridate)
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



game_log_web <- read_html('https://www.baseball-reference.com/leagues/daily.fcgi?request=1&type=p&dates=lastndays&lastndays=200000')

draft_table <- html_nodes(game_log_web, 'table')
draft <- html_table(draft_table)[[1]] %>% 
  janitor::clean_names() %>% 
  mutate(name = str_squish(name)) 
  



mlb_agg_data <- inner_join(draft, finaldf, by = c('name'='player_name')) %>% 
  mutate(across(8:so_w, as.numeric),
         player_birth_year= floor_date(mdy(birth_year), unit= 'year')) %>% 
  filter(gs > 50) %>% 
  group_by(player_birth_year) %>% 
  summarise(innings_pitched_per_game = sum(ip)/sum(gs),
            strikeouts_per_game = sum(so)/sum(gs)
            ) %>% 
  ungroup() %>% 
  arrange(desc(player_birth_year)) %>% 
  filter(player_birth_year >= 1960) %>% 
  pivot_longer(cols = -player_birth_year,names_to = 'mlb_metric')



# Load png package
library("png")                  
my_image <-  readPNG("C:\\Users\\theot\\Desktop\\Major_League_Baseball_MLB_transparent_logo.png", native = T)

# Load patchwork in inset the MLB logo above
library("patchwork") 
library(scales)


ts_mlb_plot <- mlb_agg_data %>% 
  ggplot(aes(x=player_birth_year, y=value, color= mlb_metric)) +
  geom_line(size = 1) +
  # theme(panel.grid.minor = element_blank()) +
  theme_ipsum_ps() +
  ggtitle("Pitching Metrics by Pitcher's Birth Year") +
  scale_x_date(date_breaks = "4 years", labels = date_format("%Y")) +
  scale_y_continuous(breaks=seq(4,8,0.5)) +
  scale_color_manual(name = 'Pitching Metric', 
                     labels=c('Innings per game', 'Strikeouts per game'),
                     values = c('darkred', 'darkblue')) +
  xlab("Pitcher's Birth Year") +
  ylab("") +
  inset_element(p = my_image, 0.85, 0.85, 1, 1, align_to = 'full') 

ts_mlb_plot

ggsave('ts_mlb_plot.png')
