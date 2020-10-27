#Paketler
library(ggplot2)
library(gganimate)
library(gifski)
library(av)
library(tidyverse)
library(lubridate)
library(maps)
library(ggthemes)
library(mapproj)
library(sf)

#Veri aktarma ve manipülasyon

covid <- read.csv("C:/Users/monster/Desktop/time_series_covid_19_confirmed.csv")

covid <- covid %>%
  rename(Country = Country.Region) %>%
  select(2:dim(covid)[2]) %>%
  gather(key = "date", value = "cases", 4:249)




covid$date <- str_replace_all(covid$date, pattern = "X", replacement = "0")

covid$date <- mdy(covid$date)


#Görseller


world_map <- map_data("world") %>%
  filter(region != "Antarctica")

anim <- ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="white", colour = "black") +
  theme_dark() +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(color = 'black',
                                        fill = '#0A0C0B',
                                        ),
        legend.position=c(0.1, 0.2),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "#0A0C0B"),
        legend.title = element_text(color = "white", size = 20),
        legend.text = element_text(color = "white", size=20),
        legend.key = element_rect(fill = "#0A0C0B", color = NA)
        ) +
  labs(size = "Vaka Sayýlarý") +
  geom_point(data = covid, aes(x=Long, y=Lat, size = cases, group = Country), color = '#FF2525') +
  geom_text(data=covid, aes(x=-100, y=-25, label=as.character(date), group=Country), check_overlap = TRUE, size=10, fontface="bold", color = "white") +
  scale_size_continuous(range = c(1,20), limits = c(0,7000000), labels=scales::comma, breaks = c(0, 100000, 300000, 500000, 700000, 1000000, 1500000, 3000000, 4000000, 7000000)) + 
  transition_time(date)
  



animate(anim, renderer = gifski_renderer(), width = 1920, height = 1080, duration = 15)
anim_save("europe.gif")


