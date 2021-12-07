library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)
library(readr)

thesis <- readr::read_csv("data/thesis-wordcounts.csv")

eod <- thesis %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(words = max(words)) %>%
  dplyr::ungroup()

ggplot(eod,  aes(x= date,
            y= words))+
geom_col(fill='#4d8ef7', show.legend = F)+
geom_text(aes(label= words,
              size=5,
              vjust= 0.5,
              hjust= 1.2),
          color='black')+
scale_x_date(breaks = 'day')+
coord_flip()+
ggdark::dark_mode()+
labs(
  title = "Daniel Chen Thesis Writing",
  subtitle = "word count total sums by date, posted by Daniel on Discord",
  y="word count",
  x=""
)+
theme(
  plot.title = element_text(face = 'bold',
                            size = 15,
                            color ='#4d8ef7',
                            hjust = 0.5 ),
  plot.subtitle = element_text(hjust = 0.5, face = 'bold'),
  legend.position = 'none'
)


ggplot(eod,
       aes(x = lubridate::date(date),
           y = words)) +
  geom_line(color = "orange") +
  theme_minimal() +
  ggdark::dark_mode() +
  ggtitle("Guess when the dissertation was due?",
          subtitle = "A.k.a: Cortisol Levels") +
  xlab("Date") +
  ylab("Word Count") +
  scale_x_date(breaks = "week") +
  theme(plot.title.position = "plot") +
  NULL
