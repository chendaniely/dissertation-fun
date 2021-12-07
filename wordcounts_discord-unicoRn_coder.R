library(tidyverse)
library(lubridate)
library(scales)
library(ggtext)

thesis = data.frame(
  date = mdy( c("10/18/2021", # 2000
                # "10/19/2021", # 3484
                "10/19/2021", # 5584
                "10/21/2021", # 8978
                "10/23/2021", # 9002
                "10/24/2021", # 9006
                "10/25/2021", # 9792
                # "10/26/2021", # 1- 10434
                "10/26/2021", # 2- 12344
                "10/27/2021", # 14402                #"10/28/2021",
                "10/28/2021", # 13804
                "10/29/2021", # 15788
                "10/30/2021", # 20070
                # "10/31/2021", #31-1 (22306)
                "10/31/2021",# 31-2 (24922)
                "11/05/2021", # 11/5
                # "11/08/2021", #26428
                "11/08/2021",# 29748
                "11/09/2021",
                "11/11/2021"
  )),
  words = c(2000, # 10/18
            # 3484, # 10/19 - 1
            5584, # 10/19 - 2
            8978, # 10/21
            # 9002, # 10/23
            9006, # 10/24
            9792, # 10/25
            # 10434, # 10/26 - 1
            12344, # 10/26 - 2
            14402, # 10/27
            13804, # 10/28
            15788, # 10/29
            20070, # 10/30
            22306, # 31-1
            24922, # 31-2
            24922, # 11/5
            # 26428, # 11/08 - 1
            29748, # 11/08 - 2
            30040,
            31942
  )
)

thesis %>%
  group_by(date,words) %>%
  distinct(date) %>%
  count(words) %>%
  ungroup() %>%
  ggplot( aes(x= date,
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
