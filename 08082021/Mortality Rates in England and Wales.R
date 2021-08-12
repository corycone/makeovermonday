library(tidyverse)
library(zoo) #for period/date conversion
library(lubridate)

mortality_rates.df <- read.csv("Mortality Rates in England and Wales.csv", stringsAsFactors = FALSE)

colnames(mortality_rates.df) <- c("Period", "m_england", "f_england", "m_wales", "f_wales")
mortality_rates.df$Period <- as.yearmon(mortality_rates.df$Period,format="%b-%y")
mortality_rates.df$Period <- year(mortality_rates.df$Period)

mortality_rates.df.gathered <- mortality_rates.df %>% 
  gather("m_england", "f_england", "m_wales", "f_wales", 
         key = "Type", 
         value = "Deaths")

s = 1.5

##Original Viz Recreation
ggplot(mortality_rates.df, aes(x = Period, y = m_wales, group = 1)) +
  geom_line(size = s, aes(linetype = "Male - Wales", color = "Male - Wales")) +
  geom_line(size = s, aes(x = Period, y = m_england, linetype = "Male - England", color = "Male - England"))+
  geom_line(size = s, aes(x = Period, y = f_england, linetype = "Female - England", color = "Female - England"))+
  geom_line(size = s, aes(x = Period, y = f_wales, linetype = "Female - Wales", color = "Female - Wales")) +
  ylim(0,2000) +
  xlim(2001, 2021) +
  labs(title = "Figure 1: Mortality rates for the month of June were lower in 2021 than in 
          2020, for males and females in England and Wales",
       subtitle = "Age-standardized mortality rates by sex, England and wals, deaths registered in June 2001 to June 2021
       
       
       Age-standardised mortality rates",
       caption = "Source: Office for National Statistic - Monthly mortality analysis",
       x = "Year",
       y = "",
       color  = "Guide name", 
       linetype = "Guide name", 
       shape = "Guide name") +
  theme_classic() +
  theme(panel.grid.major.y = element_line(colour = "gray"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        legend.direction = "horizontal", 
        legend.position = "bottom", 
        legend.key = element_blank(),
        legend.title = element_blank(),
        plot.caption = element_text(color = "#666666", face="bold", size = 10),
        plot.title = element_text(hjust = 0.5, color = "#666666", size = 15),
        plot.subtitle = element_text(hjust = 0.5, color = "#666666")) +
        scale_linetype_manual("Variabler",values=c("Male - Wales"=2, "Female - Wales"=2, "Male - England"=1, "Female - England"=1)) +
        scale_color_manual("Variabler", values = c("Male - England" = "#1a659c", "Female - England" = "#36a0d2", "Male - Wales" = "#153a4c", "Female - Wales" = "#247f6e")) +
        guides(fill = guide_legend(keywidth = 1, keyheight = 1), 
        linetype=guide_legend(nrow = 2, keywidth = 3, keyheight = 1),
        colour=guide_legend(nrow = 2, keywidth = 3, keyheight = 1))


##Makeover Monday Adjustements




