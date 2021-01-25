---
title: "Final Project Code"
author: "Jingru Yang, Laura Yuan, Wei Xi, Lunjing Yuan"
date: "11/10/2020"
output:
  html_document:
    df_print: paged
---

```{r, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
require("knitr")
opts_knit$set(root.dir = "/Users/laurayuan/Desktop/School/Masters/Fall_2020/Econometrics_w_R/Final_Project/")
```

```{r Basics, include=FALSE}
rm(list= ls())
library(readr)
library(dplyr)

# Reading dataframes
unemploymentrates <- read_csv("Cleaned_Unemploymentrate.csv")
race_rates <- read_csv("Cleaned_Race_with_dummy.csv")
```

## Unemployment Rate by State
```{r Scatterplot Unemployment Rates}
library(ggplot2)
ggplot(unemploymentrates, aes(x = Year_Month, y = Rate, color=State)) +
  geom_point(shape=1) + 
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle=90)) +
  scale_x_discrete(breaks=c("1999/01","1999/06","2000/01","2000/06","2001/01","2001/06","2002/01",
  "2003/01","2004/01","2005/01","2006/01","2007/01","2008/01",
  "2009/01","2010/01","2011/01","2012/01","2013/01","2014/01",
  "2015/01","2016/01","2017/01","2018/01","2019/01"))
```

```{r Boxplot of Unemployment Rate - Year}
library(ggplot2)
ggplot(gender, aes(Year_Month, Rate))+ geom_boxplot(varwidth=T, fill="plum") + 
    labs(title="Boxplot of Unemployment Rate from 1999-2019 in the U.S.", 
    subtitle = "Monthly ~ For all 50 states",
    x="Date (Year-Month)",
    y="Unemployment Rate (%)") +
    theme(axis.text.x = element_text(angle=90))
```

```{r Grouping Rates by Year}
# Group unemployment rate by year
unemploymentrates_yearavg <- unemploymentrates %>% group_by(State, Year) %>% summarise(Mean_Rate = mean(Rate))
unemploymentrates_yearavg
```

```{r Geographical Maps}
# https://socviz.co/maps.html
library(mapproj)
library(viridis)
library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)

# Creating a blank map
us_states <- map_data("state")

# Merging data
unemploymentrates$region <- tolower(unemploymentrates$State)
us_states_unemp <- left_join(us_states,unemploymentrates)

# Graph
p0 <- ggplot(data = subset(us_states_unemp, Year >= 1999),
             mapping = aes(x = long, y = lat,
                 group = group,
                 fill = Rate))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.05) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_viridis_c(option = "plasma")

p2 + theme_map() + facet_wrap(~ Year, ncol = 7) +
    theme(legend.position = "bottom",
          strip.background = element_blank()) +
    labs(fill = "Average Unemployment Rate (%)",
         title = "Unemployment Rate in the US from 1999-2019")
```

## Unemmployment Rate vs. Race
```{r Scatter Plot of Race}
library(ggplot2)
ggplot(race_rates, aes(x = Year_Qtr, y = Rate, color = Race)) +
  geom_point(shape=1) +
  labs(title="Unemployment Rates by Race (1999-2019)") +
  labs(subtitle="~ Quarterly") +
  labs(x="Date", y="Unemployment Rate (%)") +
  theme(axis.text.x = element_text(size=5,angle=90)) +
  scale_y_continuous(limits=c(0,18))
```

```{r BoxPlot of Race}
library(ggplot2)
ggplot(race_rates, aes(x=Race, y=Rate,fill=Race)) + 
  geom_boxplot() +
  labs(title="Unemployment Rates by Race (1999-2019)") +
  labs(subtitle="Boxplot of unemployment rates ")
  labs(x="Race", y="Unemployment Rate (%)") +
  scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14,16),limits=c(2,17))+
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu")
```



```{r Regression Model of Race vs. Unemployment Rate}
library(stargazer)
regmodel = lm(Rate ~ Caucasian_dummy, data = race_rates)
stargazer(regmodel, se=list(regmodel), title="Effect of chicken's age on weight",type="text", df=FALSE, digits=3)


```


