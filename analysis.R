library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)

Data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

#################################Number################################

#max_black_jail_pop
max_black_jail_pop <- Data %>%
  filter(black_jail_pop == max(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_jail_pop)
max_black_jail
#13143.92

#max_white_jail_pop
max_white_jail_pop <- Data %>%
  filter(white_jail_pop == max(white_jail_pop, na.rm = TRUE)) %>%
  pull(white_jail_pop)
max_white_jail
#7036.59


#the highest_male_jail_year_pop
Highest_male_jail_year <- Data %>% 
  filter(male_jail_pop == max(male_jail_pop, na.rm = TRUE)) %>% 
  pull(year)
Highest_male_jail_year
# Year 1996

#highest_female_jail_year_pop
Highest_female_jail_year <- Data %>%
  filter(female_jail_pop == max(female_jail_pop, na.rm = TRUE)) %>%
  pull(year)
Highest_female_jail_year
# Year 1991

#mean_black_male_1996
mean_black_male_1996 <- Data %>%
  filter(year == "1996") %>%
  summarise(mean = mean(black_male_prison_pop, na.rm = TRUE)) %>%
  pull(mean)
mean_black_male_1996
#417.7187

#mean_white_male_1996
mean_white_male_1996 <- Data %>%
  filter(year == "1996") %>%
  summarise(mean = mean(white_male_prison_pop, na.rm = TRUE)) %>%
  pull(mean)
mean_white_male_1996
#237.1446

#mean_black_female_1991
mean_black_female_1991 <- Data %>%
  filter(year == "1991") %>%
  summarise(mean = mean(black_female_prison_pop, na.rm = TRUE)) %>%
  pull(mean)
mean_black_female_1991
#11.70588

#mean_white_female_1991
mean_white_female_1991 <- Data %>%
  filter(year == "1991") %>%
  summarise(mean = mean(white_female_prison_pop, na.rm = TRUE)) %>%
  pull(mean)
mean_white_female_1991
#7.323636

#county_max_black_male
county_max_black_male <- Data %>%
  filter(black_male_prison_pop == max(black_male_prison_pop, na.rm = TRUE))%>%
  pull(county_name)
county_max_black_male
#"New York County"

#county_max_white_male
county_max_white_male <- Data %>%
  filter(white_male_prison_pop == max(white_male_prison_pop, na.rm = TRUE))%>%
  pull(county_name)
county_max_white_male
#"Los Angeles County"

#county_max_black_female
county_max_balck_female <- Data %>%
  filter(black_female_prison_pop == max(black_female_prison_pop, na.rm = TRUE))%>%
  pull(county_name)
county_max_balck_female
#"Los Angeles County"

#county_max_white_female
county_max_white_female <- Data %>%
  filter(white_female_prison_pop == max(white_female_prison_pop, na.rm = TRUE))%>%
  pull(county_name)
county_max_white_female
#"Maricopa County"

#ratio_black_white_1996_NewYorkCounty
ratio_black_white_1996_NY <- Data %>%
  filter(year == 1996 & county_name == "New York County") %>%
  summarize(ratio = black_jail_pop/white_jail_pop)%>%
  pull(ratio)
ratio_black_white_1996_NY
#5.976834

#ratio_black_white_1996_LosAngelesCounty
ratio_black_white_1996_LA <- Data %>%
  filter(year == 1996 & county_name == "Los Angeles County") %>%
  summarize(ratio = black_jail_pop/white_jail_pop)%>%
  pull(ratio)
ratio_black_white_1996_LA
#1.844801

#################################Chart################################
