# remove the eval = FALSE to run
# TO CREATE ANALYTIC DATA SET, UNCOMMENT AND RUN THIS CODE CHUNK
# bring in files from online BRFSS 2015 to 2018 data
library(haven)
library(tidyverse)
library(cowplot)
library(lemon)
#
#
# # 2014 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2014/files/LLCP2014XPT.zip", destfile = temp)
# BRFSS_2014 <- read_xpt(file = temp)
# 
# # 2015 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2015/files/LLCP2015XPT.zip", destfile = temp)
# BRFSS_2015 <- read_xpt(file = temp)
# # # #
# # # 2016 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2016/files/LLCP2016XPT.zip", destfile = temp)
# BRFSS_2016 <- read_xpt(file = temp)
# # #
# # # # 2017 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2017/files/LLCP2017XPT.zip", destfile = temp)
# BRFSS_2017 <- read_xpt(file = temp)
# # #
# # 2018 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2018/files/LLCP2018XPT.zip", destfile = temp)
# BRFSS_2018 <- read_xpt(file = temp)
# #
# # # 2019 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip", destfile = temp)
# BRFSS_2019 <- read_xpt(file = temp)
# #
# # 2020 data
# temp <- tempfile(fileext = ".zip")
# download.file(url  = "https://www.cdc.gov/brfss/annual_data/2020/files/LLCP2020XPT.zip", destfile = temp)
# BRFSS_2020 <- read_xpt(file = temp)
# 
# # # select only the variables to use in the analysis (e.g. changes 300+ variables to only needed for analysis)
# 
# #
# BRFSS_2014 <- BRFSS_2014 %>%
#   select(SEX,  TRNSGNDR, `_LLCPWT`) %>%
#   mutate(year = "2014")
# # #
# BRFSS_2015 <- BRFSS_2015 %>%
#   select(SEX, TRNSGNDR, `_RFBING5`, `_RFDRHV5`, DIABETE3,
#          PREGNANT, HADMAM, HOWLONG, HADPAP2, LASTPAP2, HPVTEST,
#          HPLSTTST, HADHYST2, PROFEXAM, LENGEXAM, PCPSAAD2, PCPSADI1,
#          PCPSARE1, PSATEST1, PSATIME, PCPSARS1, PCPSADE1, PCDMDECN,
#          `_DRNKWEK`) %>%
#   mutate(year = "2015")
# #
# BRFSS_2016 <- BRFSS_2016 %>%
#   select(SEX, `_DRNKWEK`, TRNSGNDR, `_RFDRHV5`, PREGNANT, HADMAM,
#          HOWLONG, HADPAP2, LASTPAP2, HPVTEST, HPLSTTST, HADHYST2,
#          PROFEXAM, LENGEXAM, PCPSAAD2, PCPSADI1, PCPSARE1, PSATEST1,
#          PSATIME, PCPSARS1, PCPSADE1, PCDMDECN, `_RFMAM2Y`, `_MAM5021`,
#          `_RFPAP33`, `_RFPSA21`)%>%
#   mutate(year = "2016")
# #
# BRFSS_2017 <- BRFSS_2017 %>%
#   select(SEX, `_DRNKWEK`, TRNSGNDR, `_RFDRHV5`, PREGNANT, PFPPRVN2,
#          TYPCNTR7, NOBCUSE6)%>%
#   mutate(year = "2017")
# #
# BRFSS_2018 <- BRFSS_2018 %>%
#   select(SEX1, `_DRNKWEK`,  TRNSGNDR, `_LLCPWT`)%>%
#   mutate(year = "2018")
# # #
# BRFSS_2019 <- BRFSS_2019 %>%
#   select(`_MICHD`, `_SEX`, EDUCA, `_BMI5CAT`, `_DRNKWK1`, EXERANY2, `_RACE`,
#          `_AGE80`, `_SMOKER3`, TRNSGNDR, `_LLCPWT`)%>%
#   mutate(year = "2019")
# #
# BRFSS_2020 <- BRFSS_2020 %>%
#   mutate(year = "2020") %>%
#   rename(SEX = `_SEX`)
# #
# # # renamed SEX1 in 2018 to SEX like all the other years
# BRFSS_2018 <- BRFSS_2018 %>%
#   rename(SEX = SEX1)
# BRFSS_2019 <- BRFSS_2019 %>%
#   rename(SEX = `_SEX`)
# #
# # renamed AVEDRNK3 to AVEDRNK2 in 2019 like all the other years
# BRFSS_2019 <- BRFSS_2019 %>%
#   rename(`_DRNKWEK` = `_DRNKWK1`)
# 
# # combine all years into 1 dataset
# BRFSS_all <- rbind(select(BRFSS_2014, SEX, TRNSGNDR, year),
#                    select(BRFSS_2015, SEX, TRNSGNDR, year),
#                    select(BRFSS_2016, SEX, TRNSGNDR, year),
#                    select(BRFSS_2017, SEX, TRNSGNDR, year),
#                    select(BRFSS_2018, SEX, TRNSGNDR, year),
#                    select(BRFSS_2019, SEX, TRNSGNDR, year),
#                    select(BRFSS_2020, SEX, TRNSGNDR, year))

# save BRFSS_all dataset as a .csv file so every time i want to work with, I just read that file
# in instead of running all steps above
# write.csv(BRFSS_all, file = "brfss_all.csv")

# if brfss_all file saved, comment out everything except library code above  
# and un comment next line of code to import data
BRFSS_all <- read.csv("brfss_all.csv")

# add labels
BRFSS_all_clean <- BRFSS_all %>% 
  mutate(SEX = recode_factor(SEX, 
                             '1' = 'male',
                             '2' = 'female',
                             '7' = NA_character_,
                             '9' = NA_character_)) %>% 
  mutate(TRNSGNDR = recode_factor(TRNSGNDR,
                                  '1' = 'Trans male-to-female',
                                  '2' = 'Trans female-to-male',
                                  '3' = 'Trans gender nonconforming',
                                  '4' = 'No',
                                  '7' = NA_character_,
                                  '9' = NA_character_)) %>% 
  filter(TRNSGNDR %in% c("Trans male-to-female",
                         "Trans female-to-male")) %>% 
  mutate(yearLong = recode_factor(year,
                                  '2014' = '2014: Indicate sex of respondent. Ask only if necessary.',
                                  '2015' = '2015: Indicate sex of respondent. Ask only if necessary.',
                                  '2016' = '2016: Are you... (Male, Female, Refused)',
                                  '2017' = '2017: Are you male or female?',
                                  '2018' = '2018: What is your sex? or What was your sex at birth? Was it...',
                                  '2019' = '2019: Are you male or female? or What was your sex at birth? Was it male or female?',
                                  '2020' = '2020: What was your sex at birth? Was it male or female?')) %>% 
  drop_na() %>% 
  droplevels()

# create Table 1
# dataTable1 <- BRFSS_all_clean %>% 
#   select(yearLong, TRNSGNDR, SEX) %>% 
#   group_by(yearLong, TRNSGNDR, SEX) %>% 
#   count() %>% 
#   group_by(TRNSGNDR, yearLong) %>% 
#   mutate(percent = 100*(n/sum(n)))

# table1forpaper <- 
#   table1(~ yearLong | TRNSGNDR * SEX, 
#          overall = FALSE,
#          render.factor = c(. = "Median"),
#          data = dataTable1)
# table1forpaper

# create figure 1
sixBars <- BRFSS_all_clean %>% 
  drop_na() %>% 
  group_by(TRNSGNDR, SEX, yearLong) %>% 
  count() %>% 
  group_by(TRNSGNDR, yearLong) %>% 
  mutate(percYear = 100 * n / sum(n)) %>% 
  ggplot(aes(x = TRNSGNDR, fill = SEX, y = percYear)) +
  geom_col(position = "dodge") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8)) +
  facet_wrap(~yearLong, nrow = 2,
             labeller = label_wrap_gen(20)) +
  scale_fill_manual(values = c("gray60", "black")) +
  theme_minimal() +
  labs(x = "Gender identity", y = "Percent in each sex category",
       fill = "Sex category")  

# create figure 2
overTime <- BRFSS_all_clean %>% 
  drop_na() %>% 
  mutate(year = as.numeric(as.character(year))) %>% 
  group_by(TRNSGNDR, SEX, year) %>% 
  count() %>% 
  group_by(TRNSGNDR, year) %>% 
  mutate(percYear = 100 * n / sum(n)) %>% 
  ggplot(aes(color = SEX, x = year, y = percYear)) +
  geom_path() +
  geom_point() +
  #scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  facet_wrap(~TRNSGNDR, nrow = 2) +
  scale_color_manual(values = c("gray60", "black")) +
  theme_minimal() +
  labs(x = "Year", y = "Percent in each sex category",
       color = "Sex category")  + 
  scale_x_continuous(labels=c("2014","2015","2016","2017", "2018", "2019","2020"),
                     breaks=c(2014,2015,2016,2017, 2018, 2019,2020)) +
  ylim(c(0,100))

# re-download BRFSS 2019 for fig 3 creation
temp <- tempfile(fileext = ".zip")
download.file(url  = "https://www.cdc.gov/brfss/annual_data/2019/files/LLCP2019XPT.zip", destfile = temp)
BRFSS_2019_raw <- read_xpt(file = temp)

BRFSS_2019_clean <- BRFSS_2019_raw %>% 
  mutate(SEXVAR = recode_factor(SEXVAR,
                                '1' = 'male',
                                '2' = 'female')) %>% 
  mutate(BIRTHSEX = recode_factor(BIRTHSEX,
                                  '1' = 'male',
                                  '2' = 'female',
                                  '7' = NA_character_,
                                  '9' = NA_character_)) %>% 
  mutate(TRNSGNDR = recode_factor(TRNSGNDR,
                                  '1' = 'Trans male-to-female',
                                  '2' = 'Trans female-to-male',
                                  '3' = 'Trans gender nonconforming',
                                  '4' = "No",
                                  '7' = NA_character_,
                                  '9' = NA_character_))

# create figure 3
sexVar <- BRFSS_2019_clean %>% 
  drop_na(TRNSGNDR, SEXVAR) %>% 
  filter(TRNSGNDR != "No" & TRNSGNDR != "Trans gender nonconforming") %>% 
  group_by(TRNSGNDR, SEXVAR) %>% 
  count() %>% 
  group_by(TRNSGNDR) %>% 
  mutate(percSex = 100 * n / sum(n)) %>% 
  ggplot(aes(x = TRNSGNDR, fill = SEXVAR, y = percSex)) +
  geom_col(position = "dodge") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  # facet_wrap(~yearLong, 
  #            labeller = label_wrap_gen(20)) +
  scale_fill_manual(values = c("gray60", "black")) +
  theme_minimal() +
  labs(x = "Gender identity", y = "Percent per\ntrans category\n(sex 2019)",
       fill = "Sex category")  +
  ylim(c(0, 100))

sexAtBirthVar <- BRFSS_2019_clean %>% 
  drop_na(TRNSGNDR, BIRTHSEX) %>% 
  filter(TRNSGNDR != "No" & TRNSGNDR != "Trans gender nonconforming") %>% 
  group_by(TRNSGNDR, BIRTHSEX) %>% 
  count() %>% 
  group_by(TRNSGNDR) %>% 
  mutate(percSex = 100 * n / sum(n)) %>% 
  ggplot(aes(x = TRNSGNDR, fill = BIRTHSEX, y = percSex)) +
  geom_col(position = "dodge") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  # facet_wrap(~yearLong, 
  #            labeller = label_wrap_gen(20)) +
  scale_fill_manual(values = c("gray60", "black")) +
  theme_minimal() +
  labs(x = "Gender identity", y = "Percent per\ntrans category\n(sex at birth 2019)",
       fill = "Sex category") +
  ylim(c(0, 100))

sexAtBirthVar2020 <- BRFSS_all_clean %>% 
  filter(year == 2020) %>% 
  drop_na(TRNSGNDR, SEX) %>% 
  filter(TRNSGNDR != "No" & TRNSGNDR != "Trans gender nonconforming") %>% 
  group_by(TRNSGNDR, SEX) %>% 
  count() %>% 
  group_by(TRNSGNDR) %>% 
  mutate(percSex = 100 * n / sum(n)) %>% 
  ggplot(aes(x = TRNSGNDR, fill = SEX, y = percSex)) +
  geom_col(position = "dodge") +
  #coord_flip() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  # facet_wrap(~yearLong, 
  #            labeller = label_wrap_gen(20)) +
  scale_fill_manual(values = c("gray60", "black")) +
  theme_minimal() +
  labs(x = "Gender identity", y = "Percent per \ntrans category\n(sex at birth 2020)",
       fill = "Sex category") +
  ylim(c(0, 100))

legend <- get_legend(
  # create some space to the left of the legend
  sexVar +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

## Get some percentages
percTrans <- prop.table(table(BRFSS_all_clean$TRNSGNDR, BRFSS_all_clean$SEX),
                        margin = 1)

percTrans2019 <- prop.table(table(BRFSS_2019_clean$TRNSGNDR, BRFSS_2019_clean$BIRTHSEX),
                            margin = 1)

percTransYear <- prop.table(table(BRFSS_all_clean$TRNSGNDR, 
                                  BRFSS_all_clean$SEX,
                                  BRFSS_all_clean$year),
                            margin = 1)

percTransTib <- BRFSS_all_clean %>% 
  group_by(TRNSGNDR, SEX, year) %>% 
  count() %>% 
  group_by(year, TRNSGNDR) %>% 
  mutate(percentTranYr = n/sum(n)) %>% 
  filter(year %in% c('2014', '2015', '2016'))

percTransTibAll <- BRFSS_all_clean %>% 
  group_by(TRNSGNDR, SEX, year) %>% 
  count() %>% 
  group_by(year, TRNSGNDR) %>% 
  mutate(percentTranYr = n/sum(n)) 

# Format and print figures

# Figure 1
# reposition_legend(sixBars +  
#                     facet_wrap(.~yearLong,nrow = 2,
#                                labeller = label_wrap_gen(20)), 
#                   position = "bottom",
#                   panel='panel-4-2')

# New FIGURE 1 (was Figure 2)
overTime

# New FIGURE 2 (was Figure 3)
cowplot::plot_grid(plot_grid(sexVar + theme(legend.position = "none"),
                             sexAtBirthVar + theme(legend.position = "none"),
                             sexAtBirthVar2020 + theme(legend.position = "none"), nrow = 1,
                             labels = c("a", "b", "c")),
                   legend, ncol = 1, rel_heights = c(1,.1))

