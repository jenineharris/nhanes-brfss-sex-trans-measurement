# remove the eval = FALSE to run
# TO CREATE ANALYTIC DATA SET, UNCOMMENT AND RUN THIS CODE CHUNK
# bring in files from online BRFSS 2015 to 2018 data
library(haven)
library(tidyverse)
# #
# #
# 2014 data
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
# #
# #
#
# # combine all years into 1 dataset
# BRFSS_all <- rbind(select(BRFSS_2014, SEX, TRNSGNDR, year),
#                    select(BRFSS_2015, SEX, TRNSGNDR, year),
#                    select(BRFSS_2016, SEX, TRNSGNDR, year),
#                    select(BRFSS_2017, SEX, TRNSGNDR, year),
#                    select(BRFSS_2018, SEX, TRNSGNDR, year),
#                    select(BRFSS_2019, SEX, TRNSGNDR, year),
#                    select(BRFSS_2020, SEX, TRNSGNDR, year))
#
# save BRFSS_all dataset as a .csv file so every time i want to work with, I just read that file
# in instead of running all steps above
#write.csv(BRFSS_all, file = "brfss_all.csv")

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

library(lemon) # reposition legend function
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

table(BRFSS_2019_clean$SEXVAR, BRFSS_2019_clean$BIRTHSEX)
table(BRFSS_2019_clean$TRNSGNDR, BRFSS_2019_clean$BIRTHSEX)

library(cowplot)

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
  labs(x = "Gender identity", y = "Percent in each\ntransgender category (sex 2019)",
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
  labs(x = "Gender identity", y = "Percent in each\ntransgender category (sex at birth 2019)",
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
  labs(x = "Gender identity", y = "Percent in each \ntransgender category (sex at birth 2020)",
       fill = "Sex category") +
  ylim(c(0, 100))

legend <- get_legend(
  # create some space to the left of the legend
  sexVar +
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom"))

## Get some percentages
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

# Figures

# Figure 1
reposition_legend(sixBars +  
                    facet_wrap(.~yearLong,nrow = 2,
                               labeller = label_wrap_gen(20)), 
                  position = "bottom",
                  panel='panel-4-2')

# Figure 2
overTime

# Figure 3
cowplot::plot_grid(plot_grid(sexVar + theme(legend.position = "none"),
                             sexAtBirthVar + theme(legend.position = "none"),
                             sexAtBirthVar2020 + theme(legend.position = "none"), nrow = 1,
                             labels = c("a", "b", "c")),
                   legend, ncol = 1, rel_heights = c(1,.1))



######### scraps

# basic table exploring data
library(table1)
percTrans <- prop.table(table(BRFSS_all_clean$TRNSGNDR, BRFSS_all_clean$SEX),
           margin = 1)

percTrans2019 <- prop.table(table(BRFSS_2019_clean$TRNSGNDR, BRFSS_2019_clean$BIRTHSEX),
                            margin = 1)

library(descr)

chisqtrans <- function(yr) {
  CrossTable(BRFSS_all_clean$SEX[BRFSS_all_clean$year == yr],
             BRFSS_all_clean$TRNSGNDR[BRFSS_all_clean$year == yr],
             chisq = TRUE,
             sresid = TRUE,
             prop.c = FALSE,
             prop.t = FALSE,
             prop.chisq = FALSE,
             expected = TRUE)
}

chisqtrans(2015)
chisqtrans(2016)
chisqtrans(2017)
chisqtrans(2018)
chisqtrans(2019)
chisqtrans(2020)



# +-------------+-------------------------+-------------+-------------+
#   | Q u e s t i | Question                | A s k e d o | Y ea r( s)  |
#   | o n i d     |                         | f . . .     |             |
#   +=============+=========================+=============+=============+
#   | \           |                         | c o m p u t | 20 15       |
#   | \           |                         | e d         |             |
#   | \_ D R N K  |                         |             |             |
#   | W K 1       |                         |             |             |
#   +-------------+-------------------------+-------------+-------------+
#   | \           | Binge drinkers (males   | c o m p u t | 20 15 , 20  |
#                                     | \           | having five or more     | e d         | 18          |
#                                     | \_ R F B I  | drinks on one occasion, |             |             |
#                                     | N G 5       | females having four or  |             |             |
#                                     |             | more drinks on one      |             |             |
#                                     |             | occasion)               |             |             |
#   +-------------+-------------------------+-------------+-------------+
#   | \           | Heavy drinkers (adult   | c o m p u t | 2 01 5, 2   |
#                                     | \           | men having more than 14 | e d         | 01 6, 20 17 |
#                                     | \_ R F D R  | drinks per week and     |             |             |
#                                     | H V 5       | adult women having more |             |             |
#                                     |             | than 7 drinks per week) |             |             |
#   +-------------+-------------------------+-------------+-------------+
#   | \           | Heavy drinkers (adult   | c o m p u t | 20 18       |
#                                     | \           | men having more than 14 | e d         |             |
#                                     | \_ R F D R  | drinks per week and     |             |             |
#                                     | H V 6       | adult women having more |             |             |
#                                     |             | than 7 drinks per week) |             |             |
#   +-------------+-------------------------+-------------+-------------+
#   
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   +-------------+------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   ---------------+-------------+------------------+ \| DIABETE3 \| (Ever
#                                                                     told) you have diabetes (If "Yes" and respondent is female, ask "Was
# this only when you were pregnant?". If Respondent says pre-diabetes or
# borderline diabetes, use response code 4.) \| female \| 2015 2018 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   
#   \| PREGNANT \| To your knowledge, are you now pregnant? \| female \|
#   2015, 2016, 2017, 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| HADMAM \| A mammogram is an x-ray of each breast to look for breast
# cancer. Have you ever had a mammogram? \| female \| 2015, 2016, 2018,
# 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| HOWLONG \| How long has it been since you had your last mammogram? \|
#   female \| 2015, 2016, 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| HADPAP2 \| A Pap test is a test for cancer of the cervix. Have you
# ever had a Pap test? \| female \| 2015, 2016, 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| LASTPAP2 \| How long has it been since you had your last Pap test? \|
#   female \| 2015, 2016, 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| HPVTEST \| An HPV test is sometimes given with the Pap test for
# cervical cancer screening. Have you ever had an HPV test? \| female \|
#   2015, 2016, 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| HPLSTTST \| How long has it been since you had your last HPV test? \|
#   female \| 2015, 2016, 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| HADHYST2 \| Have you had a hysterectomy? (A hysterectomy is an
#                                                operation to remove the uterus (womb).) \| female \| 2015, 2016, 2018,
# 2019 \|
#   
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| PROFEXAM \| A clinical breast exam is when a doctor, nurse, or other
# health professional feels the breast for lumps. Have you ever had a
# clinical breast exam? \| female \| 2015, 2016, 2018 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| LENGEXAM \| How long has it been since your last breast exam? \|
#   female \| 2015, 2016, 2018 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| PCPSAAD2 \| Has a doctor, nurse, or other health professional EVER
# talked with you about the advantages of the PSA test? \| male \| 2015,
# 2016 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| PCPSARE1 \| Has a doctor, nurse, or other health professional EVER
# recommended that you have a PSA test? \| male \| 2015, 2016, 2018, 2019
# \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| PCDMDECN \| Who made the decision with you? (Mark all that apply) \|
#   male \| 2015, 2016 \|
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   ---------------+-------------+------------------+ \| \_RFMAM2Y  \| Women
# respondents aged 40+ who have had a mammogram in the past two years \|
#   computed \| 2016 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| \_MAM5021  \| Women respondents aged 50-74 who have had a mammogram
# in the past two years \| computed \| 2016 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| \_RFPAP33  \| Women respondents aged 21-65 who have had a pap test in
# the past three years \| computed \| 2016 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| \_RFPSA21  \| Male respondents aged 40+ who have had a PSA test in
# the past 2 years \| computed \| 2016 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| PFPPRVN2  \| Did you or your partner do anything the last time you
# had sex to keep you from getting pregnant? \| female \| 2017, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| TYPCNTR7  \| What did you or your partner do the last time you had
# sex to keep you from getting pregnant? \| female \| 2017 \|
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   ---------------+-------------+------------------+ \| TYPCNTR8  \| What
# did you or your partner do the last time you had sex to keep you from
# getting pregnant? \| female \| 2019 \|
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   ---------------+-------------+------------------+ \| NOBCUSE6  \| What
# was your main reason for not doing anything the last time you had sex to
# keep you from getting pregnant? \| female \| 2017 \|
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   ---------------+-------------+------------------+ \| NOBCUSE7  \| What
# was your main reason for not doing anything the last time you had sex to
# keep you from getting pregnant? \| female \| 2019 \|
#   +-------------+-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   ---------------+-------------+------------------+
#   ---------------------+-------------+------------------+ \| CNCRTYP1  \|
#   With your most recent diagnoses of cancer, what type of cancer was it?
#   \| female, male \| 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| CSRVDOC1  \| What type of doctor provides the majority of your health
# care? \| female \| 2018, 2019 \|
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   
#   +-------------+----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+-------------+------------------+
#   \| \_RFMAM21  \| Women respondents aged 40+ who have had a mammogram in
# the past two years? \| female \| 2018 \|
#   +-------------+---------------------------------------------------------------------------------------------------------------------------------------------------------
#   -------------------------------------+-------------+------------------+
#   
#   +-------------+---------------------------------------------------------------------------------------------------------------------------------------------------------
#   -------------------------------------+-------------+------------------+
#   \| \_RFPAP34  \| Women respondents aged 50-74 who have had a pap test in
# the past two years? \| female \| 2018 \|
#   +-------------+---------------------------------------------------------------------------------------------------------------------------------------------------------
#   -------------------------------------+-------------+------------------+