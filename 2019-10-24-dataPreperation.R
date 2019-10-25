# Data Preperation ----
y3MeanNumScore2008 <- 396.9 # p.38 https://www.nap.edu.au/_resources/2ndStageNationalReport_18Dec_v2.pdf
y3MeanReadScore2008 <- 400.5 # p.6 https://www.nap.edu.au/_resources/2ndStageNationalReport_18Dec_v2.pdf
y3MeanNumScore2010 <- 395.4 #p. 46 https://www.nap.edu.au/_resources/NAPLAN_2010_National_Report.pdf
y3MeanReadScore2010 <- 414.3 #p. 2 https://www.nap.edu.au/_resources/NAPLAN_2010_National_Report.pdf
y5MeanNumScore2010 <- 488.8  #p. 110 https://www.nap.edu.au/_resources/NAPLAN_2010_National_Report.pdf
y5MeanReadScore2010 <- 487.4  #p. 110 https://www.nap.edu.au/_resources/NAPLAN_2010_National_Report.pdf
y5MeanNumScore2012 <- 488.7 # p. 110 https://www.nap.edu.au/_resources/NAPLAN_2012_National_Report.pdf
y5MeanReadScore2012 <- 493.6 # p. 110 https://www.nap.edu.au/_resources/NAPLAN_2012_National_Report.pdf
# libraries -----------------------------------------------------

library(tidyverse)
library(readit)
library(ggthemes)
library(wesanderson)
library(car)
library(margins)
library(mice)


# Data Preperation
# Child Data ====================================================
# Age 4 Demographics ####
cDataAge4K <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk4.sas7bdat") %>%
  select(cid = hicid, ses = csep, geo = csos, lang = cf11m2,
         iq = cppvt, indig1 =zf12m2, indig2 = zf12cm) %>%
  mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
         lang = ifelse(lang == '1201', 'eng', 'other'),
         indig = case_when(
           indig1 < 0 ~ NA_character_,
           indig2 < 0 ~ NA_character_,
           indig1 > 1 ~ 'indig',
           indig2 > 1 ~ 'indig',
           TRUE ~ 'nonIndig'
         ) %>% factor,
         cohort = 'K'
  )

cDataAge4B <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrb4.sas7bdat") %>%
  select(cid = hicid, ses = csep, geo = csos,
         iq = cppvt) %>%
  mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
         cohort = 'B')

cDataAge0 <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrb0.sas7bdat") %>%
  select(cid = hicid, indig1 =zf12m2, indig2 = zf12m3, lang = af11m2) %>%
  mutate(indig = case_when(
    indig1 < 0 ~ NA_character_,
    indig2 < 0 ~ NA_character_,
    indig1 > 1 ~ 'indig',
    indig2 > 1 ~ 'indig',
    TRUE ~ 'nonIndig'
  ) %>% factor(),
  lang = ifelse(lang == '1201', 'eng', 'other'),
  )

cDataAge4B <- right_join(cDataAge4B, cDataAge0)

cDataAge4 <- bind_rows(cDataAge4K, cDataAge4B)
#  Age 8 ####
cDataAge8K <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk8.sas7bdat") %>%
  select(cid = hicid, y3Grade = epc06a1,y3State = estate,y3Weight = eweight,
         y3Stratum = stratum, y3MathSc = epc58b2, y3MathScPar = elc08a2a,
         y3SdqPar = easdqtb, y3SdqTeach = etsdqtb,
         y3ReactivePar = esatir, y3PersistPar = esatip,
         y3ReadSc = epc58b8, y3ReadScPar = elc08a1a
  ) %>%
  mutate(y3MathSc = case_when(
    y3MathSc == 1 ~ 3,
    y3MathSc == 2 ~ 2,
    y3MathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3ReadSc = case_when(
    y3ReadSc == 1 ~ 3,
    y3ReadSc == 2 ~ 2,
    y3ReadSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3MathScPar = case_when(
    y3MathScPar == 1 ~ 5,
    y3MathScPar == 2 ~ 4,
    y3MathScPar == 3 ~ 3,
    y3MathScPar == 4 ~ 2,
    y3MathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3ReadScPar = case_when(
    y3ReadScPar == 1 ~ 5,
    y3ReadScPar == 2 ~ 4,
    y3ReadScPar == 3 ~ 3,
    y3ReadScPar == 4 ~ 2,
    y3ReadScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K',
  y3NumNation = y3MeanNumScore2008,
  y3ReadNation = y3MeanReadScore2008)

cDataAge8B <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrb8.sas7bdat") %>%
  select(cid = hicid, y3Grade = epc06a1,y3State = estate,y3Weight = eweight,
         y3Stratum = stratum, y3MathSc = epc58b2, y3MathScPar = elc08a2a,
         y3SdqPar = easdqtb, y3SdqTeach = etsdqtb,
         y3ReactivePar = esatir, y3PersistPar = esatip,
         y3ReadSc = epc58b8, y3ReadScPar = elc08a1a) %>%
  mutate(y3MathSc = case_when(
    y3MathSc == 1 ~ 3,
    y3MathSc == 2 ~ 2,
    y3MathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3ReadSc = case_when(
    y3ReadSc == 1 ~ 3,
    y3ReadSc == 2 ~ 2,
    y3ReadSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3MathScPar = case_when(
    y3MathScPar == 1 ~ 5,
    y3MathScPar == 2 ~ 4,
    y3MathScPar == 3 ~ 3,
    y3MathScPar == 4 ~ 2,
    y3MathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3ReadScPar = case_when(
    y3ReadScPar == 1 ~ 5,
    y3ReadScPar == 2 ~ 4,
    y3ReadScPar == 3 ~ 3,
    y3ReadScPar == 4 ~ 2,
    y3ReadScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'B',
  y3NumNation = y3MeanNumScore2010,
  y3ReadNation = y3MeanReadScore2010)

cDataAge8 <- bind_rows(cDataAge8K, cDataAge8B)
# Age 10 #####
cDataAge10K <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk10.sas7bdat") %>%
  select(cid = hicid, y5Grade = fpc06a1,y5State = fstate,y5Weight = fweight,
         y5Stratum = stratum, y5MathSc = fpc58b2, y5MathScPar = flc08a2a,
         y5SdqPar = fasdqtb, y5SdqTeach = ftsdqtb,
         y5ReadSc = fpc58b8, y5ReadScPar = flc08a1a,
         y5ReactivePar = fsatir, y5PersistPar = fsatip) %>% 
  mutate(y5MathSc = case_when(
    y5MathSc == 1 ~ 3,
    y5MathSc == 2 ~ 2,
    y5MathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5ReadSc = case_when(
    y5ReadSc == 1 ~ 3,
    y5ReadSc == 2 ~ 2,
    y5ReadSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5MathScPar = case_when(
    y5MathScPar == 1 ~ 5,
    y5MathScPar == 2 ~ 4,
    y5MathScPar == 3 ~ 3,
    y5MathScPar == 4 ~ 2,
    y5MathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5ReadScPar = case_when(
    y5ReadScPar == 1 ~ 5,
    y5ReadScPar == 2 ~ 4,
    y5ReadScPar == 3 ~ 3,
    y5ReadScPar == 4 ~ 2,
    y5ReadScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K',
  y5NumNation = y5MeanNumScore2010,
  y5ReadNation = y5MeanReadScore2010
  )

cDataAge10B <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrb10.sas7bdat") %>%
  select(cid = hicid, y5Grade = fpc06a1,y5State = fstate,y5Weight = fweight,
         y5Stratum = stratum, y5MathSc = fpc58b2, y5MathScPar = flc08a2a,
         y5SdqPar = fasdqtb, y5SdqTeach = ftsdqtb,
         y5ReadSc = fpc58b8, y5ReadScPar = flc08a1a,
         y5ReactivePar = fsatir, y5PersistPar = fsatip) %>% 
  mutate(y5MathSc = case_when(
    y5MathSc == 1 ~ 3,
    y5MathSc == 2 ~ 2,
    y5MathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5ReadSc = case_when(
    y5ReadSc == 1 ~ 3,
    y5ReadSc == 2 ~ 2,
    y5ReadSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5MathScPar = case_when(
    y5MathScPar == 1 ~ 5,
    y5MathScPar == 2 ~ 4,
    y5MathScPar == 3 ~ 3,
    y5MathScPar == 4 ~ 2,
    y5MathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5ReadScPar = case_when(
    y5ReadScPar == 1 ~ 5,
    y5ReadScPar == 2 ~ 4,
    y5ReadScPar == 3 ~ 3,
    y5ReadScPar == 4 ~ 2,
    y5ReadScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'B',
  y5NumNation = y5MeanNumScore2012,
  y5ReadNation = y5MeanReadScore2012
  )

cDataAge10 <- bind_rows(cDataAge10K, cDataAge10B)
# Merge Child Data ####
cData <- left_join(cDataAge4, cDataAge8, by = c("cid", "cohort") )

cData <- left_join(cData, cDataAge10, by = c("cid", "cohort") )

cData <- cData %>% 
  mutate(
    y3Grade = replace(y3Grade, y3Grade < 0, NA),
    y5Grade = replace(y5Grade, y5Grade < 0, NA),
    y3State = replace(y3State, y3State < 0, NA),
    y5State = replace(y5State, y5State < 0, NA),
    y5SdqTeach = replace(y5SdqTeach, y5SdqTeach < 0, NA)
  )

# Student Data ####
cScore <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/NAPLAN/lsacnaplan.sas7bdat") %>%
  select(cid = hicid, y3Num = y3num, y5Num = y5num, cohort,
         y3Read = y3read, y5Read = y5read,
         y3Status = y3status, y5Status = y5status) %>%
  mutate(y3Num = replace(y3Num, y3Num < 0, NA),
         y5Num = replace(y5Num, y5Num < 0, NA),
         y3Read = replace(y3Read, y3Read < 0, NA),
         y5Read = replace(y5Read, y5Read < 0, NA)
  )
# Merge Child and NAPLAN Data ####
cData <- left_join(cData, cScore, by = c("cid",'cohort'))
summary(cData)

tapply(cData$y5Num, cData$cohort, mean, na.rm=TRUE)
# School Data: 2008 ####
sData2008K <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2008 & HICID %in% cDataAge4K$cid) %>%
  select(cid = HICID, y3SId = School_ID, y3Sector = School_Sector_Code,
         y3NumSch = y3_N_SN_Mean_NAPLANScore, y3IndigSch = Indigenous_Student_Percent,
         y3SesSch = School_ICSEA, y3ReadSch = y3_R_SN_Mean_NAPLANScore) %>%
  mutate(cohort = 'K')

sData2008B <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2010 & HICID %in% cDataAge4B$cid) %>%
  select(cid = HICID, y3SId = School_ID, y3Sector = School_Sector_Code,
         y3NumSch = y3_N_SN_Mean_NAPLANScore, y3IndigSch = Indigenous_Student_Percent,
         y3SesSch = School_ICSEA, y3ReadSch = y3_R_SN_Mean_NAPLANScore)%>%
  mutate(cohort = 'B')

sData2008 <- bind_rows(sData2008K, sData2008B)
summary(sData2008)
# School Data: 2010 ####
sData2010K <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2010 & HICID %in% cDataAge4K$cid) %>%
  select(cid = HICID, y5SId = School_ID, y5Sector = School_Sector_Code,
         y5NumSch = y5_N_SN_Mean_NAPLANScore, y5IndigSch = Indigenous_Student_Percent,
         y5SesSch = School_ICSEA,
         y5ReadSch = y5_R_SN_Mean_NAPLANScore) %>%
  mutate(cohort = 'K')

sData2010B <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2012 & HICID %in% cDataAge4B$cid) %>%
  select(cid = HICID, y5SId = School_ID, y5Sector = School_Sector_Code,
         y5NumSch = y5_N_SN_Mean_NAPLANScore, y5IndigSch = Indigenous_Student_Percent,
         y5SesSch = School_ICSEA,
         y5ReadSch = y5_R_SN_Mean_NAPLANScore) %>%
  mutate(cohort = 'B')

sData2010 <- bind_rows(sData2010K, sData2010B)
summary(sData2010)
# School Data: 2012 ####
sData2012K <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2012 & HICID %in% cDataAge4K$cid) %>%
  select(cid = HICID, y7SId = School_ID, y7Sector = School_Sector_Code,
         y7NumSch = y7_N_SN_Mean_NAPLANScore, y7IndigSch = Indigenous_Student_Percent,
         y7SesSch = School_ICSEA) %>%
  mutate(cohort = 'K')

sData2012B <- readit("~/Dropbox/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2014 & HICID %in% cDataAge4B$cid) %>%
  select(cid = HICID, y7SId = School_ID, y7Sector = School_Sector_Code,
         y7NumSch = y7_N_SN_Mean_NAPLANScore, y7IndigSch = Indigenous_Student_Percent,
         y7SesSch = School_ICSEA) %>%
  mutate(cohort = 'B')

sData2012 <- bind_rows(sData2012K, sData2012B)
summary(sData2012)
# Merge Child and NAPLAN to MySchool
cData <- left_join(cData, sData2008, by = c("cid", 'cohort') )
cData <- left_join(cData, sData2010, by = c("cid", 'cohort') )

#summary(cData)
# Difference Score Creation ==================================
cData <- cData %>%
  mutate(
    y3DiffSch = y3Num - y3NumSch,
    y3DiffNat = y3Num - y3NumNation,
    y3Diff = y3DiffSch - y3DiffNat,          #END Year 3
    y5DiffSch = y5Num - y5NumSch,
    y5DiffNat = y5Num - y5NumNation,
    y5Diff = y5DiffSch - y5DiffNat,         #END Year 5
    y3DiffSchRead = y3Read - y3ReadSch,
    y3DiffNatRead = y3Read - y3ReadNation,
    y3DiffRead = y3DiffSchRead - y3DiffNatRead,          #END Year 3
    y5DiffSchRead = y5Read - y5ReadSch,
    y5DiffNatRead = y5Read - y5ReadNation,
    y5DiffRead = y5DiffSchRead - y5DiffNatRead         #END Year 5
  )

summary(cData)

sidNoY3 <- cData %>%
  group_by(y3SId) %>%
  summarize(y3SIdNo = n())

sidNoY5 <- cData %>%
  group_by(y5SId) %>%
  summarize(y5SIdNo = n())

cData <- cData %>% left_join(sidNoY3) %>% left_join(sidNoY5)

# Initial Plots by Sector -----------------------------------
cData %>% ggplot() +
  geom_histogram(aes(x=y3Diff,y=..density..), fill = cs[1],position="identity") + 
  geom_density(aes(x=y3Diff,y=..density..), colour = cs[2]) +
  theme_fivethirtyeight()

cData %>% ggplot() +
  geom_histogram(aes(x=y3DiffRead,y=..density..), fill = cs[1],position="identity") + 
  geom_density(aes(x=y3DiffRead,y=..density..), colour = cs[2]) +
  theme_fivethirtyeight()

#ggsave("figures/distortionYr3.pdf")

cData %>% ggplot() +
  geom_histogram(aes(x=y5Diff,y=..density..), fill = cs[1],position="identity") + 
  geom_density(aes(x=y5Diff,y=..density..), colour = cs[2]) +
  theme_fivethirtyeight()

cData %>% ggplot() +
  geom_histogram(aes(x=y5DiffRead,y=..density..), fill = cs[1],position="identity") + 
  geom_density(aes(x=y5DiffRead,y=..density..), colour = cs[2]) +
  theme_fivethirtyeight()

#ggsave("figures/distortionYr5.pdf")

# Yr 3
p1 <- cData %>%
  mutate(cohort = factor(cohort)) %>%
  filter(!is.na(cohort)) %>%
  ggplot(aes(x = ses, y = y3Diff, colour = cohort)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_fivethirtyeight() +
  scale_colour_manual(name="",  
                      values = c("K"=cs[2], "B" = cs[1])) +
  ggtitle("Year 3 Numeracy")

p2 <- cData %>%
  mutate(cohort = factor(cohort)) %>%
  filter(!is.na(cohort)) %>%
  ggplot(aes(x = ses, y = y3DiffRead, colour = cohort)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_fivethirtyeight() +
  scale_colour_manual(name="",  
                      values = c("K"=cs[2], "B" = cs[1])) +
  ggtitle("Year 5 Numeracy")


#ggsave("figures/distortionBySesYr3.pdf")


p3 <- cData %>%
  ggplot(aes(x = ses, y = y5Diff, colour = cohort)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_fivethirtyeight() +
  scale_colour_manual(name="",  
                      values = c("K"=cs[2], "B" = cs[1])) +
  ggtitle("Year 3 Reading")

p4 <- cData %>%
  ggplot(aes(x = ses, y = y5DiffRead, colour = cohort)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_fivethirtyeight() +
  scale_colour_manual(name="",  
                      values = c("K"=cs[2], "B" = cs[1])) +
  ggtitle("Year 5 Reading")

multiplot(p1,p2,p3,p4, cols = 2) 
#ggsave("figures/distortionBySesYr5.pdf")

cData <- cData[,-56:-67]


cData_long <- cData %>%
  filter(y3Grade == 19 & !is.na(y3Stratum) & !is.na(y3SId) & y3Status != 4) %>%
  select(y3SId,y5MathSc,y3MathSc,ses,y3Num,y3NumSch, y5Num, y5MathScPar,y3MathScPar,
         y3ReadSc, y5ReadSc, y5ReadScPar,y3ReadScPar, y3Read, y5Read,y3ReadSch,
         indig,geo,cohort,lang, y3Weight) %>%
  mutate_at(vars(geo:lang), .funs = factor)
  

glimpse(cData_long)

ini <- mice(cData_long,maxit=0)
pred1 <- ini$predictorMatrix
pred1[,c("y3SId","y3Weight")] <- 0

cData_long_imp <- mice(cData_long, method = "cart")
plot(cData_long_imp, c("y3MathSc", "y5Num", "y3MathScPar"))
densityplot(cData_long_imp, ~ y3MathSc+geo+y5Num+y3MathScPar)

library(brms)
fit_imp1 <- brm_multiple(y5Num ~ y3Num++geo+indig, data = cData_long_imp, chains = 2)
