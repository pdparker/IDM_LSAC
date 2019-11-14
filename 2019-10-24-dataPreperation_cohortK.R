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
cDataAge4 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk4.sas7bdat") %>%
  select(cid = hicid, ses = csep, geo = csos, lang = cf11m2,
         gender = zf02m1, indig1 =zf12m2, indig2 = zf12cm) %>%
  mutate(geo = ifelse(geo < 1, 'urban', 'rural'),
         lang = ifelse(lang == '1201', 'eng', 'other'),
         indig = case_when(
           indig1 < 0 ~ NA_character_,
           indig2 < 0 ~ NA_character_,
           indig1 > 1 ~ 'indig',
           indig2 > 1 ~ 'indig',
           TRUE ~ 'nonIndig'
         ) %>% factor,
         cohort = 'K',
         gender = ifelse(gender == 1, "Boy", "Girl")
  )
#  Age 8 ####
cDataAge8 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk8.sas7bdat") %>%
  select(cid = hicid, y3_grade = epc06a1,y3_state = estate,y3_weight = eweight,
         y3_stratum = stratum, y3_mathSc = epc58b2, y3_mathScPar = elc08a2a,
         y3_readSc = epc58b8, y3_readScPar = elc08a1a
  ) %>%
  mutate(y3_mathSc = case_when(
    y3_mathSc == 1 ~ 3,
    y3_mathSc == 2 ~ 2,
    y3_mathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3_readSc = case_when(
    y3_readSc == 1 ~ 3,
    y3_readSc == 2 ~ 2,
    y3_readSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3_mathScPar = case_when(
    y3_mathScPar == 1 ~ 5,
    y3_mathScPar == 2 ~ 4,
    y3_mathScPar == 3 ~ 3,
    y3_mathScPar == 4 ~ 2,
    y3_mathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y3_readScPar = case_when(
    y3_readScPar == 1 ~ 5,
    y3_readScPar == 2 ~ 4,
    y3_readScPar == 3 ~ 3,
    y3_readScPar == 4 ~ 2,
    y3_readScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K')

# Age 10 #####
cDataAge10 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk10.sas7bdat") %>%
  select(cid = hicid, y5_grade = fpc06a1,y5_state = fstate,y5_weight = fweight,
         y5_stratum = stratum, y5_mathSc = fpc58b2, y5_mathScPar = flc08a2a,
         y5_readSc = fpc58b8, y5_readScPar = flc08a1a) %>% 
  mutate(y5_mathSc = case_when(
    y5_mathSc == 1 ~ 3,
    y5_mathSc == 2 ~ 2,
    y5_mathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5_readSc = case_when(
    y5_readSc == 1 ~ 3,
    y5_readSc == 2 ~ 2,
    y5_readSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5_mathScPar = case_when(
    y5_mathScPar == 1 ~ 5,
    y5_mathScPar == 2 ~ 4,
    y5_mathScPar == 3 ~ 3,
    y5_mathScPar == 4 ~ 2,
    y5_mathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y5_readScPar = case_when(
    y5_readScPar == 1 ~ 5,
    y5_readScPar == 2 ~ 4,
    y5_readScPar == 3 ~ 3,
    y5_readScPar == 4 ~ 2,
    y5_readScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K'
  )

# Age 12 #####
cDataAge12 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/Confidentialised/SAS/lsacgrk12.sas7bdat") %>%
  select(cid = hicid, y7_grade = gpc06a1,y7_state = gstate,y7_weight = gweight,
         y7_stratum = stratum, y7_mathSc = gpc58b2, y7_mathScPar = glc08a2a,
         y7_readSc = gpc58b8, y7_readScPar = glc08a1a) %>% 
  mutate(y7_mathSc = case_when(
    y7_mathSc == 1 ~ 3,
    y7_mathSc == 2 ~ 2,
    y7_mathSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y7_readSc = case_when(
    y7_readSc == 1 ~ 3,
    y7_readSc == 2 ~ 2,
    y7_readSc == 3 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y7_mathScPar = case_when(
    y7_mathScPar == 1 ~ 5,
    y7_mathScPar == 2 ~ 4,
    y7_mathScPar == 3 ~ 3,
    y7_mathScPar == 4 ~ 2,
    y7_mathScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  y7_readScPar = case_when(
    y7_readScPar == 1 ~ 5,
    y7_readScPar == 2 ~ 4,
    y7_readScPar == 3 ~ 3,
    y7_readScPar == 4 ~ 2,
    y7_readScPar == 5 ~ 1,
    TRUE ~ NA_real_
  ) %>% ordered(),
  cohort = 'K'
  )


# Merge Child Data ####
cData <- left_join(cDataAge4, cDataAge8, by = c("cid", "cohort") )

cData <- left_join(cData, cDataAge10, by = c("cid", "cohort") ) %>%
  left_join(., cDataAge12, by = c("cid", "cohort") )

cData <- cData %>% 
  mutate(
    y3_grade = replace(y3_grade, y3_grade < 0, NA),
    y5_grade = replace(y5_grade, y5_grade < 0, NA),
    y7_grade = replace(y7_grade, y7_grade < 0, NA),
    y3_state = replace(y3_state, y3_state < 0, NA),
    y5_state = replace(y5_state, y5_state < 0, NA),
    y7_state = replace(y7_state, y7_state < 0, NA)
  )

# Student Data ####
cScore <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/NAPLAN/lsacnaplan.sas7bdat") %>%
  select(cid = hicid, y3_math = y3num, y5_math = y5num, y7_math = y7num, cohort,
         y3_read = y3read, y5_read = y5read,y7_read = y7read,
         y3_status = y3status, y5_status = y5status, y7_status = y7status) %>%
  mutate(y3_math = replace(y3_math, y3_math < 0, NA),
         y5_math = replace(y5_math, y5_math < 0, NA),
         y7_math = replace(y7_math, y7_math < 0, NA),
         y3_read = replace(y3_read, y3_read < 0, NA),
         y5_read = replace(y5_read, y5_read < 0, NA),
         y7_read = replace(y7_read, y7_read < 0, NA)
  )
# Merge Child and NAPLAN Data ####
cData <- left_join(cData, cScore, by = c("cid",'cohort'))
summary(cData)

tapply(cData$y5_math, cData$cohort, mean, na.rm=TRUE)
# School Data: 2008 ####
sData2008 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2008 & HICID %in% cDataAge4$cid) %>%
  select(cid = HICID, y3_sid = School_ID, y3_sector = School_Sector_Code,
         y3_mathSch = y3_N_SN_Mean_NAPLANScore, y3_indigSch = Indigenous_Student_Percent,
         y3_sesSch = School_ICSEA, y3_readSch = y3_R_SN_Mean_NAPLANScore) %>%
  mutate(cohort = 'K')
# School Data: 2010 ####
sData2010 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2010 & HICID %in% cDataAge4$cid) %>%
  select(cid = HICID, y5_sid = School_ID, y5_sector = School_Sector_Code,
         y5_mathSch = y5_N_SN_Mean_NAPLANScore, y5_indigSch = Indigenous_Student_Percent,
         y5_sesSch = School_ICSEA,
         y5_readSch = y5_R_SN_Mean_NAPLANScore) %>%
  mutate(cohort = 'K')
# School Data: 2012 ####
sData2012 <- readit("~/cloudstor/Databases/LSAC/Wave 6 GR CD/MySchool/lsac_myschool_gr.sas7bdat") %>%
  filter(calendar_year == 2012 & HICID %in% cDataAge4$cid) %>%
  select(cid = HICID, y7_sid = School_ID, y7_sector = School_Sector_Code,
         y7_mathSch = y7_N_SN_Mean_NAPLANScore, y7_indigSch = Indigenous_Student_Percent,
         y7_sesSch = School_ICSEA, y7_readSch = y7_R_SN_Mean_NAPLANScore) %>%
  mutate(cohort = 'K')
# Merge Child and NAPLAN to MySchool
cData <- left_join(cData, sData2008, by = c("cid", 'cohort') )
cData <- left_join(cData, sData2010, by = c("cid", 'cohort') )
cData <- left_join(cData, sData2012, by = c("cid", 'cohort') )

#summary(cData)
# Difference Score Creation ==================================
# cData <- cData %>%
#   mutate(
#     y3DiffSch = y3Num - y3NumSch,
#     y3DiffNat = y3Num - y3NumNation,
#     y3Diff = y3DiffSch - y3DiffNat,          #END Year 3
#     y5DiffSch = y5Num - y5NumSch,
#     y5DiffNat = y5Num - y5NumNation,
#     y5Diff = y5DiffSch - y5DiffNat,         #END Year 5
#     y3DiffSchRead = y3Read - y3ReadSch,
#     y3DiffNatRead = y3Read - y3ReadNation,
#     y3DiffRead = y3DiffSchRead - y3DiffNatRead,          #END Year 3
#     y5DiffSchRead = y5Read - y5ReadSch,
#     y5DiffNatRead = y5Read - y5ReadNation,
#     y5DiffRead = y5DiffSchRead - y5DiffNatRead         #END Year 5
#   )
# 
# summary(cData)

sidNoY3 <- cData %>%
  group_by(y3_sid) %>%
  summarize(y3_sidNo = n())

sidNoY5 <- cData %>%
  group_by(y5_sid) %>%
  summarize(y5_sidNo = n())

sidNoY7 <- cData %>%
  group_by(y7_sid) %>%
  summarize(y5_sidNo = n())


cData <- cData %>% left_join(sidNoY3) %>% left_join(sidNoY5)  %>% left_join(sidNoY7)

# Initial Plots by Sector -----------------------------------
# cData %>% ggplot() +
#   geom_histogram(aes(x=y3Diff,y=..density..), fill = cs[1],position="identity") + 
#   geom_density(aes(x=y3Diff,y=..density..), colour = cs[2]) +
#   theme_fivethirtyeight()
# 
# cData %>% ggplot() +
#   geom_histogram(aes(x=y3DiffRead,y=..density..), fill = cs[1],position="identity") + 
#   geom_density(aes(x=y3DiffRead,y=..density..), colour = cs[2]) +
#   theme_fivethirtyeight()
# 
# #ggsave("figures/distortionYr3.pdf")
# 
# cData %>% ggplot() +
#   geom_histogram(aes(x=y5Diff,y=..density..), fill = cs[1],position="identity") + 
#   geom_density(aes(x=y5Diff,y=..density..), colour = cs[2]) +
#   theme_fivethirtyeight()
# 
# cData %>% ggplot() +
#   geom_histogram(aes(x=y5DiffRead,y=..density..), fill = cs[1],position="identity") + 
#   geom_density(aes(x=y5DiffRead,y=..density..), colour = cs[2]) +
#   theme_fivethirtyeight()
# 
# #ggsave("figures/distortionYr5.pdf")

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

z <- function(x) (x-mean(x,na.rm=TRUE))/sd(x, na.rm=TRUE)
cData_long <- cData %>%
  filter(y7_grade == 25 & y7_status != 4) %>%
  select(y3_sid,y5_sid,y7_sid, ses, indig,geo,cohort,lang, gender,
         y3_weight,y5_weight,y7_weight,
         y3_mathSc,y5_mathSc,y7_mathSc,
         y3_readSc,y5_readSc,y7_readSc,
         y3_mathScPar,y5_mathScPar,y7_mathScPar,
         y3_readScPar,y5_readScPar,y7_readScPar,
         y3_math,y5_math,y7_math, y3_read,y5_read,y7_read,
         y3_mathSch,y5_mathSch,y7_mathSch, y3_readSch,y5_readSch,y7_readSch
         ) %>%
  mutate_at(vars(geo:lang), .funs = factor) %>%
  mutate_at(vars(y3_math:y7_readSch), .funs = z)
  

glimpse(cData_long)

ini <- mice(cData_long,maxit=0)
pred1 <- ini$predictorMatrix
pred1[,c("y3_sid","y3_weight","y5_sid","y5_weight","y7_sid","y7_weight")] <- 0

cData_long_imp <- mice(cData_long, method = "cart",maxit = 15)
cData_long_imp %>% summary
plot(cData_long_imp, c("y3_mathSc", "y5_math", "y3_mathScPar"))
densityplot(cData_long_imp, ~ y3_mathSc+y7_math)

save(list = c("cData_long_imp", "cData_long"),file = "data/2019-10-25_data.RData")

