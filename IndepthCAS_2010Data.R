# Add contents - list of sections (getting data in and creating scales; descriptives; correlation; regression etc)

setwd("C:/Users/jckenned/Documents/Research/EPL Project/DataFiles")  # Working directory containing R files.

library(dplyr)   # Used for joining new variables to existing dataframe and other manipulations.
library(sjPlot)  # Used for reading in spss files, viewing them, etc.
library(psych)   # Used for creating scales and calculating reliability and other descriptives.
library(car)     # Used for the recode function.
library(haven)   # needed for following read_spss function.
library(sjmisc)
library(ggplot2)

# Only need to use following for first time reading spss data into R. Can use 'load' subsequently.
casedata <- sjmisc::read_spss("C:/Users/jckenned/Documents/Research/EPL Project/Values&EPL paper/DataFiles/Files from CAS zip file/02_CAS_Indepth2010_Phase1_n272_ItemsNotScales.sav")

# NOTE: See http://www.strengejacke.de/sjPlot/datainit/. Can also specify use of 'foreign' though this
# also requires specifying additional options to get value labels in.

# probably don't need these - can just read in the spss file each time.

# save(casedata, file="C:/Users/jckenned/Documents/Research/EPL Project/DataFiles/02_CAS_Indepth2010_Phase1_n272_ItemsNotScales.Rdata")
# load("C:/Users/jckenned/Documents/Research/EPL Project/DataFiles/02_CAS_Indepth2010_Phase1_n272_ItemsNotScales.Rdata")

# names(casedata)  # lists all the variable names with column numbers, so can reference by number.
# Use Indepth_ID to label the rows:
row.names(casedata) <- casedata$Indepth_ID

# Reverse code items (uses recode function from car package)
casedata$MOT25_PNCr <- recode(casedata$MOT25_PNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1 ; else=NA")
casedata$MOT55_LNCr <- recode(casedata$MOT55_LNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata$MOT48_LAIr <- recode(casedata$MOT48_LAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata$INT21_PIr <- recode(casedata$INT21_PI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata$INT6_LIr <- recode(casedata$INT6_LI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")

# Set up keys list for scoring the scales (http://www.personality-project.org/r/html/score.items.html).
keys.list <- list(Indepth_ID=c("Indepth_ID"),
                  MOT9IT_ENT=c("MOT1_EAI","MOT10_EAI","MOT18_EAI","MOT5_ENC","MOT8_ENC","MOT9_ENC","MOT6_ESN",
                               "MOT15_ESN","MOT16_ESN"),
                  MOT9IT_PROF=c("MOT28_PAI","MOT38_PAI","MOT39_PAI","MOT25_PNCr","MOT27_PNC","MOT40_PNC","MOT32_PSN",
                                "MOT41_PSN","MOT42_PSN"),
                  MOT9IT_LDRMGR=c("MOT48_LAIr","MOT61_LAI","MOT62_LAI","MOT53_LNC","MOT55_LNCr","MOT60_LNC",
                                  "MOT46_LSN","MOT57_LSN","MOT63_LSN"),
                  MEAN_EAI3IT=c("MOT1_EAI","MOT10_EAI","MOT18_EAI"),
                  MEAN_ECL3IT=c("MOT5_ENC","MOT8_ENC","MOT9_ENC"),
                  MEAN_ESN3IT=c("MOT6_ESN","MOT15_ESN","MOT16_ESN"),
                  MEAN_PAI3IT=c("MOT28_PAI","MOT38_PAI","MOT39_PAI"),
                  MEAN_PCL3IT=c("MOT25_PNCr","MOT27_PNC","MOT40_PNC"),
                  MEAN_PSN3IT=c("MOT32_PSN","MOT41_PSN","MOT42_PSN"),
                  MEAN_LAI3IT=c("MOT48_LAIr","MOT61_LAI","MOT62_LAI"),
                  MEAN_LNC3IT=c("MOT53_LNC","MOT55_LNCr","MOT60_LNC"),
                  MEAN_LSN3IT=c("MOT46_LSN","MOT57_LSN","MOT63_LSN"),
				  EFF7IT_ENT=c("EFF1_Ent","EFF2_Ent","EFF3_Ent","EFF4_Ent","EFF5_Ent","EFF6_Ent","EFF7_Ent"),
                  EFF5IT_PROF=c("EFF19_Prof","EFF20_Prof","EFF21_Prof","EFF22_Prof","EFF23_Prof"),
				  EFF7IT_LDR=c("EFF12_LMgt","EFF13_LMgt","EFF14_LMgt","EFF15_LMgt","EFF16_LMgt","EFF17_LMgt","EFF18_LMgt"),
				  INTENT_EI2IT=c("INT17_EI","INT2_EI"),
				  INTENT_PI4IT=c("INT1_PI","INT16_PI","INT9_PI","INT21_PIr"),
				  INTENT_LI3IT=c("INT3_LI","INT11_LI","INT6_LIr"),
				  COLLIND6IT=c("COLIND3","COLIND6","COLIND9","COLIND12","COLIND15","COLIND17"),
				  UA5IT=c("UA2","UA5","UA8","UA11","UA14"),
				  PD5IT=c("PD1","PD7","PD10","PD13","PD16"),
				  SVS_MRAT=c("value1", "value2", "value3", "value4", "value5", "value6", "value7", "value8",
				             "value9", "value10", "value11", "value12", "value13", "value14", "value15", "value16",
				             "value17", "value18", "value19", "value20", "value21", "value22", "value23", "value24",
				             "value25", "value26", "value27", "value28", "value29", "value30", "value31", "value32",
				             "value33", "value34", "value35", "value36", "value37", "value38", "value39", "value40",
				             "value41", "value42", "value43", "value44", "value45", "value46", "value47", "value48",
				             "value49", "value50", "value51", "value52", "value53", "value54", "value55", "value56", "value57"))

# Create itemsUsed vector by copying the keys.list input above and pasting. Can then subset dataframe and use select to 
# choose only the columns required for scale calcs (& an ID column for subsequent joining back to dataframe).
# Or, could explore how to pullout the new scale names from keys.list and combine, c(MEAN_EAI3IT, MEAN_ECL3IT, etc).

itemsUsed <- c("Indepth_ID",
               "MOT1_EAI","MOT10_EAI","MOT18_EAI","MOT5_ENC","MOT8_ENC","MOT9_ENC","MOT6_ESN","MOT15_ESN","MOT16_ESN",
               "MOT28_PAI","MOT38_PAI","MOT39_PAI","MOT25_PNCr","MOT27_PNC","MOT40_PNC","MOT32_PSN","MOT41_PSN","MOT42_PSN",
               "MOT48_LAIr","MOT61_LAI","MOT62_LAI","MOT53_LNC","MOT55_LNCr","MOT60_LNC","MOT46_LSN","MOT57_LSN","MOT63_LSN",
               "EFF1_Ent","EFF2_Ent","EFF3_Ent","EFF4_Ent","EFF5_Ent","EFF6_Ent","EFF7_Ent",
               "EFF19_Prof","EFF20_Prof","EFF21_Prof","EFF22_Prof","EFF23_Prof",
               "EFF12_LMgt","EFF13_LMgt","EFF14_LMgt","EFF15_LMgt","EFF16_LMgt","EFF17_LMgt","EFF18_LMgt",
               "INT17_EI","INT2_EI",
               "INT1_PI","INT16_PI","INT9_PI","INT21_PIr",
               "INT3_LI","INT11_LI","INT6_LIr",
               "COLIND3","COLIND6","COLIND9","COLIND12","COLIND15","COLIND17",
               "UA2","UA5","UA8","UA11","UA14",
               "PD1","PD7","PD10","PD13","PD16",
               "value1", "value2", "value3", "value4", "value5", "value6", "value7", "value8", "value9", "value10",
               "value11", "value12", "value13", "value14", "value15", "value16", "value17", "value18", "value19",
               "value20", "value21", "value22", "value23", "value24", "value25", "value26", "value27", "value28",
               "value29", "value30", "value31", "value32", "value33", "value34", "value35", "value36", "value37",
               "value38", "value39", "value40", "value41", "value42", "value43", "value44", "value45", "value46",
               "value47", "value48", "value49", "value50", "value51", "value52", "value53", "value54", "value55",
               "value56", "value57")
subsetCasedata <- casedata %>% select(one_of(itemsUsed))
keys <- make.keys(subsetCasedata, keys.list)
scores.MotEffIndCol <- scoreItems(keys, subsetCasedata, missing=TRUE, impute="median", digits = 3)
# add the new scale scores to casedata using dplyr::left_join, matching by ID.
casedata <- dplyr::left_join(casedata, as.data.frame(scores.MotEffIndCol$scores), by = "Indepth_ID")
# View(casedata) # Shows dataframe in nice format (part of dplyr), though not all vars shown (truncated).

# Compute the new Schwarz value scores by subtracting individual means and saving to dataframe.
# Used 'transform' when first writing this, but dplyr::mutate is probably a better choice. Mutate allows
# subsequent use of a variable created in an earlier step.
casedata <- transform(casedata,
			SVS_MRAT1 = value1  - SVS_MRAT,
			SVS_MRAT2 = value2  - SVS_MRAT,
            SVS_MRAT3 = value3  - SVS_MRAT,
            SVS_MRAT4 = value4  - SVS_MRAT,
            SVS_MRAT5 = value5  - SVS_MRAT,
            SVS_MRAT6 = value6  - SVS_MRAT,
            SVS_MRAT7 = value7  - SVS_MRAT,
            SVS_MRAT8 = value8  - SVS_MRAT,
            SVS_MRAT9 = value9  - SVS_MRAT,
            SVS_MRAT10 = value10  - SVS_MRAT,
            SVS_MRAT11 = value11  - SVS_MRAT,
            SVS_MRAT12 = value12  - SVS_MRAT,
            SVS_MRAT13 = value13  - SVS_MRAT,
            SVS_MRAT14 = value14  - SVS_MRAT,
            SVS_MRAT15 = value15  - SVS_MRAT,
            SVS_MRAT16 = value16  - SVS_MRAT,
            SVS_MRAT17 = value17  - SVS_MRAT,
            SVS_MRAT18 = value18  - SVS_MRAT,
            SVS_MRAT19 = value19  - SVS_MRAT,
            SVS_MRAT20 = value20  - SVS_MRAT,
            SVS_MRAT21 = value21  - SVS_MRAT,
            SVS_MRAT22 = value22  - SVS_MRAT,
            SVS_MRAT23 = value23  - SVS_MRAT,
            SVS_MRAT24 = value24  - SVS_MRAT,
            SVS_MRAT25 = value25  - SVS_MRAT,
            SVS_MRAT26 = value26  - SVS_MRAT,
            SVS_MRAT27 = value27  - SVS_MRAT,
            SVS_MRAT28 = value28  - SVS_MRAT,
            SVS_MRAT29 = value29  - SVS_MRAT,
            SVS_MRAT30 = value30  - SVS_MRAT,
            SVS_MRAT31 = value31  - SVS_MRAT,
            SVS_MRAT32 = value32  - SVS_MRAT,
            SVS_MRAT33 = value33  - SVS_MRAT,
            SVS_MRAT34 = value34  - SVS_MRAT,
            SVS_MRAT35 = value35  - SVS_MRAT,
            SVS_MRAT36 = value36  - SVS_MRAT,
            SVS_MRAT37 = value37  - SVS_MRAT,
            SVS_MRAT38 = value38  - SVS_MRAT,
            SVS_MRAT39 = value39  - SVS_MRAT,
            SVS_MRAT40 = value40  - SVS_MRAT,
            SVS_MRAT41 = value41  - SVS_MRAT,
            SVS_MRAT42 = value42  - SVS_MRAT,
            SVS_MRAT43 = value43  - SVS_MRAT,
            SVS_MRAT44 = value44  - SVS_MRAT,
            SVS_MRAT45 = value45  - SVS_MRAT,
            SVS_MRAT46 = value46  - SVS_MRAT,
            SVS_MRAT47 = value47  - SVS_MRAT,
            SVS_MRAT48 = value48  - SVS_MRAT,
            SVS_MRAT49 = value49  - SVS_MRAT,
            SVS_MRAT50 = value50  - SVS_MRAT,
            SVS_MRAT51 = value51  - SVS_MRAT,
            SVS_MRAT52 = value52  - SVS_MRAT,
            SVS_MRAT53 = value53  - SVS_MRAT,
            SVS_MRAT54 = value54  - SVS_MRAT,
            SVS_MRAT55 = value55  - SVS_MRAT,
            SVS_MRAT56 = value56  - SVS_MRAT,
            SVS_MRAT57 = value57  - SVS_MRAT)

keys.list <- list(Indepth_ID=c("Indepth_ID"),
                  SVSMRAT9_POWER=c("SVS_MRAT3","SVS_MRAT12","SVS_MRAT27","SVS_MRAT46"),
				  SVSMRAT8_ACHIEVEMENT=c("SVS_MRAT34","SVS_MRAT39","SVS_MRAT43","SVS_MRAT55"),
				  SVSMRAT7_HEDONISM=c("SVS_MRAT4","SVS_MRAT50","SVS_MRAT57"),
				  SVSMRAT6_STIMULATION=c("SVS_MRAT9","SVS_MRAT25","SVS_MRAT37"),
				  SVSMRAT5_SELFDIRECTION=c("SVS_MRAT5","SVS_MRAT16","SVS_MRAT31","SVS_MRAT41","SVS_MRAT53"),
				  SVSMRAT4_UNIVERSALISM=c("SVS_MRAT1","SVS_MRAT17","SVS_MRAT24","SVS_MRAT26","SVS_MRAT29",
				                          "SVS_MRAT30","SVS_MRAT35","SVS_MRAT38"),
				  SVSMRAT3_BENEVOLENCE=c("SVS_MRAT33","SVS_MRAT45","SVS_MRAT49","SVS_MRAT52","SVS_MRAT54"),
				  SVSMRAT2_TRADITION=c("SVS_MRAT18","SVS_MRAT32","SVS_MRAT36","SVS_MRAT44","SVS_MRAT51"),
				  SVSMRAT1_CONFORMITY=c("SVS_MRAT11","SVS_MRAT20","SVS_MRAT40","SVS_MRAT47"),
				  SVSMRAT10_SECURITY=c("SVS_MRAT8","SVS_MRAT13","SVS_MRAT15","SVS_MRAT22","SVS_MRAT56"))
itemsUsed <- c("Indepth_ID",
               "SVS_MRAT3","SVS_MRAT12","SVS_MRAT27","SVS_MRAT46",
               "SVS_MRAT34","SVS_MRAT39","SVS_MRAT43","SVS_MRAT55",
			   "SVS_MRAT4","SVS_MRAT50","SVS_MRAT57",
			   "SVS_MRAT9","SVS_MRAT25","SVS_MRAT37",
			   "SVS_MRAT5","SVS_MRAT16","SVS_MRAT31","SVS_MRAT41","SVS_MRAT53",
			   "SVS_MRAT1","SVS_MRAT17","SVS_MRAT24","SVS_MRAT26","SVS_MRAT29","SVS_MRAT30","SVS_MRAT35","SVS_MRAT38",
			   "SVS_MRAT33","SVS_MRAT45","SVS_MRAT49","SVS_MRAT52","SVS_MRAT54",
			   "SVS_MRAT18","SVS_MRAT32","SVS_MRAT36","SVS_MRAT44","SVS_MRAT51",
			   "SVS_MRAT11","SVS_MRAT20","SVS_MRAT40","SVS_MRAT47",
			   "SVS_MRAT8","SVS_MRAT13","SVS_MRAT15","SVS_MRAT22","SVS_MRAT56")
subsetCasedata <- casedata %>% select(one_of(itemsUsed))
keys <- make.keys(subsetCasedata, keys.list)
scores.Schwarz10 <- scoreItems(keys, subsetCasedata, missing=TRUE, impute="median", digits = 3)
# add the new scale scores to casedata using plyr::left_join, matching by ID.
casedata <- dplyr::left_join(casedata, as.data.frame(scores.Schwarz10$scores), by = "Indepth_ID")

# Create the four higher order groups (underlying dimensions) of Schwartz values.
keys.list <- list(Indepth_ID=c("Indepth_ID"),
                  Open2Change=c("SVSMRAT5_SELFDIRECTION","SVSMRAT6_STIMULATION"),
                  SelfEnhance=c("SVSMRAT7_HEDONISM", "SVSMRAT8_ACHIEVEMENT", "SVSMRAT9_POWER"),
                  Conservation=c("SVSMRAT10_SECURITY", "SVSMRAT1_CONFORMITY", "SVSMRAT2_TRADITION"),
                  SelfTransend=c("SVSMRAT4_UNIVERSALISM", "SVSMRAT4_UNIVERSALISM"))
itemsUsed <- c("Indepth_ID",
               "SVSMRAT5_SELFDIRECTION","SVSMRAT6_STIMULATION",
               "SVSMRAT7_HEDONISM", "SVSMRAT8_ACHIEVEMENT", "SVSMRAT9_POWER",
               "SVSMRAT10_SECURITY", "SVSMRAT1_CONFORMITY", "SVSMRAT2_TRADITION",
               "SVSMRAT4_UNIVERSALISM", "SVSMRAT4_UNIVERSALISM")
subsetCasedata <- casedata %>% select(one_of(itemsUsed))
keys <- make.keys(subsetCasedata, keys.list)
scores.SchwarzDims <- scoreItems(keys, subsetCasedata, missing=TRUE, impute="median", digits = 3)
# add the new scale scores to casedata using plyr::left_join, matching by ID.
casedata <- dplyr::left_join(casedata, as.data.frame(scores.SchwarzDims$scores), by = "Indepth_ID")

view_df(casedata, showFreq = TRUE, showPerc = TRUE) #appends freq and % for values.

################################################################################
#
# Run preceding code to get all scale scores into casedata. Following syntax
# can be run as needed to explore and analyse.
#
################################################################################

################## MEAN CENTRED SCALE RELIABILITIES, ALPHA ETC #################
#
# use scores.MotEffIndCol$scores to get scale scores, $alpha for alpha, $item.cor for item-total correln etc.
# Use names(scores.MotEffIndCol) to find out range of info produced.
# sim for scores.Schwarz10
# Compared output with spss, and alpha values, scale means, etc all matched.
# Uncomment and run following lines as necessary:

scores.MotEffIndCol$alpha
# scores.MotEffIndCol$cor
# scores.MotEffIndCol$item.cor
scores.Schwarz10$alpha
# as.data.frame(scores.MotEffIndCol$alpha) %>% select(starts_with("MOT"))
# as.data.frame(scores.MotEffIndCol$cor) %>% select(starts_with("MOT"))
# as.data.frame(scores.MotEffIndCol$missing) %>% select(starts_with("SVS"))

################## RAW SCALE RELIABILITIES ####################################
#
# Psych package has alpha, for checking scale reliability. alpha
# Create dataframe subsets of uncentred vars for each scale, and check alpha.
raw_POWER.df <- casedata %>% 
    select(one_of(c("value3","value12","value27","value46")))
raw_ACHIEVEMENT.df <- casedata %>% 
    select(one_of(c("value34","value39","value43","value55")))
raw_HEDONISM.df <- casedata %>% 
    select(one_of(c("value4","value50","value57")))
raw_STIMULATION.df <- casedata %>% 
    select(one_of(c("value9","value25","value37")))
raw_SELFDIRECTION.df <- casedata %>% 
    select(one_of(c("value5","value16","value31","value41","value53")))
raw_UNIVERSALISM.df <- casedata %>% 
    select(one_of(c("value1","value17","value24","value26","value29","value30","value35","value38")))
raw_BENEVOLENCE.df <- casedata %>% 
    select(one_of(c("value33","value45","value49","value52","value54")))
raw_TRADITION.df <- casedata %>% 
    select(one_of(c("value18","value32","value36","value44","value51")))
raw_CONFORMITY.df <- casedata %>% 
    select(one_of(c("value11","value20","value40","value47")))
raw_SECURITY.df <- casedata %>% 
    select(one_of(c("value8","value13","value15","value22","value56")))

alpha_POWER <- alpha(raw_POWER.df) # alpha = .64
alpha_ACHIEVEMENT <- alpha(raw_ACHIEVEMENT.df) # alpha = .64
alpha_HEDONISM <- alpha(raw_HEDONISM.df) # alpha = .70
alpha_STIMULATION <- alpha(raw_STIMULATION.df) # alpha = .75
alpha_SELFDIRECTION <- alpha(raw_SELFDIRECTION.df) # alpha = .68
alpha_UNIVERSALISM <- alpha(raw_UNIVERSALISM.df) # alpha = .8
alpha_BENEVOLENCE <- alpha(raw_BENEVOLENCE.df) # alpha = .8
alpha_TRADITION <- alpha(raw_TRADITION.df) # alpha = .63
alpha_CONFORMITY <- alpha(raw_CONFORMITY.df) # alpha = .67
alpha_SECURITY <- alpha(raw_SECURITY.df) # alpha = .67
# summary(alpha_SECURITY) gives alpha, G6, average interitem correlation.
# See names(alpha_SECURITY) for other info - e.g., stats with one item removed etc.

################## DESCRIPTIVES #################################################
#
# Can use 'describe' from psych package to have a quick look at n, mean, sd, median, min, max,
# skew, kurtosis, for subsets of variables. using 'fast = TRUE' limits output to main summary stats. E.g.:
# casedata %>% 
#     select(starts_with("INT")) %>% 
#     describe(fast = TRUE)
# casedata %>% 
#     select(starts_with("EFF")) %>% 
#     describe(fast = TRUE)
# casedata %>% 
#     select(starts_with("MOT")) %>% 
#     describe(fast = TRUE)
# casedata %>% 
#     select(starts_with("SVS_MRAT")) %>% 
#     describe(fast = TRUE)
# casedata %>% 
#     select(starts_with("value")) %>% 
#     describe(fast = TRUE)
# casedata %>% 
#     select(PD1:COLIND17) %>% 
#     describe(fast = TRUE)
# casedata  %>% 
#     select(starts_with("MOT9IT")) %>% 
#     describe(fast=TRUE)
# casedata %>% 
#     select(one_of(c("Open2Change", "SelfEnhance", "Conservation", "SelfTransend"))) %>% 
#     describe(fast=TRUE)

################## CORRELATIONS #################################################
#
correlations <- casedata %>% 
    select(starts_with("SVSMRAT"), starts_with("MOT9IT"), starts_with("INTENT"), one_of(c("EFF7IT_ENT","EFF5IT_PROF","EFF7IT_LDR"))) %>% 
    corr.test()
# correlations$r = values; also have n, t, p, se, adjust, sym, ci.
# Following creates vectors of variable names so only get correlations we're interested in (as opposed to full square
# matrix of above syntax):
SVSMRAT <- casedata %>% 
    select(starts_with("SVSMRAT"))
SVS2ndOrder <- casedata %>% 
    select(one_of(c("Open2Change", "SelfEnhance", "Conservation", "SelfTransend")))
UAINDCOL <- casedata %>% 
    select(one_of(c("COLLIND6IT", "UA5IT", "PD5IT")))
MotIntEff <- casedata %>% 
    select(starts_with("MOT9IT"), starts_with("INTENT"), one_of(c("EFF7IT_ENT","EFF5IT_PROF","EFF7IT_LDR")))
MOT <- casedata %>% 
    select(starts_with("MOT9IT"))
AgeGender <- casedata %>% 
    select(one_of(c("AGEASAT14JAN2011","gencode")))
# Could look at Affective Identify mots; AI motn found to be strong predictor of EPL career intentions
# in Chan et al EAWOP paper 2013. 
AffIdent <- casedata %>% 
    dplyr::select(one_of(c("MEAN_EAI3IT", "MEAN_PAI3IT", "MEAN_LAI3IT")))
SocNorm <- casedata %>% 
    dplyr::select(one_of(c("MEAN_ESN3IT", "MEAN_PSN3IT", "MEAN_LSN3IT")))
Calc <- casedata %>% 
    dplyr::select(one_of(c("MEAN_ECL3IT", "MEAN_PCL3IT", "MEAN_LNC3IT")))
# sink("correlations2.txt", append = TRUE)
# corr.test(SVSMRAT, AffIdent, adjust = "none")
# corr.test(SVSMRAT, SocNorm, adjust = "none")
# corr.test(SVSMRAT, Calc, adjust = "none")
# corr.test(UAINDCOL, AffIdent, adjust = "none")
# corr.test(UAINDCOL, SocNorm, adjust = "none")
# corr.test(UAINDCOL, Calc, adjust = "none")
# corr.test(UAINDCOL, MOT, adjust = "none")
# sink() # To return output to screen.

# To send the correlations to a text file then can use sink() command:
# sink("correlations.txt", append = TRUE)
# corr.test(UAINDCOL, SVSMRAT, adjust = "none")
# corr.test(UAINDCOL, MotIntEff, adjust = "none")
# corr.test(SVSMRAT, adjust = "none")
# corr.test(MOT, adjust = "none")
# corr.test(SVSMRAT, MotIntEff, adjust = "none")
# corr.test(SVSMRAT, MOT, adjust = "none")
# corr.test(SVSMRAT, AgeGender, adjust = "none")
# corr.test(MotIntEff, AgeGender, adjust = "none")
# corr.test(AgeGender, adjust = "none")

# sink() # To return output to screen.

# Examples of correlation plots:
# as.data.frame(scores.MotEffIndCol$scores) %>% 
#     select(starts_with("MOT")) %>% 
#     sjp.corr()
# 
# as.data.frame(scores.MotEffIndCol$scores) %>% 
#     select(contains("IT")) %>% 
#     sjp.corr()
# 
# as.data.frame(scores.MotEffIndCol$scores) %>% 
#     sjp.corr()

# Check factor structure of Dorfman's work values.
# NOTE: PD4 is poor item, not included in scale calculation so take care to drop it
# from correlations, reliability, factor analysis etc.
DorfmanItems <- casedata %>% 
    select(PD1:COLIND17) %>% select(-PD4)
head(DorfmanItems)

# fa.Dorfman <- fa.parallel(DorfmanItems)
# fa.Dorfman$nfact
# Because full response range was not used on all Dorfman items, need to use
# global=FALSE.
fa.Dorfman <- fa.parallel(DorfmanItems, cor="poly", use = "listwise", global=FALSE)

library(lavaan)
# p. 33 of lavaan tutorial describes info which can be obtained for fitted model.

DorfmanModel <- '
                UA =~ UA2 + UA5 + UA8 + UA11 + UA14
                PD =~ PD1 + PD7 + PD10 + PD13 + PD16
                COLIND =~ COLIND3 + COLIND6 + COLIND9 + COLIND12 + COLIND15 + COLIND17 '
Dorfman.fit <- cfa(DorfmanModel, data = casedata)
summary(Dorfman.fit, fit.measures = TRUE)
parameterEstimates(Dorfman.fit)
standardizedSolution(Dorfman.fit)
residuals(Dorfman.fit, type = "standardized")
fitMeasures(Dorfman.fit)  # Gives all fit measures. Use fitMeasures(Dorfman.fit, "cfi") to get just cfi.
# For multiple fit measures, provide a vector:
# fitMeasures(Dorfman.fit, c("cfi", "rmsea", "srmr"))
inspect(Dorfman.fit)

#################################################
# Dorfman reliabilities (Cronbach and ordinal)
#################################################

DorfmanPD <- dplyr::select(DorfmanItems, starts_with("PD"))
DorfmanUA <- dplyr::select(DorfmanItems, starts_with("UA"))
DorfmanCOLIND <- dplyr::select(DorfmanItems, starts_with("COLIND"))
alpha(DorfmanPD)         #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
alpha(DorfmanUA)         #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
alpha(DorfmanCOLIND)     #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
poly_PD <- polychoric(DorfmanPD) # saves polychoric corr matrix and tau values
alpha(poly_PD$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.
poly_UA <- polychoric(DorfmanUA) # saves polychoric corr matrix and tau values
alpha(poly_UA$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.
poly_COLIND <- polychoric(DorfmanCOLIND) # saves polychoric corr matrix and tau values
alpha(poly_COLIND$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.


myfit <- lm(MOT9IT_ENT ~ SVSMRAT8_ACHIEVEMENT + 
                SVSMRAT6_STIMULATION + SVSMRAT5_SELFDIRECTION + 
                SVSMRAT1_CONFORMITY + SVSMRAT10_SECURITY, casedata)
summary(myfit)
qplot(SVSMRAT6_STIMULATION, MOT9IT_ENT, data = casedata)
                

###############################################################
# Ordinal reliability for Likert-type items. EN 4990
# Gadermann, Guhn, & Zumbo (2012)
###############################################################

# SYNTAX FROM ARTICLE USING SAMPLE DATASET FROM PSYCH PACKAGE
# Following includes commands enabling comparison with alpha based on Pearson correlations. For
# ordinal correlations, only need the [examplename <- polychoric(bfi5items)] command and the 
# [alpha(examplename$rho)] command.
data(bfi)
attach(bfi)
bfi5items <- data.frame(N1, N2, N3, N4, N5)
describe(bfi5items)
bfi5items
cor(bfi5items, y=NULL, use="complete.obs", method=c("pearson")) # Pearson corr, only cases with complete data
cov(bfi5items, y=NULL, use="complete.obs", method=c("pearson")) # Pearson covar matrix, cases with complete data
skew(bfi5items)          # skew
kurtosi(bfi5items)       # kurtosis
scree(bfi5items)         # scree plot of eigenvalues for factor analysis and pca
examplename <- polychoric(bfi5items) # saves polychoric corr matrix and tau values in examplename
alpha(examplename$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.
alpha(bfi5items)         #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices. 
guttman(examplename$rho) # alternative est of reliability - here, based on polychoric therefore ordinal estimates.
omega(examplename$rho)   #ordinal versions of reliability coefficients omega




###############################################################
#
# Playing around to find out how to create a new variable which identifies the 
# highest value across the three MOT9IT vars - idea is to categorise each case as being 
# strongest in either E, P, or L motivation. Can then try using discriminant analysis. Though
# looking cursorily at data, it seems there are a lot of ties between P and L. Using 
# which.max will pick first max, so will overweight the existence of P. So, one possibility
# might be to collapse P and L into one category to see if it can be discriminated
# from E.
# Also there is the problem that very few cases have E as their maximum score on motivation scales.
# add a column to casedata which indicates highest motivation (E,P,L) - though for ties, it lists first.
casedata <- casedata %>%  mutate(maxMOT = colnames(casedata[148:150])[apply(casedata[148:150],1,which.max)])
casedata <- casedata %>%  mutate(maxINT = colnames(casedata[154:156])[apply(casedata[154:156],1,which.max)])

# To count the number of each category, use table:
# a <- table(casedata$maxMOT) # then type 'a' to see result.

library(MASS) # NB this will mask 'select' from dplyr!
fit <- lda(maxMOT ~ SVSMRAT8_ACHIEVEMENT + SVSMRAT7_HEDONISM 
           + SVSMRAT6_STIMULATION + SVSMRAT5_SELFDIRECTION 
           + SVSMRAT1_CONFORMITY + SVSMRAT10_SECURITY, data=casedata, na.action="na.omit", CV=TRUE)
fit <- lda(maxINT ~ SVSMRAT8_ACHIEVEMENT
           + SVSMRAT6_STIMULATION + SVSMRAT5_SELFDIRECTION 
           + SVSMRAT1_CONFORMITY + SVSMRAT10_SECURITY, data=casedata, na.action="na.omit", CV=TRUE)
fit
# Assess accuracy of prediction (http://www.statmethods.net/advstats/discriminant.html)
# Percent correct fo each category of maxMOT
ct <- table(casedata$maxINT, fit$class)
diag(prop.table(ct, 1))
# Total percent correct
sum(diag(prop.table(ct)))
# Voss did t-tests for individual values; try regression on ones he found sig (5 Freedom; 53 Curious; 34 Ambitious).
fit <- lm(MOT9IT_ENT ~ SVS_MRAT5 + SVS_MRAT53 + SVS_MRAT34,  data = casedata)
summary(fit)
fit <- lm(MOT9IT_ENT ~ COLLIND6IT + UA5IT + PD5IT, data = casedata)
summary(fit)
fit <- lm(MOT9IT_PROF ~ COLLIND6IT + UA5IT + PD5IT, data = casedata)
summary(fit)
fit <- lm(MOT9IT_LDRMGR ~ COLLIND6IT + UA5IT + PD5IT, data = casedata)
summary(fit)
plot(fit)



















############################################################################
#Temp play, using the dataset to calculate some lavaan sem models
#to check df for paper I'm reviewing.
############################################################################

library(lavaan)

correlations <- matrix(c(1, .11, .23, .07, .11, 1, .30, .27, .23, .30, 1, .16, .07, .27, .16, 1),nrow=4,ncol=4)
stdev = c(.69, 1.32, .53, .56)
varnames = c("MO", "EO", "OL", "SP")
covmat <- cor2cov(R = correlations, sds = stdev, names = varnames)
Fig1 <- '
   OL ~ EO
   SP ~ EO '
fitFig1 <- sem(Fig1, sample.cov = covmat, sample.nobs = 192)
summary(fitFig1)

BP.model <- 'BP =~ value1 + value2 + value3 + value4 + value5 + value6 + value7 + value8 + value9 + value10 + value11 + value12'
fitBP <- cfa(BP.model, data = casedata)
summary(fitBP)

MO.model <- 'cust =~ value1 + value2 + value3 + value4 + value5 + value6
             comp =~ value7 + value8 + value9 + value10
             MO =~ cust + comp '
fitMO <- cfa(MO.model, data = casedata)
summary(fitMO)

OL.model <- 'explor =~ value1 + value2 + value3 + value4 + value5 + value6 + value7 + value8 + value9
             exploit =~ value10+ value11 + value12 + value13+ value14 + value15 + value16 + value17
             OL =~ explor + exploit '
fitOL <- cfa(OL.model, data = casedata)
summary(fitOL)

EO.model <- 'inno =~ value1 + value2 + value3
             proac =~  value4 + value5 + value6
             risk =~ value7 + value8 + value9
             EO =~ inno + proac + risk '
fitEO <- cfa(EO.model, data = casedata)
summary(fitEO)
