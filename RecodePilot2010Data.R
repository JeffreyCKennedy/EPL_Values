
# SYNYTAX NOTES: This syntax incorporates the recode, compute, and reliability commands from the file
# <01_Pilot2010_Compute.sps>. The other 2010 syntax file <02_CAS_Indepth2010_Compute.sps> has a subset 
# of the same recode statements, including ECLIM4.

library(dplyr)   # Used for joining new variables to existing dataframe and other manipulations.
library(sjPlot)  # Used for reading in spss files, viewing them, etc.
library(psych)   # Used for creating scales and calculating reliability and other descriptives.
library(car)     # Used for the recode function.
library(haven)   # needed for following read_spss function.
library(sjmisc)
library(ggplot2)

casedata.Pilot <- sjmisc::read_spss("C:/Users/jckenned/Documents/Research/EPL Project/Values&EPL paper/DataFiles/Files from CAS zip file/01_Pilot2010_n=304.sav")
# view_df(casedata.Pilot, showFreq = TRUE, showPerc = TRUE) # shows var names & labels; appends freq and % for values.

# NOTE: In original spss dataset I had to delete the label for "ID" variable. It seems that the label
# caused a change in object type (atomic with label) which stuffed up the left_join later on (when adding 
# scale values back into the dataframe). It seems that the ID variable in the scale scores dataframe was numeric
# and original value was atomic so matching couldn't be done. Deleting spss variable label and reimporting the data
# fixed the problem.

# NOTE: See http://www.strengejacke.de/sjPlot/datainit/. Can also specify use of 'foreign' though this
# also requires specifying additional options to get value labels in.

# names(casedata.Pilot)  # lists all the variable names with column numbers, so can reference by number.
# names(casedata.Pilot[-1:-60]) #drops first 60 columns.

# Need to put recode statements first as some reverse scored items are used.
# Use concatenate in excel, using variable names copied in from spss syntax.
# Should probably use else = NA at end of recode string.

casedata.Pilot$MOT59_LAIr <- recode(casedata.Pilot$MOT59_LAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT54_LAIr <- recode(casedata.Pilot$MOT54_LAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT48_LAIr <- recode(casedata.Pilot$MOT48_LAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT51_LNCr <- recode(casedata.Pilot$MOT51_LNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT47_LSNr <- recode(casedata.Pilot$MOT47_LSN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT43_LSNr <- recode(casedata.Pilot$MOT43_LSN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT55_LNCr <- recode(casedata.Pilot$MOT55_LNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT56_LSNr <- recode(casedata.Pilot$MOT56_LSN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT45_LNCr <- recode(casedata.Pilot$MOT45_LNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT60_LNCr <- recode(casedata.Pilot$MOT60_LNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT7_EAIr <- recode(casedata.Pilot$MOT7_EAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT21_ENCr <- recode(casedata.Pilot$MOT21_ENC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT19_EAIr <- recode(casedata.Pilot$MOT19_EAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT14_EAIr <- recode(casedata.Pilot$MOT14_EAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT2_ENCr <- recode(casedata.Pilot$MOT2_ENC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT4_ENCr <- recode(casedata.Pilot$MOT4_ENC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT17_ESNr <- recode(casedata.Pilot$MOT17_ESN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT12_ESNr <- recode(casedata.Pilot$MOT12_ESN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT20_ESNr <- recode(casedata.Pilot$MOT20_ESN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT23_PAIr <- recode(casedata.Pilot$MOT23_PAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT22_PAIr <- recode(casedata.Pilot$MOT22_PAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT24_PNCr <- recode(casedata.Pilot$MOT24_PNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT37_PAIr <- recode(casedata.Pilot$MOT37_PAI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT33_PSNr <- recode(casedata.Pilot$MOT33_PSN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT25_PNCr <- recode(casedata.Pilot$MOT25_PNC,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT29_PSNr <- recode(casedata.Pilot$MOT29_PSN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$MOT36_PSNr <- recode(casedata.Pilot$MOT36_PSN,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$INT4_EIr <- recode(casedata.Pilot$INT4_EI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$INT13_EIr <- recode(casedata.Pilot$INT13_EI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$INT21_PIr <- recode(casedata.Pilot$INT21_PI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$INT6_LIr <- recode(casedata.Pilot$INT6_LI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
casedata.Pilot$INT10_LIr <- recode(casedata.Pilot$INT10_LI,"1=5 ; 2=4 ; 3=3 ; 4=2 ; 5=1; else=NA")
# Note: Original spss syntax did not include a recode for IND16_VI, though RELIABILITY syntax specified it.
casedata.Pilot$IND16_VIr <- recode(casedata.Pilot$IND16_VI,"1=9 ; 2=8 ; 3=7 ; 4=6; 5=5 ; 6=4 ; 7=3 ; 8=2 ; 9=1; else=NA")

# Set up keys list for scoring the scales (http://www.personality-project.org/r/html/score.items.html).
keys.list <- list(ID=c("ID"),
                  MEAN_EAI3IT=c("MOT1_EAI","MOT10_EAI","MOT18_EAI"),
                  MEAN_ECL3IT=c("MOT5_ENC","MOT8_ENC","MOT9_ENC"),
                  MEAN_ESN3IT=c("MOT6_ESN","MOT15_ESN","MOT16_ESN"),
                  MOT_ENT9IT=c("MOT1_EAI","MOT10_EAI","MOT18_EAI","MOT5_ENC","MOT8_ENC","MOT9_ENC","MOT6_ESN","MOT15_ESN","MOT16_ESN"),
                  MEAN_PAI3IT=c("MOT28_PAI","MOT38_PAI","MOT39_PAI"),
                  MEAN_PCL3IT=c("MOT25_PNCr","MOT27_PNC","MOT40_PNC"),
                  MEAN_PSN3IT=c("MOT32_PSN","MOT41_PSN","MOT42_PSN"),
                  MOT_PRO9IT=c("MOT28_PAI","MOT38_PAI","MOT39_PAI","MOT25_PNCr","MOT27_PNC","MOT40_PNC","MOT32_PSN","MOT41_PSN","MOT42_PSN"),
                  MEAN_LAI3IT=c("MOT48_LAIr","MOT61_LAI","MOT62_LAI"),
                  MEAN_LNC3IT=c("MOT53_LNC","MOT55_LNCr","MOT60_LNC"),
                  MEAN_LSN3IT=c("MOT46_LSN","MOT57_LSN","MOT63_LSN"),
                  MOT_LDR9IT=c("MOT48_LAIr","MOT61_LAI","MOT62_LAI","MOT53_LNC","MOT55_LNCr","MOT60_LNC","MOT46_LSN","MOT57_LSN","MOT63_LSN"),
                  INT_ENT2IT=c("INT2_EI","INT17_EI"),
                  INT_PRO4IT=c("INT1_PI","INT9_PI","INT16_PI","INT21_PIr"),
                  INT_LDR3IT=c("INT3_LI","INT6_LIr","INT11_LI"),
                  EFF_ENT7IT=c("EFF1_Ent","EFF2_Ent","EFF3_Ent","EFF4_Ent","EFF5_Ent","EFF6_Ent","EFF7_Ent"),
                  EFF_PRO5IT=c("EFF19_Prof","EFF20_Prof","EFF21_Prof","EFF22_Prof","EFF23_Prof"),
                  EFF_LDR7IT=c("EFF12_LMgt","EFF13_LMgt","EFF14_LMgt","EFF15_LMgt","EFF16_LMgt","EFF17_LMgt","EFF18_LMgt"),
                  INDCOL_HC7IT=c("IND1_HC","IND5_HC","IND9_HC","IND13_HC","IND17_HC","IND21_HC","IND25_HC"),
                  INDCOL_HI7IT=c("IND2_HI","IND6_HI","IND10_HI","IND14_HI","IND18_HI","IND22_HI","IND26_HI"),
                  INDCOL_VC7IT=c("IND3_VC","IND7_VC","IND11_VC","IND15_VC","IND19_VC","IND23_VC","IND27_VC"),
                  INDCOL_VI7IT=c("IND4_VI","IND8_VI","IND12_VI","IND16_VIr","IND20_VI","IND24_VI","IND28_VI"))

# Use select to choose only columns required for scale calcs. 
# Create itemsUsed vector by copying the keys.list input above into N++ and editing. Or, could
# pullout the new scale names from keys.list and combine, c(MEAN_EAI3IT, MEAN_ECL3IT, etc).

itemsUsed <- c("ID",
               "MOT1_EAI","MOT10_EAI","MOT18_EAI",
               "MOT5_ENC","MOT8_ENC","MOT9_ENC",
               "MOT6_ESN","MOT15_ESN","MOT16_ESN",
               "MOT28_PAI","MOT38_PAI","MOT39_PAI",
               "MOT25_PNCr","MOT27_PNC","MOT40_PNC",
               "MOT32_PSN","MOT41_PSN","MOT42_PSN",
               "MOT48_LAIr","MOT61_LAI","MOT62_LAI",
               "MOT53_LNC","MOT55_LNCr","MOT60_LNC",
               "MOT46_LSN","MOT57_LSN","MOT63_LSN",
               "INT2_EI","INT17_EI",
               "INT1_PI","INT9_PI","INT16_PI","INT21_PIr",
               "INT3_LI","INT6_LIr","INT11_LI",
               "EFF1_Ent","EFF2_Ent","EFF3_Ent","EFF4_Ent","EFF5_Ent","EFF6_Ent","EFF7_Ent",
               "EFF19_Prof","EFF20_Prof","EFF21_Prof","EFF22_Prof","EFF23_Prof",
               "EFF12_LMgt","EFF13_LMgt","EFF14_LMgt","EFF15_LMgt","EFF16_LMgt","EFF17_LMgt","EFF18_LMgt",
               "IND1_HC","IND5_HC","IND9_HC","IND13_HC","IND17_HC","IND21_HC","IND25_HC",
               "IND2_HI","IND6_HI","IND10_HI","IND14_HI","IND18_HI","IND22_HI","IND26_HI",
               "IND3_VC","IND7_VC","IND11_VC","IND15_VC","IND19_VC","IND23_VC","IND27_VC",
               "IND4_VI","IND8_VI","IND12_VI","IND16_VIr","IND20_VI","IND24_VI","IND28_VI")
subsetCasedata.Pilot <- casedata.Pilot %>% dplyr::select(one_of(itemsUsed))
# Note - if library(MASS) has been loaded (e.g., for discrim anal) then 'select' is masked, so need
# to specify dplyr::select in preceding command.
keys <- make.keys(subsetCasedata.Pilot, keys.list)
scores.NewScales <- scoreItems(keys, subsetCasedata.Pilot, missing=TRUE, impute="median", digits = 3)
# use scores.NewScales$scores to get scale scores; scores.NewScales$alpha for alpha, etc. names(scores)
# Compared output with spss, and alpha values, scale means, etc all matched.
# Average item intercorrelations were a bit different, and item-total correlations were
# quite a bit different, so may be some issues there.

# add the new scale scores to casedata.Pilot using dplyr::left_join, matching by ID.
casedata.Pilot <- dplyr::left_join(casedata.Pilot, as.data.frame(scores.NewScales$scores), by = "ID")

view_df(casedata.Pilot, showFreq = TRUE, showPerc = TRUE) # shows var names & labels; appends freq and % for values.

################### RUN ABOVE SYNTAX TO RECODE AND COMPUTE VARS IN DATAFRAME #################



# Using sjPlot to visualise correlations. Make sure sjPlot is loaded.
small.df <- select(casedata.pilot, INT_ENT2IT:INDCOL_VI7IT) 
sjp.corr(small.df)
# Following uses dplyr to select non-adjacent columns:
sjp.corr(dplyr::select(casedata.Pilot, one_of(c("MOT_ENT9IT","MOT_PRO9IT","MOT_LDR9IT",
                                                "INT_ENT2IT","INT_PRO4IT","INT_LDR3IT",
                                                "EFF_ENT7IT","EFF_PRO5IT","EFF_LDR7IT",
                                                "INDCOL_HC7IT","INDCOL_HI7IT","INDCOL_VC7IT",
                                                "INDCOL_VI7IT"))))

scatterplotMatrix(
    ~ INDCOL_HC7IT + INDCOL_HI7IT + INDCOL_VC7IT + INDCOL_VI7IT,
    data=casedata.Pilot, spread=FALSE, smoother.args=list(lty=2), 
    main="Scatter Plot Matrix via car Package")
    
with(casedata.Pilot, smoothScatter(INDCOL_VI7IT, MOT_PRO9IT, main="Scatter Plot Colored by Smoothed Densities"))

Singelis <- casedata.Pilot %>% 
    dplyr::select(one_of(c("INDCOL_HC7IT","INDCOL_HI7IT","INDCOL_VC7IT", "INDCOL_VI7IT")))
AffIdent <- casedata.Pilot %>% 
    dplyr::select(one_of(c("MEAN_EAI3IT", "MEAN_PAI3IT", "MEAN_LAI3IT")))
SocNorm <- casedata.Pilot %>% 
    dplyr::select(one_of(c("MEAN_ESN3IT", "MEAN_PSN3IT", "MEAN_LSN3IT")))
Calc <- casedata.Pilot %>% 
    dplyr::select(one_of(c("MEAN_ECL3IT", "MEAN_PCL3IT", "MEAN_LNC3IT")))
MotScales <- casedata.Pilot %>% 
    select(starts_with("MOT_"))
Singelis_VI <- dplyr::select(casedata.Pilot, one_of(c("IND4_VI","IND8_VI","IND12_VI","IND16_VIr","IND20_VI","IND24_VI","IND28_VI")))
Singelis_VC <- dplyr::select(casedata.Pilot, ends_with("VC"))
Singelis_HI <- dplyr::select(casedata.Pilot, ends_with("HI"))
Singelis_HC <- dplyr::select(casedata.Pilot, ends_with("HC"))

#################################################
# Singelis reliabilities (Cronbach and ordinal)
#################################################

sink("corrs and alpha Singelis.txt")
scores.NewScales$alpha # Cronbach alpha for scales
corr.test(Singelis, adjust = "none")
corr.test(Singelis, MotScales, adjust = "none")
corr.test(Singelis, AffIdent, adjust = "none")
corr.test(Singelis, SocNorm, adjust = "none")
corr.test(Singelis, Calc, adjust = "none")

alpha(Singelis_VI)         #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
alpha(Singelis_VC)         #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
alpha(Singelis_HI)     #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
alpha(Singelis_HC)     #raw Cronbachs & stdised alpha from Pearsons corr & cov matrices.
########
# Following don't work - get error message that Singelis has more than 8 categories
# for items, so polychoric prob not needed. Scale has 9 items. Seems that the polychoric
# matrix is not created, so subsequent command to show rho throws an error.
########
poly_VI <- polychoric(Singelis_VI) # saves polychoric corr matrix and tau values
alpha(poly_VI$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.
poly_VC <- polychoric(Singelis_VC) # saves polychoric corr matrix and tau values
alpha(poly_VC$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.
poly_HI <- polychoric(Singelis_HI) # saves polychoric corr matrix and tau values
alpha(poly_HI$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.
poly_HC <- polychoric(Singelis_HC) # saves polychoric corr matrix and tau values
alpha(poly_HC$rho)   # raw and stdised alpha, & item stats - as matrix is polychor corr, alpha is ordinal alpha.

sink()


myfit <- lm(MOT_ENT9IT ~ INDCOL_HC7IT + INDCOL_HI7IT + INDCOL_VC7IT + INDCOL_VI7IT, casedata.Pilot)
myfit <- lm(MOT_PRO9IT ~ INDCOL_HC7IT + INDCOL_HI7IT + INDCOL_VC7IT + INDCOL_VI7IT, casedata.Pilot)
myfit <- lm(MOT_LDR9IT ~ INDCOL_HC7IT + INDCOL_HI7IT + INDCOL_VC7IT + INDCOL_VI7IT, casedata.Pilot)
summary(myfit)
plot(myfit)

valueVars <- paste("VALUE", 1:20, sep="")
regression <- formula(paste("MOT_ENT9IT ~ ", paste(valueVars, collapse=" + ")))
fit <- lm(regression, data = casedata.Pilot)
