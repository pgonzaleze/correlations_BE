
##### ================================================== #####
#####             Classify correlations;                 #####
#####           a cross-sectional analysis               #####
##### ================================================== #####

# Simon Fraser University
# Resource & Environmental Management
# Author: Pedro C. González-Espinosa
# Created: 13/ SEP /2023
# Last update: 28/ SEP /2024

library(readxl)
library(tidyr)
library(dplyr)
library(broom)
library(corrplot)

# Read files
# adjust the name if necessary
PortInf= read_xlsx("Port infrastructure.xlsx", sheet = "QualPortInf_timeseries")
HRRL = read_xlsx("Human rights and rule of law.xlsx", sheet = "HRRL_timeseries")
GII = read_xlsx("Gender Inequality Index.xlsx", sheet = 'GII_timeseries')
GrpGriev = read_xlsx("Group Grievance.xlsx", sheet = "GrpGriev_timeseries")
FacElit = read_xlsx("Factionalized Elites.xlsx", sheet = "FactElit_timeseries")
EconIneq = read_xlsx('Economic Inequality.xlsx', sheet = "EconIneq_timeseries")
HFBD = read_xlsx('Human Flight and Brain Drain.xlsx', sheet = 'HFBD_timeseries')
ContCorr = read_xlsx("Control_of_corruption.xlsx", sheet = "ContCorr_timeseries")
StatLegit = read_xlsx("State Legitimacy.xlsx", sheet = "StatLegit_timeseries")
GovEff = read_xlsx("Government Effectiveness.xlsx", sheet = "GovEff_timeseries")
StatEcon = read_xlsx("State of the Economy.xlsx", sheet = "StatEcon_timeseries")
InvPI = read_xlsx("Investor_protection_index.xlsx", sheet = "InvPI_timeseries")
RegQual = read_xlsx("Regulatory quality.xlsx", sheet = "RegQual_timeseries")
BEnv = read_xlsx('Business_environment.xlsx', sheet = "BEnv_timeseries") 
PubServ = read_xlsx("Public services.xlsx", sheet = "PubServ_timeseries")
ShipConn = read_xlsx("Shipping connectivity.xlsx", sheet = "ShipConn_timeseries")
AirTransport = read_xlsx("Air transport.xlsx", sheet = "AirTransport_timeseries")
SecIntServ = read_xlsx("Secure Internet servers.xlsx", sheet = "SecIntServ_timeseries")
BioHabitat = read_xlsx("Biodiversity_Habitat.xlsx", sheet = "BioHabitat_timeseries")
BioSpp = read_xlsx("Biodiversity_Species.xlsx", sheet = "BioSpp_timeseries")
CleanWaters = read_xlsx("clean_waters.xlsx", sheet ="CleanWaters_timeseries")

# Verify data is numeric
str(PortInf)
str(EconIneq)
str(ContCorr)
str(FacElit)
str(GrpGriev)
str(HRRL)
str(HFBD)
str(GII) # ranges from 0 to 10
str(StatLegit)
str(GovEff)
str(StatEcon)
str(InvPI) # ranges from 0 (little to no investor protection) to 10 (greater investor protection). 
str(RegQual)
str(BEnv) # ranges from 1 (easiest) to 183 (most difficult)
str(PubServ)
str(ShipConn) # ranges from 0-174 (100 = highest connectivity) But, in this case China has 174
str(AirTransport) # Number of registered takeoffs
str(SecIntServ) # Internet servers per million people
str(BioHabitat) # range 0-100  
str(BioSpp) # range 0-100 
str(CleanWaters) # range 0-100 

# Covert to numeric if necessary
FacElit$FactElit = as.numeric(FacElit$FactElit)
PortInf$PortInf = as.numeric((PortInf$PortInf))

################################################################################
' Normalize data to avoid negative and positive values. E.g., because corruption
    runs from -2-5 (weak) to 2.5 (strong), or BEnv which runs from 1-183
    it will be normalized to run from 1 to 10'

##### ContCorr #####
original_range <- max(ContCorr$ContCorr, na.rm = T) - min(ContCorr$ContCorr, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
ContCorr$ContCorr = (ContCorr$ContCorr - min(ContCorr$ContCorr, na.rm = T)) * scaling_factor + target_min


##### GovEff #####
original_range <- max(GovEff$GovEff, na.rm = T) - min(GovEff$GovEff, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
GovEff$GovEff = (GovEff$GovEff - min(GovEff$GovEff, na.rm = T)) * scaling_factor + target_min


##### RegQual #####
original_range <- max(RegQual$RegQual, na.rm = T) - min(RegQual$RegQual, na.rm = T)
# Target data range 
target_min <- 1
target_max <- 10
# Calculate scaling factor
scaling_factor <- (target_max - target_min) / original_range
# Normalize the data to the range 1 to 10
RegQual$RegQual = (RegQual$RegQual - min(RegQual$RegQual, na.rm = T)) * scaling_factor + target_min


################################################################################
'Reverse values of the indicator if necessary, 
     As an example, for economic inequality values, the lower the value the 
     lower the inequality, this should be reversed where the higher the value 
     lower inequality.' 

##### Economic Inequalities #####
EconIneq$EconIneq <- (1+max(EconIneq$EconIneq, na.rm = T)) - EconIneq$EconIneq

##### Factionalized Elites #####
FacElit$FactElit <- (1+max(FacElit$FactElit, na.rm = T)) - FacElit$FactElit

##### Group Grievance #####
GrpGriev$GrpGriev <- (1+max(GrpGriev$GrpGriev, na.rm = T)) - GrpGriev$GrpGriev

##### Human rights and rule of law #####
HRRL$HRRL <- (1+max(HRRL$HRRL, na.rm = T)) - HRRL$HRRL

##### Human flight and Brain drain #####
HFBD$HFBD <- (1+max(HFBD$HFBD, na.rm = T)) - HFBD$HFBD

##### Gender Inequality Index ####  0 to 1
GII$GII <- (0+max(GII$GII, na.rm = T)) - GII$GII

##### State of the Economy #####
StatEcon$StatEcon  <- (1+max(StatEcon$StatEcon, na.rm = T)) - StatEcon$StatEcon

#### State legitimacy #####
StatLegit$StatLegit <- (1+max(StatLegit$StatLegit, na.rm = T)) - StatLegit$StatLegit

##### Business Environment #####
BEnv$BEnv <- (1+max(BEnv$BEnv, na.rm = T)) - BEnv$BEnv

##### Public services #####
PubServ$PubServ <- (1+max(PubServ$PubServ, na.rm = T)) - PubServ$PubServ

############################################################################
# Summarize and update each dataframe by Territory
AirTransport <- AirTransport %>% 
  group_by(Territory) %>%
  summarise(AirTransport = mean(AirTransport, na.rm = TRUE))

BEnv <- BEnv %>% 
  group_by(Territory) %>% 
  summarise(BEnv = mean(BEnv, na.rm = TRUE))
  
BioHabitat <- BioHabitat %>% 
  group_by(Territory) %>% 
  summarise(BioHabitat = mean(BioHabitat, na.rm = TRUE))

BioSpp <- BioSpp %>%
  group_by(Territory) %>% 
  summarise(BioSpp = mean(BioSpp, na.rm = TRUE))

CleanWaters <- CleanWaters %>% 
  group_by(Territory) %>% 
  summarise(CleanWaters = mean(CleanWaters, na.rm = TRUE))

ContCorr <- ContCorr %>%
  group_by(Territory) %>% 
  summarise(ContCorr = mean(ContCorr, na.rm = TRUE))

EconIneq <- EconIneq %>% 
  group_by(Territory) %>% 
  summarise(EconIneq = mean(EconIneq, na.rm = TRUE))

FacElit <- FacElit %>%
  group_by(Territory) %>% 
  summarise(FactElit = mean(FactElit, na.rm = TRUE))

GII <- GII %>%
  group_by(Territory) %>% 
  summarise(GII = mean(GII, na.rm = TRUE))

GovEff <- GovEff %>% 
  group_by(Territory) %>% 
  summarise(GovEff = mean(GovEff, na.rm = TRUE))

GrpGriev <- GrpGriev %>%
  group_by(Territory) %>% 
  summarise(GrpGriev = mean(GrpGriev, na.rm = TRUE))

HFBD <- HFBD %>%
  group_by(Territory) %>% 
  summarise(HFBD = mean(HFBD, na.rm = TRUE))

HRRL <- HRRL %>% 
  group_by(Territory) %>% 
  summarise(HRRL = mean(HRRL, na.rm = TRUE))

InvPI <- InvPI %>% 
  group_by(Territory) %>% 
  summarise(InvPI = mean(InvPI, na.rm = TRUE))

PortInf <- PortInf %>% 
  group_by(Territory) %>% 
  summarise(PortInf = mean(PortInf, na.rm = TRUE))

PubServ <- PubServ %>% 
  group_by(Territory) %>% 
  summarise(PubServ = mean(PubServ, na.rm = TRUE))

RegQual <- RegQual %>% 
  group_by(Territory) %>% 
  summarise(RegQual = mean(RegQual, na.rm = TRUE))

SecIntServ <- SecIntServ %>% 
  group_by(Territory) %>% 
  summarise(SecIntServ = mean(SecIntServ, na.rm = TRUE))

ShipConn <- ShipConn %>%   
  group_by(Territory) %>% 
  summarise(ShipConn = mean(ShipConn, na.rm = TRUE))

StatEcon <- StatEcon %>%   
  group_by(Territory) %>% 
  summarise(StatEcon = mean(StatEcon, na.rm = TRUE))

StatLegit <- StatLegit %>% 
  group_by(Territory) %>% 
  summarise(StatLegit = mean(StatLegit, na.rm = TRUE))

# merge dataframes in descending order by lenght 
Indicators_DF <- left_join(AirTransport, GII, by = "Territory") %>% 
  left_join(GovEff, by = "Territory") %>% 
  left_join(ContCorr, by = "Territory") %>% 
  left_join(RegQual, by = "Territory") %>%   
  left_join(ShipConn, by = "Territory") %>%
  left_join(StatEcon, by = "Territory") %>%
  left_join(EconIneq, by = "Territory") %>% 
  left_join(FacElit, by = "Territory") %>%
  left_join(GrpGriev, by = "Territory") %>%
  left_join(HFBD, by = "Territory") %>%
  left_join(HRRL, by = "Territory") %>% 
  left_join(PubServ, by = "Territory") %>%
  left_join(StatLegit, by = "Territory") %>%
  left_join(BioHabitat, by = "Territory") %>%
  left_join(BioSpp, by = "Territory") %>% 
  left_join(CleanWaters, by = "Territory") %>%
  left_join(SecIntServ, by = "Territory") %>%
  left_join(PortInf, by = "Territory") %>% 
  left_join(InvPI, by = "Territory") %>%
  left_join(BEnv, by = "Territory") %>%
  select(Territory, !(starts_with("Territory")))



#########################################################################
####                     compute correlations                      ######
#########################################################################
C <-cor(Indicators_DF[,2:22], use = "pairwise.complete.obs")
head(round(C,2))

# Plot correlation matrix using corrplot
plotC <- corrplot(C, method = "color", type = "lower", order="alphabet", 
                  tl.col="black", tl.srt=45)
plotC_number <- corrplot(C, method = "number", type = "lower", number.cex = 0.5, 
                         tl.col="black", tl.srt=45)


#########################################################################
####                         Add p-values                          ######
#########################################################################

Indicators_DF[, 2:22] <- apply(Indicators_DF[, 2:22], 2, as.numeric)

# Use kendal just to verify everything is correct 
# P_kendall <- apply(Indicators_DF[, 2:22], 2, function(col1) {
#   sapply(Indicators_DF[, 2:22], function(col2) {
#     cor.test(col1, col2, method = "kendall")$p.value
#   })
# })
# 
# plotC <- corrplot(C, p.mat = P_kendall, sig.level = c(0.001, 0.01, 0.05),
#                   pch.cex = 0.75,
#                   insig = 'label_sig',
#                   method = "color", type = "lower", order="alphabet", 
#                   tl.col="black", tl.srt=45)

P_spearman <- apply(Indicators_DF[, 2:22], 2, function(col1) {
  sapply(Indicators_DF[, 2:22], function(col2) {
    cor.test(col1, col2, method = "spearman")$p.value
  
  })
})

plotC <- corrplot(C, p.mat = P_spearman, sig.level = c(0.001, 0.01, 0.05),
                  pch.cex = 0.75,
                  insig = 'label_sig',
                  method = "color", type = "lower", order="alphabet", 
                  tl.col="black", tl.srt=45)

###### ================================================= ######
######                   Subset by dimensison            ######
###### ================================================= ######

###########             Social Equity; SE            ########
rows <- c("HRRL","GII", "GrpGriev", "FactElit", "EconIneq", "HFBD", "ContCorr")
cols <- c("HRRL","GII", "GrpGriev", "FactElit", "EconIneq", "HFBD", "ContCorr")

plot_SE <- corrplot(C[rows, cols], p.mat = P_spearman[rows, cols], sig.level = c(0.001, 0.01, 0.05),
                  pch.cex = 0.75,
                  insig = 'label_sig', 
                  method = "number", type = "lower", order="alphabet", 
                  tl.col="black", tl.srt=45)

##########         Economic Viability; EV         ###########
rows <- c("StatLegit", "GovEff", "StatEcon", "InvPI", "RegQual", "PortInf",
          "BEnv", "PubServ", "ShipConn", "AirTransport", "SecIntServ")
cols <- c("StatLegit", "GovEff", "StatEcon", "InvPI", "RegQual", "PortInf",
          "BEnv", "PubServ", "ShipConn", "AirTransport", "SecIntServ")

plot_EV <- corrplot(C[rows, cols], p.mat = P_spearman[rows, cols], sig.level = c(0.001, 0.01, 0.05),
                    pch.cex = 0.75,
                    insig = 'label_sig',
                    method = "number", type = "lower", order="alphabet", 
                    tl.col="black", tl.srt=45)

##########          Environmental Sustainability; ES        ##########
rows <- c("BioHabitat", "BioSpp", "CleanWaters")
cols <- c("BioHabitat", "BioSpp", "CleanWaters")

plot_ES <- corrplot(C[rows, cols], p.mat = P_spearman[rows, cols], sig.level = c(0.001, 0.01, 0.05),
                    pch.cex = 0.75,
                    insig = 'label_sig',
                    method = "number", type = "lower", order="alphabet", 
                    tl.col="black", tl.srt=45)




# PLot with histograms
library("PerformanceAnalytics")

chart.Correlation(Indicators_DF[,2:22], histogram=TRUE, pch=19)

corrplotHisto <- chart.Correlation(Indicators_DF[,2:22], histogram=TRUE, pch=19)
