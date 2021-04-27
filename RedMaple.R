setwd("D:/Mapl")
dat <- read.csv("New.csv")

str(dat)
head(dat)
# # Add Species name:
# dat$SciNam <- paste0(dat$GENUS, "_", dat$SPECIES)
# summary(as.factor(dat$SciNam))
# dat <- subset(dat, select = -c("SciNam"))

# Give Scientific Names to Maple species
dat$SciNam <- NA

dat$SciNam[dat$SPCD == 318] <- "Acer_saccharum"
dat$SciNam[dat$SPCD == 316] <- "Acer_rubrum"
dat$SciNam <- as.factor(dat$SciNam)
summary(as.factor(dat$SPCD))
summary(dat$SciNam)


# Converting data to Metric units:
dat$DBH <- dat$DIA * 2.54 # DBH inch to cm
dat$TPH <- dat$TPA_UNADJ/0.405 # acre to hectare
dat$ELEV <- dat$ELEV*0.305 # ft to m
dat$TPHMORT <- dat$TPAMORT_UNADJ/0.405
dat$TPHREMV <- dat$TPAREMV_UNADJ/0.405

dat$SPCD <- as.factor(dat$SPCD)
dat$STATUSCD <- as.factor(dat$STATUSCD) # choose statuscd 2(dead), 3 (removed)
dat$AGENTCD <- as.factor(dat$AGENTCD)
dat$DSTRBCD1 <- as.factor(dat$DSTRBCD1) # observed disturbance within past 5 yrs
dat$TRTCD1 <- as.factor(dat$TRTCD1) # type of stand treatment within past 5 yrs

head(dat)
summary(dat$AGENTCD)

# Combine agentcodes and name it as Agents
dat$AGENTCD_BINS <- NA

dat$AGENTCD_BINS[dat$AGENTCD %in% c(10, 11, 12, 13, 15, 17, 19)] <- "Insect"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(20,21,22,23,24,25,26,27,28,29)] <- "Disease"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(30, 31)] <- "Fire"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(40, 41,42,43,44,45)] <- "Animal"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(50,51,52,53,54,55,56)] <- "Weather"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(60, 65)] <- "VegSupp"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(70, 71,72,73,76,77)] <- "Unknown"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(80,81,82,83,84,85,86)] <- "Silvi"

library(dplyr)
dat <- rename(dat, Agents = AGENTCD_BINS)
summary(as.factor(dat$Agents))


##### Make new column with Treatments #####
dat$TRT_BINS <- NA

dat$TRT_BINS[dat$TRTCD1 == 00] <- "No treat"
dat$TRT_BINS[dat$TRTCD1 == 10] <- "Cutting"
dat$TRT_BINS[dat$TRTCD1 == 20] <- "Site prep"
dat$TRT_BINS[dat$TRTCD1 == 30] <- "Artf reg"
dat$TRT_BINS[dat$TRTCD1 == 40] <- "Nat reg"
dat$TRT_BINS[dat$TRTCD1 == 50] <- "Other"

library(dplyr)
dat <- rename(dat, Treatments = TRT_BINS)
summary(as.factor(dat$Treatments))



# Identifier for plot and year
dat$PLT_YR <- paste0(dat$PLT_CN, "_", dat$INVYR)

# Calculate Key plot-level attributes:
names(dat)

var <- c("PLT_YR", "INVYR", "PLT_CN", "STATUSCD", "DBH", "TPH", "TPHREMV", "TPHMORT",
         "Agents", "Treatments", "KINDCD", "LAT", "LON", "ELEV", "VOLCFNET", "SPCD" )

dat1 <- dat[ , var]
# datl <- subset(dat1, dat1$STATUSCD == 1) # select live trees
# datd <- subset(dat1, dat1$STATUSCD == 2) # select dead trees
# datr <- subset(dat1, dat1$STATUSCD == 3) # select removal trees

##################################################################################
###### Start working on Plot level attributes #########
# Calculate total number of trees by plot & year
N <-  tapply(dat1$TPH, dat1$PLT_YR, sum) # this includes every tree species fromo TREE.csv dataset
summary(N)

# Calculate total number of Red Maple trees by plot & year
N_ar <-  tapply(ifelse(dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR, sum)
summary(N_ar)


# Calculate Removal trees
# N_rem <- tapply(datr$TPHREMV, datr$PLT_YR, sum)
# summary(N_rem)

# Percentage of removal trees:
N_rem_perc <- tapply(ifelse(dat1$STATUSCD == 3, dat1$TPH, 0), dat1$PLT_YR,sum)/N *100
summary(N_rem_perc)

# Removal percentage for Red maple
N_rem_perc_ar <- tapply(ifelse(dat1$STATUSCD == 3 & dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR,sum)/N_ar *100
summary(N_rem_perc_ar)


# Percentage of Mortality trees:
N_mort_perc <- tapply(ifelse(dat1$STATUSCD == 2, dat1$TPH, 0), dat1$PLT_YR,sum)/N *100
summary(N_mort_perc)

# Mortality percentage for Red maple
N_mort_perc_ar <- tapply(ifelse(dat1$STATUSCD == 2 & dat1$SPCD == 316, dat1$TPH, 0), dat1$PLT_YR,sum)/N_ar *100
summary(N_mort_perc_ar)


###################################################

# Calculate total stand basal area
B <- tapply(3.14*dat1$DBH^2/40000*dat1$TPH, dat1$PLT_YR, sum)
summary(B)

# Calculate total stand basal area for Red Maple
B_ar <- tapply(ifelse(dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)
summary(B_ar)


######################################################
# Stand basal area removal percentage for Red Maple
B_rem_perc_ar <- tapply(ifelse(dat1$STATUSCD == 3 & dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)/B_ar *100
summary(B_rem_perc_ar)


# Percentage Stand basal area loss d/t mortality sugar maple
B_mort_perc_ar <- tapply(ifelse(dat1$STATUSCD == 2 & dat1$SPCD == 316, 3.14*dat1$DBH^2/40000*dat1$TPH, 0), dat1$PLT_YR, sum)/B_ar * 100
summary(B_mort_perc_ar)

# Plot coordinates
Coords_y <- tapply(dat1$LAT,dat1$PLT_YR,mean)
Coords_x <- tapply(dat1$LON,dat1$PLT_YR,mean)

# Plot
PLT <- tapply(dat1$PLT_CN, dat1$PLT_YR, mean)

# Year
YR <- tapply(dat1$INVYR, dat1$PLT_YR, mean)

# Merge plot-level data
PLT_df <- cbind.data.frame(PLT, YR, Coords_x, Coords_y, N, B, N_ar, B_ar, B_mort_perc_ar, B_rem_perc_ar, N_rem_perc_ar, N_rem_perc, N_mort_perc_ar)
PLT_df$PLT_YR <- paste0(PLT_df$PLT,"_",PLT_df$YR)

PLT_df <- PLT_df[-which(is.na(PLT_df$N)),] # remove NA's from dataset (N is absent)

# Save plot-level data
write.csv(PLT_df,"FIA_PLT.csv",row.names = FALSE)


# Make Separate csv files for Red Maple
redMap <- dat1 %>% filter(SPCD == 316)
write.csv(redMap, "RedMaple.csv", row.names = FALSE)

####################################################
# redMap <- read.csv("RedMaple.csv")

###################### Mortality/dead Red Maple ##########
RM_dead <- subset(redMap, STATUSCD == 2)

# Agents summary for all dead Red maple trees:
RM_dead$Agents <- as.factor(RM_dead$Agents)

RM_dead %>%
  count(Agents)

# Add time period in the dataset
RM_dead$Prd <- NA
RM_dead$Prd[RM_dead$INVYR < 2000] <- "P1"
RM_dead$Prd[between(RM_dead$INVYR, 2000, 2010)] <- "P2"
RM_dead$Prd[RM_dead$INVYR > 2010] <- "P3"


# Calculate summary count for each agents for three period
sum_dead <- RM_dead %>%
  group_by(Prd) %>%
  count(Agents)

print(sum_dead, n = nrow(sum_dead))

# Pivot sum_dead to tidydata:
View(sum_dead)
library(tidyr)
DeadRM <- sum_dead %>%
  pivot_wider(names_from = Prd, values_from = n)

DeadRM

#################### Cut/Harvest Red Maple ###############
RM_cut <- subset(redMap, STATUSCD == 3)

# Agents summary for all cut/removed Red maple trees:
summary(as.factor(RM_cut$Agents))


# Treatment summary for Red maple trees:
RM_cut %>%
  count(Treatments)

# Add time period in the dataset
RM_cut$Prd <- NA
RM_cut$Prd[RM_cut$INVYR < 2000] <- "P1"
RM_cut$Prd[between(RM_cut$INVYR, 2000, 2010)] <- "P2"
RM_cut$Prd[RM_cut$INVYR > 2010] <- "P3"

# Calculate summary count for each agents for three period
sum_cut <- RM_cut %>%
  group_by(Prd) %>%
  count(Treatments)

print(sum_cut, n = nrow(sum_cut))

# Pivot sum_cut to tidydata with each period:
library(tidyr)
CutRM <- sum_cut %>%
  pivot_wider(names_from = Prd, values_from = n)

CutRM

