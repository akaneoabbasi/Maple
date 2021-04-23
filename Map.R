############## Data Extraction ########################################
setwd("~/Big_data")
memory.limit(80000)
memory.size(80000)
library(dplyr)

#################################
# Load tree data:
tvars <- c( "CN", "PLT_CN", "INVYR", "STATUSCD", "SPCD", "DIA", "HT", "AGENTCD","MORTCD", "TPA_UNADJ")

tree_df <- read.csv("TREE.csv")[ , tvars]

# x <- filter(Intree_df, MORTCD != "NA") # Mortality code not available

# STATUSCD: 0 no status, 1 live, 2 dead, 3 removed
# AGENTCD: cause of death:00(no agent), 10(insect), 20(disease), 30(fire), 40(animal),
#          50(weather), 60(veg. competition), 70(unknown), 80(silvicultural)
# MORTCD: (tree died within 5 yrs):1 (qualify as mortality), 0 (does not qualify)


# Remove periodic inventory and save to new object
# In_tree_df <- filter(Intree_df, STATUSCD != 0) # Periodic inventory has status 0

####################################
# Load TREE_GRM_ESTN.csv

#mvar <- c("CN", "INVYR", "PLT_CN", "TRE_CN", "COMPONENT", "TPAREMV_UNADJ", "TPAMORT_UNADJ", "REMOVALS", "MORTALITY", "M", "C")
grmvars <- c("TRE_CN", "COMPONENT", "REMOVALS", "MORTALITY")

# TRE_CN foreign key (CN in TREE table)
# COMPONENT Growth component type: SURVIVOR, INGROWTH, MORTALITY1, MORTALITY2, CUT1, 
# CUT2, REVERSION1, REVERSION2, DIVERSION1, DIVERSION2, CULLINCR, CULLDECR, N/A-P2A, N/A-PERIODIC


# Read required variables from the TREE_GRM_ESTN data:
grm_df <- read.csv("TREE_GRM_ESTN.csv")[ , grmvars]

#  Rename TRE_CN to CN to connect with TREE dataframe 
names(grm_df)
names(grm_df)[1] <- "CN"
names(grm_df)


####################################################################
# Merge TREE with TREE_GRM_ESTN
trees <- merge(tree_df, grm_df, by = "CN")



##################################################################

# Select needed variables from the species table
svars   <- c("SPCD", "GENUS", "SPECIES")

# Load species data:
spp_df <-read.csv("REF_SPECIES.csv")[ ,svars]


################################################################
# Merge species data to trees data
trees <- merge(trees, spp_df, by = "SPCD")


###################################
# Load plot data:

# Name required variables from the Plot table
pvars   <- c("CN", "KINDCD", "LAT", "LON", "ELEV", "ECOSUBCD", "SAMP_METHOD_CD")

# Read required variables from the plot data:
plot_df <- read.csv("PLOT.csv")[ , pvars]

# KINDCD: 0 annual, 1 new entry, 2 remeasured plots
# SAMP_METHOD_CD: 1 field visit, 2 remotely sensed


names(plot_df)
# Rename plot CN to PLT_CN
names(plot_df)[1] <- "PLT_CN"
names(plot_df)

##############################################
# Merge plot with trees data
trees <- merge(trees, plot_df, by = "PLT_CN")

levels(as.factor(trees$SAMP_METHOD_CD))

#######################
# Select needed variables from the Condition table
cvars   <- c("PLT_CN", "DSTRBCD1", "DSTRBYR1", "TRTCD1","TRTYR1", "LAND_COVER_CLASS_CD", "AFFORESTATION_CD", "PREV_AFFORESTATION_CD")

cond_df <- read.csv("COND.csv")[, cvars]

# HARVEST_TYPE1_SRS: 11(clearcut), 12(partial), 13(Shelterwood), 14(Commercial thinning),
#       15(timber stand improvement), 16(salvage cutting)

# STAND_STRUCTURE_SRS: 0(nonstocked), 1(singlestoried), 2(2-storied), 3(multi-storied)


# Almost entire dataset in condition table has NA values; not very useful

## Merge COND dataset with tree data:
trees <- merge(trees, cond_df, by = "PLT_CN")

summary(trees$MORTCD)

###########################################################################
# Check each variables before keeping it for final dataset and remove empty columns
# In_trees <- select(In_trees, -MORTCD) # Since all values are NA let's drop MORTCD

# Filter Genus Acer (Maple only)
# Maple <- filter(trees, GENUS == "Acer", SPECIES %in% c("saccharum", "rubrum"))

# Save Maple dataset for Indiana
# In_Maple <- select(In_Maple, -c("AFFORESTATION_CD", "PREV_AFFORESTATION_CD", "HARVEST_TYPE1_SRS", "LAND_USE_SRS", "STAND_STRUCTURE_SRS"))

# write.csv(Maple, "Maple.csv", row.names = FALSE)

M <- subset(trees, SPCD == 316 | SPCD == 318)

write.csv(M, "Mapledata.csv", row.names = FALSE)


######################################################################

setwd("C:/Users/dburl/BD/Maple")
dat <- read.csv("Mapledata.csv")
library(dplyr)


# # Read Tree regional Biomass data:
# bvars <- c("TRE_CN", "REGIONAL_DRYBIOT", "REGIONAL_DRYBIOM")
# biom_df <- read.csv("TREE_REGIONAL_BIOMASS.csv")[, bvars]
# 
# names(biom_df)
# names(biom_df)[1] <- "CN"
# 
# library(dplyr)
# dat <- dat %>% left_join(biom_df, by = "CN")
# 

str(dat)
head(dat)
# Add Species name:
dat$SciNam <- paste0(dat$GENUS, "_", dat$SPECIES)
summary(as.factor(dat$SciNam))


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
# dat$AGENTCD_BINS <- as.factor(dat$AGENTCD_BINS)
dat$AGENTCD_BINS[dat$AGENTCD %in% c(20,21,22,23,24,25,26,27,28,29)] <- "Disease"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(30, 31)] <- "Fire"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(40, 41,42,43,44,45)] <- "Animal"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(50,51,52,53,54,55,56)] <- "Weather"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(60, 65)] <- "VegSupp"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(70, 71,72,73,76,77)] <- "Unknown"
dat$AGENTCD_BINS[dat$AGENTCD %in% c(80,81,82,83,84,85,86)] <- "Silvi"


dat <- rename(dat, Agents = AGENTCD_BINS)
summary(as.factor(dat$Agents))

# Identifier for plot and year
dat$PLT_YR <- paste0(dat$PLT_CN, "_", dat$INVYR)

# Calculate Key plot-level attributes:
names(dat)

var <- c("PLT_YR", "INVYR", "PLT_CN", "STATUSCD", "DBH", "TPH", "SciNam", "TPHREMV", "TPHMORT",
         "Agents", "KINDCD", "LAT", "LON", "ELEV", "ECOSUBCD", "VOLCFNET", "SPCD" )

dat1 <- dat[ , var]

# Make Separate csv files for each species of Maple 

redMap <- dat1 %>% filter(SPCD == 316)
write.csv(redMap, "RedMaple.csv", row.names = FALSE)

sugarMap <- dat1 %>% filter(SPCD == 318)
write.csv(sugarMap, "SugarMaple.csv", row.names = FALSE)


##### Standing trees affected by the disturbance??
##### Subset dead and removed trees
redMap <- read.csv("RedMaple.csv")

RM_dead <- subset(redMap, STATUSCD == 2)
RM_cut <- subset(redMap, STATUSCD == 3)

# Agents summary for all dead and removed/cut maple trees:
RM_dead$Agents <- as.factor(RM_dead$Agents)
summary(RM_dead$Agents)

# RM_cut$Agents <- as.factor(RM_cut$Agents)
# summary(RM_cut$Agents)

# Add time period in the dataset
RM_dead$Prd <- NA
RM_dead$Prd[RM_dead$INVYR < 2000] <- "P1"
RM_dead$Prd[between(RM_dead$INVYR, 2000, 2010)] <- "P2"
RM_dead$Prd[RM_dead$INVYR > 2010] <- "P3"

summary(as.factor(RM_dead$Prd))


tapply(RM_dead$TPHREMV, RM_dead$Prd, mean, na.rm = TRUE)
tapply(RM_dead$TPHMORT, RM_dead$Prd, sd)
tapply(RM_dead$TPHMORT, RM_dead$Prd, sum)

# tapply(RM_dead$TPHMORT, RM_dead$Prd, mean, na.rm = TRUE)
RM_dead %>% 
  group_by(Prd) %>%
  summarise(meanTMort = mean(TPHMORT),
            Tmortsd = sd(TPHMORT), 
            meanTRemv = mean(TPHREMV, na.rm = TRUE))


# Calculate summary count for each agents for three period
sum_dead <- RM_dead %>%
  group_by(Prd) %>%
  count(Agents)

print(sum_dead, n = nrow(sum_dead))

ggplot(sum_dead, aes(Agents, n))+
  geom_boxplot()


print(sum_dead, n = nrow(sum_dead))

glimpse(RM_dead)
RM_dead %>%
  count(Agents)

ggplot(RM_dead, aes(Agents)) +
  geom_bar() +
  scale_x_discrete(drop = FALSE)


ggplot(DeadRM, aes(P1, fct_reorder(Agents, P1))) +
  geom_point()






sum_dead %>%
  count(Prd,  wt = n)

# Pivot sum_dead to tidydata:
View(sum_dead)
DeadRM <- sum_dead %>%
  pivot_wider(names_from = Prd, values_from = n)

# Visualize trends:
ggplot(DeadRM, aes(Agents)) +
  geom_density()


# Visualize changes over time:
ggplot(sum_dead, aes(Agents, n))+
  geom_line(aes(group = Prd)) +
  geom_point(aes(color = Prd))





M <- RM_dead %>% 
  group_by(Prd, Agents) %>%
  summarise(sum(TPHMORT))

print(M, n = nrow(M))



####################################################
### Replicate for Sugar Maple

