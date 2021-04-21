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
