# Load tree data
tvars <- c( "CN", "PLT_CN", "INVYR", "STATUSCD", "SPCD", "DIA", "HT", "AGENTCD", "TPA_UNADJ", "STOCKING", "VOLCFGRS", "VOLCFNET", "TPAMORT_UNADJ", "TPAREMV_UNADJ")

tree_df <- read.csv("TREE.csv")[ , tvars]

# Load Removal, Mortality data
grmvars <- c("TRE_CN", "COMPONENT", "REMOVALS", "MORTALITY")
grm_df <- read.csv("TREE_GRM_ESTN.csv")[ , grmvars]

#  Rename TRE_CN to CN to connect with TREE dataframe 
names(grm_df)
names(grm_df)[1] <- "CN"
names(grm_df)

# Merge TREE with TREE_GRM_ESTN
trees <- merge(tree_df, grm_df, by = "CN")

# Select needed variables from the species table
svars   <- c("SPCD", "GENUS", "SPECIES")

# Load species data:
spp_df <-read.csv("REF_SPECIES.csv")[ ,svars]

# Merge species data to trees data
trees <- merge(trees, spp_df, by = "SPCD")

# Load plot data:
pvars   <- c("CN", "KINDCD", "LAT", "LON", "ELEV", "ECOSUBCD", "SAMP_METHOD_CD")
plot_df <- read.csv("PLOT.csv")[ , pvars]
names(plot_df)
# Rename plot CN to PLT_CN
names(plot_df)[1] <- "PLT_CN"
names(plot_df)

# Merge plot with trees data
trees <- merge(trees, plot_df, by = "PLT_CN")

# Load Condition table
cvars   <- c("PLT_CN", "DSTRBCD1", "DSTRBYR1", "TRTCD1","TRTYR1", "LAND_COVER_CLASS_CD", "AFFORESTATION_CD", "PREV_AFFORESTATION_CD")
cond_df <- read.csv("COND.csv")[, cvars]

# Merge Condition dataset with tree data:
trees <- merge(trees, cond_df, by = "PLT_CN")

# Select Sugar Maple and Red Maple species
M <- subset(trees, SPCD == 316 | SPCD == 318)

write.csv(M, "Mapledata.csv", row.names = FALSE)
