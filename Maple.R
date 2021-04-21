dat <- read.csv("Mapledata.csv")

# Read Tree regional Biomass data:
bvars <- c("TRE_CN", "REGIONAL_DRYBIOT", "REGIONAL_DRYBIOM")
biom_df <- read.csv("TREE_REGIONAL_BIOMASS.csv")[, bvars]

names(biom_df)
names(biom_df)[1] <- "CN"

library(dplyr)
dat <- dat %>% left_join(biom_df, by = "CN")


str(dat)
head(dat)
# Add Species name:
dat$SciNam <- paste0(dat$GENUS, "_", dat$SPECIES)
summary(as.factor(dat$SciNam))


# Converting data to Metric units:
dat$DBH <- dat$DIA * 2.54 # DBH inch to cm
dat$TPH <- dat$TPA_UNADJ/0.405 # acre to hectare
dat$ELEV <- dat$ELEV*0.305 # ft to m

summary(dat)

# Identifier for plot and year
dat$PLT_YR <- paste0(dat$PLT_CN, "_", dat$INVYR)

# Calculate Key plot-level attributes:
names(dat)

var <- c("PLT_YR", "INVYR", "PLT_CN", "STATUSCD", "DBH", "TPH", "SciNam", "AGENTCD", "KINDCD", "LAT", "LON", "ELEV", "ECOSUBCD", "REGIONAL_DRYBIOT" )

dat1 <- dat[, var]

# do not subset here: 
### dat1 <- na.omit(dat1)
### dat1 <- subset(dat1, dat2$STATUSCD == 1)

summary(dat1)


# Calculate total number of trees by plot and year
# (might be absent in some because we are lookin only for maple species)

N <- tapply(dat1$TPH, dat1$PLT_YR, sum)
summary(N)


# Calculate Stand Basal Area (only for Maple species)
B <- tapply(3.14*dat1$DBH^2/40000*dat1$TPH, dat1$PLT_YR, sum)
summary(B)


# Number of Removal trees per plot:
R <- tapply(dat1$STATUSCD == 3, dat1$PLT_YR, sum)

# Regional Dry Biomass Volume

??? 


# Plot coordinates:
Coords_y <- tapply(dat1$LAT, dat1$PLT_YR, mean)
Coords_x <- tapply(dat1$LON, dat1$PLT_YR, mean)



# Plot
PLT <- tapply(dat1$PLT_CN, dat1$PLT_YR, mean)

# Year
YR <- tapply(dat1$INVYR, dat1$PLT_YR, mean)


# Merge plot level data:
Plt_df <- cbind.data.frame(PLT, YR, Coords_x, Coords_y, N, B, R)
Plt_df$PLT_YR <- paste0(Plt_df$PLT, "_", Plt_df$YR)


## Save plot level data:
write.csv(Plt_df, "MAPLE_PLT.csv", row.names = FALSE)



########### FIA 4.R ###############
# Number of trees by plot and basal area
N_acer   <- tapply(ifelse(dat1$SciNam=="Acer_rubrum" | dat1$SciNam=="Acer_saccharum", dat1$TPH, 0), dat1$PLT_YR,sum)
summary(N_acer)
summary(N)


# Basal area by plot
B_acer   <-  tapply(ifelse(dat1$SciNam=="Acer_rubrum" | dat1$SciNam=="Acer_saccharum",3.14*dat1$DBH^2/40000*dat1$TPH,0), dat1$PLT_YR, sum)
summary(B_acer)


### Harvest tree number and Volume Remaining







# Make new dataframe:
NB_df <- cbind.data.frame(N, B)
NB_df$PLT_YR <- row.names(NB_df)















