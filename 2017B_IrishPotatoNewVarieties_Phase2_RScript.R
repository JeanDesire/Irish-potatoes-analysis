#Author: Jean Desire HABIYAMBERE
# PURPOSE: Season 17B Irish potatoes new seeds analysis
# Prepared: 07/06/2019

rm(list=ls()) # this clears the r environment
cat("\014") # this clears the screen

library(ggplot2)
library(plyr)
suppressMessages(library(Rmisc))
library(multcomp)
library(reshape2)
library(stringr)
library(foreign)
library(dplyr)
library(stringr)
library(readxl)
library(car)
library(rmarkdown)

#Setting working Directories
#----------------------------

#wd <- "D:/Users/One Acre Fund/Desktop/Arsene Working Directory/17B Analysis/17B_RAY_New_seed_Analysis"
wd <- "C:/Users/tuyis/Dropbox/4. R Data/6. 17B"

dd <- paste(wd, "data", sep="/")
od <- paste(wd, "output", sep="/")

# ------------ SECTION 1 - CLEAN DATA Generation ------------------

# ***********************************************************
# 1. Importing Datasets in R
# ***********************************************************

pl <- suppressWarnings(read_excel(paste(dd, "AgInn - 17B RAY variety - Raw data.xlsx",
                                        sep = "/"), sheet="plot.level.data"))   
pl <- as.data.frame(pl) # This sheet contains data on the plot level

cl <- suppressWarnings(read_excel(paste(dd, "AgInn - 17B RAY variety - Raw data.xlsx", 
                                        sep = "/"), sheet="Parent Cases"))   
cl <- as.data.frame(cl) # This sheet contains case id of farmers

fl <- suppressWarnings(read_excel(paste(dd, "AgInn - 17B RAY variety - Raw data.xlsx", sep = "/"),
                                  sheet="farmer.level.data"))
fl <- as.data.frame(fl) # This is sheet contains data on the farmer level

# **********************************************************
# 2. MERGING Datasets to get one common dataset including pl,cl and fl
# **********************************************************
# Merge: First I will have to merge pl and cl by row.number

names(cl)[names(cl)=="row.number"] <- "row.number1"
names(cl)[names(cl)=="row.number__0"] <- "row.number"
d1 <- merge(pl, cl, by= c("row.number"))
d0 <- merge(fl,d1, by=c("case.id"))
names(d0)

# 3. CLEANING
# ----------------------------------------------------------

# delete unwanted columns
d0$officer.y = NULL
d0$row.number.y = NULL
d0$row.number1 = NULL
d0$row.number__1 = NULL
d0$farmer.id.y = NULL
d0$case_type = NULL
d0$trial.no.y = NULL
d0$agzone1 = NULL
d0$name.y = NULL

# DROPING almost blank rows: here I want to see the rows which are almost fully blank in the dataset
blankRows <- rowSums(is.na(d0)) 
table(blankRows)
View(d0[which(blankRows>95),])

# then drop
#d0 <- d0[-which(blankRows>169),]
#dim(d0)

# No rows have been deleted

# check teatments, timeframe and Crop
d0$treatment[d0$treatment.code=="A"] = "New local seed"
d0$treatment[d0$treatment.code=="B"] = "New Kinigi seed (certified)"
d0$treatment[d0$treatment.code=="C"] = "New Kinigi seed (base)"
d0$treatment[d0$treatment.code=="D"] = "New Kirundo seed"
d0$treatment[d0$treatment.code=="E"] = "new Sangenma seed"
d0$treatment[d0$treatment.code=="F"] = "New Cruza seed"

d0$timeframe <- "Seasonal"
d0$crop <- "irish_potato"

# recode specific codes like 9999, 999 etc.
d0[d0==9999 | d0==99999] <- NA

# drop test farmers
testDrop <- grep("test", d0$name.x)
View(d0[testDrop,])
#d0 <- d0[-which(grepl("test", d0$name.x)),]
# There are no test farmers in the dataset

# Delete variation Mbi
d0$q87_variation_mbi[is.na(d0$q87_variation_mbi)]<-"no"
d <- d0[-which(grepl("yes", d0$q87_variation_mbi)),]

# Sorting data by: Region, officer, farmer.id, name, plot, treatment.code
d <- d[order(d$region, d$officer.x, d$farmer.id.x, d$name.x, d$plot, d$treatment.code), ]

# ----------------------------------------------------------

# 4. DATA CHECKS
# ----------------------------------------------------------

# Here we will have to Check: 
# surface and crop rows
# Seed qtty, 
# Germination rate,
# Inputs grams,
# harvest box (hb), 
# tuber kg,
# Ranking
# Taste
# Ares planted
# Total Are owned


# A. Surface & Number of Crop rows(Here we want to correct errors in plot rows)
ggplot(d, aes(plot.surface)) + geom_density(bw=0.8) + facet_wrap(~ cell, scales="free")
d[d$name.x=="Tuyishime Fabien", c("officer.x", "plot.width", "plot.length", "plot.surface", "treatment.code", "count.rows.plot")]
d$plot.surface.2 = d$plot.surface

d$plot.surface.2 = ifelse(d$name.x=="Tuyishime Fabien"&d$plot.surface.2==9.60, 19.60, d$plot.surface.2)
d$plot.surface.2 = ifelse(d$name.x=="mpakaniye fabien"&d$plot.surface.2==80, 40, d$plot.surface.2)
d$plot.surface = d$plot.surface.2
d$plot.surface.2 = NULL

d$spacing.lines.cm = 60
d$spacing.holes.cm = 40

d$z.lines.plot <- (d$plot.length*100)/60

d[d$count.rows.plot==116, c("name.x", "District","treatment.code","treatment","plot.surface", "plot.length", "z.lines.plot")]
d[, "count.rows.plot"][d$count.rows.plot==116] <- 16

d$z.lines.diff <- d$z.lines.plot - d$count.rows.plot 
d$z.est.length <- ((d$count.rows.plot-1)*60)/100
d$z.plot.surface <- d$z.est.length * d$plot.width
d$z.plot.surface.diff <- d$plot.surface - d$z.plot.surface
table(d$z.plot.surface.diff, useNA = "ifany")

# B. Seeds planted
table(d$seed.grams.planted, useNA = "ifany")

# For the grams below 2000, I suspect that they are typing errors,but let me filter them first to get an overview of those data
#After reviewing those data I noticed that the values below 2000 are typos.

d$seed.grams.planted.2 = d$seed.grams.planted
d$seed.grams.planted.2 = ifelse(d$seed.grams.planted.2== 60, d$seed.grams.planted.2*100, d$seed.grams.planted.2)
d$seed.grams.planted.2 = ifelse(d$seed.grams.planted.2< 2000, d$seed.grams.planted.2*10,d$seed.grams.planted.2)
d$seed.grams.planted[d$name.x=="HAKORIMANA  BERNARD"&d$treatment.code=="B"] = 10000
d$seed.grams.planted = d$seed.grams.planted.2
d$seed.grams.planted.2 = NULL

#aggregate(d[, c("seed.grams.planted","plot.surface")], by=list(d$treatment.code,d$officer.x),FUN=mean, na.rm=T)
d$z_seed_are_kg <- (d$seed.grams.planted/1000)*(100/d$plot.surface)
table(d$z_seed_are_kg, useNA = "ifany")

ggplot(data=d, aes(x=treatment.code, y = z_seed_are_kg)) + geom_boxplot() +
  facet_wrap(~ agzone, scales="free")
###In the regression we will run the regression with and without the outliers to check if there are some changes in the results

# C. Prices

# - Fertilizer
d$q143a_input1_cost_kg_frw <- 520 # to exclude typos in NPK price

# - Seed
table(d$seed.cost.unit, useNA = "ifany") 
d$seed.variety[d$treatment.code=="A"] = "Local"
d$seed.variety[d$treatment.code=="B"] = "Kinigi"
d$seed.variety[d$treatment.code=="C"] = "Kinigi_base"
d$seed.variety[d$treatment.code=="D"] = "Kirundo"
d$seed.variety[d$treatment.code=="E"] = "Sangema"
d$seed.variety[d$treatment.code=="F"] = "Cruza"

d$seed.cost.unit[d$treatment.code=="B"|
                 d$treatment.code=="D"|
                 d$treatment.code=="E"|
                 d$treatment.code=="F"] = 550
d$seed.cost.unit[d$treatment.code=="C"] = 600

aggregate(d$seed.cost.unit, by=list(d$treatment.code,d$District), FUN= mean, na.rm=T)
d$seed.cost.unit[d$treatment.code=="A"&d$seed.cost.unit==999] = 344
d$seed.cost.unit[d$cell=="Bugera"&d$treatment.code=="A"&is.na(d$seed.cost.unit)] = 296
d$seed.cost.unit[d$cell=="Kabeza"&d$treatment.code=="A"&is.na(d$seed.cost.unit)] = 260
d$seed.cost.unit[d$cell=="Rwesero"&d$treatment.code=="A"&is.na(d$seed.cost.unit)] = 344
d$seed.cost.unit[d$cell=="Ryaruhanga"&d$treatment.code=="A"&is.na(d$seed.cost.unit)] = 432
d$seed.cost.unit[d$cell=="Gati"&d$treatment.code=="A"&is.na(d$seed.cost.unit)] = 437

# - Fresh tuber revenues in harvest
table(d$q144b_revenue1_frw_kg, useNA = "ifany") 
# I have viewed 2 typos. To solve them I refered on the original dataset, for 2 farmers Uruvugundi alphoncine,Uwimana Gérard

# to compare the typo with the value reported on the same farmer
d$q144b_revenue1_frw_kg <- recode(d$q144b_revenue1_frw_kg,"8=0;
                                  20=200")
### We will use the aggregates since in the esoko data we have missing information in Nyanza District
d$q144b_revenue1_frw_kg[d$q144b_revenue1_frw_kg==0] = NA
aggregate(d$q144b_revenue1_frw_kg, by=list(d$District), FUN= mean, na.rm=T)

d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "GAT")] <- 221
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "KYZ")] <- 173
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "MUG")] <- 219
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "NRU")] <- 153
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "NYM")] <- 87
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "NYZ")] <- 199
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "OMA")] <- 217
d$q144b_revenue1_frw_kg[which(is.na(d$q144b_revenue1_frw_kg) & d$District == "RSZ")] <- 181

# D.Fertilizer Quantity
table(d$input1.grams.used, useNA = "ifany")
#there are 2 typos in input1.grams.used
d[, "input1.grams.used"][d$input1.grams.used==6000] <- 600
d[, "input1.grams.used"][d$input1.grams.used==10000] <- 1000

# -------- Clean Fert/Are, otherwise profit will be unreasonable!! ------------

d$input1.grams.used[d$name.x=="Rucamaneke Jean Claude"&
                      d$treatment.code=="A"] = 1500
d$input1.grams.used[d$name.x=="Singirankabo J, damascene"&
                      d$treatment.code=="A"] = 1500
d$plot.surface[d$name.x=="Nyiranziza Dorothe"&
                      d$treatment.code=="A"] = 49.60
d$input1.grams.used[d$name.x=="Hakizamungu   Jean  pierre"&
                 d$treatment.code=="F"] = 1289
d$plot.surface[d$name.x=="Nyiransabimana    Elder"&
                 d$treatment.code=="F"] = 48.40
d$plot.surface[d$name.x=="karemera nepomsene"&
                 d$treatment.code=="A"] = 24.8
d$plot.surface[d$name.x=="karemera nepomsene"&
                 d$treatment.code=="B"] = 50
d$plot.surface[d$name.x=="Tuyishime Fabien"&
                 d$treatment.code=="D"] = 9.6
d$plot.surface[d$name.x=="mukasibomana beatrice"&
                 d$treatment.code=="D"] = 30
d$plot.surface[d$name.x=="bayavuge esperence"&
                 d$treatment.code=="F"] = 36
d$plot.surface[d$name.x=="ntibazirikana didas"&
                 d$treatment.code=="A"] = 50
d$plot.surface[d$name.x=="ntibazirikana didas"&
                 d$treatment.code=="A"] = 50
d$plot.surface[d$name.x=="ruzindana peter"&
                 d$treatment.code=="B"] = 49.75
d$plot.surface[d$name.x=="nyangabo alphonse"&
                 d$treatment.code=="B"] = 44
d$plot.surface[d$name.x=="ntibazirikana didas"&
                 d$treatment.code=="E"] = 26.35
d$plot.surface[d$name.x=="nyangabo alphonse"&
                 d$treatment.code=="F"] = 25.8
d$input1.grams.used[d$name.x=="nyangabo alphonse"&
                 d$treatment.code=="F"] = 787.5
d$input1.grams.used[d$name.x=="mbonyincuti evaliste"&
                      d$treatment.code=="D"] = 601
d$input1.grams.used[d$name.x=="ntibazirikana didas"&
                      d$treatment.code=="D"] = 601
d$input1.grams.used[d$name.x=="ntibazirikana didas"&
                      d$treatment.code=="F"] = 601
d$input1.grams.used[d$name.x=="mbonyincuti evaliste"&
                      d$treatment.code=="F"] = 601

d$input1.grams.used[d$name.x=="Singirankabo J, damascene"&
                      d$treatment.code=="C"] = 1005
d$input1.grams.used[d$name.x=="Ngerageze Matien"&
                      d$treatment.code=="C"] = 855
d$input1.grams.used[d$name.x=="Mukagashugi Selapie"&
                      d$treatment.code=="C"] = 855
d$plot.surface[d$name.x=="HAKORIMANA  BERNARD"&
                      d$treatment.code=="B"] = 33.7
d$plot.surface[d$name.x=="HAKORIMANA  BERNARD"&
                 d$treatment.code=="C"] = 34.2
d$input1.grams.used[d$name.x=="HAKORIMANA  BERNARD"&
                 d$treatment.code=="C"] = 1015
d$plot.surface[d$name.x=="ZIGIRINSHUTI  EMMANUEL"&
                      d$treatment.code=="D"] = 27.3
d$plot.surface[d$name.x=="U WITIJE  AGNES"&
                      d$treatment.code=="D"] = 27.3
d$plot.surface[d$name.x=="HABINEZA  GAD"&
                      d$treatment.code=="D"] = 27.3
d$plot.surface[d$name.x=="IRAGUHA  JEAN  DE  DIEU"&
                 d$treatment.code=="F"] = 38.5
d$plot.surface[d$name.x=="U WITIJE  AGNES"&
                 d$treatment.code=="F"] = 38.5

d$input1.grams.used[d$name.x=="Nyiransabimana    Elder"&
                 d$treatment.code=="B"] = 935
d$plot.surface[d$name.x=="Bihutu Samson"&
                      d$treatment.code=="B"] = 30
d$input1.grams.used[d$name.x=="Mukamudenge    Vereria"&
                 d$treatment.code=="B"] = 935
d$input1.grams.used[d$name.x=="Niyikiza   Console"&
                      d$treatment.code=="B"] = 935

d$input1.grams.used[d$name.x=="Hakizamungu   Jean  pierre"&
                      d$treatment.code=="C"] = 1005
d$input1.grams.used[d$name.x=="Mukamurara  Speciose"&
                      d$treatment.code=="C"] = 1005
d$input1.grams.used[d$name.x=="Nyiransabimana    Elder"&
                      d$treatment.code=="C"] = 1005
d$input1.grams.used[d$name.x=="Niyikiza   Console"&
                      d$treatment.code=="C"] = 1005

d$input1.grams.used[d$name.x=="Mukamurara  Speciose"&
                      d$treatment.code=="D"] = 686
d$input1.grams.used[d$name.x=="Rucogoza Uziel"&
                      d$treatment.code=="D"] = 686
d$input1.grams.used[d$name.x=="Nyiransabimana    Elder"&
                      d$treatment.code=="D"] = 686
d$input1.grams.used[d$name.x=="Mukamudenge    Vereria"&
                      d$treatment.code=="D"] = 686
d$input1.grams.used[d$name.x=="Niyikiza   Console"&
                      d$treatment.code=="D"] = 686

d$input1.grams.used[d$name.x=="Nyiransabimana    Elder"&
                      d$treatment.code=="E"] = 983
d$input1.grams.used[d$name.x=="Niyikiza   Console"&
                      d$treatment.code=="F"] = 951
d$plot.surface[d$name.x=="Mutegaraba M Gorethie"&
                      d$treatment.code=="A"] = 48
d$plot.surface[d$name.x=="Mutegaraba M Gorethie"&
                 d$treatment.code=="B"] = 49

# -----------------------------------------------------------------------------
d$z_input1_are_kg <- (d$input1.grams.used/1000)*(100/d$plot.surface)
table(d$z_input1_are_kg, useNA = "ifany")

# E. Harvest box
table(d$q137_hb_size_m2, useNA = "ifany")
# The values that may be suspecious are 2, 9.6, 50, 249935 

# case of 2, 50, 249935:
d$q137_hb_size_m2 <- ifelse(d$q137_hb_size_m2==2, 25, d$q137_hb_size_m2)
d$q137_hb_size_m2 <- ifelse(d$q137_hb_size_m2==50, 25, d$q137_hb_size_m2)
d$q137_hb_size_m2 <- ifelse(d$q137_hb_size_m2==249935, 24.9935, d$q137_hb_size_m2)

# F. Yield
table(d$q141c_kg_tubers, useNA = "ifany") # 69 NAs identified
d$q141c_kg_tubers[d$q141c_kg_tubers==520] <- 52 

table(d$q83c_prob1_yield_reduction, useNA = "ifany") 
d$q141c_kg_tubers[d$q83c_prob1_yield_reduction=="completly"] <- 0 # this code works as ifelse
d$q137_hb_size_m2[d$plot.surface>25&d$q83c_prob1_yield_reduction=="completly"] <- 25
d$q141c_kg_tubers[d$q84c_prob2_yield_reduction=="completly"] <- 0 

# As we still have NAs, let's clean them
d$q141c_kg_tubers[is.na(d$q141c_kg_tubers)] <- 0 
d$q137_hb_size_m2[d$q137_hb_size_m2==0] <- 25 

# G. Ranking
table(d$q146_treat_rank_harvest, useNA = "ifany")
d$top.choice <-recode(d$q146_treat_rank_harvest,"1:2=1;3:4=0")

#I. GERMINATION RATE
Germination.rate.agg <- aggregate(d$q43_first_germination_rate, by=list(d$treatment), FUN = mean, na.rm=T)
Germination.rate.agg

# --------------------------------------------------
# 5. DATA FINALIZATION
# --------------------------------------------------

# Create yield.are and profit.are

d$yield.kg.are <- d$q141c_kg_tubers * (100/d$q137_hb_size_m2)

d$cost.are.fert <- d$z_input1_are_kg * d$q143a_input1_cost_kg_frw
d$cost.are.seed <- d$z_seed_are_kg * d$seed.cost.unit
d$Total.cost.are = d$cost.are.fert + d$cost.are.seed

table(d$District) # to check the districts we have in data.
# This is important because we calculate revenue according to the price
# of fresh tuber in each district. This number is obtained from:
# https://docs.google.com/spreadsheets/d/1chzMC7D3XkveZIkqbzpzfe3JLXQeUUCHuxMk_FwT4qc/edit?ts=589dd560#gid=1704811174
# using "Irish potato, Farmer Price (FRw/kg)" table. And our checklist recommend
# to use Average of May + June + July if is the season is B.

tuber.price <- aggregate(d$q144b_revenue1_frw_kg, by=list(d$District), FUN = mean, na.rm=T)
tuber.price

# GAT: GD = 223 | Harvest data = 221
# KYZ: GD = 116 | Harvest data = 176
# MUG: GD = 156 | Harvest data = 232
# NRU: GD = 206 | Harvest data = 170 
# NYM: GD = 217 | Harvest data = 109
# NYZ: GD =     | Harvest data = 199
# OMA: GD = 56  | Harvest data = 217
# RSZ: GD = 91  | Harvest data = 201

# As GD file is missing some values and Inconsistent, I decide to use Harvest data
d$revenue.are <- d$yield.kg.are * d$q144b_revenue1_frw_kg

d$profit.are <- d$revenue.are - d$Total.cost.are


# Plotting to get a general sense on outlier
ggplot(d, aes(x = yield.kg.are)) + geom_density() + 
  facet_wrap(~ treatment.code + cell, scales="free")

boxplot(yield.kg.are~treatment.code,data=d,
        main="Yield per Are by Treatment Code",
        col=(c("green","blue","red","orange","grey","yellow")),xlab="treatment code ",
        ylab="yield per are ")

ggplot(data=d, aes(x=treatment.code, y = yield.kg.are)) + geom_boxplot() +
  facet_wrap(~ District, scales="free") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 0, hjust = 1)) + 
  labs(title = "Box Plots by Group and Variation", x = "Variation", y = "Group")
## ************************************
kg.sum <- aggregate(d$q144b_revenue1_frw_kg, by=list(d$District), FUN = mean, na.rm=T)

#leveneTest(d$yield.kg.are,as.factor(d$treatment.code), center=mean)
#these graphs are not flagging unreasonable outliers which can cause issues

# Deleting Unwanted columns after analysis
names(d)
d$z.lines.plot = NULL
d$z.lines.diff = NULL
d$z.est.length = NULL
d$z_input1_hb_kg = NULL
d$q144f_profit_hb = NULL
d$q144g_profit_are = NULL
d$q144e_total_revenue_hb = NULL
d$z.plot.surface.diff = NULL
d$z.plot.surface = NULL

# 
# ---------------- SECTION 2 - DATA ANALYSIS ----------------------
# *****************************************************************

# *** Load cleaned dataset Here, then continue analysis.
write.csv(d, file=paste(od, "17B_RAY_new_seed V2.csv", sep="/"), 
          row.names=F)

# Data Import
# **********************************************
d = read.csv(paste(od, "17B_RAY_new_seed V2.csv" , sep="/"), 
             header=T, stringsAsFactors=F)
#helping functions, Used :
agg.stats <- function(x, y) {
  
  md=aggregate(x, by=list(y), FUN=median, na.rm=T)
  mn=aggregate(x, by=list(y), FUN=mean, na.rm=T)
  ste = aggregate(x, by=list(y), function(z) {
    sqrt(var(z,na.rm=TRUE)/length(na.omit(z)))})
  out <-as.data.frame(paste(round(mn$x,2), "|",round(md$x,2),"|", " (", round(ste$x,2), ")", sep=""))
  names(out) <- "stats"
  return(out)
}

comp.letters <- function(x) {
  tuk <- glht(x, linfct=mcp(treatment.code="Tukey"))
  let <- cld(tuk, level=0.05)$mcletters[1]
  return(let$Letters)
}

# 1. Yield & Profit
d$block <- d$case.id
d$treatment.code <- as.factor(d$treatment.code)
d$block <- as.factor(d$block)
d$agzone <- as.factor(d$agzone)
d$fertility.rank <- as.factor(d$fertility.rank)
d$tubura.client <- as.factor(d$tubura.client)
d$compost.quality <- as.factor(d$compost.quality)
d$q148_product_buy_in_future <- as.numeric(d$q148_product_buy_in_future)

ag.zone.basic <- function(w, y) {
  
  tab <- do.call(rbind, lapply(split(w, w[,y]), function(x) {
    
    fsh.yield <- agg.stats(x$yield.kg.are, x$treatment.code)
    
    fsh.profit <- agg.stats(x$profit.are, x$treatment.code)
    
    buy.future <- agg.stats (x$q148_product_buy_in_future, x$treatment.code)
    kg.future.sum <- agg.stats(x$q149_product_kg_in_future, x$treatment.code)
    top.choice.sum <- agg.stats(x$top.choice, x$treatment.code)
    germination.rate.sum<- agg.stats(x$q43_first_germination_rate, x$treatment.code)
    #ANOVA - yield
    fs.yield <- aov(yield.kg.are ~ treatment.code + block, data=x)
    fs.yield.let <- comp.letters(fs.yield)
    
    #ANOVA - profit
    fs.profit <- aov(profit.are ~ treatment.code + block, data=x)
    fs.profit.let <- comp.letters(fs.profit)
    
    
    # design results table
    output <- data.frame(
      season = unique(x$Season),
      phase = "on-farm",
      agzone = unique(x$agzone),
      treat.code= unique(x$treatment.code),
      treat.name = unique(x$treatment),
      district = str_c(unique(x$District), sep=" ", collapse=" "),
      cell = str_c(unique(x$cell), sep=" ", collapse=" "),
      n = aggregate(x$treatment.code, by=list(x$treatment.code), function(x) {length(x)})$x, 
      crop = str_c(unique(x$crop), sep=" ", collapse=" "),
      yield = paste(fsh.yield$stats,fs.yield.let, sep = " "),
      profit = paste(fsh.profit$stats, fs.profit.let, sep=" "),
      top.choice = paste(top.choice.sum$stats),
      adopt.future = paste(buy.future$stats),
      kg.future = paste(kg.future.sum$stats),
      germination_rate = paste(germination.rate.sum$stats)
      
    )
    
    return(output)
    
  }))
  
  return(tab)
} 

res1 <- ag.zone.basic(d,"agzone") 

ag.zone.all <- function(x) {
  
  fsh.yield <- agg.stats(x$yield.kg.are, x$treatment.code)
  
  fsh.profit <- agg.stats(x$profit.are, x$treatment.code)
  
  buy.future <- agg.stats (x$q148_product_buy_in_future, x$treatment.code)
  kg.future.sum <- agg.stats(x$q149_product_kg_in_future, x$treatment.code)
  top.choice.sum <- agg.stats(x$top.choice, x$treatment.code)
  germination.rate.sum<- agg.stats(x$q43_first_germination_rate, x$treatment.code)
  
  #ANOVA - yield
  fs.yield <- aov(yield.kg.are ~ treatment.code + block + agzone, data=x)
  fs.yield.let <- comp.letters(fs.yield)
  
  #ANOVA - profit
  fs.profit <- aov(profit.are ~ treatment.code + block + agzone, data=x)
  fs.profit.let <- comp.letters(fs.profit)
  
  
  # design results table
  output <- data.frame(
    season = unique(x$Season),
    phase = "on-farm",
    agzone = "All Agzones",
    treat.code= unique(x$treatment.code),
    treat.name = unique(x$treatment),
    district = str_c(unique(x$District), sep=" ", collapse=" "),
    cell = str_c(unique(x$cell), sep=" ", collapse=" "),
    n = aggregate(x$treatment.code, by=list(x$treatment.code), function(x) {length(x)})$x, 
    crop = str_c(unique(x$crop), sep=" ", collapse=" "),
    yield = paste(fsh.yield$stats,fs.yield.let, sep = " "),
    profit = paste(fsh.profit$stats, fs.profit.let, sep=" "),
    top.choice = paste(top.choice.sum$stats),
    adopt.future = paste(buy.future$stats),
    kg.future = paste(kg.future.sum$stats),
    germination_rate = paste(germination.rate.sum$stats)
    
  )
  return(output)
}

res2 <- ag.zone.all(d)

results <- rbind(res1, res2)

# ************************
# 2. Diseases vulnerability 
d$q55b_stress3_severity_pd1[is.na(d$q55b_stress3_severity_pd1)] = 0
d$q53b_stress1_severity_pd1[is.na(d$q53b_stress1_severity_pd1)] = 0 
d$q54b_stress2_severity_pd1[is.na(d$q54b_stress2_severity_pd1)] = 0
d$q89b_stress1_severity_pd2[is.na(d$q89b_stress1_severity_pd2)] = 0
d$q90b_stress2_severity_pd2[is.na(d$q90b_stress2_severity_pd2)] = 0
d$q91b_stress3_severity_pd2[is.na(d$q91b_stress3_severity_pd2)] = 0
d$q117b_stress1_severity_pd3[is.na(d$q117b_stress1_severity_pd3)] = 0
d$q118b_stress2_severity_pd3[is.na(d$q118b_stress2_severity_pd3)] = 0
d$q119b_stress3_severity_pd3[is.na(d$q119b_stress3_severity_pd3)] = 0

d$S1.sev.agv <- (d$q53b_stress1_severity_pd1 + d$q89b_stress1_severity_pd2 + d$q117b_stress1_severity_pd3)/3
d$S2.sev.agv <- (d$q54b_stress2_severity_pd1 + d$q90b_stress2_severity_pd2 + d$q118b_stress2_severity_pd3)/3
d$S3.sev.agv <- (d$q55b_stress3_severity_pd1 + d$q91b_stress3_severity_pd2 + d$q119b_stress3_severity_pd3)/3

diseases <- aggregate(d[,c("S1.sev.agv","S2.sev.agv","S3.sev.agv","q53_stress1_present_pd1","q54_stress2_present_pd1","q55_stress3_present_pd1","q89_stress1_present_pd2",
                           "q90_stress2_present_pd2","q91_stress3_present_pd2","q117_stress1_present_pd3","q118_stress2_present_pd3", "q119_stress3_present_pd3",
                           "q53a_stress1_incidence_pd1","q54a_stress2_incidence_pd1","q55a_stress3_incidence_pd1","q89a_stress1_incidence_pd2",
                           "q90a_stress2_incidence_pd2","q91a_stress3_incidence_pd2","q117a_stress1_incidence_pd3","q118a_stress2_incidence_pd3","q119a_stress3_incidence_pd3")], by=list(d$treatment.code, d$agzone), FUN = mean, na.rm=T)


# Treatment E is mostly attacked by Kirabiranya (S2)

# *************************
# taste
table(d$q156b_taste_rank, useNA = "ifany")
d$top.taste <- recode(d$q156b_taste_rank,"1=1;2:3=0")
d$bad.taste <- recode(d$q156b_taste_rank,"3=1;1:2=0")
d$top.cook <- recode(d$q156a_cooking_time,"1=1;2:3=0")
d$late.cook <- recode(d$q156a_cooking_time,"2=1;1:2=0")
d$top.choice.ph <- recode(d$q156g_treat_rank_post_harvest,"1:2=1;3:4=0")

feedback <- aggregate(d[,c("top.taste","bad.taste","top.cook","late.cook","top.choice.ph")], by=list(d$treatment.code, d$agzone), FUN = mean, na.rm=T)

# ***********************
# Regressions

yield.effects <- summary(lm(yield.kg.are ~ treatment.code + agzone + slope.degree + fertility.rank + 
                              compost.kg.plot + compost.quality + q109_minutes_home_field + tubura.client + 
                              q112_total_ares_trial_basic_crop, data=d))
yield.effects

#Regression Interpretation.(Many observations have been deleted, to investigate)

#Dependent variable: Yield.kg.are
#Explanatory variables:treatment, agzone,slope.degree,fertility.rank, compost.kg.plot,compost.quality,q109_minutes_home_field ,tubura.client, q112_total_ares_trial_basic_crop

# Important driver besides agzones and treatmments is: slope.degree

# slope degree
# As we observe in the regression, the agzone Eastern savanah, did have an effect on the yield.but others didn't have significant effect
# In our regression we have a R-squared= 0.4871, that means that 48.7 of the variation in the yield is explained by the explanatory variables.
# ************************
# RESULTS OUTPUT

write.csv(results, file=paste(od, "17B RAY new seeds results.csv", sep="/"), 
          row.names=F)
write.csv(diseases, file=paste(od, "17B RAY new seeds diseases.csv", sep="/"), 
          row.names=F)
write.csv(feedback, file=paste(od, "17B RAY new seeds feedback.csv", sep="/"), 
          row.names=F)

write.csv(d, file=paste(od, "2017B_IrishPotatoNewVarieties_Phase2_Database.csv", sep="/"), 
          row.names=F)

###############################################################################################################################################################
