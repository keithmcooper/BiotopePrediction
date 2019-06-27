#################################################################################################
#################################################################################################
##                MATCHING NEW SAMPLE DATA TO EXISTING BIO AND PHY CLUSTER GROUPS              ##
#################################################################################################
#################################################################################################

## Ths script matches new faunal data to the existing cluster groups in Cooper and Barry (2017)

## Set working directory
setwd('C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/BiotopePrediction')

#### PREPARE MAPPING LAYERS ####

## Install packages 
#install.packages("ggplot2")
#install.packages("rgdal")
#install.packages("maptools")
#install.packages("plyr")

## Call packages
library(ggplot2)
library(rgdal)
library(maptools)
library(plyr)
library(RPostgreSQL) 
library(sf)
library(methods)
methods(class = "sf")
install_github("edzer/rgdal2")
library(devtools) # maybe install first?

  
  ## Produce a high definition european coast map
  # Load shapefile
  eu <- readOGR("DATA","EUROPE")
eu@data$id <- rownames(eu@data)

## Create a data.frame from our spatial object
euDF <- fortify(eu, region = "id")

## Merge the "fortified" data with the data from our spatial object. Object euDF 
# is a dataframe of the polygon coordinates.  This is combined with the other attribute
# data from object eu
euDF2 <- merge(euDF, eu@data, by = "id")

## Create layer for coastline including borders between UK countries
eubound = readOGR("DATA","EuropeLiteScoWal")
eubound@data$id = rownames(eubound@data)
eubound.points = fortify(eubound, region="id")
eubound.df = join(eubound.points, eubound@data, by="id")

## Defra DEM (bathy) 
defra.dem = readOGR("DATA","DEFRADEMKC8")
defra.dem$id = rownames(defra.dem@data)
defra.dem.points = fortify(defra.dem, region="id")
defra.dem.df = join(defra.dem.points, defra.dem@data, by="id")
defra.dem.df$Depth = factor(defra.dem.df$Depth,levels(defra.dem.df$Depth)[c(1,2,5,8,9,10,3,6,11,
                                                                            4,7)])
levels(defra.dem.df$Depth)

## Create grey scale colour palette for use with bathy fill. 0=black, 1=white
grey=grey.colors(11, start = 0.95, end = 0.5,  alpha = NULL,gamma = 2.2)#, end=0.01


#### GENERATE TEST DATA ####

## Enter new dataset into PRIMER PART C (see subfolder: data)

## Output test data from Primer as a .txt file. PRIMER 6>File>Save data as>PARTC24072017.txt

## Open file PARTC24072017.txt in Excel and (i) remove 1st row, (ii) remove column between faunal
# data and metadata, and (iii) add header 'Sample' to first column. Save file as PARTC24072017.csv

## Bring in test data
test.raw=read.csv("DATA/PARTC16112018.csv", header=T,na.strings=c("NA", "-","?","<null>"),
                  stringsAsFactors=F,check.names=FALSE)


## Check dataset dimensions
dim(test.raw)# 554 13783

## Check column order
options(max.print=1000000)# to see all column names
names(test.raw) # Structure: Faunal data followed by other variables  

## Select only variables of interest 
# 1:13531 Faunal data
# 13532 Order
# 13533 SampleCode
# 13534 SurveyName
# 13535 SurveyPurpose
# 13536 Latitude_WGS84
# 13537 Longitude_WGS84
# 13538 WaterDepth
# 13539 Gear
# 13540 GearArea
# 13541 Sieve
# 13542 Year
# 13543 Month
# 13544 Date
# 13545 GrabSampleSize
# 13546:13644 Sediment Sieve Data
# 13645 Sector
# 13646 Source
# 13647 Owner
# 13648 Repository
# 13649 SampleCode2
# 13652 PSASubSample
# 13653 DataPublicalyAvailable
# 13654 Notes
# 13655 Treatment
# 13656 MacroLab
# 13657 PSALab
# 13713 ices_ann_avg_saltB
# 13729 ices_ann_avg_tempB
# 13768 clim_chlorophyll_a_Annual Mean_2002to2010_mean
# 13794 clim_suspended_matters_Annual Mean_2002to2010
# 13795 bathy_500
# 13804 PeakWaveOrb
# 13805 Av_current
# 13806 PeakWC_Stress
# 13808 IncCol
# 13809 TopSieveEmpty
# 13812 Data
# 13813 Programme
# 13814 WentworthSuitability

test.raw2=test.raw[,c(1:13531,13532,13533,13534,13535,13536,13537,13538,13539,13540,13541,13542,13543,13544,13545,13546:13644,13645,13646,13647,13648,13649,13652,13653,13654,13655,13656,13657,13713,13729,13768,13794,13795,13804,13805,13806,13808,13809,13812,13813,13814)]

## Check required columns present
names(test.raw2)
dim(test.raw2)


## Get Survey names
test.raw2$SurveyName=as.factor(test.raw2$SurveyName)
levels(test.raw2$SurveyName)

## Add col for SurveyName summary (to use with plots as full names too long)
test.raw2$Survey=as.character(test.raw2$SurveyName)
#names(test.raw2)

## Populate FaunalCluster col with new names (see dendrogram from Step 21)
test.raw2$Survey[test.raw2$Survey == "Haisborough, Hammond and Winterton 2016 cSACSCI"] <- "HHW"
test.raw2$Survey[test.raw2$Survey == "Inner Dowsing, Race Bank and North Ridge 2016 cSACSCI"] <- "IDRBNR"
test.raw2$Survey[test.raw2$Survey == "North Norfolk Sandbanks and Saturn Reef 2016  cSACSCI"] <- "NNSB"
test.raw2$Survey[test.raw2$Survey == "NWJB2017"] <- "NWJB"
test.raw2$Survey[test.raw2$Survey == "South Coast Regional Seabed Monitoring Programme 2017/2018"] <- "SCRSMP"
test.raw2$Survey[test.raw2$Survey == "Utopia2016"] <- "U"

## Make test.raw2$SurveyNameShort a factor
test.raw2$Survey=as.factor(test.raw2$Survey)

## Get number of samples by Survey (Table 1)
library(dplyr)
count=test.raw2%>%group_by(SurveyName)%>%summarise(n=n(),maxLat=max(Latitude_WGS84),minLat=min(Latitude_WGS84),maxLon=max(Longitude_WGS84),minLon=min(Longitude_WGS84))
View(count)

## Find gear used for different surveys (Table 1)
names(test.raw2)
View(test.raw2[,c(13669,13539,13540,13541)])

## Select only the faunal data. 
test.raw3=test.raw2[,1:13531]

## Change any NAs to 0
test.raw3[is.na(test.raw3)] <- 0

## Transpose the faunal data (keep column names)
t_test.raw3 = setNames(data.frame(t(test.raw3[,-1])), test.raw3[,1])
View(t_test.raw3)

## Bring in aggregation data
agg=read.csv("DATA/PARTCAGG16112018.csv", header=T,
             na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)
#View(agg)# View aggreation file
#dim(agg)

## Join the aggregation file to df of faunal data ('t_test.raw3')
all=cbind(agg,t_test.raw3)

## Remove unwanted taxa
all2 = subset(all, Include=='Y')

## Identify dimensions of df 'alla2'
dim(all2)# note the total number of columns

## View df 'alla2'
#View(all2)# identify the col nos for Family and samples
names(all2)

## Take only the Family and sample columns. Make sure you change the sample col numbers
all2fam=all2[,c(19,31:666)]

## Aggregate data (sum) by family for multiple samples
famabundtest=aggregate(. ~ Family, all2fam, sum)

## Check number of columns has reduced
dim(famabundtest)# 776 families 637 cols
#View(famabundtest)

## Find number of familes (rows with total of >1)
numfamtest=rowSums(famabundtest[,c(2:637)])
sum(numfamtest > 0)# 311

## Write file and close
#write.csv(famabundtest,file = "DATA/famabundtest.csv",row.names=TRUE)



####  BRING IN TRAINING DATASET ####

## Bring in training data file
train.data=read.csv("DATA/C5922DATASETFAM13022017.csv", header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)

## Remove 1st col
train.data[1] <- NULL    

## Check dataset dimensions
dim(train.data)#33198 900

## View dataset
#View(train.data)

## Change variable data types (factors required for plotting).
train.data$Sieve = as.factor(train.data$Sieve)
train.data$Year2 = as.factor(train.data$Year)
train.data$Month = as.factor(train.data$Month)
train.data$Source = as.factor(train.data$Source)
train.data$Gear = as.factor(train.data$Gear)
train.data$Data = as.factor(train.data$Data)

## Number of Surveys
length(rle(sort(train.data$Survey))$values)# 777

## Number of samples by Survey
table(train.data$Survey)


#### PREPARE TRAINING DATASET ####

## Prepare baseline data for faunal analysis - subset of comparable samples. This is Step 16 in Cooper & Barry (2017) R Script

## Subset data by gear (all 0.1m2 grabs)
train.data2 = subset(train.data, Gear=="MHN" | Gear=="DG" | Gear=="VV" | Gear=="SM")

## Check dimensions of df 'data2'
dim(train.data2)# 32044 901

## subset by sieve size (1mm only)
train.data3 = subset(train.data2, Sieve=='1')

## Check dimensions of df 'train.data3'
dim(train.data3)# 27622 901

## Check names of df 'data3'to identify faunal data columns
names(train.data3) # 2:775

## Remove samples from the faunal data (df data3) where no fauna present
train.data4 = train.data3[ rowSums(train.data3[,2:775])!=0, ]

## Check dimensions of df 'data4'
dim(train.data4)#27432 901

## Identify (usinig the .csv file) variables with no abundance
#write.csv(data4,file = "OUTPUTS/data4.csv",row.names=TRUE)

## Remove variables with no abund (got list from csv file). There will be taxa
# with no abund as some samples with taxa present have been deleted in gear step above  
train.data4.5 <- train.data4[-c(4,21,32,40,47,57,100,153,157,167,172,175,177,200,204,207,213,237,246,
                                252,283,294,313,325,362,376,379,381,410,413,414,417,418,426,428,430,446,
                                448,459,460,469,474,485,486,489,505,508,509,528,556,558,575,578,598,601,
                                603,605,607,630,633,638,647,675,687,705,709,726,734,760,765,775)]

## Check dimensions of df 'data4.5'
dim(train.data4.5)# 27432 830

## Show names of df 'data4.5'
names(train.data4.5)  

## Faunal subset (ie remove Sample,Latitude_WGS84, Longitude_WGS84, month and year)
train.data5=train.data4.5[,2:704]

## Check dimensions of df 'data5'
dim(train.data5) #27432 703

## Check df 'data5' is just the faunal data
names(train.data5)# it is

## Change class of df data5 to a matrix
data6=data.matrix(train.data5)

## Create a df 'pos' for Sample, Latitude_WGS84 and Longitude_WGS84 
pos=train.data4.5[,c(1,807:808)]

## Check names of df 'pos'
names(pos)


#### TEST DATA: DF WITH SAME FAMILIES AS TRAIN DATA ####

## Note that test data (df famabundtest) has 776 families, whereas train has only 703. Therefore we need to remove the additional families from test data for predict. Note that this doesn't mean the test data has 774 families as many rows (taxa) will have zero abundance.

## First create a dataframe for family names from the train data (df train.data5)
template=colnames(train.data5)# this is a vector
template=as.data.frame(template) # this is a df

## Change name of col 1 from 'template' to 'Family'
colnames(template)[1] <- "Family"
dim(template)# 703 1
#View(template)
#write.csv(template, file="FaunalDataTemplate.csv")# this can be populated manualy if not using code below

## Now need to create a version of the test data that only includes families from the train data
# merge two data frames by ID. Merge keeps only the common columns
testfamall <- merge(template,famabundtest,by="Family")
#View(testfamall)
dim(testfamall)# should be 703 (taxa) and how ever many samples you have (in this case 441)
class(testfamall)# df
row.names(testfamall)
str(testfamall)
View(testfamall)

## Transpose df 'testfamall' so rows are samples and cols are variables. Note this changes object to a matrix.
testfamallt=t(testfamall)
#View(testfamallt)

## Taxa names are in 1st row - need to make these the column headers
colnames(testfamallt) = testfamallt[1, ]
testfamallt = testfamallt[-1, ]# remove 1st row (taxon names)
#View(testfamallt)

## Row names in df 'testfamallt' are the Sample codes - these need to be removed 
row.names(testfamallt) <- NULL
#View(testfamallt)

##Change class of object 'testfamallt' from matrix to dataframe
testfamallt2=as.data.frame(testfamallt)
#class(testfamallt2)
#str(testfamallt2)

## Note that data (taxon counts) are factors. Need to convert to numeric
testfamallt2[] <- lapply(testfamallt2, function(x) as.numeric(as.character(x)))
#str(testfamallt2)
names(testfamallt2)

############################
## Create a copy of the data for trial use in Shiny app
dim(testfamallt2)
dim(test.raw2)
names(test.raw2)
output=cbind(test.raw2[1],test.raw2[13536],test.raw2[13537],testfamallt2)
names(output)
write.csv(output,file = "OUTPUTS/ShinyTemplateCompleted.csv",row.names=FALSE)
##########################
## Create a blank template for use with R Shiny
ShinyTemplate=testfamallt2[1,]
ShinyTemplate[ShinyTemplate==""]<-0# change all values to zero
#View(ShinyTemplate)
ShinyTemplate$Sample=NA
ShinyTemplate$Latitude_WGS84=NA
ShinyTemplate$Longitude_WGS84=NA
ShinyTemplate=ShinyTemplate[,c(704:706,1:703)]
names(ShinyTemplate)
write.csv(ShinyTemplate,file = "OUTPUTS/ShinyTemplate.csv",row.names=FALSE)

## Transform the data
testfamallt3=testfamallt2^(0.25)

## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84. NB/You may need to update the colrefs for Lat and Long 
names(test.raw2)
pos.test=test.raw2[,c(1,13536:13537)]

## Check names of df 'pos'
names(pos.test)
dim(pos.test)


#### TEST DATA: GENERATE UNIVARIATE SUMMARY METRICS ####

##Start with df for test faunal data
View(testfamallt2)

## Call library
library(vegan)

## Calculate univariate summary measures based on faunal abundance data in df 'data5'
Richness = specnumber(testfamallt2) # Species Richness(S)
Abundance=rowSums(testfamallt2) # Abundance
## To calculate Pielou's eveness J first calculate the Shannon diversity H
H <- diversity(testfamallt2)
J <- H/log(specnumber(testfamallt2))

univar=rbind(Richness,Abundance,J)
univar_t=t(univar)
#View(univar_t)
univarfinal=cbind(pos.test,univar_t)
#View(univarfinal)
dim(pos.test)# same as df 'univarfinal'

## Now save data
write.csv(univarfinal,file = "OUTPUTS/testunivariateresults.csv",row.names=TRUE)


#### TRAIN DATA: FAUNAL CLUSTER ANALYSIS ####

## Steps 19 and 21 in Cooper & Barry (2017) R Script

## Transform the data (fourth-root transformation)
datat=data6^(0.25)

## Perform Kmeans clusterinig of data. Results (cluster group) to the object 'results'
set.seed(1234)
results=kmeans(datat,12,algorithm="MacQueen",iter.max=100,nstart=25)

## Save object 'resultsA' for use in Shiny app
saveRDS(results, file = "OUTPUTS/results")

#### TRAIN DATA: DISTANCE TO CLUSTER CENTRES ####

## Find distances to cluster centre
#see https://stackoverflow.com/questions/44137906/r-data-output-ordered-by-distance-from-cluster-center
DistancesToCentersp1 <- as.matrix(dist(rbind(results$centers, datat[1:5000,])))[-(1:12),1:12]
DistancesToCentersp2 <- as.matrix(dist(rbind(results$centers, datat[5001:10000,])))[-(1:12),1:12]
DistancesToCentersp3 <- as.matrix(dist(rbind(results$centers, datat[10001:15000,])))[-(1:12),1:12]
DistancesToCentersp4 <- as.matrix(dist(rbind(results$centers, datat[15001:20000,])))[-(1:12),1:12]
DistancesToCentersp5 <- as.matrix(dist(rbind(results$centers, datat[20001:25000,])))[-(1:12),1:12]
DistancesToCentersp6 <- as.matrix(dist(rbind(results$centers, datat[25001:27432,])))[-(1:12),1:12]

## Bring results together
DistancesToCentersAll=rbind(DistancesToCentersp1,DistancesToCentersp2,DistancesToCentersp3,DistancesToCentersp4,DistancesToCentersp5,DistancesToCentersp6)

## Add Sample column
#names(pos)
DistancetoCentersTrain=cbind(pos$Sample,DistancesToCentersAll)
#View(DistancetoCentersTrain)

## Update column names
colnames(DistancetoCentersTrain)=c("Sample","A2a","D2a","B1a","C1a","C1b","D2c","B1b","A2b","D2d","D2b","A1","D1")

## Change column order
DistancetoCentersTrain=DistancetoCentersTrain[,c(1,12,2,9,4,8,5,6,13,3,11,7,10)]
#View(DistancetoCentersTrain)

## Add Survey column
trainsurveynames=train.data4.5[,806]# get Survey names
DistancetoCentersTrain2=cbind(trainsurveynames,DistancetoCentersTrain)
colnames(DistancetoCentersTrain2)[1]="Survey"
#View(DistancetoCentersTrain2)

## Add in Faunal cluster group (1st need to create object 'faunal.cluster' - see below)
DistancetoCentersTrain3=cbind(DistancetoCentersTrain2[,1:2],faunal.cluster$FaunalCluster,DistancetoCentersTrain2[,3:14])
colnames(DistancetoCentersTrain3)[3] <- "FaunalCluster"
#View(DistancetoCentersTrain3)

## Change object 'DistancetoCentersTrain3' from matrix to dataframe
class(DistancetoCentersTrain3) # matrix
DistancetoCentersTrain3=as.data.frame(DistancetoCentersTrain3)

## Add in Phy cluster group from object 'train.phy.output' which inc Sample and PhyCluster
DistancetoCentersTrain3$PhyCluster=NA
DistancetoCentersTrain4 <-merge(DistancetoCentersTrain3, train.phy.output, by="Sample")
DistancetoCentersTrain5=DistancetoCentersTrain4[,c(2,1,3,19,4:15)]
names(DistancetoCentersTrain5)=c("Survey","Sample","FaunalCluster","PhyCluster","A1","A2a","A2b","B1a","B1b" ,"C1a" ,"C1b","D1","D2a","D2b","D2c","D2d")
#View(DistancetoCentersTrain5)
###############
DistancetoCentersTrain5=DistancetoCentersTrain3
#############
## Change column format
DistancetoCentersTrain5$Survey <- as.character(as.character(DistancetoCentersTrain5$Survey))
DistancetoCentersTrain5$Sample <- as.character(as.character(DistancetoCentersTrain5$Sample))
DistancetoCentersTrain5$FaunalCluster <- as.character(as.character(DistancetoCentersTrain5$FaunalCluster))
DistancetoCentersTrain5$PhyCluster <- as.character(as.character(DistancetoCentersTrain5$PhyCluster))
DistancetoCentersTrain5$A1 <- as.numeric(as.character(DistancetoCentersTrain5$A1))
DistancetoCentersTrain5$A2a <- as.numeric(as.character(DistancetoCentersTrain5$A2a))
DistancetoCentersTrain5$A2b <- as.numeric(as.character(DistancetoCentersTrain5$A2b))
DistancetoCentersTrain5$B1a <- as.numeric(as.character(DistancetoCentersTrain5$B1a))
DistancetoCentersTrain5$B1b <- as.numeric(as.character(DistancetoCentersTrain5$B1b))
DistancetoCentersTrain5$C1a <- as.numeric(as.character(DistancetoCentersTrain5$C1a))
DistancetoCentersTrain5$C1b <- as.numeric(as.character(DistancetoCentersTrain5$C1b))
DistancetoCentersTrain5$D1 <- as.numeric(as.character(DistancetoCentersTrain5$D1))
DistancetoCentersTrain5$D2a <- as.numeric(as.character(DistancetoCentersTrain5$D2a))
DistancetoCentersTrain5$D2b <- as.numeric(as.character(DistancetoCentersTrain5$D2b))
DistancetoCentersTrain5$D2c <- as.numeric(as.character(DistancetoCentersTrain5$D2c))
DistancetoCentersTrain5$D2d <- as.numeric(as.character(DistancetoCentersTrain5$D2d))

## Create subsets of the data by cluster group 
A1dist=subset(DistancetoCentersTrain5, FaunalCluster=="A1")
A2adist=subset(DistancetoCentersTrain5, FaunalCluster=="A2a")
A2bdist=subset(DistancetoCentersTrain5, FaunalCluster=="A2b")
B1adist=subset(DistancetoCentersTrain5, FaunalCluster=="B1a")
B1bdist=subset(DistancetoCentersTrain5, FaunalCluster=="B1b")
C1adist=subset(DistancetoCentersTrain5, FaunalCluster=="C1a")
C1bdist=subset(DistancetoCentersTrain5, FaunalCluster=="C1b")
D1dist=subset(DistancetoCentersTrain5, FaunalCluster=="D1")
D2adist=subset(DistancetoCentersTrain5, FaunalCluster=="D2a")
D2bdist=subset(DistancetoCentersTrain5, FaunalCluster=="D2b")
D2cdist=subset(DistancetoCentersTrain5, FaunalCluster=="D2c")
D2ddist=subset(DistancetoCentersTrain5, FaunalCluster=="D2d")


## Take only relevant columns for later calculation of mean and sd (change these nos if inc phy clus grp)
A1dist=A1dist[,1:4]
A2adist=A2adist[,c(1:3,5)]
A2bdist=A2bdist[,c(1:3,6)]
B1adist=B1adist[,c(1:3,7)]
B1bdist=B1bdist[,c(1:3,8)]
C1adist=C1adist[,c(1:3,9)]
C1bdist=C1bdist[,c(1:3,10)]
D1dist=D1dist[,c(1:3,11)]
D2adist=D2adist[,c(1:3,12)]
D2bdist=D2bdist[,c(1:3,13)]
D2cdist=D2cdist[,c(1:3,14)]
D2ddist=D2ddist[,c(1:3,15)]

## Get mean and sd
meanA1dist=mean(A1dist$A1)
sdA1dist=sqrt(var(A1dist$A1))

meanA2adist=mean(A2adist$A2a)
sdA2adist=sqrt(var(A2adist$A2a))

meanA2bdist=mean(A2bdist$A2b)
sdA2bdist=sqrt(var(A2bdist$A2b))

meanB1adist=mean(B1adist$B1a)
sdB1adist=sqrt(var(B1adist$B1a))

meanB1bdist=mean(B1bdist$B1b)
sdB1bdist=sqrt(var(B1bdist$B1b))

meanC1adist=mean(C1adist$C1a)
sdC1adist=sqrt(var(C1adist$C1a))

meanC1bdist=mean(C1bdist$C1b)
sdC1bdist=sqrt(var(C1bdist$C1b))

meanD1dist=mean(D1dist$D1)
sdD1dist=sqrt(var(D1dist$D1))

meanD2adist=mean(D2adist$D2a)
sdD2adist=sqrt(var(D2adist$D2a))

meanD2bdist=mean(D2bdist$D2b)
sdD2bdist=sqrt(var(D2bdist$D2b))

meanD2cdist=mean(D2cdist$D2c)
sdD2cdist=sqrt(var(D2cdist$D2c))

meanD2ddist=mean(D2ddist$D2d)
sdD2ddist=sqrt(var(D2ddist$D2d))

## Create a copy of the data 'DistancetoCentersTrain5'
DistancetoCentersTrain6=DistancetoCentersTrain5

## Calculate z-scores
DistancetoCentersTrain6$zA1=(DistancetoCentersTrain6$A1-meanA1dist)/sdA1dist
DistancetoCentersTrain6$zA2a=(DistancetoCentersTrain6$A2a-meanA2adist)/sdA2adist
DistancetoCentersTrain6$zA2b=(DistancetoCentersTrain6$A2b-meanA2bdist)/sdA2bdist
DistancetoCentersTrain6$zB1a=(DistancetoCentersTrain6$B1a-meanB1adist)/sdB1adist
DistancetoCentersTrain6$zB1b=(DistancetoCentersTrain6$B1b-meanB1bdist)/sdB1bdist
DistancetoCentersTrain6$zC1a=(DistancetoCentersTrain6$C1a-meanC1adist)/sdC1adist
DistancetoCentersTrain6$zC1b=(DistancetoCentersTrain6$C1b-meanC1bdist)/sdC1bdist
DistancetoCentersTrain6$zD1=(DistancetoCentersTrain6$D1-meanD1dist)/sdD1dist
DistancetoCentersTrain6$zD2a=(DistancetoCentersTrain6$D2a-meanD2adist)/sdD2adist
DistancetoCentersTrain6$zD2b=(DistancetoCentersTrain6$D2b-meanD2bdist)/sdD2bdist
DistancetoCentersTrain6$zD2c=(DistancetoCentersTrain6$D2c-meanD2cdist)/sdD2cdist
DistancetoCentersTrain6$zD2d=(DistancetoCentersTrain6$D2d-meanD2ddist)/sdD2ddist
#View(DistancetoCentersTrain6)

## Calculate z-scores percentiles
DistancetoCentersTrain6$pA1=round(pnorm(DistancetoCentersTrain6$zA1)*100,1)
DistancetoCentersTrain6$pA2a=round(pnorm(DistancetoCentersTrain6$zA2a)*100,1)
DistancetoCentersTrain6$pA2b=round(pnorm(DistancetoCentersTrain6$zA2b)*100,1)
DistancetoCentersTrain6$pB1a=round(pnorm(DistancetoCentersTrain6$zB1a)*100,1)
DistancetoCentersTrain6$pB1b=round(pnorm(DistancetoCentersTrain6$zB1b)*100,1)
DistancetoCentersTrain6$pC1a=round(pnorm(DistancetoCentersTrain6$zC1a)*100,1)
DistancetoCentersTrain6$pC1b=round(pnorm(DistancetoCentersTrain6$zC1b)*100,1)
DistancetoCentersTrain6$pD1=round(pnorm(DistancetoCentersTrain6$zD1)*100,1)
DistancetoCentersTrain6$pD2a=round(pnorm(DistancetoCentersTrain6$zD2a)*100,1)
DistancetoCentersTrain6$pD2b=round(pnorm(DistancetoCentersTrain6$zD2b)*100,1)
DistancetoCentersTrain6$pD2c=round(pnorm(DistancetoCentersTrain6$zD2c)*100,1)
DistancetoCentersTrain6$pD2d=round(pnorm(DistancetoCentersTrain6$zD2d)*100,1)

## Select only samples belonging to chosen cluster and produce histogram
#D2cclusdist=DistancetoCentersTrain6[ which(DistancetoCentersTrain6$FaunalCluster=='D2c'& DistancetoCentersTrain6$PhyCluster==1 ),]
D2cclusdist=DistancetoCentersTrain6[ which(DistancetoCentersTrain6$FaunalCluster=='D2c'),]
hist(D2cclusdist$zD2c,breaks=40)

## Create a csv for train sample distance/zscore/percentile for use in shiny app. 
View(DistancetoCentersTrain6)
write.csv(DistancetoCentersTrain6,file = "OUTPUTS/DistancetoCentersTrain6.csv",row.names=F)

## Select stations from survey
#NWJBCentres=DistancetoCentersTrain6[ which(DistancetoCentersTrain6$Survey=='North West Jones Bank_MCZ_infauna'& DistancetoCentersTrain6$FaunalCluster=='D2c'),] 
NWJBCentres=DistancetoCentersTrain6[ which(DistancetoCentersTrain6$Survey=='North West Jones Bank_MCZ_infauna'),] 
#View(NWJBCentres)
write.csv(NWJBCentres,file = "OUTPUTS/NWJBCentres.csv",row.names=F)


#### TRAIN DATA: DF FOR FAUNAL CLUSTER MAPS ####

## Step 23 in Cooper & Barry (2017) R Script

## Add cluster group from kmeans results file to df 'pos' which includes 'Sample',
# 'Latitude_WGS84' and 'Longitude_WGS84'
faunal.cluster=cbind(pos,results$cluster)

## Change name of col 'results$cluster' to 'ClusterNum'
names(faunal.cluster)[4]<-paste("ClusterNum")

## Add a new empty col 'FaunalCluster' to df 'faunal.cluster
faunal.cluster["FaunalCluster"]=NA

## Populate FaunalCluster col with new names (see dendrogram from Step 21)
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 11] <- "A1"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 1]<- "A2a"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 8] <- "A2b"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 3]<- "B1a"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 7] <- "B1b"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 4] <- "C1a"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 5] <- "C1b"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 12] <- "D1"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 2] <- "D2a"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 10] <- "D2b"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 6] <- "D2c"
faunal.cluster$FaunalCluster[faunal.cluster$ClusterNum == 9]<- "D2d"

## Save baseline cluster output for use in Shiny
#View(faunal.cluster)
write.csv(faunal.cluster,file = "OUTPUTS/BaselineFunalCluster.csv",row.names=FALSE)

#### TRAIN DATA: MAP OF FAUNAL CLUSTER DISTRIBUTION ####

## Produce map
p2= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=0.45,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.15)+
  scale_colour_manual(values = c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3",
                                 "palegreen1","#b40202","red1","darkorange","yellow",
                                 "#b4b404"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  theme_bw(base_size = 24)+ 
  labs(x="Longitude",y="Latitude")

fig4a=p2+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=6)))

## Save plot to an image file (png or tiff)
png("OUTPUTS/FIGURE C5922 FAUNAL CLUSTERS.png",width = 29.7,height = 42,units = "cm", res = 600,
    pointsize = 48)
#tiff("OUTPUTS/FIGURE 4a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
fig4a
dev.off()

## Save legend for use in Figure 7
legendfclus <- get_legend(p2 + theme_bw(base_size=24)+ guides(colour = guide_legend(override.aes = list(size=8))))
plot(legendfclus)



#### TEST DATA: IDENTIFY FAUNAL CLUSTER GROUPS ####

## Call library
library("flexclust")

## Convert kmeans output to kcca type (this is necessary when using predict function in flexclust
# package).
resultsA=as.kcca(results,datat)

## Save object 'resultsA' for use in Shiny app
saveRDS(resultsA, file = "OUTPUTS/resultsA")
#my_data <- readRDS("OUTPUTS/resultsA")
#my_data

## Check predicted cluster groups are the same as those from clustering of training data set
pred_train <- predict(resultsA)
pred_train # they are

dim(datat)

## Now use predict function to predict cluster groups for test data.
#pred_test <- predict(resultsA, newdata=testfamallt2[,1:703])#703 is the number of variable cols
pred_test <- predict(resultsA, newdata=testfamallt3)#703 is the number of variable cols
pred_test 


#View(pred_test)
## Add cluster group from kmeans results file to df 'pos' which includes 'Sample',
# 'Latitude_WGS84' and 'Longitude_WGS84'
faunal.cluster.test=cbind(pos.test,pred_test)
names(faunal.cluster.test)

## Change name of col 'results$cluster' to 'ClusterNum'
names(faunal.cluster.test)[4]<-paste("ClusterNum")

## Add a new empty col 'FaunalCluster' to df 'faunal.cluster
faunal.cluster.test["FaunalCluster"]=NA

## Populate FaunalCluster col with new names (see dendrogram from Step 21)
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 11] <- "A1"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 1]<- "A2a"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 8] <- "A2b"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 3]<- "B1a"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 7] <- "B1b"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 4] <- "C1a"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 5] <- "C1b"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 12] <- "D1"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 2] <- "D2a"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 10] <- "D2b"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 6] <- "D2c"
faunal.cluster.test$FaunalCluster[faunal.cluster.test$ClusterNum == 9]<- "D2d"

## Note col FaunalCluster is currently a chr - need to covery to a factor
str(faunal.cluster.test)
faunal.cluster.test$FaunalCluster=as.factor(faunal.cluster.test$FaunalCluster)

## Save df for faunal cluster groups
#View(faunal.cluster.test)
#write.csv(faunal.cluster.test,file = "OUTPUTS/testcluster results.csv",row.names=TRUE)


#### TEST DATA: DISTANCE TO CLUSTER CENTRES ####

## Find distances to cluster centres
#https://stackoverflow.com/questions/44137906/r-data-output-ordered-by-distance-from-cluster-center
DistancesToCentersTest <- as.matrix(dist(rbind(results$centers, testfamallt3)))[-(1:12),1:12]
#View(DistancesToCentersTest)

## Add Sample column
names(pos.test)
DistancetoCentersTest=cbind(pos.test$Sample,DistancesToCentersTest)
#View(DistancetoCentersTest)

## Update column names
colnames(DistancetoCentersTest)=c("Sample","A2a","D2a","B1a","C1a","C1b","D2c","B1b","A2b","D2d","D2b","A1","D1")

## Change column order
DistancetoCentersTest=DistancetoCentersTest[,c(1,12,2,9,4,8,5,6,13,3,11,7,10)]
#View(DistancetoCentersTest)

## Change object from matrix to dataframe
class(DistancetoCentersTest)
DistancetoCentersTest=as.data.frame(DistancetoCentersTest)
str(DistancetoCentersTest)

## Add Survey column
testsurveynames=as.character(test.raw2$Survey)
DistancetoCentersTest2=cbind(testsurveynames,DistancetoCentersTest)
colnames(DistancetoCentersTest2)[1]="Survey"
#View(DistancetoCentersTest2)
#class(DistancetoCentersTest2)
#DistancetoCentersTest2=as.data.frame(DistancetoCentersTest2)

## Add column for faunal cluster group
DistancetoCentersTest3=cbind(DistancetoCentersTest2[,1:2],faunal.cluster.test$FaunalCluster,DistancetoCentersTest2[,3:14])
colnames(DistancetoCentersTest3)[3]="FaunalCluster"
#View(DistancetoCentersTest3)

## Create a copy of'DistancetoCentersTrain3'
DistancetoCentersTest4=DistancetoCentersTest3

# Change cols into correct format
DistancetoCentersTest4$Survey <- as.character(as.character(DistancetoCentersTest4$Survey))
DistancetoCentersTest4$Sample <- as.character(as.character(DistancetoCentersTest4$Sample))
DistancetoCentersTest4$FaunalCluster <- as.character(as.character(DistancetoCentersTest4$FaunalCluster))
DistancetoCentersTest4$A1 <- as.numeric(as.character(DistancetoCentersTest4$A1))
DistancetoCentersTest4$A2a <- as.numeric(as.character(DistancetoCentersTest4$A2a))
DistancetoCentersTest4$A2b <- as.numeric(as.character(DistancetoCentersTest4$A2b))
DistancetoCentersTest4$B1a <- as.numeric(as.character(DistancetoCentersTest4$B1a))
DistancetoCentersTest4$B1b <- as.numeric(as.character(DistancetoCentersTest4$B1b))
DistancetoCentersTest4$C1a <- as.numeric(as.character(DistancetoCentersTest4$C1a))
DistancetoCentersTest4$C1b <- as.numeric(as.character(DistancetoCentersTest4$C1b))
DistancetoCentersTest4$D1 <- as.numeric(as.character(DistancetoCentersTest4$D1))
DistancetoCentersTest4$D2a <- as.numeric(as.character(DistancetoCentersTest4$D2a))
DistancetoCentersTest4$D2b <- as.numeric(as.character(DistancetoCentersTest4$D2b))
DistancetoCentersTest4$D2c <- as.numeric(as.character(DistancetoCentersTest4$D2c))
DistancetoCentersTest4$D2d <- as.numeric(as.character(DistancetoCentersTest4$D2d))
#str(DistancetoCentersTest4)

## Calculate z-score
DistancetoCentersTest4$zA1=(DistancetoCentersTest4$A1-meanA1dist)/sdA1dist
DistancetoCentersTest4$zA2a=(DistancetoCentersTest4$A2a-meanA2adist)/sdA2adist
DistancetoCentersTest4$zA2b=(DistancetoCentersTest4$A2b-meanA2bdist)/sdA2bdist
DistancetoCentersTest4$zB1a=(DistancetoCentersTest4$B1a-meanB1adist)/sdB1adist
DistancetoCentersTest4$zB1b=(DistancetoCentersTest4$B1b-meanB1bdist)/sdB1bdist
DistancetoCentersTest4$zC1a=(DistancetoCentersTest4$C1a-meanC1adist)/sdC1adist
DistancetoCentersTest4$zC1b=(DistancetoCentersTest4$C1b-meanC1bdist)/sdC1bdist
DistancetoCentersTest4$zD1=(DistancetoCentersTest4$D1-meanD1dist)/sdD1dist
DistancetoCentersTest4$zD2a=(DistancetoCentersTest4$D2a-meanD2adist)/sdD2adist
DistancetoCentersTest4$zD2b=(DistancetoCentersTest4$D2b-meanD2bdist)/sdD2bdist
DistancetoCentersTest4$zD2c=(DistancetoCentersTest4$D2c-meanD2cdist)/sdD2cdist
DistancetoCentersTest4$zD2d=(DistancetoCentersTest4$D2d-meanD2ddist)/sdD2ddist
#View(DistancetoCentersTest4)

## Calculate z-scores percentiles
DistancetoCentersTest4$pA1=round(pnorm(DistancetoCentersTest4$zA1)*100,1)
DistancetoCentersTest4$pA2a=round(pnorm(DistancetoCentersTest4$zA2a)*100,1)
DistancetoCentersTest4$pA2b=round(pnorm(DistancetoCentersTest4$zA2b)*100,1)
DistancetoCentersTest4$pB1a=round(pnorm(DistancetoCentersTest4$zB1a)*100,1)
DistancetoCentersTest4$pB1b=round(pnorm(DistancetoCentersTest4$zB1b)*100,1)
DistancetoCentersTest4$pC1a=round(pnorm(DistancetoCentersTest4$zC1a)*100,1)
DistancetoCentersTest4$pC1b=round(pnorm(DistancetoCentersTest4$zC1b)*100,1)
DistancetoCentersTest4$pD1=round(pnorm(DistancetoCentersTest4$zD1)*100,1)
DistancetoCentersTest4$pD2a=round(pnorm(DistancetoCentersTest4$zD2a)*100,1)
DistancetoCentersTest4$pD2b=round(pnorm(DistancetoCentersTest4$zD2b)*100,1)
DistancetoCentersTest4$pD2c=round(pnorm(DistancetoCentersTest4$zD2c)*100,1)
DistancetoCentersTest4$pD2d=round(pnorm(DistancetoCentersTest4$zD2d)*100,1)
#View(DistancetoCentersTest4)

# Select stations from survey
#NWJBMonCentres=DistancetoCentersTest4[ which(DistancetoCentersTest4$Survey=='NWJB'& DistancetoCentersTest4$FaunalCluster=='D2c'),] 
NWJBMonCentres=DistancetoCentersTest4[ which(DistancetoCentersTest4$Survey=='NWJB'),] 
#View(NWJBMonCentres)
write.csv(NWJBMonCentres,file = "OUTPUTS/NWJBMonCentres.csv",row.names=F)


#### TEST DATA: SAMPLE LOCATION MAP ####

#names(test.raw2)
#LOW HIGH

## To see colours
scales::show_col(scales::hue_pal()(6)) 
#F8766D
#B79F00
#00BA38
#00BFC4
#619CFF
#F564E3


## Enter box coordinates 
HHW=data.frame(x1=1.6390083, x2=2.2590820,y1=52.65830, y2=52.96870,t=c('a'), r=c("A"))
IDRBNR=data.frame(x1=0.5124109, x2=0.9848433,y1=53.20440, y2=53.28629,t=c('a'), r=c("A"))
NNSB=data.frame(x1=1.6999140, x2=2.4646310,y1=53.03373	, y2=53.71136,t=c('a'), r=c("A"))
NWJB=data.frame(x1=-8.0632254, x2=-8.3109440,y1=49.83386, y2=49.98435,t=c('a'), r=c("A"))
SCRSMP=data.frame(x1=0.6889000, x2=-1.7970300,y1=50.36368, y2=50.80682,t=c('a'), r=c("A"))
U=data.frame(x1=-0.8455141, x2=-0.8887576,y1=50.63572, y2=50.66383,t=c('a'), r=c("A"))

## Produce map
testloc= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_point(data=test.raw2,aes(Longitude_WGS84,Latitude_WGS84,col=Survey),
             size=0.35,show.legend = TRUE)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.15)+
  #guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  geom_rect(data=HHW, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="#F8766D",alpha=0, linetype=1,size=0.7)+# HHW bounding box
  geom_rect(data=IDRBNR, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="#B79F00",alpha=0, linetype=1,size=0.7)+# IDRBNR bounding box
  geom_rect(data=NNSB, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="#00BA38",alpha=0, linetype=1,size=0.7)+# NNSB bounding box
  geom_rect(data=NWJB, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="#00BFC4",alpha=0, linetype=1,size=0.7)+# NWJB bounding box
  geom_rect(data=SCRSMP, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="#619CFF",alpha=0, linetype=1,size=0.7)+# SCRSMP bounding box
  geom_rect(data=U, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="#F564E3",alpha=0, linetype=1,size=0.7)+# U bounding box
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  #coord_map(xlim = c(-1.3, -0.7),ylim = c(50.5, 50.85))+ #set x,y limits of plot#UTOPIA
  theme_bw(base_size = 24)+ 
  labs(x="Longitude",y="Latitude")
#theme(legend.position="bottom")

testloc1=testloc+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=6)))

png("OUTPUTS/FIGURE 1.png",width = 29.7,height = 42,units = "cm", res = 600,
    pointsize = 48)
testloc1
dev.off()


#### TEST DATA: FAUNAL CLUSTER MAP ####

## Identify colours required for map below
levels(faunal.cluster.test$FaunalCluster)#[1] "A1"  "A2a" "A2b" "C1a" "C1b" "D1"  "D2a" "D2b" "D2c" "D2d"
##Colours
#A1 "blue2"
#A2a "cyan1"
#A2b "#05aae1"
#B1a "plum2"
#B1b "darkorchid3"
#C1a "green3"
#C1b "palegreen1"
#D1 "#b40202"
#D2a "red1"
#D2b "darkorange"
#D2c "yellow"
#D2d "#b4b404"


## Produce map
p3= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=0.7,show.legend = TRUE)+#size was 0.45,shape=3
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.15)+
  scale_colour_manual(values = c("blue2","cyan1","#05aae1","green3",
                                 "palegreen1","#b40202","red1","darkorange","yellow",
                                 "#b4b404"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  #coord_map(xlim = c(0, 3),ylim = c(52.5, 54))+ #set x,y limits of plot 
  #coord_map(xlim = c(-1.5, -0.5),ylim = c(50.5, 51))+ #set x,y limits of plot#UTOPIA
  theme_bw(base_size = 24)+
  labs(x="Longitude",y="Latitude")+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank())#axis.title.x=element_blank() ,,axis.text.x=element_blank()


testmap=p3+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=6)))

png("OUTPUTS/FIGURE test data with Utopia.png",width = 29.7,height = 42,units = "cm", res = 600,
    pointsize = 48)
testmap
dev.off()

levels(faunal.cluster.test$FaunalCluster)


#### TRAIN & TEST DATA: MAP OF FAUNAL CLUSTER DISTRIBUTION ####

library(cowplot)

png("OUTPUTS/FIGURE 2.png", width=52, height=37, units="cm", res=400)
plot_grid(fig4a,testmap,labels=c("a)"," b)"),label_size = 24,rel_widths = c(1,1.30),align = "h" )
dev.off()

#################################################################################################
### STEP REF:                                                                                 ###  
###                                                                                           ###
### TASK:           Train (transparent) and Test together                                     ###
###                                                                                           ###
### NOTES:                                                                                    ###
#################################################################################################

## Need to add test colours to bathy colours
#grey1=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","blue2","cyan1","palegreen1","#b40202","red1","yellow","#b4b404")

grey1=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","blue2","cyan1","#05aae1","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404")

## Produce map
p2= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=1,shape=16,alpha=0.3,show.legend = TRUE)+#size=3
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=0.8,shape=21,stroke = 0.3)+ #,show.legend = TRUE size=2
  scale_colour_manual(values = c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3",
                                 "palegreen1","#b40202","red1","darkorange","yellow","#b4b404"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  #coord_map(xlim = c(0, 3),ylim = c(52, 54))+ #set x,y limits of plot
  #coord_map(xlim = c(-1.5, -0.5),ylim = c(50.5, 51))+ #set x,y limits of plot#UTOPIA
  #coord_map(xlim = c(-8.5,-7.5),ylim = c(49.5, 50.3))+ #set x,y limits of plot#NWJB 
  theme_bw(base_size = 20)+ 
  labs(x="Longitude",y="Latitude")

plot1=p2+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=6)))


## Save plot to an image file (png or tiff)
png("OUTPUTS/FIGURE Map of transparent Train and ringed Test samples.png",width = 30,height = 29,units = "cm", res = 600)
#tiff("OUTPUTS/FIGURE 4a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
plot1
dev.off()

#################################################################################################
### STEP REF:                                                                                 ###  
###                                                                                           ###
### TASK:           Train (transparent) and Test together                                     ###
###                                                                                           ###
### NOTES:          Now with prescribed alpha colour for baseline samples                                                                          ###
#################################################################################################

## Identify washed out colours for the 12 faunal groups. Choose a lighter tint (5th from left, bottom row) of the parent colour (http://www.color-hex.com/color/e1e19a). HEX colours for 12 groups are:#0000EE,#00FFFF,#05AAC1,#EEAEEE,#9A32CD,#00CD00,#9AFF9A,#B40202,#FF0000,#FF8C00,#FFFF00,#B4B404

## Need to add test colours to bathy colours
#grey1=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","blue2","cyan1","palegreen1","#b40202","red1","yellow","#b4b404")

#grey1=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","blue2","cyan1","#05aae1", "green3","palegreen1","#b40202","red1","yellow","#b4b404")# slot faunal colours in as required (see: levels(faunal.cluster.test$FaunalCluster) )

grey1=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","blue2","cyan1","#05aae1","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404")

## Produce map
p2= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=3.2,shape=16,show.legend = TRUE)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=3.2,shape=21,stroke = 0.2)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  #coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  #coord_map(xlim = c(0, 3),ylim = c(52, 54))+ #main area
  #coord_map(xlim = c(-8.5,-7.9),ylim = c(49.7, 50.1))+ #NWJB
  #coord_map(xlim = c(0.45,1.10),ylim = c(53.15, 53.35))+ #IDRBNR
  #coord_map(xlim = c(1.5,2.6),ylim = c(52.99, 53.9))+ #NNSB
  #coord_map(xlim = c(-0.8,-0.92),ylim = c(50.60,50.69))+ #U
  #coord_map(xlim = c(0.6,-1.85),ylim = c(50.35,50.85))+ #SCRSMP
  coord_map(xlim = c(1.5,2.35),ylim = c(52.6,53.0))+ #HHW
  theme_bw(base_size = 20)+ 
  labs(x="Longitude",y="Latitude")

plot2=p2+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(alpha=1,size=6)))


## Save plot to an image file (png or tiff)
png("OUTPUTS/FIGURE 3 HHW.png",width = 36,height = 23,units = "cm", res = 600)
#tiff("OUTPUTS/FIGURE 4a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
plot2
dev.off()

#### TRAIN & TEST MAP: IDRBNR ####
IDRBNR= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=F)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=5,shape=16,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=5,shape=21,stroke = 0.4)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(0.45,1.10),ylim = c(53.15, 53.35))+ #IDRBNR
  theme_bw(base_size = 20)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())# axis.text.y=element_blank(),,axis.text.x=element_blank()

IDRBNR


#### TRAIN & TEST MAP: HHW ####

HHW= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=5,shape=16,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=5,shape=21,stroke = 0.4)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(1.5,2.35),ylim = c(52.6,53.0))+ #HHW
  theme_bw(base_size = 20)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())# axis.text.y=element_blank(),,axis.text.x=element_blank() 

HHW


#### TRAIN & TEST MAP: NNSB ####

NNSB= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=5,shape=16,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=5,shape=21,stroke = 0.4)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(1.5,2.6),ylim = c(52.99, 53.9))+ #NNSB
  theme_bw(base_size = 20)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())# axis.text.y=element_blank(),,axis.text.x=element_blank() 

NNSB


#### TRAIN & TEST MAP: U ####

U= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=5,shape=16,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=5,shape=21,stroke = 0.4)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-0.8,-0.92),ylim = c(50.60,50.69))+ #U
  theme_bw(base_size = 20)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())# axis.text.y=element_blank(),,axis.text.x=element_blank()


U

#### TRAIN & TEST MAP: NWJB ####
NWJB= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=5,shape=16,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=5,shape=21,stroke = 0.4)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-8.5,-7.9),ylim = c(49.7, 50.1))+ #NWJB
  theme_bw(base_size = 20)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())# axis.text.y=element_blank(),,axis.text.x=element_blank()


NWJB


#### TRAIN & TEST MAP: SCRSMP ####

SCRSMP= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey1,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=5,shape=16,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.3)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=FaunalCluster),col="black",size=5,shape=21,stroke = 0.4)+ #,show.legend = TRUE, stroke changes line thickness. sIZE WAS 2.75
  scale_colour_manual(values = c("#9999F8","#99FFFF","#9BDDE6","#F8DEF8","#D6ADEB","#99EB99","#D6FFD6","#E19999","#FF9999","#FFD199","#FFFF99","#E1E19A"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(0.75,-1.85),ylim = c(50.1,51.1))+ #SCRSMP
  theme_bw(base_size = 20)+
  theme(axis.title.y=element_blank(),axis.title.x=element_blank())# axis.text.y=element_blank(),,axis.text.x=element_blank()


SCRSMP

#####################################
library(cowplot)

top=plot_grid(IDRBNR,SCRSMP, labels = c("a)","b)"),nrow=2, align='hv',label_size = 20)
#bl=plot_grid(NNSB,NWJB, labels = c("c)","e)"), align='hv',label_size = 20)
#br=plot_grid(HHW,U, labels = c("d)","f)"), align='hv',label_size = 20)
bl=plot_grid(NNSB,NWJB, labels = c("c)","e)"), align='v',label_size = 20)
br=plot_grid(HHW,U, labels = c("d)","f)"), align='v',label_size = 20)

b=plot_grid(bl,br,ncol=1,align='hv',label_size = 20)
f4=plot_grid(top,b,ncol=1,align='hv',label_size = 20)

f5=plot_grid(NULL,f4,ncol=2, rel_widths=c(0.02,1))
f6=plot_grid(f5,NULL,ncol=1, rel_heights=c(1,0.02))

f7=f6+draw_label("Longitude", x=0.5, y=  0, vjust=-0.5, angle= 0, size=20) +#
  draw_label("Latitude", x= 0, y=0.5, vjust= 1.5, angle=90, size=20)#

png("OUTPUTS/Figure_4.png",width=38, height=80, units="cm", res=400)#res=600
f7
dev.off()

## to get legend 
#http://htmlpreview.github.io/?https://github.com/wilkelab/cowplot/blob/master/inst/doc/shared_legends.html
# LEGEND STORED IN legendfclus

p <- plot_grid(f7, legendfclus, ncol = 2, rel_widths = c(1, 0.1))
png("OUTPUTS/Figure_4.png",width=38, height=80, units="cm", res=400)#res=600
p
dev.off()


#######
bottomleft=plot_grid(HHW,NWJB, labels = c("c)","d)"),nrow=1, align='hv',label_size = 20)
left=plot_grid(SCRSMP,bottomleft,ncol=1, align='v',rel_widths=c(1.5,1),label_size = 20)

bottomright=plot_grid(NNSB,U, labels = c("c)","d)"),nrow=1, align='hv',label_size = 20)
right=plot_grid(IDRBNR,bottomright,ncol=1, align='v',rel_widths=c(1.5,1),label_size = 20)





f6=plot_grid(left,right, nrow=1, align='hv',rel_widths=c(1.5,1),label_size = 20)


f7=f6+draw_label("Longitude", x=0.5, y=  0, vjust=-0.5, angle= 0, size=20) +#
  draw_label("Latitude", x= 0, y=0.5, vjust= 1.5, angle=90, size=20)#


## to get legend 
#http://htmlpreview.github.io/?https://github.com/wilkelab/cowplot/blob/master/inst/doc/shared_legends.html
# LEGEND STORED IN legendfclus

p <- plot_grid(f7, legendfclus, ncol = 2, rel_widths = c(1, 0.1))
png("OUTPUTS/Figure_4v2.png",width=60, height=20, units="cm", res=400)#res=600
p
dev.off()


####################################

#### TRAIN & TEST MAPS TOGETHER #### 
library(cowplot)
plot4left =plot_grid(HHW,NNSB,U,labels = c("a) HHW","b) NNSB","c) U"),nrow = 3,rel_widths = c(1,1,1),label_size = 26,hjust=0.01,vjust=1,align = 'h')#scale = 0.95

png("OUTPUTS/Figure_4.png",width=60, height=80, units="cm", res=400)#res=600
#tiff("OUTPUTS/Figure_4.tif",width=45, height=63, units="cm", res=250,compression = "lzw")

plot4left 
dev.off()

plot4right =plot_grid(NWJB,SCRSMP,IDRBNR,labels = c("d) NWJB","e) SCRSMP","f) IDRBNR"),nrow = 3,rel_widths = c(1,1,1),label_size = 26,hjust=0.01,vjust=1,align = 'h')#scale = 0.95

png("OUTPUTS/Figure_4p2.png",width=60, height=80, units="cm", res=400)#res=600
#tiff("OUTPUTS/Figure_4.tif",width=45, height=63, units="cm", res=250,compression = "lzw")

plot4right 
dev.off()

plot4all=plot_grid(plot4left,plot4right,nrow = 1,rel_widths = c(1,1),label_size = 26,hjust=0.01,vjust=1,align = 'h')#scale = 0.95
png("OUTPUTS/Figure_4all.png",width=60, height=80, units="cm", res=600)#res=600
#tiff("OUTPUTS/Figure_4.tif",width=45, height=63, units="cm", res=250,compression = "lzw")

plot4all 
dev.off()


####################################################################################################
##              NOW DO SAME FOR PHY VARIABLES                                                     ##
####################################################################################################

#################################################################################################
# # #                                  (E) SEDIMENT ANALYSIS                                # # #
#################################################################################################

#################################################################################################
### STEP REF:       42                                                                        ###
###                                                                                           ###
### PAPER SECTION:  METHODS \ Faunal-sediment relationships \ Identification of physical      ###
###                 cluster groups                                                            ###
###                 RESULTS \ Faunal-sediment relationships \ Identification of physical      ###
###                 cluster groups                                                            ###                 
###                                                                                           ###
### TASK:           Prepare data for clustering of physical variables                         ###
#################################################################################################

## Select relevant variables from df 'data4.5' (subset of raw data based on selection criteria
# for: gear, sieve, faunal presence)
names(train.data4.5)

## Check dimensions of df 'data4.5'
dim(train.data4.5)#27432 830

## Create a df 'phy.data' which is the subset of chosen variables (see above) from df 'data4.5
# Variables of interest are: Sample (1), Latitude_WGS84 (807), Longitude_WGS84 (808),Sal (817),
# Temp (818), Chla (819), SPM (820), Depth (821), WOV (822), AvCur (823), Stress (824)
train.phy.data=train.data4.5[,c(1,807,808,817,818,819,820,821,822,823,824)]

## Check selected columns present
names(train.phy.data)

## Change bathy values to positive
train.phy.data$Depth=abs(train.phy.data$Depth)

## Remove rows where there is a value of NA in one of the columns 
train.phy.data.complete=train.phy.data[complete.cases(train.phy.data),]

## Remove variables from df phy.data that won't be used for clustering (i.e. Sample,
# Latitude_WGS84, and Longitude_WGS84)
train.phy.data.final=train.phy.data.complete[,4:11] 

## Check unwanted varaibales removed
names(train.phy.data.final)

## Check dimensions of df 'phy.data.final
dim(train.phy.data.final)#26406 8
View(train.phy.data.final)
## Identify any skewness in the data. Intuitively, the skewness is a measure of symmetry. As a
# rule, negative skewness indicates that the mean of the data values is less than the median,
# and the data distribution is left-skewed. Positive skewness would indicates that the mean of
# the data values is larger than the median, and the data distribution is right-skewed.
summary(train.phy.data.final)# RS (Mean>median) observed for SPM (4), Depth (5), Stress (8)

## Create a copy of df train.phy.data.final
train.phy.data.finalT=train.phy.data.final
View(train.phy.data.finalT)
## Transform relevant columns
train.phy.data.finalT[,c(4,5,8)]=log(train.phy.data.finalT[,c(4,5,8)]+0.1)

## Normalisation of phy variable data (subtract by pop mean and sd)
trainPhy.df <- scale(train.phy.data.finalT)# scale the data 

## Dimensions of df 'df'
dim(trainPhy.df)#26406 8
#View(trainPhy.df)


#################################################################################################
### STEP:           44                                                                        ###
###                                                                                           ###
### PAPER SECTION:  METHODS \ Faunal-sediment relationships \ Identification of physical      ###
###                 cluster groups                                                            ###
###                 RESULTS \ Faunal-sediment relationships \ Identification of physical      ###
###                 cluster groups                                                            ###                       
###                                                                                           ###
### TASK:           K-means clustering of Physical variables data                             ###                 
#################################################################################################

## kmeans clutering
set.seed(1234)
train.phy.results=kmeans(trainPhy.df,10,algorithm="MacQueen",iter.max=100,nstart=25)

## Create a df 'phy.cluster' for the cluster groups
train.phy.cluster=as.data.frame(train.phy.results$cluster)

## Create an object 'phy.output' with sample coordinates and cluster group.
train.phy.output=cbind(train.phy.data.complete$Sample,train.phy.data.complete$Latitude_WGS84,
                       train.phy.data.complete$Longitude_WGS84,train.phy.cluster)

## Reinstate the appropriate column names
colnames(train.phy.output) <- c("Sample", "Latitude_WGS84","Longitude_WGS84","PhyCluster")

## Change variable PhyCluster from an integer to a factor
train.phy.output$PhyCluster=as.factor(train.phy.output$PhyCluster)

## Check names of df 'phy.output'
names(train.phy.output)

## Number of samples belonging to each cluster group
train.phy.results$size# 2472 1732 3818 4299 1344 2108 3093 4788 1750 1002
View(train.phy.output)

#################################################################################################
### STEP REF:       47                                                                        ###
###                                                                                           ###
### PAPER SECTION:  METHODS \ Faunal-sediment relationships \ Identification of physical      ###
###                 cluster groups                                                            ###
###                 RESULTS \ Faunal-sediment relationships \ Identification of physical      ###
###                 cluster groups                                                            ###                        
###                                                                                           ###
### TASK:           Figure 8a (Physical cluster group identity for individual stations)       ###
#################################################################################################

## Produce a map of physical cluster group distributuion
PhyClusMap= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.05)+
  geom_point(data=train.phy.output,aes(Longitude_WGS84,Latitude_WGS84,col=PhyCluster), size=0.45,
             show.legend = TRUE)+
  scale_colour_manual(values=c('#e31a1c','#FF62BC','#fdbf6f','#ff7f00','#FFFF32','#8681E5',
                               '#00BFC4','#A3A500','#1f78b4','#39B600'))+ 
  geom_polygon(data=licence.df, aes(x=long, y=lat, group=group), colour="black",size=0.15,
               fill=NA)+#Licencsed areas
  geom_polygon(data=appl.df, aes(x=long, y=lat, group=group), colour="black",size=0.15,
               fill=NA)+#Application Areas
  geom_polygon(data=goodwinlic.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               fill=NA)+#Goodwin Sands
  geom_polygon(data=humSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Humber SIZs
  geom_polygon(data=angSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Anglian SIZs
  geom_polygon(data=thaSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Thames SIZs
  geom_polygon(data=tha5012SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Thames Area 501/2 SIZ
  geom_polygon(data=scSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#S.Coast SIZs
  geom_polygon(data=eecSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#EEC SIZs
  geom_polygon(data=bcSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Bristol Channel SIZs
  geom_polygon(data=nw457SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Area 457 North West SIZ
  geom_polygon(data=nw392SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Area 392 North West SIZ
  geom_polygon(data=goodwinSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Goodwin SIZ
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62)) + #set x,y limits of plot
  theme_bw(base_size=24)+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  labs(x="Longitude",y="Latitude")#
#facet_wrap(~PhyCluster)# Add this line to facet by PhyCluster

fig8a=PhyClusMap+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=8))) 

## Save plot to an image file (png or tiff)
png("OUTPUTS/FIGURE 8a.png", width = 29.7,height = 42,units = "cm",res = 1000,pointsize = 48)
#tiff("OUTPUTS/FIGURE 8a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
fig8a
dev.off()

##############################################################################################
## Generate the test data for phy variables

## Check col names have changed
names(test.raw2)

## Select only the phy data: Sal(13625),Temp (13626), Chla (13627), SPM(13628),
# Depth (13629), WOV (13630), AvCur (13631), Stress (13632)
test.raw3.phy=test.raw2[,c(1, 13625:13632)]
names(test.raw3.phy)
View(test.raw3.phy)
###############################################################
##  MAKE A COPY OF ENV VARIABLES INC SED DATA ###

##Make acopy of df 'test.raw3.phy' but including sieve data (13515:13613)
test.raw3.phy2=test.raw2[,c(1, 13625:13632, 13515:13613)]

names(test.raw3.phy2)

## Change sieve data 'NA' values to zero. Sieve data in cols 776:784
test.raw3.phy2[, 10:108][is.na(test.raw3.phy2[, 10:108])] <- 0

## Add in cols for Mud, Sand and Gravel
test.raw3.phy2$Mud=rowSums(test.raw3.phy2[70:108])
test.raw3.phy2$Sand=rowSums(test.raw3.phy2[43:69])
test.raw3.phy2$Gravel=rowSums(test.raw3.phy2[42:10])


test.raw3.phy2$CoarseGravel=rowSums(test.raw3.phy2[28:10])
test.raw3.phy2$MediumGravel=rowSums(test.raw3.phy2[33:29])
test.raw3.phy2$FineGravel=rowSums(test.raw3.phy2[42:34])
test.raw3.phy2$CoarseSand=rowSums(test.raw3.phy2[43:51])
test.raw3.phy2$MediumSand=rowSums(test.raw3.phy2[52:58])
test.raw3.phy2$FineSand=rowSums(test.raw3.phy2[59:69])

## Add col for total percent (across sediment sieves - should be 100%)
test.raw3.phy2$Tpercent=rowSums(test.raw3.phy2[109:111])

## Take only the columns of interest (variables used in modelling)
test.raw3.phy3=test.raw3.phy2[,c(1:9,109,118,117,116,115,114,113,110,111)]

## Save df for phy variables
#View(faunal.cluster.test)
write.csv(test.raw3.phy3,file = "OUTPUTS/testphyvariables.csv",row.names=TRUE)

######################################




## Now make sure phy data treated in same way as baseline dataset
## Change bathy values to positive
test.raw3.phy$Depth=abs(test.raw3.phy$Depth)

names(test.raw3.phy)

## Create a copy of df train.phy.data.final
test.raw3.phyT=test.raw3.phy

## Transform relevant columns SPM (5), Depth (6), Stress (9)
test.raw3.phyT[,c(5,6,9)]=log(test.raw3.phyT[,c(5,6,9)]+0.1)

View(test.raw3.phyT)
str(test.raw3.phyT)

## Remove the sample column
test.raw3.phyT2=test.raw3.phyT[,2:9]
View(test.raw3.phyT2)
dim(test.raw3.phyT2)#440 8

## Normalise test data. Need to do this using pop mean and sd (i.e. baseline data)

## Get variable means from baseline dataset
sapply(train.phy.data.finalT, mean, na.rm=TRUE)

## Get variable sd from baseline dataset
sapply(train.phy.data.finalT, sd, na.rm=TRUE)

#Sal       Temp       Chla        SPM      Depth        WOV      AvCur     Stress 
#34.4981225 11.0206515  1.8128280  1.4654568  3.2614150  0.5464542  0.4634078  1.3932260 

#Sal      Temp      Chla       SPM     Depth       WOV     AvCur    Stress 
#0.3613594 1.2568787 0.9668017 0.9284768 0.8065650 0.2082228 0.1732323 0.9398125 

## Normalise data by subtracting pop mean and dividing by sd. This is the same as using
# function 'scale', but allowing means and sd to be specified
SalMean=34.4981225
SalSD=0.3613594
test.raw3.phyT2$Sal=(test.raw3.phyT2$Sal-SalMean)/SalSD

TempMean=11.0206515
TempSD=1.2568787
test.raw3.phyT2$Temp=(test.raw3.phyT2$Temp-TempMean)/TempSD

ChlaMean=1.8128280
ChlaSD=0.9668017
test.raw3.phyT2$Chla=(test.raw3.phyT2$Chla-ChlaMean)/ChlaSD

SPMMean=1.4654568
SPMSD=0.9284768
test.raw3.phyT2$SPM=(test.raw3.phyT2$SPM-SPMMean)/SPMSD

DepthMean=3.2614150
DepthSD=0.8065650
test.raw3.phyT2$Depth=(test.raw3.phyT2$Depth-DepthMean)/DepthSD

WOVMean=0.5464542
WOVSD=0.2082228
test.raw3.phyT2$WOV=(test.raw3.phyT2$WOV-WOVMean)/WOVSD

AvCurMean=0.4634078
AvCurSD=0.1732323
test.raw3.phyT2$AvCur=(test.raw3.phyT2$AvCur-AvCurMean)/AvCurSD

StressMean=1.3932260
StressSD=0.9398125
test.raw3.phyT2$Stress=(test.raw3.phyT2$Stress-StressMean)/StressSD

testPhy.df=test.raw3.phyT2
View(testPhy.df)

#################################################################################################
### STEP REF:       16                                                                        ###  
###                                                                                           ###
### TASK:           Identify Phy groups for test samples (predict::Flexclust)              ###
###                                                                                           ###
### NOTES:                                                                                     ###
#################################################################################################

## Call library
library("flexclust")

## Convert kmeans output to kcca type (this is necessary when using predict function in flexclust
# package).
resultsPhy=as.kcca(train.phy.results,trainPhy.df)

## Check predicted cluster groups are the same as those from clustering of training data set
pred_trainPhy <- predict(resultsPhy)
pred_trainPhy # they are



## Now use predict function to predict cluster groups for test data.
pred_testPhy <- predict(resultsPhy, testPhy.df[,1:8])
pred_testPhy 

View(pred_testPhy)

## Add cluster group from kmeans results file to df 'pos' which includes 'Sample',
# 'Latitude_WGS84' and 'Longitude_WGS84'
Phy.cluster.test=cbind(pos.test,pred_testPhy)
names(Phy.cluster.test)
View(Phy.cluster.test)

## Change name of col 'results$cluster' to 'ClusterNum'
names(Phy.cluster.test)[4]<-paste("PhyCluster")

## Change Phy.cluster.test$PhyCluster from an integer to a factor
str(Phy.cluster.test)
Phy.cluster.test$PhyCluster=as.factor(Phy.cluster.test$PhyCluster)


levels(Phy.cluster.test$PhyCluster)
## Produce a map of physical cluster group distributuion
PhyClusMap= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.05)+
  geom_point(data=Phy.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,col=PhyCluster), size=0.45,
             show.legend = TRUE)+
  #  scale_colour_manual(values=c('#e31a1c','#FF62BC','#fdbf6f','#ff7f00','#FFFF32','#8681E5',
  #                               '#00BFC4','#A3A500','#1f78b4','#39B600'))+ 
  scale_colour_manual(values=c('#fdbf6f','#00BFC4','#A3A500'))+   
  
  geom_polygon(data=licence.df, aes(x=long, y=lat, group=group), colour="black",size=0.15,
               fill=NA)+#Licencsed areas
  geom_polygon(data=appl.df, aes(x=long, y=lat, group=group), colour="black",size=0.15,
               fill=NA)+#Application Areas
  geom_polygon(data=goodwinlic.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               fill=NA)+#Goodwin Sands
  geom_polygon(data=humSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Humber SIZs
  geom_polygon(data=angSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Anglian SIZs
  geom_polygon(data=thaSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Thames SIZs
  geom_polygon(data=tha5012SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Thames Area 501/2 SIZ
  geom_polygon(data=scSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#S.Coast SIZs
  geom_polygon(data=eecSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#EEC SIZs
  geom_polygon(data=bcSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Bristol Channel SIZs
  geom_polygon(data=nw457SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Area 457 North West SIZ
  geom_polygon(data=nw392SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Area 392 North West SIZ
  geom_polygon(data=goodwinSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Goodwin SIZ
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62)) + #set x,y limits of plot
  theme_bw(base_size=24)+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  labs(x="Longitude",y="Latitude")#
#facet_wrap(~PhyCluster)# Add this line to facet by PhyCluster

fig8a=PhyClusMap+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=8))) 

## Save plot to an image file (png or tiff)
png("OUTPUTS/test phy clusters.png", width = 29.7,height = 42,units = "cm",res = 1000,pointsize = 48)
#tiff("OUTPUTS/FIGURE 8a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
fig8a
dev.off()


#################################################################################################
### STEP REF:                                                                                 ###  
###                                                                                           ###
### TASK:           Map of Phy cluster identity for baseline and test samples                 ###
###                                                                                           ###
### NOTES:          Test samples as filled circles                                            ###
#################################################################################################

## Need to add test colours to bathy colours (0,10,100,1000,20,200,2000,3(PhyClus),30,40,50,500,7(PhyClus),8(PhyClus))
grey2=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#fdbf6f","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","#00BFC4","#A3A500")


## Produce a map of physical cluster group distributuion
PhyClusMap= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey2,name="Depth (m)",guide=FALSE)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.05)+
  
  geom_point(data=train.phy.output,aes(Longitude_WGS84,Latitude_WGS84,col=PhyCluster), size=0.45,alpha=0.3,
             show.legend = TRUE)+  
  geom_point(data=Phy.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=PhyCluster), col="black",size=2,shape=21,stroke = 0.3)+  
  scale_colour_manual(values=c('#e31a1c','#FF62BC','#fdbf6f','#ff7f00','#FFFF32','#8681E5',
                               '#00BFC4','#A3A500','#1f78b4','#39B600'))+ 
  
  geom_polygon(data=licence.df, aes(x=long, y=lat, group=group), colour="black",size=0.15,
               fill=NA)+#Licencsed areas
  geom_polygon(data=appl.df, aes(x=long, y=lat, group=group), colour="black",size=0.15,
               fill=NA)+#Application Areas
  geom_polygon(data=goodwinlic.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               fill=NA)+#Goodwin Sands
  geom_polygon(data=humSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Humber SIZs
  geom_polygon(data=angSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Anglian SIZs
  geom_polygon(data=thaSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Thames SIZs
  geom_polygon(data=tha5012SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Thames Area 501/2 SIZ
  geom_polygon(data=scSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#S.Coast SIZs
  geom_polygon(data=eecSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#EEC SIZs
  geom_polygon(data=bcSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Bristol Channel SIZs
  geom_polygon(data=nw457SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Area 457 North West SIZ
  geom_polygon(data=nw392SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Area 392 North West SIZ
  geom_polygon(data=goodwinSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.15,
               linetype=2,fill=NA)+#Goodwin SIZ
  #coord_map(xlim = c(-10.7, 4),ylim = c(48, 62)) + #set x,y limits of plot
  coord_map(xlim = c(0, 3),ylim = c(52, 54))+ #set x,y limits of plot 
  theme_bw(base_size=24)+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  labs(x="Longitude",y="Latitude")#
#facet_wrap(~PhyCluster)# Add this line to facet by PhyCluster

fig8a=PhyClusMap+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=8))) 

## Save plot to an image file (png or tiff)
png("OUTPUTS/test phy clusters.png", width = 29.7,height = 42,units = "cm",res = 1000,pointsize = 48)
#tiff("OUTPUTS/FIGURE 8a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
fig8a
dev.off()

#################################################################################################
### STEP REF:                                                                                 ###  
###                                                                                           ###
### TASK:           Map of Phy cluster identity for baseline and test samples                 ###
###                                                                                           ###
### NOTES:          Test samples as crosses                                                   ###
#################################################################################################

## Produce a map of physical cluster group distributuion
PhyClusMap= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.2)+
  
  geom_point(data=train.phy.output,aes(Longitude_WGS84,Latitude_WGS84,col=PhyCluster), size=0.7,alpha=0.25,
             show.legend = TRUE)+  
  #  geom_point(data=Phy.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,fill=PhyCluster), col="black",size=2,shape=21,stroke = 0.3)+  
  geom_point(data=Phy.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,col=PhyCluster), size=3,shape=3)+   
  
  scale_colour_manual(values=c('#e31a1c','#FF62BC','#fdbf6f','#ff7f00','#FFFF32','#8681E5',
                               '#00BFC4','#A3A500','#1f78b4','#39B600'))+ 
  
  geom_polygon(data=licence.df, aes(x=long, y=lat, group=group), colour="black",size=0.2,
               fill=NA)+#Licencsed areas
  geom_polygon(data=appl.df, aes(x=long, y=lat, group=group), colour="black",size=0.2,
               fill=NA)+#Application Areas
  geom_polygon(data=goodwinlic.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               fill=NA)+#Goodwin Sands
  geom_polygon(data=humSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Humber SIZs
  geom_polygon(data=angSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Anglian SIZs
  geom_polygon(data=thaSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Thames SIZs
  geom_polygon(data=tha5012SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Thames Area 501/2 SIZ
  geom_polygon(data=scSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#S.Coast SIZs
  geom_polygon(data=eecSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#EEC SIZs
  geom_polygon(data=bcSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Bristol Channel SIZs
  geom_polygon(data=nw457SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Area 457 North West SIZ
  geom_polygon(data=nw392SIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Area 392 North West SIZ
  geom_polygon(data=goodwinSIZ.df, aes(x=long, y=lat, group=group), colour="black", size=0.2,
               linetype=2,fill=NA)+#Goodwin SIZ
  #coord_map(xlim = c(-10.7, 4),ylim = c(48, 62)) + #set x,y limits of plot
  coord_map(xlim = c(0, 3),ylim = c(52, 54))+ #set x,y limits of plot 
  theme_bw(base_size=14)+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  labs(x="Longitude",y="Latitude")#
#facet_wrap(~PhyCluster)# Add this line to facet by PhyCluster

fig8a=PhyClusMap+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=8))) 

## Save plot to an image file (png or tiff)
png("OUTPUTS/baseline and test - cross - phy clusters.png", width = 24,height = 21,units = "cm",res = 1000)
#tiff("OUTPUTS/FIGURE 8a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
fig8a
dev.off()