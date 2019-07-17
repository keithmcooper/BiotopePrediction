####### R SCRIPT FOR ANALYSIS OF DATA IN COOPER (2019) #######

##  Reference: Cooper, K.M. (2019) A new machine learning approach to seabed biotope classification. Science Advances

## Required Files:
# 1. BiotopePredictionScript.R (i.e. this script. Available from https://doi.org/10.14466/CefasDataHub.72).
# 2. EUROPE.shp (European Coastline) (Available from https://doi.org/10.14466/CefasDataHub.72)
# 3. EuropeLiteScoWal.shp (European Coastline with UK boundaries) (https://doi.org/10.14466/CefasDataHub.72)
# 4. DEFRADEMKC8.shp (Seabed bathymetry) (Available from https://doi.org/10.14466/CefasDataHub.72)
# 5. C5922DATASETFAM13022017.csv (Training dataset) (Available from https://doi.org/10.14466/CefasDataHub.34)
# 6. PARTC16112018.csv (Test dataset) (Available from https://doi.org/10.14466/CefasDataHub.72)
# 7. PARTCAGG16112018.csv (Aggregation data) (Available from https://doi.org/10.14466/CefasDataHub.72)

## Required folder structure:
# C:\BiotopePrediction
#         \R 
#         \DATA
#         \OUTPUTS

## Folder Contents: 
# C:\FINAL\R (file 1) 
# C:\FINAL\DATA (files 2:7)
# C:\FINAL\OUTPUTS (.png and .csv files resulting from script)

## Notes: Ths script matches new macrofaunal data to the existing macrofaunal cluster groups identified in Cooper and Barry (2017) and will generate figures and table contents in Cooper (2019). The script is divided into 22 individual steps. 

## Set working directory
setwd('C:/Users/kmc00/OneDrive - CEFAS/R_PROJECTS/BiotopePrediction')

## Call packages
library(ggplot2)
library(rgdal)
library(maptools)
library(plyr)
library(sf)
library(methods)
library(cowplot)



#### 1. PREPARE MAPPING LAYERS ####

## Produce a high definition european coast map
# Load shapefile
eu <- readOGR("DATA","EUROPE")
eu@data$id <- rownames(eu@data)

## Create a data.frame from our spatial object
euDF <- fortify(eu, region = "id")

## Merge the "fortified" data with the data from our spatial object. Object euDF is a dataframe of the polygon coordinates. This is combined with the other attribute data from object eu
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
defra.dem.df$Depth = factor(defra.dem.df$Depth,levels(defra.dem.df$Depth)[c(1,2,5,8,9,10,3,6,11,4,7)])
levels(defra.dem.df$Depth)

## Create grey scale colour palette for use with bathy fill. 0=black, 1=white
grey=grey.colors(11, start = 0.95, end = 0.5,  alpha = NULL,gamma = 2.2)



####  2. TRAIN DATA: LOAD ####

## Bring in training data file
train.data=read.csv("DATA/C5922DATASETFAM13022017.csv", header=T,na.strings=c("NA", "-","?","<null>"),stringsAsFactors=F,check.names=FALSE)

## Remove 1st col
train.data[1] <- NULL    

## Check dataset dimensions
dim(train.data)#33198 900

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



#### 3. TRAIN DATA: PREPARE ####

## Prepare baseline data for faunal analysis - subset of comparable samples. This is Step 16 in Cooper & Barry (2017) R Script

## Subset data by gear (all 0.1m2 grabs)
train.data2 = subset(train.data, Gear=="MHN" | Gear=="DG" | Gear=="VV" | Gear=="SM")

## Check dimensions of df 'train.data2'
dim(train.data2)# 32044 901

## subset by sieve size (1mm only)
train.data3 = subset(train.data2, Sieve=='1')

## Check dimensions of df 'train.data3'
dim(train.data3)# 27622 901

## Check names of df 'train.data3'to identify faunal data columns
names(train.data3) # 2:775

## Remove samples from the faunal data (df train.data3) where no fauna present
train.data4 = train.data3[ rowSums(train.data3[,2:775])!=0, ]

## Check dimensions of df 'train.data4'
dim(train.data4)#27432 901

## Identify (usinig the .csv file) variables with no abundance
#write.csv(data4,file = "OUTPUTS/data4.csv",row.names=TRUE)

## Remove variables with no abund (got list from csv file). There will be taxa
# with no abund as some samples with taxa present have been deleted in gear step above  
train.data4.5 <- train.data4[-c(4,21,32,40,47,57,100,153,157,167,172,175,177,200,204,207,213,237,246,
                                252,283,294,313,325,362,376,379,381,410,413,414,417,418,426,428,430,446,
                                448,459,460,469,474,485,486,489,505,508,509,528,556,558,575,578,598,601,
                                603,605,607,630,633,638,647,675,687,705,709,726,734,760,765,775)]

## Check dimensions of df 'train.data4.5'
dim(train.data4.5)# 27432 830

## Show names of df 'train.data4.5'
names(train.data4.5)  

## Faunal subset (ie remove Sample,Latitude_WGS84, Longitude_WGS84, month and year)
train.data5=train.data4.5[,2:704]

## Check dimensions of df 'train.data5'
dim(train.data5) #27432 703

## Check df 'train.data5' is just the faunal data
names(train.data5)# it is

## Change class of df 'train.data5' to a matrix
data6=data.matrix(train.data5)

## Create a df 'pos' for Sample, Latitude_WGS84 and Longitude_WGS84 
pos=train.data4.5[,c(1,807:808)]

## Check names of df 'pos'
names(pos)



#### 4. TRAIN DATA: FAUNAL CLUSTER ANALYSIS ####

## Steps 19 and 21 in Cooper & Barry (2017) R Script

## Transform the data (fourth-root transformation)
datat=data6^(0.25)

## Perform Kmeans clusterinig of data. Results (cluster group) to the object 'results'
set.seed(1234)
results=kmeans(datat,12,algorithm="MacQueen",iter.max=100,nstart=25)

## Save object 'resultsA' for use in Shiny app
saveRDS(results, file = "OUTPUTS/results")



#### 5. TRAIN DATA: DF FOR FAUNAL CLUSTER MAPS ####

## Add cluster group from kmeans results file to df 'pos' which includes 'Sample',
# 'Latitude_WGS84' and 'Longitude_WGS84'
faunal.cluster=cbind(pos,results$cluster)

## Change name of col 'results$cluster' to 'ClusterNum'
names(faunal.cluster)[4]<-paste("ClusterNum")

## Add a new empty col 'FaunalCluster' to df 'faunal.cluster'
faunal.cluster["FaunalCluster"]=NA

## Populate FaunalCluster col with new names (see faunal dendrogram in Cooper and Barry, 2017 (step 21))
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
write.csv(faunal.cluster,file = "OUTPUTS/BaselineFunalCluster.csv",row.names=FALSE)



#### 6. TRAIN DATA: MAP OF FAUNAL CLUSTER DISTRIBUTION ####

## Produce map
p2= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=0.45,show.legend = T)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.15)+
  scale_colour_manual(values = c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3",
                                 "palegreen1","#b40202","red1","darkorange","yellow",
                                 "#b4b404"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  theme_bw(base_size = 24)+ 
  labs(x="Longitude",y="Latitude")

fig1a=p2+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=6)))

## Save plot to an image file (png or tiff)
#png("OUTPUTS/FIGURE 2.png",width = 29.7,height = 42,units = "cm", res = 600,pointsize = 48)
#tiff("OUTPUTS/FIGURE 4a.tiff",width = 29.7,height = 42,units = "cm",res = 600,pointsize = 48)
fig1a
#dev.off()

## Save legend for use in Figure 4 see http://htmlpreview.github.io/?https://github.com/wilkelab/cowplot/blob/master/inst/doc/shared_legends.html)
legendfclus <- get_legend(p2 + theme_bw(base_size=24)+ guides(colour = guide_legend(override.aes = list(size=8))))
plot(legendfclus)



#### 7. TRAIN DATA: DISTANCE TO CLUSTER CENTRES ####

## Find distances to cluster centres
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
DistancetoCentersTrain=cbind(pos$Sample,DistancesToCentersAll)

## Update column names
colnames(DistancetoCentersTrain)=c("Sample","A2a","D2a","B1a","C1a","C1b","D2c","B1b","A2b","D2d","D2b","A1","D1")

## Change column order
DistancetoCentersTrain=DistancetoCentersTrain[,c(1,12,2,9,4,8,5,6,13,3,11,7,10)]

## Add Survey column
trainsurveynames=train.data4.5[,806]# get Survey names
DistancetoCentersTrain2=cbind(trainsurveynames,DistancetoCentersTrain)
colnames(DistancetoCentersTrain2)[1]="Survey"

## Add in Faunal cluster group (1st need to create object 'faunal.cluster' - see below)
DistancetoCentersTrain3=cbind(DistancetoCentersTrain2[,1:2],faunal.cluster$FaunalCluster,DistancetoCentersTrain2[,3:14])
colnames(DistancetoCentersTrain3)[3] <- "FaunalCluster"

## Change object 'DistancetoCentersTrain3' from matrix to dataframe
class(DistancetoCentersTrain3) # matrix
DistancetoCentersTrain3=as.data.frame(DistancetoCentersTrain3)

## Change column format
DistancetoCentersTrain3$Survey <- as.character(as.character(DistancetoCentersTrain3$Survey))
DistancetoCentersTrain3$Sample <- as.character(as.character(DistancetoCentersTrain3$Sample))
DistancetoCentersTrain3$FaunalCluster <- as.character(as.character(DistancetoCentersTrain3$FaunalCluster))
DistancetoCentersTrain3$A1 <- as.numeric(as.character(DistancetoCentersTrain3$A1))
DistancetoCentersTrain3$A2a <- as.numeric(as.character(DistancetoCentersTrain3$A2a))
DistancetoCentersTrain3$A2b <- as.numeric(as.character(DistancetoCentersTrain3$A2b))
DistancetoCentersTrain3$B1a <- as.numeric(as.character(DistancetoCentersTrain3$B1a))
DistancetoCentersTrain3$B1b <- as.numeric(as.character(DistancetoCentersTrain3$B1b))
DistancetoCentersTrain3$C1a <- as.numeric(as.character(DistancetoCentersTrain3$C1a))
DistancetoCentersTrain3$C1b <- as.numeric(as.character(DistancetoCentersTrain3$C1b))
DistancetoCentersTrain3$D1 <- as.numeric(as.character(DistancetoCentersTrain3$D1))
DistancetoCentersTrain3$D2a <- as.numeric(as.character(DistancetoCentersTrain3$D2a))
DistancetoCentersTrain3$D2b <- as.numeric(as.character(DistancetoCentersTrain3$D2b))
DistancetoCentersTrain3$D2c <- as.numeric(as.character(DistancetoCentersTrain3$D2c))
DistancetoCentersTrain3$D2d <- as.numeric(as.character(DistancetoCentersTrain3$D2d))


#### 8. TRAIN DATA: SUBSET DISTANCES BY CLUSTER GROUP ####


## Start by making subsets of the train data (train1, train2, train3 etc). Also find number of samples in each cluster (l1, l2, l3 etc)
use = results$cluster == 1  # Boolean true/false sample belongs to cluster group 1
train1 = datat[use,] # train data for samples belonging to cluster group 1
l1 = sum(use) #number of samples in cluster grp 1 (707)

use = results$cluster == 2
train2 = datat[use,]
l2 = sum(use)

use = results$cluster == 3
train3 = datat[use,]
l3 = sum(use)

use = results$cluster == 4
train4 = datat[use,]
l4 = sum(use)

use = results$cluster == 5
train5 = datat[use,]
l5 = sum(use)

use = results$cluster == 6            
train6 = datat[use,]
l6 = sum(use)

use = results$cluster == 7
train7 = datat[use,]
l7 = sum(use)

use = results$cluster == 8
train8 = datat[use,]
l8 = sum(use)

use = results$cluster == 9
train9 = datat[use,]
l9 = sum(use)

use = results$cluster == 10
train10 = datat[use,]
l10 = sum(use)

use = results$cluster == 11
train11 = datat[use,]
l11 = sum(use)

use = results$cluster == 12
train12 = datat[use,]
l12 = sum(use)

## Create empty numeric vector to hold distances
distsfor1 = rep(0,l1) # empty numeric vector for number of samples in cluster 1
distsfor2 = rep(0,l2)
distsfor3 = rep(0,l3)
distsfor4 = rep(0,l4)
distsfor5 = rep(0,l5)
distsfor6 = rep(0,l6)
distsfor7 = rep(0,l7)
distsfor8 = rep(0,l8)
distsfor9 = rep(0,l9)
distsfor10 = rep(0,l10)
distsfor11 = rep(0,l11)
distsfor12 = rep(0,l12)

## Find distances to cluster centre (sum of the squared distances) and store in above objects
## Function to work out train distances for each cluster group

f1 = function(x,z) {
  # z = results$centers[1,]
  # x = train1[j,]
  a = sum((z - x)^2)
  a
}

distsfor1 = apply(train1, 1, f1, z=results$centers[1,]) # vector of distances for cluster 1 (there re 707 of these)
distsfor2 = apply(train2, 1, f1, z=results$centers[2,])
distsfor3 = apply(train3, 1, f1, z=results$centers[3,])
distsfor4 = apply(train4, 1, f1, z=results$centers[4,])
distsfor5 = apply(train5, 1, f1, z=results$centers[5,])
distsfor6 = apply(train6, 1, f1, z=results$centers[6,])
distsfor7 = apply(train7, 1, f1, z=results$centers[7,])
distsfor8 = apply(train8, 1, f1, z=results$centers[8,])
distsfor9 = apply(train9, 1, f1, z=results$centers[9,])
distsfor10 = apply(train10, 1, f1, z=results$centers[10,])
distsfor11 = apply(train11, 1, f1, z=results$centers[11,])
distsfor12 = apply(train12, 1, f1, z=results$centers[12,])

##Save distances for use in app
#View(distsfor12)
write.csv(distsfor1,file = "OUTPUTS/distsfor1.csv",row.names=F)
write.csv(distsfor2,file = "OUTPUTS/distsfor2.csv",row.names=F)
write.csv(distsfor3,file = "OUTPUTS/distsfor3.csv",row.names=F)
write.csv(distsfor4,file = "OUTPUTS/distsfor4.csv",row.names=F)
write.csv(distsfor5,file = "OUTPUTS/distsfor5.csv",row.names=F)
write.csv(distsfor6,file = "OUTPUTS/distsfor6.csv",row.names=F)
write.csv(distsfor7,file = "OUTPUTS/distsfor7.csv",row.names=F)
write.csv(distsfor8,file = "OUTPUTS/distsfor8.csv",row.names=F)
write.csv(distsfor9,file = "OUTPUTS/distsfor9.csv",row.names=F)
write.csv(distsfor10,file = "OUTPUTS/distsfor10.csv",row.names=F)
write.csv(distsfor11,file = "OUTPUTS/distsfor11.csv",row.names=F)
write.csv(distsfor12,file = "OUTPUTS/distsfor12.csv",row.names=F)



#### 9. TRAIN DATA: PERCENTILES ####


dim(datat)
trainpercentile = rep(0,27432) # numeric vector of 0's length 27432 (train set)

for (j in 1:27432) {
  traincluster = results$cluster[j]  # assiged train sample cluster groups 
  # loop through train cluster group and get distances to cluster centre of assigned group
  if (traincluster==1) {
    distfortrain = sum((results$centers[1,] - datat[j,])^2)
    combined = c(distfortrain, distsfor1) }
  
  if (traincluster==2) {
    distfortrain = sum((results$centers[2,] - datat[j,])^2)
    combined = c(distfortrain, distsfor2) }
  
  if (traincluster==3) {
    distfortrain = sum((results$centers[3,] - datat[j,])^2)
    combined = c(distfortrain, distsfor3) }
  
  if (traincluster==4) {
    distfortrain = sum((results$centers[4,] - datat[j,])^2)
    combined = c(distfortrain, distsfor4) }
  
  if (traincluster==5) {
    distfortrain = sum((results$centers[5,] - datat[j,])^2)
    combined = c(distfortrain, distsfor5) }
  
  if (traincluster==6) {
    distfortrain = sum((results$centers[6,] - datat[j,])^2)
    combined = c(distfortrain, distsfor6) }
  
  if (traincluster==7) {
    distfortrain = sum((results$centers[7,] - datat[j,])^2)
    combined = c(distfortrain, distsfor7) }
  
  if (traincluster==8) {
    distfortrain = sum((results$centers[8,] - datat[j,])^2)
    combined = c(distfortrain, distsfor8) }
  
  if (traincluster==9) {
    distfortrain = sum((results$centers[9,] - datat[j,])^2)
    combined = c(distfortrain, distsfor9) }
  
  if (traincluster==10) {
    distfortrain = sum((results$centers[10,] - datat[j,])^2)
    combined = c(distfortrain, distsfor10) }
  
  if (traincluster==11) {
    distfortrain = sum((results$centers[11,] - datat[j,])^2)
    combined = c(distfortrain, distsfor11) }
  
  if (traincluster==12) {
    distfortrain = sum((results$centers[12,] - datat[j,])^2)
    combined = c(distfortrain, distsfor12) }
  
  ## rank of sample divided by total number of samples in cluster *100
  ranktrain = rank(combined)[1]
  trainpercentile[j] = round(100*(ranktrain - 0.5) / length(combined), 1)
  
}

View(trainpercentile) # percetile = near 100% means that your test percentile is near to the most extreme of the originals in that cluster

## DF for assigned cluster group and percentile
trainresults = as.data.frame(cbind(results$cluster, trainpercentile))
View(trainresults)

## Swap cluster numbers for codes
## Populate FaunalCluster col with new names (see dendrogram from Step 21)
trainresults$V1[trainresults$V1 == 11] <- "A1"
trainresults$V1[trainresults$V1 == 1]<- "A2a"
trainresults$V1[trainresults$V1 == 8] <- "A2b"
trainresults$V1[trainresults$V1 == 3]<- "B1a"
trainresults$V1[trainresults$V1 == 7] <- "B1b"
trainresults$V1[trainresults$V1 == 4] <- "C1a"
trainresults$V1[trainresults$V1 == 5] <- "C1b"
trainresults$V1[trainresults$V1 == 12] <- "D1"
trainresults$V1[trainresults$V1 == 2] <- "D2a"
trainresults$V1[trainresults$V1 == 10] <- "D2b"
trainresults$V1[trainresults$V1 == 6] <- "D2c"
trainresults$V1[trainresults$V1 == 9]<- "D2d"

## Note col FaunalCluster is currently a chr - need to covert to a factor
trainresults$V1=as.factor(trainresults$V1)
colnames(trainresults)[1]="Cluster"
colnames(trainresults)[2]="Percentile"
str(trainresults) # TRAIN DATA CLUSTER GROUP AND ASSOCIATED PERCENTILE


## CreatE a df equivalent to DistanceToClusterCentres6.csv
## Need to add gear to object 'DistancetoCentersTrain6'
str(DistancetoCentersTrain3)
names(train.data4.5)
DistancetoCentersTrain4=cbind(DistancetoCentersTrain3,trainresults$Percentile,train.data4.5[809])
names(DistancetoCentersTrain4)

## Update column name for 'Percentile'
colnames(DistancetoCentersTrain4)[16] <- "Percentile"

## Update gear names
DistancetoCentersTrain4$Gear=as.character(DistancetoCentersTrain4$Gear)# change to character
DistancetoCentersTrain4$Gear[DistancetoCentersTrain4$Gear=="DG"] <- "0.1m2 Day Grab"
DistancetoCentersTrain4$Gear[DistancetoCentersTrain4$Gear=="MHN"] <- "0.1m2 Hamon Grab"
DistancetoCentersTrain4$Gear[DistancetoCentersTrain4$Gear=="VV"] <- "0.1m2 Van Veen Grab"
DistancetoCentersTrain4$Gear[DistancetoCentersTrain4$Gear=="SM"] <- "0.1m2 Smith McIntyre Grab"

## Create a csv for train sample distance/percentile for use in shiny app. 
write.csv(DistancetoCentersTrain4,file = "OUTPUTS/DistancetoCentersTrain4.csv",row.names=F)

## Select stations from survey
#NWJBCentres=DistancetoCentersTrain6[ which(DistancetoCentersTrain6$Survey=='North West Jones Bank_MCZ_infauna'),] 

## Write NWJB data to csv
#write.csv(NWJBCentres,file = "OUTPUTS/NWJBCentres.csv",row.names=F)



#### 10. TEST DATA: GENERATE ####

## Enter new dataset into PRIMER PART C (see subfolder: data)

## Output test data from Primer as a .txt file. PRIMER 6>File>Save data as>PARTC16112018.txt

## Open file PARTC16112018.txt in Excel and (i) remove 1st row, (ii) remove column between faunal
# data and metadata, and (iii) add header 'Sample' to first column. Save file as PARTC16112018.csv

## Bring in test data
test.raw=read.csv("DATA/PARTC16112018.csv", header=T,na.strings=c("NA", "-","?","<null>"),
                  stringsAsFactors=F,check.names=FALSE)

## Check dataset dimensions
dim(test.raw)# 636 13814

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
dim(test.raw2) # 636 13668

## Get Survey names
test.raw2$SurveyName=as.factor(test.raw2$SurveyName)
levels(test.raw2$SurveyName)

## Add col for brief SurveyName (to use with plots as full names too long)
test.raw2$Survey=as.character(test.raw2$SurveyName)

## Populate FaunalCluster col with new names (see dendrogram from Step 21)
test.raw2$Survey[test.raw2$Survey == "Haisborough, Hammond and Winterton 2016 cSACSCI"] <- "HHW"
test.raw2$Survey[test.raw2$Survey == "Inner Dowsing, Race Bank and North Ridge 2016 cSACSCI"] <- "IDRBNR"
test.raw2$Survey[test.raw2$Survey == "North Norfolk Sandbanks and Saturn Reef 2016  cSACSCI"] <- "NNSB"
test.raw2$Survey[test.raw2$Survey == "NWJB2017"] <- "NWJB"
test.raw2$Survey[test.raw2$Survey == "South Coast Regional Seabed Monitoring Programme 2017/2018"] <- "SCRSMP"
test.raw2$Survey[test.raw2$Survey == "Utopia2016"] <- "U"

## Make test.raw2$Survey a factor
test.raw2$Survey=as.factor(test.raw2$Survey)

## Get number of samples by Survey (Table 1)
library(dplyr)
count=test.raw2%>%group_by(SurveyName)%>%summarise(n=n(),maxLat=max(Latitude_WGS84),minLat=min(Latitude_WGS84),maxLon=max(Longitude_WGS84),minLon=min(Longitude_WGS84))
#View(count)

## Find gear used for different surveys (Table 1)
names(test.raw2)
#View(test.raw2[,c(13669,13539,13540,13541)])

## Select only the faunal data. 
test.raw3=test.raw2[,1:13531]

## Change any NAs to 0
test.raw3[is.na(test.raw3)] <- 0

## Transpose the faunal data (keep column names)
t_test.raw3 = setNames(data.frame(t(test.raw3[,-1])), test.raw3[,1])
#View(t_test.raw3)

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
dim(all2)# note the total number of columns (666)

## Take only the Family and sample columns. Make sure you change the sample col numbers
names(all2)
all2fam=all2[,c(19,31:666)]

## Aggregate data (sum) by family for multiple samples
famabundtest=aggregate(. ~ Family, all2fam, sum)

## Check number of columns has reduced
dim(famabundtest)# 776 families 637 cols

## Find number of families (rows with total of >1)
numfamtest=rowSums(famabundtest[,c(2:637)])
sum(numfamtest > 0)# 311

## Write file and close
#write.csv(famabundtest,file = "DATA/famabundtest.csv",row.names=TRUE)



#### 11. TEST DATA: IDENTIFY FAMILIES NOT PRESENT IN TRAIN DATA ####

## Get taxon names for test data
testfamnames=famabundtest[,1]

## Get taxon names for train data
trainfamnames=colnames(train.data5)

## Find common taxon (family) names
testfamnames[testfamnames %in% trainfamnames]

## Families only in test
uniquetestfam=testfamnames[!(testfamnames %in% trainfamnames)]
uniquetestfam

## Find out if any of the families unique to the test set have abundance >0
numfamtest=rowSums(famabundtest[,c(2:637)])
sum(numfamtest > 0)# 311

## Make df for test fam names and total abund (across all rows)
df1=as.data.frame(cbind(testfamnames,numfamtest))
#View(df1)

## Subset df1 for only unique families in test
df2=df1[df1$testfamnames %in% uniquetestfam,]
df2 # Answer: 2 familes in test set not present in train set (Laonidae = Gastropod (3) and Mimosellidae = Bryozoan (2))
# percentage 2/703=0.0028%



#### 12. TEST DATA: DF WITH SAME FAMILIES AS TRAIN DATA ####

## Note that test data (df famabundtest) has 776 families, whereas train has only 703. Therefore we need to remove the additional families from test data for predict. Note that this doesn't mean the test data has 776 families as many rows (taxa) will have zero abundance.

## First create a dataframe for family names from the train data (df train.data5)
template=colnames(train.data5)# this is a vector
template=as.data.frame(template) # this is a df

## Change name of col 1 from 'template' to 'Family'
colnames(template)[1] <- "Family"
dim(template)# 703 1
#View(template)
#write.csv(template, file="FaunalDataTemplate.csv")# this can be populated manualy if not using code below

## Now need to create a version of the test data that only includes families from the train data. Merge two data frames by Family (Merge keeps only the common columns)
testfamall <- merge(template,famabundtest,by="Family")
#View(testfamall)

## Check merge has worked as expected
dim(testfamall)# should be 703 (taxa) and how ever many samples you have (in this case 636)
#View(testfamall)

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

## Create a copy of the test data for trial use in Shiny app
dim(testfamallt2)
dim(test.raw2)
names(test.raw2)
output=cbind(test.raw2[1],test.raw2[13536],test.raw2[13537],testfamallt2)
names(output)
write.csv(output,file = "OUTPUTS/ShinyTemplateCompleted.csv",row.names=FALSE)

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

## Transform the test data
testfamallt3=testfamallt2^(0.25)

## Create a df 'pos.test' for Sample, Latitude_WGS84 and Longitude_WGS84. NB/You may need to update the colrefs for Lat and Long if using a different test dataset
names(test.raw2)
pos.test=test.raw2[,c(1,13536:13537)]

## Check names of df 'pos'
names(pos.test)
dim(pos.test) #636   3



#### 13. TEST DATA: IDENTIFY FAUNAL CLUSTER GROUPS ####

## Call library
library("flexclust")

## Convert kmeans output to kcca type (this is necessary when using predict function in flexclust
# package).
resultsA=as.kcca(results,datat)

## Save object 'resultsA' for use in Shiny app
saveRDS(resultsA, file = "OUTPUTS/resultsA")

## Check predicted cluster groups are the same as those from clustering of training data set
pred_train <- predict(resultsA)
pred_train # they are

dim(datat)

## Now use predict function to predict cluster groups for test data.
pred_test <- predict(resultsA, newdata=testfamallt3)
pred_test 

## Add cluster group from kmeans results file to df 'pos' which includes 'Sample', 'Latitude_WGS84' and 'Longitude_WGS84'
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



#### 14. TEST DATA: DISTANCE TO CLUSTER CENTRES ####

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



#### 15. TEST DATA: PERCENTILES ####
#### Note that you only need to do the above stuff once for each
#### of the clusters. Then you store it and wait for a test case
#### to turn up.

#testpercentile = rep(0,636) # numeric vector of 0's length 636 (test set)
testpercentile = rep(0,length(pred_test))

#for (j in 1:636) {
  for (j in 1:length(pred_test)) {  
  #testcluster = pred_test_JB[j]
  testcluster = pred_test[j]  # assiged test sample cluster groups 
  # loop through test cluster group and get distances to cluster centre of assigned group
  if (testcluster==1) {
    distfortest = sum((results$centers[1,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor1) }
  
  if (testcluster==2) {
    distfortest = sum((results$centers[2,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor2) }
  
  if (testcluster==3) {
    distfortest = sum((results$centers[3,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor3) }
  
  if (testcluster==4) {
    distfortest = sum((results$centers[4,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor4) }
  
  if (testcluster==5) {
    distfortest = sum((results$centers[5,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor5) }
  
  if (testcluster==6) {
    distfortest = sum((results$centers[6,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor6) }
  
  if (testcluster==7) {
    distfortest = sum((results$centers[7,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor7) }
  
  if (testcluster==8) {
    distfortest = sum((results$centers[8,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor8) }
  
  if (testcluster==9) {
    distfortest = sum((results$centers[9,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor9) }
  
  if (testcluster==10) {
    distfortest = sum((results$centers[10,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor10) }
  
  if (testcluster==11) {
    distfortest = sum((results$centers[11,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor11) }
  
  if (testcluster==12) {
    distfortest = sum((results$centers[12,] - testfamallt3[j,])^2)
    combined = c(distfortest, distsfor12) }
  
  ## rank of sample divided by total number of samples in cluster *100
  ranktest = rank(combined)[1]
  testpercentile[j] = round(100*(ranktest - 0.5) / length(combined), 1)
  
}

####
#### percetile = near 100% means that your test percentile is
#### near to the most extreme of the originals in that cluster
####

testresults = as.data.frame(cbind(pred_test, testpercentile))
str(testresults)

## Swap cluster numbers for codes
## Populate FaunalCluster col with new names (see dendrogram from Step 21)
testresults$pred_test[testresults$pred_test == 11] <- "A1"
testresults$pred_test[testresults$pred_test == 1]<- "A2a"
testresults$pred_test[testresults$pred_test == 8] <- "A2b"
testresults$pred_test[testresults$pred_test == 3]<- "B1a"
testresults$pred_test[testresults$pred_test == 7] <- "B1b"
testresults$pred_test[testresults$pred_test == 4] <- "C1a"
testresults$pred_test[testresults$pred_test == 5] <- "C1b"
testresults$pred_test[testresults$pred_test == 12] <- "D1"
testresults$pred_test[testresults$pred_test == 2] <- "D2a"
testresults$pred_test[testresults$pred_test == 10] <- "D2b"
testresults$pred_test[testresults$pred_test == 6] <- "D2c"
testresults$pred_test[testresults$pred_test == 9]<- "D2d"


## Note col FaunalCluster is currently a chr - need to covert to a factor
testresults$pred_test=as.factor(testresults$pred_test)
str(testresults) # TEST DATA CLUSTER GROUP AND ASSOCIATED PERCENTILE

## Change names of cols in object 'testresults'
names(testresults)[1]<-paste("Cluster")
names(testresults)[2]<-paste("Percentile")
str(testresults)




#### 16. TEST DATA: SAMPLE LOCATION MAP ####

## Identify 6 distinct colours for use in map
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

testloc1



#### 17. TRAIN AND TEST: FAUNAL CLUSTER TRAIN AND TEST SAMPLE LOCATIONS (FIGURE 1) ####

## Save image as .png
png("OUTPUTS/FIGURE 1.png",width=58, height=37, units="cm", res=800)
#tiff("OUTPUTS/FIGURE 4.tiff",width=58, height=37, units="cm", res=800)
plot_grid(fig4a, testloc1, labels = c("a)","b)"),nrow = 1,label_size = 24)  
dev.off()



#### 18. TEST DATA: FAUNAL CLUSTER MAP (FIGURE 3) ####

## 1st create an inset map based on Figure 1a

## Enter box coordinates (bounding box for all test data plus small margin)
testarea=data.frame(x1=-8.4109440, x2=2.5646310,y1=49.73386, y2=53.81136,t=c('a'), r=c("A"))

## Produce map
inset= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=0.1,show.legend = F)+
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.07)+
  scale_colour_manual(values = c("blue2","cyan1","#05aae1","plum2","darkorchid3","green3",
                                 "palegreen1","#b40202","red1","darkorange","yellow",
                                 "#b4b404"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  #bounding box
  geom_rect(data=testarea, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color="black",alpha=0, linetype=1,size=0.5)+#
  coord_map(xlim = c(-10.7, 4),ylim = c(48, 62))+ #set x,y limits of plot
  theme_bw(base_size = 24)+ 
  labs(x="Longitude",y="Latitude")+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.title.x=element_blank() ,axis.text.x=element_blank())+
#theme(axis.ticks = element_blank(),plot.margin = rep(unit(0,"null"),4))
  theme(axis.ticks = element_blank(),plot.margin = margin(0,0,0,0,"cm"))+
  theme(plot.background=element_rect(fill=NA, colour=NA))


inset2=inset+theme(legend.key.size = unit(1, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=6)))

## View inset map
inset2

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


## Produce map showing cluster group identity of test samples
p3= ggplot()+
  geom_polygon(data=defra.dem.df, aes(x=long, y=lat, group=group,fill=Depth))+
  scale_fill_manual(values=grey,name="Depth (m)",guide=FALSE)+
  geom_point(data=faunal.cluster.test,aes(Longitude_WGS84,Latitude_WGS84,col=FaunalCluster),
             size=0.8,show.legend = TRUE)+#size was 0.45,shape=3
  geom_polygon(data = euDF2, aes(x=long, y=lat, group = group),fill="white",colour="black",
               size=0.15)+
  scale_colour_manual(values = c("blue2","cyan1","#05aae1","green3", "palegreen1","#b40202","red1","darkorange","yellow","#b4b404"),name="Cluster")+
  guides(colour = guide_legend(override.aes = list(size=3)))+ # Change size of legend dots
  coord_map(xlim = c(-8.4109440, 2.5646310),ylim = c(49.73386, 53.81136))+ #set x,y limits of plot
  theme_bw(base_size = 18)+
  labs(x="Longitude",y="Latitude")#+

testmap=p3+theme(legend.key.size = unit(0.75, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=5)))

testmap


levels(faunal.cluster.test$FaunalCluster)

## Plot Figure 3 for test sample faunal cluster groups with inset map for baseline data
png(file="OUTPUTS/FIGURE 3.png",w=3300,h=1800, res=300) 
grid.newpage() 
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map 
v2<-viewport(width = 0.7, height = 0.6, x = 0.19, y = 0.665) #plot area for the inset map 
print(testmap,vp=v1)  
print(inset2,vp=v2) 
dev.off() 


#### 19. TRAIN AND TEST SAMPLE TOGETHER ####

## Identify washed out colours for the 12 faunal groups. Choose a lighter tint (5th from left, bottom row) of the parent colour (http://www.color-hex.com/color/e1e19a). HEX colours for 12 groups are:#0000EE,#00FFFF,#05AAC1,#EEAEEE,#9A32CD,#00CD00,#9AFF9A,#B40202,#FF0000,#FF8C00,#FFFF00,#B4B404

## Need to add test colours to bathy colours
grey1=c("#F2F2F2","#EAEAEA","#B8B8B8","#909090","#E1E1E1","#ACACAC", "#808080","#D8D8D8","#CECECE","#C3C3C3","#9F9F9F","blue2","cyan1","#05aae1","green3","palegreen1","#b40202","red1","darkorange","yellow","#b4b404")

## Produce map (change coordinates for different sites)
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

plot2

## Use above code to produce maps for the different test sites (below) 



#### 20. TRAIN & TEST MAP: IDRBNR ####

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



#### 21. TRAIN & TEST MAP: HHW ####

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



#### 22. TRAIN & TEST MAP: NNSB ####

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



#### 23. TRAIN & TEST MAP: U ####

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



#### 24. TRAIN & TEST MAP: NWJB ####

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



#### 25. TRAIN & TEST MAP: SCRSMP ####

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



## 26. PRODUCE SINGLE FIGURE FOR ALL SITES (FIGURE 4)

top=plot_grid(IDRBNR,SCRSMP, labels = c("a)","d)"),nrow=2, align='v',label_size = 22, hjust = 0.05)

bl=plot_grid(NNSB,NWJB, labels = c("b)","c)"), align='v',label_size = 22)
br=plot_grid(HHW,U, labels = c("e)","f)"), align='v',label_size = 22)

b=plot_grid(bl,br,ncol=1,align='hv',label_size = 22)
f4=plot_grid(top,b,ncol=2,align='hv',label_size = 22)

f5=plot_grid(NULL,f4,ncol=2, rel_widths=c(0.06,1))
f6=plot_grid(f5,NULL,ncol=1, rel_heights=c(1,0.06))

f7=f6+draw_label("Longitude", x=0.5, y=  0, vjust=-0.5, angle= 0, size=22) +#
  draw_label("Latitude", x= 0, y=0.5, vjust= 1.5, angle=90, size=22)#

## Add legend (stored in object 'legendfclus')
p <- plot_grid(f7, legendfclus, ncol = 2, rel_widths = c(1, 0.1))

## Save image as .png
png("OUTPUTS/FIGURE 4.png",width=70, height=38, units="cm", res=400)#res=600
p
dev.off()
