# R Summarize percent disturbance of habitat types X NSR for polygon tool

library(Matrix)

# New version to use each separate soil type, with a different structure to the transition matrix.  Old version using coarse soil types is below
q<-load("C:/Dave/ABMI/Data/Km2 grid/2018/veg-hf_transitions_v6hf2016v3noDistVeg.Rdata")  # The monster (sparse) matrix
trSoil<-as.array(trSoil)
rm(trVeg)
gc()
# Load NSR info for each km2 pixel
km2infofile<-"C:/Dave/ABMI/Data/Km2 grid/2017/kgrid_table_km.Rdata"   # 1km2 grid info from Daiyuan processed by Peter - lat long, natural region, subregion, climate variables, etc.
load(km2infofile)  # kgrid with info for km2 rasters
names(kgrid)[which(names(kgrid)=="POINT_X")]<-"Long"
names(kgrid)[which(names(kgrid)=="POINT_Y")]<-"Lat"
kgrid<-data.frame(LinkID=rownames(kgrid),kgrid[,c("Lat","Long","NRNAME","NSRNAME")])
names(kgrid)[which(names(kgrid)=="NRNAME")]<-"NR"  # Simpler and consistent with last year
names(kgrid)[which(names(kgrid)=="NSRNAME")]<-"NSR"
i<-match(rownames(trSoil),kgrid$LinkID)
sum(1:length(i) - i)  # Check that pixels are in same order - should be 0

# Get rid of rows that are all unknown soils (or unknown and water)
unk.col<-which(regexpr("UNK",colnames(trSoil))>0)
water.col<-which(regexpr("Water->",colnames(trSoil))>0 | colnames(trSoil)=="Water")  # Avoiding colnames that have "Water" as part of HF name
i<-which(rowSums(trSoil[,-c(unk.col,water.col)])==0)
trSoil<-trSoil[-i,]
kgrid<-kgrid[-i,]
# And select only parkland and grassland NSRs
Region<-ifelse(kgrid$NSR %in% c("Peace River Parkland","Central Parkland","Northern Fescue","Dry Mixedgrass","Foothills Fescue","Foothills Parkland","Mixedgrass"),"South","North")
trSoil<-trSoil[Region=="South",]
kgrid<-kgrid[Region=="South",]

nsr.list<-sort(unique(as.character(kgrid$NSR)))
# Sort ch2soil (lookup for column names in trSoil) in same order as trSoil
ch2soil<-ch2soil[match(colnames(trSoil),rownames(ch2soil)),]

# New file, detailed soils: Summarize percent disturbed and percent of total provincial area for each soil type in each NSR
# Total area of each soil type
area.total.S1<-apply(trSoil,2,sum)/1000000 # km2
area.total.S<-by(area.total.S1,ch2soil$rf,sum)
# NSR summaries
# First, disturbance level for each column of trSoil
HFwt<-read.csv("C:/Dave/ABMI/Misc/Polygon tool/HF impact/Disturbance weightings of HF NEW.csv")  # Make sure that this includes all HF type in the cr column of ch2soil
HFwt$HF<-as.character(HFwt$HF)
pc.dist<-HFwt$pcDist[match(ch2soil$cr,HFwt$HF)]
pc.dist<-ifelse(is.na(pc.dist),0,pc.dist)  # Check that all NA's are associated with no-HF (non-) transitions
soil.list<-sort(unique(colnames(trSoil)[pc.dist==0]))
# Then summarize percent disturbed for each soil type * NSR
NSR.dist.S<-NSR.pc.area.S<-array(0,c(length(soil.list),length(nsr.list)))  # pc weighted disturbance and percent of total provincial area by soil, NSR
rownames(NSR.dist.S)<-rownames(NSR.pc.area.S)<-soil.list
colnames(NSR.dist.S)<-colnames(NSR.pc.area.S)<-nsr.list
for (i in 1:length(nsr.list)) {  # Go through each NSR
  for (j in 1:length(soil.list)) {  # and soil type
    t1<-trSoil[kgrid$NSR==nsr.list[i],ch2soil$rf==soil.list[j]]
    pc.dist1<-pc.dist[ch2soil$rf==soil.list[j]]
    t1.wt<-t(t(t1)*pc.dist1/100)  # Area weighted by pc disturbed
    NSR.dist.S[j,i]<-sum(t1.wt)/sum(t1)*100
    NSR.pc.area.S[j,i]<-sum(t1)/1000000/area.total.S[match(soil.list[j],names(area.total.S))]*100
  }
}
# Total provincial pc disturbed
Prov.dist.S<-NULL
for (j in 1:length(soil.list)) {  # and soil type
  t1<-trSoil[,ch2soil$rf==soil.list[j]]
  pc.dist1<-pc.dist[ch2soil$rf==soil.list[j]]
  t1.wt<-t(t(t1)*pc.dist1/100)  # Area weighted by pc disturbed
  Prov.dist.S[j]<-sum(t1.wt)/sum(t1)*100
}
names(Prov.dist.S)<-soil.list
# Average NSR and province disturbed (unless <1% of province total area, then just use province)
Ave.dist.S<-array(NA,dim(NSR.dist.S))
rownames(Ave.dist.S)<-rownames(NSR.dist.S)
colnames(Ave.dist.S)<-colnames(NSR.dist.S)
for (i in 1:dim(NSR.dist.S)[2]) {
  Ave.dist.S[,i]<-ifelse(NSR.pc.area.S[,i]>1,(NSR.dist.S[,i]+Prov.dist.S)/2,Prov.dist.S)  # Average NSR and provincial percent disturbed if soil type has >1% of provincial area in that NSR; otherwise, just use provincial percent disturbed
}
# Convert to 0-100% by NSR and overall
x<-ifelse(NSR.pc.area.S>1,Ave.dist.S,0)  # Don't use types that are rare in NSR for maximum - otherwise, these just reflect provincial averages of types not even found in the NSR
Ave.dist.scaled.NSR.S<-Ave.dist.S/rep(apply(x,2,max),each=nrow(Ave.dist.S))*100  # Each NSR is scaled 0-100 independently.  Will be >100 for some rare types - changed below
Ave.dist.scaled.NSR<-ifelse(Ave.dist.scaled.NSR.S>100,100,Ave.dist.scaled.NSR.S)  # And truncate those high rare types
Ave.dist.scaled.S<-Ave.dist.S/max(Ave.dist.S)*100  # This one just uses the overall provincial maximum for scaling

# Output tables and save
write.table(Ave.dist.scaled.NSR.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC Disturbed scaled by NSR NEW DETAILED SOIL.csv",sep=",",col.names=NA)
write.table(Ave.dist.scaled.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC Disturbed scaled by province NEW DETAILED SOIL.csv",sep=",",col.names=NA)
write.table(Ave.dist.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC Disturbed unscaled NEW DETAILED SOIL.csv",sep=",",col.names=NA)
write.table(NSR.pc.area.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC of provincial area by NSR NEW DETAILED SOIL.csv",sep=",",col.names=NA)
save(file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/R objects PC disturbed for polygon tool South  NEW DETAILED SOIL.Rdata",NSR.dist.S,NSR.pc.area.S,Ave.dist.scaled.NSR.S,Ave.dist.scaled.S,trSoil,ch2soil,kgrid)


# The original summaries using separate transition matrices for each NSR * LUF unit start here.  First Veg for North, then Soils for South (but only broad soils types)

dir1<-"C:/Dave/ABMI/Misc/Polygon tool/HF impact/transitions_km2014/"  # Rdata monster matrix summaries from Peter
files<-dir(dir1)
# Pull out NSR
j<-regexpr("_",files)  # Location of separator of NSR name
NSR<-substr(files,j+1,nchar(files)-6)
NSR.list<-unique(NSR)
Region<-ifelse(NSR %in% c("PeaceRiverParkland","CentralParkland","NorthernFescue","DryMixedgrass","FoothillsFescue","FoothillsParkland","Mixedgrass"),"South","North")

HFgroupfile<-"C:/Dave/ABMI/Data/Site info/2016/lookup-hf-class.csv"  # Lookup table for HF to HF group from Peter
HFgl<-read.csv(HFgroupfile)  # HF group lookup

HFwt<-read.csv("C:/Dave/ABMI/Misc/Polygon tool/HF impact/Disturbance weightings of HF.csv")

# North region
# Pull out habitats, HF groups - assume the same for all files
fname<-paste(dir1,files[1],sep="")  # Assuming all files have the same colnames
load(fname)
x<-data.frame(as.matrix(trVeg))
q<-names(x)
hab<-rep(q[1:53],length(q)/53)  # Check if the habitat types change.  Assumes habitats repeat regularly for each HF type
j<-regexpr("\\..",q)  # Location of separator of habitat .. HF
HF<-substr(q,j+2,nchar(q))
HF1<-NULL
for (i in 1:length(HF)) {
  if (HF[i] %in% hab) {  # With no ".." in colname, these are just the habitat types
    HF1[i]<-"None"
  } else {  # These have HF transitions
    HF1[i]<-as.character(HFgl$UseInAnalysis[match(HF[i],HFgl$HF_GROUP)])  # Grouped HF
    HF1[i]<-ifelse(substr(HF[i],1,2)=="CC",HF[i],HF1[i])  # Keep forestry as is - with cut type and age
  }
}
files.north<-files[Region=="North"]

# Go through each file and extract habXHF matrix
NSR.north<-NSR[Region=="North"]
t.north<-array(0,c(length(unique(NSR.north)),length(unique(hab)),length(unique(HF1))))
dimnames(t.north)<-list(sort(unique(NSR.north)),sort(unique(hab)),sort(unique(HF1)))
for (f in 1:length(files.north)) {
  fname<-paste(dir1,files.north[f],sep="")  # Assuming all files have the same colnames
  load(fname)
  x<-data.frame(as.matrix(trVeg))
  x.sum<-colSums(x)
  t<-xtabs(x.sum~hab+HF1)
  t.north[NSR.north[f],,]<-t.north[NSR.north[f],,]+t
}

# Summarize percent disturbed and percent of total provincial area for each habitat type in each NSR
# Total area of each habitat type
area.total<-apply(t.north,2,sum)/1000000 # km2
# NSR summaries
pc.dist<-HFwt$pcDist[match(dimnames(t.north)[[3]],HFwt$HF)]
NSR.dist<-NSR.pc.area<-array(0,c(dim(t.north)[2],dim(t.north)[1]))  # pc weighted disturbance and percent of total provincial area by habitat, NSR
rownames(NSR.dist)<-rownames(NSR.pc.area)<-dimnames(t.north)[[2]]
colnames(NSR.dist)<-colnames(NSR.pc.area)<-dimnames(t.north)[[1]]
for (i in 1:dim(t.north)[1]) {  # Go through each NSR
  t1<-t.north[i,,]
  t1.wt<-t(t(t1)*pc.dist/100)  # Area weighted by pc disturbed
  NSR.dist[,i]<-rowSums(t1.wt)/rowSums(t1)*100
  NSR.pc.area[,i]<-rowSums(t1)/1000000/area.total*100
}
# Total provincial pc disturbed
t1.prov<-apply(t.north,c(2,3),sum)
t1.prov.wt<-t(t(t1.prov)*pc.dist/100)  # Area weighted by pc disturbed
Prov.dist<-rowSums(t1.prov.wt)/rowSums(t1.prov)*100
# Average NSR and provnince disturbed (unless <1% of province total area, then just use province)
Ave.dist<-array(NA,dim(NSR.dist))
rownames(Ave.dist)<-rownames(NSR.dist)
colnames(Ave.dist)<-colnames(NSR.dist)
for (i in 1:dim(NSR.dist)[2]) {
  Ave.dist[,i]<-ifelse(NSR.pc.area[,i]>1,(NSR.dist[,i]+Prov.dist)/2,Prov.dist)  # Average NSR and provincial percent disturbed if habitat type has >1% of provincial area in that NSR; otherwise, just use provincial percent disturbed
}
# Smooth age classes
stand.types<-c("BlackSpruce","Deciduous","Mixedwood","Pine","WhiteSpruce")
ages<-c("R","1","2","3","4","5","6","7","8")
age.group<-list(0:2,-1:2,-2:2,-2:2,-2:2,-2:2,-2:2,-2:1,-2:0)  # Age classes to use in smoothing average for each class.  These are offsets from the target class
Smooth.dist<-Ave.dist
# Calculate running average of age class and the two adjacent classes
for (i in 1:length(stand.types)) {
  for (j in 1:length(ages)) {
    q<-0
    for (k in 1:length(age.group[[j]])) {  # Add up each age group in the smoothing window
      hab2<-paste(stand.types[i],ages[j+age.group[[j]][k]],sep="")
      q<-q+Ave.dist[hab2,]
    }
    q<-q/length(age.group[[j]])  # And take average
    hab2<-paste(stand.types[i],ages[j],sep="")  # The age to assign that average to
    Smooth.dist[hab2,]<-q
  }  # Next age
}  # Next stand type
# Convert to 0-100% by NSR and overall
x<-ifelse(NSR.pc.area>1,Smooth.dist,0)  # Don't use types that are rare in NSR for maximum - otherwise, these just reflect provincial averages of types not even found in the NSR
Smooth.dist.scaled.NSR<-Smooth.dist/rep(apply(x,2,max),each=nrow(Smooth.dist))*100  # Each NSR is scaled 0-100 independently.  Will be >100 for some rare types - changed below
Smooth.dist.scaled.NSR<-ifelse(Smooth.dist.scaled.NSR>100,100,Smooth.dist.scaled.NSR)  # And truncate those high rare types
Smooth.dist.scaled<-Smooth.dist/max(Smooth.dist)*100  # This one just uses the overall provincial maximum for scaling

# South region - soils
# Pull out soils, HF groups - assume the same for all files
fname<-paste(dir1,files[30],sep="")  # Assuming all files have the same colnames.  Using a parkland area
load(fname)
x<-data.frame(as.matrix(trSoil))
q<-names(x)
soil<-rep(q[1:6],length(q)/6)  # Check if the habitat types change.  Assumes habitats repeat regularly for each HF type
j<-regexpr("\\..",q)  # Location of separator of habitat .. HF
HF<-substr(q,j+2,nchar(q))
HF1<-NULL
for (i in 1:length(HF)) {
  if (HF[i] %in% soil) {  # With no ".." in colname, these are just the soil types
    HF1[i]<-"None"
  } else {  # These have HF transitions
    HF1[i]<-as.character(HFgl$UseInAnalysis[match(HF[i],HFgl$HF_GROUP)])  # Grouped HF
    HF1[i]<-ifelse(substr(HF[i],1,2)=="CC",HF[i],HF1[i])  # Keep forestry as is - with cut type and age - but none in south anyway
  }
}
files.south<-files[Region=="South"]

# Go through each file and extract soilXHF matrix
NSR.south<-NSR[Region=="South"]
t.south<-array(0,c(length(unique(NSR.south)),length(unique(soil)),length(unique(HF1))))
dimnames(t.south)<-list(sort(unique(NSR.south)),sort(unique(soil)),sort(unique(HF1)))
for (f in 1:length(files.south)) {
  fname<-paste(dir1,files.south[f],sep="")  # Assuming all files have the same colnames
  load(fname)
  x<-data.frame(as.matrix(trSoil))
  x.sum<-colSums(x)
  t<-xtabs(x.sum~soil+HF1)
  t.south[NSR.south[f],,]<-t.south[NSR.south[f],,]+t
}

# Summarize percent disturbed and percent of total provincial area for each soil type in each NSR
# Total area of each soil type
area.total.S<-apply(t.south,2,sum)/1000000 # km2
# NSR summaries
pc.dist<-HFwt$pcDist[match(dimnames(t.south)[[3]],HFwt$HF)]
NSR.dist.S<-NSR.pc.area.S<-array(0,c(dim(t.south)[2],dim(t.south)[1]))  # pc weighted disturbance and percent of total provincial area by soil, NSR
rownames(NSR.dist.S)<-rownames(NSR.pc.area.S)<-dimnames(t.south)[[2]]
colnames(NSR.dist.S)<-colnames(NSR.pc.area.S)<-dimnames(t.south)[[1]]
for (i in 1:dim(t.south)[1]) {  # Go through each NSR
  t1<-t.south[i,,]
  t1.wt<-t(t(t1)*pc.dist/100)  # Area weighted by pc disturbed
  NSR.dist.S[,i]<-rowSums(t1.wt)/rowSums(t1)*100
  NSR.pc.area.S[,i]<-rowSums(t1)/1000000/area.total.S*100
}
# Total provincial pc disturbed
t1.prov<-apply(t.south,c(2,3),sum)
t1.prov.wt<-t(t(t1.prov)*pc.dist/100)  # Area weighted by pc disturbed
Prov.dist.S<-rowSums(t1.prov.wt)/rowSums(t1.prov)*100
# Average NSR and province disturbed (unless <1% of province total area, then just use province)
Ave.dist.S<-array(NA,dim(NSR.dist.S))
rownames(Ave.dist.S)<-rownames(NSR.dist.S)
colnames(Ave.dist.S)<-colnames(NSR.dist.S)
for (i in 1:dim(NSR.dist.S)[2]) {
  Ave.dist.S[,i]<-ifelse(NSR.pc.area.S[,i]>1,(NSR.dist.S[,i]+Prov.dist.S)/2,Prov.dist.S)  # Average NSR and provincial percent disturbed if soil type has >1% of provincial area in that NSR; otherwise, just use provincial percent disturbed
}
# Convert to 0-100% by NSR and overall
x<-ifelse(NSR.pc.area.S>1,Ave.dist.S,0)  # Don't use types that are rare in NSR for maximum - otherwise, these just reflect provincial averages of types not even found in the NSR
Ave.dist.scaled.NSR.S<-Ave.dist.S/rep(apply(x,2,max),each=nrow(Ave.dist.S))*100  # Each NSR is scaled 0-100 independently.  Will be >100 for some rare types - changed below
Ave.dist.scaled.NSR<-ifelse(Ave.dist.scaled.NSR.S>100,100,Ave.dist.scaled.NSR.S)  # And truncate those high rare types
Ave.dist.scaled.S<-Ave.dist.S/max(Ave.dist.S)*100  # This one just uses the overall provincial maximum for scaling

# Output tables and save
write.table(Smooth.dist.scaled.NSR,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/North PC Disturbed smoothed scaled by NSR.csv",sep=",",col.names=NA)
write.table(Smooth.dist.scaled,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/North PC Disturbed smoothed scaled by province.csv",sep=",",col.names=NA)
write.table(Smooth.dist,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/North PC Disturbed smoothed unscaled.csv",sep=",",col.names=NA)
write.table(NSR.pc.area,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/North PC of provincial area by NSR.csv",sep=",",col.names=NA)
write.table(Ave.dist.scaled.NSR.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC Disturbed scaled by NSR.csv",sep=",",col.names=NA)
write.table(Ave.dist.scaled.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC Disturbed scaled by province.csv",sep=",",col.names=NA)
write.table(Ave.dist.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC Disturbed unscaled.csv",sep=",",col.names=NA)
write.table(NSR.pc.area.S,file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/Outputs/South PC of provincial area by NSR.csv",sep=",",col.names=NA)
save(file="C:/Dave/ABMI/Misc/Polygon tool/HF impact/R objects PC disturbed for polygon tool.Rdata",NSR.dist,NSR.dist.S,NSR.pc.area,NSR.pc.area.S,Smooth.dist.scaled.NSR,Smooth.dist.scaled,Ave.dist.scaled.NSR.S,Ave.dist.scaled.S,t.north,t.south)

