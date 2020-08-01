##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir())
###############Section 2: Packages########################################
install.packages('reshape') #For re-structuring data
install.packages('ggplot2'); install.packages('ggpubr') #For plotting
###############Section 3: Libraries#######################################
library(reshape)
library(ggplot2); library(ggpubr)
###############Section 4: Private functions###############################
###############Section 5: Plot and save###################################
dir.create('./Graphs') #Directory for graphs
plot<-TRUE; save<-FALSE

##############Part 1: Reorganise chemical data (10 mins)##################
###############Section 1: In- and output##################################
df<-read.table('D_C4_AllChemicalData.txt',header = T,sep = ',')
###############Section 2: Link par numbers with name######################
v.par.temp<-sort(unique(df$Parnr),decreasing = F); v.par.name<-rep(NA,length(v.par.temp))
for (i in c(1:length(v.par.temp))){
  df.temp<-df[df$Parnr == v.par.temp[i],]
  if(length(unique(df.temp$Unit)) == 1){
    v.par.name[i]<-as.character(df.temp$Unit[1])
  } else {
    message(paste("Issue with par.num = ",v.par.temp[i],sep = ''))
  }
}
df.parname<-cbind.data.frame(v.par.temp,v.par.name); names(df.parname)<-c('ParNr','ParName')
###############Section 3: Change outlook of matrix into dataframe#########
df.chem<-as.data.frame(matrix(nrow = nrow(unique(df[,1:2])),ncol = (2+nrow(df.parname)))); names(df.chem)<-c('Mp','Date',as.character(df.parname$ParName))
v.loc<-unique(df$Mp); w<-1
for (i in c(1:length(v.loc))){
  df.temp<-df[df$Mp == v.loc[i],]; v.dat<-unique(df.temp$Datum)
  for (j in c(1:length(v.dat))){
    df.chem$Mp[w]<-v.loc[i]; df.chem$Date[w]<-v.dat[j]
    df.temp2<-df.temp[df.temp$Datum == v.dat[j],]
    for (k in c(1:nrow(df.temp2))){
      index<-which(colnames(df.chem) == df.temp2$Unit[k])
      df.chem[w,index]<-df.temp2$Waarde[k]
    }
    w<-w+1
  }
}
df.sum<-data.frame(matrix(nrow = ncol(df.chem)-2,ncol = 6)); names(df.sum)<-c('Variable','Min','Max','Mean','Median','Missing')
for (i in c(1:nrow(df.sum))){
  s.var<-names(df.chem)[i+2]
  df.sum$Variable[i]<-s.var; df.sum$Min[i]<-min(df.chem[,names(df.chem) == s.var],na.rm = T); df.sum$Max[i]<-max(df.chem[,names(df.chem) == s.var],na.rm = T)
  df.sum$Mean[i]<-mean(df.chem[,names(df.chem) == s.var],na.rm = T); df.sum$Median[i]<-median(df.chem[,names(df.chem) == s.var],na.rm = T)
  df.sum$Missing[i]<-100*sum(is.na(df.chem[,names(df.chem) == s.var]))/nrow(df.chem)
}
###############Section 4: Plotting and saving#############################
if(plot){
  #####First plot: Data availability per variable and instance
  #Determine available information per variable
  df.prev<-as.data.frame(apply(df.chem[,c(3:ncol(df.chem))],2,function(x) 100*sum(!is.na(x))/nrow(df.chem))); names(df.prev)<-c('Availability')
  df.prev$Variable<-row.names(df.prev); row.names(df.prev)<-c()
  #Sort according to degree availability
  df.prev<-df.prev[order(df.prev$Availability,decreasing = TRUE),]; df.prev$VarNr<-c(1:nrow(df.prev))
  p.InformationVariable<-ggplot(df.prev,aes(x = VarNr, y = Availability)) + geom_hline(yintercept = 50,colour = 'grey',linetype = 'dashed') + geom_area(fill = 'black') + 
    scale_x_continuous('Variable') + scale_y_continuous('Availability (%)',limits = c(0,100)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black'))
  plot(p.InformationVariable)
  #Determine available information per instance
  df.mis<-as.data.frame(apply(df.chem[,c(3:ncol(df.chem))],1,function(x) 100*sum(!is.na(x))/(ncol(df.chem)-2))); names(df.mis)<-c('Availability')
  df.mis$Instance<-df.chem$Mp; row.names(df.mis)<-c()
  #Sort according to degree availability
  df.mis<-df.mis[order(df.mis$Availability,decreasing = TRUE),]; df.mis$InstNr<-c(1:nrow(df.mis))
  p.InformationInstance<-ggplot(df.mis,aes(x = InstNr, y = Availability)) + geom_hline(yintercept = 20,colour = 'grey',linetype = 'dashed') + geom_area(fill = 'black') + 
    scale_x_continuous('Instance') + scale_y_continuous('Availability (%)',limits = c(0,60)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black'))
  plot(p.InformationInstance)
  #Combine plots
  p.InformationChemical<-ggarrange(p.InformationVariable,p.InformationInstance,nrow = 2,align = 'hv',labels = 'AUTO',label.x = 0.085,label.y = 0.975,font.label = list(size = 12))
  plot(p.InformationChemical)
  if(save){
    tiff('./Graphs/F_C4S1_InformationChemicalData.tiff',units = 'mm',width = 160,height = 120,res = 300,pointsize = 7)
    plot(p.InformationChemical)
    dev.off()
  }
  #####Third plot: Annual observations
  v.year<-do.call('rbind',strsplit(as.character(do.call('rbind',strsplit(as.character(df.chem$Date),' '))[,1]),'/'))[,3]
  df.temp.chem<-as.data.frame(table(v.year)); names(df.temp.chem)<-c('Year','Observations')
  p.AnnualChem<-ggplot(df.temp.chem,aes(x = Year,y = Observations)) + 
    geom_col(width = 0.5,fill = 'black') + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black',size = 7),axis.text.x = element_text(angle = 60,vjust = 1,hjust = 1),axis.title = element_text(size = 9))
  plot(p.AnnualChem)
  if(save){
    tiff('./Graphs/F_SI_C4S1_AnnualObservationsChemical.tiff',units = 'mm',width = 160,height = 45,res = 300,pointsize = 7)
    plot(p.AnnualChem)
    dev.off()
  }
}
if(save){
  write.table(df.sum,'D_SI_C4S1_SummaryChemicalInformation.txt',row.names = FALSE,sep = ',')
  write.table(df.chem,'./Graphs/FD_C4S1_InformationChemicalData.txt',row.names = FALSE,sep = ',')
}
###############Section 5: Further cleaning################################
##'Manually' combine data of field and lab
for (i in c(1:nrow(df.chem))){
  if(is.na(df.chem$pH[i])){ df.chem$pH[i]<-df.chem$`pH (field)`[i] }
  if(is.na(df.chem$Conductivity[i])){ df.chem$Conductivity[i]<-df.chem$`Conductivity (field)`[i] }
}
df.chem<-df.chem[,-which(colnames(df.chem) %in% c('pH (field)','Conductivity (field)'))]
##Export data for later
if(save){ write.table(df.chem,file = 'D_C4S1_AllChemicalData_clean.txt',row.names = FALSE,sep = ',') }
###############Section 6: Variable removal################################
rm(df,v.par.temp,v.par.name,df.temp,df.parname,v.loc,v.dat,df.temp2,index,i,j,k,w,s.var,df.prev,p.InformationChemical,v.year,df.temp.chem,p.AnnualChem)

##############Part 3: Reorganise macrophyte data (30 hours)###############
###############Section 1: In- and output##################################
df<-read.table('D_C4_AllMacrophytes.txt',header = T,sep = ',')
###############Section 2: Splitting name strings + adding to original#####
for (i in c(1:nrow(df))){
  v.name<-strsplit(as.character(df$Naam[i])," ",fixed = T)[[1]]
  df$Family[i]<-v.name[1]; df$Species[i]<-v.name[2]
}
###############Section 3: Cleaning by removing unwanted macrophytes#######
##Removal of macrophytes with incomplete Family specification
df<-df[-which(df$Family %in% c('GEEN','x')),]
##Removal of macrophytes without species-specification
df<-df[!is.na(df$Species),] #699489 rows
##Removal of macrophytes classified as algae (-wier)
df<-df[-which(df$Family %in% c('draadwier','Draadwier','Draadwieren','Groenwieren','Bruinwier','Sphaerotilus')),] #692857 rows
##Removal of hybrids (species = 'x')
df<-df[df$Species != 'x',] #692436 rows
##Recombine remaining macrophytes
df$Name<-paste(df$Family,df$Species,sep = ' '); df$Name<-factor(df$Name)
###############Section 4: Change outlook of matrix into dataframe#########
df.bio<-as.data.frame(matrix(nrow = nrow(unique(df[,1:2])),ncol = 2+length(levels(df$Name)))); names(df.bio)<-c('Mp','Date',levels(df$Name))
v.loc<-unique(df$Mp)
w<-1
for (i in c(1:length(v.loc))){ #Adapt to length(v.loc)
  message(paste('Location ',i,' of ',length(v.loc),sep = ''))
  df.temp<-df[df$Mp == v.loc[i],]; v.dat<-unique(df.temp$Datum)
  for (j in c(1:length(v.dat))){
    df.temp2<-df.temp[df.temp$Datum == v.dat[j],]; df.temp2$Name<-factor(df.temp2$Name)
    df.bio$Mp[w]<-v.loc[i]; df.bio$Date[w]<-v.dat[j]
    df.bio[w,-which(names(df.bio) %in% c('Mp','Date'))]<-'Absent' #To check, expected to place absent and replaced by present if so.
    for (k in c(1:length(levels(df.temp2$Name)))){
      index<-which(levels(df.temp2$Name)[k] == names(df.bio))
      df.bio[w,index]<-'Present'
    }
    w<-w+1
  }
} #77200 observations with 1150 variables (1148 macrophytes)
###############Section 5: Plotting and saving#############################
if(plot){
  #####First plot: Data availability
  #Determine available information
  df.prev<-as.data.frame(apply(df.bio[,c(3:ncol(df.bio))],2,function(x) 100*sum(x == 'Present')/nrow(df.bio))); names(df.prev)<-c('Availability')
  df.prev$Macrophyte<-row.names(df.prev); row.names(df.prev)<-c()
  #Sort according to degree availability
  df.prev<-df.prev[order(df.prev$Availability,decreasing = TRUE),]; df.prev$MacrophyteNr<-c(1:nrow(df.prev))
  p.InformationMacrophytes<-ggplot(df.prev,aes(x = MacrophyteNr, y = Availability)) + geom_hline(yintercept = 10,colour = 'grey',linetype = 'dashed') + geom_area(fill = 'black') + 
    scale_x_continuous('Macrophyte',breaks = c(0,250,500,750,1000)) + scale_y_continuous('Availability (%)',limits = c(0,20)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black'))
  plot(p.InformationMacrophytes)
  if(save){
    tiff('./Graphs/F_C4S1_InformationMacrophyteData.tiff',units = 'mm',width = 160,height = 60,res = 300,pointsize = 7)
    plot(p.InformationMacrophytes)
    dev.off()
  }
  #####Second plot: Annual observations
  v.year<-do.call('rbind',strsplit(as.character(do.call('rbind',strsplit(as.character(df.bio$Date),' '))[,1]),'/'))[,3]
  df.temp.bio<-as.data.frame(table(v.year)); names(df.temp.bio)<-c('Year','Observations')
  p.AnnualBio<-ggplot(df.temp.bio,aes(x = Year,y = Observations)) + 
    geom_col(width = 0.5,fill = 'black') + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black',size = 7),axis.text.x = element_text(angle = 60,vjust = 1,hjust = 1),axis.title = element_text(size = 9))
  plot(p.AnnualBio)
  if(save){
    tiff('./Graphs/F_SI_C4S1_AnnualObservationsBiological.tiff',units = 'mm',width = 160,height = 45,res = 300,pointsize = 7)
    plot(p.AnnualBio)
    dev.off()
  }
}
if(save){
  write.table(df.bio,file = './Graphs/FD_C4S1_InformationMacrophyteData.txt',row.names = FALSE,sep = ',')
}
###############Section 6: Further cleaning################################
##Remove macrophytes without any observation
df.bio<-as.data.frame(df.bio[,which(apply(df.bio,2,function(x) sum(x == 'Absent') != nrow(df.bio)))])
##Export data for later
if(save){ write.table(df.bio,'D_C4S1_AllMacrophyteData_clean.txt',row.names = FALSE,sep = ',') }
###############Section 7: Variable removal################################
rm(df,i,v.name,v.loc,w,df.temp,v.dat,j,df.temp2,k,index,df.prev,p.InformationMacrophytes)

##############Part 4: Combine chemical and macrophyte data (25 mins)######
#df.chem<-read.table('D_C4S1_AllChemicalData_clean.txt',header = TRUE,sep = ',')
#df.bio<-read.table('D_C4S1_AllMacrophyteData_clean.txt',header = TRUE,sep = ',')
df.loc<-read.table('D_C4_AllLocationsType.txt',header = TRUE,sep = ','); df.loc<-df.loc[!duplicated(df.loc),]
###############Section 1: Determine common locations and time#############
df.temp<-as.data.frame(rbind(unique(df.chem[,1:2]),unique(df.bio[,1:2])))
df.chembio<-df.temp[duplicated(df.temp),] #4344 observations
##############Section 2: Geographical at common###########################
df.info<-df.chembio
for (i in c(1:nrow(df.info))){
  index<-which(as.character(df.loc$Mp) == as.character(df.chembio$Mp[i]))
  if(length(index) == 1){ #Test to exclude different geographical information assigned to same location
    df.info$Xcoor[i]<-df.loc[index,2]; df.info$Ycoor[i]<-df.loc[index,3]
    df.info$Basin[i]<-as.character(df.loc[index,4]); df.info$Type[i]<-as.character(df.loc[index,5])
  }
} #Length = 4344
table(df.info$Type)
###############Section 3: Chemical at common##############################
df.chem.new<-as.data.frame(matrix(nrow = nrow(df.chembio),ncol = ncol(df.chem))); names(df.chem.new)<-names(df.chem)
for (i in c(1:nrow(df.chem.new))){
  df.chem.new$Mp[i]<-as.character(df.chembio$Mp[i]); df.chem.new$Date[i]<-as.character(df.chembio$Date[i])
  index<-which(as.character(df.chem$Mp) == as.character(df.chembio$Mp[i]) & as.character(df.chem$Date) == as.character(df.chembio$Date[i]))
  for (j in c(3:ncol(df.chem.new))){
    df.chem.new[i,j]<-df.chem[index,j] #Is this not possible for simply the whole row?
  }
}
df.chem.new<-as.data.frame(df.chem.new[,-which(as.data.frame(apply(df.chem.new,2,function (x) (sum(is.na(x))))) == nrow(df.chem.new))]) #Removal of variables without value
100*sum(is.na(df.chem.new))/(nrow(df.chem.new)*(ncol(df.chem.new)-2)) #Percentage missing data
###############Section 4: Macrophytes at common###########################
lst.mf<-list()
for (i in c(1:nrow(df.chembio))){
  lst.mf[[i]]<-subset(df.bio,as.character(df.bio$Mp) == as.character(df.chembio$Mp[i]) & as.character(df.bio$Date) == as.character(df.chembio$Date[i]))
}
df.bio.new<-do.call(rbind,lst.mf)
df.bio.new<-as.data.frame(df.bio.new[,which(apply(df.bio.new,2,function (x) (sum(x == 'Absent'))) != nrow(df.bio.new))]) #Removal of variables without value
###############Section 5: Plotting and saving#############################
if(plot){
  #####First plot: Data availability per variable and instance
  #Determine available information per variable
  df.prev<-as.data.frame(apply(df.chem.new[,c(3:ncol(df.chem.new))],2,function(x) 100*sum(!is.na(x))/nrow(df.chem.new))); names(df.prev)<-c('Availability')
  df.prev$Variable<-row.names(df.prev); row.names(df.prev)<-c()
  #Sort according to degree availability
  df.prev<-df.prev[order(df.prev$Availability,decreasing = TRUE),]; df.prev$VarNr<-c(1:nrow(df.prev))
  p.InformationVariable<-ggplot(df.prev,aes(x = VarNr, y = Availability)) + geom_hline(yintercept = 50,colour = 'grey',linetype = 'dashed') + geom_area(fill = 'black') + 
    scale_x_continuous('Variable') + scale_y_continuous('Availability (%)',limits = c(0,100)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black'))
  plot(p.InformationVariable)
  #Determine available information per instance
  df.mis<-as.data.frame(apply(df.chem.new[,c(3:ncol(df.chem.new))],1,function(x) 100*sum(!is.na(x))/(ncol(df.chem.new)-2))); names(df.mis)<-c('Availability')
  df.mis$Instance<-df.chem.new$Mp; row.names(df.mis)<-c()
  #Sort according to degree availability
  df.mis<-df.mis[order(df.mis$Availability,decreasing = TRUE),]; df.mis$InstNr<-c(1:nrow(df.mis))
  p.InformationInstance<-ggplot(df.mis,aes(x = InstNr, y = Availability)) + geom_hline(yintercept = 20,colour = 'grey',linetype = 'dashed') + geom_area(fill = 'black') + 
    scale_x_continuous('Instance') + scale_y_continuous('Availability (%)',limits = c(0,50)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black'))
  plot(p.InformationInstance)
  #Determine available information per macrophyte
  df.prev.bio<-as.data.frame(apply(df.bio.new[,c(3:ncol(df.bio.new))],2,function(x) 100*sum(x == 'Present')/nrow(df.bio.new))); names(df.prev.bio)<-c('Availability')
  df.prev.bio$Macrophyte<-row.names(df.prev.bio); row.names(df.prev.bio)<-c()
  #Sort according to degree availability
  df.prev.bio<-df.prev.bio[order(df.prev.bio$Availability,decreasing = TRUE),]; df.prev.bio$MacrophyteNr<-c(1:nrow(df.prev.bio))
  p.InformationMacrophytes<-ggplot(df.prev.bio,aes(x = MacrophyteNr, y = Availability)) + geom_hline(yintercept = 10,colour = 'grey',linetype = 'dashed') + geom_area(fill = 'black') + 
    scale_x_continuous('Macrophyte',breaks = c(0,125,250,375,500)) + scale_y_continuous('Availability (%)',limits = c(0,40)) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black'))
  plot(p.InformationMacrophytes)
  #Combine plots
  p.InformationCommon<-ggarrange(p.InformationVariable,p.InformationInstance,p.InformationMacrophytes,nrow = 3,align = 'hv',labels = 'AUTO',label.x = 0.085,label.y = 0.975,font.label = list(size = 12))
  plot(p.InformationCommon)
  if(save){
    tiff('./Graphs/F_C4S1_InformationCommonData.tiff',units = 'mm',width = 160,height = 150,res = 300,pointsize = 7)
    plot(p.InformationCommon)
    dev.off()
  }
  ####Second plot: Data availability versus time
  v.year<-do.call('rbind',strsplit(as.character(do.call('rbind',strsplit(as.character(df.info$Date),' '))[,1]),'/'))[,3]
  v.month<-do.call('rbind',strsplit(as.character(do.call('rbind',strsplit(as.character(df.info$Date),' '))[,1]),'/'))[,2]
  df.temp.info<-as.data.frame(table(v.year)); names(df.temp.info)<-c('Year','Observations')
  p.AnnualInfo<-ggplot(df.temp.info,aes(x = Year,y = Observations)) + 
    geom_col(width = 0.5,fill = 'black') + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black',size = 7),axis.text.x = element_text(angle = 60,vjust = 1,hjust = 1),axis.title = element_text(size = 9))
  plot(p.AnnualInfo)
  if(save){
    tiff('./Graphs/F_SI_C4S1_AnnualObservationsCombined.tiff',units = 'mm',width = 160,height = 45,res = 300,pointsize = 7)
    plot(p.AnnualInfo)
    dev.off()
  }
  df.temp<-melt(table(cbind.data.frame(v.year,v.month))); names(df.temp)<-c('Year','Month','Instances')
  df.temp$Year<-as.factor(df.temp$Year); df.temp$Month<-factor(df.temp$Month,c(1:12))
  p.MonthYear<-ggplot(df.temp,aes(x = Year,y = Month)) + geom_tile(aes(fill = Instances)) + geom_point(aes(colour = Instances > 0),size = 0.8) +
    scale_colour_manual(values = c('white','black'),guide = FALSE) + scale_fill_gradient(low = 'white',high = 'black') + scale_x_discrete('Year') + scale_y_discrete('Month',limits = rev(levels(df.temp$Month))) + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black',size = 7),axis.text.x = element_text(angle = 60,vjust = 1,hjust = 1),axis.title = element_text(size = 9),legend.text = element_text(size = 6),legend.title = element_text(size = 7))
  plot(p.MonthYear)
  if(save){
    tiff('./Graphs/F_C4S1_MonthlyObservationsCombined.tiff',units = 'mm',width = 160,height = 60,res = 300,pointsize = 7)
    plot(p.MonthYear)
    dev.off()
  }
  ####Third plot: Missingness pattern
  df.pat<-df.chem.new
  df.pat<-as.data.frame(apply(df.chem.new,c(1,2),function(x) ifelse(is.na(x),1,0))); df.pat$Mp<-df.chem.new$Mp
  df.pat<-df.pat[,order(apply(df.pat,2,function(x) sum(x == 0)),decreasing = TRUE)]
  df.pat$Mp<-order(apply(df.pat,1,function(x) sum(x == 0)),decreasing = TRUE)
  df.pat.m<-melt(df.pat[,-which(names(df.pat) == 'Date')],id.vars = c('Mp'))
  p.Pattern<-ggplot(df.pat.m,aes(x = variable,y = as.character(Mp),fill = as.factor(value))) + geom_tile() + 
    scale_fill_manual(values = c('black','white')) + 
    scale_x_discrete('Variable') + scale_y_discrete('Instance') +
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),legend.position = 'none')
  plot(p.Pattern)
  if(save){
    tiff('./Graphs/F_SI_C4S1_MissingnessCombinedData.tiff',units = 'mm',width = 160,height = 210,res = 300,pointsize = 7)
    plot(p.Pattern)
    dev.off()
  }
}
if(save){ 
  write.table(df.info,'D_C4S1_Common_LocationsInfo.txt',row.names = FALSE,sep = ',')
  write.table(df.chem.new,'D_C4S1_Common_Chemical.txt',row.names = FALSE,sep = ',')
  write.table(df.bio.new,'D_C4S1_Common_Macrophytes.txt',row.names = FALSE,sep = ',') 
}
###############Section 6: Variable removal################################
rm(df.temp,df.chembio,index,lst.mf)

##############Part 5: Describing missingness in chemical data (5 mins)####
###############Section 1: In- and output##################################
lst.temp<-list(); w<-1
lst.temp[[w]]<-read.table('D_C4S1_Common_Chemical.txt',header = TRUE,sep = ',')
f_MD<-sum(is.na(lst.temp[[w]]))/(nrow(lst.temp[[w]])*(ncol(lst.temp[[w]])))
df.red.data<-as.data.frame(matrix(nrow = 1,ncol = 6)); names(df.red.data)<-c('w','Nvar','Ninst','Ndp','MD','f')
df.red.data[w,]<-c(w,ncol(lst.temp[[w]]),nrow(lst.temp[[w]]),ncol(lst.temp[[w]])*nrow(lst.temp[[w]]),sum(is.na(lst.temp[[w]])),f_MD)
###############Section 2: Reducing missingness stepwise###################
while(f_MD > 0 & ncol(lst.temp[[w]]) > 3){
  message(paste('Run ',w,sep = ''))
  ############Subsection: Removal of variable (can cause instance to be without information)
  df.var<-lst.temp[[w]][,order(apply(lst.temp[[w]],2,function(x) sum(is.na(x))),decreasing = FALSE)]
  df.var<-df.var[,-ncol(df.var)]
  empty.v<-which(apply(df.var,1,function(x) sum(is.na(x))) == ncol(df.var[,-c(1,2)]))
  if(length(empty.v) > 0){
    df.var<-df.var[-empty.v,]
  }
  ############Subsection: Removal of instance (can cause variable to have no information left)
  df.inst<-lst.temp[[w]][order(apply(lst.temp[[w]][,-c(1,2)],1,function(x) sum(is.na(x))),decreasing = FALSE),]
  df.inst<-df.inst[-nrow(df.inst),]
  empty.i<-which(apply(df.inst,2,function(x) sum(is.na(x))) == nrow(df.inst))
  if(length(empty.i) > 0){
    df.inst<-df.inst[,-empty.i]
  }
  ############Subsection: selection of variable/instance to be removed####
  mis.var<-sum(is.na(df.var[,-c(1,2)]))/(nrow(df.var)*ncol(df.var[,-c(1,2)]))
  mis.inst<-sum(is.na(df.inst[,-c(1,2)]))/(nrow(df.inst)*ncol(df.inst[,-c(1,2)]))
  if(mis.var == min(mis.inst,mis.var) & ncol(df.var) > 4) {#Keeps at least 3 variables in the data, i.e. not selected if reduced data has only 2 variables
    lst.temp[[w+1]]<-df.var
    if(length(empty.v)>0){message(paste('Removal of ',length(empty.v),' instances without data',sep = ''))}
  } else {
    lst.temp[[w+1]]<-df.inst
    if(length(empty.i)>0){message(paste('Removal of ',length(empty.i),' variables without data',sep = ''))}
  }
  w<-w+1
  f_MD<-sum(is.na(lst.temp[[w]]))/(nrow(lst.temp[[w]])*(ncol(lst.temp[[w]]))); print(f_MD)
  df.red.data[w,]<-c(w,ncol(lst.temp[[w]]),nrow(lst.temp[[w]]),ncol(lst.temp[[w]])*nrow(lst.temp[[w]]),sum(is.na(lst.temp[[w]])),f_MD)
}
###############Section 3: Plotting and saving#############################
if(plot){
  df.red.data.m<-melt(df.red.data,measure.vars = c('Nvar','Ninst'),id.vars = 'f')
  df.red.data.m$variable<-factor(df.red.data.m$variable,levels = c('Nvar','Ninst'),labels = c('Number of variables (-)','Number of instances (-)'))
  p.DataReduction<-ggplot(df.red.data.m,aes(x = 100*f,y = value)) + geom_point() + 
    scale_x_reverse('Fraction missing data (%)') + scale_y_continuous('') + 
    facet_grid(variable~.,scales = 'free_y',switch = 'y') + 
    theme_bw() + theme(panel.grid = element_blank(),axis.text = element_text(colour = 'black',size = 7),axis.title = element_text(size = 9)) + 
    theme(strip.background = element_blank(),strip.placement = 'outside',strip.text = element_text(size = 8))
  plot(p.DataReduction)
  if(save){
    tiff('./Graphs/F_SI_C4S1_DataReductionMissingness.tiff',units = 'mm',width = 160,height = 90,res = 300,pointsize = 7)
    plot(p.DataReduction)
    dev.off()
  }
}
if(save){
  write.table(df.red.data,'./Graphs/FD_SI_C4S1_DataReductionMissingness.txt',row.names = FALSE,sep = ',')
}
###############Section 4: Variable removal################################
rm(lst.temp,f_MD,df.red.data,df.var,empty.v,df.inst,empty.i,mis.var,mis.inst,w,df.red.data.m)
