#DESCRIPIPTION##################################################################

#Title: NUMERICAL HIERARCHICAL CLUSTER
#Version: 01
#Description: Use of clusters for better understating a mall base customers
#Data source: https://www.kaggle.com/datasets/akram24/mall-customers



# 00. THE CASE AND INTODUCTION  #################################################

# We use cases as a story telling for this experiments

TheCase<- "We want to target the mall efforts in marketing, product development and in general how the mall can focus on resources of the actual merchants or future ones. The clusters can give insights for a former optimization."

# 01. PREPARE THE OPERATION ####################################################

## PREPARE THE LIBRAIES ========================================================


install.packages("readxl")
install.packages("outliers") 
install.packages("NbClust")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("janitor")
install.packages("reshape")

library(outliers)
library(readxl)
library(NbClust)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(janitor)
library(reshape)

## PREPARE THE THEMES ==========================================================

###Define Colors------------------------------------------------------------------

BrandColorGroup4<-c("#172933","#143140","#1D4762","#FFA400","#FCC400")
BrandColorGroup4
BrandColorGroup2<-c("#143140","#FFA400")
SoftGrey<-"#666666ff"
SoftGrey2<-"#E7E6E6"
SoftGrey3<-"808080"
SoftGrey4<-"gray97"
mainColor<-"#FFA400"
secondaryColor<-"#1D4762"
terciaryColor<-"#172933"
titleTextColorTable<-"white"
titleTextColorTable2="black"
ligthYellow<-"#FFF1C2"
ligthBlue<-"#BBCAD6"
  
  
### check your colors-----------------------------------------------------------


#Build a vector whit the collection of colors
colorColection<-c(
  BrandColorGroup4,
  BrandColorGroup2,
  SoftGrey,
  SoftGrey2,
  SoftGrey3,
  SoftGrey4,
  ligthYellow,
  ligthBlue
)

# Create the verification theme
themeColorcheck<-ttheme_minimal(
  core=list(
    bg_params=list(fill=colorColection)
  )
)

# Print the verification table
grid.table(colorColection,theme=themeColorcheck) #print it


###Define Themes------------------------------------------------------------------

#Theme Table 1
themeTable1<-ttheme_default(
  colhead=list(
    bg_params=list(fill=mainColor),
    fg_params=list(col=titleTextColorTable)
  )
)


#Theme Table 2
themeTable2<-ttheme_default(
  colhead=list(
    bg_params=list(fill=secondaryColor),
    fg_params=list(col=titleTextColorTable)
  )
)


#Theme Table 3
themeTable3<-ttheme_default(
  colhead=list(
    bg_params=list(fill=SoftGrey),
    fg_params=list(col=titleTextColorTable)
  )
)



#Theme Text Box 1
themeTextBox1<-ttheme_default(
  colhead=list(
    bg_params=list(fill=SoftGrey),
    fg_params=list(col=titleTextColorTable)
  )
)

#Theme Text Box 2
themeTextBox2<-ttheme_minimal(
  colhead=list(
    bg_params=list(fill=SoftGrey),
    fg_params=list(col=titleTextColorTable)
  )
)


#Theme Text Box 3
themeTextBox3<-ttheme_minimal(
  colhead=list(
    bg_params=list(fill=SoftGrey2),
    fg_params=list(col=titleTextColorTable2)
  )
)





# 02, PREPARE THE  INFORMATION ##################################################

raw_data <- read.csv("Mall_Customers.csv")
class(raw_data)
summary(raw_data)
str(raw_data)

#Retain Only relevant variables
  # the "id" is not relevant for the exercise 
main_data<-raw_data[-1] # Select just the correct data
summary(main_data)
str(main_data)


#Know the class of the variables (Columns)
class_summary<-sapply(main_data,class)
class_summary
class_summary_table<-as.data.frame(table(class_summary))

#Plot summary
grid.table(class_summary_table,theme=themeTable1, rows= NULL)

# Plot detail
grid.table(as.data.frame(class_summary),theme=themeTable1)


#Select the numeric variables
main_data_numeric <- select_if(main_data, is.numeric)
classcol<-sapply(main_data_numeric,class)
classcol
clascsolTable<-as.data.frame(classcol)
grid.table(clascsolTable,theme=themeTable1)
table(classcol) # Summary the variable class



# 03.  REMOVE OUTLIERS ################################################################

#Select numeric
main_data_numeric<-select(main_data,where(is.numeric))
main_data_numeric



# Find Outliers
Outliers<-scores(main_data_numeric, type="iqr",prob=NA, lim="iqr")
Outliers
SummaryOutliers<-summary(Outliers)
SummaryOutliers
grid.table(SummaryOutliers, theme=themeTable3)

Comnent_Outliers<-" All the results turn in to false in this first verification of outliers"

# Use the depurated data
#in case of outliers the data should be depurated.

main_data_dep<-main_data #Use a depurated df.
SummaryMaindata<-summary(main_data_dep)
grid.table(SummaryMaindata[,-1], theme=themeTable2)

# 04 SCALE THE NUMERIC DATA ####################################################

#The scaled main data will be use to build the cluster classification
#but the clusters are build with the original data

main_data_dep_numeric<-select(main_data_dep,where(is.numeric))
main_data_dep_numeric

scaled_main_data_dep <-scale(main_data_dep_numeric)
str(scaled_main_data_dep)
SummaryScaled<-summary(scaled_main_data_dep)

grid.table(SummaryScaled, theme=themeTable3)

ObservationScaleData<- "The data is scaled to avoid large numbers to dominate the model, you can see the difference in range between the two summary tables"

# 05. BUILD THE CLUSTERTS ######################################################


## CALCULATE DE DISTANCE =======================================================

ComentMetodClustering<- "The method used for the cluster is the average Euclidean"

dist_among_observ <-dist(scaled_main_data_dep)# euclidean is the default method
class(dist_among_observ) # this is a "dist" class
?dist

## HIERARCHICAL CLUSTERNING APPROACH ===========================================

#Method : Average
cluster_fit_average<-hclust(dist_among_observ, method="average")
class(cluster_fit_average) # Cluster model



## DEFINE THE OTIMAL NUMER OF CLUSTERS ==========================================

number_of_clusters<-NbClust(scaled_main_data_dep, distance="euclidean", min.nc=2, max.nc=7, method="average")
class(number_of_clusters) #list

bestPartitionQuantity<-max(number_of_clusters[["Best.partition"]]) # get the maximun clusters
bestPartitionQuantity # Get the quantity

notePartittion<-paste("The best partition in is determinated by",bestPartitionQuantity,"clusters")
notePartittion

#PENDIGN ENTENDER LAS GRÁFICAS


## BULD THE FINAL CLUSTERS =====================================================

final_clusters_av<-cutree(cluster_fit_average,k=bestPartitionQuantity)
final_clusters_av
class(final_clusters_av)

## ASIGN THE CLUSTERS ==========================================================

cluster_and_observtion_av<-cbind(final_clusters_av,main_data_dep)
cluster_and_observtion_av
str(cluster_and_observtion_av)
summary(cluster_and_observtion_av) 



# 06 CLUSTER PROFILE ###########################################################

## CLUSTER COMPOSITION  ========================================================

# Number and classs of variables per cluster
QVaribles<-ncol(cluster_and_observtion_av)-1 # the -1 is because of the cluster
Coment_Summary<-paste("Each culster is compose by",QVaribles,"variables:")
Coment_Summary

# Build a composition for the clusters 

Variables=names(cluster_and_observtion_av)
Variables
Type=sapply(cluster_and_observtion_av,class)
Type
introClusters<-data.frame(Variables,Type)
introClusters

grid.table(introClusters[-1,],theme=themeTable1, rows= NULL)


## SUMMARY OF THE CUSTERS ======================================================

###CREATE THE SUMMARY ----------------------------------------------------------

# Total observation  per clusters

SummaryCluster<-aggregate(cluster_and_observtion_av$final_clusters_av, by=list(cluster_and_observtion_av$final_clusters_av),length)
colnames(SummaryCluster) <- c("Cluster", "Count") #change the name
SummaryCluster #create the summary table

SummaryCluster$Cluster <- paste("Cluster",SummaryCluster$Cluster) #change the name
SummaryCluster

# Total percentage participation by cluster
totalQcluster<-sum(SummaryCluster$Count)
SummaryCluster$propotion<-(SummaryCluster$Count/totalQcluster) # Add it to the summary
SummaryCluster 


### Mean of the numeric variables-------------------------------------------------

#Select the numeric variables
numericData_Cluster<-select(cluster_and_observtion_av,where(is.numeric))
SummaryCluster_numeric<-round(aggregate(numericData_Cluster[-1], by=list(cluster_and_observtion_av$final_clusters_av),mean),1)
SummaryCluster_numeric

numericNames<-colnames(SummaryCluster_numeric)
newnumericNames<-paste("Average",numericNames,sep="\n") # creat the correct name "Average"
newnumericNames
colnames(SummaryCluster_numeric)<-newnumericNames #change the name
SummaryCluster_numeric

SummaryCluster<-cbind(SummaryCluster,SummaryCluster_numeric[-1])# Add it to the summary
SummaryCluster


### Summary of selected categorical variables ----------------------------------
# Is done for the gender composition 

#Calculate the proportion for each cluster
SummaryCluster_Cat_invert<-tabyl(cluster_and_observtion_av,Gender,final_clusters_av)%>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
SummaryCluster_Cat_invert # you can see is inverted 

#transpose it
SummaryCluster_Cat <- data.frame(t(SummaryCluster_Cat_invert[-1]))
colnames(SummaryCluster_Cat) <- SummaryCluster_Cat_invert[, 1]
SummaryCluster_Cat # you can see is in the correct disposition


# Add to the summary
SummaryCluster<-cbind(SummaryCluster,SummaryCluster_Cat)
SummaryCluster

### Final summay table ---------------------------------------------------------

SummaryCluster
grid.table(SummaryCluster,theme=themeTable1, rows= NULL)


# 07. CLUSTER ANALISS ##########################################################

Intro<-"The objective in the case is to identify insights about the customers,
punctually the analysis would be focus on the Spending Score ("Spending.Score..1.100"),
this will indicate the performance and interest of action for each cluster"



## PARTICIPATION ANALISIS =============================================================

doughnutData<-SummaryCluster
doughnutValue<-as.vector(SummaryCluster$propotion)
doughnutLabel_name<-as.factor(SummaryCluster$Cluster)
doughnutLabel_value<-as.factor(SummaryCluster$propotion)

hole_size<-1.2 # Define it before, it will be need it in two parts of the code.
ggplot(data = doughnutData, 
       aes(x = hole_size, y = doughnutValue, fill = doughnutLabel_name))+ # size en x
  geom_bar(stat = "identity")+
  coord_polar("y", start = 500) +
  geom_text(position = position_stack(vjust = 0.5), 
            aes(label = paste(doughnutLabel_name,"\n",round(doughnutValue*100,1),"%", sep = "")), 
            col = "white") + # Ad teh laboel
  theme_void() +
  xlim(.2,hole_size+0.5)+# size en x
  theme(legend.position="none")+ # remove legend
  scale_fill_manual(values = BrandColorGroup4)+
  ggtitle("CLUSTER PARTICIPATION") # title


## ANALICE THE IMPORTANCE OF CLUSTERS ==========================================

### CALCULAETE THE THE WEIGHTED AVERAGE OF SPENDING SCORE ----------------------

#Calculate the mean spending score
#Note the use of the complete data, not the summary, to make the correct math
meanSepending<-mean(cluster_and_observtion_av$Spending.Score..1.100.)
meanSepending
SummaryCluster

#Calculate the weighted proportion
SummaryCluster$Power_Cluster<-SummaryCluster$propotion*SummaryCluster$`Average
Spending.Score..1.100.`
SummaryCluster

# Divide over the mean
SummaryCluster$Power_Cluster<-((SummaryCluster$propotion*SummaryCluster$`Average
Spending.Score..1.100.`)/meanSepending)
SummaryCluster

# Add it to the summary
importanceCluster<-select(SummaryCluster,Cluster,propotion,`Average\nSpending.Score..1.100.`,Power_Cluster)
importanceCluster

#PENDING: TRATAR DE VOLCVERLO PORCENTAJE 
Tratar de volverlo porcentage
adorn_pct_formatting(dat=importanceCluster,digits = 1,,,c(propotion,Power_Cluster))
importanceCluster


### Plot the importance --------------------------------------------------------

long_importanceCluster<-melt(importanceCluster)
long_importanceCluster
long_importanceCluster$value<-round(long_importanceCluster$value,2)
long_importanceCluster

ggplot(data=long_importanceCluster,aes(x=Cluster, y=value))+
  geom_bar(stat="identity",position="dodge",fill=c(BrandColorGroup4,BrandColorGroup4,BrandColorGroup4))+
  facet_grid(variable~., scales="free_y")+
  geom_text(aes(label=value), vjust=1.5, colour = "white")


ComentImpotance<-"Check the report"


## PROFILE OF THE CLUSTERS =====================================================


# rename the clusters 
cluster_and_observtion_av$final_clusters_av<-paste(cluster_and_observtion_av$final_clusters_av,"Cluster")
str(cluster_and_observtion_av)

#Transform the data in the long format
Long_Data <- melt(cluster_and_observtion_av, id = c("final_clusters_av","Gender"))
Long_Data
str(Long_Data)


### PLOT THE CLUSTERS ----------------------------------------------------------
boxplotData<-Long_Data

boxplot1<-ggplot(boxplotData, aes(x=final_clusters_av, y=value,fill=final_clusters_av)) + 
  geom_boxplot(color=SoftGrey3)+
  stat_summary(fun=mean, geom='text', label="")+ # ad the mean
  stat_summary(fun=mean, geom="errorbar",aes(ymax = ..y.., ymin = ..y..),linetype = "dashed",col=SoftGrey2)+
  stat_summary(fun=quantile, geom='text', label="")+ # ad the quantile
  facet_grid(.~final_clusters_av, scales="free")+
  facet_grid(variable~., scales="free")+
  theme(panel.background = element_rect(fill = SoftGrey4))+
  theme(legend.position="none")+
  scale_fill_manual(values = BrandColorGroup4)

boxplot1

# USE THE BOXPLOT INFORMATION TO HAVE MEAN AND QUANTILE
boxplot1Values<-ggplot_build(boxplot1) #Get the values of the box plot

justMeanV<-round(boxplot1Values[["data"]][[3]][["y"]],1) # extract the mean
justMeanV

justQuantileV<-round(boxplot1Values[["data"]][[4]][["y"]],1) #Extract the Quantile
justQuantileV


# CHANGE THE LABLES FOR THE REAL VALUES

boxplot1+
  stat_summary(fun=mean, geom='text', label=paste("Mean",justMeanV), position = position_nudge(y = 2), color="white", size=3)+ # ad the mean
  stat_summary(fun=quantile, geom='text', label=paste(justQuantileV,"-"), position = position_nudge(x = -0.5),color="black", size=3 ) # ad the mean

# Conclusions
Conclusion<- "check the report"


ggplot(data=Long_Data,aes(x=final_clusters_av, y=value))+
  geom_bar(stat="summary",fun="mean", position="dodge", fill=c(BrandColorGroup4,BrandColorGroup4,BrandColorGroup4))+
  facet_grid(variable~., scales="free")



## ANALICE CATEGORICAL VALUES: GENDER ==========================================

#Plot the proportion for gender in general
barGender<-ggplot(cluster_and_observtion_av, aes(x=final_clusters_av, fill=Gender)) +
  geom_bar(stat="Count", position="fill")+
  scale_fill_manual(values = BrandColorGroup2)
barGender

#Pot the proportion for gender for each attribute 
ggplot(data=Long_Data,aes(x=final_clusters_av, y=value,fill=Gender))+
         geom_bar(stat="summary",fun="mean", position="dodge")+
        facet_grid(variable~., scales="free")+
  scale_fill_manual(values = BrandColorGroup2)


NoteGender<-"the is nor mutch diference in the gender distribution"



# INDIVIDUAL ANALISYS BY CATHEGORY OF INTERETS =================================

NotesDeeepr<-" Check the report"

# Spending Score vs Age
ggplot(data=cluster_and_observtion_av,aes(x=Spending.Score..1.100., y=Age,fill=Gender))+
  geom_bar(stat="summary",fun="mean", position="dodge")+
  facet_grid(final_clusters_av~., scales="free_x")+
  scale_fill_manual(values = BrandColorGroup2)+
  theme(panel.background = element_rect(fill = ligthYellow), legend.position="bottom")

# Spending Score vs Anual Income
ggplot(data=cluster_and_observtion_av,aes(x=Spending.Score..1.100., y=Annual.Income..k..,fill=Gender))+
  geom_bar(stat="summary",fun="mean", position="dodge")+
  facet_grid(final_clusters_av~., scales="free_x")+
  scale_fill_manual(values = BrandColorGroup2)+
  theme(panel.background = element_rect(fill = ligthBlue),  legend.position="bottom")



#QUITAR ESTO
# cuales son los que más gastan
Long_Data_interest <- melt(cluster_and_observtion_av, id = c("final_clusters_av","Gender","Spending.Score..1.100."))
Long_Data_interest


ggplot(data=Long_Data_interest,aes(x=Spending.Score..1.100., y=value,fill=Gender))+
  geom_bar(stat="summary",fun="mean", position="dodge")+
  facet_grid(variable~final_clusters_av, scales="free_y")+
  scale_fill_manual(values = BrandColorGroup2)





# EJEXUTIVE SUMARRY ############################################################


SummaryCluster
sumaryTablesPlot<-as.data.frame(SummaryCluster$Cluster,
                        SummaryCluster$`Average\nAge`,
                        SummaryCluster$`Average\nAnnual.Income`, 
                        SummaryCluster$`Average\nSpending.Score..1.100.`,
                        SummaryCluster$Power_Cluster)

SummaryTablePlot<-SummaryCluster[,c(1,4,5,6,3,9)]
SummaryTablePlot


long_SummaryTablePlot<-melt(SummaryTablePlot)
long_SummaryTablePlot
long_SummaryTablePlot$value<-round(long_SummaryTablePlot$value,2)
long_SummaryTablePlot

ggplot(data=long_SummaryTablePlot,aes(x=Cluster, y=value))+
  geom_bar(stat="identity",position="dodge",fill=c(BrandColorGroup4,BrandColorGroup4,BrandColorGroup4,BrandColorGroup4,BrandColorGroup4))+
  facet_grid(variable~., scales="free_y")+
  geom_text(aes(label=value), vjust=1.5, colour = "white")


