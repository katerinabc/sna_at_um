#Script to analyze SNA at UM dataset.
#Created on XX November 2015
#Updated March 6th 2016
#next step: combine answers for project 2


# analysis ----------------------------------------------------------------

#do logistic regression
#calculate network characteristics for its members.
#indegree, outdegree, density for each group
#beweenness
#structural equivalence
#if part of triad....

#argument: inverted U curve RS between ties and performance. 
#if collaboration judged good = center in network, many links


# Questions ------------------------------------------------------
# what is answer 0 in att3? missing values?

# preface ------------------------------------------------------
# start with a clean working space
rm(list=ls())
# load previous data

# set the working directory
setwd("~/Dropbox/Network Analysis Innovation at UM/paper")
# load packages
# library(statnet)
library(statnet) 

# load data ------------------------------------------------------

# att1.raw<-read.csv("attribute.csv", sep=";", header=T, stringsAsFactors=F) #project info (name, position, performance)
# #check last 4 rows are empty
# tail(att1.raw)
# att1.raw<-att1.raw[1:129,]
# att2.raw<-read.csv("attribute_Final.csv", header=T, na.strings=c(999999, "#N/A")) #indi info (gener, faculty)
# att3.raw<-read.csv("attribute_kwsharingFINAL.csv", header=T, na.strings=("#N/A"), stringsAsFactors=F) #kw sharing info

att.raw<-read.csv("SnaUmAttributes.csv", header=T)
att.all.raw<-read.csv("SnaUmAttributesAll.csv", header=T)

net.raw<-read.table("edgearray_maastricht.csv", header=T, sep=";", 
                  stringsAsFactors=F, na.strings=c("Not in list","MV"))
#take out empty rows. problem with excel
net.raw<-net.raw[!net.raw$Ego == "",1:29] 
str(net.raw)
#something is weird here. column 7 imported as character vector
net.raw[,c(7)]<-as.numeric(net.raw[,c(7)]) 

# Data preparation: Attributes ------------------------------------------------
# this section is only needed when cleaning up and combining the different
# attributes file. This is not needed anymore. Thus commented out.

# #recode answers from text to numbers, only need to do once
# att3 <- att3.raw
# att3 <- apply(att3, 2, function(x){replace(x, x == "strong disagreement",1)})
# att3 <- as.data.frame(apply(att3, 2, function(x){replace(x, x == "disagreement",2)}))
# att3 <- as.data.frame(apply(att3, 2,  function(x){replace(x, x == "neutral",3)}))
# att3 <- as.data.frame(apply(att3, 2,  function(x){replace(x, x == "agreement",4)}))
# att3 <- as.data.frame(apply(att3, 2,  function(x){replace(x, x == "strong agreement",5)}))
# att3<-apply(att3, 2, as.numeric) #the code above changes the character vector into factor because it is a mix of numbers and characters. factor is the smalled common type that allows numbers and characters. this line of code transforms it back to numbers
# rownames(att3)<-att3.raw[,1]
# #hist(att3)# plot answers
# 
# #find people with no faculty number 
# 
# names(att1.raw)
# att1.raw[which(is.na(att1.raw[,2])),]
# att1.raw[68,2]<-6 #6 = sbe
# att1.raw[93,2]<-6
# att1.raw[122,2]<-2 #FHML = 2
# 
# names(att2.raw)
# att2.raw[which(is.na(att2.raw[,6])),]
# att2.raw[c(61, 63, 81),6]<-2
# att2.raw[c(67, 72, 82),6]<-6
# 
# #Fix the names
# att1.raw[1,1]<- "A. Duijvestijn"
# att1.raw[27,1]<-"Eleonore Kuhler"
# att1.raw[64,1]<- "Karen Konings"
# att1.raw[73,1]<-"M van Zandvoort"
# att1.raw[88,1]<-"Mirko Reithler"
# att1.raw[101,1]<-"Peter Wilms van Kersbergen"
# 
# att2.raw[,1] <- as.character(att2.raw[,1])
# att2.raw[1,1] <- "A. Duijvestijn"
# att2.raw[27,1] <-"Eleonore Kuhler"
# att2.raw[64,1] <- "Karen Konings"
# att2.raw[73,1] <-"M van Zandvoort"
# att2.raw[87,1]<-"Mirko Reithler"
# att2.raw[99,1]<-"Peter Wilms van Kersbergen"
# 
# rownames(att3)[24]<-att1.raw[64,1]
# rownames(att3)[35] <- att1.raw[112,1]
# rownames(att3)[51]<-att1.raw[64,1]
# 
# #att 3 has some people filling out the questionnaire several times
# #take out NA's
# idx<-which(is.na(att3[,2]))
# att3<-att3[-idx,]
# #take out duplicates
# att3<-att3[!duplicated(rownames(att3)),]
# att3<-att3[order(rownames(att3)),]
# rownames(att3)[5] <- att1.raw[10,1]
# rownames(att3)[13] <- "Eleonore Kuhler"
# rownames(att3)[16] <- att1.raw[35,1]
# rownames(att3)[25] <- att1.raw[67,1]
# rownames(att3)[26] <- att1.raw[70,1]
# rownames(att3)[49] <- att1.raw[126,1]
# 
# # #combine all the attribute datasets
# # #matching rows to att1
# att2<-att2.raw[match(att1.raw[,1], att2.raw[,1]),]
# att3.mod<-att3[match(att1.raw[,1], rownames(att3)),]
#   
# #combine att1 and att2 with background variables 
# att.12<-cbind(att1.raw, att2)
# write.csv(att.12, "SnaUmAttributes.csv")
# 
# att.all<-cbind(att1.raw, att2, rownames(att3.mod), att3.mod) #combine att1, att2, att3
# att.all<-att.all[,c(1,4,13,14,2:3,5:12,15:44)]
# idx<-which(is.na(att.all[,3])) #att 3 had the smallest data, could take NA's out
# att.all<-att.all[-idx,]
# att.all<-att.all[,c(1,5:ncol(att.all))]
# write.csv(att, "SnaUmAttributesAll.csv")

# Data preparation: Networks ----------------------------------------------

#create get info network by averaging getinfo and give info
#give info needs to be transposed(columbs become rows and rows colums)

#project 1
getinfo.raw<-net.raw[,c(1:2, 9)]
getinfo.raw$dyad<-paste(getinfo.raw[,1], getinfo.raw[,2], sep="_")
length(unique(getinfo.raw[,4])) #671 unique dyads
giveinfo.raw<-net.raw[,c(2,1,12)] #transposed of giveinfo
giveinfo.raw$dyad<-paste(giveinfo.raw[,1], giveinfo.raw[,2], sep="_")
length(unique(giveinfo.raw[,4])) #671 unique dyads

#sort give info.raw so that col1 and col2 match the colum names in getinfo.raw
#if I sort both alpahebetically it should work
getinfo.raw<-getinfo.raw[order(getinfo.raw[,4]),]
giveinfo.raw<-giveinfo.raw[order(giveinfo.raw[,4]),]
lapply(list(getinfo.raw, giveinfo.raw), dim) #returns dimension of both objects

#check if both data frames have the same dyads
table(getinfo.raw[,4] == giveinfo.raw[,4]) #true only 37 times

#find the matches
idx.gt<-which(getinfo.raw[,4] %in% giveinfo.raw[,4]) #563 dyads are in both data frames
idx.gv<-which(giveinfo.raw[,4] %in% getinfo.raw[,4])

#subset for the dyads that are in both dataframes
getinfo.raw2<-getinfo.raw[idx,]

#do the same for giveinfo
giveinfo.raw2<-giveinfo.raw[which(giveinfo.raw[,4] %in% getinfo.raw[,4]),]
#dim: 563 - 4

#sort both alphabetically
getinfo.raw2<-getinfo.raw2[order(getinfo.raw2[,4]),]
giveinfo.raw2<-giveinfo.raw2[order(giveinfo.raw2[,4]),]

#inspet giveinfo.raw2 manually
match(giveinfo.raw2[,4], getinfo.raw2[,4])
#row 35 is twice (Anita van Gils_Jeannette Hommes)
#remove 1 instance
giveinfo.raw2<-giveinfo.raw2[-35,]

#again subset for same dyads
getinfo.raw3<-getinfo.raw2[which(getinfo.raw2[,4] %in% giveinfo.raw2[,4]),]
giveinfo.raw3<-getinfo.raw2[which(giveinfo.raw2[,4] %in% giveinfo.raw2[,4]),]
lapply(list(getinfo.raw3, giveinfo.raw3), dim)
getinfo.raw3<-getinfo.raw3[match(getinfo.raw3[,4], giveinfo.raw3[,4]),]
getinfo.raw3<-getinfo.raw3[-nrow(getinfo.raw3),]

#check if dyads in same order
table(getinfo.raw3[,4] == giveinfo.raw3[,4]) #finally ok

#create information seeking for project 1
#composed of the average of getinfo.raw2 and giveinfo.raw2
#added to it the dyads than don't have a match. these numbers could be biased.

ie1.raw<-cbind(getinfo.raw3, giveinfo.raw3)
table(ie1.raw[, 4] == ie1.raw[,8])#check again if same dyads

#calculate row mean and 
ie1<-cbind(ie1.raw[,1:2], ie1 = rowMeans(ie1.raw[,c(3,7)], na.rm=T))
#add the dyads for which I only have one sided information
ie1<-rbind(ie1, setNames(getinfo.raw[-idx.gt,1:3], names(ie1)), 
           setNames(giveinfo.raw[-idx.gv,1:3], names(ie1)))

#take out those who have NA has value
ie1<-ie1[!is.na(ie1[,3]),]

#check for duplicates
ie1$dyad<-paste(ie1[,1], ie1[,2], sep="-")
table(duplicated(ie1$dyad)) #8 dubbles. inspect them
idx.du<-which(duplicated(ie1$dyad))
dyad.dub<-ie1[idx.du,4]
View(ie1[ie1$dyad %in% dyad.dub, ])
View(ie1[ie1[,4] == dyad.dub,])
#no difference in ie1 frequency
#exception: dyad: Paul Adriaas - Wim G: 4 and 2.5. -> Take the 2.5, that is based on the average.
ie1<-ie1[-idx.du,]
table(duplicated(ie1$dyad)) #no duplicates. 

#Project 2
save.image("snaatum.RData")



net.raw<-cbind(net.raw, ie_pr1, ie_pr2, ie_pr3)

#RUN LOOP ON SERVER



# this loops creates a valued network for every column in the network file
for (i in 3:ncol(net.raw)){
  # subset the netwwork file. i selects the column
  #mat<-as.matrix(subset(net.raw, select=c(1:2,i)))
  # create a network object
  nv<-as.network(subset(net.raw, select=c(1:2,i)), directed=T, 
                 matrix.type="edgelist",ignore.eval=F, names.eval="value")
  # assign edge value to the network objects
  nv%e%"value" <- net.raw[,i]
  # assign an name (identifier) for the network object nv
  assign(paste(colnames(net.raw)[i], "val", sep= "."),nv) 
}

# this loops creates a binary network for every column in the network file. 
# The standard ERGM procedure only takes binary data for the dependent netwokr.
for (i in 3:ncol(net.raw)){
  # subset the netwwork file. i selects the column
  df<-subset(net.raw, select=c(1:2,i))
  df[which(df[,3] < 4),3 ]<-0
  df[which(df[,3] > 3),3 ]<-1
  # dichotomize the network
  # in the object df, take the rows for which the 3rd column has a value above 3
  # and then replace the value that is in column 3 with 0
  # create a network object
  
  #does something funny
  n<-network(df, directed=T, matrix.type="edgelist")
  # assign an name (identifier) for the network object
  assign(colnames(net.raw)[i],n) 
}

# # prepare mega matrix for ERGM --------------------------------------------
# 
# # take information sharing network
# # turn ties that exists to 0 and ties that can NOT exists to 1
# net.mega_pr1<-as.sociomatrix(ie_pr1)
# # convert into
# net.mega_pr1[net.mega_pr1[,3] ,] <-0
