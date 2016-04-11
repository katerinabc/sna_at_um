####################################
# R SCRIPT ABOUT SNA AT UM RESEARCH
# ANALYSIS OF DATA
# CREATED ON 17TH MARCH 2016
# UPDATED ON
####################################


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
load("snaatum_cleaned.RData")

# set the working directory
setwd("~/Dropbox/Network Analysis Innovation at UM/paper")
#library(dls) uses igraph package. igraph and sna have the same name for some function.
#don't load dls or igraph. code will break. 

# load packages
#library(statnet) 
library(plyr)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
library(reshape2)

source("functions.R") #needs to load after the packages as it overrides a number of functiosn

# descriptives ------------------------------------------------------------

um.i$faculty<-as.factor(um.i$faculty)
um.i$gender<-as.factor(um.i$gender)
um.i$performance<-as.factor(um.i$performance)

summary(um.i)
summary(um.t)

var.columns<-c(3:4,7:51)
pdf("um_descp_indi.pdf")
par(mfrow=c(3,2))
for (i in var.columns) {
  print(hist(um.i[,i], xlab=colnames(um.i)[i],main=paste("Frequency of", colnames(um.i)[i])))
  #col.var<-um.i[,i]
  #print(plot_list[[i]]<-qplot(x=col.var, data=um.i, geom="histogram", xlab=colnames(um.i)[i],main=paste("Frequency of", colnames(um.i)[i])) )
}
dev.off()

var.columns<-c(1,3:ncol(um.t))
pdf("um_descp_team.pdf")
par(mfrow=c(3,2))
for (i in var.columns) {
  print(hist(um.t[,i], xlab=colnames(um.t)[i],main=paste("Frequency of", colnames(um.t)[i])))
  #col.var<-um.i[,i]
  #print(plot_list[[i]]<-qplot(x=col.var, data=um.i, geom="histogram", xlab=colnames(um.i)[i],main=paste("Frequency of", colnames(um.i)[i])) )
}
dev.off()

#subset for teams with less than 30 % missing data
um.t.sub<-um.t[um.t$mis.data<0.3875,]

pdf("um_descp_team_sub.pdf")
par(mfrow=c(3,2))
for (i in var.columns) {
  print(hist(um.t.sub[,i], xlab=colnames(um.t.sub)[i],main=paste("Frequency of", colnames(um.t.sub)[i])))
  #col.var<-um.i[,i]
  #print(plot_list[[i]]<-qplot(x=col.var, data=um.i, geom="histogram", xlab=colnames(um.i)[i],main=paste("Frequency of", colnames(um.i)[i])) )
}
dev.off()

um.i.var<-apply(um.i[,c(3:4, 7:51)], 2, function(x) round(var(x, na.rm=T),2))
um.t.var<-apply(um.t[,c(1,3:ncol(um.t)),], 2, function(x) round(var(x, na.rm=T),2))

#observations um.i:
#performance skewed: most rated their project a success --> sign of bias?
#ksa (individual knowldge sharing attitudes): normal distribution
#ksd (departmental knowledge sharing attitudes): 
#indegree: right tail; most low to 0 indegrees for the different variables
#outdegree: right tail; most low to 0 outdegrees for the different variables
#betweenness:right tail; most low to 0 betweeness for the different variables
#closeness: most low (0 degree) or high (1). A couple in the middle

#observations um.t.sub:
#a lot of assymetric ties

# correlation  -------------------------------------------------------------

corr.i<-rcorr(as.matrix(um.i[,c(3:4,7:51)]))
apply(um.i, 2, function(x) table(is.na(x)))
#leave out those w/ more than 100 missing values (78%): 
#col name:btw_b_ph; cls_b_c, cls_b_ph, cls_b_h
um.i2<-um.i[,-c(38,45,47:48)]

corr.i_att<-rcorr(as.matrix(um.i2[,c(3:4,7:15)]))
corr.i2_in<-rcorr(as.matrix(um.i2[,c(16:24)]))
corr.i2_out<-rcorr(as.matrix(um.i2[,c(25:33)]))
corr.i2_btw<-rcorr(as.matrix(um.i2[,c(34:41)]))
corr.i2_cls<-rcorr(as.matrix(um.i2[,c(42:47)]))
corr.i2_pf<-rcorr(as.matrix(um.i2[,c(7:47)]), type="spearman")
# corr.i2_pf[[1]]<-corr.i2_pf[[1]][,1]
# corr.i2_pf[[2]]<-corr.i2_pf[[2]][,1]
# corr.i2_pf[[3]]<-corr.i2_pf[[3]][,1]
corr.pf<-cbind(r= corr.i2_pf[[1]][2:nrow(corr.pf),1],
               n=corr.i2_pf[[2]][2:nrow(corr.pf),1], 
               p=corr.i2_pf[[3]][2:nrow(corr.pf),1])

corr.t<-rcorr(as.matrix(um.t[,c(3:23)]))


pdf("correlation.pdf")
corrplot(corr.i_att[[1]], tl.srt=0, tl.col="black",
         type="lower", order="hclust", method="color",addCoef.col = "black",
         p.mat = corr.i_att[[3]], sig.level = 0.05, insig = "blank", 
         title="Correlation between KWS attitudes", diag=FALSE)

corrplot(corr.i2_in[[1]],tl.srt=0,tl.col="black",
         type="lower", order="hclust", method="color",addCoef.col = "black",
         p.mat = corr.i2_in[[3]], sig.level = 0.05, insig = "blank", pch=4, pch.cex=2,pch.col="red",
         title="Correlation between Indegrees", diag=FALSE)

corrplot(corr.i2_out[[1]],tl.srt=0,
         type="lower", order="hclust", method="color", addCoef.col = "black",
         p.mat = corr.i2_out[[3]], sig.level = 0.05, insig = "pch", pch=4, pch.cex=2,pch.col="red",
         title="Correlation between Indegrees", diag=FALSE)

corrplot(corr.i2_btw[[1]], tl.srt=0,tl.col="black",
         type="lower", order="hclust", method="color",addCoef.col = "black",
         p.mat = corr.i2_btw[[3]], sig.level = 0.05, insig = "blank", 
         main="Correlation between Betweenness", diag=FALSE)

corrplot(corr.i2_cls[[1]], tl.srt=0,tl.col="black",
         #stupid error here.doesn't want to blank out based on pvalues. 
         type="lower", order="hclust", method="color",addCoef.col = "black",
         p.mat = corr.i2_cls[[3]], sig.level = 0.05, insig = "pch", pch=4, pch.cex=2,pch.col="red",
         main="Correlation between Closeness", diag=FALSE)

ggplot(data.frame(corr.pf), aes(x=row.names(corr.pf), y=r)) + 
  geom_bar(stat="identity", aes(fill=corr.pf[,3]<0.05), guides=F) +
  xlab("Variables") + 
  ylab("Correlation") +
  ggtitle("Correlation between Variables and Performance") +  
  theme(axis.text.y=element_text(vjust=1))+
  coord_flip()

corrplot(corr.t[[1]], tl.srt=0,tl.col="black",
         type="lower", order="hclust", method="color",
         #addCoef.col = "black",
         p.mat = corr.t[[3]], sig.level = 0.05, insig = "blank",
         main="Correlation between Team Level Variables", diag=FALSE)

dev.off()

#notes:
#nothing correlates with performance
#follow-boss: r=0.89*
#indegree:ie, kw, val, contribution: r> 0.7*
#outdegree: prior relationship, contribution and kw r>0.9
#information seeking correlated with: 
#for indegree:knowing, contribution, prior relationship, valuing, prior interaction, prior contact, prior physical distance
#for outdegree:contribution, prior relationship, knowing, valuing, prior interaction, prior contact, prior physical distance, prior hierarchy, 
#for betweeness: prior interaction, contributions
#for closeness:valuing, contribution, knowing, prior relationship, 


# PCA on factors ----------------------------------------------------------
#too many missing values to compute....

#should be done on raw items for attitudes
table(is.na(att.all.raw[,c(16:53)]))

#per kw sharing attitude
library(lavaan)
ksa.m<-'ksa =~ ksa1 + ksa2 + ksa3 + ksa4 + ksa5'
ksa.fit<-cfa(ksa.m, na.omit(att.all.raw[,c(16:20)]))
summary(ksa.fit)


# analysis 1: predicting individual perception of success -----------------
# logistic regression

#how did R deal with coding of variables?
#contrasts(um.i2$performance)
#not interesting here as variables numeric for performance. 

#because low frequency of failed projects (complete or only team/tool) group them into 1
#70 NA, 14 failed, 43 successful
levels(um.i2$performance)<-c("fail", "fail", "success")

m1<-glm(performance ~ ksa + ksd + dks + kso, family = binomial, data = um.i2)
summary(m1)

m2.form<-as.formula(paste("performance ~", paste(colnames(um.i2)[8:15],collapse="+" )))
m2<-glm(m2.form, family=binomial, data=um.i2)
summary(m2)

m3.form<-as.formula(paste("performance ~", paste(colnames(um.i2)[16:24],collapse="+" )))
m3<-glm(m3.form, family=binomial, data=um.i2)
summary(m3)

#scrap that idea and think again.
#social capital = your connections. using your connection => more resources

#RQ: Are there differences in terms of network charac between projects demeed successful and unsuccessful

um.i2N<-um.i2[-which(is.na(um.i2[,7])),-ncol(um.i2)]
pdf("Difference successful vs failed project.pdf")
ggplot(melt(um.i2N[,c(2,7:15)]), 
       aes(x=variable ,y=value, fill=performance)) + 
  geom_boxplot() +
  ggtitle("Knowledge sharing attitudes")
ggplot(melt(um.i2N[,c(2,7, 16:24)]), 
       aes(x=variable ,y=value, fill=performance)) + 
  geom_boxplot() + 
  ggtitle("Indegree")
ggplot(melt(um.i2N[,c(2,7, 25:33)]), 
       aes(x=variable ,y=value, fill=performance)) + 
  geom_boxplot() + 
  ggtitle("Outdegree")
ggplot(melt(um.i2N[,c(2,7, 34:41)]), 
       aes(x=variable ,y=value, fill=performance)) + 
  geom_boxplot() + 
  ggtitle("Betweenness")
ggplot(melt(um.i2N[,c(2,7, 42:47)]), 
       aes(x=variable ,y=value, fill=performance)) + 
  geom_boxplot() + 
  ggtitle("Closeness")
dev.off()

#Differences of in terms of high and low information seeking based on median split
median(um.i2$indeg_ie, na.rm=T) #median = 0.125
summary(um.i2$indeg_ie)
um.i2$mob<-ifelse(um.i2$indeg_ie > median(um.i2$indeg_ie, na.rm=T), "high", "low")
table(um.i2$mob)

pdf("Difference high vs low information seeker.pdf")
ggplot(melt(um.i2[,c(49, 2,8:15)], id=c(1:2)), 
       aes(x=variable, y=value, fill=as.factor(mob))) +
  geom_boxplot() +
  ggtitle("Knowledge sharing attitudes")
ggplot(melt(um.i2[,c(49, 2,16:24)], id=c(1:2)), 
       aes(x=variable, y=value, fill=as.factor(mob))) +
  geom_boxplot() + 
  ggtitle("Indegree")
ggplot(melt(um.i2[,c(49, 2,25:33)], id=c(1:2)), 
       aes(x=variable, y=value, fill=as.factor(mob))) +
  geom_boxplot() + 
  ggtitle("Outdegree")
ggplot(melt(um.i2[,c(49, 2,34:41)], id=c(1:2)), 
       aes(x=variable, y=value, fill=as.factor(mob))) +
  geom_boxplot() + 
  ggtitle("Betweenness")
ggplot(melt(um.i2[,c(49, 2,42:47)], id=c(1:2)), 
       aes(x=variable, y=value, fill=as.factor(mob))) +
  geom_boxplot() + 
  ggtitle("Closeness")
dev.off()


