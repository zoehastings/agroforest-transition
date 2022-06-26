#Zoe Hastings
#University of Hawaii at Manoa
#June 2022
#Non-native fallows hold high potential for restoration through agroforestry in a Pacific Island ecosystem


#set working directory
setwd("xxx")


#### Changes in plant community characteristics btwn trts pre- and 2 yrs. post-restoration (Fig. 4) ####

#read in dataset
#plot means (only use this for tree density)
sumplot<-read.csv(file="summaryplot.csv",header=TRUE)
str(sumplot)

#subplot data
sumsubplot<-read.csv(file="summarysubplot.csv",header=TRUE)
str(sumsubplot)

#convert to factor
sumplot$year<-as.factor(sumplot$year)
sumplot$plot<-as.factor(sumplot$plot)
sumplot$trt<-as.factor(sumplot$trt)
sumsubplot$year<-as.factor(sumsubplot$year)
sumsubplot$plot<-as.factor(sumsubplot$plot)
sumsubplot$trt<-as.factor(sumsubplot$trt)
sumsubplot$subplot<-as.factor(sumsubplot$subplot)

#check histograms
hist(change$treedens)
hist(change$peragund)
hist(change$percanopcov)
hist(change$perweed)
hist(change$rich_agund)
hist(change$rich_weedund)

#read in ggplot
library(ggplot2)

### make boxplots ###
#tree density
bxpl1 <- ggplot(sumplot, aes(x=year, y=treedens, fill=trt)) + 
  geom_boxplot() + theme_classic() + scale_fill_brewer(palette="Greys")+
  labs(x = "", y = "Tree (>5cm dbh density)")+
  theme(legend.position="none")
bxpl1
#canopy cover percent
bxpl2 <- ggplot(sumsubplot, aes(x=year, y=percanopcov, fill=trt)) + 
  geom_boxplot() + theme_classic() + scale_fill_brewer(palette="Greys")+
  labs(x = "", y = "Canopy cover (%)")+ ylim(1,100) +
  theme(legend.position="none")
bxpl2
#understory cover of weeds
bxpl3 <- ggplot(sumsubplot, aes(x=year, y=perweed, fill=trt)) + 
  geom_boxplot() + theme_classic() + scale_fill_brewer(palette="Greys")+
  labs(x = "", y = "Understory cover weeds (%)")+
  theme(legend.position="none") + scale_y_continuous(limits=c(1,100),breaks=c(25,50,75,100))
bxpl3
#understory cover of agroforest species
bxpl4 <- ggplot(sumsubplot, aes(x=year, y=peragund, fill=trt)) + 
  geom_boxplot() + theme_classic() + scale_fill_brewer(palette="Greys")+
  labs(x = "", y = "Understory cover agroforest (%)")+
  theme(legend.position="none") + scale_y_continuous(limits=c(1,100),breaks=c(25,50,75,100))
bxpl4
#understory richness of agroforest species
bxpl5 <- ggplot(sumsubplot, aes(x=year, y=rich_agund, fill=trt)) + 
  geom_boxplot() + theme_classic() + scale_fill_brewer(palette="Greys")+
  labs(x = "", y = "Understory richness agroforest")+
  theme(legend.position="none") + scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))
bxpl5
#understory richness of weed species
bxpl6 <- ggplot(sumsubplot, aes(x=year, y=rich_weedund, fill=trt)) + 
  geom_boxplot() + theme_classic() + scale_fill_brewer(palette="Greys")+
  labs(x = "", y = "Understory richness weeds") + theme(legend.position="none") +
  scale_y_continuous(limits=c(1,7),breaks=c(1,2,3,4,5,6,7))
bxpl6

#arrange the boxplots and make them the same size
install.packages("cowplot")
library(cowplot)
fig1<-plot_grid(bxpl1,bxpl2,bxpl3,bxpl4,bxpl6,bxpl5, align="v", nrow=3, rel_heights=c(1/3, 1/3, 1/3),
                labels = c("(a)","(b)","(c)","(d)","(e)","(f)"), label_size = 12,
                hjust = -0.1)
fig1
#extract legend from bxpl1
legend_b<-get_legend(bxpl1 + guides(color = guide_legend(nrow = 1)) +
                       theme(legend.position = "bottom") + 
                       scale_fill_brewer(palette="Greys",
                                         name="Treatment",
                                         labels=c("erosion control","early succession")))

#add legend to combined plot 
plot_grid(fig1, legend_b, ncol = 1, rel_heights = c(1, .1))


### Run stats to explore effects of treatment and time ###
install.packages("glmmTMB")
library(glmmTMB)
install.packages("lme4")
library(lme4)

###(a) tree density###
hist(sumplot$treedens)
tdens<-glm(treedens ~ trt + year + trt:year, 
           data=sumplot, family=quasipoisson)
summary(tdens)

###(b) canopy cover###
hist(sumsubplot$percanopcov)
canopy1<-lmer(percanopcov ~ trt + year +  trt:year + (1|plot),
              data=sumsubplot)
canopy2<-lmer(percanopcov ~ trt + year +  (1|plot),
              data=sumsubplot)

#compare the models
install.packages("MuMIn")
library(MuMIn)
AICc(canopy1,canopy2) #model with interaction is best
summary(canopy1)

#look at residuals
ranef(canopy1) #random effects of model
fixef(canopy1) #fixed parameter estimates
resid(canopy1) #residuals
fitted(canopy1) #fitted values
qqnorm(resid(canopy1))#quantile-quantile plot of residuals
hist(resid(canopy1)) #histogram of residuals
library(lattice) #for smoother
xyplot(resid(canopy1)~fitted(canopy1), #residuals vs fitted with smoother
       panel=function(x,y){panel.xyplot(x,y); panel.loess(x,y, span=0.75)})


###(c) understory cover weeds###
hist(sumsubplot$perweed)
covwe1<-lmer(perweed ~ trt + year + trt:year + (1|plot),
             data=sumsubplot)
covwe2<-lmer(perweed ~ year + trt + (1|plot),
             data=sumsubplot)
covwe3<-lmer(perweed ~ year + (1|plot),
             data=sumsubplot)
summary(covwe1)
AICc(covwe1,covwe2,covwe3) #first model is best fit

#look at residuals
ranef(covwe1) #random effects of model
fixef(covwe1) #fixed parameter estimates
resid(covwe1) #residuals
fitted(covwe1) #fitted values
qqnorm(resid(covwe1))#quantile-quantile plot of residuals
hist(resid(covwe1)) #histogram of residuals
xyplot(resid(covwe1)~fitted(covwe1), #residuals vs fitted with smoother
       panel=function(x,y){panel.xyplot(x,y); panel.loess(x,y, span=0.75)})
plot(resid(covwe)~sumsubplot$trt) #residuals by trt
length(sumsubplot$trt)
length(resid(covwe))


###(d) understory cover agroforest###
yr1<-subset(sumsubplot, year=="2018")
yr2<-subset(sumsubplot, year=="2020")

hist(yr2$peragund)
agund<-glmmTMB(peragund ~ trt+ (1|plot), 
               data=yr2, ziformula=~1,
               family=poisson) 
summary(agund)

agund2<-glmmTMB(peragund ~ trt+ (1|plot), 
                data=yr2, family=poisson) 
summary(agund2)

AIC(agund,agund2)

agund3<-glmmTMB(peragund ~ 1+ (1|plot), 
                data=yr2, ziformula=~1,
                family=poisson) 

AIC(agund,agund2,agund3)

###(e) understory richness weeds###

richweedyr<-read.csv(file="summarysubplot_richweed.csv",header=TRUE)
str(richweedyr)
richweedyr$trt<-as.factor(richweedyr$trt)
richweedyr$subplot<-as.factor(richweedyr$subplot)
histogram(richweedyr$rich_weedund18)
histogram(richweedyr$rich_weedund20)
histogram(log(richweedyr$rich_weedund18))
histogram(log(richweedyr$rich_weedund20))

richwe20a<-lmer(rich_weedund20 ~ rich_weedund18 + trt + rich_weedund18:trt + (1|plot),
                data=richweedyr)
richwe20b<-lmer(rich_weedund20 ~ rich_weedund18 + trt + (1|plot), #best fit
                data=richweedyr)
richwe20c<-lmer(rich_weedund20 ~ trt + (1|plot),
                data=richweedyr)
richwe20d<-lmer(rich_weedund20 ~ 1 + (1|plot),
                data=richweedyr)
AICc(richwe20a,richwe20b,richwe20c,richwe20d)
summary(richwe20b)

ranef(richwe20a) #random effects of model
fixef(richwe20a) #fixed parameter estimates
resid(richwe20a)#residuals
fitted(richwe20a)#fitted values

qqnorm(resid(richwe20a))#quantile-quantile plot of residuals
hist(resid(richwe20a)) #histogram of residuals
library(lattice)
xyplot(resid(richwe20a)~fitted(richwe20a), #residuals vs fitted with smoother
       panel=function(x,y){panel.xyplot(x,y); panel.loess(x,y, span=0.75)})

###(f) understory richness agroforest###
hist(sumsubplot$rich_agund) #zero inflated

richag<-glmmTMB(rich_agund ~ trt + (1|plot), 
                data=yr2, ziformula=~1,family=poisson)
summary(richag)
richag2<-glmmTMB(rich_agund ~ trt + (1|plot), 
                 data=yr2,ziformula=~1,family=poisson) 
summary(richag2)

AIC(richag,richag2)
richag3<-glmmTMB(rich_agund ~ 1 + (1|plot), 
                 data=yr2,ziformula=~1,family=poisson)
summary(richag3)

AIC(richag,richag2, richag3)


#### Comparing rates of outplant survival across treatments and species (Fig. 5) ####

#read in datasets
surv1<-read.csv(file="surv_yr1.csv",header=TRUE) #this is survival from 6mo to 1 year post-planting
surv2<-read.csv(file="surv_yr1.5.csv",header=TRUE) #this is survival from 1 year to 1.5 year post-planting
str(surv1)
str(surv2)

#convert variables to factor
surv1$plot<-as.factor(surv1$plot)
surv1$tag<-as.factor(surv1$tag)
surv2$plot<-as.factor(surv2$plot)
surv2$tag<-as.factor(surv2$tag)
surv1$surv1yr<-as.numeric(surv1$surv1yr)

#visualize rates
hist(surv1$surv1yr)
hist(surv2$surv1.5yr)

tapply(surv1$surv1yr,surv1$species,mean)
tapply(surv2$surv1.5yr,surv2$species,mean)
tapply(surv1$surv1yr,surv1$plot,mean)
tapply(surv2$surv1.5yr,surv2$plot,mean)
#range of surv is low across the plots, meaning diff in trt/management
#good from an agro perspective, not some areas of the ridge that are more finicky

tapply(surv1$surv1yr,surv1$trt,mean)
tapply(surv2$surv1.5yr,surv2$trt,mean)
tapply(surv1$surv1yr,surv1$trt,sd)
tapply(surv2$surv1.5yr,surv2$trt,sd)

boxplot(data1$canopycov~data1$plot) 
boxplot(data1$perweed~data1$plot)

##Make graph of rates that include 0-6 mo, 1yr, and 1.5yrs
survtot3<-read.csv(file="surv_tot.csv",header=TRUE)

#calculate survival rates by treatment for each timestamp
tapply(survtot3$surv6mo,survtot3$trt,mean)
tapply(survtot3$surv1yr,survtot3$trt,mean)
tapply(survtot3$surv1.5yr,survtot3$trt,mean)

#make a dataframe with survival rates by trt at 1yr and 1.5yr
survrates3 <- data.frame(trt=rep(c("Early succession","Erosion control"),4),
                         time=rep(c("0","0","0.5","0.5","1","1","1.5","1.5")),
                         rate=c(100, 100, 63.5, 66.0, 35.6, 41.4, 28.4, 35.1))

#make a line plot of survrates by trt
ggplot(survrates3, aes(x=time, y=rate, group=trt)) +
  geom_line(aes(linetype=trt))+
  geom_point()+
  ylim(0, 100)+
  scale_linetype_discrete(name="Treatment")+
  labs(x="Time after planting (years)", y = "Survival rate (%)")

##add species to plot
#make dataframe
library(plyr)
library(dplyr)
spdf0<-data.frame(species=c("aalii","awa","aweoweo","iholena","koa","maile","mamaki",
                            "ohia_ahihi","ohia_mamo","pohinahina","pualoalo"),
                  time=rep(c(0.0),11),
                  trt=c("e","b","s","e","s","b","s","b","e","e","s"),
                  mean=rep(c(1.000),11))

spdf=ddply(survtot3, "species", summarise, mean = mean(surv6mo))
spdf$time=rep(c(0.5),11)
spdf$trt=c("e","b","s","e","s","b","s","b","e","e","s")

spdf1=ddply(survtot3, "species", summarise, mean = mean(surv1yr))
spdf1$time=rep(c(1.0),11)
spdf1$trt=c("e","b","s","e","s","b","s","b","e","e","s")

spdf2=ddply(survtot3, "species", summarise, mean = mean(surv1.5yr))
spdf2$time=rep(c(1.5),11)
spdf2$trt=c("e","b","s","e","s","b","s","b","e","e","s")

#merge the three dataframes so that all times are together
df_list <- list(spdf0, spdf, spdf1, spdf2)
spdf3<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list, accumulate=FALSE)
spdf3$rate<-(spdf3$mean)*100
spdf3$trt=factor(spdf3$trt, levels=c("s","e","b"))
spdf3

#make the dataframe with treatment means
survrates3$trt<-c("s","e","s","e","s","e","s","e")
str(survrates3)
survrates3$trt<-as.factor(survrates3$trt)
survrates3$time<-c("0","0","0.5","0.5","1","1","1.5","1.5")
survrates3$time<-as.numeric(survrates3$time)

#make a line plot of survrates by trt
ggplot(spdf3, aes(x=time, y=rate, col=trt)) +
  geom_line(aes(group=species),linetype="dotted",size=1.05) +
  geom_point() +
  geom_line(data=survrates3, size=2) +
  theme_bw()+
  ylim(0, 100)+
  scale_color_brewer(palette="Set2",labels=c("Early succession","Erosion control", "Both"),
                     name="Treatment") +
  labs(x="Time after planting (years)", y = "Survival rate (%)") +
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        axis.title = element_text(size = 16))

#check for color blind friendly palette
library(RColorBrewer)
display.brewer.all(colorblindFriendly=TRUE)

#### Changes in understory community composition (Fig. 6) ####
#code for NMDS

#read in packages
library(vegan)
library(MASS)
library(nlme)
library(tidyverse)

#read in dataset
undercov<-read.csv(file="understory_cover6.csv",header=TRUE)
str(undercov)
head(undercov)

undercov$year<-as.factor(undercov$year)
undercov$plot<-as.factor(undercov$plot)

species<-undercov[6:25] #remove name columns
head(species)

#run metaMDS
nmds <- metaMDS(species, #the species matrix
                k=3, #can specify the number of reduced dimensions
                trymax=800)
nmds #display results
#dimensions: 3
#Stress: 0.09973 
#Stress type 1, weak ties
#Two convergent solutions found after 196 tries

#Make a Shepard stress plot to look at the relationship between
#the actual dissimilarities between objects and the ordination distances
#if well correlated, ordination stress will be low and visualization good
stressplot(nmds) #decent linear relationship

###PLOT the NMDS using ggplot###

#build a data frame with NMDS coordinates
MDS1 = nmds$points[,1]
MDS2 = nmds$points[,2]

#define groups
yr<-c(rep("Pre-restoration",48),rep("Post-restoration",58))

nmds.gg = data.frame(NMDS1 = MDS1, NMDS2 = MDS2)
nmds.gg$yr <- yr   #add the year variable created earlier

head(nmds.gg)
nmds.gg$yr<- factor(nmds.gg$yr,levels = c("Pre-restoration","Post-restoration")) #order the factor

#build a data frame with NMDS species coordinates
sp1 = nmds$species[,1]
sp2 = nmds$species[,2]
nmds.sp = data.frame(NMDS1 = sp1, NMDS2 = sp2)
nmds.sp$species = rownames(nmds.sp)
status<-c(rep("nonnative",10),rep("native",8),rep("nonnative",2))
nmds.sp$status<- status
nmds.sp$status<-factor(nmds.sp$status,levels=c("native","nonnative"))
rank<-c(rep("0",10),rep("1",8),rep("0",2))
nmds.sp$rank<- rank
nmds.sp$rank<-as.integer(nmds.sp$rank)
str(nmds.sp)
head(nmds.sp)

#make NMDS plot and include species labels
ggplot() +
  geom_point(data=nmds.gg, aes(x=NMDS1, y=NMDS2, col=yr)) +
  #geom_point(data=nmds.sp, aes(x=NMDS1, y=NMDS2, col="grey")) +
  geom_text(data=nmds.sp, aes(x=NMDS1, y=NMDS2,size=status), 
            label=nmds.sp$species,check_overlap = TRUE, nudge_y = 0.05) + 
  stat_ellipse(data=nmds.gg, aes(x=NMDS1, y=NMDS2, col=yr)) +
  theme_bw()+ 
  scale_size_discrete(name="Species status", 
                      labels=c("native"="outplant", "nonnative"="weed"),
                      limits=c("nonnative","native"),
                      range=c(3.5,5))+
  scale_color_brewer(palette="Set2",name="Time", labels=c("2018","2020")) +
  guides(colour = guide_legend(order = 1), 
         size = guide_legend(order = 2))
#### Predictors of understory cover of agroforestry species-1 on 1 regressions (Fig. 7) ####
#read in dataset
data<-read.csv(file="agunder_predictors.csv",header=TRUE)
str(data)
head(data)

#explore predictor variables
hist(data$perweed) #percent understory cover of weeds
hist(data$canopycov) #percent canopy cover
hist(data$percarb) #percent soil carbon
hist(data$agunder) #percent understory cover of agroforestry species
hist(data$domweed) #category of dominant weed
hist(log(data$agunder+0.1))

#subset dataset by time period
data1<-subset(data,time=="6mo1yr")
str(data1)
data2<-subset(data,time=="1yr1.5yr")
str(data2)

#read in packages
install.packages("glmmTMB")
library("glmmTMB")
install.packages("bbmle")
library("bbmle") ## for AICtab
library("ggplot2")


### Model year 1 (6mo - 1yr post-planting) ###

#explore the data
hist(data1$agunder)
hist(data1$perweed)
boxplot(agunder~plot, data=data1)
boxplot(agunder~trt, data=data1)
boxplot(perweed~trt, data=data1)
boxplot(perweed~plot, data=data1)

#plot regressions
plot(agunder~trt, data=data1)
plot(agunder~canopycov, data=data1)
plot(agunder~perweed, data=data1)
plot(agunder~domweed, data=data1)

## FIT A MODEL FOR TRT ##
#agunder ~ trt (model for the conditional effects)
# (1|plot) (model for the random effect and grouping factor)
# specify error distribution: zero-inflated poisson or negative binomial
#ziformula~1 #a zero-inflated Poisson model with a single zero-inflation parameter applying to all observations

fit_zipoisson<-glmmTMB(agunder ~ trt + (1|plot), data=data1, ziformula=~1, family=poisson)
fit_zinbinom2<-update(fit_zipoisson, family=nbinom2) # "NB2" fit
#default negative binomial parameterization
fit_zinbinom1<-update(fit_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(fit_zipoisson,fit_zinbinom2,fit_zinbinom1) # fit_zinbinom2 is best
summary(fit_zinbinom2) # trt NOT a significant predictor
#resid(fit_zinbinom2) #residuals
#fitted()#fits
#ranef() #eBLUPs
#fixef()#fixed parameter estimates


## FIT A MODEL FOR CANOPYCOV ##
fit2_zipoisson<-glmmTMB(agunder ~ canopycov + (1|plot), data=data1, ziformula=~1, family=poisson)
fit2_zinbinom2<-update(fit2_zipoisson, family=nbinom2) # "NB2" fit
fit2_zinbinom1<-update(fit2_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(fit2_zipoisson,fit2_zinbinom2,fit2_zinbinom1) # fit_zinbinom2 is best
summary(fit2_zinbinom2)  # canopycov NOT a significant predictor


## FIT A MODEL FOR PERWEED ##
fit3_zipoisson<-glmmTMB(agunder ~ perweed + (1|plot), data=data1, ziformula=~1, family=poisson)
fit3_zinbinom2<-update(fit3_zipoisson, family=nbinom2) # "NB2" fit
fit3_zinbinom1<-update(fit3_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(fit3_zipoisson,fit3_zinbinom2,fit3_zinbinom1) # fit_zinbinom2 is best
summary(fit3_zinbinom2)  # perweed IS a significant predictor


## FIT A MODEL FOR DOMWEED ##
fit4_zipoisson<-glmmTMB(agunder ~ domweed + (1|plot), data=data1, ziformula=~1, family=poisson)
fit4_zinbinom2<-update(fit4_zipoisson, family=nbinom2) # "NB2" fit
fit4_zinbinom1<-update(fit4_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(fit4_zipoisson,fit4_zinbinom2,fit4_zinbinom1) # fit_zinbinom2 is best
summary(fit4_zinbinom2)  # domweed IS a significant predictor

## SUMMARY of agunder predictors in yr 1 ##
#NOT significant predictors: trt, canopycov
#SIGNIFICANT predictors: perweed, domweed


### Model year 2 (1yr - 1.5 yrs post-planting) ###

#explore the data
hist(data2$agunder)
hist(data2$perweed)
boxplot(agunder~plot, data=data2)
boxplot(agunder~trt, data=data2)
boxplot(perweed~trt, data=data2)
boxplot(perweed~plot, data=data2)

#plot regressions
plot(agunder~trt, data=data2)
plot(agunder~canopycov, data=data2)
plot(agunder~perweed, data=data2)
plot(agunder~domweed, data=data2)


## FIT A MODEL FOR TRT ##
yr2fit_zipoisson<-glmmTMB(agunder ~ trt + (1|plot), data=data2, ziformula=~1, family=poisson)
yr2fit_zinbinom2<-update(yr2fit_zipoisson, family=nbinom2) # "NB2" fit
#model convergence problem
yr2fit_zinbinom1<-update(yr2fit_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(yr2fit_zipoisson,yr2fit_zinbinom2,yr2fit_zinbinom1) # fit_zinbinom1 and zinbiom2 are same
summary(yr2fit_zinbinom1) # trt NOT a significant predictor


## FIT A MODEL FOR CANOPYCOV ##
yr2fit2_zipoisson<-glmmTMB(agunder ~ canopycov + (1|plot), data=data2, ziformula=~1, family=poisson)
yr2fit2_zinbinom2<-update(yr2fit2_zipoisson, family=nbinom2) # "NB2" fit
#model convergence problem
yr2fit2_zinbinom1<-update(yr2fit2_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(yr2fit2_zipoisson,yr2fit2_zinbinom2,yr2fit2_zinbinom1) # fit_zinbinom1 is best
summary(yr2fit2_zinbinom1) # canopycov NOT a significant predictor


## FIT A MODEL FOR PERWEED ##
yr2fit3_zipoisson<-glmmTMB(agunder ~ perweed + (1|plot), data=data2, ziformula=~1, family=poisson)
yr2fit3_zinbinom2<-update(yr2fit3_zipoisson, family=nbinom2) # "NB2" fit
yr2fit3_zinbinom1<-update(yr2fit3_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(yr2fit3_zipoisson,yr2fit3_zinbinom2,yr2fit3_zinbinom1) # fit_zinbinom2 is best
summary(yr2fit3_zinbinom2) # perweed IS a significant predictor


## FIT A MODEL FOR DOMWEED ##
yr2fit4_zipoisson<-glmmTMB(agunder ~ domweed + (1|plot), data=data2, ziformula=~1, family=poisson)
yr2fit4_zinbinom2<-update(yr2fit4_zipoisson, family=nbinom2) # "NB2" fit
# convergence problem
yr2fit4_zinbinom1<-update(yr2fit4_zipoisson, family=nbinom1) # "NB1 fit

#compare model fit
AICtab(yr2fit4_zipoisson,yr2fit4_zinbinom2,yr2fit4_zinbinom1) # fit_zinbinom2 is best
summary(yr2fit4_zinbinom2) # domweed IS a significant predictor


## SUMMARY of agunder predictors yr2##
#NOT significant predictors: trt, canopycov
#SIGNIFICANT predictors: perweed, domweed


## PERWEED ~ TRT ##
hist(data2$perweed) #left/negative skewed

yr2fit_perweed<-glm(perweed~trt + (1|plot), data=data2)
summary(fit_perweed)
yrfit2_perweed<-glm(perweed~canopycov + (1|plot), data=data2)
summary(fit2_perweed) # canopycov IS significant predictor of perweed


### Create figure showing regressions for both yrs, smoothers only if significant ###
library(MASS)

#re-labeling the facet
time_list<-c('6mo1yr' = "a",
             '1yr1.5yr' = "b")
data$time<- factor(data$time,levels = c("6mo1yr", "1yr1.5yr")) #order the facet

ptrf<-ggplot(data, aes(trt, agunder))+
  geom_point()+
  labs(x = "Treatment", y = "Understory cover agroforest sp. (%)")+
  facet_grid(cols = vars(time_f), labeller=as_labeller(time_list)) +
  theme_bw()

pcaf<- ggplot(data, aes(canopycov, agunder))+
  geom_point()+
  labs(x = "Canopy cover (%)", y = "")+
  facet_grid(cols = vars(time_f),labeller=as_labeller(time_list)) +
  theme_bw()

pwef<-ggplot(data, aes(perweed, agunder))+
  geom_point()+
  labs(x = "Understory cover weeds (%)", y = "Understory cover agroforest sp. (%)")+
  geom_smooth(method = "glm.nb", se=FALSE, color="grey")+  ##add smoother since significant - use negative binomial
  facet_grid(cols = vars(time_f),labeller=as_labeller(time_list)) +
  theme_bw()

pdof<- ggplot(data, aes(domweed, agunder))+
  geom_point()+
  labs(x = "Dominant weed type (ranked by density of root and surface cover)", y = "")+
  geom_smooth(method = "glm.nb", se=FALSE, color="grey")+ ##add smoother since signficant
  facet_grid(cols = vars(time_f),labeller=as_labeller(time_list)) +
  theme_bw()

#arrange the plots together
library(gridExtra)
grid.arrange(ptrf,pcaf,pwef,pdof)


#### Predictors of understory cover of agroforestry species-SEM (Fig. 2) ####

### YEAR 1 ###

library(lavaan)
yr1mod1a <- 'agunder ~ trt + canopycov + perweed
perweed ~ trt + canopycov'
yr1mod1a.fit <- sem(yr1mod1a, data=data1, estimator="MLM") 
summary(yr1mod1a.fit,modindices=TRUE)

yr1mod2a<-'agunder ~ trt + perweed
perweed ~ trt + canopycov'
#removed agunder~canopycov since it had highest P of regressions in mod1a
yr1mod2a.fit <- sem(yr1mod2a, data=data1, estimator="MLM") 
summary(yr1mod2a.fit)
#df=1, model P-value = 0.840 - model is significant

yr1mod3a<-'agunder ~ perweed
perweed ~ trt + canopycov'
#removed agunder~trt b/c highest P of regressions in mod2a
yr1mod3a.fit <- sem(yr1mod3a, data=data1, estimator="MLM") 
summary(yr1mod3a.fit)
#model is significant

yr1mod4a<-'agunder ~ perweed
perweed ~ canopycov'
#removed perweed~trt b/c highest P of regressions in mod3a, but it also has 
#the highest estimate/co-efficient
yr1mod4a.fit <- sem(yr1mod4a, data=data1, estimator="MLM") 
summary(yr1mod4a.fit)
#model is significant and both regressions are significant

#compare using AIC
library(AICcmodavg)

aictab(list(yr1mod1a.fit,yr1mod2a.fit,yr1mod3a.fit)
       ,c("yr1mod1a","yr1mod2a","yr1mod3a"))

anova(yr1mod2a.fit,yr1mod3a.fit,yr1mod4a.fit)
#mod3a has lower AIC, mod4a has lower BIC 
#Chisq indicates we can drop trt

#mod3a = agunder ~ perweed
#perweed ~ trt + canopycov
#mod4a = agunder ~ perweed
#perweed ~ canopycov

#now interpret results using standardized values:
summary(yr1mod3a.fit, rsq=T,standardized=T) 
summary(yr1mod4a.fit, rsq=T,standardized=T) 

#add in spatial autocorrelation
library(survey)
library(lavaan.survey)

#mod3a
yr1mod3a.fit<-sem(yr1mod3a,data=data1, meanstructure=T) 

survey.design3<-svydesign(ids=~plot,nest=TRUE,data=data1)

yr1mod3a.fit.adj<-lavaan.survey(lavaan.fit=yr1mod3a.fit, survey.design=survey.design3)

summary(yr1mod3a.fit.adj,rsq=T)
summary(yr1mod3a.fit.adj,rsq=T,standardized=T) 

#mod4a
yr1mod4a.fit<-sem(yr1mod4a,data=data1, meanstructure=T) 

survey.design4<-svydesign(ids=~plot,nest=TRUE,data=data1)

yr1mod4a.fit.adj<-lavaan.survey(lavaan.fit=yr1mod4a.fit, survey.design=survey.design4)

summary(yr1mod4a.fit.adj,rsq=T)
summary(yr1mod4a.fit.adj, rsq=T,standardized=T) 


### YEAR 2 ###

## MODEL 1 ##
yr2mod1 <- 'agunder ~ trt + canopycov + perweed
perweed ~ trt + canopycov
canopycov ~ trt'
yr2mod1.fit <- sem(yr2mod1, data=data2, estimator="MLM") 
summary(yr2mod1.fit) #model is saturated, no test statistic
#cant get modification indices since no test statistics


## MODEL 2 ##
#removed perweed~canopycov because highest P value of regressions

yr2mod2 <- '
agunder ~ trt + canopycov + perweed
perweed ~ trt
canopycov ~ trt'
yr2mod2.fit <- sem(yr2mod2, data=data2, estimator="MLM") 
summary(yr2mod2.fit,modindices=TRUE)
#no modification indices over 1, so not going to add any links


## MODEL 3 ##
#removing agunder~trt b/c highest P value of regressions

yr2mod3 <- '
agunder ~ canopycov + perweed
perweed ~ trt
canopycov ~ trt'
yr2mod3.fit <- sem(yr2mod3, data=data2, estimator="MLM") 
summary(yr2mod3.fit)

## MODEL 4 ##
#removing agunder~canopycov b/c highest P value of regressions

yr2mod4 <- '
agunder ~ perweed
perweed ~ trt
canopycov ~ trt'
yr2mod4.fit <- sem(yr2mod4, data=data2, estimator="MLM") 
summary(yr2mod4.fit)

## MODEL 5 ##
#removing perweed~trt b/c highest P value of regressions

yr2mod5 <- '
agunder ~ perweed
canopycov ~ trt'
yr2mod5.fit <- sem(yr2mod5, data=data2, estimator="MLM") 
summary(yr2mod5.fit) #the model regressions are both significant

## MODEL 6 ##
#going back to MOD1, removing agunder~trt b/c 2nd highest P value of regressions

yr2mod6 <- '
agunder ~ canopycov + perweed
perweed ~ trt + canopycov
canopycov ~ trt'
yr2mod6.fit <- sem(yr2mod6, data=data2, estimator="MLM") 
summary(yr2mod6.fit) #the model regressions are both significant

## MODEL 7 ##
#removing perweed~canopycov b/c highest P value of regressions
#but if I do that it's the same as MOD3
#instead removing agunder~canopycov b/c 2nd highest P

yr2mod7 <- '
agunder ~ perweed
perweed ~ trt + canopycov
canopycov ~ trt'
yr2mod7.fit <- sem(yr2mod7, data=data2, estimator="MLM") 
summary(yr2mod7.fit) #the model regressions are both significant

#all the rest of the models would be the same as ones I've alread run

#compare using AIC
aictab(list(yr2mod1.fit,yr2mod2.fit,yr2mod3.fit,yr2mod4.fit,yr2mod5.fit,
            yr2mod6.fit,yr2mod7.fit)
       ,c("yr2mod1","yr2mod2","yr2mod3","yr2mod4","yr2mod5","yr2mod6",
          "yr2mod7"))
#mod5 is best

anova(yr2mod1.fit,yr2mod2.fit,yr2mod3.fit,yr2mod4.fit,
      yr2mod5.fit,yr2mod6.fit,yr2mod7.fit)
#mod5 is best

#now interpret results using standardized values:
summary(yr2mod5.fit, rsq=T,standardized=T)

#adjust for spatial autocorrelation
yr2mod5.fit<-sem(yr2mod5,data=data2, meanstructure=T) 

survey.design5<-svydesign(ids=~plot,nest=TRUE,data=data2)
#warning message: No weights or probabilities supplied, assuming equal probability

yr2mod5.fit.adj<-lavaan.survey(lavaan.fit=yr2mod5.fit, survey.design=survey.design5)

summary(yr2mod5.fit.adj,rsq=T)
summary(yr2mod5.fit.adj, rsq=T,standardized=T)


