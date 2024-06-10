#READING IN DATA
pilot_whale_data = read.csv("pilot_whale_data.csv")
pilot_whale_data = as.data.frame(pilot_whale_data)
colnames(pilot_whale_data)[7] = 'Depth'
xy = cbind(pilot_whale_data,pilot_whale_data$DayNight,pilot_whale_data$Category)
xy = data.frame(xy)
head(xy)
library(lmerTest)
library(MuMIn)
summary(lmer(NumBuzzDive~Depth + (1|DayNight),data=pilot_whale_data))
rand(lmer(NumBuzzDive~Depth + (1|DayNight),data=pilot_whale_data))
head(pilot_whale_data)


#vioplots
library(vioplot)
vioplot(scale(yeo(pilot_whale_data[,c(9,12,13,14,19,21,23)])))
hist(yeo(pilot_whale_data[,c(9)]))

hist(log(pilot_whale_data[,c(12)]))
hist(yeo(pilot_whale_data[,c(13)]))
hist(yeo(pilot_whale_data[,c(14)]))
hist(yeo(pilot_whale_data[,c(19)]))
hist(yeo(pilot_whale_data[,c(21)]))
hist(yeo(pilot_whale_data[,c(23)]))

#TRANSFORM X VARIABLES MAKE HISTOGRAMS UNTIL "NORMAL"
#SQRT YEO LOG
#CHANGE DAYNIGHT & CATEGORY TO BINARY 1,0

vioplot(scale(pilot_whale_data[,c(7,8,11,15,22,26)]))
#numeric x vioplot
vioplot(scale(pilot_whale_data[,c(7,8,11,22)]))
#numeric x vioplot sqrt
vioplot(scale(sqrt(pilot_whale_data[,c(7,8,11,22)])))
#numeric x vioplot yeo
vioplot(scale(yeo(pilot_whale_data[,c(7,8,11,22)])))
#numeric x vioplot log
vioplot(scale(log(na.omit(pilot_whale_data[,c(7,8,11,22)]))))

hist(yeo(pilot_whale_data[,7]))
hist(yeo(pilot_whale_data[,8]))
hist(yeo(pilot_whale_data[,11]))
hist(yeo(pilot_whale_data[,22]))

hist(sqrt(pilot_whale_data[,7]))
hist(sqrt(pilot_whale_data[,8]))
hist(sqrt(pilot_whale_data[,11]))
hist(sqrt(pilot_whale_data[,22]))

#yeo johnson transform function
yeo <- function(x){
  ((x + 1)^0.0000001 - 1) / 0.0000001
}

#y variable matrix
yvar = pilot_whale_data[,c(12,13,14,19,21,23)]
#transform
for (i in 1:ncol(yvar))
  yvar[,i]=yeo(yvar[,i])
#GETTING RID OF NANs
yvar = as.matrix(yvar)
yvar[is.nan(unlist(yvar))] = NA
vioplot(scale(yvar)) #PUT THIS IN THE REPORT

#PCA
summary(princomp(na.omit(yvar)))
princomp(na.omit(yvar))$loadings
scores = princomp(na.omit(yvar))$scores
plot(scores)

#LMER COMPONENT 1
#COMPONENT 1 CONSISTS OF THE NUMBER OF BENTHIC BUZZES
lm1 = lmer(scores[,1]~Depth+Duration..s.+DayNight+Category+MinAltitude+(1|tagID),data = pilot_whale_data[as.numeric(rownames(scores)),])
summary(lm1)
#RANEF SHOWING VARIATION IN WHALES: COMPONENT 1
ranef(lm1)
#RAND MEANS SIGNIFICANT VARIATION BETWEEN WHALES: COMPONENT 1
rand(lm1)

#38% VARIANCE BETWEEN DIVES: COMPONENT 1
#36% VARIANCE BETWEEN WHALES: COMPONENT 1
#why is variance now different??
r.squaredGLMM(lm1)


#LMER COMPONENT 2
#COMPONENT 2 CONSISTS OF THE NUMBER OF BUZZES ON DESCENT
lm2 = lmer(scores[,2]~Depth+Duration..s.+DayNight+Category+MinAltitude+(1|tagID),data = pilot_whale_data[as.numeric(rownames(scores)),])
summary(lm2)
#RANEF SHOWING   : COMPONENT 2
ranef(lm2)
#RAND MEANS SIGNIFICANT VARIANCE BETWEEN WHALES? P ONLY 0.04: COMPONENT 2
rand(lm2)

#VARIANCE BETWEEN DIVES AND INDIVIDUALS
r.squaredGLMM(lm2)



#LMER COMPONENT 3
#COMPONENT 3 CONSISTS OF THE NUMBER OF SPRINT ASSOCIATED BUZZES
lm3 = lmer(scores[,3]~Depth+Duration..s.+DayNight+Category+MinAltitude+(1|tagID),data = pilot_whale_data[as.numeric(rownames(scores)),])
summary(lm3)
#RANEF SHOWING   : COMPONENT 3
ranef(lm3)
#RAND MEANS NO SIGNIFICANT VARIANCE BETWEEN WHALES:  COMPONENT 3
rand(lm3)

#VARIANCE BETWEEN DIVES AND INDIVIDUALS
r.squaredGLMM(lm3)

#GRAPHING
plot(hclust(dist(scale(t(yvar))))) #PUT THIS IN THE REPORT
t(scale(yvar))
cor(na.omit(yvar),method = "s")

duration = pilot_whale_data$Duration..s.[as.numeric(rownames(scores))]
depth = pilot_whale_data$Depth[as.numeric(rownames(scores))]
#PLOT SHOWS: MORE BUZZES ON DESCENT FOR LONGER DIVES
#WHY DOES DURATION NOT SHOW UP AS SIGNIFICANT FOR COMP2
plot(scores,cex=exp(scale(duration))^0.5) #PUT THIS IN THE REPORT
plot(scores,cex=exp(scale(duration))^0.75)

#DEPTH plot
#MORE BUZZES ON DESCENT FOR DEEPER DIVES AS WELL?
plot(scores,cex=exp(scale(depth))^0.5)
plot(scores,cex=exp(scale(depth))^0.75)
plot(scores,cex=exp(scale(depth))^1)

#PARALLEL ANALYSIS
install.packages("paran")
library(paran)
yvar2 = na.omit(yvar)
yvar2 = as.matrix(yvar2)
paran(yvar2)
