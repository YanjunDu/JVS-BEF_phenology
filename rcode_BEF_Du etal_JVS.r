##BEF Phenology study 2019-JVS ####

# #########################################################
setwd("E:\\5.BEF phenology\\JVS\\JVS-R1")
dat <- read.csv("t.test.phenology.csv",header=T)
summary(dat)

wilcox.test(dat$leaf.out~dat$group,alternative = "two.sided") # p <0.001, W=3395


par(mfrow = c(1, 2))

library("ggpubr")
tiff("Fig2a.tif",units="cm", width=8, height=8,res=400) 
ggboxplot(dat, x = "group", y = "leaf.out", font.label=12,legend='none',
          color = "group", palette = c("#00AFBB", "#E7B800"),title="",
          ylab = "Julian day of leaf-out date", xlab = "",font.x=12,font.y=12,
		  font.tickslab=12) # save 495*450
dev.off() 

tiff("Fig2b.tif",units="cm", width=8, height=8,res=400) 
dat2 <- dat[complete.cases(dat[,5]),]
wilcox.test(dat2$peak.fl~dat2$group,alternative = "two.sided") 
ggboxplot(dat2, x = "group", y = "peak.fl", font.label=12,legend='none',
          color = "group", palette = c("#00AFBB", "#E7B800"),title="",
          ylab = "Julian day of flowering date", xlab = "",font.x=12,font.y=12,
		  font.tickslab=12) # save 495*450
dev.off() 
# PS: merge two plots - Fig2.

  
############ Figure 3.  ##### ##### ##### ##### ##### ##### 
setwd("E:\\5.BEF phenology\\JVS\\JVS-R1")
library(ggplot2)
dat <- read.csv("Data_BEF_phenology.csv",header=T)
dat2 <- dat[complete.cases(dat[,3]),]
attach(dat2)

# scale plot size
tiff("Fig3.tif",units="cm", width=20, height=18,res=400)
ggplot(dat2, aes(diversity, leaf.out, shape=Species, colour=Species, fill=Species)) +
  geom_smooth(method="lm",se=FALSE) +
  geom_point(cex=dat2$N^5*3000, alpha=0.5) +
  scale_shape_manual(values=c(1,7,9,15,16,17,23))+
  labs(x = "Target species diversity",y = "Julian day of Leaf-out") +
  scale_y_continuous(breaks = c(60,70,80,90,100,110,120)) + 
  scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
  theme_grey(base_size =14)

dev.off()


#################  # Table2  ####################
dat <- read.csv("Data_BEF_phenology.csv",header=T)
summary(dat)
#dat2 <- dat[complete.cases(dat[,3]),]
sp1 <- dat[which(dat$Species=="Daphniphyllum oldhami"),]
dat2 <- sp1[complete.cases(sp1[,3]),]
dat2
m1 <- lm(dat2$leaf.out~dat2$diversity)
summary(m1)


############ Fig4.  flowering    ##### ##### ##### ##### ##### ##### 
library(ggplot2)
dat <- read.csv("Data_BEF_phenology.csv",header=T)
dat2 <- dat[complete.cases(dat[,4]),]
dat3 <- dat2[dat2$Species%in%c("Quercus glandulifera","Cyclobalanopsis glauca","Sapium discolor","Sapium sebiferum"),]
attach(dat3)

tiff("Fig4.tif",units="cm", width=20, height=18,res=400) 
ggplot(dat3, aes(diversity, Julian.peak, shape=Species, colour=Species, fill=Species)) +
  geom_smooth(method="lm",se=FALSE) +
  geom_point(size=4) +
  scale_shape_manual(values=c(1,7,9,15,16,17,23))+
  labs(x = "Target species diversity",y = "Julian day of peak flowering") +
  scale_y_continuous(breaks = c(80,100,120,140,160,180)) + 
  scale_x_continuous(breaks = c(0,4,8,12,16,20,24))+
  theme(axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))+
  theme_grey(base_size =14)
dev.off() 


  
######################################################
### 加环境因子+多样性因子  ##
 block and species ID -random effect 
 Before fitting models, all numeric variables were
standardized using the scale function in R. We
We used the dredge function in the
MuMIn package (41) of R to fit all possible models;
# library(lme4)
# library(MuMIn)# r.squaredGLMM(model1)
library(lmerTest)
setwd("E:\\5.BEF phenology\\JVS\\JVS-R1")
dat <- read.csv("Data_BEF_phenology.csv",header=T)
summary(dat)
dat2 <- dat[complete.cases(dat[,3:4]),]
summary(dat2)


m1 <- lm(dat2$leaf.out~dat2$diversity)
summary(m1)
plot(dat2$leaf.out~dat2$diversity)
abline(m1)

#  
model1 <- lmer(leaf.out ~ scale(N) +  scale(C) + scale(diversity) + +(1|species.cn)+(1|plot.ID), data=dat2)
summary(model1)

#
model2 <- lmer(Julian.peak~ scale(N) +  scale(C) + scale(diversity)+  +(1|species.cn)+(1|plot.ID), data=dat2)
summary(model2)


#
m1 <- lm(dat2$leaf.out~dat2$scale(N))
summary(m1)
plot(dat2$leaf.out~dat2$N)
abline(m1)

model1 <- lmer(leaf.out ~ scale(N) +  scale(C) + scale(diversity) +scale(N)*scale(diversity) +(1|species.cn)+(1|plot.ID), data=dat2)
summary(model1)

m1 <- lm(dat2$diversity~dat2$N)
summary(m1)
plot(dat2$diversity~dat2$N)


