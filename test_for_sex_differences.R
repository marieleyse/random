#Marie-Elyse Lafaille-Magnan, PhD
#marie-elyse.lafaille-magnan@mail.mcgill.ca

setwd("/Users/Marie-Elyse/Downloads")
NEW = read.csv("MAVAN_DATA_APR2020_fix_wo_nan.csv")
#LONG = read.csv("longitudinal_adhd_sex.csv")

###works
###REDO WITH THE PARENT SPECIFIC FOR THE TESTS
####parent conners_parent_60	conners_parent_72	SDQ_parent_60	SDQ_parent_72

png(filename = 'sex_differences_conners.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("conners_mother_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.60m", "conners_father_adhd_score.72m")],
       y = list(NEW$gender_male), # we make this a list so it has length(1)
       ylab = c("conners_mother_adhd_score.60m", "conners_mother_adhd_score.72m", "conners_father_adhd_score.60m", "conners_father_adhd_score.72m"),
       xlab = "" # empty x-lab
)
dev.off()

png(filename = 'sex_differences_SDQ.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("SDQ60_mother_hyperactivity", "SDQ72_mother_hyperactivity", "SDQ60_father_hyperactivity", "SDQ72_father_hyperactivity")],
       y = list(NEW$gender_male), # we make this a list so it has length(1)
       ylab = c("SDQ60_mother_hyperactivity", "SDQ72_mother_hyperactivity", "SDQ60_father_hyperactivity", "SDQ72_father_hyperactivity"),
       xlab = "" # empty x-lab
)
dev.off()

png(filename = 'sex_differences_Dom_CBCL_PAPA.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = NEW[,c("Dominic72_ADHD", "CBCL48_sc6raw", "CBCL60_sc6raw", "PAPA_p4nadhd")],
       y = list(NEW$gender_male), # we make this a list so it has length(1)
       ylab = c("Dominic72_ADHD", "CBCL48_sc6raw", "CBCL60_sc6raw", "PAPA_p4nadhd"),
       xlab = "" # empty x-lab
)
dev.off()

sink('sex_differences_kk_adhd.txt')
kruskal.test(conners_mother_adhd_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_mother_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(conners_father_adhd_score.60m ~ gender_male, data=NEW)
kruskal.test(conners_father_adhd_score.72m ~ gender_male, data=NEW)
kruskal.test(SDQ60_mother_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ72_mother_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ60_father_hyperactivity ~ gender_male, data=NEW)
kruskal.test(SDQ72_father_hyperactivity ~ gender_male, data=NEW)
kruskal.test(Dominic72_ADHD ~ gender_male, data=NEW)
kruskal.test(CBCL48_sc6raw ~ gender_male, data=NEW)
kruskal.test(CBCL60_sc6raw ~ gender_male, data=NEW)
kruskal.test(PAPA_p4nadhd ~ gender_male, data=NEW)

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_mother_adhd_score.60m, na.rm = TRUE),
    sd = sd(conners_mother_adhd_score.60m, na.rm = TRUE),
    median = median(conners_mother_adhd_score.60m, na.rm = TRUE),
    IQR = IQR(conners_mother_adhd_score.60m, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_mother_adhd_score.72m, na.rm = TRUE),
    sd = sd(conners_mother_adhd_score.72m, na.rm = TRUE),
    median = median(conners_mother_adhd_score.72m, na.rm = TRUE),
    IQR = IQR(conners_mother_adhd_score.72m, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_father_adhd_score.60m, na.rm = TRUE),
    sd = sd(conners_father_adhd_score.60m, na.rm = TRUE),
    median = median(conners_father_adhd_score.60m, na.rm = TRUE),
    IQR = IQR(conners_father_adhd_score.60m, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_father_adhd_score.72m, na.rm = TRUE),
    sd = sd(conners_father_adhd_score.72m, na.rm = TRUE),
    median = median(conners_father_adhd_score.72m, na.rm = TRUE),
    IQR = IQR(conners_father_adhd_score.72m, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ60_mother_hyperactivity, na.rm = TRUE),
    sd = sd(SDQ60_mother_hyperactivity, na.rm = TRUE),
    median = median(SDQ60_mother_hyperactivity, na.rm = TRUE),
    IQR = IQR(SDQ60_mother_hyperactivity, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ72_mother_hyperactivity, na.rm = TRUE),
    sd = sd(SDQ72_mother_hyperactivity, na.rm = TRUE),
    median = median(SDQ72_mother_hyperactivity, na.rm = TRUE),
    IQR = IQR(SDQ72_mother_hyperactivity, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ60_father_hyperactivity, na.rm = TRUE),
    sd = sd(SDQ60_father_hyperactivity, na.rm = TRUE),
    median = median(SDQ60_father_hyperactivity, na.rm = TRUE),
    IQR = IQR(SDQ60_father_hyperactivity, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ72_father_hyperactivity, na.rm = TRUE),
    sd = sd(SDQ72_father_hyperactivity, na.rm = TRUE),
    median = median(SDQ72_father_hyperactivity, na.rm = TRUE),
    IQR = IQR(SDQ72_father_hyperactivity, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(Dominic72_ADHD, na.rm = TRUE),
    sd = sd(Dominic72_ADHD, na.rm = TRUE),
    median = median(Dominic72_ADHD, na.rm = TRUE),
    IQR = IQR(Dominic72_ADHD, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(CBCL48_sc6raw, na.rm = TRUE),
    sd = sd(CBCL48_sc6raw, na.rm = TRUE),
    median = median(CBCL48_sc6raw, na.rm = TRUE),
    IQR = IQR(CBCL48_sc6raw, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(CBCL60_sc6raw, na.rm = TRUE),
    sd = sd(CBCL60_sc6raw, na.rm = TRUE),
    median = median(CBCL60_sc6raw, na.rm = TRUE),
    IQR = IQR(CBCL60_sc6raw, na.rm = TRUE)
  )

group_by(NEW, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(PAPA_p4nadhd, na.rm = TRUE),
    sd = sd(PAPA_p4nadhd, na.rm = TRUE),
    median = median(PAPA_p4nadhd, na.rm = TRUE),
    IQR = IQR(PAPA_p4nadhd, na.rm = TRUE)
    )

png(filename = 'conners_60.png')
qplot(NEW$conners_mother_adhd_score.60m,NEW$conners_father_adhd_score.60m)
dev.off()

plot(NEW$conners_mother_adhd_score.60m, NEW$conners_father_adhd_score.60m)
cor.test(NEW$conners_mother_adhd_score.60m, NEW$conners_father_adhd_score.60m)
rcorr(NEW$conners_mother_adhd_score.60m, NEW$conners_father_adhd_score.60m, type="spearman") 

png(filename = 'conners60_by_sex.png')
ggplot(NEW, aes(conners_mother_adhd_score.60m,conners_father_adhd_score.60m)) +
  geom_jitter(
    aes(color = gender_male),
    position = position_jitter(0.2)
  )
dev.off()

png(filename = 'conners_72.png')
qplot(NEW$conners_mother_adhd_score.72m,NEW$conners_father_adhd_score.72m)
dev.off()

png(filename = 'conners72_by_sex.png')
ggplot(NEW, aes(conners_mother_adhd_score.72m,conners_father_adhd_score.72m)) +
  geom_jitter(
    aes(color = gender_male),
    position = position_jitter(0.2)
  )
dev.off()

plot(NEW$conners_mother_adhd_score.72m, NEW$conners_father_adhd_score.72m)
cor.test(NEW$conners_mother_adhd_score.72m, NEW$conners_father_adhd_score.72m)
rcorr(NEW$conners_mother_adhd_score.72m, NEW$conners_father_adhd_score.72m, type="spearman") 

png(filename = 'SDQ60.png')
qplot(NEW$SDQ60_mother_hyperactivity,NEW$SDQ60_father_hyperactivity)
dev.off()

png(filename = 'SDQ60_by_sex.png')
ggplot(NEW, aes(SDQ60_father_hyperactivity, SDQ60_mother_hyperactivity)) +
  geom_jitter(
    aes(color = gender_male),
    position = position_jitter(0.2)
  )
dev.off()

plot(NEW$SDQ60_mother_hyperactivity, NEW$SDQ60_father_hyperactivity)
cor.test(NEW$SDQ60_mother_hyperactivity, NEW$SDQ60_father_hyperactivity)
rcorr(NEW$SDQ60_mother_hyperactivity, NEW$SDQ60_father_hyperactivity, type="spearman") 

png(filename = 'SDQ72.png')
qplot(NEW$SDQ72_mother_hyperactivity,NEW$SDQ72_father_hyperactivity)
dev.off()

png(filename = 'SDQ72_by_sex.png')
ggplot(NEW, aes(SDQ72_father_hyperactivity, SDQ72_mother_hyperactivity)) +
  geom_jitter(
    aes(color = gender_male),
    position = position_jitter(0.2)
  )
dev.off()

plot(NEW$SDQ72_mother_hyperactivity, NEW$SDQ72_father_hyperactivity)
cor.test(NEW$SDQ72_mother_hyperactivity, NEW$SDQ72_father_hyperactivity)
rcorr(NEW$SDQ72_mother_hyperactivity, NEW$SDQ72_father_hyperactivity, type="spearman")

NEW$gender_male <- to_factor(NEW$gender_male)

# ggplot(data=NEW[!is.na(NEW$gender_male),], aes(x=as.factor(gender_male, y=SDQ72_mother_hyperactivity, fill=gender_male)) +
#   ggtitle("Boxplot of ") +
#   xlab(" ") +
#   ylab(" ADHD severity") +
#   scale_x_discrete(labels=c("0" = " ", "1" = " ")) +
#   geom_violin()+
#   geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.21))+
#   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
#   theme(legend.title = element_blank(),legend.position = "none")

# png(filename = 'test.png')
# ggplot(data=NEW[!is.na(NEW$gender_male),], aes(x=as.factor(SDQ72_mother_hyperactivity), y=SDQ72_father_hyperactivity, fill=gender_male)) +
# ggtitle("") +
#   xlab("") +
#   ylab("") +
#   #scale_x_discrete(labels=c("0" = "No University", "1" = "University")) +
#   # geom_violin()+
#   geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.21))+
#   #stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
#   theme(legend.title = element_blank(),legend.position = "none")
# dev.off()

lmm <- lmer(conners_mother_adhd_score.60m ~ Sconners_father_adhd_score.60m + (1 | gender_male), data = NEW,
            REML = FALSE)
summary(lmm) #estimate of the variance explained by the random effect. This number is important, because if it's indistinguishable from zero, then your random effect probably doesn't matter and you can go ahead and do a regular linear model instead. 
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of child sex on adhd score

lmm <- lmer(conners_mother_adhd_score.72m ~ Sconners_father_adhd_score.72m + (1 | gender_male), data = NEW,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of child sex on adhd score

lmm <- lmer(SDQ60_mother_hyperactivity ~ SDQ60_father_hyperactivity + (1 | gender_male), data = NEW,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of child sex on adhd score

lmm <- lmer(SDQ72_mother_hyperactivity ~ SDQ72_father_hyperactivity + (1 | gender_male), data = NEW,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of child sex on adhd score

fit <-lm(conners_mother_adhd_score.60m ~ conners_father_adhd_score.60m + gender_male, data=NEW)
summary(fit)
fit <-lm(conners_mother_adhd_score.72m ~ conners_father_adhd_score.72m + gender_male, data=NEW)
summary(fit)
fit <-lm(SDQ60_mother_hyperactivity ~ SDQ60_father_hyperactivity + gender_male, data=NEW)
summary(fit)
fit <-lm(SDQ72_mother_hyperactivity ~ SDQ72_father_hyperactivity + gender_male, data=NEW)
summary(fit)

table(NEW$conners_mother_adhd_score.60m, NEW$gender_male)  
addmargins(table(NEW$conners_mother_adhd_score.60m, NEW$gender_male))

table(NEW$conners_mother_adhd_score.72m, NEW$gender_male)  
addmargins(table(NEW$conners_mother_adhd_score72m, NEW$gender_male))

table(NEW$conners_father_adhd_score.72m, NEW$gender_male)  
addmargins(table(NEW$conners_father_adhd_score72m, NEW$gender_male))

table(NEW$SDQ60_mother_hyperactivity, NEW$gender_male)  
addmargins(table(NEW$SDQ60_mother_hyperactivity, NEW$gender_male))

table(NEW$SDQ72_mother_hyperactivity, NEW$gender_male)  
addmargins(table(NEW$SDQ72_mother_hyperactivity, NEW$gender_male))

table(NEW$SDQ60_father_hyperactivity, NEW$gender_male)  
addmargins(table(NEW$SDQ60_father_hyperactivity, NEW$gender_male))

table(NEW$SDQ72_father_hyperactivity, NEW$gender_male)  
addmargins(table(NEW$SDQ72_father_hyperactivity, NEW$gender_male))

table(NEW$Dominic72_ADHD, NEW$gender_male)  
addmargins(table(NEW$Dominic72_ADHD, NEW$gender_male))

table(NEW$CBCL48_sc6raw, NEW$gender_male)  
addmargins(table(NEW$CBCL48_sc6raw, NEW$gender_male))

table(NEW$CBCL60_sc6raw, NEW$gender_male)  
addmargins(table(NEW$CBCL60_sc6raw, NEW$gender_male))

table(NEW$PAPA_p4nadhd, NEW$gender_male)  
addmargins(table(NEW$PAPA_p4nadhd, NEW$gender_male))

sink()

##############################
##############################
##############################
##############################
##############################
##############################
####parent conners_parent_60	conners_parent_72	SDQ_parent_60	SDQ_parent_72

png(filename = 'differences_long_parent.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Mother", "Father"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = LONG[,c("conners_60", "conners_72", "SDQ60", "SDQ_72")],
       y = list(LONG$parent), # we make this a list so it has length(1)
       ylab = c("conners_60", "conners_72", "SDQ60", "SDQ_72"),
       xlab = "" # empty x-lab
)
dev.off()

png(filename = 'differences_long_sex.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Girls", "Boys"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = LONG[,c("conners_60", "conners_72", "SDQ60", "SDQ_72")],
       y = list(LONG$gender_male_fixed), # we make this a list so it has length(1)
       ylab = c("conners_60", "conners_72", "SDQ60", "SDQ_72"),
       xlab = "" # empty x-lab
)
dev.off()

##############################
####
####parent conners_parent_60	conners_parent_72	SDQ_parent_60	SDQ_parent_72

png(filename = 'differences_long_conners_60_parent.png')
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Mother", "Father"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = LONG[,c("conners_60")],
       y = list(LONG$conners_parent_60), # we make this a list so it has length(1)
       ylab = c("conners_60"),
       xlab = "" # empty x-lab
)
dev.off()

png(filename = 'differences_long_conners_72_parent.png')
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Mother", "Father"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = LONG[,c("conners_72")],
       y = list(LONG$conners_parent_72), # we make this a list so it has length(1)
       ylab = c("conners_72"),
       xlab = "" # empty x-lab
)
dev.off()

png(filename = 'differences_long_sdq_72_parent.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Mother", "Father"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = LONG[,c("SDQ60")],
       y = list(LONG$SDQ_parent_60), # we make this a list so it has length(1)
       ylab = c("SDQ60"),
       xlab = "" # empty x-lab
)
dev.off()

png(filename = 'differences_long_sdq_72_parent.png')
par(mfrow=c(1,4))
myBox <- function(x, y, ylab, xlab) boxplot(x ~ y, col=rainbow(2), names=c("Mother", "Father"),ylab=ylab, xlab=xlab)
mapply(myBox,
       x = LONG[,c("SDQ_72")],
       y = list(LONG$SDQ_parent_72), # we make this a list so it has length(1)
       ylab = c("SDQ_72"),
       xlab = "" # empty x-lab
)
dev.off()


sink('sex_parent_differences_kk_long.txt')
kruskal.test(conners_60 ~ gender_male, data=LONG)
kruskal.test(conners_72 ~ gender_male, data=LONG)
kruskal.test(SDQ60 ~ gender_male, data=LONG)
kruskal.test(SDQ_72 ~ gender_male, data=LONG)

kruskal.test(conners_60 ~ parent, data=LONG)
kruskal.test(conners_72 ~ parent, data=LONG)
kruskal.test(SDQ60 ~ parent, data=LONG)
kruskal.test(SDQ_72 ~ parent, data=LONG)

####parent conners_parent_60	conners_parent_72	SDQ_parent_60	SDQ_parent_72

kruskal.test(conners_60 ~ conners_parent_60, data=LONG)
kruskal.test(conners_72 ~ conners_parent_72, data=LONG)
kruskal.test(SDQ60 ~ SDQ_parent_60, data=LONG)
kruskal.test(SDQ_72 ~ SDQ_parent_72, data=LONG)

chisq.test(LONG$parent, LONG$gender_male_fixed, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

chisq.test(LONG$conners_parent_60, LONG$gender_male_fixed, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

chisq.test(LONG$conners_parent_72, LONG$gender_male_fixed, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

chisq.test(LONG$SDQ_parent_60, LONG$gender_male_fixed, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

chisq.test(LONG$SDQ_parent_72, LONG$gender_male_fixed, correct=FALSE) # if p value above 0.05 variables are independent if below they are dependant

group_by(LONG, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_60, na.rm = TRUE),
    sd = sd(conners_60, na.rm = TRUE),
    median = median(conners_60, na.rm = TRUE),
    IQR = IQR(conners_60, na.rm = TRUE)
  )

group_by(LONG, parent) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_60, na.rm = TRUE),
    sd = sd(conners_60, na.rm = TRUE),
    median = median(conners_60, na.rm = TRUE),
    IQR = IQR(conners_60, na.rm = TRUE)
  )

group_by(LONG, conners_parent_60) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_60, na.rm = TRUE),
    sd = sd(conners_60, na.rm = TRUE),
    median = median(conners_60, na.rm = TRUE),
    IQR = IQR(conners_60, na.rm = TRUE)
  )

group_by(LONG, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_72, na.rm = TRUE),
    sd = sd(conners_72, na.rm = TRUE),
    median = median(conners_72, na.rm = TRUE),
    IQR = IQR(conners_72, na.rm = TRUE)
  )

group_by(LONG, parent) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_72, na.rm = TRUE),
    sd = sd(conners_72, na.rm = TRUE),
    median = median(conners_72, na.rm = TRUE),
    IQR = IQR(conners_72, na.rm = TRUE)
  )

group_by(LONG, conners_parent_72) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(conners_72, na.rm = TRUE),
    sd = sd(conners_72, na.rm = TRUE),
    median = median(conners_72, na.rm = TRUE),
    IQR = IQR(conners_72, na.rm = TRUE)
  )

group_by(LONG, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ60, na.rm = TRUE),
    sd = sd(SDQ60, na.rm = TRUE),
    median = median(SDQ60, na.rm = TRUE),
    IQR = IQR(SDQ60, na.rm = TRUE)
  )

group_by(LONG, parent) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ60, na.rm = TRUE),
    sd = sd(SDQ60, na.rm = TRUE),
    median = median(SDQ60, na.rm = TRUE),
    IQR = IQR(SDQ60, na.rm = TRUE)
  )

group_by(LONG, SDQ_parent_60) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ60, na.rm = TRUE),
    sd = sd(SDQ60, na.rm = TRUE),
    median = median(SDQ60, na.rm = TRUE),
    IQR = IQR(SDQ60, na.rm = TRUE)
  )

group_by(LONG, gender_male) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ_72, na.rm = TRUE),
    sd = sd(SDQ_72, na.rm = TRUE),
    median = median(SDQ_72, na.rm = TRUE),
    IQR = IQR(SDQ_72, na.rm = TRUE)
  )

group_by(LONG, parent) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ_72, na.rm = TRUE),
    sd = sd(SDQ_72, na.rm = TRUE),
    median = median(SDQ_72, na.rm = TRUE),
    IQR = IQR(SDQ_72, na.rm = TRUE)
  )

group_by(LONG, SDQ_parent_72) %>%
  dplyr::summarise(
    count = sum(n()),
    mean = mean(SDQ_72, na.rm = TRUE),
    sd = sd(SDQ_72, na.rm = TRUE),
    median = median(SDQ_72, na.rm = TRUE),
    IQR = IQR(SDQ_72, na.rm = TRUE)
  )

lmm <- lmer(conners_60 ~ gender_male_fixed + parent + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(conners_72 ~ gender_male_fixed + parent + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(SDQ60 ~ gender_male_fixed + parent + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(SDQ_72 ~ gender_male_fixed + parent + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(conners_60 ~ gender_male_fixed + conners_parent_60 + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(conners_72 ~ gender_male_fixed + conners_parent_72 + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(SDQ60 ~ gender_male_fixed + SDQ_parent_60 + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

lmm <- lmer(SDQ_72 ~ gender_male_fixed + SDQ_parent_72 + (1 | PSCID), data = LONG,
            REML = FALSE)
summary(lmm)
Anova(lmm) #Anova function does a Wald test how confident we are of our estimate of the effect of parent or child sex on adhd score

table(LONG$conners_60, LONG$gender_male_fixed)  
addmargins(table(LONG$conners_60, LONG$gender_male_fixed))

table(LONG$conners_72, LONG$gender_male_fixed)  
addmargins(table(LONG$conners_72, LONG$gender_male_fixed))

table(LONG$SDQ60, LONG$gender_male_fixed)  
addmargins(table(LONG$SDQ60, LONG$gender_male_fixed))

table(LONG$SDQ_72, LONG$gender_male_fixed)  
addmargins(table(LONG$SDQ_72, LONG$gender_male_fixed))

table(LONG$conners_60, LONG$parent)  
addmargins(table(LONG$conners_60, LONG$parent))

table(LONG$conners_72, LONG$parent)  
addmargins(table(LONG$conners_72, LONG$parent))

table(LONG$SDQ60, LONG$parent)  
addmargins(table(LONG$SDQ60, LONG$parent))

table(LONG$SDQ_72, LONG$parent)  
addmargins(table(LONG$SDQ_72, LONG$parent))

table(LONG$conners_60, LONG$conners_parent_60)  
addmargins(table(LONG$conners_60, LONG$conners_parent_60))

table(LONG$conners_72, LONG$conners_parent_72)  
addmargins(table(LONG$conners_72, LONG$conners_parent_72))

table(LONG$SDQ60, LONG$SDQ_parent_60)  
addmargins(table(LONG$SDQ60, LONG$SDQ_parent_60))

table(LONG$SDQ_72, LONG$SDQ_parent_72)  
addmargins(table(LONG$SDQ_72, LONG$SDQ_parent_72))

cor.test(LONG$conners_60, LONG$gender_male_fixed, method="kendall")
cor.test(LONG$conners_72, LONG$gender_male_fixed, method="kendall")
cor.test(LONG$SDQ60, LONG$gender_male_fixed, method="kendall")
cor.test(LONG$SDQ_72, LONG$gender_male_fixed, method="kendall")

cor.test(LONG$conners_60, LONG$parent, method="kendall")
cor.test(LONG$conners_72, LONG$parent, method="kendall")
cor.test(LONG$SDQ60, LONG$parent, method="kendall")
cor.test(LONG$SDQ_72, LONG$parent, method="kendall")

cor.test(LONG$conners_60, LONG$conners_parent_60, method="kendall")
cor.test(LONG$conners_72, LONG$conners_parent_72, method="kendall")
cor.test(LONG$SDQ60, LONG$SDQ_parent_60, method="kendall")
cor.test(LONG$SDQ_72, LONG$SDQ_parent_72, method="kendall")

cor.test(LONG$parent, LONG$gender_male_fixed, method="kendall")

##parent conners_parent_60	conners_parent_72	SDQ_parent_60	SDQ_parent_72

# Two Way Factorial Design
fit <- aov(conners_60 ~ conners_parent_60*gender_male_fixed, data=LONG) # same thing
#plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
#TukeyHSD(fit) # where fit comes from aov()
drop1(fit,~.,test="F") # type III SS and F Tests
summary.aov(fit)


fit <- aov(conners_72 ~ conners_parent_72*gender_male_fixed, data=LONG) # same thing
#plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
#TukeyHSD(fit) # where fit comes from aov()
drop1(fit,~.,test="F") # type III SS and F Tests
summary.aov(fit)


fit <- aov(SDQ60 ~ SDQ_parent_60*gender_male_fixed, data=LONG) # same thing
#plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
#TukeyHSD(fit) # where fit comes from aov()
drop1(fit,~.,test="F") # type III SS and F Tests
summary.aov(fit) #univariate stas


fit <- aov(SDQ_72 ~ SDQ_parent_72*gender_male_fixed, data=LONG) # same thing
#plot(fit) # diagnostic plots
summary(fit) # display Type I ANOVA table
#TukeyHSD(fit) # where fit comes from aov()
drop1(fit,~.,test="F") # type III SS and F Tests
summary.aov(fit)

png(filename = 'SDQ60_parent.png')
qplot(LONG$SDQ60,LONG$parent)
dev.off()

data(LONG)
LONG = apply_labels(LONG,
                      parent = "sex of parent",
                      parent = c("Mother" = 0,
                                 "Father"=1),
                      gender_male_fixed = "sex of child",
                                c("Girls" = 0,
                                  "Boys"=1),
                      conners_60 = "Conners 60M",
                      conners_72 = "Conners 72M",
                      SDQ60 = "SDQ60",
                      SDQ_72 = "SDQ72"
)

LONG %>%
  tab_cells(LONG$conners_60, LONG$conners_72, LONG$SDQ60, LONG$SDQ_72) %>%
  tab_cols(total(), LONG$parent, LONG$gender_male_fixed) %>% 
  tab_stat_mean_sd_n() %>%
  tab_last_sig_means(subtable_marks = "both") %>% 
  tab_pivot() %>% 
  set_caption("Table with summary statistics and significance marks.")  

sink()

png(filename = 'long_conners_60_by_sex_of_child_and_parent.png')
ggplot(LONG, aes(conners_72, gender_male_fixed)) +
  geom_jitter(
    aes(color = parent),
    position = position_jitter(0.2)
  ) 
dev.off()

png(filename = 'long_conners_72_by_sex_of_child_and_parent.png')
ggplot(LONG, aes(conners_72, gender_male_fixed)) +
  geom_jitter(
    aes(color = parent),
    position = position_jitter(0.2)
  ) 
dev.off()

png(filename = 'long_SDQ60_by_sex_of_child_and_parent.png')
ggplot(LONG, aes(SDQ60, gender_male_fixed)) +
  geom_jitter(
    aes(color = parent),
    position = position_jitter(0.2)
  ) 
dev.off()

png(filename = 'long_SDQ72_by_sex_of_child_and_parent.png')
ggplot(LONG, aes(SDQ_72, gender_male_fixed)) +
  geom_jitter(
    aes(color = parent),
    position = position_jitter(0.2)
  ) 
dev.off()

##parent conners_parent_60	conners_parent_72	SDQ_parent_60	SDQ_parent_72

png(filename = 'long_conners_60_by_sex_of_child_and_parent_proper.png')
ggplot(LONG, aes(conners_72, gender_male_fixed)) +
  geom_jitter(
    aes(color = conners_parent_60),
    position = position_jitter(0.2)
  ) 
dev.off()

png(filename = 'long_conners_72_by_sex_of_child_and_parent_proper.png')
ggplot(LONG, aes(conners_72, gender_male_fixed)) +
  geom_jitter(
    aes(color = conners_parent_72),
    position = position_jitter(0.2)
  ) 
dev.off()

png(filename = 'long_SDQ60_by_sex_of_child_and_parent_proper.png')
ggplot(LONG, aes(SDQ60, gender_male_fixed)) +
  geom_jitter(
    aes(color = SDQ_parent_60),
    position = position_jitter(0.2)
  ) 
dev.off()

png(filename = 'long_SDQ72_by_sex_of_child_and_parent_proper.png')
ggplot(LONG, aes(SDQ_72, gender_male_fixed)) +
  geom_jitter(
    aes(color = SDQ_parent_72),
    position = position_jitter(0.2)
  ) 
dev.off()

# png(filename = 'test_long.png')
# ggplot(data=LONG[!is.na(NEW$gender_male_fixed),], aes(x=as.factor(parent), y=SDQ_72, fill=gender_male_fixed)) +
#   ggtitle("") +
#   xlab("") +
#   ylab("") +
#   #scale_x_discrete(labels=c("0" = "No University", "1" = "University")) +
#   geom_violin()+
#   geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.21))+
#   stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
#   theme(legend.title = element_blank(),legend.position = "none")
# dev.off()




