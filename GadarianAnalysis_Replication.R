##############################
#Loading Packages
#############################

library(lda)
library(slam)
library(stm)


#############################
#Cleaning Data
#############################

#setwd("../Data")

dataldac <- readLdac("gadarian_final_tdm.csv")
vocabcoarse <- read.csv("gadarian_vocab.csv")$term
meta <- read.csv("gadarian_metadata.csv")
meta$treatment <- as.numeric(meta$treat=="1. worried")

data <- prepDocuments(dataldac, vocabcoarse, meta=meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

#####################################
#Model with fear treatment and PID interaction
#####################################

mod <- stm(documents, vocab, 3, prevalence=~meta$treatment, seed=6170546)

labelTopics(mod)

#Treatment effect plot
prep <- estimateEffect(1:2 ~ treatment,
                       mod,
                       metadata = meta,
                       uncertainty = "Global")

plot(prep,
     choosetopics=1:2,
     covariate = "treatment", 
     model=mod, 
     xlab="Difference in Topic Proportions (Treated-Control)", 
     labeltype="numbers", 
     width = 40,
     xlim=c(-.4,.3))


#Interaction plot
prep21 <- estimateEffect(1 ~ pid_rep,
                       mod,
                       metadata = meta,
                       uncertainty = "Global")

plot(prep21, 
     topics=1,
     covariate = "pid_rep",
     method = "continuous", 
     subset = (meta$treatment == 1),
     model=mod, 
     ylab="Mean Topic Proportions", 
     xlab="Party ID", 
     labeltype="prob",
     ci.level = 0.9,
     npoints = 100,
     xlim=c(0,1), 
     printlegend=F, 
     ylim=c(0,.7), 
     xaxt="n")

prep20 <- estimateEffect(1 ~ pid_rep,
                         mod,
                         metadata = meta,
                         uncertainty = "Global")
plot(prep20, 
     choosetopics=1,
     covariate = "pid_rep",
     method = "continuous",
     model=mod, 
     ylab="Mean Topic Proportions", 
     xlab="Party ID", 
     labeltype="prob",
     subset= (meta$treatment==0), 
     ci.level=0.9, 
     xlim=c(0,1), 
     add=T, 
     printlegend=F, 
     linecol="blue")

axis(1, at=c(0,.5,1), c("Strong \n Democrat", "Moderate", "Strong \n Republican"), padj=.5)

text(.35, .55, "Treated", col="red")
text(.75, .15, "Control", col="blue")

#Find thoughts
thoughts <- findThoughts(mod, texts=meta$open.ended.response, topics=1:3, n=10)

meta$fear_ra1[thoughts$index[[1]]]
meta$fear_ra2[thoughts$index[[1]]]
plotQuote(meta$open.ended.response[thoughts$index[[1]][1]], width=7)
plotQuote(meta$open.ended.response[thoughts$index[[4]][1]], width=5)

meta$fear_ra1[thoughts$index[[2]]]
meta$fear_ra2[thoughts$index[[2]]]
plotQuote(meta$open.ended.response[thoughts$index[[3]][2]], width=6)
plotQuote(meta$open.ended.response[thoughts$index[[1]][2]], width=5)

#Estimating treatment effect
topic1effect <- t.test(mod$theta[,1][meta$treatment==1], mod$theta[,1][meta$treatment==0], conf.level=.9)
topic1effect$estimate[1] - topic1effect$estimate[2]
topic1effect$conf.int

#Estimating PID effect
topic1effect <- t.test(mod$theta[,1][meta$pid_rep>.5], mod$theta[,1][meta$pid_rep<.5], conf.level=.9)
topic1effect$estimate[1] - topic1effect$estimate[2]
topic1effect$conf.int


#Estimating Interaction effect
topic1effect <- t.test(mod$theta[,1][meta$pid_rep>.5 & meta$treatment==1], mod$theta[,1][meta$pid_rep<.5 & meta$treatment==0], conf.level=.9)
topic1effect$estimate[1] - topic1effect$estimate[2]
topic1effect$conf.int

#Average amont of time on each topic
apply(mod$theta[meta$pid_rep>.5 & meta$treatment==1,],2,mean)
apply(mod$theta[meta$pid_rep<.5 & meta$treatment==0,],2,mean)

