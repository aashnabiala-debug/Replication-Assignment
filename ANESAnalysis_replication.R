##############################
#Loading Packages
#############################

library(lda)
library(slam)
library(stm)


#############################
#Cleaning Data
#############################

##setwd("../Data")

##change to reading to Ldac, previous labelling was wrong
dataldac <- readLdac("final_anes.csv")
vocabcoarse <- read.csv("final_anes_vocab.csv")$term
meta <- read.csv("final_anes_metadata.csv")

names(meta) <- gsub("\\.", "_", names(meta))

data <- prepDocuments(dataldac, vocabcoarse, meta=meta)
documents <- data$documents
vocab <- data$vocab
meta <- data$meta

##Function for fixing duplicates
fix_doc <- function(doc) {
  if (ncol(doc) == 0) return(doc)
  
  ids <- doc[1, ]
  counts <- doc[2, ]
  
  # Aggregate duplicate word IDs
  agg <- tapply(counts, ids, sum)
  
  # Return in STM format
  rbind(as.integer(names(agg)), as.integer(agg))
}

documents <- lapply(documents, fix_doc)


#####################################
#Model with fear treatment and PID interaction
#####################################

#Select Model
set.seed(01238)
models <- selectModel(documents, vocab, K = 60, prevalence = ~pid_summary + age + highest_grade_completed + highest_grade_completed* pid_summary, data = meta, runs = 50)
plotModels(models)

mod <- models$runout[[10]]
plot.STM(mod, n=1, numtopics=5, labeltype="frex")

##Creating neater plots and saving them
dev.new(width = 18, height = 12)
plot.STM(mod, n = 1, numtopics = 5, labeltype = "frex")

png("stm_plot.png", width = 2500, height = 2000, res = 200)
plot.STM(mod, n = 1, numtopics = 5, labeltype = "frex")
dev.off()

mod$settings$seed

prep21 <- estimateEffect(1:36 ~ highest_grade_completed,
                         mod,
                         meta = meta,
                         uncertainty = "Global")


keep <- meta$pid_summary < 3



prep21 <- estimateEffect(1:36 ~ highest_grade_completed,
                         mod,
                         meta = meta[keep, ],
                         uncertainty = "Global")

plot(
  prep21,
  covariate = "highest_grade_completed",
  topics=36,
  model=mod,
  method = "continuous",
  ylab="Probability of Topic", 
  xlab="Years of Education", 
  labeltype="prob", 
  printlegend=F, 
  ylim=c(0,.1), 
  xlim=c(13,17), 
  linecol="blue", 
  main="STM War Topic and Education")


text(16, .09, "Democrat", col="blue", cex=1.2)
text(14.5, .03, "Republican", col="red", cex=1.2)


#ANES Coding comparison
anes <- 3
meta$warall <- ifelse((meta$mippol1_code1==anes | meta$mippol1_code2==anes | meta$mippol1_code3==anes | meta$mippol1_code4==anes | meta$mippol1_code5==anes |
                         meta$mippol1_code6==anes | meta$mippol1_code7==anes | meta$mippol1_code8==anes),1,NA)
meta$warall[is.na(meta$warall)] <- 0
main <- "ANES War Topic and Education"
sub <- meta[meta$pid_summary<3,]
fit <- loess(sub$warall ~ sub$highest_grade_completed, span=2)
newdat <- data.frame(x=seq(min(sub$highest_grade_completed), max(sub$highest_grade_completed)))
pred <- predict(fit, newdata=newdat$x, se=T)
plot(pred$fit ~ newdat$x, type="l", ylim=c(0,.1), col="blue", xlim=c(13,17), xlab="Years of Education", ylab="Probability of Topic", main=main)
lines((pred$fit + 1.64*pred$se)~ newdat$x, col="blue", lty=2)
lines((pred$fit - 1.64*pred$se)~ newdat$x, col="blue", lty=2)

sub <- meta[meta$pid_summary>3,]
fit <- loess(sub$warall ~ sub$highest_grade_completed, span=2)
newdat <- data.frame(x=seq(min(sub$highest_grade_completed), max(sub$highest_grade_completed)))
pred <- predict(fit, newdata=newdat$x, se=T)
lines(pred$fit ~ newdat$x, type="l", ylim=c(min(pred$fit-1.64*pred$se, na.rm=T), max(pred$fit +1.64*pred$se, na.rm=T)), col="red")
lines((pred$fit + 1.64*pred$se)~ newdat$x, col="red", lty=2)
lines((pred$fit - 1.64*pred$se)~ newdat$x, col="red", lty=2)
text(14, .02, "Republican", col="red", cex=1.2)
text(14.5,.08, "Democrat", col="blue", cex=1.2)


#Comparison with hand coding
topmat <- matrix(nrow=nrow(meta), ncol=ncol(mod$theta))
for(i in 1:nrow(meta)){
  topmat[i,] <- mod$theta[i,]>.2
}
sumtopics <- apply(topmat,2,sum)
toptopics <- matrix(nrow=4, ncol=3)
toptopics[1,] <- c(sumtopics[56], "The Economy", sum(meta$mippol1_code1==50 |meta$mippol1_code1==51 | meta$mippol1_code1==52 | meta$mippol1_code1==53 | meta$mippol1_code1==54 | meta$mippol1_code1==55))
toptopics[2,] <- c(sumtopics[53] + sumtopics[29], "War, or Iraq War", sum(meta$mippol1_code1==4 | meta$mippol1_code1==3))
toptopics[3,] <- c(sumtopics[47] + sumtopics[30], "Don't Know",sum(meta$mippol1_code1==95))
toptopics[4,] <- c(sumtopics[40] + sumtopics[19] + sumtopics[20] + sumtopics[36], "Employment",sum(meta$mippol1_code1==27))

rownames(toptopics) <- c("Economy", "War or Iraq War", "Don't Know", "Unemployment and Job")
colnames(toptopics) <- c("STM", "", "Hand-Coding")

library(xtable)
xtable(toptopics)


#Individual responses hand coding
meta[,29:36][is.na(meta[,29:36])] <- 0
ok <- (mod$theta[,58]>.2)  & meta$mippol1_code1!=5 & meta$mippol1_code2!=5 & meta$mippol1_code3!=5 & meta$mippol1_code4!=5 & meta$mippol1_code5!=5 & meta$mippol1_code6!=5 & meta$mippol1_code7!=5 & meta$mippol1_code8!=5 #&


meta$mip_1[ok]
meta$mippol1_code1[ok]

ok <- ((mod$theta[,58]<.2) & (meta$mippol1_code1==5 | meta$mippol1_code2==5 | meta$mippol1_code3==5 | meta$mippol1_code4==5 | meta$mippol1_code5==5 | meta$mippol1_code6==5 | meta$mippol1_code7==5 | meta$mippol1_code8==5))

meta$mip_1[ok]
meta$mippol1_code1[ok]

#Number of categories for each response
topmat <- matrix(nrow=nrow(meta), ncol=ncol(mod$theta))
for(i in 1:nrow(meta)){
  topmat[i,] <- mod$theta[i,]>.2
}
catnum <- apply(topmat,1,sum)
anescatnum <- apply(meta[,29:36],1,function (x) sum(x!=0))
table(anescatnum, catnum)
table(anescatnum, catnum)/apply(table(anescatnum,catnum),1,sum)
t(table(anescatnum, catnum))/(apply(table(anescatnum,catnum),2,sum))
