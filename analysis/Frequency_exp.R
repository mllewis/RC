# refcomplex frequency experiments

rm(list=ls())

#--LOAD PACKAGES--
library(boot)
library(ggplot2)
library(plyr)

# proportions and CIs for forced choice tasks
p.fc <- function(d, dv){ 
 # category = "\"low_freq\"" 
   category = dv
  d$response = d$responseValue
  
  # get proportions
  complex_proport_c = sum(d$response==category)/length(d$response)
  
  if (!is.na(complex_proport_c)) { 
    # Bootstrap across subjects proportion responses for each category
    b <- boot(d$response, function(u,i) table(u[i])[category]/length(u), R = 1000) 
    ci <- boot.ci(b, type =  "basic")  
    ciwl = ci$basic[4]
    ciwu = ci$basic[5]
    
    es <- data.frame(p_complex = complex_proport_c,
                     ciwl = ciwl,
                     ciul = ciwu,
                     n=length(d$workerid))
    
  } else {
    es <- data.frame(p_c = NA,
                     ciwl = NA,
                     ciul = NA,
                     n=NA)
    
  }
  return (es)
}


getDuplicateSubj <- function (df, workers_columnname, time_columnname){
  wi = which(names(df) == as.character(workers_columnname))
  ai = which(names(df) == as.character(time_columnname))
  df[,ai] = as.POSIXct(df[,ai], format="%a %b %d %H:%M:%S") #change time format
  duplicate_rows <- df[,wi] %in% unique(df[duplicated(df[,wi]), wi])
  
  for (obs in 1:dim(df)[1]){
    s_duplicate_rows = which((duplicate_rows == TRUE) & (df[,wi] == df[obs,wi])) # get indices of duplicates for particular observations
    
    if (length(s_duplicate_rows)==0) {  #not a duplicate
      df$duplicateS[obs] <- "no" 
    } else if (df[obs, ai] == min(df[s_duplicate_rows, ai])) { #duplicate but same same hit (dif trial) or first hit
      df$duplicateS[obs] <- "no"
    } else { #duplicate and different hit
      df$duplicateS[obs] <- "yes" 
    }
  } 
  df$duplicateS = as.factor(df$duplicateS)
  return(df)
}

#--READ IN DATA--
setwd("/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_6/") # set working directory and read in data 
d1 <- read.csv("RefComplex6.results_1",sep="\t",header=TRUE)
d2 <- read.csv("RefComplex6.results_2",sep="\t",header=TRUE)
d <- rbind(d1, d2)

RC6 = subset(d, Answer.frequency_condition ==  '\"uneven\"')
RC6[RC6$Answer.lang_condition == '\"long\"' & RC6$Answer.crit == '\"correct\"',"Answer.crit_response"] = "\"low_freq\"" 
RC6[RC6$Answer.lang_condition == '\"long\"' & RC6$Answer.crit == '\"incorrect\"', "Answer.crit_response"] = "\"high_freq\""
RC6[RC6$Answer.lang_condition == '\"short\"' & RC6$Answer.crit == '\"correct\"', "Answer.crit_response"] = "\"high_freq\""
RC6[RC6$Answer.lang_condition == '\"short\"'& RC6$Answer.crit == '\"incorrect\"', "Answer.crit_response"] = "\"low_freq\"" 

RC6$Answer.frequency_condition <- NULL
RC6$Answer.crit<- NULL
RC6$Answer.crit_targ <- NULL
RC6$Answer.crit_dist <- NULL
RC6$Answer.check_dist <- NULL
RC6$Answer.name_check_correct <- NULL
RC6$Answer.check_targ <- NULL
RC6$Answer.check <- NULL
RC6$sample = 6

RC17 <- read.csv("/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_17/RefComplex17.results",sep="\t",header=TRUE)
RC17NR = RC17[RC17$Answer.cond == "\"RC6_replication\"", ]
RC17NR$sample = 17
RC172 <- read.csv("/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_17_2/RefComplex17_2.results",sep="\t",header=TRUE)
RC172$sample = 172
RC182 <- read.csv("/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_18/RefComplex18_2.results",sep="\t",header=TRUE)
RC182 = RC182[RC182$Answer.freq == 0.1,]
RC182$sample = 182
RC182$Answer.freq <- NULL
RC_rep = rbind(RC17NR,RC172, RC182)

RC_rep$Answer.obj_high <- NULL
RC_rep$Answer.obj_low <- NULL
RC_rep$Answer.dist_check_correct <- NULL

D = rbind(RC6, RC_rep)
D$sample = as.factor(D$sample)
D$Answer.crit_response = as.factor(D$Answer.crit_response)
names(D)[35] = "responseValue"

# RC 6,17,182 -simultaneous presentation
E1 = ddply(D ,.(Answer.lang_condition), function (d,dv) {p.fc(d, "\"low_freq\"")}, .inform = TRUE)
E1$exp = "simultaneous presentation"

# RC17 -simultaneous presentation, no cover AND simultaneous presentation, real ojects
RC17NC = RC17[RC17$Answer.cond == "\"RC6_noCover\"", ]
names(RC17NC)[32] = "responseValue"

E2 = ddply(RC17NC,.(Answer.lang_condition), function (d) {p.fc(d)}, .inform = TRUE)
E2$exp = "simultaneous presentation, no cover"

RC17RS = RC17[RC17$Answer.cond == "\"RC6_realStim\"", ]
names(RC17RS)[32] = "responseValue"

E3 = ddply(RC17RS,.(Answer.lang_condition), function (d) {p.fc(d)}, .inform = TRUE)
E3$exp = "simultaneous presentation, real ojects"

# RC 18 - simultaneous presentation, same speaker
RC18 <- read.csv("/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_18/RefComplex18.results",sep="\t",header=TRUE)
RC18SS = RC18[RC18$Answer.cond == "\"RC6_sameSpeaker\"",]
names(RC18SS)[38] = "responseValue"
E4 = ddply(RC18SS,.(Answer.lang_condition), function (d) {p.fc(d)}, .inform = TRUE)
E4$exp = "simultaneous presentation, same speaker"

# RC 16 - sequential presentation
RC16 <- read.csv("/Documents/GRADUATE_SCHOOL/Projects/ref_complex/Experiment_16/RefComplex16.results",sep="\t",header=TRUE)
RC16 <- RC16[RC16$Answer.frequency_condition == '\"uneven\"', ]
levels(RC16$Answer.crit_selection) = c("\"high_freq\"", "\"low_freq\"")
names(RC16)[35] = "responseValue"
E5 = ddply(RC16,.(Answer.lang_condition), function (d) {p.fc(d)}, .inform = TRUE)
E5$exp = "sequential presentation"

E = rbind(E1, E2, E3, E4, E5)

total_n = ddply(E,.(exp), function (d) {return (sum(d$n))}, .inform = TRUE)
index <- match(E$exp, total_n$exp)
E$total_n<- total_n$V1[index]

#plot
qplot(Answer.lang_condition,p_complex, fill = Answer.lang_condition,  ylim=c(0,1), position=position_dodge(),
      data=E,geom="bar",ylab="Proportion selection low frequency object", xlab="Language condition", stat="identity")  +
  geom_linerange(aes(ymin=ciwl,ymax=ciul), position=position_dodge(.9)) +
  #annotate("text", x=2, y=1, label=paste("n=",E$total_n[1])) +
  facet_wrap(~exp) +
  theme_bw() +
  theme(axis.title = element_text( face = "bold")) +
  theme(axis.text.x = element_text(colour = 'black')) +
  theme(axis.text.y = element_text( colour = 'black')) +
  theme(legend.position="none") +
  geom_abline(intercept = .5, slope = 0, linetype = 2)  #


#### For paper, only include simultaneous (collaposed across all 4 experiments) and sequential 

# save simultaneious without duplicate workers
D = getDuplicateSubj(D, "workerid", "assignmentaccepttime")
write.csv(D[D$duplicateS == "no",], "RC_sequential_frequency_all.csv")






