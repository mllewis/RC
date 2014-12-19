# Plot data for RCI_02_G2


### Iteration 1 (N = 50)
### Read in data and preprocess

```r
## --read in data--
setwd("/Documents/GRADUATE_SCHOOL/Projects/iterated_RC/RCI_02/RCI_02_G1/") 
d <- read.csv("RCI_02.results",sep="\t",header=TRUE)

## -- pre-process --
n <- names(d)
colsN = c(n[grepl("guessedLabelLen", n)], n[grepl("RT", n)])
colsF = c(n[grepl(".label", n)],  n[grepl(".index", n)], 
          n[grepl(".guessedLabel", n)],  n[grepl(".quintile", n)])
colsF = colsF[!grepl("labelLen", colsF)]
colsF = colsF[!grepl("guessedLabelLen", colsF)]

mdN <- melt(d, id.vars = c("workerid"), measure.vars = colsN)
mdF <- melt(d, id.vars = c("workerid"), measure.vars = colsF)
md = rbind(mdN, mdF)

md$obj <- matrix(lapply(str_split(md$variable,"\\."),function(x) {x[2]}))
md$obj <- as.numeric(matrix(lapply(str_split(as.character(md$obj),"_"),function(x) {x[2]})))
md$variable <-  matrix(lapply(str_split(md$variable,"\\."),function(x) {x[3]}))
md$value = as.factor(md$value)
md$variable = as.factor(unlist(md$variable))

md$seq <- with(md, ave(value, workerid, variable, FUN = seq_along))
da = dcast(workerid + seq  ~ variable, data = md, value.var = "value")
da$seq <- NULL

# make all variables a factor
da$RT0 = as.numeric(da$RT0)
da$RT1 = as.numeric(da$RT1)
da$RT2 = as.numeric(da$RT2)
da$RT3 = as.numeric(da$RT3)
da$quintile = as.numeric(gsub("[[:punct:]]", "", da$quintile))
da$guessedLabelLen = as.numeric(da$guessedLabelLen )
da$guessedLabel = as.factor(da$guessedLabel)
da$label = as.factor(da$label)
da$index = as.factor(da$index)
da$workerid = as.factor(da$workerid)
da$labelLen = nchar(as.character(da$label)) - 2

da$index = as.factor(as.numeric(gsub("[[:punct:]]", "", da$index)))

# get rid of multiword utterances for analyses
da$guessed_label_Nwords = as.factor(sapply(gregexpr("\\W+", da$guessedLabel), length) - 1)
da = da[da$guessed_label_Nwords == 1,]
```

(1) Accuracy as a function of word length

```r
da$accuracy = as.factor(ifelse(as.character(da$guessedLabel) == as.character(da$label), "correct", "incorrect"))
da$labelLenF = as.factor(da$labelLen)
da_cc <- ddply(da, .(labelLenF), function (d) {p.fc(d, "correct")}, .inform= TRUE)

qplot(labelLenF, p_correct, fill = "blue", ylim=c(0,1), position=position_dodge(),
      data=da_cc,geom="bar",ylab="proportion correct", xlab="label length (characters)", stat="identity")  +
  geom_linerange(aes(ymin=ciwl,ymax=ciul), position=position_dodge(.9)) +
  #annotate("text", x=2, y=1, label=paste("n=",E$total_n[1])) +
  theme_bw() +
  theme(axis.title = element_text( face = "bold")) +
  theme(axis.text.x = element_text(colour = 'black')) +
  theme(axis.text.y = element_text( colour = 'black')) +
  theme(legend.position="none") 
```

```
## Warning: Removed 1 rows containing missing values (geom_segment).
## Warning: Removed 1 rows containing missing values (geom_segment).
```

<img src="RCI_02_analysis_files/figure-html/unnamed-chunk-3.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" width="672" />

(2) Edits as a function of word length. 

```r
da$removedCharacters = da$labelLen - da$guessedLabelLen # difference betwen actual and real
ggplot(da, aes(factor(removedCharacters), factor(labelLen))) + 
  stat_sum(aes(group = 1)) +
  xlab("Actual label length - Guessed label length (chars)") +
  ylab("Actual label length") + 
  annotate("text",x=3, y=.5,label= "guessing too long name", color = "red") +
  annotate("text",x=13, y=.5,label= "guessing too short name", color = "red") 
```

<img src="RCI_02_analysis_files/figure-html/unnamed-chunk-41.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="672" />

```r
lmeans = aggregate(guessedLabelLen ~ labelLen, data=da, mean)
lmeans$cih <- aggregate(guessedLabelLen ~ labelLen, data=da, ci.high)$guessedLabelLen
lmeans$cil <- aggregate(guessedLabelLen ~ labelLen, data=da, ci.low)$guessedLabelLen
ggplot(lmeans, aes(x = labelLen, y = guessedLabelLen, )) + 
  geom_line(position="dodge") +
  geom_point(size = 4) +
  geom_pointrange(aes(ymax = guessedLabelLen+cih,ymin=guessedLabelLen-cil), size = 1.2) + 
  ylim (3, 13) +
  xlim (3, 13) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  ylab("Guessed label length (chars)") +
  xlab("Actual label length (chars)") +
  ggtitle("Guessed vs. Actual")
```

```
## ymax not defined: adjusting position using y instead
```

<img src="RCI_02_analysis_files/figure-html/unnamed-chunk-42.png" title="plot of chunk unnamed-chunk-4" alt="plot of chunk unnamed-chunk-4" width="672" />

(3) Edits as a function of norms










