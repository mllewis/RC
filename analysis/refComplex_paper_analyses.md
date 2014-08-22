Referential Complexity Analyses
================================
M. Lewis 

August 21, 2014

***
***

<h2> Analyses:<h2>

1. [Cross-linguistic analyses](#google) <br/> 
  (A) [Complexity Norms](#1a) <br/>
  (B) [Correlation between all lengths](#1b) <br/> 
  (C) [Correlation between all lengths, controling for frequency, open class only](#1c) <br/> 
  (D) [Correlation between all lengths and complexity, controling for frequency](#1d) <br/> 
  (E) [Translation check data](#1e) <br/> 
  
2. [High frequency words in mapping task](#HF)

3. [Novel real objects](#novelRealObjs)<br/> 
  (A) [Norms](#3a)<br/> 
  (B) [Mappping task (adults)](#3b) <br/> 
  (C) [Mapping task (children)](#3c) TO DO <br/> 
  (D) [Production task (labels + descriptions)](#3d) <br/> 

4. [Geons](#geons) <br/> 
  (A) [Norms](#4a) <br/> 
  (B) [Mappping task](#4b)<br/>

* figure out how to clear before start new experiment
* clean up so that only see plots and critical statistical results
* check that all experients remove duplicates

#### SET GLOBAL VARIABLES

```r
rm(list=ls())

whichSubjRemove = 'withinRepeatSubj' #remove repeat subjects? ('keepAll', 'repeatSubj', 'withinRepeatSubj')
processNorms = TRUE # process norms or load norms? 
savePlots = FALSE # save plots to pdf?
doSlow = TRUE # do time-consuming pre-processing steps?
```

#### LOAD PACKAGES, FUNCTIONS, AND REPEAT SUBJ DATA FILE


***
***

<a name="google"/>

## (1) Cross-linguistic analyses [(Complexity norms task)][task26]

<a name="1a"/>
###  (A) Norms

* preprocess


* read in xling data and merge with English complexity norms


* look at words by class
![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

* look at accuracy for translations checks
+ preprocess


+ plot and stats
+  accuracy

```
## Error: object 'accuracy' not found
```

```
## Error: object 'dfa' not found
```

```
## Error: object 'dfa' not found
```

```
## [1] "total accuracy: 0.919004676018704"
```

```
## Error: error in evaluating the argument 'x' in selecting a method for function 'print': Error in mean(open_accuracy) : 
##   error in evaluating the argument 'x' in selecting a method for function 'mean': Error: object 'open_accuracy' not found
## Calls: paste -> mean
```

```
## [1] "open_bare_accuracy: 0.92296389588581"
```

<a name="1b"/>
###  (B) Correlation between all lengths

```r
lens = c(which(grepl("LEN",names(xling)))) # get length column indices
col1 <- colorRampPalette(c("blue", "white" , "red"))

## Correlations between all lengths, all words
xling_len = xling[, lens] 
names(xling_len) = as.character(tolower(lapply(str_split(names(xling_len),"_"),function(x) {x[1]})))

# Correlations between all lengths
cmat = cor(xling_len, use = "pairwise.complete.obs")
corrplot(cmat,  tl.cex=.5, tl.srt=45, method = "color", tl.col = "black" ,col =col1(100),order = "FPC")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-81.png) 

```r
mean(cmat)
```

```
## [1] 0.3213
```

```r
## Correlations between all lengths, open class words only
xlingO = xling[xling$Open_class != 0,lens] 
names(xlingO) = as.character(tolower(lapply(str_split(names(xlingO),"_"),function(x) {x[1]})))

# Correlations between all lenghts
cmat = cor(xlingO, use = "pairwise.complete.obs")
corrplot(cmat,  tl.cex=.5, tl.srt=45, method = "color", tl.col = "black" ,col =col1(100), order = "FPC")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-82.png) 

```r
mean(cmat)
```

```
## [1] 0.2876
```
<a name="1c"/>
###  ( C ) Correlation between all lengths, controling for frequency

```r
## all words
xling_len_p = xling[,c(lens, which(names(xling)== "log.e.freq"))] 
names(xling_len_p) = as.character(tolower(lapply(str_split(names(xling_len_p),"_"),function(x) {x[1]})))

# correlations between all lengths, open class only
cmat.p = partial.r(xling_len_p,1:80,81 ) 
mean(cmat.p)
```

```
## [1] 0.216
```

```r
## open class words only
xlingOF = xling[xling$Open_class !=0 ,c(lens, which(names(xling)== "log.e.freq"))] 
names(xlingOF) = as.character(tolower(lapply(str_split(names(xlingOF),"_"),function(x) {x[1]})))

# correlations between all lengths, open class only
cmat.p = partial.r(xlingOF,1:80,81 ) 

# sorted by first principle component
if (savePlots) {pdf('sort.pdf',height = 10, width = 10)}
corrplot(cmat.p,  tl.cex=.5, tl.srt=45,  order = "FPC", method = "color", tl.col = "black" ,col =col1(100))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-91.png) 

```r
if (savePlots) {dev.off() }
# sorted by  angular order of the eigenvectors.
corrplot(cmat.p,  tl.cex=.5, tl.srt=45,  order = "AOE", method = "color", tl.col = "black" ,col =col1(100))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-92.png) 

```r
# sorted by hierarchical clustering
corrplot(cmat.p,  tl.cex=.5, tl.srt=45,  order = "hclus", method = "color", tl.col = "black", col =col1(100) )
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-93.png) 

```r
mean(cmat.p)
```

```
## [1] 0.2201
```


<a name="1d"/>
###  (D) Correlation between  lengths and complexity, open class only, controling for frequency



```r
c_l$checked_only = ifelse(c_l$Checked == "yes", c_l$corr, 0)
c_l$uci = ifelse(c_l$Checked == "yes",  c_l$upper.ci, 0)
c_l$lci = ifelse(c_l$Checked == "yes",  c_l$lower.ci, 0)

### Plot with bootsrapped CIs on pearsons are, and parial frequencies
if (savePlots) {pdf("figure/p3.pdf", width = 10, height = 6 ) }
ggplot(c_l, aes(language, corr, fill = Checked)) + 
  geom_bar(stat = "identity", ) + 
  ylab("Pearson's r") + xlab("Language") + 
  #ggtitle("Correlation between word length and complexity norms") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_linerange(aes(ymax=upper.ci, ymin=lower.ci)) +
  geom_point(data=c_l, mapping=aes(x=language, y=p.corr), size=2, shape = 17) +
  geom_hline(y=mean(c_l$corr),lty=2) +
  theme(
    plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   #,panel.border = element_blank()
  )  +
  theme(axis.title.x = element_text( size=25), axis.text.x  = element_text( size=10),
        axis.title.y = element_text( size=25), axis.text.y  = element_text( size=10)) +
  theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10)) +
 # annotate("text", x = 75, y =mean(c_l$corr) + .02 , label=paste("M=",round(mean(c_l$corr),2), sep = "")) +
  scale_fill_manual(values=c("pink", "red")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(-.07, .7)) 
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 

```r
if (savePlots) {dev.off()}

# verify correlations by looking at English
partial.r(xlingOC,c(1,which(names(xlingOC) == "complexity")), which(names(xlingOC) == "log.e.freq"))
```

```
## partial correlations 
##            english complexity
## english       1.00       0.57
## complexity    0.57       1.00
```

```r
cor(xlingOC$english,xlingOC$complexity, use = "pairwise")
```

```
## [1] 0.6387
```


```r
# mean correlation
mean(c_l$corr)
```

```
## [1] 0.3089
```

***
***

<a name="HF"/>
## (2) High frequency words in mapping task [(Task)][task32]

* read in data and prep variables


* merge in stuff


* get quintiles


* aggregate by word


* plot bet to long word vs. complexity norms

```r
ggplot(ms, aes(norms.lf, LongBet)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbarh(aes(xmin=complexity_cil, xmax=complexity_cih), size=0.2, colour="grey") +
  geom_errorbar(aes(ymin=bet_cil, ymax=bet_cih), size=0.2, colour="grey") +
  annotate("text", x=6, y=25, label=paste("r=",round(cor(ms$norms.lf, ms$LongBet, use = "complete"), 2)))+
  xlab("Complexity Norms") +
  ylab("Bet to Long Word") +
  geom_vline(xintercept = q, col = "red") +
  ggtitle("High Frequency meanings (words)")
```

```
## Error: 'x' must be numeric
```

* correlation between norms and length

```r
# correlation between norms and bets to long word (all)
cor.test(d$LongBet,d$complexity)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  d$LongBet and d$complexity
## t = 4.229, df = 1998, p-value = 2.459e-05
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.05056 0.13744
## sample estimates:
##     cor 
## 0.09418
```

```r
bm.partial(d$LongBet,d$complexity, d$log.e.freq )
```

```
## [1] 0.0904
```

```r
#partial.r(d[,c(4,8,10)],c(1,2),3 )

# correlation between norms and bets to long word (aggregated across words)
cor.test(ms$LongBet,ms$complexity)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms$LongBet and ms$complexity
## t = 2.998, df = 94, p-value = 0.003476
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.1009 0.4682
## sample estimates:
##    cor 
## 0.2954
```

```r
bm.partial(ms$LongBet,ms$complexity, ms$log.e.freq  )
```

```
## [1] 0.3163
```

```r
summary(lmer(LongBet ~ complexity + log.e.freq + (1|trial) + (1|workerid), d))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: LongBet ~ complexity + log.e.freq + (1 | trial) + (1 | workerid)
##    Data: d
## 
## REML criterion at convergence: 17981
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -2.2832 -0.7451  0.0176  0.7584  2.3186 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  workerid (Intercept)  15.4     3.93   
##  trial    (Intercept)   1.6     1.26   
##  Residual             634.9    25.20   
## Number of obs: 1931, groups: workerid, 200; trial, 10
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   53.188      3.037   17.51
## complexity     2.779      0.660    4.21
## log.e.freq    -3.860      0.624   -6.19
## 
## Correlation of Fixed Effects:
##            (Intr) cmplxt
## complexity -0.728       
## log.e.freq -0.665  0.035
```

* plot by quintiles

```r
#aggregate by quintile
ms <- aggregate(LongBet  ~ quintile , data=d, mean)
ms$bet_cil <- aggregate(LongBet  ~ quintile, data=d, ci.low)$LongBet  
ms$bet_cih <- aggregate(LongBet  ~ quintile, data=d, ci.high)$LongBet  

ggplot(ms, aes(quintile, LongBet)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbar(aes(ymin=bet_cil, ymax=bet_cih), size=0.2, colour="black") +
  annotate("text", x=5, y=25, label=paste("r=",round(cor(ms$quintile, ms$LongBet, use = "complete"), 2)))+
  scale_y_continuous(limits = c(20, 80)) +
  #scale_x_continuous(limits = c(0, 7), breaks = 1:7, labels = 1:7)  +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
  xlab("Complexity Norm quintile") +
  ylab("Bet to Long Word") +
  ggtitle("High Frequency meanings (words)")
```

![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19.png) 

* correlations with quintiles

```r
# correlation between norms quintiles and bets to long word 
cor.test(d$quintile, d$LongBet)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  d$quintile and d$LongBet
## t = 4.083, df = 1998, p-value = 4.621e-05
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.04732 0.13426
## sample estimates:
##     cor 
## 0.09097
```

```r
# correlation between norms quintiles and bets to long word (aggregated across words)
cor.test(ms$quintile, ms$LongBet)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms$quintile and ms$LongBet
## t = 2.084, df = 3, p-value = 0.1285
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.3520  0.9838
## sample estimates:
##    cor 
## 0.7691
```

* residual quintiles (controls for frequency)
+ get quintiles


* plot

```r
#aggregate by quintile
ms.qr <- aggregate(LongBet  ~ resid.quintile , data=d, mean)
ms.qr$bet_cil <- aggregate(LongBet  ~ resid.quintile, data=d, ci.low)$LongBet  
ms.qr$bet_cih <- aggregate(LongBet  ~ resid.quintile, data=d, ci.high)$LongBet  

ggplot(ms.qr, aes(resid.quintile, LongBet)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbar(aes(ymin=bet_cil, ymax=bet_cih), size=0.2, colour="black") +
  annotate("text", x=5, y=25, label=paste("r=",round(cor(d$resid.quintile, d$LongBet, use = "complete"), 2)))+
  scale_y_continuous(limits = c(20, 80)) +
  #scale_x_continuous(limits = c(0, 7), breaks = 1:7, labels = 1:7)  +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
  xlab("RESIDUAL Complexity Norm quintile") +
  ylab("Bet to Long Word") +
ggtitle("High Frequency meanings (words)")
```

```
## Warning: NaNs produced
```

![plot of chunk unnamed-chunk-22](figure/unnamed-chunk-22.png) 

* residual quintiles correlations

```r
cor.test(d$resid.quintile, d$LongBet) ### highly correlated
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  d$resid.quintile and d$LongBet
## t = 5.218, df = 1998, p-value = 1.998e-07
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.07248 0.15897
## sample estimates:
##    cor 
## 0.1159
```

```r
cor.test(ms.qr$resid.quintile, ms.qr$LongBet) 
```

```
## Error: not enough finite observations
```
***
***
<a name="novelRealObjs"/>
## (3) Novel real objects

<a name="3a"/>
### (A) Norms [Complexity norming task][task30] [RT task][task9]

#### Complexity Norms


* Get reliability between two samples

```r
  cor(co_norms$rating_1, co_norms$rating_2)
```

```
## Error: supply both 'x' and 'y' or a matrix-like 'x'
```

#### RT Norms


<a name="3b"/>
### (B) Mapping task (adults) [(Task)][task34]

* read in data and format


* make everything factors


* merge in norms








































