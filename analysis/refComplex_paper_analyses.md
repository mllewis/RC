Referential Complexity Analyses
================================
M. Lewis 

September 02, 2014

***
***

<h2> Analyses:<h2>

1. [Cross-linguistic analyses](#google) <br/> 
  (A) [Complexity Norms](#1a) <br/>
  (B) [Correlation between all lengths](#1b) <br/> 
  (C) [Correlation between all lengths, controling for frequency, open class only](#1c) <br/> 
  (D) [Correlation between all lengths and complexity, controling for frequency](#1d) <br/> 
  (E) [Translation check data](#1e) <br/> 
  
3. [Novel real objects](#novelRealObjs)<br/> 
  (A) [Norms](#3a)<br/> 
  (B) [Mappping task (adults)](#3b) <br/> 
  (C) [Mapping task (children)](#3c) TO DO <br/> 
  (D) [Production task (labels + descriptions)](#3d) <br/> 

4. [Geons](#geons) <br/> 
  (A) [Norms](#4a) <br/> 
  (B) [Mappping task](#4b)<br/>

#### SET GLOBAL VARIABLES

```r
rm(list=ls())

whichSubjRemove = 'keepAll' #remove repeat subjects? ('keepAll', 'repeatSubj', 'withinRepeatSubj')
processNorms = TRUE # process norms or load norms? 
savePlots = TRUE # save plots to pdf?
doSlow = FALSE # do time-consuming pre-processing steps?
```

#### LOAD PACKAGES, FUNCTIONS, AND REPEAT SUBJ DATA FILE


***
***

<a name="google"/>

## (1) Cross-linguistic analyses [(Complexity norms task)][task26]

<a name="1a"/>
###  (A) Norms

* preprocess


* aggregate across words of the same length

```r
ms2 <- aggregate(complexity ~ nchars, data=ms, mean)
```

```
## Error: object 'ms' not found
```

```r
ms2$cih <- aggregate(complexity ~ nchars,  data=ms, ci.high)$complexity
```

```
## Error: object 'ms' not found
```

```r
ms2$cil <- aggregate(complexity ~ nchars,  data=ms, ci.low)$complexity
```

```
## Error: object 'ms' not found
```

```r
ggplot(ms2, aes(complexity, nchars)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbarh(aes(xmax=complexity+cih,xmin=complexity-cil), size=.15, colour="black") +
  annotate("text", x=2, y=10, size = 10, label=paste("r=",round(cor(ms$complexity, ms$nchars), 2)))+
  scale_y_continuous(limits = c(0, 15), breaks = 1:14, labels = 1:14) +
  scale_x_continuous(limits = c(0, 7), breaks = 1:7, labels = 1:7)  +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
  xlab('Complexity Rating') +
  ylab('Word Length (characters)') +
  ggtitle("Number of characters vs. complexity rating")
```

```
## Error: object 'ms2' not found
```

* aggregate across participants 

```r
  # have to do this separately for each length variable because drops words for which there are NAs *for any of the length vars*
  ms.syl <- aggregate(complexity ~ word + mrc.syl, data=eng, mean)
  ms.phon <- aggregate(complexity ~ word + mrc.phon, data=eng, mean)
  ms.morph <- aggregate(complexity ~ word + clx.morph, data=eng, mean)
```

* Correlations

```r
cor.test(ms.syl$complexity, ms.syl$mrc.syl)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms.syl$complexity and ms.syl$mrc.syl
## t = 18, df = 497, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.5719 0.6785
## sample estimates:
##    cor 
## 0.6281
```

```r
cor.test(ms.phon$complexity, ms.phon$mrc.phon)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms.phon$complexity and ms.phon$mrc.phon
## t = 19.23, df = 468, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.6106 0.7120
## sample estimates:
##    cor 
## 0.6644
```

```r
cor.test(ms.morph$complexity, ms.morph$clx.morph)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms.morph$complexity and ms.morph$clx.morph
## t = 10.53, df = 486, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3559 0.5006
## sample estimates:
##   cor 
## 0.431
```

```r
# mono-morphemic
ms_mono.syl = aggregate(complexity ~ word + mrc.syl, data=md[md$clx.morph == 1,], mean)
ms_mono.phon = aggregate(complexity ~ word + mrc.phon, data=md[md$clx.morph == 1,], mean)

cor.test(ms_mono.syl$complexity, ms_mono.syl$mrc.syl)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms_mono.syl$complexity and ms_mono.syl$mrc.syl
## t = 10.19, df = 385, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3785 0.5359
## sample estimates:
##    cor 
## 0.4608
```

```r
cor.test(ms_mono.phon$complexity, ms_mono.phon$mrc.phon)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms_mono.phon$complexity and ms_mono.phon$mrc.phon
## t = 12.03, df = 364, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.4557 0.6029
## sample estimates:
##    cor 
## 0.5333
```

```r
# open class
ms_open.syl = aggregate(complexity ~ word + mrc.syl, data=md[md$class != 0,], mean)
ms_open.phon = aggregate(complexity ~ word + mrc.phon, data=md[md$class != 0,], mean)
ms_open.morph = aggregate(complexity ~ word + clx.morph, data=md[md$class != 0,], mean)
summary(ms_open.syl)
```

```
##           word        mrc.syl       complexity  
##  ache       :  1   Min.   :1.00   Min.   :1.25  
##  affirmation:  1   1st Qu.:1.00   1st Qu.:2.57  
##  after      :  1   Median :2.00   Median :3.40  
##  age        :  1   Mean   :1.79   Mean   :3.44  
##  aid        :  1   3rd Qu.:2.00   3rd Qu.:4.22  
##  all        :  1   Max.   :5.00   Max.   :6.33  
##  (Other)    :447
```

```r
cor.test(ms_open.syl$complexity, ms_open.syl$mrc.syl)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms_open.syl$complexity and ms_open.syl$mrc.syl
## t = 16.91, df = 451, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.5632 0.6763
## sample estimates:
##   cor 
## 0.623
```

```r
cor.test(ms_open.phon$complexity, ms_open.phon$mrc.phon)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms_open.phon$complexity and ms_open.phon$mrc.phon
## t = 17.89, df = 426, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.5972 0.7059
## sample estimates:
##    cor 
## 0.6549
```

```r
cor.test(ms_open.morph$complexity, ms_open.morph$clx.morph)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  ms_open.morph$complexity and ms_open.morph$clx.morph
## t = 9.959, df = 445, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3480 0.4999
## sample estimates:
##    cor 
## 0.4269
```

* relationship beween length and complexity, control for everything

```r
# all
m_syl = lm(mrc.syl ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng)
m_phon = lm(mrc.phon ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng)
m_morph = lm(clx.morph ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng)
summary(m_syl)
```

```
## 
## Call:
## lm(formula = mrc.syl ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.972 -0.529 -0.089  0.445  3.323 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.813777   0.088517   31.79  < 2e-16 ***
## complexity     0.095021   0.005179   18.35  < 2e-16 ***
## mrc.fam        0.001236   0.000211    5.86  4.9e-09 ***
## mrc.imag       0.000313   0.000162    1.93    0.053 .  
## b.conc        -0.245203   0.015355  -15.97  < 2e-16 ***
## subt.log.freq -0.457728   0.014508  -31.55  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.777 on 6984 degrees of freedom
##   (308 observations deleted due to missingness)
## Multiple R-squared:  0.324,	Adjusted R-squared:  0.323 
## F-statistic:  669 on 5 and 6984 DF,  p-value: <2e-16
```

```r
summary(m_phon)
```

```
## 
## Call:
## lm(formula = mrc.phon ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.891 -1.068 -0.144  0.936  7.562 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    7.718613   0.202310   38.15  < 2e-16 ***
## complexity     0.214601   0.011518   18.63  < 2e-16 ***
## mrc.fam        0.001639   0.000487    3.37  0.00077 ***
## mrc.imag       0.001027   0.000364    2.82  0.00486 ** 
## b.conc        -0.546185   0.034531  -15.82  < 2e-16 ***
## subt.log.freq -1.122311   0.032758  -34.26  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.67 on 6564 degrees of freedom
##   (728 observations deleted due to missingness)
## Multiple R-squared:  0.387,	Adjusted R-squared:  0.387 
## F-statistic:  830 on 5 and 6564 DF,  p-value: <2e-16
```

```r
summary(m_morph)
```

```
## 
## Call:
## lm(formula = clx.morph ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -0.864 -0.287 -0.113  0.150  2.407 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.70e+00   5.29e-02   32.14  < 2e-16 ***
## complexity     2.85e-02   3.11e-03    9.16  < 2e-16 ***
## mrc.fam        9.12e-04   1.26e-04    7.24  5.0e-13 ***
## mrc.imag       4.30e-04   9.69e-05    4.44  9.2e-06 ***
## b.conc        -1.56e-01   9.17e-03  -17.00  < 2e-16 ***
## subt.log.freq -2.32e-01   8.69e-03  -26.74  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.462 on 6862 degrees of freedom
##   (430 observations deleted due to missingness)
## Multiple R-squared:  0.223,	Adjusted R-squared:  0.222 
## F-statistic:  393 on 5 and 6862 DF,  p-value: <2e-16
```

```r
# mono-morphemic
m_syl_m = lm(mrc.syl ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng[eng$clx.morph == 1,])
m_phon_m = lm(mrc.phon ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng[eng$clx.morph == 1,])
summary(m_syl_m)
```

```
## 
## Call:
## lm(formula = mrc.syl ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng[eng$clx.morph == 1, ])
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.085 -0.424 -0.181  0.402  3.309 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.863115   0.079002   23.58  < 2e-16 ***
## complexity     0.049478   0.004587   10.79  < 2e-16 ***
## mrc.fam        0.000605   0.000197    3.06   0.0022 ** 
## mrc.imag       0.000656   0.000143    4.58  4.8e-06 ***
## b.conc        -0.142304   0.013733  -10.36  < 2e-16 ***
## subt.log.freq -0.232356   0.013718  -16.94  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.596 on 5366 degrees of freedom
##   (383 observations deleted due to missingness)
## Multiple R-squared:  0.15,	Adjusted R-squared:  0.15 
## F-statistic:  190 on 5 and 5366 DF,  p-value: <2e-16
```

```r
summary(m_phon_m)
```

```
## 
## Call:
## lm(formula = mrc.phon ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng[eng$clx.morph == 1, ])
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -3.743 -0.756 -0.138  0.608  6.299 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    5.756289   0.183786   31.32  < 2e-16 ***
## complexity     0.117192   0.010259   11.42  < 2e-16 ***
## mrc.fam        0.000909   0.000466    1.95    0.051 .  
## mrc.imag       0.001650   0.000322    5.13  3.1e-07 ***
## b.conc        -0.387371   0.030343  -12.77  < 2e-16 ***
## subt.log.freq -0.680914   0.031636  -21.52  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.29 on 5069 degrees of freedom
##   (680 observations deleted due to missingness)
## Multiple R-squared:  0.244,	Adjusted R-squared:  0.243 
## F-statistic:  327 on 5 and 5069 DF,  p-value: <2e-16
```

```r
# open class
m_syl_o = lm(mrc.syl ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng[eng$class != 0,])
m_phon_o = lm(mrc.phon ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng[eng$class != 0,])
m_morph_o = lm(clx.morph ~ complexity + mrc.fam + mrc.imag + b.conc + subt.log.freq, eng[eng$class != 0,])
summary(m_syl_o)
```

```
## 
## Call:
## lm(formula = mrc.syl ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng[eng$class != 0, ])
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -1.994 -0.557 -0.097  0.487  3.306 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    2.875059   0.094716   30.35  < 2e-16 ***
## complexity     0.097953   0.005576   17.57  < 2e-16 ***
## mrc.fam        0.001178   0.000223    5.29  1.3e-07 ***
## mrc.imag       0.000230   0.000179    1.29      0.2    
## b.conc        -0.249263   0.016172  -15.41  < 2e-16 ***
## subt.log.freq -0.450754   0.016446  -27.41  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8 on 6404 degrees of freedom
##   (242 observations deleted due to missingness)
## Multiple R-squared:  0.304,	Adjusted R-squared:  0.303 
## F-statistic:  558 on 5 and 6404 DF,  p-value: <2e-16
```

```r
summary(m_phon_o)
```

```
## 
## Call:
## lm(formula = mrc.phon ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng[eng$class != 0, ])
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -4.971 -1.112 -0.165  0.974  7.486 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    7.870476   0.213747   36.82  < 2e-16 ***
## complexity     0.219959   0.012306   17.87  < 2e-16 ***
## mrc.fam        0.001742   0.000511    3.41  0.00065 ***
## mrc.imag       0.000323   0.000400    0.81  0.42000    
## b.conc        -0.536074   0.036215  -14.80  < 2e-16 ***
## subt.log.freq -1.085803   0.037015  -29.33  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.72 on 6044 degrees of freedom
##   (602 observations deleted due to missingness)
## Multiple R-squared:  0.352,	Adjusted R-squared:  0.352 
## F-statistic:  658 on 5 and 6044 DF,  p-value: <2e-16
```

```r
summary(m_morph_o)
```

```
## 
## Call:
## lm(formula = clx.morph ~ complexity + mrc.fam + mrc.imag + b.conc + 
##     subt.log.freq, data = eng[eng$class != 0, ])
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -0.890 -0.300 -0.119  0.164  2.410 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    1.665764   0.056453   29.51  < 2e-16 ***
## complexity     0.030717   0.003327    9.23  < 2e-16 ***
## mrc.fam        0.000954   0.000133    7.19  7.2e-13 ***
## mrc.imag       0.000595   0.000106    5.59  2.4e-08 ***
## b.conc        -0.164054   0.009622  -17.05  < 2e-16 ***
## subt.log.freq -0.248820   0.009783  -25.44  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.475 on 6350 degrees of freedom
##   (296 observations deleted due to missingness)
## Multiple R-squared:  0.22,	Adjusted R-squared:  0.219 
## F-statistic:  357 on 5 and 6350 DF,  p-value: <2e-16
```

* create english word data frame with relevant variables for xling analysis

```r
  xling.eng <- aggregate(complexity ~ word + class, data=eng, mean)

  # -- add morphemes --
  morph = read.csv("data/numMorph_celex2.csv")
  index <- match(xling.eng$word, morph$ENGLISH)
  xling.eng$clx.morph <- morph$clx.numMorph[index]

  # -- add frequency --
  freqs = read.table("data/SUBTLEXusDataBase.txt",header=TRUE)
  index <- match(xling.eng$word, freqs$Word)
  xling.eng$subt.log.freq <- freqs$Lg10WF[index]
  
  xling.eng$class = as.factor(xling.eng$class)
  xling.eng$clx.morph = as.factor(xling.eng$clx.morph)
```

* read in xling data and merge with English complexity norms


* look at accuracy for translations checks


<a name="1b"/>
###  (B) Correlation between complextity and length

```r
lens = c(which(grepl("LEN",names(xling)))) # get length column indices
xlingCORR = xling[c(lens, which(names(xling)== "subt.log.freq"), which(names(xling)== "complexity"), which(names(xling)== "clx.morph"), which(names(xling)== "class"))] 

# get correlations with for all 499 words bootstrapped CIs
if (doSlow) {
  c_l = data.frame (language = character(), lower.ci = numeric(0), corr = numeric(0), upper.ci = numeric(0))
  levels(c_l$language) = names(xlingCORR)
  complexity_i = which(names(xlingCORR)== "complexity")
  
  for (i in 1:length(lens)){
    c_l[i, 2:4] = boot.cor(xlingCORR[,i], xlingCORR[,complexity_i])
    c_l[i, "language"] = names(xlingCORR)[i]
    print(names(xlingCORR)[i])
    }
  write.csv(c_l, "data/complexity_length_corrs.csv")
}
c_l = read.csv("data/complexity_length_corrs.csv")

# now do correlations, partialling out frequency
cmat.p = partial.r(xlingCORR,c(1:80, which(names(xlingCORR) == "complexity")), which(names(xlingCORR) == "subt.log.freq") )
```

```
## Error: 'x' must be numeric
```

```r
cxl = as.data.frame(cmat.p[which(row.names(cmat.p) == "complexity"),1:80])
```

```
## Error: object 'cmat.p' not found
```

```r
cxl$lang = row.names(cxl)
```

```
## Error: object 'cxl' not found
```

```r
names(cxl) = c("corr", "lang")
```

```
## Error: object 'cxl' not found
```

```r
# merge in partials
index <- match(c_l$language, cxl$lang)
```

```
## Error: object 'cxl' not found
```

```r
c_l$p.corr<- cxl$corr[index]
```

```
## Error: object 'cxl' not found
```

```r
# get correlations for mono only
mono_cor = data.frame (language = character(), mono.cor = numeric(0))
complexity_i = which(names(xlingCORR)== "complexity")
levels(mono_cor$language) = names(xlingCORR)


for (i in 1:length(lens)){
  mono_cor[i, "mono.cor"] = cor(xlingCORR[xlingCORR$clx.morph == 1, i], 
                                xlingCORR[xlingCORR$clx.morph == 1, complexity_i], use = "complete")
  mono_cor[i, "language"] = names(xlingCORR)[i]
}

# merge in mono
index <- match(c_l$language, mono_cor$language)
c_l$mono.cor<- mono_cor$mono.cor[index]

# get correlations for open only
open_cor = data.frame(language = character(), open.cor = numeric(0))
complexity_i = which(names(xlingCORR)== "complexity")
levels(open_cor$language) = names(xlingCORR)

for (i in 1:length(lens)){
  open_cor[i, "open.cor"] = cor(xlingCORR[xlingCORR$class != 0, i], 
                                xlingCORR[xlingCORR$class != 0, complexity_i], use = "complete")
  open_cor[i, "language"] = names(xlingCORR)[i]
}

# merge in open
index <- match(c_l$language, open_cor$language)
c_l$open.cor<- open_cor$open.cor[index]

# prep for plotting
c_l = c_l[order(c_l$corr),] # sort by correlation
c_l$language=  as.character(tolower(lapply(str_split(c_l$language,"_"),function(x) {x[1]}))) # clean up names
c_l$language <- factor(c_l$language, levels =  rev(as.character(c_l$language)))


# add check information
coded_languages = c("english", "spanish", "welsh", "vietnamese", "russian", "portuguese", "persian", "bosnian", "french", "hebrew", "italian", "korean", "polish" )
c_l$checked = as.factor(ifelse( is.element(c_l$language, coded_languages), "yes", "no"))
```

* plot

```r
if (savePlots) {pdf("figure/xling.pdf", height = 6, width = 12)}

ggplot(c_l, aes(language, corr, fill = checked)) + 
  geom_bar(stat = "identity") + 
  geom_linerange(aes(ymax=upper.ci, ymin=lower.ci)) +
  geom_point(data=c_l, mapping=aes(x=language, y=p.corr), size=2, shape = 17) +
  geom_point(data=c_l, mapping=aes(x=language, y=mono.cor), size=2, shape = 16) +
  geom_point(data=c_l, mapping=aes(x=language, y=open.cor), size=2, shape = 15) +
  geom_hline(y=mean(c_l$corr), lty=2) +
  ylab("Pearson's r") +
  xlab("Language") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.background = element_blank(),
   panel.grid.major = element_blank(),
   panel.grid.minor = element_blank(),
   panel.border = element_blank())  +
  theme(axis.title.x = element_text(size=25), axis.text.x  = element_text(size=10),
        axis.title.y = element_text(size=25), axis.text.y  = element_text(size=10)) +
  theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10)) +
  theme(axis.line = element_line(color = 'black'))+
  scale_fill_manual(values=c("pink", "red")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(-.07, .75)) 
```

```
## Error: object 'p.corr' not found
```

```r
if(savePlots){dev.off()}
```

```
## pdf 
##   2
```

* Grand means

```r
mean(c_l$corr)
```

```
## [1] 0.3356
```

```r
mean(c_l$mono.cor)
```

```
## [1] 0.2333
```

```r
mean(c_l$open.cor)
```

```
## [1] 0.3089
```

```r
mean(c_l$p.corr)
```

```
## Warning: argument is not numeric or logical: returning NA
```

```
## [1] NA
```

***
***

## (3) Novel real objects

<a name="3a"/>
### (A) Norms [Complexity norming task][task30] [RT task][task9]

#### Complexity Norms


* Get reliability between two samples

```r
  cor.test(co_norms$rating_1, co_norms$rating_2)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  co_norms$rating_1 and co_norms$rating_2
## t = 18.91, df = 58, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.8812 0.9563
## sample estimates:
##    cor 
## 0.9276
```

* There is one participant who was in both samples. Look at correlation between samples without this participant in Sample #2.

```r
  ## Sample #2 
  # melt
  mdb <- melt(db[db$workerid != 'A1BQEX75BE1AYE',],id.vars=c("workerid"),measure.vars=names(db)[grepl("rating",names(db))])
  names(mdb) <- c("workerid", "rating", "value")
  msb = aggregate(value ~ rating, mdb, mean)
  
  co_norms_unique_sample = merge( ms, msb, by="rating")
  co_norms_unique_sample$ratingNum <- as.numeric(matrix(sapply(str_split(matrix(sapply(str_split(co_norms_unique_sample$rating,"rating")
                                                                                       ,function(x) {x[2]})),"_"),function(x){x[1]})))
```

```
## Warning: NAs introduced by coercion
```

```r
  co_norms_unique_sample = co_norms_unique_sample[!is.na(co_norms_unique_sample$ratingNum),] # get rid of ball and motherboard

  co_norms_unique_sample$rating <- NULL
  names(co_norms_unique_sample) = c( "rating_1", "rating_2", "ratingNum")
  cor(co_norms_unique_sample$rating_1, co_norms_unique_sample$rating_2) # nearly identical to original sample
```

```
## [1] 0.9274
```

#### RT Norms


* correlation between norms

```r
index <- match(co_norms$ratingNum, rto_norms$Answer.train_image)
co_norms$log.rt <- rto_norms$log.rt[index]
cor.test(co_norms$meanRating, co_norms$log.rt)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  co_norms$meanRating and co_norms$log.rt
## t = 4.865, df = 58, p-value = 9.152e-06
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.3294 0.6970
## sample estimates:
##    cor 
## 0.5383
```

<a name="3b"/>
### (B) Mapping task (adults) [(Task)][task34]

* read in data and format


* make everything factors


* merge in norms


* get effect sizes


* get obj conds


* plot

```r
# set graphical params
fs = 25
rs = 8

# complexity plot
obj_c_plot = ggplot(de, aes(y=effect_size, x=c.Mratio)) +
  geom_pointrange(aes(ymax = cill, ymin=ciul),position="dodge")+
  geom_hline(yintercept=0,lty=2) +
  stat_smooth(method="lm") +
  #geom_text(aes(c.Mratio+.03, effect_size, label=objCondition2), position="dodge") +
  #geom_text(aes(c.Mratio+.04, effect_size, label=objCondition2), position="dodge") +
  ylab("effect size (Cohen's d)") +
  xlab("complexity rating ratio") + 
  #ggtitle("complexity ratio vs. effect size") +
  scale_x_continuous(limits = c(.25, 1.29)) +
  theme(text = element_text(size=fs), plot.title = element_text(size=20)) +
  annotate("text", x=1.15, y=.5, col = "red",label=paste(
    "r=",round(cor(de$effect_size, de$c.Mratio, use = "complete"), 2)), size = rs) +
  theme(plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(-.33, .66))

# RT ratio plot
obj_rt_plot = ggplot(de, aes(y=effect_size, x=rt.Mratio)) +
  geom_pointrange( aes(ymax = cill, ymin=ciul))+
  geom_hline(yintercept=0,lty=2) +
  stat_smooth(method="lm") +
  #geom_text(aes(rt.Mratio+.0008, effect_size, label=objCondition2)) +
  #geom_text(aes(rt.Mratio+.002, effect_size, label=objCondition2)) +
  ylab("effect size (Cohen's d)") +
  xlab("reaction time ratio") +
  scale_x_continuous(limits = c(.949, 1.005)) +
  #ggtitle("RT ratio vs. effect size") +
  theme(text = element_text(size=fs), plot.title = element_text(size=20)) +
  annotate("text", x=.997, y=.5, col = "red",label=paste("r=",round(cor(de$effect_size, de$rt.Mratio, use = "complete"), 2)), size = rs) +  
  theme(plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) +
  scale_y_continuous(limits = c(-.33, .66))

if (savePlots) {pdf("figure/realobjs.pdf", height = 6, width = 12)}
multiplot(obj_c_plot, obj_rt_plot, cols = 2)
if (savePlots) {dev.off()}
```

```
## pdf 
##   2
```

* correlations between effect size at complexity conditions

```r
cor.test(de$objRatio, de$effect_size)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  de$objRatio and de$effect_size
## t = -3.051, df = 13, p-value = 0.009273
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.8703 -0.1999
## sample estimates:
##    cor 
## -0.646
```

```r
cor.test(de$c.Mratio, de$effect_size)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  de$c.Mratio and de$effect_size
## t = -3.494, df = 13, p-value = 0.00396
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.8907 -0.2854
## sample estimates:
##     cor 
## -0.6959
```

```r
cor.test(de$rt.Mratio, de$effect_size)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  de$rt.Mratio and de$effect_size
## t = -3.659, df = 13, p-value = 0.002887
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.8972 -0.3149
## sample estimates:
##     cor 
## -0.7123
```

<a name="3d"/>
### (D) Production task (labels + desecriptions) 
#### (1) Labels [(Task)][task27]
##### read in data and prep data frame


* relationship between condition and description length

```r
t.test(md[md$condition == '"complex"',"log.length"],md[md$condition == '"simple"',"log.length"],paired = TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  md[md$condition == "\"complex\"", "log.length"] and md[md$condition == "\"simple\"", "log.length"]
## t = 3.735, df = 286, p-value = 0.0002269
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.03846 0.12417
## sample estimates:
## mean of the differences 
##                 0.08131
```

```r
summary(lmer(log.length~condition + (1+trial|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length ~ condition + (1 + trial | workerid)
##    Data: md
## 
## REML criterion at convergence: -0.4
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -4.009 -0.626  0.061  0.633  2.773 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr
##  workerid (Intercept) 1.53e-02 0.12358      
##           trial       3.40e-06 0.00184  1.00
##  Residual             4.92e-02 0.22189      
## Number of obs: 574, groups: workerid, 59
## 
## Fixed effects:
##                   Estimate Std. Error t value
## (Intercept)         1.9350     0.0218    88.7
## condition"simple"  -0.0797     0.0185    -4.3
## 
## Correlation of Fixed Effects:
##             (Intr)
## cndtn"smpl" -0.427
```

* relationship with complicated norms

```r
index <- match(md$picture, co_norms$ratingNum)
md$c.norms <- co_norms$meanRating[index]

ms <- aggregate(log.length ~ c.norms + picture, data=md, mean)
ms$cih <- aggregate(log.length ~ c.norms + picture, data=md, ci.high)$log.length
ms$cil <- aggregate(log.length ~ c.norms + picture, data=md, ci.low)$log.length

#plot
ggplot(ms, aes(c.norms,log.length)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbar(aes(ymax=log.length+cih,ymin=log.length-cil), size=0.2, colour="grey") +
  theme_bw() +
  xlab("Object Complexity Norms") +
  ylab("Log Word Length (characters)") +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) +
  annotate("text", x=.75, y=1.6, color = "red", size = 8,
    label=paste("r=",round(cor(md$log.length,md$c.norms), 2)))
```

![plot of chunk unnamed-chunk-23](figure/unnamed-chunk-23.png) 

* relationship with RT norms

```r
index <- match(md$picture, rto_norms$Answer.train_image)
md$rt.norms <- rto_norms$log.rt[index]

ms <- aggregate(log.length ~ rt.norms + picture, data=md, mean)
ms$cih <- aggregate(log.length ~ rt.norms + picture, data=md, ci.high)$log.length
ms$cil <- aggregate(log.length ~ rt.norms + picture, data=md, ci.low)$log.length

#plot
ggplot(ms, aes(rt.norms,log.length)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbar(aes(ymax=log.length+cih,ymin=log.length-cil), size=0.2, colour="grey") +
  theme_bw() +
  xlab("Object Complexity Norms") +
  ylab("Log Word Length (characters)") +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15))+
  annotate("text", x=.75, y=1.6, color = "red", size = 8,
    label=paste("r=",round(cor(md$log.length,md$rt.norms), 2)))
```

![plot of chunk unnamed-chunk-24](figure/unnamed-chunk-24.png) 

#### (2) Descriptions [(Task)][task25]

* read in data and prep data frame


* relationship between condition and description length

```r
#summary(lmer(length_c~condition + (1|workerid), md))
#summary(lmer(length_c~condition + trial + (1+trial|workerid), md))

summary(lmer(log.length_c~md$condition + (1|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length_c ~ md$condition + (1 | workerid)
##    Data: md
## 
## REML criterion at convergence: 863.1
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.004 -0.616  0.071  0.599  4.269 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  workerid (Intercept) 0.476    0.690   
##  Residual             0.175    0.418   
## Number of obs: 600, groups: workerid, 60
## 
## Fixed effects:
##                      Estimate Std. Error t value
## (Intercept)            3.3545     0.0922    36.4
## md$condition"simple"  -0.1128     0.0342    -3.3
## 
## Correlation of Fixed Effects:
##             (Intr)
## md$cndtn"s" -0.185
```

```r
summary(lmer(log.length_c~condition + trial + (1+trial|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length_c ~ condition + trial + (1 + trial | workerid)
##    Data: md
## 
## REML criterion at convergence: 823.1
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.185 -0.559  0.071  0.594  3.841 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  workerid (Intercept) 0.43168  0.6570        
##           trial       0.00177  0.0421   -0.02
##  Residual             0.14949  0.3866        
## Number of obs: 600, groups: workerid, 60
## 
## Fixed effects:
##                   Estimate Std. Error t value
## (Intercept)        3.54229    0.09310    38.0
## condition"simple" -0.12772    0.03260    -3.9
## trial             -0.03278    0.00773    -4.2
## 
## Correlation of Fixed Effects:
##             (Intr) cndt""
## cndtn"smpl" -0.189       
## trial       -0.252  0.031
```

```r
## plot
ggplot(md, aes(x=log.length_c, fill=condition)) + geom_density(alpha = 0.2)
```

![plot of chunk unnamed-chunk-26](figure/unnamed-chunk-26.png) 

* correlations with complexity norms

```r
index <- match(md$picture, co_norms$ratingNum)
md$c.norms <- co_norms$meanRating[index]

summary(lmer(log.length_c~c.norms + (1+trial|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length_c ~ c.norms + (1 + trial | workerid)
##    Data: md
## 
## REML criterion at convergence: 826.8
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.272 -0.551  0.069  0.587  3.893 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  workerid (Intercept) 0.44295  0.6655        
##           trial       0.00275  0.0524   -0.11
##  Residual             0.14874  0.3857        
## Number of obs: 600, groups: workerid, 60
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   3.2392     0.0944    34.3
## c.norms       0.2921     0.0691     4.2
## 
## Correlation of Fixed Effects:
##         (Intr)
## c.norms -0.346
```

```r
summary(lmer(log.length_c~c.norms + trial + (1|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length_c ~ c.norms + trial + (1 | workerid)
##    Data: md
## 
## REML criterion at convergence: 836
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.062 -0.625  0.060  0.597  4.042 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  workerid (Intercept) 0.477    0.690   
##  Residual             0.165    0.406   
## Number of obs: 600, groups: workerid, 60
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)  3.34180    0.10143    32.9
## c.norms      0.28220    0.07006     4.0
## trial       -0.03248    0.00577    -5.6
## 
## Correlation of Fixed Effects:
##         (Intr) c.nrms
## c.norms -0.322       
## trial   -0.304 -0.027
```

```r
# complexity norms predict length

cor.test(md$log.length_c,md$c.norms)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  md$log.length_c and md$c.norms
## t = 1.965, df = 598, p-value = 0.04993
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  3.457e-05 1.591e-01
## sample estimates:
##     cor 
## 0.08008
```

* complexity norms plot

```r
ms <- aggregate(log.length_c ~ c.norms + picture, data=md, mean)
ms$cih <- aggregate(log.length_c ~ c.norms + picture, data=md, ci.high)$log.length_c
ms$cil <- aggregate(log.length_c ~ c.norms + picture, data=md, ci.low)$log.length_c

ggplot(ms, aes(c.norms,log.length_c)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbar(aes(ymax=log.length_c+cih,ymin=log.length_c-cil), size=0.2, colour="grey") +
  theme_bw() +
  xlab("Object Complexity Norms") +
  ylab("Log Description Length (characters)") +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) 
```

![plot of chunk unnamed-chunk-28](figure/unnamed-chunk-28.png) 

* correlations with RT norms

```r
index <- match(md$picture, rto_norms$Answer.train_image)
md$rt.norms <- rto_norms$log.rt[index]

summary(lmer(log.length_c~rt.norms + (1+trial|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length_c ~ rt.norms + (1 + trial | workerid)
##    Data: md
## 
## REML criterion at convergence: 825.6
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.306 -0.589  0.083  0.590  3.705 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev. Corr 
##  workerid (Intercept) 0.43683  0.6609        
##           trial       0.00277  0.0527   -0.08
##  Residual             0.14889  0.3859        
## Number of obs: 600, groups: workerid, 60
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept)   -2.262      1.373   -1.65
## rt.norms       0.770      0.187    4.12
## 
## Correlation of Fixed Effects:
##          (Intr)
## rt.norms -0.998
```

```r
summary(lmer(log.length_c~rt.norms + trial + (1|workerid), md))
```

```
## Linear mixed model fit by REML ['lmerMod']
## Formula: log.length_c ~ rt.norms + trial + (1 | workerid)
##    Data: md
## 
## REML criterion at convergence: 836.3
## 
## Scaled residuals: 
##    Min     1Q Median     3Q    Max 
## -3.119 -0.587  0.079  0.615  3.803 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  workerid (Intercept) 0.481    0.693   
##  Residual             0.165    0.406   
## Number of obs: 600, groups: workerid, 60
## 
## Fixed effects:
##             Estimate Std. Error t value
## (Intercept) -1.71467    1.39726   -1.23
## rt.norms     0.70745    0.19008    3.72
## trial       -0.03215    0.00578   -5.57
## 
## Correlation of Fixed Effects:
##          (Intr) rt.nrm
## rt.norms -0.998       
## trial    -0.009 -0.014
```

```r
#rt norms predict length

cor.test(md$log.length_c,md$rt.norms)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  md$log.length_c and md$rt.norms
## t = 0.848, df = 598, p-value = 0.3968
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.04551  0.11438
## sample estimates:
##     cor 
## 0.03466
```

* rt norms plot

```r
ms <- aggregate(log.length_c ~ rt.norms + picture, data=md, mean)
ms$cil <- aggregate(log.length_c ~ rt.norms + picture, data=md, ci.low)$log.length_c
ms$cih <- aggregate(log.length_c ~ rt.norms + picture, data=md, ci.high)$log.length_c

ggplot(ms, aes(rt.norms,log.length_c)) +
  geom_point() + 
  geom_smooth(method = "lm", color="blue", formula = y ~ x) +
  geom_errorbar(aes(ymax=log.length_c+cih,ymin=log.length_c-cil), size=0.2, colour="grey") +
  theme_bw() +
  xlab("Object RT Norms") +
  ylab("Log Description Length (characters)") +
  theme(axis.title=element_text(size=20), axis.text=element_text(size=15)) 
```

![plot of chunk unnamed-chunk-30](figure/unnamed-chunk-30.png) 

```r
# reliable when control for random effects
```

***
***
<a name="geons"/>

## (4) Geons

<a name="4a"/>
### (A) Norms [(Complexity Task)][task34] [(RT task)][task35]
#### (1) Complexity Norms 


#### (2) RT Norms 


* Get correlation between between # geons and complexity rating, and RT and complexity rating

```r
# remove quotes from norms
cg_norms$obj <- as.factor(as.numeric(gsub("[[:punct:]]", "", cg_norms$obj)))
cg_norms$obj_class = as.numeric(substr(cg_norms$obj, 1, 1))
cg_norms$obj_item = as.numeric(substr(cg_norms$obj, 2, 2))

rg_norms$obj <- as.factor(as.numeric(gsub("[[:punct:]]", "", rg_norms$obj)))
rg_norms$obj_class = substr(rg_norms$obj, 1, 1)
rg_norms$obj_item = substr(rg_norms$obj, 2, 2)

index <- match(cg_norms$obj, rg_norms$obj)
cg_norms$rt_meanRating <- rg_norms$log.rt[index]

# correlation between num geons and complexity
cor.test(cg_norms$obj_class, cg_norms$meanRating)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  cg_norms$obj_class and cg_norms$meanRating
## t = 15.39, df = 38, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.8677 0.9617
## sample estimates:
##    cor 
## 0.9283
```

```r
# correlation between RT and complexity
cor.test(cg_norms$rt_meanRating, cg_norms$meanRating)
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  cg_norms$rt_meanRating and cg_norms$meanRating
## t = 12.14, df = 38, p-value = 1.199e-14
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  0.8033 0.9416
## sample estimates:
##    cor 
## 0.8917
```

<a name="4b"/> 
### (B) Mapping task  [(Task)][task37] 


* get effect sizes


* get obj conds


* plot

```r
# set graphical params
fs = 25
rs = 8

# complexity ratio
geon_c_plot = ggplot(de, aes(y=effect_size, x=c.Mratio)) +
  geom_pointrange( aes(ymax = cill, ymin=ciul))+
  geom_hline(yintercept=0,lty=2) +
  stat_smooth(method="lm") +
  #geom_text(aes(c.Mratio + .05, effect_size, label=objCondition2)) +
  ylab("effect size (Cohen's d)") +
  xlab("complexity rating ratio") + 
  theme(text = element_text(size=fs))  +
  scale_y_continuous(limits = c(-.33, .66)) +
  scale_x_continuous(limits = c(.25, 1.29)) +
  annotate("text", x=1.15, y=.5, color = "red", size = rs,
           label=paste("r=",round(cor(de$effect_size, de$c.Mratio), 2))) +
  theme(plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) 

# rt ratio
geon_rt_plot = ggplot(de, aes(y=effect_size, x=rt.Mratio)) +
  geom_pointrange( aes(ymax = cill, ymin=ciul))+
  geom_hline(yintercept=0,lty=2) +
  stat_smooth(method="lm") +
  #geom_text(aes(rt.Mratio + .0025, effect_size, label=objCondition2)) +
  ylab("effect size (Cohen's d)") +
  xlab("reaction time ratio") + 
  theme(text = element_text(size=fs)) +  
  scale_y_continuous(limits = c(-.33, .66)) +
  scale_x_continuous(limits = c(.949, 1.005)) +
  annotate("text", x=.997, y=.5, color = "red", size = rs,
           label=paste("r=",round(cor(de$effect_size, de$rt.Mratio), 2))) +
  theme(plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()) 

if (savePlots) {pdf("figure/geon.pdf", height = 6, width = 12)}
multiplot(geon_c_plot, geon_rt_plot, cols = 2)
```

```
## Warning: Removed 15 rows containing missing values (stat_smooth).
## Warning: Removed 15 rows containing missing values (geom_segment).
## Warning: Removed 15 rows containing missing values (geom_point).
## Warning: Removed 15 rows containing missing values (stat_smooth).
## Warning: Removed 15 rows containing missing values (geom_segment).
## Warning: Removed 15 rows containing missing values (geom_point).
```

```r
if (savePlots) {dev.off()}
```

```
## pdf 
##   2
```

* correlations between norms and effect sizes

```r
cor.test(de$rt.Mratio, de$effect_size)
```

```
## Error: not enough finite observations
```

```r
cor.test(de$c.Mratio, de$effect_size)
```

```
## Error: not enough finite observations
```

[task30]: http://tinyurl.com/qz59yuh
[task32]: http://tinyurl.com/keznp7v
[task9]:  http://langcog.stanford.edu/expts/MLL/refComplex/Experiment9/ref_complex_9.html
[task26]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment26/ref_complex_26.html
[task34]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment34/ref_complex_34.html
[task15]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment15/ref_complex_15.html
[task25]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment25/ref_complex_25.html
[task27]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment27/ref_complex_27.html
[task34]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment34/ref_complex_34.html
[task35]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment35/ref_complex_35.html
[task37]: http://langcog.stanford.edu/expts/MLL/refComplex/Experiment37/ref_complex_37.html
