# Helper functions for refComplex analysis

# bootstrap CIs for correlations (http://dodonovs.com/R/002-r.htm)
boot.cor = function(x, y, n=5000, p=0.95, method="pearson"){ 
  w = length(x); x.r = x; y.r = y 
  sm = 1:w; cor.b = 1:n 
  for (k in 1:n){ 
    s = sample(sm, w, replace = T) 
    for (i in 1:w) 
    { 
      x.r[i] = x[s[i]] 
      y.r[i] = y[s[i]] 
    } 
    cor.b[k] = cor(x.r, y.r, use = "pairwise", method) 
  } 
  cor.b = sort(cor.b); a = round(n*(1-p)/2,0); b = round(n*(p+1)/2,0) 
  vec = c(cor.b[a+1], cor(x, y, use = "pairwise", method), cor.b[b-1]) 
  n.r = c("value"); n.c = c("lower_bound", "correlation", "upper_bound") 
  matrix(vec,1,3,dimnames = list(n.r,n.c)) 
}

# proportions and CIs for forced choice tasks
p.fc <- function(d, dv){ 
  # get proportions
  complex_proport_c = sum(d$responseValue==dv)/length(d$responseValue)
  
  # get bootstrapped 95% CIs
  if (!is.na(complex_proport_c)) { 
    # bootstrap across subjects proportion responses for each category
    b <- boot(d$responseValue, function(u, i) table(u[i])[dv]/length(u), R = 1000) 
    ci <- boot.ci(b, type = "basic")  
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
                     n = NA)
    
  }
  return (es)
}

# effect sizes for forced choice tasks
d.fc <- function(d) {
  d <- d[d$langCondition == "\"long\"" | d$langCondition ==  "\"short\"",]
  d <- droplevels(d)
  
  # use odds ratio to calculate d
  ns = table(d$langCondition, d$responseValue)
  or = (ns["\"long\"", "\"complex\""] * ns["\"short\"", "\"simple\""]) / 
    (ns["\"short\"", "\"complex\""] * ns["\"long\"", "\"simple\""])
  cf = sqrt(3)/pi
  effect_size = log(or) * cf # calculate d
  
  # calculate 95 CI
  ## parametrically
  var_lor = (1/ns[1]) + (1/ns[2]) + (1/ns[3]) + (1/ns[4]) # sampling variance of lor
  var_d  = (3/(pi^2)) * var_lor # sampling variance of d
  d_err = sqrt(var_d) * 1.96
  
  cill = effect_size - d_err
  ciul = effect_size + d_err
  rt.Mratio = mean(d$rt.ratio, na.rm = T)
  c.Mratio = mean(d$c.ratio, na.rm = T)
  
  es <- data.frame(effect_size=effect_size,
                   cill = cill,
                   ciul = ciul,
                   rt.Mratio = rt.Mratio,
                   c.Mratio = c.Mratio)
  return (es)
}

theme_set(theme_bw())
themeML = theme(text = element_text(size=fs),
                plot.title=element_text(size=ts, face = "bold"),
                plot.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.line = element_line(color = 'black')) 


## for bootstrapping 95% confidence intervals
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  as.numeric(mean(x,na.rm=na.rm) - quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm))}
ci.high <- function(x,na.rm=T) {
  as.numeric(quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm) - mean(x,na.rm=na.rm))}

## Multiple plot function
# note, from internet. 
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

pcor.test <- function(x,y,z,use="mat",method="p",na.rm=T){
  # The partial correlation coefficient between x and y given z
  #
  # pcor.test is free and comes with ABSOLUTELY NO WARRANTY.
  #
  # x and y should be vectors
  #
  # z can be either a vector or a matrix
  #
  # use: There are two methods to calculate the partial correlation coefficient.
  #	 One is by using variance-covariance matrix ("mat") and the other is by using recursive formula ("rec").
  #	 Default is "mat".
  #
  # method: There are three ways to calculate the correlation coefficient, 
  #	    which are Pearson's ("p"), Spearman's ("s"), and Kendall's ("k") methods.
  # 	    The last two methods which are Spearman's and Kendall's coefficient are based on the non-parametric analysis.
  #	    Default is "p".
  #
  # na.rm: If na.rm is T, then all the missing samples are deleted from the whole dataset, which is (x,y,z).
  #        If not, the missing samples will be removed just when the correlation coefficient is calculated.
  #	   However, the number of samples for the p-value is the number of samples after removing 
  #	   all the missing samples from the whole dataset.
  #	   Default is "T".
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  if(use == "mat"){
    p.use <- "Var-Cov matrix"
    pcor = pcor.mat(x,y,z,method=method,na.rm=na.rm)
  }else if(use == "rec"){
    p.use <- "Recursive formula"
    pcor = pcor.rec(x,y,z,method=method,na.rm=na.rm)
  }else{
    stop("\'use\' should be either \"rec\" or \"mat\"!\n")
  }
  
  # print the method
  if(gregexpr("p",method)[[1]][1] == 1){
    p.method <- "Pearson"
  }else if(gregexpr("s",method)[[1]][1] == 1){
    p.method <- "Spearman"
  }else if(gregexpr("k",method)[[1]][1] == 1){
    p.method <- "Kendall"
  }else{
    stop("\'method\' should be \"pearson\" or \"spearman\" or \"kendall\"!\n")
  }
  
  # sample number
  n <- dim(na.omit(data.frame(x,y,z)))[1]
  
  # given variables' number
  gn <- dim(z)[2]
  
  # p-value
  if(p.method == "Kendall"){
    statistic <- pcor/sqrt(2*(2*(n-gn)+5)/(9*(n-gn)*(n-1-gn)))
    p.value <- 2*pnorm(-abs(statistic))
    
  }else{
    statistic <- pcor*sqrt((n-2-gn)/(1-pcor^2))
    p.value <- 2*pnorm(-abs(statistic))
  }
  
  data.frame(estimate=pcor,p.value=p.value,statistic=statistic,n=n,gn=gn,Method=p.method,Use=p.use)
}	

# By using var-cov matrix
pcor.mat <- function(x,y,z,method="p",na.rm=T){
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  if(dim(z)[2] == 0){
    stop("There should be given data\n")
  }
  
  data <- data.frame(x,y,z)
  
  if(na.rm == T){
    data = na.omit(data)
  }
  
  xdata <- na.omit(data.frame(data[,c(1,2)]))
  Sxx <- cov(xdata,xdata,m=method)
  
  xzdata <- na.omit(data)
  xdata <- data.frame(xzdata[,c(1,2)])
  zdata <- data.frame(xzdata[,-c(1,2)])
  Sxz <- cov(xdata,zdata,m=method)
  
  zdata <- na.omit(data.frame(data[,-c(1,2)]))
  Szz <- cov(zdata,zdata,m=method)
  
  # is Szz positive definite?
  zz.ev <- eigen(Szz)$values
  if(min(zz.ev)[1]<0){
    stop("\'Szz\' is not positive definite!\n")
  }
  
  # partial correlation
  Sxx.z <- Sxx - Sxz %*% solve(Szz) %*% t(Sxz)
  
  rxx.z <- cov2cor(Sxx.z)[1,2]
  
  rxx.z
}

# By using recursive formula
pcor.rec <- function(x,y,z,method="p",na.rm=T){
  # 
  
  x <- c(x)
  y <- c(y)
  z <- as.data.frame(z)
  
  if(dim(z)[2] == 0){
    stop("There should be given data\n")
  }
  
  data <- data.frame(x,y,z)
  
  if(na.rm == T){
    data = na.omit(data)
  }
  
  # recursive formula
  if(dim(z)[2] == 1){
    tdata <- na.omit(data.frame(data[,1],data[,2]))
    rxy <- cor(tdata[,1],tdata[,2],m=method)
    
    tdata <- na.omit(data.frame(data[,1],data[,-c(1,2)]))
    rxz <- cor(tdata[,1],tdata[,2],m=method)
    
    tdata <- na.omit(data.frame(data[,2],data[,-c(1,2)]))
    ryz <- cor(tdata[,1],tdata[,2],m=method)
    
    rxy.z <- (rxy - rxz*ryz)/( sqrt(1-rxz^2)*sqrt(1-ryz^2) )
    
    return(rxy.z)
  }else{
    x <- c(data[,1])
    y <- c(data[,2])
    z0 <- c(data[,3])
    zc <- as.data.frame(data[,-c(1,2,3)])
    
    rxy.zc <- pcor.rec(x,y,zc,method=method,na.rm=na.rm)
    rxz0.zc <- pcor.rec(x,z0,zc,method=method,na.rm=na.rm)
    ryz0.zc <- pcor.rec(y,z0,zc,method=method,na.rm=na.rm)
    
    rxy.z <- (rxy.zc - rxz0.zc*ryz0.zc)/( sqrt(1-rxz0.zc^2)*sqrt(1-ryz0.zc^2) )
    return(rxy.z)
  }
}

# anonymize subject ids by giving them a value 1:num_subjects
anonymize.sids <- function(df, subject_column_label) {
  subj_col = which(names(df) == subject_column_label) # get workerid column index
  temp <- data.frame(workerid = unique(df[,subj_col])) # make new df of unique workerids
  temp$subid <- 1:length(unique(df[,subj_col])) # make list of subids
  index <- match(df[,subj_col], temp$workerid) 
  df$subids <- temp$subid[index]
  df[,subj_col] <- NULL 
  df$subids  = as.factor(df$subids)
  return(df)
}

