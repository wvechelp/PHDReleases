##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
#Remarks: Last successful run: 20/12/2019; Last changes: 23/11/2019
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
# install.packages('Hmisc') # For mean imputation
# install.packages('VIM') # For kNN imputation
# install.packages('reshape2') # For melting tables, a.o.
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(missForest)
library(Hmisc)
library(VIM)
library(reshape2)
library(ggplot2)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
v.fVar <- c(1.50, 1.00, 0.50) # To limit potential correlations within the data, affecting imputation
v.fObs <- c(1.0, 0.75, 0.50, 0.25) # Decreases number of observations, which affects power and techniques
v.fNA <- c(0.01, 0.05, 0.10, 0.20, 0.50, 0.75) # Fraction of missing data to be created
n.Nrep <- 10 # Repetitions for creating missing data
v.met <- c('mean', 'ls', 'kNN', 'mF') # Vector with methods
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Baseline data (30 secs)#############################
###############Section 1: In- and output##################################
lst.base <- list()
for (i in c(1:length(v.fVar))){ 
  lst.base[[i]] <- read.table(paste0(s.data, 'D_C5S1_BaselineData_', 
                                     round(100 * v.fVar[i], 0), '.txt'),
                              header = T, sep = ',') 
}
df.info <- read.table(paste0(s.data, 'D_C5S1_BaselineDataInfo.txt'), 
                      header = T, sep = ',')

###############Section 2: Selecting number of observations################
lst.base.ext <- list()
w <- 1
for (i in c(1:length(v.fVar))){
  for (j in c(1:length(v.fObs))){
    set.seed(621) # Seed to make results reproducible
    lst.base.ext[[w]] <- lst.base[[i]][sample(c(1:nrow(lst.base[[i]])), 
                                              floor(v.fObs[j] * nrow(lst.base[[i]])),
                                              replace = F), ]
    lst.base.ext[[w]] <- lst.base.ext[[w]][order(row.names(lst.base.ext[[w]])), ]
    w <- w + 1
  }
}

if(length(v.fVar) * length(v.fObs) != length(lst.base.ext)){
  message('Incorrect number of data sets - Check codes!')
}

###############Section 3: Creating artificially missing data##############
lst.data <- list()
w <- 1
for (i in c(1:length(lst.base.ext))){
  for (j in c(1:length(v.fNA))){
    for (k in c(1:n.Nrep)){
      set.seed(k) # Seed to make results reproducible, semi-fixed (otherwise exact replicates)
      lst.data[[w]] <- cbind(lst.base.ext[[i]][, c(1, 2)],
                             prodNA(lst.base.ext[[i]][, c(3:ncol(lst.base.ext[[i]]))],
                                    noNA = v.fNA[j]))
      w <- w + 1
    }
  }
}

###############Section 4: Variable removal################################
rm(lst.base, w, i, j, k)

##############Part 2: Comparison of imputation methods (...)###########
###############Section 1: In- and output##################################
v.sel <- c(183, 428, 713)
lst.orig <- lst.mis <- lst.avg <- lst.ls <- lst.kNN <- lst.mF <- list()
w <- 1
for (i in v.sel){
  lst.orig[[w]] <- melt(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]][, c(3:ncol(lst.base.ext[[ceiling(i / (n.Nrep * length(v.fNA)))]]))])
  lst.orig[[w]]$Series <- 'Data'
  lst.orig[[w]]$Method <- 'Original'
  lst.mis[[w]] <- melt(lst.data[[i]][, c(3:ncol(lst.data[[i]]))])
  lst.mis[[w]]$Series <- 'Data'
  lst.mis[[w]]$Method <- 'Missing'
  w <- w + 1
}

###############Section 2: Imputation######################################
################Imputation via mean (10 secs/run)#########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation ', w, ' of ', length(v.sel)))
  df.imp.avg <- df.data <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Imputation
  for (j in c(1:ncol(df.data))){
    df.imp.avg[, j] <- impute(df.data[, j], mean)
  }
  ##############Subsection: NRMSE
  lst.avg[[w]] <- melt(df.imp.avg)
  lst.avg[[w]]$Series <- 'Method'
  lst.avg[[w]]$Method <- 'mean'
  w <- w + 1
}

################Imputation via ls (LSImpute) (6 secs/run)#################
#Remark: check Bo (2004) for more information
w<-1
for (i in v.sel){ #Adapt to (i in sel)
  print(paste('Imputation ',w,' of ',length(v.sel),sep = ''))
  df.data<-lst.data[[i]][,c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Start with mean
  df.temp<-df.data
  for (j in c(1:ncol(df.data))){
    df.temp[,j]<-impute(df.temp[,j],mean)
  }
  ##############Subsection: Iterative imputation
  v <- 2; max.e <- c(1, 0)
  while(max.e[v] / max.e[(v - 1)] < 0.99 & v < 11){ # Thresholds are subjective...
    if(max.e[2] == 0){ v <- 1 }
    print(paste0('Iteration ', v))
    v.avg <- round(apply(df.temp, 2, mean), 2)
    v.sd <- round(apply(df.temp, 2, sd), 2)
    m.cov <- cov(df.temp)
    df.imp.ls <- df.temp
    for (j in c(1:ncol(df.imp.ls))){
      # Determine matrix product of covariances for each variable/ 'solve' is for inverse
      cov.scale <- (m.cov[j, -j]) %*% solve(m.cov[-j, -j]) 
      for (k in c(1:nrow(df.imp.ls))){
        if(is.na(df.data[k, j])){
          df.imp.ls[k, j] <- v.avg[j] + cov.scale %*% t(df.temp[k, -j] - v.avg[-j])
        }
      }
    }
    max.e[v + 1] <- max(abs(cov(scale(df.imp.ls, center = T, scale = T)) - 
                              cov(scale(df.temp, center = T, scale = T))))
    df.temp <- df.imp.ls
    v <- v + 1
  }
  ##############Subsection: NRMSE
  lst.ls[[w]] <- melt(df.imp.ls)
  lst.ls[[w]]$Series <- 'Method'
  lst.ls[[w]]$Method <- 'ls'
  w <- w + 1
}

################Imputation via kNN (20 secs/run)##########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Scaling of continuous, dummy scores for categorical
  v.avg <- round(apply(df.chem, 2, function(x) mean(x, na.rm = T)), 2)
  v.sd <- round(apply(df.chem, 2, function(x) sd(x, na.rm = T)), 2)
  df.scale <- as.data.frame(scale(df.chem, center = T, scale = T))
  ##############Subsection: Imputation
  df.imp.temp <- as.data.frame(kNN(df.scale, variable = colnames(df.scale), 
                                   numFun = mean, imp_var = F))
  row.names(df.imp.temp) <- row.names(df.scale)
  ##############Subsection: Re-scaling
  df.imp.kNN <- df.chem
  for (j in c(1:ncol(df.chem))){
    for (k in c(1:nrow(df.chem))){
      if(is.na(df.imp.kNN[k, j])){
        df.imp.kNN[k, j] <- (df.imp.temp[k, j] * v.sd[j]) + v.avg[j]        
      }
    }
  }
  ##############Subsection: NRMSE
  lst.kNN[[w]] <- melt(df.imp.kNN)
  lst.kNN[[w]]$Series <- 'Method'
  lst.kNN[[w]]$Method <- 'kNN'
  w <- w + 1
}

################Imputation via mF (90 secs/run)###########################
w <- 1
for (i in v.sel){
  print(paste0('Imputation for dataset ', i))
  df.chem <- lst.data[[i]][, c(3:ncol(lst.data[[i]]))]
  ##############Subsection: Imputation
  set.seed(621) # Seed to make results reproducible
  df.imp.mF <- missForest(df.chem, replace = F)$ximp # missForest creates more output, only imputed part is extracted
  ##############Subsection: NRMSE
  lst.mF[[w]] <- melt(df.imp.mF)
  lst.mF[[w]]$Series <- 'Method'
  lst.mF[[w]]$Method <- 'mF'
  w <- w + 1
}

###############Section 3: Plotting and saving#############################
df183 <- rbind.data.frame(lst.orig[[1]], lst.mis[[1]], lst.avg[[1]], 
                          lst.ls[[1]], lst.kNN[[1]], lst.mF[[1]])
v.na <- is.na(lst.mis[[1]]$value)
df183 <- rbind.data.frame(lst.orig[[1]][v.na, ], lst.mis[[1]][v.na, ], 
                          lst.avg[[1]][v.na, ], lst.ls[[1]][v.na, ], 
                          lst.kNN[[1]][v.na, ], lst.mF[[1]][v.na, ])
p.183 <- ggplot(df183[which(df183$variable %in% levels(df183$variable)[6:10]), ], 
                aes(x = Method, y = value)) + 
  geom_boxplot() + 
  facet_grid(variable~Series, scales = 'free', switch = 'y') + 
  theme_bw() + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = 'black', size = 7))
plot(p.183)

###############Section 4: Variable removal################################
rm(v.sel, v.nrmse.avg, v.nrmse.ls, v.nrmse.kNN, v.nrmse.mF, v.time.avg, 
   v.time.ls, v.time.kNN, v.time.mF, w,n.time0, n.time1, df.imp.avg, 
   df.data, i, j, df.orig, df.avg, df.temp, v, max.e, v.avg, v.sd, m.cov, 
   cov.scale, k, df.ls, df.chem, df.scale, df.kNN, df.mF)
