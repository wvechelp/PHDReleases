##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last changes: 17/11/2019; Last successful run: 17/11/2019
###############Section 2: Packages########################################
# install.packages('missForest') # To create artificially missing data
###############Section 3: Libraries#######################################
library(missForest)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
v.fVar <- c(1.50, 1.00, 0.50) # To limit potential correlations within the data, affecting imputation
v.fObs <- c(1.0, 0.75, 0.50, 0.25) # Decreases number of observations, which affects power and techniques
v.fNA <- c(0.01, 0.05, 0.10, 0.20, 0.50, 0.75) # Fraction of missing data to be created
v.Nrep <- 10 # Repetitions for creating missing data
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Baseline data selection (30 mins)###################
###############Section 1: In- and output##################################
df.data <- read.table(paste0(s.data, 'D_C5_AllChemicalData_clean.txt'),
                      header = T, sep = ',')

###############Section 2: Selecting number of variables###################
v.obs <- v.var <- c()
v.obs.sel <- v.var.sel <- c()
lst.range <- lst.sel <- lst.base <- list()
lst.range[[1]] <- seq(0, nrow(df.data), 500)
w <- v <- 1
# Dynamic iteration to determine starting point for number of variables
for (h in c(1, 2)){ 
  for (i in lst.range[[h]]){
    # Variable selection with more than i observations
    df.temp <- df.data[, which(as.data.frame(apply(df.data, 2, function(x) (sum(!is.na(x))))) > i)] 
    df.comp <- df.temp[complete.cases(df.temp), ] 
    v.obs[w] <- nrow(df.comp); v.var[w] <- ncol(df.comp) - 2
    # Record whenever a new 'complete-case dataset' is created, and store it
    if(w > 1){ 
      if(v.obs[w] != v.obs[w-1]){
        v.obs.sel[v] <- v.obs[w]; v.var.sel[v] <- v.var[w]
        lst.sel[[v]] <- df.comp
        v <- v + 1
      }
    }
    w <- w + 1
  }
  if(h == 1){
    n.max <- lst.range[[1]][which(v.var * v.obs == max(v.var * v.obs))[1]]
    # Construct new range around max information, for second interation
    lst.range[[2]] <- seq(n.max - 500, n.max + 500, 5) 
  }
}

# Selects number of variables with most complete data set
n.var.max <- v.var.sel[which(v.var.sel * v.obs.sel == max(v.var.sel * v.obs.sel))[1]] 

###############Section 3: Creating baseline data sets (3)#################
for (i in c(1:length(v.fVar))){ 
  # Define index of data set with necessary number of variables
  index <- which(v.var.sel - round(v.fVar * n.var.max, 0)[i] == 
                 min(abs(v.var.sel - round(v.fVar * n.var.max, 0)[i])))[1] 
  lst.base[[i]] <- lst.sel[[index]]
}

###############Section 4: Plotting and saving#############################
if(b.save){
  for (i in c(1:length(lst.base))){ 
    write.table(lst.base[[i]], sep = ',', row.names = F, 
                file = paste0(s.data, 'D_C5S1_BaselineData_', round(100 * v.fVar[i], 0), '.txt'))
  }
}

###############Section 5: Variable removal################################
rm(df.data, v.obs, v.var, v.obs.sel, v.var.sel, lst.range, lst.sel, w, v, h, i, 
   df.temp, df.comp, n.max, n.var.max, index)

##############Part 2: Creating data set extensions (15 secs)##############
###############Section 1: In- and output##################################
# lst.base <- list()
# for (i in c(1:length(v.fVar))){ lst.base[[i]] <- read.table(paste0('D_C5S1_BaselineData_', round(100 * v.fVar[i], 0), '.txt'), header = TRUE, sep = ',') }
###############Section 2: Selecting number of observations################
lst.base.ext <- list()
w <- 1
for (i in c(1:length(v.fVar))){
  for (j in c(1:length(v.fObs))){
    set.seed(621) # Seed to make results reproducible
    lst.base.ext[[w]] <- lst.base[[i]][sample(c(1:nrow(lst.base[[i]])), 
                                              floor(v.fObs[j] * nrow(lst.base[[i]])), 
                                              replace = FALSE), ]
    w <- w + 1
  }
}

if(length(v.fVar) * length(v.fObs) != length(lst.base.ext)){
  message('Incorrect number of data sets - Check codes!')
}

###############Section 3: Creating artificially missing data##############
lst.base.NA <- list()
df.info <- data.frame(matrix(nrow = length(lst.base.ext) * length(v.fNA) * v.Nrep, ncol = 5))
names(df.info) <- c('MD', 'Var', 'Obs', 'NVar', 'NObs')
w <- 1
for (i in c(1:length(lst.base.ext))){
  for (j in c(1:length(v.fNA))){
    for (k in c(1:v.Nrep)){
      set.seed(k) # Seed to make results reproducible, semi-fixed (otherwise exact replicates)
      lst.base.NA[[w]] <- cbind(lst.base.ext[[i]][, c(1, 2)], 
                                prodNA(lst.base.ext[[i]][, c(3:ncol(lst.base.ext[[i]]))], 
                                       noNA = v.fNA[j]))
      df.info$MD[w] <- v.fNA[j]
      df.info$Var[w] <- v.fVar[ceiling(i / 4)]
      df.info$Obs[w] <- v.fObs[((i - 1) %% 4 + 1)]
      df.info$NVar[w] <- (ncol(lst.base.ext[[i]]) - 2)
      df.info$NObs[w] <- nrow(lst.base.ext[[i]])
      w <- w + 1
    }
  }
}

###############Section 4: Plotting and saving#############################
if(b.save){
  write.table(df.info,file = paste0(s.data, 'D_C5S1_BaselineDataInfo.txt'), 
              sep = ',', row.names = F)  
}

###############Section 5: Variable removal################################
rm(lst.base, lst.base.ext, w, i, j, lst.base.NA, df.info, k)
