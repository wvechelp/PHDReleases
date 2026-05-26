##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 03/02/2020; Last changes: 03/02/2020
###############Section 2: Packages########################################
# install.packages('party') # For cForest modelling
# install.packages('PresenceAbsence') # For confusion matrix and metrics
# install.packages('reshape2') # For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
# install.packages('gridExtra') # For plotting
# install.packages('ggpubr') # For plotting
# install.packages('future') # For parallel computing on HPC
###############Section 3: Libraries#######################################
library(party)
library(PresenceAbsence)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(parallel)
library(foreach)
library(doParallel)
library(future)
###############Section 4: Private functions###############################
f.OptDiscrThreshold <- function(data, lab = 1, obs, pred, plot = F, metric = 'MinSnSp'){
  df.data <- data[, c(lab, obs, pred)]
  v.threshold <- seq(0.01, 0.99, 0.01)
  df.temp <- as.data.frame(matrix(nrow = length(v.threshold), ncol = 7))
  names(df.temp) <- c('Threshold', 'Kappa', 'Sn', 'Sp', 'TSS', 'F1', 'MinSnSp')
  for (i in c(1:length(v.threshold))){
    confusion <- cmx(df.data, threshold = v.threshold[i])
    df.temp[i, 1] <- v.threshold[i]
    df.temp[i, 2] <- Kappa(confusion, st.dev = F)
    df.temp[i, 3] <- sensitivity(confusion, st.dev = F)
    df.temp[i, 4] <- specificity(confusion, st.dev = F)
    df.temp[i, 5] <- df.temp[i, 3] + df.temp[i, 4] - 1
    df.temp[i, 6] <- (2 * confusion[1, 1]) / 
      (2 * (confusion[1, 1]) + confusion[1, 2] + confusion[2, 1])
    df.temp[i, 7] <- 1 / abs(sensitivity(confusion, st.dev = F) - 
                               specificity(confusion, st.dev = F))
  }
  df.best <- df.temp[which.max(df.temp[, which(names(df.temp) == metric)]), ]
  if(plot){
    p <- ggplot(df.temp, aes(x = Threshold, y = df.temp[, which(names(df.temp) == metric)])) + 
      geom_line() + 
      ggtitle(paste0('Optimal threshold selection (', metric, ')')) +
      scale_y_continuous(paste0(metric, ' (-)')) + 
      theme_bw() + 
      theme(panel.grid = element_blank(), 
            axis.text = element_text(colour = 'black'))
    plot(p)
  }
  
  return(df.best[, c(1:6)])
}
f.Performance <- function(data, lab = 1, obs, pred, thresh = NULL, metric = 'MinSnSp'){
  v.perf <- as.data.frame(matrix(ncol = 7, nrow = 1))
  names(v.perf) <- c('AUC', 'Kappa', 'Sn', 'Sp', 'TSS', 'F1', 'R2')
  v.perf[1] <- auc(data[, c(lab, obs, pred)], st.dev = F)
  if (is.null(thresh)){ 
    thresh <- f.OptDiscrThreshold(data, lab = lab, obs = obs, pred = pred, 
                                  plot = F, metric = metric)$Threshold
  }
  confusion <- cmx(data[, c(lab, obs, pred)], threshold = thresh)
  v.perf[2] <- Kappa(confusion, st.dev = F)
  v.perf[3] <- sensitivity(confusion, st.dev = F)
  v.perf[4] <- specificity(confusion, st.dev = F)
  v.perf[5] <- v.perf[3] + v.perf[4] - 1
  v.perf[6] <- (2 * confusion[1, 1]) / 
    (2 * (confusion[1, 1]) + confusion[1, 2] + confusion[2, 1])
  v.perf[7] <- cor(data[, obs], ifelse(data[, pred] >= thresh, 1, 0))[1]
  
  return(list(v.perf, c('Threshold', thresh)))
}
f.RangeRemoval <- function(data, cols = c(1:ncol(data)), quant = 0.95, n_out = 0){
  # Define range table
  df.range.temp <- as.data.frame(matrix(nrow = length(cols), ncol = 3))
  names(df.range.temp) <- c('Variable', 'Q_low', 'Q_high')
  # Define temporary dataframes
  df.pres.temp <- data[which(data$Macrophyte == 1), ]
  df.abs.temp <- data[which(data$Macrophyte == 0), ]
  # Identify ranges of each variable
  for (i in c(1:nrow(df.range.temp))){
    df.range.temp[i, 1] <- names(df.pres.temp)[i + cols[1] - 1]
    df.range.temp[i, 2] <- quantile(df.pres.temp[, (cols[1] - 1 + i)], (1 - quant) / 2)
    df.range.temp[i, 3] <- quantile(df.pres.temp[, (cols[1] - 1 + i)], (quant + 1) / 2)
  }
  # Indicate if instance can be considered as Absence (= 1)
  df.abs.temp$PsA <- 0 #Initially, all instances within range
  for (i in c(1:nrow(df.abs.temp))){
    n_abs <- 0
    for (j in c(1:nrow(df.range.temp))){
      # Check for each variable if within range, else add 1 to the count of potential absence reason (n_abs)
      if (df.abs.temp[i, (cols[1] - 1 + j)] < df.range.temp$Q_low[j] | 
          df.abs.temp[i, (cols[1] - 1 + j)] > df.range.temp$Q_high[j]){
        n_abs <- n_abs + 1
        # Check if n_abs is higher than the threshold to qualify as 'True' absence, if so, change dataframe value and move to next row
        if (n_abs > n_out){
          df.abs.temp$PsA[i] <- 1
          break
        }
      }
    }
  }
  df.train <- rbind(df.pres.temp, df.abs.temp[which(df.abs.temp$PsA == 1), 
                                              c(1:(ncol(df.abs.temp) - 1))])
  
  return(df.train)
}
f.SummaryPerf <- function(data, plot = F){
  # Determine mean and sd per cross-validation
  df.mean <- df.sd <- c()
  for (i in c(1:max(data$n))){
    df.mean <- as.data.frame(rbind(df.mean, apply(data[data$n == i, ], 2, mean)))
    df.sd <- as.data.frame(rbind(df.sd, apply(data[data$n == i, ], 2, sd)))
  }
  # Melt together in general dataframe and add information on type
  df.cv.perf <- cbind(melt(df.mean, 
                           measure.vars = names(df.mean)[1:(ncol(df.mean) - 2)],
                           id.vars = 'n'), 
                      melt(df.sd, 
                           measure.vars = names(df.sd)[1:(ncol(df.sd) - 2)], 
                           id.vars = 'n')$value)
  names(df.cv.perf) <- c('n', 'Metric', 'Mean', 'sd')
  df.cv.perf$Type <- c('CV')
  # Use mean data to calculate mean and sd over repetitions
  df.rep.perf <- cbind.data.frame('1', names(df.mean)[c(1:(ncol(df.mean) - 2))], 
                                  apply(df.mean[c(1:(ncol(df.mean) - 2))], 2, mean), 
                                  apply(df.mean[c(1:(ncol(df.mean) - 2))], 2, sd), 'Rep')
  names(df.rep.perf) <- names(df.cv.perf)
  rownames(df.rep.perf) <- c()
  # Combine cross-validation and repetitions in 1 dataframe
  df.all <- rbind.data.frame(df.cv.perf, df.rep.perf)
  df.all <- df.all[, c('Metric', 'Type', 'n', 'Mean', 'sd')]
  if (plot){
    p.SumPerf <- ggplot() + 
      geom_hline(data = df.all[df.all$Type == 'Rep', ], 
                 aes(yintercept = Mean), colour = 'grey') +
      geom_hline(data = df.all[df.all$Type == 'Rep', ], 
                 aes(yintercept = Mean - sd), colour = 'grey', linetype = 'dotted') +
      geom_hline(data = df.all[df.all$Type == 'Rep', ], 
                 aes(yintercept = Mean + sd), colour = 'grey', linetype = 'dotted') +
      geom_point(data = df.all[df.all$Type=='CV', ], aes(n, Mean)) + 
      geom_pointrange(data=df.all[df.all$Type == 'CV', ], 
                      aes(n, Mean, ymin = Mean - sd, ymax = Mean + sd)) + 
      facet_grid(Metric~., scales = 'free_y', switch = 'y') + 
      theme_bw() + 
      theme(panel.grid = element_blank(), 
            axis.text = element_text(colour = 'black'))
    plot(p.SumPerf)
  }
  
  return(df.all)
}
f.PerfLongWide <- function(data, lab = 1, mean, sd){
  df.new <- c(data[, mean], data[, sd])
  names(df.new) <- c(as.character(data[, 1]), paste0('sd_', data[, 1]))
  
  return(df.new)
}
f.CForest <- function(data, cols = c(1:ncol(data)), n.ntree = 200, 
                      n.mtry = NULL, n.rep, n.cv = 5){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  # Open different clusters for n
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.out <- foreach(n = 1:n.rep, .packages = c('party', 'PresenceAbsence'), 
                    .export = c('f.Performance', 'f.OptDiscrThreshold')) %dopar% {
    lst.data <- list()
    t0 <- Sys.time()
    df.pres.base.t <- df.pres.base
    df.abs.base.t <- df.abs.base
    # Make different folds
    for (i in c(1:n.cv)){
      set.seed(n)
      s.pres <- sample(c(1:nrow(df.pres.base.t)), size = n.inst, replace = F)
      set.seed(n)
      s.abs <- sample(c(1:nrow(df.abs.base.t)), size = n.inst, replace = F)
      lst.data[[i]] <- rbind(df.pres.base.t[s.pres, ], df.abs.base.t[s.abs, ])
      df.pres.base.t <- df.pres.base.t[-s.pres, ]
      df.abs.base.t <- df.abs.base.t[-s.abs, ]
    }
    # Perform cross-validation
    df.perf.temp <- c()
    for (i in c(1:n.cv)){
      df.train <- c()
      for (j in c(1:length(lst.data))){
        if (i == j){
          df.test <- lst.data[[j]]
        } else {
          df.train <- rbind(df.train, lst.data[[j]])
        }
      }
      set.seed(n)
      if (is.null(n.mtry)){
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = floor(sqrt(length(cols)))))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, 
                            f.Performance(df.test, lab = 1, obs = (ncol(df.test) - 1), 
                                          pred = ncol(df.test))[[1]])
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    t1 <- Sys.time()
    list(df.perf.temp, round(as.numeric(difftime(t1, t0, units = 'secs')), 4))
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.temp <- v.time<-c()
  for (i in c(1:n.rep)){
    df.perf.temp <- rbind(df.perf.temp, df.out[[i]][[1]])
    v.time[i] <- df.out[[i]][[2]]
  }
  df.perf <- f.SummaryPerf(df.perf.temp)
  
  return(list(df.perf.temp, df.perf[df.perf$Type == 'Rep', c(1, 4, 5)], v.time))
}
f.CForestVI <- function(data, cols = c(1:ncol(data)), n.ntree = 200, 
                        n.mtry = NULL, n.rep, n.cv = 5){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  # Open different clusters for n -> To do
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.out <- foreach(n = 1:n.rep, .packages = c('party', 'PresenceAbsence'), 
                    .export = c('f.Performance', 'f.OptDiscrThreshold')) %dopar% {
    lst.data <- list()
    t0 <- Sys.time()
    df.pres.base.t <- df.pres.base
    df.abs.base.t <- df.abs.base
    # Make different folds
    for (i in c(1:n.cv)){
      set.seed(n)
      s.pres <- sample(c(1:nrow(df.pres.base.t)), size = n.inst, replace = F)
      set.seed(n)
      s.abs <- sample(c(1:nrow(df.abs.base.t)), size = n.inst, replace = F)
      lst.data[[i]] <- rbind(df.pres.base.t[s.pres, ], df.abs.base.t[s.abs, ])
      df.pres.base.t <- df.pres.base.t[-s.pres, ]
      df.abs.base.t <- df.abs.base.t[-s.abs, ]
    }
    # Perform cross-validation
    df.perf.temp <- df.vi.temp <- c()
    for (i in c(1:n.cv)){
      df.train <- c()
      for (j in c(1:length(lst.data))){
        if (i == j){
          df.test <- lst.data[[j]]
        } else {
          df.train <- rbind(df.train, lst.data[[j]])
        }
      }
      set.seed(n)
      if (is.null(n.mtry)){
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, f.Performance(df.test, lab = 1, 
                                                        obs = (ncol(df.test) - 1), 
                                                        pred = ncol(df.test))[[1]])
      # Extract variable importance
      df.vi.temp <- 
        rbind.data.frame(df.vi.temp, t(as.data.frame(varimp(cf, conditional = F, 
                                                            threshold = 0.99))))
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    df.vi.temp$k <- c(1:n.cv)
    df.vi.temp$n <- n
    row.names(df.vi.temp) <- c()
    t1 <- Sys.time()
    list(df.perf.temp, df.vi.temp, round(as.numeric(difftime(t1, t0, units = 'secs')), 4))
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.temp <- df.vi.temp <- v.time <- c()
  for (i in c(1:n.rep)){
    df.perf.temp <- rbind(df.perf.temp, df.out[[i]][[1]])
    df.vi.temp <- rbind(df.vi.temp, df.out[[i]][[2]])
    v.time[i] <- df.out[[i]][[3]]
  }
  df.perf <- f.SummaryPerf(df.perf.temp)
  df.vi <- f.SummaryPerf(df.vi.temp)
  
  return(list(df.perf[df.perf$Type == 'Rep', c(1, 4, 5)], 
              df.vi[df.vi$Type == 'Rep', c(1, 4, 5)], v.time))
}
###############Section 5: Defining global variables#######################
s.data <- '../Data/'
s.figs <- '../Figures/'
###############Section 6: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Baseline information (< 5 sec)######################
###############Section 1: Loading of dataset##############################
df.all <- read.table(paste0(s.data, 'D_C6S1_AllData_ImputedAndMerged.txt'), 
                     header = T, sep = ',')
df.all <- df.all[order(row.names(df.all)), ]
mf.index <- min(c(which('Absent' == df.all[1, ]), 
                  which('Present' == df.all[1, ])))
df.all[, c(mf.index:ncol(df.all))] <- 
  ifelse(df.all[, c(mf.index:ncol(df.all))] == 'Present', 1, 0)
df.all[, c(5:ncol(df.all))] <- apply(df.all[, c(5:ncol(df.all))], 2, as.numeric)
names(df.all)[mf.index:ncol(df.all)] <- 
  gsub('[.]', ' ', names(df.all)[mf.index:ncol(df.all)])

###############Section 2: Listing macrophytes#############################
mf.list <- names(df.all)[mf.index:ncol(df.all)]
mf.sel <- c('Phragmites australis', 'Lemna minor', 'Ceratophyllum demersum', 
            'Mentha aquatica', 'Lemna minuta')

###############Section 3: Creating training and test data#################
set.seed(621)
v.sample <- sample(c(1:nrow(df.all)), size = 0.1 * nrow(df.all), replace = F)
lst.all.train <- lst.all.test <- list()
for (i in c(1:length(mf.list))){
  lst.all.train[[i]] <- df.all[-v.sample, c(1:(mf.index - 1), 
                                            which(names(df.all) == mf.list[i]))]
  names(lst.all.train[[i]])[ncol(lst.all.train[[i]])] <- 'Macrophyte'
  lst.all.test[[i]] <- df.all[v.sample, c(1:(mf.index - 1), 
                                          which(names(df.all) == mf.list[i]))]
  names(lst.all.test[[i]])[ncol(lst.all.test[[i]])] <- 'Macrophyte'
  lst.all.test[[i]] <- lst.all.test[[i]][order(row.names(lst.all.test[[i]])), ]
}

###############Section 4: Variable removal################################
rm(i, v.sample)

##############Part 2: Baseline performance (30 mins)######################
###############Section 1: In- and output##################################
df.info <- data.frame(matrix(nrow = length(mf.list), ncol = 4))
colnames(df.info) <- c('Macrophyte', 'Series', 'Instances', 'Features')
df.perf <- data.frame(matrix(nrow = length(mf.list), ncol = 14))
df.time <- data.frame(matrix(nrow = length(mf.list), ncol = 3))
colnames(df.time) <- c('TimeM', 'sd_TimeM', 'TimeT')

###############Section 2: Modeling########################################
for (i in c(1:length(mf.list))){
  message(paste0('Baseline for ', mf.list[i]))
  df.train <- lst.all.train[[i]]
  # Step 1: Modelling
  t0 <- Sys.time()
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
  t1 <- Sys.time()
  timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeM <- round(mean(lst.temp[[3]]), 4)
  # Step 2: Store results
  df.info[i, ] <- c(mf.list[i], 'Baseline', nrow(df.train), ncol(df.train))
  df.perf[i, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  if(i == 1){ 
    names(df.perf) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  }
  df.time[i, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT)
}
df.perf.base <- cbind.data.frame(df.info, df.perf, df.time)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.plt <- df.perf.base[df.perf.base$Macrophyte %in% mf.sel, ]
  df.plt$Macrophyte <- factor(df.plt$Macrophyte, mf.sel)
  p.BaselinePerformance <- ggplot(df.plt, aes(x = Macrophyte, y = AUC)) + 
    geom_pointrange(aes(ymin = AUC - sd_AUC, ymax = AUC + sd_AUC), size = 0.1) + 
    scale_x_discrete('') + 
    scale_y_continuous('AUC (-)') +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.text.x = element_text(face = 'italic'), 
          axis.title = element_text(size = 9))
  plot(p.BaselinePerformance)
}
if(b.save){
  write.table(df.perf.base, paste0(s.data, 'D_SI_C6S4_PerformanceBaseline.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(i, p.BaselinePerformance)

##############Part 3: Complete pre-processing: OL-PsA-Corr-Imp (86 mins)##
###############Section 1: In- and output##################################
df.info <- data.frame(matrix(nrow = length(mf.list), ncol = 4))
colnames(df.info) <- c('Macrophyte', 'Series', 'Instances', 'Features')
df.perf <- data.frame(matrix(nrow = length(mf.list), ncol = 14))
df.time <- data.frame(matrix(nrow = length(mf.list), ncol = 3))
colnames(df.time) <- c('TimeM', 'sd_TimeM', 'TimeT')
t.ol <- 3; t.psa <- 0.05; t.cor <- 0.7; t.imp <- 0.10

###############Section 2: Pre-processing & modelling######################
for (i in c(1:length(mf.list))){
  message(paste0('Analysis for ', mf.list[i]))
  df.train <- lst.all.train[[i]]
  t0 <- Sys.time()
  # Step 1: Removal of outliers
  v.up <- apply(df.train[, c(5:24)], 2, 
                function(x) quantile(x, 0.75) + t.ol * (quantile(x, 0.75) - quantile(x, 0.25)))
  v.low <- apply(df.train[, c(5:24)], 2, 
                 function(x) max(0, quantile(x, 0.25) - t.ol * (quantile(x, 0.75) - quantile(x, 0.25))))
  df.train$OL <- 0
  for (j in c(1:ncol(df.train[, c(5:24)]))){
    for (k in c(1:nrow(df.train))){
      if(df.train[k, (j + 4)] > v.up[j] | df.train[k, (j + 4)] < v.low[j]){
        df.train$OL[k] <- df.train$OL[k] + 1
      }
    }
  }
  # Here, choice can be made which data is kept (0 -> each instance with outlier(s) removed)
  df.train <- df.train[which(df.train$OL == 0), -ncol(df.train)] 
  # Step 2: Removal of pseudoabsences
  df.train <- f.RangeRemoval(df.train, cols = c(5:24), quant = (1 - t.psa))
  # Step 3: Removal of correlated variables
  v.var <- names(df.train[, c(5:24)])
  corr <- 1
  while(corr > t.cor){
    # Calculate correlation and order
    df.corr <- c()
    for (j in c(1:(length(v.var) - 1))){
      df.cor.temp <- as.data.frame(matrix(nrow = (length(v.var) - j), ncol = 3))
      names(df.cor.temp) <- c('Var1', 'Var2', 'R2')
      v <- 1
      for (k in c((j + 1):length(v.var))){
        df.cor.temp$Var1[v] <- v.var[j]
        df.cor.temp$Var2[v] <- v.var[k]
        df.cor.temp$R2[v] <- cor(df.train[, which(names(df.train) == v.var[j])], 
                                 df.train[, which(names(df.train) == v.var[k])])
        v <- v + 1
      }
      df.corr <- rbind(df.corr, df.cor.temp)
    }
    df.corr <- df.corr[order(abs(df.corr$R2), decreasing = T),]
    if(corr > t.cor){
      cor.var1 <- cor(df.train[, which(names(df.train) == df.corr$Var1[1])], 
                      df.train$Macrophyte)
      cor.var2 <- cor(df.train[, which(names(df.train) == df.corr$Var2[1])], 
                      df.train$Macrophyte)
      if (abs(cor.var1) >= abs(cor.var2)){
        v.var <- v.var[-which(v.var == df.corr$Var2[1])]
      } else {
        v.var <- v.var[-which(v.var == df.corr$Var1[1])]
      }
    }
    corr <- max(df.corr$R2)
  }
  df.train <- df.train[, c(1:4, which(names(df.train) %in% v.var), ncol(df.train))]
  # Step 4: Removal of redundant variables
  df.vi.temp <- f.CForestVI(df.train, cols = c(5:ncol(df.train)), 
                            n.ntree = 200, n.rep = 10)[[2]]
  v.var <- v.var[order(df.vi.temp$Mean, decreasing = F)][which(sort(df.vi.temp$Mean) / max(df.vi.temp$Mean) > t.imp)]
  df.train <- df.train[, c(c(1:4), which(names(df.train) %in% v.var), ncol(df.train))]
  # Step 5: Modelling
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
  t1 <- Sys.time()
  timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeM <- round(mean(lst.temp[[3]]), 4)
  # Step 6: Store results
  df.info[i, ] <- c(mf.list[i], 'Processed', nrow(df.train), ncol(df.train))
  df.perf[i, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  if(i == 1){ 
    names(df.perf) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  }
  df.time[i, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT)
}
df.perf <- cbind.data.frame(df.info, df.perf, df.time)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.plt <- df.perf[df.perf$Macrophyte %in% mf.sel, ]
  df.plt$Macrophyte <- factor(df.plt$Macrophyte, mf.sel)
  p.ProcessedPerformance <- ggplot(df.plt, aes(x = Macrophyte, y = AUC)) + 
    geom_pointrange(aes(ymin = AUC - sd_AUC, ymax = AUC + sd_AUC), size = 0.1) + 
    scale_x_discrete('') + 
    scale_y_continuous('AUC (-)') +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.text.x = element_text(face = 'italic'),
          axis.title = element_text(size = 9))
  plot(p.ProcessedPerformance)
}

if(b.save){
  write.table(df.perf, paste0(s.data, 'D_SI_C6S4_PerformanceProcessed.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(i, df.train, corr, df.corr, j, df.cor.temp, v, k, df.vi.temp, cor.var1, 
   cor.var2, v.var, p.ProcessedPerformance)

##############Part 4: Overview############################################
###############Section 1: In- and output##################################
df.base <- read.table(paste0(s.data, 'D_SI_C6S4_PerformanceBaseline.txt'), 
                      header = T, sep = ',')
df.prep <- read.table(paste0(s.data, 'D_SI_C6S4_PerformanceProcessed.txt'), 
                      header = T, sep = ',')

###############Section 2: Plotting and saving 5 macrophytes###############
if(b.plot){
  df.all <- rbind(df.base, df.prep)
  df.all <- df.all[which(df.all$Macrophyte %in% mf.sel), ]
  df.all$Macrophyte <- factor(df.all$Macrophyte, rev(mf.sel))
  df.all$Series <- factor(df.all$Series, levels = c('Processed', 'Baseline'))
  p.EffectProcessing <- ggplot(df.all, aes(x = Macrophyte, y = AUC, fill = Series)) + 
    geom_col(color = 'black', position = position_dodge(), width = 0.5) + 
    geom_errorbar(aes(ymin = AUC - sqrt(10) * sd_AUC, ymax = AUC + sqrt(10) * sd_AUC), 
                  width = 0.2, position = position_dodge(0.5)) + 
    scale_fill_manual(values = c('grey40', 'grey90'), guide = guide_legend(reverse = T)) +
    scale_x_discrete('') + 
    scale_y_continuous('AUC (-)') + 
    coord_flip() + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.text.y = element_text(face = 'italic'), 
          axis.title = element_text(size = 9), 
          legend.text = element_text(size = 7),
          legend.title = element_blank())
  plot(p.EffectProcessing)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S4_PerformanceComparison.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.EffectProcessing)
    dev.off()
  }
}

###############Section 3: Plotting and saving all macrophytes#############
if(b.plot){
  df.all <- rbind(df.base, df.prep)
  df.all$Macrophyte <- factor(df.all$Macrophyte, mf.list)
  # Performance
  df.auc <- cbind.data.frame(df.all$Macrophyte[c(1:length(mf.list))], 
                             df.all$AUC[which(df.all$Series == 'Baseline')], 
                             df.all$AUC[which(df.all$Series == 'Processed')])
  names(df.auc) <- c('Macrophyte', 'Original', 'Processed')
  p.ComparisonAUC <- ggplot(df.auc,aes(x = Original, y = Processed)) + 
    geom_point(size = 0.2) + 
    geom_abline(slope = 1, intercept = 0) + 
    scale_x_continuous('Baseline AUC (-)', limits = c(0.5, 1)) + 
    scale_y_continuous('AUC after processing (-)', limits = c(0.5, 1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 7))
  plot(p.ComparisonAUC)
  
  # Time
  df.time <- cbind.data.frame(df.all$Macrophyte[c(1:length(mf.list))], 
                              df.all$TimeT[which(df.all$Series == 'Baseline')], 
                              df.all$TimeT[which(df.all$Series == 'Processed')])
  names(df.time) <- c('Macrophyte', 'Original', 'Processed')
  p.ComparisonTime <- ggplot(df.time, aes(x = Original, y = Processed)) + 
    geom_point(size = 0.2) + 
    geom_abline(slope = 1, intercept = 0) + 
    scale_x_continuous('Baseline Time (s)', limits = c(0, max(df.time[, 2:3]))) + 
    scale_y_continuous('Time including processing (s)', limits = c(0, max(df.time[, 2:3]))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 7))
  plot(p.ComparisonTime)
  
  # Tradeoff
  df.plot <- cbind.data.frame(df.auc$Macrophyte, 
                              (df.auc$Processed - df.auc$Original) / df.auc$Original, 
                              (df.time$Processed - df.time$Original) / df.time$Original)
  names(df.plot) <- c('Macrophyte', 'AUC', 'Time')
  p.Tradeoff <- ggplot(df.plot, aes(x = Time, y = AUC)) + 
    geom_point(size = 0.2) + 
    geom_abline(slope = 1, intercept = 0) + 
    scale_x_continuous('Relative change in time (-)', 
                       limits = c(0, max(df.plot$Time))) + 
    scale_y_continuous('Relative change in AUC (-)', 
                       limits = c(0, max(df.plot$Time))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 7))
  plot(p.Tradeoff)
  
  # Combine
  p.combo <- ggarrange(p.ComparisonAUC, p.ComparisonTime, p.Tradeoff, 
                       nrow = 1, ncol = 3, labels = 'AUTO', label.x = 0.2, 
                       label.y = 0.95, font.label = list(size = 7, face = 'plain'))
  plot(p.combo)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S4_TradeoffPreprocessingPerformance.tiff'), 
         units = 'mm', width = 160, height = 50, res = 300, pointsize = 7)
    plot(p.combo)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.base, df.prep)
