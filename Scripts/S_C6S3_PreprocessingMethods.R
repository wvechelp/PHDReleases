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
  for (i in c(1:length(v.threshold))) {
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
  if (is.null(thresh)) { 
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
  df.abs.temp$PsA <- 0 # Initially, all instances within range
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
  df.cv.perf <- cbind(melt(df.mean, measure.vars = names(df.mean)[1:(ncol(df.mean) - 2)], 
                           id.vars = 'n'), melt(df.sd,measure.vars = names(df.sd)[1:(ncol(df.sd) - 2)],
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
      geom_hline(data = df.all[df.all$Type == 'Rep', ], aes(yintercept = Mean), colour = 'grey') +
      geom_hline(data = df.all[df.all$Type == 'Rep', ], aes(yintercept = Mean - sd),
                 colour = 'grey', linetype = 'dotted') +
      geom_hline(data = df.all[df.all$Type == 'Rep', ], aes(yintercept = Mean + sd), 
                 colour = 'grey', linetype = 'dotted') +
      geom_point(data = df.all[df.all$Type == 'CV', ], aes(n, Mean)) + 
      geom_pointrange(data = df.all[df.all$Type == 'CV', ], 
                      aes(n, Mean, ymin = Mean - sd, ymax = Mean + sd)) + 
      facet_grid(Metric~., scales = 'free_y', switch = 'y') + 
      theme_bw() + 
      theme(panel.grid = element_blank(), 
            axis.text = element_text(colour = 'black'))
    plot(p.SumPerf)
  }
  
  return(df.all)
}
f.PerfLongWide<-function(data, lab = 1, mean, sd){
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
                      controls = cforest_unbiased(ntree = n.ntree, 
                                                  mtry = floor(sqrt(length(cols)))))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, f.Performance(df.test, lab = 1, 
                                                        obs = (ncol(df.test) - 1), 
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
      df.vi.temp <- rbind.data.frame(df.vi.temp, t(as.data.frame(varimp(cf, conditional = F, threshold = 0.99))))
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
  df.perf.temp <- df.vi.temp <- v.time<-c()
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
df.all[, c(5:ncol(df.all))] <- 
  apply(df.all[, c(5:ncol(df.all))], 2,as.numeric)
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
  lst.all.train[[i]] <- 
    df.all[-v.sample, c(1:(mf.index-1), which(names(df.all) == mf.list[i]))]
  names(lst.all.train[[i]])[ncol(lst.all.train[[i]])] <- 'Macrophyte'
  lst.all.test[[i]] <- 
    df.all[v.sample, c(1:(mf.index - 1), which(names(df.all) == mf.list[i]))]
  names(lst.all.test[[i]])[ncol(lst.all.test[[i]])] <- 'Macrophyte'
  lst.all.test[[i]] <- lst.all.test[[i]][order(row.names(lst.all.test[[i]])), ]
}

###############Section 4: Variable removal################################
rm(i, v.sample)

##############Part 2: Outlier removal (9 hours)###########################
###############Section 1: In- and output##################################
lst.inst.rem.perf <- list() 
v.OL <- rev(seq(0, 15, 1)) # Range for outliers is variable, threshold for removal is fixed

###############Section 2: Outlier removal#################################
for (i in c(1:length(mf.list))){
  message(paste0('Outlier removal for ', mf.list[i]))
  df.train <- df.train.temp <- lst.all.train[[i]]
  w <- 1
  df.info <- data.frame(matrix(nrow = (length(v.OL) + 1), ncol = 3))
  names(df.info) <- c('Macrophyte', 'InstRem', 'IQR')
  df.perf <- data.frame(matrix(nrow = (length(v.OL) + 1), ncol = 14))
  df.time <- data.frame(matrix(nrow = (length(v.OL) + 1), ncol = 6))
  names(df.time) <- c('TimeM', 'sd_TimeM', 'TimeT', 'TimeMRed', 'sd_TimeMRed', 'TimeTRel')
  # Baseline
  t0 <- Sys.time()
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
  t1 <- Sys.time()
  v.time.ref <- lst.temp[[3]]
  timeT.ref <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeM <- round(mean(lst.temp[[3]]), 4)
  
  df.info[w, ] <- c(mf.list[i], 0, 'Inf')
  df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  names(df.perf) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                    mean(lst.temp[[3]] / v.time.ref), 
                    sd(lst.temp[[3]] / v.time.ref), 0)
  
  w <- w + 1
  while(w <= nrow(df.info)){
    t0 <- Sys.time()
    # Outlier detection
    v.up <- apply(df.train.temp[, c(5:24)], 2, 
                  function(x) quantile(x, 0.75) + v.OL[w - 1] * 
                    (quantile(x, 0.75) - quantile(x, 0.25)))
    v.low <- apply(df.train.temp[, c(5:24)], 2, 
                   function(x) max(0, quantile(x, 0.25) - v.OL[w-1] * 
                                     (quantile(x, 0.75) - quantile(x, 0.25))))
    df.train.temp$OL <- 0
    for (j in c(1:ncol(df.train.temp[, c(5:24)]))){
      for (k in c(1:nrow(df.train.temp))){
        if(df.train.temp[k, (j + 4)] > v.up[j] | 
           df.train.temp[k, (j + 4)] < v.low[j]){
          df.train.temp$OL[k] <- df.train.temp$OL[k] + 1
        }
      }
    }
    # New training data
    # Here, choice can be made which data is kept (0 -> each instance with outlier(s) removed)
    df.train <- df.train.temp[which(df.train.temp$OL == 0), c(1:ncol(df.train))] 
    df.info[w, ] <- c(mf.list[i], nrow(df.train.temp) - nrow(df.train), v.OL[w - 1])
    if(sum(df.train$Macrophyte == 0) < 10 | sum(df.train$Macrophyte == 1) < 10){ 
      message(paste0('---Insufficient data for v.OL = ', v.OL[w - 1]))
      break 
    } # At least 10 instances needed for cross-validation
    # Model development
    lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
    t1 <- Sys.time()
    timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
    timeM <- round(mean(lst.temp[[3]]), 4)
    # Data extraction
    df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
    df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                      mean(lst.temp[[3]] / v.time.ref), 
                      sd(lst.temp[[3]] / v.time.ref), 
                      ((timeT - df.time$TimeT[1]) / df.time$TimeT[1]))
    w <- w + 1
  }
  lst.inst.rem.perf[[i]] <- cbind.data.frame(df.info, df.perf, df.time)
}
df.inst.rem.perf <- do.call('rbind', lst.inst.rem.perf)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.inst.rem.perf.m <- cbind(melt(df.inst.rem.perf[df.inst.rem.perf$Macrophyte %in% mf.sel, ], 
                                   id.vars = c('Macrophyte', 'InstRem', 'IQR'), 
                                   measure.vars = c('AUC', 'TimeMRed')), 
                              melt(df.inst.rem.perf[df.inst.rem.perf$Macrophyte %in% mf.sel, ], 
                                   id.vars = c('Macrophyte', 'InstRem', 'IQR'), 
                                   measure.vars = c('sd_AUC', 'sd_TimeMRed'))$value)
  names(df.inst.rem.perf.m) <- c('Macrophyte', 'InstRem', 'IQR', 
                                 'Series', 'Value', 'sd')
  df.inst.rem.perf.m$Macrophyte <- factor(df.inst.rem.perf.m$Macrophyte, mf.sel)
  df.inst.rem.perf.m$IQR <- as.numeric(df.inst.rem.perf.m$IQR)
  df.inst.rem.perf.m$InstRem <- as.numeric(df.inst.rem.perf.m$InstRem)
  df.inst.rem.perf.m$Value[df.inst.rem.perf.m$Series == 'TimeMRed'] <- 
    100 * df.inst.rem.perf.m$Value[df.inst.rem.perf.m$Series == 'TimeMRed']
  levels(df.inst.rem.perf.m$Series) <- list('AUC (-)' = 'AUC', 'Time (%)' = 'TimeMRed')
  p.InstanceSelectionOutliers <- ggplot(df.inst.rem.perf.m, aes(x = IQR, y = Value)) + 
    geom_vline(xintercept = 3, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = Value - sd, ymax = Value + sd), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('Outlier threshold ', tau['o'], ' (-)'))) + 
    scale_y_continuous('') +
    facet_grid(Series~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          strip.text.x = element_text(face = 'italic'))
  plot(p.InstanceSelectionOutliers)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S3_InstanceSelectionOutlier.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.InstanceSelectionOutliers)
    dev.off()
  }
  
  p.InstancesVsIQR <- ggplot(df.inst.rem.perf.m, aes(x = IQR, y = InstRem)) + 
    geom_vline(xintercept = 3, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    geom_point() + 
    scale_x_reverse(expression(paste('Outlier threshold ', tau['o'], ' (-)'))) + 
    scale_y_continuous('Instances removed (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9))
  plot(p.InstancesVsIQR)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_InstancesVsIQR.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.InstancesVsIQR)
    dev.off()
  }
}

if(b.save){
  write.table(df.inst.rem.perf, paste0(s.data, 'D_C6S3_InstanceSelectionOutliers.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(lst.inst.rem.perf, v.OL, i, df.train, df.train.temp, df.perf, w, df.info,
   df.time, t0, t1, lst.temp, v.time.ref, timeT.ref, timeT, timeM, v.up, v.low, 
   df.inst.rem.perf.m, p.InstanceSelectionOutliers)

##############Part 3: Pseudoabsence removal (9 hours)#####################
###############Section 1: In- and output##################################
lst.inst.rem.perf <- list()
v.psa <- rev(seq(0, 0.15, 0.01)) # Range is variable, the number of instances outside the variable range is fixed

###############Section 2: Pseudoabsences##################################
for (i in c(1:length(mf.list))){
  message(paste0('Pseudo-absence removal for ', mf.list[i]))
  df.train <- df.train.temp <- lst.all.train[[i]]
  w <- 1
  df.info <- data.frame(matrix(nrow = (length(v.psa) + 1), ncol = 3))
  names(df.info) <- c('Macrophyte', 'InstRem', 'Threshold')
  df.perf <- data.frame(matrix(nrow = (length(v.psa) + 1), ncol = 14))
  df.time <- data.frame(matrix(nrow = (length(v.psa) + 1), ncol = 6))
  names(df.time) <- c('TimeM', 'sd_TimeM', 'TimeT', 'TimeMRed', 'sd_TimeMRed', 'TimeTRel')
  # Baseline
  t0 <- Sys.time()
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
  t1 <- Sys.time()
  v.time.ref <- lst.temp[[3]]
  timeT.ref <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeM <- round(mean(lst.temp[[3]]), 4)
  
  df.info[w, ] <- c(mf.list[i], 0, 'Inf')
  df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  names(df.perf) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                    mean(lst.temp[[3]] / v.time.ref), 
                    sd(lst.temp[[3]] / v.time.ref), 0)
  
  w <- w + 1
  while(w <= nrow(df.info)){
    t0 <- Sys.time()
    # New training data
    df.train <- f.RangeRemoval(df.train.temp, cols = c(5:24), quant = (1 - v.psa[w - 1]))
    df.info[w, ] <- c(mf.list[i], nrow(df.train.temp) - nrow(df.train), v.psa[w - 1])
    if(sum(df.train$Macrophyte == 0) < 10 | sum(df.train$Macrophyte == 1) < 10){ 
      message(paste0('---Insufficient data for psa = ', v.psa[w - 1]))
      break 
    } # At least 10 instances needed for cross-validation
    # Model development
    lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
    t1 <- Sys.time()
    timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
    timeM <- round(mean(lst.temp[[3]]), 4)
    # Data extraction
    df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
    df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                      mean(lst.temp[[3]] / v.time.ref), 
                      sd(lst.temp[[3]] / v.time.ref), 
                      ((timeT - df.time$TimeT[1]) / df.time$TimeT[1]))
    w <- w + 1
  }
  lst.inst.rem.perf[[i]] <- cbind.data.frame(df.info, df.perf, df.time)
}
df.inst.rem.perf <- do.call('rbind', lst.inst.rem.perf)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.inst.rem.perf.m <- cbind(melt(df.inst.rem.perf[df.inst.rem.perf$Macrophyte %in% mf.sel, ], 
                                   id.vars = c('Macrophyte', 'InstRem', 'Threshold'), 
                                   measure.vars = c('AUC', 'TimeMRed')), 
                              melt(df.inst.rem.perf[df.inst.rem.perf$Macrophyte %in% mf.sel, ], 
                                   id.vars = c('Macrophyte', 'InstRem', 'Threshold'), 
                                   measure.vars = c('sd_AUC', 'sd_TimeMRed'))$value)
  names(df.inst.rem.perf.m) <- c('Macrophyte', 'InstRem', 'Threshold', 
                                 'Series', 'Value', 'sd')
  df.inst.rem.perf.m$Macrophyte <- factor(df.inst.rem.perf.m$Macrophyte, mf.sel)
  df.inst.rem.perf.m$Threshold <- as.numeric(df.inst.rem.perf.m$Threshold)
  df.inst.rem.perf.m$InstRem <- as.numeric(df.inst.rem.perf.m$InstRem)
  df.inst.rem.perf.m$Value[df.inst.rem.perf.m$Series == 'TimeMRed'] <- 
    100 * df.inst.rem.perf.m$Value[df.inst.rem.perf.m$Series == 'TimeMRed']
  levels(df.inst.rem.perf.m$Series) <- list('AUC (-)' = 'AUC', 'Time (%)' = 'TimeMRed')
  p.InstanceSelectionPseudoabsence <- 
    ggplot(df.inst.rem.perf.m, aes(x = 100 * Threshold, y = Value)) +
    geom_vline(xintercept = 5, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = Value - sd, ymax = Value + sd), alpha = 0.2) + 
    geom_line() +  
    scale_x_reverse(expression(paste('False absence threshold ', tau['a'], ' (%)'))) + 
    scale_y_continuous('') + 
    facet_grid(Series~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8))
  plot(p.InstanceSelectionPseudoabsence)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S3_InstanceSelectionPseudoabsence.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.InstanceSelectionPseudoabsence)
    dev.off()
  }
  
  p.InstancesVsThreshold <- ggplot(df.inst.rem.perf.m, aes(x = 100 * Threshold, y = InstRem)) + 
    geom_vline(xintercept = 5, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    geom_point() + 
    facet_grid(.~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    scale_x_reverse(expression(paste('False absence threshold ', tau['a'], ' (%)'))) + 
    scale_y_continuous('Instances removed (-)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8))
  plot(p.InstancesVsThreshold)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_InstancesVsThreshold.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.InstancesVsThreshold)
    dev.off()
  }
}

if(b.save){
  write.table(df.inst.rem.perf, paste0(s.data, 'D_C6S3_InstanceSelectionPseudoabsence.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(lst.inst.rem.perf, v.psa, i, df.train, df.train.temp, w, df.info, df.perf, 
   df.time, t0, t1, lst.temp, v.time.ref, timeT.ref, timeT, timeM, 
   df.inst.rem.perf.m, p.InstanceSelectionPseudoabsence)

##############Part 4: Variable correlation (9 hours)######################
###############Section 1: In- and output##################################
lst.var.rem.perf <- list()
v.Corr <- rev(seq(0.25, 0.95, 0.05))

###############Section 2: Correlation-based removal#######################
for (i in c(1:length(mf.list))){
  message(paste0('Variable removal for ', mf.list[i]))
  df.train <- df.train.temp <- lst.all.train[[i]]
  v.var <- v.var.temp <- names(df.train[, c(5:24)])
  df.perf <- c()
  w <- 1
  df.info <- data.frame(matrix(nrow = (length(v.Corr) + 1), ncol = 3))
  names(df.info) <- c('Macrophyte', 'VarRem', 'Correlation')
  df.perf <- data.frame(matrix(nrow = (length(v.Corr) + 1), ncol = 14))
  df.time <- data.frame(matrix(nrow = (length(v.Corr) + 1), ncol = 6))
  names(df.time) <- c('TimeM', 'sd_TimeM', 'TimeT', 'TimeMRed', 'sd_TimeMRed', 'TimeTRel')
  # Baseline
  t0 <- Sys.time()
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
  t1 <- Sys.time()
  v.time.ref <- lst.temp[[3]]
  timeT.ref <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeM <- round(mean(lst.temp[[3]]), 4)
  
  df.info[w, ] <- c(mf.list[i], 0, 1)
  df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  names(df.perf) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                    mean(lst.temp[[3]] / v.time.ref), 
                    sd(lst.temp[[3]] / v.time.ref), 0)
  
  w <- w + 1
  # Iterative removal
  while (w <= nrow(df.info)){
    t0 <- Sys.time()
    maxR <- 1
    v.var <- v.var.temp
    df.train <- df.train.temp
    while (maxR >= v.Corr[w - 1]){
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
      df.corr <- df.corr[order(abs(df.corr$R2), decreasing = T), ]
      cor.var1 <- cor(df.train[, which(names(df.train) == df.corr$Var1[1])], 
                      df.train$Macrophyte)
      cor.var2 <- cor(df.train[, which(names(df.train) == df.corr$Var2[1])], 
                      df.train$Macrophyte)
      if (abs(cor.var1) >= abs(cor.var2)){
        v.var <- v.var[-which(v.var == df.corr$Var2[1])]
      } else {
        v.var <- v.var[-which(v.var == df.corr$Var1[1])]
      }
      maxR <- df.corr$R2[1]
    }
    df.train <- df.train[, c(1:4, which(names(df.train) %in% v.var), ncol(df.train))]
    lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
    t1 <- Sys.time()
    timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
    timeM <- round(mean(lst.temp[[3]]), 4)
    
    df.info[w, ] <- c(mf.list[i], length(v.var.temp) - length(v.var), v.Corr[w - 1])
    df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
    df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                      mean(lst.temp[[3]] / v.time.ref), 
                      sd(lst.temp[[3]] / v.time.ref), 
                      ((timeT - df.time$TimeT[1]) / df.time$TimeT[1]))
    
    w <- w + 1
  }
  lst.var.rem.perf[[i]] <- cbind.data.frame(df.info, df.perf, df.time)
}
df.var.rem.perf <- do.call('rbind', lst.var.rem.perf)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.var.rem.perf.m <- cbind(melt(df.var.rem.perf[df.var.rem.perf$Macrophyte %in% mf.sel, ], 
                                  id.vars = c('Macrophyte', 'VarRem', 'Correlation'), 
                                  measure.vars = c('AUC', 'TimeMRed')), 
                             melt(df.var.rem.perf[df.var.rem.perf$Macrophyte %in% mf.sel, ], 
                                  id.vars = c('Macrophyte', 'VarRem', 'Correlation'), 
                                  measure.vars = c('sd_AUC', 'sd_TimeMRed'))$value)
  names(df.var.rem.perf.m) <- c('Macrophyte', 'VarRem', 'Correlation', 
                                'Series', 'Value', 'sd')
  df.var.rem.perf.m$Macrophyte <- factor(df.var.rem.perf.m$Macrophyte, mf.sel)
  df.var.rem.perf.m$Value[df.var.rem.perf.m$Series == 'TimeMRed'] <- 
    100 * df.var.rem.perf.m$Value[df.var.rem.perf.m$Series == 'TimeMRed']
  levels(df.var.rem.perf.m$Series) <- list('AUC (-)' = 'AUC', 'Time (%)' = 'TimeMRed')
  p.VariableSelectionCorrelation <- 
    ggplot(df.var.rem.perf.m, aes(x = abs(as.numeric(Correlation)), y = Value)) + 
    geom_vline(xintercept = 0.7, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = Value - sd, ymax = Value + sd), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('Correlation threshold ', tau['c'], ' (-)')), 
                    limits = c(1, 0.25)) + 
    scale_y_continuous('') + 
    facet_grid(Series~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          strip.text.x = element_text(face = 'italic'))
  plot(p.VariableSelectionCorrelation)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S3_VariableSelectionCorrelation.tiff'), 
         units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.VariableSelectionCorrelation)
    dev.off()
  }

  p.VariablesVsCorrelation <- 
    ggplot(df.var.rem.perf.m, aes(x = abs(as.numeric(Correlation)), y = as.numeric(VarRem))) + 
    geom_vline(xintercept = 0.7, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    geom_point() +
    scale_x_reverse(expression(paste('Correlation threshold ', tau['c'], ' (-)')), 
                    limits = c(1, 0.25)) + 
    scale_y_continuous('Variables removed (-)') + 
    facet_grid(.~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(face = 'italic',size = 8))
  plot(p.VariablesVsCorrelation)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_VariableVsCorrelation.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.VariablesVsCorrelation)
    dev.off()
  }
}

if(b.save){
  write.table(df.var.rem.perf, paste0(s.data, 'D_C6S3_VariableSelectionCorrelation.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(lst.var.rem.perf, df.train, v.var, w, df.perf, df.info, df.time, t0, 
   t1, lst.temp, v.time.ref, timeT.ref, timeT, timeM, df.corr, i, j, k, v, 
   df.cor.temp, cor.var1, cor.var2, df.var.rem.perf.m, 
   p.VariableSelectionCorrelation)

##############Part 5: Variable importance (21 hours)######################
###############Section 1: In- and output##################################
lst.var.rem.perf <- list()
v.VI <- seq(0.05, 0.75, 0.05)

###############Section 2: Modeling########################################
for (i in c(1:length(mf.list))){
  message(paste0('Variable removal for ', mf.list[i]))
  df.train <- lst.all.train[[i]]
  v.var <- names(df.train[, c(5:24)])
  w <- 1
  df.info <- data.frame(matrix(nrow = (length(v.VI) + 1), ncol = 3))
  names(df.info) <- c('Macrophyte', 'VarRem', 'RVI')
  df.perf <- data.frame(matrix(nrow = (length(v.VI) + 1), ncol = 14))
  df.time <- data.frame(matrix(nrow = (length(v.VI) + 1), ncol = 6))
  names(df.time) <- c('TimeM', 'sd_TimeM', 'TimeT', 'TimeMRed', 'sd_TimeMRed', 'TimeTRel')
  # Baseline
  t0 <- Sys.time()
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
  t1 <- Sys.time()
  v.time.ref <- lst.temp[[3]]
  timeT.ref <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
  timeM <- round(mean(lst.temp[[3]]), 4)
  
  df.info[w, ] <- c(mf.list[i], 0, 0)
  df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  names(df.perf) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                    mean(lst.temp[[3]] / v.time.ref), 
                    sd(lst.temp[[3]] / v.time.ref), 0)
  
  w <- w + 1
  # Repeated removal
  while(w <= nrow(df.info)){
    t0 <- Sys.time() 
    lst.temp <- f.CForestVI(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, n.rep = 10)
    df.vi.temp <- lst.temp[[2]]
    df.vi.temp$RVI <- df.vi.temp$Mean / max(df.vi.temp$Mean)
    v.varRem <- df.vi.temp$Metric[(df.vi.temp$RVI < v.VI[w - 1])]
    if(length(v.varRem) > 0){ 
      v.var.new <- v.var[-which(v.var %in% (v.varRem))] 
    } else { 
      v.var.new <- v.var 
    }
    df.train.new <- df.train[, c(c(1:4), which(names(df.train) %in% v.var.new), ncol(df.train))]
    lst.temp <- f.CForest(df.train.new, cols = c(5:ncol(df.train.new)), n.ntree = 200, n.rep = 10)
    t1 <- Sys.time()
    timeT <- round(as.numeric(difftime(t1, t0, units = 'secs')), 4)
    timeM <- round(mean(lst.temp[[3]]), 4)
    
    df.info[w, ] <- c(mf.list[i], length(v.var) - length(v.var.new), v.VI[w - 1])
    df.perf[w, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
    df.time[w, ] <- c(timeM, round(sd(lst.temp[[3]]), 4), timeT, 
                      mean(lst.temp[[3]] / v.time.ref), 
                      sd(lst.temp[[3]] / v.time.ref), 
                      ((timeT - df.time$TimeT[1]) / df.time$TimeT[1]))
    
    w <- w + 1
  }
  lst.var.rem.perf[[i]] <- cbind.data.frame(df.info, df.perf, df.time)
}
df.var.rem.perf <- do.call('rbind', lst.var.rem.perf)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.var.rem.perf.m <- cbind(melt(df.var.rem.perf[df.var.rem.perf$Macrophyte %in% mf.sel, ], 
                                  id.vars = c('Macrophyte', 'VarRem', 'RVI'), 
                                  measure.vars = c('AUC', 'TimeMRed')), 
                             melt(df.var.rem.perf[df.var.rem.perf$Macrophyte %in% mf.sel, ], 
                                  id.vars = c('Macrophyte', 'VarRem', 'RVI'), 
                                  measure.vars = c('sd_AUC', 'sd_TimeMRed'))$value)
  names(df.var.rem.perf.m) <- c('Macrophyte', 'VarRem', 'Importance', 
                                'Series', 'Value', 'sd')
  df.var.rem.perf.m$Macrophyte <- factor(df.var.rem.perf.m$Macrophyte, mf.sel)
  df.var.rem.perf.m$Value[df.var.rem.perf.m$Series == 'TimeMRed'] <- 
    100 * df.var.rem.perf.m$Value[df.var.rem.perf.m$Series == 'TimeMRed']
  levels(df.var.rem.perf.m$Series) <- list('AUC (-)' = 'AUC', 'Time (%)' = 'TimeMRed')
  p.VariableSelectionImportance <- 
    ggplot(df.var.rem.perf.m, aes(x = 100 * as.numeric(Importance), y = Value)) + 
    geom_vline(xintercept = 10, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = Value - sd, ymax = Value + sd), alpha = 0.2) + 
    geom_line() + 
    scale_x_continuous(expression(paste('Importance threshold ', tau['i'], ' (%)'))) + 
    scale_y_continuous('') + 
    facet_grid(Series~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(size = 8), 
          strip.text.x = element_text(face = 'italic'), 
          strip.placement = 'outside')
  plot(p.VariableSelectionImportance)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S3_VariableSelectionImportance.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.VariableSelectionImportance)
    dev.off()
  }
  
  p.VariablesVsImportance <- 
    ggplot(df.var.rem.perf.m, aes(x = 100 * as.numeric(Importance), y = as.numeric(VarRem))) + 
    geom_vline(xintercept = 10, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    geom_point() + 
    scale_x_continuous(expression(paste('Importance threshold ', tau['i'], ' (%)'))) + 
    scale_y_continuous('Variables removed (-)') + 
    facet_grid(.~Macrophyte, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text.x = element_text(face = 'italic',size = 8))
  plot(p.VariablesVsImportance)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_VariablesVsImportance.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.VariablesVsImportance)
    dev.off()
  }
}

if(b.save){
  write.table(df.var.rem.perf, paste0(s.data, 'D_C6S3_VariableSelectionImportance.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(lst.var.rem.perf, v.VI, df.train, v.var, df.perf, w, df.info, df.time, t0, 
   t1, lst.temp, v.time.ref, timeT.ref, timeT, timeM, df.vi.temp,i, 
   df.var.rem.perf.m, p.VariableSelectionImportance)

##############Part 6: Overviews and Summaries for subset##################
###############Section 1: Loading data####################################
# Read data
df.inst.ol <- read.table(paste0(s.data, 'D_C6S3_InstanceSelectionOutliers.txt'), 
                         header = T, sep = ',')
df.inst.psa <- read.table(paste0(s.data, 'D_C6S3_InstanceSelectionPseudoabsence.txt'), 
                          header = T, sep = ',')
df.var.imp <- read.table(paste0(s.data, 'D_C6S3_VariableSelectionImportance.txt'), 
                         header = T, sep = ',')
df.var.cor <- read.table(paste0(s.data, 'D_C6S3_VariableSelectionCorrelation.txt'), 
                         header = T, sep = ',')
if(b.plot){
  # Select macrophytes
  df.inst.ol <- df.inst.ol[which(df.inst.ol$Macrophyte %in% mf.sel), ]
  df.inst.psa <- df.inst.psa[which(df.inst.psa$Macrophyte %in% mf.sel), ]
  df.var.imp <- df.var.imp[which(df.var.imp$Macrophyte %in% mf.sel), ]
  df.var.cor <- df.var.cor[which(df.var.cor$Macrophyte %in% mf.sel), ]
}

###############Section 2: Plotting time###################################
if(b.plot){
  # Instances
  df.inst.ol.m <- melt(df.inst.ol, measure.vars = c('TimeTRel'), 
                       id.vars = c('Macrophyte', 'InstRem', 'IQR'))
  df.inst.ol.m$Macrophyte <- factor(df.inst.ol.m$Macrophyte, mf.sel)
  df.inst.psa.m <- melt(df.inst.psa, measure.vars = c('TimeTRel'), 
                        id.vars = c('Macrophyte', 'InstRem', 'Threshold'))
  df.inst.psa.m$Macrophyte <- factor(df.inst.psa.m$Macrophyte, mf.sel)
  
  p.InstancesOutliers <- ggplot(df.inst.ol.m, aes(x = IQR, y = 100 * value)) + 
    geom_vline(xintercept = 3, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    scale_x_reverse(expression(paste('Outlier threshold ', tau['o'], ' (-)'))) + 
    scale_y_continuous('Time (%)') + 
    facet_grid(.~Macrophyte, scales = 'free_y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.InstancesOutliers)
  
  p.InstancesPseudo <- ggplot(df.inst.psa.m, aes(x = 100 * Threshold, y = 100 * value)) + 
    geom_vline(xintercept = 5, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    scale_x_reverse(expression(paste('False absence threshold ', tau['a'], ' (%)'))) + 
    scale_y_continuous('Time (%)') + 
    facet_grid(.~Macrophyte, scales = 'free_y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8, colour = 'white'), 
          legend.title = element_blank())
  plot(p.InstancesPseudo)
  
  # Variable
  df.var.cor.m <- melt(df.var.cor, measure.vars = c('TimeTRel'), 
                       id.vars = c('Macrophyte', 'VarRem', 'Correlation'))
  df.var.cor.m$Macrophyte <- factor(df.var.cor.m$Macrophyte, mf.sel)
  df.var.imp.m <- melt(df.var.imp, measure.vars = c('TimeTRel'), 
                       id.vars = c('Macrophyte', 'VarRem', 'RVI'))
  df.var.imp.m$Macrophyte <- factor(df.var.imp.m$Macrophyte, mf.sel)
  
  p.VariablesCorrelation <- ggplot(df.var.cor.m, aes(x = Correlation, y = 100 * value)) + 
    geom_vline(xintercept = 0.7, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    scale_x_reverse(expression(paste('Correlation threshold ', tau['c'], ' (-)'))) + 
    scale_y_continuous('Time (%)') + 
    facet_grid(.~Macrophyte, scales = 'free_y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8, colour = 'white'), 
          legend.title = element_blank())
  plot(p.VariablesCorrelation)
  
  p.VariablesImportance <- ggplot(df.var.imp.m, aes(x = 100 * RVI, y = 100 * value)) + 
    geom_vline(xintercept = 10, colour = 'grey70', linetype = 'dashed') + 
    geom_line() + 
    scale_x_continuous(expression(paste('Importance threshold ', tau['i'], ' (%)'))) + 
    scale_y_continuous('Time (%)') + 
    facet_grid(.~Macrophyte, scales = 'free_y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8, colour = 'white'), 
          legend.title = element_blank())
  plot(p.VariablesImportance)
  
  # Combined
  p.TimeInstancesVariables <- ggarrange(p.InstancesOutliers, 
                                        p.InstancesPseudo, 
                                        p.VariablesCorrelation, 
                                        p.VariablesImportance, 
                                        nrow = 4, ncol = 1)
  plot(p.TimeInstancesVariables)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_TimeTotalOverview.tiff'), units = 'mm', 
         width = 240, height = 135, res = 300, pointsize = 7)
    plot(p.TimeInstancesVariables)
    dev.off()
  }
}

###############Section 3: Variable removal################################
rm(df.inst.ol, df.inst.psa, df.var.imp, df.var.cor)

##############Part 7: Supplementary graphs - all species##################
###############Section 1: Loading data####################################
# Read data
df.inst.ol <- read.table(paste0(s.data, 'D_C6S3_InstanceSelectionOutliers.txt'), 
                         header = T, sep = ',')
df.inst.psa <- read.table(paste0(s.data, 'D_C6S3_InstanceSelectionPseudoabsence.txt'), 
                          header = T, sep = ',')
df.var.imp <- read.table(paste0(s.data, 'D_C6S3_VariableSelectionImportance.txt'), 
                         header = T, sep = ',')
df.var.cor <- read.table(paste0(s.data, 'D_C6S3_VariableSelectionCorrelation.txt'), 
                         header = T, sep = ',')

###############Section 2: Plotting all species############################
if(b.plot){
  # Outliers, plots 1 & 2
  p.InstancesOutliersP1 <- 
    ggplot(df.inst.ol[which(df.inst.ol$Macrophyte %in% mf.list[1:30]),] ,aes(x = IQR, y = AUC)) + 
    geom_vline(xintercept = 3, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('Outlier threshold ', tau['o'], ' (-)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', ncol = 6, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() +
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.InstancesOutliersP1)
  
  p.InstancesOutliersP2 <- 
    ggplot(df.inst.ol[which(df.inst.ol$Macrophyte %in% mf.list[31:length(mf.list)]), ], aes(x = IQR, y = AUC)) + 
    geom_vline(xintercept = 3, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('Outlier threshold ', tau['o'], ' (-)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', ncol = 6, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.InstancesOutliersP2)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_OutliersAllSpecies1.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.InstancesOutliersP1)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C6S3_OutliersAllSpecies2.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.InstancesOutliersP2)
    dev.off()
  }
  
  # False absences
  p.InstancesPseudoP1 <- 
    ggplot(df.inst.psa[which(df.inst.psa$Macrophyte %in% mf.list[1:30]), ], 
           aes(x = 100 * Threshold, y = AUC)) + 
    geom_vline(xintercept = 5, colour = 'grey70', linetype = 'dashed') +  
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, 
                    ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('False absence threshold ', tau['a'], ' (%)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', ncol = 6, 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.InstancesPseudoP1)
  
  p.InstancesPseudoP2 <- 
    ggplot(df.inst.psa[which(df.inst.psa$Macrophyte %in% mf.list[31:length(mf.list)]), ], 
           aes(x = 100 * Threshold, y = AUC)) + 
    geom_vline(xintercept = 5, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, 
                    ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('False absence threshold ', tau['a'], ' (%)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', ncol = 6, 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.InstancesPseudoP2)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_AbsencesAllSpecies1.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.InstancesPseudoP1)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C6S3_AbsencesAllSpecies2.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.InstancesPseudoP2)
    dev.off()
  }
  
  # Correlated variables, plot 1 & 2
  p.VariablesCorrelationP1 <- 
    ggplot(df.var.cor[which(df.var.cor$Macrophyte %in% mf.list[1:30]), ], 
           aes(x = Correlation, y = AUC)) + 
    geom_vline(xintercept = 0.7, colour = 'grey70', linetype = 'dashed') +  
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, 
                    ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('Correlation threshold ', tau['c'], ' (-)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.VariablesCorrelationP1)
  
  p.VariablesCorrelationP2 <- 
    ggplot(df.var.cor[which(df.var.cor$Macrophyte %in% mf.list[31:length(mf.list)]), ], 
           aes(x = Correlation, y = AUC)) + 
    geom_vline(xintercept = 0.7, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, 
                    ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_reverse(expression(paste('Correlation threshold ', tau['c'], ' (-)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.VariablesCorrelationP2)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_CorrelationAllSpecies1.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.VariablesCorrelationP1)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C6S3_CorrelationAllSpecies2.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.VariablesCorrelationP2)
    dev.off()
  }
  
  # Important variables, plot 1 & 2
  p.VariablesImportanceP1 <- 
    ggplot(df.var.imp[which(df.var.imp$Macrophyte %in% mf.list[1:30]), ], 
           aes(x = 100 * RVI, y = AUC)) + 
    geom_vline(xintercept = 10, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, 
                    ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_continuous(expression(paste('Importance threshold ', tau['i'], ' (%)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', ncol = 6, 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.VariablesImportanceP1)
  
  p.VariablesImportanceP2 <- 
    ggplot(df.var.imp[which(df.var.imp$Macrophyte %in% mf.list[31:length(mf.list)]), ], 
           aes(x = 100 * RVI, y = AUC)) + 
    geom_vline(xintercept = 10, colour = 'grey70', linetype = 'dashed') + 
    geom_ribbon(aes(ymin = AUC - sqrt(10) * sd_AUC, 
                    ymax = AUC + sqrt(10) * sd_AUC), alpha = 0.2) + 
    geom_line() + 
    scale_x_continuous(expression(paste('Importance threshold ', tau['i'], ' (%)'))) + 
    scale_y_continuous('AUC (-)') + 
    facet_wrap(~Macrophyte, scales = 'free_y', ncol = 6, 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text.x = element_text(face = 'italic', size = 8), 
          legend.title = element_blank())
  plot(p.VariablesImportanceP2)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S3_ImportanceAllSpecies1.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.VariablesImportanceP1)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C6S3_ImportanceAllSpecies2.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.VariablesImportanceP2)
    dev.off()
  }
}

###############Section 3: Variable removal################################
rm(df.inst.ol, df.inst.psa, df.var.imp, df.var.cor, p.InstancesOutliersP1, 
   p.InstancesOutliersP2, p.InstancesPseudoP1, p.InstancesPseudoP2, 
   p.VariablesCorrelationP1, p.VariablesCorrelationP2, 
   p.VariablesImportanceP1, p.VariablesImportanceP2)
