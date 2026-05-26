##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 16/11/2019; Last changes: 03/02/2020
###############Section 2: Packages########################################
# install.packages('party') # For cForest modelling
# install.packages('PresenceAbsence') # For confusion matrix and metrics
# install.packages('reshape2') # For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
# install.packages('gridExtra') 
# install.packages('dplyr') # For arranging variables in plot
# install.packages('future') # For parallel computing on HPC
###############Section 3: Libraries#######################################
library(party)
library(PresenceAbsence)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(parallel)
library(foreach)
library(doParallel)
library(future)
###############Section 4: Defining functions##############################
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
f.SummaryPerf <- function(data, plot = F){
  # Determine mean and sd per cross-validation
  df.mean <- df.sd <- c()
  for (i in c(1:max(data$n))){
    df.mean <- as.data.frame(rbind(df.mean, apply(data[data$n == i, ], 2, mean)))
    df.sd <- as.data.frame(rbind(df.sd, apply(data[data$n == i, ], 2, sd)))
  }
  # Melt together in general dataframe and add information on type
  df.cv.perf <- cbind(melt(df.mean, measure.vars = names(df.mean)[1:(ncol(df.mean) - 2)], id.vars = 'n'), 
                      melt(df.sd,measure.vars = names(df.sd)[1:(ncol(df.sd) - 2)], id.vars = 'n')$value)
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
      geom_point(data = df.all[df.all$Type=='CV', ], aes(n, Mean)) + 
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
f.PerfLongWide <- function(data, lab = 1, mean, sd){
  df.new <- c(data[, mean], data[, sd])
  names(df.new) <- c(as.character(data[, 1]), paste0('sd_', data[, 1]))
  
  return(df.new)
}
f.CForest <- function(data, cols = c(1:ncol(data)), eval = NULL, n.ntree = 200, 
                      n.mtry = NULL, n.split = 0.2, n.leaf = 0.01, n.rep, n.cv = 5){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  # Open different clusters for n
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.out <- foreach(n = 1:n.rep, .packages = c('party', 'PresenceAbsence'), 
                    .export = c('f.Performance', 'f.OptDiscrThreshold')) %dopar% {
    lst.data <- list()
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
    df.perf.temp <- df.perf.eval <- c()
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
                                                  mtry = floor(sqrt(length(cols))), 
                                                  minsplit = n.split * nrow(df.train), 
                                                  minbucket = n.leaf * nrow(df.train)))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, 
                                                  mtry = n.mtry, 
                                                  minsplit = n.split * nrow(df.train), 
                                                  minbucket = n.leaf * nrow(df.train)))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, f.Performance(df.test, lab = 1, 
                                                        obs = (ncol(df.test) - 1), 
                                                        pred = ncol(df.test))[[1]])
      if(!is.null(eval)){
        eval$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = eval))
        df.perf.eval <- rbind(df.perf.eval, f.Performance(eval, lab = 1, 
                                                          obs = (ncol(eval) - 1), 
                                                          pred = ncol(eval))[[1]])
      }
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    if(!is.null(eval)){
      df.perf.eval$k <- c(1:n.cv)
      df.perf.eval$n <- n
    }
    list(df.perf.temp, df.perf.eval)
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.train <- df.perf.eval <- c()
  for (i in c(1:n.rep)){
    df.perf.train <- rbind(df.perf.train, df.out[[i]][[1]])
    df.perf.eval <- rbind(df.perf.eval, df.out[[i]][[2]])
  }
  df.perf.train.sum <- f.SummaryPerf(df.perf.train)
  if(!is.null(eval)){
    df.perf.eval.sum <- f.SummaryPerf(df.perf.eval)
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
                df.perf.eval, df.perf.eval.sum[df.perf.eval.sum$Type == 'Rep', c(1, 4, 5)]))
  } else {
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)]))  
  }
}
f.CForestVI <- function(data, cols = c(1:ncol(data)), eval = NULL, n.ntree = 200, 
                        n.mtry = NULL, n.split = 0.2, n.leaf = 0.01, n.rep, n.cv = 5){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  # Open different clusters for n
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.out <- foreach(n = 1:n.rep, .packages = c('party', 'PresenceAbsence'), 
                    .export = c('f.Performance', 'f.OptDiscrThreshold')) %dopar% {
    lst.data <- list()
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
    df.perf.temp <- df.perf.eval <- df.vi.temp <- c()
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
                                                  mtry = floor(sqrt(length(cols))), 
                                                  minsplit = n.split * nrow(df.train), 
                                                  minbucket = n.leaf * nrow(df.train)))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry,
                                                  minsplit = n.split * nrow(df.train), 
                                                  minbucket = n.leaf * nrow(df.train)))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, f.Performance(df.test, lab = 1, 
                                                        obs = (ncol(df.test) - 1), 
                                                        pred = ncol(df.test))[[1]])
      # Extract variable importance
      set.seed(n)
      v.vi.temp <- t(as.data.frame(varimp(cf, conditional = F, threshold = 0.99)))
      df.vi.temp <- rbind.data.frame(df.vi.temp, v.vi.temp / max(v.vi.temp))
      # Determine testing performance
      if(!is.null(eval)){
        eval$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = eval))
        df.perf.eval <- rbind(df.perf.eval, f.Performance(eval, lab = 1, 
                                                          obs = (ncol(eval) - 1), 
                                                          pred = ncol(eval))[[1]])
      }
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    df.vi.temp$k <- c(1:n.cv)
    df.vi.temp$n <- n
    row.names(df.vi.temp) <- c()
    if(!is.null(eval)){
      df.perf.eval$k <- c(1:n.cv)
      df.perf.eval$n <- n
    }
    list(df.perf.temp, df.perf.eval, df.vi.temp)
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.train <- df.perf.eval <- df.vi.temp <- c()
  for (i in c(1:n.rep)){
    df.perf.train <- rbind(df.perf.train, df.out[[i]][[1]])
    df.perf.eval <- rbind(df.perf.eval, df.out[[i]][[2]])
    df.vi.temp <- rbind(df.vi.temp, df.out[[i]][[3]])
  }
  df.perf.train.sum <- f.SummaryPerf(df.perf.train)
  df.vi.sum <- f.SummaryPerf(df.vi.temp)
  if(!is.null(eval)){
    df.perf.eval.sum <- f.SummaryPerf(df.perf.eval)
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
                df.perf.eval, df.perf.eval.sum[df.perf.eval.sum$Type == 'Rep', c(1, 4, 5)], 
                df.vi.temp, df.vi.sum[df.vi.sum$Type == 'Rep', c(1, 4, 5)]))
  } else {
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
                df.vi.temp, df.vi.sum[df.vi.sum$Type == 'Rep', c(1, 4, 5)]))  
  }
}
f.CForestPDP <- function(data, cols = c(1:ncol(data)), eval = NULL, range = NULL, 
                         n.ntree = 200, n.mtry = NULL, n.split = 0.2, 
                         n.leaf = 0.01, n.rep, n.cv = 5, breaks = 3){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  # Open different clusters for n
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.out <- foreach(n = 1:n.rep, .packages = c('party', 'PresenceAbsence'), 
                    .export = c('f.Performance', 'f.OptDiscrThreshold')) %dopar% {
    lst.data <- list()
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
    df.perf.temp <- df.perf.eval <- df.pdp <- c()
    lst.pdp <- list()
    if(!is.null(range)){ range<-range[which(range[, 1] %in% names(data)), ] }
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
                                                  mtry = floor(sqrt(length(cols))), 
                                                  minsplit = n.split * nrow(df.train), 
                                                  minbucket = n.leaf * nrow(df.train)))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry, 
                                                  minsplit = n.split * nrow(df.train), 
                                                  minbucket = n.leaf * nrow(df.train)))
      }
      # Internal validation
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, f.Performance(df.test, lab = 1, 
                                                        obs = (ncol(df.test) - 1), 
                                                        pred = ncol(df.test))[[1]])
      # External testing
      if(!is.null(eval)){
        eval$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = eval))
        df.perf.eval <- rbind(df.perf.eval, f.Performance(eval, lab = 1, 
                                                          obs = (ncol(eval) - 1), 
                                                          pred = ncol(eval))[[1]])
      }
      # Partial dependence
      if(!is.null(range)){
        for (k in c(1:nrow(range))){
          df.part <- data
          v.var <- seq(range$Min[k], range$Max[k], length.out = breaks)
          df.pdp <- as.data.frame(matrix(nrow = length(v.var), ncol = 3))
          names(df.pdp) <- c('Variable', 'Value', 'SI')
          for (m in c(1:length(v.var))){
            df.part[, which(names(df.part) == range[k, 1])] <- v.var[m]
            v.pred <- do.call('rbind', predict(cf, type = 'prob', newdata = df.part))
            df.pdp$Variable[m] <- range[k, 1]
            df.pdp$Value[m] <- v.var[m]
            df.pdp$SI[m] <- mean(v.pred)
          }
          if (i == 1){
            lst.pdp[[k]] <- df.pdp[, c(1, 2)]
          }
          lst.pdp[[k]] <- cbind(lst.pdp[[k]], df.pdp$SI)
        }
      }
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    if(!is.null(eval)){
      df.perf.eval$k <- c(1:n.cv)
      df.perf.eval$n <- n
    }
    if(!is.null(range)){
      df.pdp <- do.call('rbind', lst.pdp)
      df.pdp <- cbind.data.frame(df.pdp[, c(1:2)], apply(df.pdp[, c(3:ncol(df.pdp))], 1, mean))
      names(df.pdp)[3] <- 'SI'
    }
    list(df.perf.temp, df.perf.eval, df.pdp)
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.train <- df.perf.eval <- c()
  for (i in c(1:n.rep)){
    df.perf.train <- rbind(df.perf.train, df.out[[i]][[1]])
    df.perf.eval <- rbind(df.perf.eval, df.out[[i]][[2]])
    if(!is.null(range)){
      if(i == 1){ df.pdp <- df.out[[i]][[3]][, c(1:2)] }
      df.pdp <- cbind(df.pdp, df.out[[i]][[3]]$SI)
      names(df.pdp)[2 + i] <- paste0('N', i)
    }
  }
  df.perf.train.sum <- f.SummaryPerf(df.perf.train)
  if(!is.null(range)){
    df.pdp.sum <- df.pdp[, c(1:2)]
    df.pdp.sum$Mean <- apply(df.pdp[, c(3:ncol(df.pdp))], 1, mean)
    df.pdp.sum$SEM <- apply(df.pdp[, c(3:ncol(df.pdp))], 1, sd)
  } else {
    df.pdp <- df.pdp.sum <- NULL
  }
  if(!is.null(eval)){
    df.perf.eval.sum <- f.SummaryPerf(df.perf.eval)
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
                df.perf.eval, df.perf.eval.sum[df.perf.eval.sum$Type == 'Rep', c(1, 4, 5)], 
                df.pdp, df.pdp.sum))
  } else {
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
                df.pdp, df.pdp.sum))  
  }
}
###############Section 5: Defining global variables#######################
s.data <- '../Data/'
s.figs <- '../Figures/'
###############Section 6: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Baseline information (< 5 sec)######################
###############Section 1: Loading of dataset##############################
df.all <- read.table(paste0(s.data, 'D_C7_AllData_ImputedAndMerged.txt'), 
                     header = T, sep = ',')
df.all <- df.all[order(row.names(df.all)), ]
mf.index <- min(c(which('Absent' == df.all[1, ]), which('Present' == df.all[1, ])))
df.all[, c(mf.index:ncol(df.all))] <- ifelse(df.all[, c(mf.index:ncol(df.all))] == 'Present', 1, 0)
df.all[, c(5:ncol(df.all))] <- apply(df.all[, c(5:ncol(df.all))], 2, as.numeric)
names(df.all)[mf.index:ncol(df.all)] <- gsub('[.]', ' ', names(df.all)[mf.index:ncol(df.all)])
df.set <- read.table(paste0(s.data, 'D_C7S3_OptimisedSettings.txt'), 
                     header = T, sep = ',')
v.var <- names(df.all)[c(5:(mf.index - 1))]

###############Section 2: Listing macrophytes#############################
mf.list <- names(df.all)[mf.index:ncol(df.all)]
mf.sel <- c('Phragmites australis', 'Lemna minor', 'Ceratophyllum demersum', 
            'Mentha aquatica', 'Lemna minuta')

###############Section 3: Creating training and test data#################
lst.all.train <- lst.all.test <- list()
for (i in c(1:length(mf.list))){
  df.temp <- read.table(paste0(s.data, 'DataTraining/D_C7S1_Data_', 
                               sub(' ', '_', mf.list[i]), '.txt'), header = T, sep = ',')
  set.seed(621)
  v.sample <- sample(c(1:nrow(df.temp)), size = 0.1 * nrow(df.temp), replace = F)
  lst.all.train[[i]] <- df.temp[-v.sample, ]
  lst.all.test[[i]] <- df.temp[v.sample, ]
  lst.all.test[[i]] <- lst.all.test[[i]][order(row.names(lst.all.test[[i]])), ]
}

###############Section 4: Variable removal################################
rm(i, df.all, v.sample, mf.index, df.temp)

##############Part 2: Variable importance#################################
###############Section 1: In- and output##################################
lst.vi <- list()
m.varuse <- matrix(nrow = length(mf.list), ncol = length(v.var))
rownames(m.varuse) <- mf.list
colnames(m.varuse) <- v.var
m.varuse[, ] <- 0
m.varimp <- m.varuse

###############Section 2: Overview variable usage#########################
for (i in c(1:length(mf.list))){
  m.varuse[i, which(colnames(m.varuse) %in% names(lst.all.train[[i]]))] <- 1
}

###############Section 3: Variable importance#############################
for (i in c(1:length(mf.list))){
  message(paste0('Variable importance for ', mf.list[i]))
  df.train <- lst.all.train[[i]]
  index <- which(df.set$Macrophyte == mf.list[i])
  # lst.temp <- f.CForestVI(df.train, cols = c(5:ncol(df.train)), n.ntree = 10, 
  # n.mtry = 3, n.split = 0.1, n.leaf = 0.1, n.rep = 3)
  lst.temp <- f.CForestVI(df.train, cols = c(5:ncol(df.train)), n.ntree = df.set$Ntree[index], 
                          n.mtry = df.set$Mtry[index], n.split = df.set$Nsplit[index], 
                          n.leaf = df.set$Nleaf[index], n.rep = 10)
  m.varimp[i, which(colnames(m.varimp) %in% lst.temp[[4]]$Metric)] <- lst.temp[[4]]$Mean
  lst.vi[[i]] <- cbind(mf.list[i], lst.temp[[4]][order(lst.temp[[4]]$Mean, decreasing = T), ])
  names(lst.vi[[i]])[1] <- 'Macrophyte'
}
df.varimp <- melt(m.varimp)
names(df.varimp) <- c('Macrophyte', 'Variable', 'Ratio')
df.vi <- do.call('rbind', lst.vi)
v.var.imp <- sort(apply(m.varimp, 2, mean), decreasing = T)

###############Section 4: Plotting and saving#############################
# Heatmap
if(b.plot){
  df.varimp$Macrophyte <- factor(df.varimp$Macrophyte)
  p.VariableImportanceHeatMap <- 
    ggplot(df.varimp, aes(x = Variable, y = Macrophyte, fill = Ratio)) + 
    geom_tile() + 
    scale_fill_gradient(low = 'grey90', high = 'grey10') + 
    scale_y_discrete(limits = rev(levels(df.varimp$Macrophyte))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          legend.title = element_text(size = 9), 
          legend.text = element_text(size = 8), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_blank(), 
          axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1), 
          axis.text.y = element_text(face = 'italic'))
  plot(p.VariableImportanceHeatMap)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C7S4_VariableImportanceHeatMap.tiff'), units = 'mm', 
         width = 160, height = 180, res = 300, pointsize = 7)
    plot(p.VariableImportanceHeatMap)
    dev.off()
  }
}

# Importance scores, limited to 5 per species
if(b.plot){
  df.vi.sub <- c()
  for (i in c(1:length(mf.sel))){
    df.vi.sub <- rbind(df.vi.sub, df.vi[df.vi$Macrophyte == mf.sel[i], ][c(1:min(nrow(df.vi[df.vi$Macrophyte == mf.sel[i], ]), 5)), ])
  }
  df.vi.sub$Macrophyte <- factor(df.vi.sub$Macrophyte, mf.sel)
  df.vi.sub <- df.vi.sub[order(df.vi.sub$Mean, decreasing = T), ]
  # Make sure the variables are arranged per species, showing decreasing trend in importance
  df.vi.sub <- df.vi.sub %>%
    ungroup() %>%
    arrange(Macrophyte, Mean) %>%
    mutate(order = row_number())
  df.vi.sub$ymax <- ifelse(df.vi.sub$Mean + sqrt(10) * df.vi.sub$sd < 1, 
                           df.vi.sub$Mean + sqrt(10) * df.vi.sub$sd, 1)
  p.VariableImportanceRange <- ggplot(df.vi.sub, aes(x = order, y = Mean)) + 
    geom_col(fill = 'grey40') + 
    geom_errorbar(aes(ymin = Mean - sqrt(10) * sd, ymax = ymax), width = 0.2)  + 
    scale_x_reverse('', breaks = df.vi.sub$order, labels = df.vi.sub$Metric) + 
    scale_y_continuous('Model Improvement Ratio (-)') + 
    facet_grid(.~Macrophyte, scales = 'free_x', labeller = label_wrap_gen(width = 10)) +
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7),
          axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(face = 'italic', size = 8))
  plot(p.VariableImportanceRange)
  if(b.save){
    tiff(paste0(s.figs, 'F_C7S4_VariableImportanceRange.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.VariableImportanceRange)
    dev.off()
  }
}

if(b.save){
  write.table(df.varimp, paste0(s.data, 'D_C7S4_SI_VariableImportanceHeatMap.txt'), 
              row.names = F, sep = ',')
  write.table(df.vi, paste0(s.data, 'D_C7S4_VariableImportance.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(lst.vi, v.var, m.varuse, m.varimp, i, df.train, index, lst.temp, df.varimp, 
   df.vi, df.vi.sub, p.VariableImportanceHeatMap, p.VariableImportanceRange)

##############Part 3: Partial dependence (3 weeks)########################
###############Section 1: In- & output####################################
df.range <- as.data.frame(matrix(nrow = 5, ncol = 3))
names(df.range) <- c('Variable', 'Min', 'Max')
df.pdp <- c()
dir.create(paste0(s.data, 'PartialDependence'))

###############Section 2: Range definition################################
df.range$Variable <- names(v.var.imp)[1:nrow(df.range)]
for (i in c(1:length(mf.sel))){
  for (j in c(1:nrow(df.range))){
    if(length(which(names(lst.all.train[[i]]) == df.range$Variable[j])) > 0){
      if(is.na(df.range$Min[j])){ 
        df.range$Min[j] <- min(lst.all.train[[i]][, which(names(lst.all.train[[i]]) == df.range$Variable[j])])
      } else {
        df.range$Min[j] <- min(lst.all.train[[i]][, which(names(lst.all.train[[i]]) == df.range$Variable[j])], 
                               df.range$Min[j])
      }
      if(is.na(df.range$Max[j])){
        df.range$Max[j] <- max(lst.all.train[[i]][, which(names(lst.all.train[[i]]) == df.range$Variable[j])])
      } else {
        df.range$Max[j] <- max(lst.all.train[[i]][, which(names(lst.all.train[[i]]) == df.range$Variable[j])], 
                               df.range$Max[j])
      }
    }
  }
}

###############Section 3: Partial dependence##############################
for (i in c(1:length(mf.sel))){
  message(paste0('Partial dependence for ', mf.sel[i]))
  # Check if selected variables are within training data
  if(length(which(df.range$Variable %in% names(lst.all.train[[i]]))) > 0){ 
    df.train <- lst.all.train[[i]]
    index <- which(df.set$Macrophyte == mf.sel[i])
    # Model
    # lst.temp <- f.CForestPDP(df.train, cols = c(5:ncol(df.train)), 
    # range = df.range, n.ntree = 10, n.rep = 3)
    lst.temp <- f.CForestPDP(df.train, cols = c(5:ncol(df.train)), range = df.range, 
                             n.ntree = df.set$Ntree[index], n.mtry = df.set$Mtry[index], 
                             n.split = df.set$Nsplit[index], n.leaf = df.set$Nleaf[index], 
                             n.rep = 10, breaks = 21)
    df.pdp <- rbind(df.pdp, cbind.data.frame(mf.sel[i], lst.temp[[4]]))
    write.table(lst.temp[[4]], paste0(s.data, 'PartialDependence/D_C7S4_PartialDependence_', 
                                      sub(' ', '_', mf.sel[i]), '.txt', sep = ''), 
                row.names = F, sep = ',')
  } else {
    print('Data does not contain selected variables - No PDP performed')
  }
}
names(df.pdp)[1] <- 'Macrophyte'

###############Section 4: Plotting and saving#############################
df.pdp <- c()
for (i in c(1:length(mf.sel))){
  if(length(which(df.range$Variable %in% names(lst.all.train[[i]]))) > 0){
    message(paste0('Combining data for ', mf.sel[i]))
    df.temp <- read.table(paste0(s.data, 'PartialDependence/D_C7S4_PartialDependence_', 
                                 sub(' ', '_', mf.sel[i]), '.txt'), header = T, sep = ',')
    df.pdp <- rbind(df.pdp, cbind.data.frame(mf.sel[i], df.temp))
  } else {
    print('No PDP data')
  }
}
names(df.pdp)[1] <- 'Macrophyte'
if(b.plot){
  ##############Subsection: Species-specific
  df.pdp.s <- df.pdp[which(df.pdp$Macrophyte %in% mf.sel), ]
  df.pdp.s$Macrophyte <- factor(df.pdp.s$Macrophyte, mf.sel)
  df.pdp.s$Variable <- factor(df.pdp.s$Variable, c('Temperature', 'Nitrate', 'Oxygen', 
                                                   'Ammonium', 'pH'))
  levels(df.pdp.s$Variable) <- list('Temperature~("?C")' = 'Temperature', 
                                    'Nitrate*"-"*N~(mg%.%L^{"-"*1})' = 'Nitrate', 
                                    'Oxygen~(mg%.%L^{"-"*1})' = 'Oxygen', 
                                    'Ammonium*"-"*N~(mg%.%L^{"-"*1})' = 'Ammonium', 
                                    'pH~("-")' = 'pH')
  p.PartialDependence <- ggplot(df.pdp.s, aes(x = Value, y = Mean)) + 
    geom_ribbon(aes(ymin = Mean - sqrt(10) * SEM, ymax = Mean + sqrt(10) * SEM, 
                    group = Macrophyte), alpha = 0.2) + 
    geom_line(aes(linetype = Macrophyte, colour = Macrophyte)) + 
    scale_y_continuous('Mean Habitat Suitability Index') + 
    scale_color_manual(values = c('grey30', 'grey30', 'black', 'grey50', 'grey50'), 
                       labels = c('P. australis', 'L. minor', 'C. demersum', 
                                  'M. aquatica', 'L. minuta')) + 
    scale_linetype_manual(values = c('dashed', 'solid', 'dotted', 'dashed', 'solid'), 
                          labels = c('P. australis', 'L. minor', 'C. demersum', 
                                     'M. aquatica', 'L. minuta')) + 
    facet_grid(.~Variable, scales = 'free_x', labeller = labeller(Variable = label_parsed)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 7), 
          legend.position = c(0.9, 0.15), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.text = element_text(size = 8, face = 'italic'))
  plot(p.PartialDependence)
  ##############Subsection: All species
  df.pdp$Variable <- as.factor(df.pdp$Variable)
  df.pdp.all <- data.frame(matrix(nrow = nrow(unique(df.pdp[, c(2, 3)])), ncol = 4))
  colnames(df.pdp.all) <- c('Variable', 'Value', 'Mean', 'sd')
  w <- 1
  for (i in c(1:length(levels(df.pdp$Variable)))){
    df.temp <- df.pdp[df.pdp$Variable == levels(df.pdp$Variable)[i], ]
    for (j in unique(df.temp$Value)){
      df.pdp.all$Variable[w] <- levels(df.pdp$Variable)[i] 
      df.pdp.all$Value[w] <- j
      df.pdp.all$Mean[w] <- mean(df.temp$Mean[df.temp$Value == j])
      df.pdp.all$sd[w] <- sd(df.temp$Mean[df.temp$Value == j])
      w <- w + 1
    }
  }
  df.pdp.all$Variable <- factor(df.pdp.all$Variable, c('Temperature', 'Nitrate', 
                                                       'Oxygen', 'Ammonium', 'pH'))
  levels(df.pdp.all$Variable) <- list('Temperature~("?C")' = 'Temperature', 
                                      'Nitrate*"-"*N~(mg%.%L^{"-"*1})' = 'Nitrate', 
                                      'Oxygen~(mg%.%L^{"-"*1})' = 'Oxygen', 
                                      'Ammonium*"-"*N~(mg%.%L^{"-"*1})' = 'Ammonium', 
                                      'pH~("-")' = 'pH')
  p.PartialDependenceAll <- ggplot(df.pdp.all, aes(x = Value, y = Mean)) + 
    geom_ribbon(aes(ymin = Mean - sd, ymax = Mean + sd), alpha = 0.2) + 
    geom_line(colour = 'black', size = 0.4) + 
    scale_x_continuous('Variable value') + 
    scale_y_continuous('Habitat Suitability Index (-)') + 
    facet_wrap(~Variable, nrow = 1, ncol = 5, scales = 'free_x', labeller = label_parsed) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(size = 7))
  plot(p.PartialDependenceAll)
  if(b.save){
    tiff(paste0(s.figs, 'F_C7S4_PartialDependencePlot.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.PartialDependence)
    dev.off()
    tiff(paste0(s.figs, 'F_C7S4_PartialDependenceAll.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.PartialDependenceAll)
    dev.off()
  }
}
if(b.save){
  write.table(df.pdp, paste0(s.data, 'D_C7S4_PartialDependence.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(df.range, df.pdp, i, j, df.train, lst.temp, p.PartialDependence, 
   p.PartialDependenceAll)
