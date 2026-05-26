##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 15/07/2019; Last changes: 22/10/2019
###############Section 2: Packages########################################
# install.packages('party') # For cForest modelling
# install.packages('PresenceAbsence') # For confusion matrix and metrics
# install.packages('reshape2') # For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
# install.packages('gridExtra')
# install.packages('future') # For parallel computing on HPC
###############Section 3: Libraries#######################################
library(party)
library(PresenceAbsence)
library(reshape2)
library(ggplot2)
library(gridExtra)
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
    df.mean <- as.data.frame(rbind(df.mean,apply(data[data$n == i, ], 2, mean)))
    df.sd <- as.data.frame(rbind(df.sd, apply(data[data$n == i, ], 2, sd)))
  }
  # Melt together in general dataframe and add information on type
  df.cv.perf <- cbind(melt(df.mean,measure.vars = names(df.mean)[1:(ncol(df.mean) - 2)], id.vars = 'n'), 
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
                      controls = cforest_unbiased(ntree = n.ntree, mtry = floor(sqrt(length(cols))), 
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

##############Part 2: Selection starting point (33 hours)#################
###############Section 1: In- and output##################################
n.ntree <- seq(100, 1000, 10)
n.mtry <- seq(2, 20, 1)
n.split <- seq(0.01, 0.5, 0.01)
n.leaf <- seq(0.01, 0.25, 0.01)
df.settings <- expand.grid(n.ntree, n.mtry, n.split, n.leaf)
names(df.settings) <- c('Ntree', 'Mtry', 'Nsplit', 'Nleaf')
set.seed(621)
df.set.sub <- df.settings[sample(c(1:nrow(df.settings)), 
                                 nrow(df.settings), replace = F), ][sample(c(1:nrow(df.settings)), 60, replace = F), ]
df.set.sub <- df.set.sub[order(df.set.sub$Ntree, decreasing = F), ]
lst.set.perf <- list()
df.set.start <- c()
dir.create(paste0(s.data, 'StartSettings'))

###############Section 2: Starting point selection########################
for (i in c(1:length(mf.list))){
  message(paste0('Starting point selection for ', mf.list[i]))
  df.train <- lst.all.train[[i]]
  df.temp <- c()
  for (j in c(1:nrow(df.set.sub))){
    lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), 
                          n.ntree = df.set.sub$Ntree[j], 
                          n.mtry = df.set.sub$Mtry[j], 
                          n.split = df.set.sub$Nsplit[j], 
                          n.leaf = df.set.sub$Nleaf[j], 
                          n.rep = 10, n.cv = 5)
    df.temp <- rbind(df.temp, f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
  }
  lst.set.perf[[i]] <- cbind.data.frame(mf.list[i], df.set.sub, df.temp)
  names(lst.set.perf[[i]])[1] <- 'Macrophyte'
  write.table(lst.set.perf[[i]], paste0(s.data, 'StartSettings/D_C7S3_SI_Settings_', 
                                        sub(' ', '_', mf.list[i]), '.txt'), 
              row.names = F, sep = ',')
  df.temp <- lst.set.perf[[i]][which(lst.set.perf[[i]]$AUC == max(lst.set.perf[[i]]$AUC)), ] # Selects all cases with 'maximal' performance
  df.set.start <- rbind(df.set.start, df.temp[which.min(df.temp$Ntree), ]) # Selects lowest extent in case of similar performance
}
df.set.perf <- do.call('rbind', lst.set.perf)

###############Section 3: Plotting and Saving#############################
if(b.save){
  write.table(df.set.start, paste0(s.data, 'D_C7S3_StartingPointBestPerformance.txt'),
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(df.set.sub, lst.set.perf, df.temp, df.set.start, df.train, df.set.perf)

##############Part 3: Further optimisation (5 hours)######################
###############Section 1: In- and output##################################
df.set.start <- read.table(paste0(s.data, 'D_C7S3_StartingPointBestPerformance.txt'), 
                           header = T, sep = ',')
lst.set.perf <- list()
df.set.opt <- c()
dir.create(paste0(s.data, 'Optimisation'))

###############Section 2: Iterative selection#############################
for (i in c(1:length(mf.list))){
  message(paste0('Setting optimisation for ', mf.list[i]))
  # Define training set and starting settings
  df.train <- lst.all.train[[i]]
  df.perf.temp <- df.set.start[df.set.start$Macrophyte == mf.list[i], ]
  # Start iteration
  x <- y <- 0
  while (x < 3 & y < 6){
    print(paste0('Iteration ', y + 1))
    # Define temporary best settings and reference AUC value
    df.set.best <- df.perf.temp[which.max(df.perf.temp$AUC), ]
    # Define new individual settings
    ntree.new <- c(df.set.best$Ntree - (200) * ((1 / 2)^(x)), df.set.best$Ntree, 
                   df.set.best$Ntree + (200) * ((1 / 2)^(x)))
    ntree.new <- ntree.new[ntree.new > 0]
    mtry.new <- c(df.set.best$Mtry - (4) * ((1 / 2)^(x)), df.set.best$Mtry, 
                  df.set.best$Mtry + (4) * ((1 / 2)^(x)))
    mtry.new <- mtry.new[mtry.new > 1 & mtry.new < 21]
    split.new <- c(df.set.best$Nsplit - (0.2) * ((1 / 2)^(x)), df.set.best$Nsplit, 
                   df.set.best$Nsplit + (0.2) * ((1 / 2)^(x)))
    split.new <- split.new[split.new > 0 & split.new <= 1]
    leaf.new <- c(df.set.best$Nleaf - (0.2) * ((1 / 2)^(x)), df.set.best$Nleaf, 
                  df.set.best$Nleaf + (0.2) * ((1 / 2)^(x)))
    leaf.new <- leaf.new[leaf.new > 0 & leaf.new <= 1]
    # Expand settings and check if already calculated
    df.set.new <- expand.grid(ntree.new, mtry.new, split.new, leaf.new)
    names(df.set.new) <- c('Ntree', 'Mtry', 'Nsplit', 'Nleaf')
    df.set.old <- df.perf.temp[, c('Ntree', 'Mtry', 'Nsplit', 'Nleaf')]
    df.set.all <- unique(rbind(df.set.old, df.set.new))
    df.set.new <- df.set.all[-c(1:nrow(df.set.old)), ]
    df.set.new <- df.set.new[order(df.set.new$Ntree, decreasing = T), ]
    if(nrow(df.set.new) == 0){ break }
    # Model for each combination
    df.set.perf <- c()
    for (j in c(1:nrow(df.set.new[1:4, ]))){
      lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), 
                            n.ntree = df.set.new$Ntree[j], 
                            n.mtry = df.set.new$Mtry[j], 
                            n.split = df.set.new$Nsplit[j], 
                            n.leaf = df.set.new$Nleaf[j], n.rep = 10)
      df.set.perf <- rbind(df.set.perf, f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3))
    }
    # Combine data in preferred format
    df.set.perf <- cbind.data.frame(mf.list[i], df.set.new[1:4, ], df.set.perf)
    names(df.set.perf)[1] <- 'Macrophyte'
    df.perf.temp <- rbind(df.perf.temp, df.set.perf)
    # Check if same settings (setting-wise comparison) give best performance
    if(sum(df.set.best == df.perf.temp[which.max(df.perf.temp$AUC), ]) == ncol(df.perf.temp)){
      x <- x + 1
    }
    y <- y + 1
  }
  # Save macrophyte-specific settings
  lst.set.perf[[i]] <- df.perf.temp
  write.table(df.perf.temp, paste0(s.data, 'Optimisation/D_C7S3_SettingOptimisation_', 
                                   sub(' ', '_', mf.list[i]), '.txt'), 
              row.names = F, sep = ',')
  df.set.opt <- rbind(df.set.opt, df.perf.temp[which.max(df.perf.temp$AUC), ])
}

###############Section 3: Saving and plotting#############################
if(b.save){
  write.table(df.set.opt, paste0(s.data, 'D_C7S3_OptimisedSettings.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(df.train, df.perf.temp, x, y, df.set.best, ntree.new, mtry.new, split.new, 
   leaf.new, df.set.new, df.set.old, df.set.perf, lst.set.perf)

##############Part 4: Evaluation optimised settings (2 hours)#############
###############Section 1: In- and output##################################
df.set <- read.table(paste0(s.data, 'D_C7S3_OptimisedSettings.txt'), 
                     header = T, sep = ',')
df.info.t <- data.frame(matrix(nrow = length(mf.list), ncol = 4))
colnames(df.info.t) <- c('Macrophyte', 'Series', 'Instances', 'Features')
df.perf.t <- data.frame(matrix(nrow = length(mf.list), ncol = 14))
df.info.e <- data.frame(matrix(nrow = length(mf.list), ncol = 4))
colnames(df.info.e) <- c('Macrophyte', 'Series', 'Instances', 'Features')
df.perf.e <- data.frame(matrix(nrow = length(mf.list), ncol = 14))

###############Section 2: Modelling#######################################
for (i in c(1:length(mf.list))){
  message(paste0('Final model for ', mf.list[i]))
  index <- which(df.set$Macrophyte == mf.list[i])
  df.train <- lst.all.train[[i]]
  df.test <- lst.all.test[[i]]
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), eval = df.test, 
                        n.ntree = df.set$Ntree[index], 
                        n.mtry = df.set$Mtry[index], 
                        n.split = df.set$Nsplit[index], 
                        n.leaf = df.set$Nleaf[index], n.rep = 10)
  # Store results
  df.info.t[i, ] <- c(mf.list[i], 'Training', nrow(df.train), ncol(df.train))
  df.perf.t[i, ] <- f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)
  if(i == 1){ names(df.perf.t) <- names(f.PerfLongWide(lst.temp[[2]], mean = 2, sd = 3)) }
  df.info.e[i, ] <- c(mf.list[i], 'Testing', nrow(df.test), ncol(df.test))
  df.perf.e[i, ] <- f.PerfLongWide(lst.temp[[4]], mean = 2, sd = 3)
  if(i == 1){ names(df.perf.e) <- names(f.PerfLongWide(lst.temp[[4]], mean = 2, sd = 3)) }
}
df.perf.t <- cbind.data.frame(df.info.t, df.perf.t)
df.perf.e <- cbind.data.frame(df.info.e, df.perf.e)
df.perf <- rbind(df.perf.t, df.perf.e)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.perf.m <- cbind(melt(df.perf, id.vars = c('Macrophyte', 'Series'), 
                          measure.vars = c('AUC', 'Kappa', 'Sn', 'Sp')), 
                     melt(df.perf, id.vars = c('Macrophyte', 'Series'), 
                          measure.vars = c('sd_AUC', 'sd_Kappa', 'sd_Sn', 'sd_Sp'))$value)
  names(df.perf.m) <- c('Macrophyte', 'Series', 'Metric', 'Mean', 'sd')
  p.EvaluationPerformance <- ggplot(df.perf.m, aes(x = Macrophyte, y = Mean)) + 
    geom_pointrange(aes(ymin = Mean - sd, ymax = Mean + sd,colour = Series)) + 
    scale_y_continuous('') + 
    scale_color_manual(values = c('Training' = 'black', 'Testing' = 'grey')) +
    facet_grid(Metric~., scales = 'free_y', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black'), 
          axis.text.x = element_text(face = 'italic', angle = 60, hjust = 1, vjust = 1), 
          strip.background = element_blank(), 
          strip.placement = 'outside')
  plot(p.EvaluationPerformance)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C7S3_PerformanceOptimisedSettings.tiff'), 
         units = 'mm', width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.EvaluationPerformance)
    dev.off()
  }
}
if(b.save){
  write.table(df.perf, paste0(s.data, 'D_C7S3_PerformanceEvaluation.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(df.set, i, df.train, df.test, index, df.perf.m, p.EvaluationPerformance)