##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 03/02/2020; Last changes: 03/02/2020
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
                      melt(df.sd, measure.vars = names(df.sd)[1:(ncol(df.sd) - 2)], id.vars = 'n')$value)
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
    df.perf.temp <- df.perf.eval <- df.si.all <- c()
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
      if(!is.null(eval)){
        eval$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = eval))
        df.si.all <- cbind(df.si.all, eval$SI)
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
    list(df.perf.temp, df.perf.eval, df.si.all)
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.train <- df.perf.eval <- df.si.all <- c()
  for (i in c(1:n.rep)){
    df.perf.train <- rbind(df.perf.train, df.out[[i]][[1]])
    if(!is.null(eval)){
      df.perf.eval <- rbind(df.perf.eval, df.out[[i]][[2]])
      df.si.all <- cbind(df.si.all, apply(df.out[[i]][[3]], 1, mean))
    }
  }
  df.perf.train.sum <- f.SummaryPerf(df.perf.train)
  if(!is.null(eval)){
    df.perf.eval.sum <- f.SummaryPerf(df.perf.eval)
    eval$SI <- apply(df.si.all, 1, mean)
    eval$sd <- apply(df.si.all, 1, function(x) sd(x) / sqrt(length(x)))
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
                df.perf.eval, df.perf.eval.sum[df.perf.eval.sum$Type == 'Rep', c(1, 4, 5)], eval))
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
df.set <- read.table(paste0(s.data, 'D_C7S3_OptimisedSettings.txt'), 
                     header = T, sep = ',')

###############Section 2: Listing macrophytes#############################
mf.list <- names(df.all)[mf.index:ncol(df.all)]
mf.sel <- c('Phragmites australis', 'Lemna minor', 'Ceratophyllum demersum', 
            'Mentha aquatica', 'Lemna minuta')

###############Section 3: Creating training and test data#################
lst.all.train <- lst.all.test <- list()
for (i in c(1:length(mf.list))){
  df.temp <- read.table(paste0(s.data, 'DataTraining/D_C7S1_Data_', 
                               sub(' ', '_', mf.list[i]), '.txt'), 
                        header = T, sep = ',')
  set.seed(621)
  v.sample <- sample(c(1:nrow(df.temp)), size = 0.1 * nrow(df.temp), replace = F)
  lst.all.train[[i]] <- df.temp[-v.sample, ]
  lst.all.test[[i]] <- df.temp[v.sample, ]
  lst.all.test[[i]] <- lst.all.test[[i]][order(row.names(lst.all.test[[i]])), ]
}

###############Section 4: Variable removal################################
rm(i, v.sample, mf.index, df.temp)

##############Part 2: Occurrence & Potential trend (13 hours)#############
###############Section 1: In- and output##################################
lst.all.si <- list()
df.trend <- c()

###############Section 2: Suitability assessment##########################
for (i in c(1:length(mf.list))){
  message(paste0('Suitability assessment for ', mf.list[i]))
  df.train <- lst.all.train[[i]]
  index <- which(df.set$Macrophyte == mf.list[i])
  df.test <- df.all[, which(names(df.all) %in% c(names(lst.all.train[[i]])[-1], mf.list[i]))]
  # lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), eval = df.test, 
  # n.rep = 3, n.ntree = 10, n.mtry = 2, n.split = 0.2, n.leaf = 0.01)
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), eval = df.test, 
                        n.rep = 10, n.ntree = df.set$Ntree[index], 
                        n.mtry = df.set$Mtry[index], 
                        n.split = df.set$Nsplit[index], 
                        n.leaf = df.set$Nleaf[index])
  lst.all.si[[i]] <- lst.temp[[5]]
}

###############Section 3: Trend identification############################
for (i in c(1:length(mf.list))){
  message(paste0('Trend assessment for ', mf.list[i]))
  df.si <- lst.all.si[[i]]
  names(df.si)[which(names(df.si) == mf.list[i])] <- 'Macrophyte'
  index <- which(df.set$Macrophyte == mf.list[i])
  # Determine year of sample
  df.si$Year <- do.call('rbind', strsplit(as.character(do.call('rbind', strsplit(as.character(df.si$Date), ' '))[, 1]), '/'))[, 3]
  df.si$Year <- factor(df.si$Year)
  v.year <- levels(df.si$Year)
  v.obs <- v.pred <- rep(0, length(v.year))
  # Convert predictions to discrete
  n.thresh <- f.OptDiscrThreshold(df.si, pred = which(names(df.si) == 'SI'), 
                                  obs = which(names(df.si) == 'Macrophyte'))[1]
  for (j in c(1:nrow(df.si))){ 
    df.si$Prediction[j] <- ifelse(df.si$SI[j] > n.thresh, 1, 0)
  }
  # Determine prevalence for observations and predictions
  for (j in c(1:length(v.year))){
    df.sub <- df.si[df.si$Year == v.year[j], ]
    v.obs[j] <- sum(df.sub$Macrophyte) / nrow(df.sub)
    v.pred[j] <- sum(df.sub$Prediction) / nrow(df.sub)
  }
  df.temp <- cbind.data.frame(mf.list[i], v.year, v.obs, v.pred)
  names(df.temp) <- c('Macrophyte', 'Year', 'Observations', 'Predictions')
  df.trend <- rbind(df.trend, df.temp)
}

###############Section 4: Plotting and saving#############################
if(b.plot){
  ##############Subsection: for selection of macrophytes
  df.trend.m <- melt(df.trend[which(df.trend$Macrophyte %in% mf.sel), ], 
                     measure.vars = c('Observations', 'Predictions'), 
                     id.vars = c('Macrophyte', 'Year'))
  df.trend.m$Macrophyte <- factor(df.trend.m$Macrophyte, mf.sel)
  df.trend.m$Year <- as.numeric(as.character(df.trend.m$Year))
  p.TemporalTrendAnalysis <- ggplot(df.trend.m[df.trend.m$Year >= 1980 & 
                                                 df.trend.m$Year < 2011, ], 
                                    aes(x = Year, y = value, group = variable)) + 
    geom_line(aes(linetype = variable)) + 
    scale_x_continuous(limits = c(1978, 2012), breaks = c(1980, 1990, 2000, 2010)) + 
    scale_y_continuous('Prevalence') + 
    facet_grid(.~Macrophyte, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(face = 'italic', size = 8), 
          legend.title = element_blank(), 
          legend.position = c(0.08, 0.93), 
          legend.background = element_blank(), 
          legend.text = element_text(size = 8))
  plot(p.TemporalTrendAnalysis)
  
  ##############Subsection: for all macrophytes
  df.trend.m <- melt(df.trend, measure.vars = c('Observations', 'Predictions'), 
                     id.vars = c('Macrophyte', 'Year'))
  df.trend.m$Macrophyte <- factor(df.trend.m$Macrophyte, mf.list)
  df.trend.m$Year <- as.numeric(as.character(df.trend.m$Year))
  p.TemporalTrendAnalysisAllP1 <- 
    ggplot(df.trend.m[df.trend.m$Macrophyte %in% mf.list[1:30] & 
                        df.trend.m$Year >= 1980 & df.trend.m$Year < 2011, ], 
           aes(x = Year, y = value, group = variable)) + 
    geom_line(aes(linetype = variable)) + 
    scale_x_continuous(limits = c(1978, 2012), breaks = c(1980, 1990, 2000, 2010)) + 
    scale_y_continuous('Prevalence') + 
    facet_wrap(~Macrophyte, ncol = 6, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() +  
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(face = 'italic', size = 8), 
          legend.title = element_blank(), 
          legend.position = c(0.08, 0.98), 
          legend.background = element_blank(), 
          legend.text = element_text(size = 8))
  plot(p.TemporalTrendAnalysisAllP1)
  
  p.TemporalTrendAnalysisAllP2 <- 
    ggplot(df.trend.m[df.trend.m$Macrophyte %in% mf.list[31:length(mf.list)] & 
                        df.trend.m$Year >= 1980 & df.trend.m$Year < 2011, ], 
           aes(x = Year, y = value, group = variable)) + 
    geom_line(aes(linetype = variable)) + 
    scale_x_continuous(limits = c(1978, 2012), breaks = c(1980, 1990, 2000, 2010)) + 
    scale_y_continuous('Prevalence') + 
    facet_wrap(~Macrophyte, ncol = 6, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.text = element_text(face = 'italic', size = 8), 
          legend.title = element_blank(), 
          legend.position = c(0.08, 0.98), 
          legend.background = element_blank(), 
          legend.text = element_text(size = 8))
  plot(p.TemporalTrendAnalysisAllP2)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C7S5_TemporalTrendPrevalence.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.TemporalTrendAnalysis)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C7S5_TemporalPrevalenceAllSpecies1.tiff'), 
         units = 'mm', width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.TemporalTrendAnalysisAllP1)
    dev.off()
    tiff(paste0(s.figs, 'F_SI_C7S5_TemporalPrevalenceAllSpecies2.tiff'), 
         units = 'mm', width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.TemporalTrendAnalysisAllP2)
    dev.off()
  }
}

if(b.save){
  write.table(df.trend, paste0(s.data, 'D_C7S5_TemporalTrendPrevalence.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(i, df.train, lst.temp, df.si, v.year, v.obs, v.pred, n.thresh, j, df.sub, 
   df.temp, df.trend.m, p.TemporalTrendAnalysis)

##############Part 3: Comparison Lemnas (1 hour)##########################
###############Section 1: In- and output##################################
mf.sub <- c('Lemna minor', 'Lemna minuta')
lst.all.si <- list()
df.threat <- df.all$Mp

###############Section 2: Suitability Assessment##########################
for (i in c(1:length(mf.sub))){
  message(paste0('Suitability assessment for ', mf.sub[i]))
  df.train <- lst.all.train[[which(mf.sel == mf.sub[i])]]
  index <- which(df.set$Macrophyte == mf.sub[i])
  df.test <- df.all[, which(names(df.all) %in% 
                              c(names(lst.all.train[[which(mf.sel == mf.sub[i])]])[-1], mf.sub[i]))]
  # lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), eval = df.test, 
  # n.rep = 3, n.ntree = 10, n.mtry = 2, n.split = 0.2, n.leaf = 0.01)
  lst.temp <- f.CForest(df.train, cols = c(5:ncol(df.train)), eval = df.test, 
                        n.rep = 10, n.ntree = df.set$Ntree[index], 
                        n.mtry = df.set$Mtry[index], 
                        n.split = df.set$Nsplit[index], 
                        n.leaf = df.set$Nleaf[index])
  lst.all.si[[i]] <- lst.temp[[5]]
  names(lst.all.si[[i]])[which(names(lst.all.si[[i]]) == mf.sub[i])] <- 'Macrophyte'
  df.temp <- cbind.data.frame(lst.all.si[[i]]$Macrophyte, lst.all.si[[i]]$SI)
  names(df.temp)[c(ncol(df.temp) - 1, ncol(df.temp))] <- 
    c(paste0('Obs', as.character(strsplit(mf.sub[i], ' ')[[1]][2])),
      paste0('Pred', as.character(strsplit(mf.sub[i], ' ')[[1]][2])))
  df.threat <- cbind(df.threat, df.temp)
}

###############Section 3: Plotting and saving#############################
if(b.plot){
  for (i in c(1:nrow(df.threat))){
    df.threat$Obsminor[i] <- ifelse(df.threat$Obsminor[i] == 1, 'Present', 'Absent')
    df.threat$Obsminuta[i] <- ifelse(df.threat$Obsminuta[i] == 1, 'Present', 'Absent')
  }
  df.threat$Obsminor <- factor(df.threat$Obsminor)
  df.threat$Obsminuta <- factor(df.threat$Obsminuta)
  p.SpeciesContrast <- ggplot(df.threat, aes(x = Predminor, y = Predminuta)) + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_point(size = 0.4) + 
    scale_x_continuous(expression(paste('Suitability score for ', italic('Lemna minor'))), limits = c(0, 1)) + 
    scale_y_continuous(expression(paste('Suitability score for ', italic('Lemna minuta'))), limits = c(0, 1), 
                       position = 'right') + 
    facet_grid(Obsminuta~Obsminor, switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          legend.position = 'none')
  plot(p.SpeciesContrast)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C7S5_InvasionThreatLemna.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.SpeciesContrast)
    dev.off()
  }
}
if(b.save){
  write.table(df.threat, paste0(s.data, 'D_C7S5_InvasionThreatLemna.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(mf.sel.sub, lst.all.si, df.threat, i, df.train, index, df.test, lst.temp, 
   df.temp, p.SpeciesContrast)
