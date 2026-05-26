##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 31/01/2020; Last changes: 31/01/2020
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
library(reshape2); 
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
    df.temp[i, 7] <- 1 / 
      abs(sensitivity(confusion, st.dev = F) - specificity(confusion, st.dev = F))
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
  
  return(df.best[,c(1:6)])
}
f.Performance <- function(data, lab = 1, obs, pred, thresh = NULL, metric = 'MinSnSp'){
  v.perf <- as.data.frame(matrix(ncol = 7, nrow = 1))
  names(v.perf) <- c('AUC', 'Kappa', 'Sn', 'Sp', 'TSS', 'F1', 'R2')
  v.perf[1] <- auc(data[, c(lab,obs,pred)], st.dev = F)
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
  df.mean <- c(); df.sd <- c()
  for (i in c(1:max(data$n))){
    df.mean <- as.data.frame(rbind(df.mean, apply(data[data$n == i, ], 2, mean)))
    df.sd <- as.data.frame(rbind(df.sd, apply(data[data$n == i, ], 2, sd)))
  }
  # Melt together in general dataframe and add information on type
  df.cv.perf <- cbind(melt(df.mean, measure.vars = names(df.mean)[1:(ncol(df.mean) - 2)], 
                           id.vars = 'n'), melt(df.sd, measure.vars = names(df.sd)[1:(ncol(df.sd)-2)], 
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
  if(plot){
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
f.CForestCV <- function(data, cols = c(1:ncol(data)), n.ntree = 200, 
                        n.mtry = NULL, n.cv = 5, seed = 621){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  lst.data <- list()
  # Make different folds
  for (i in c(1:n.cv)){
    set.seed(seed)
    s.pres <- sample(c(1:nrow(df.pres.base)), size = n.inst, replace = F)
    set.seed(seed)
    s.abs <- sample(c(1:nrow(df.pres.base)), size = n.inst, replace = F)
    lst.data[[i]] <- rbind(df.pres.base[s.pres, ], df.abs.base[s.abs, ])
    df.pres.base <- df.pres.base[-s.pres, ]
    df.abs.base <- df.abs.base[-s.abs, ]
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
    set.seed(seed)
    if (is.null(n.mtry)){
      cf <- cforest(Macrophyte~., data = df.train[, cols], 
                    controls = cforest_unbiased(ntree = n.ntree, 
                                                mtry = floor(sqrt(length(cols)))))  
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
  df.perf.temp
  return(df.perf.temp)
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
mf.index <- min(c(which('Absent' == df.all[1, ]), which('Present' == df.all[1, ])))
df.all[, c(mf.index:ncol(df.all))] <- ifelse(df.all[, c(mf.index:ncol(df.all))] == 'Present', 1, 0)
df.all[, c(5:ncol(df.all))] <- apply(df.all[, c(5:ncol(df.all))], 2, as.numeric)
names(df.all) <- gsub('[.]', ' ', names(df.all))

###############Section 2: Listing macrophytes#############################
mf.list <- names(df.all)[mf.index:ncol(df.all)]
mf.sel <- c('Phragmites australis', 'Lemna minor', 'Ceratophyllum demersum', 
            'Mentha aquatica', 'Lemna minuta')

###############Section 3: Creating training and test data#################
set.seed(621)
v.sample <- sample(c(1:nrow(df.all)), size = 0.1 * nrow(df.all), replace = F)
lst.all.train <- lst.all.test <- list()
for (i in c(1:length(mf.sel))){
  lst.all.train[[i]] <- df.all[-v.sample, c(1:(mf.index - 1),
                                            which(names(df.all) == mf.sel[i]))]
  names(lst.all.train[[i]])[ncol(lst.all.train[[i]])] <- 'Macrophyte'
  lst.all.test[[i]] <- df.all[v.sample,c(1:(mf.index - 1),
                                         which(names(df.all) == mf.sel[i]))]
  names(lst.all.test[[i]])[ncol(lst.all.test[[i]])] <- 'Macrophyte'
  lst.all.test[[i]] <- lst.all.test[[i]][order(row.names(lst.all.test[[i]])), ]
}

###############Section 4: Variable removal################################
rm(i, v.sample, mf.index)

##############Part 2: Selection of baseline Ntree (6.5 hours)#############
###############Section 1: In- and output##################################
n.ntree <- seq(50, 1000, 50)
lst.perf.ntree <- list()
df.perf.ntree <- as.data.frame(matrix(nrow = length(n.ntree), ncol = 12))
names(df.perf.ntree) <- c('Macrophyte', 'ntree', 'AUC', 'sd_AUC', 'Kappa', 
                          'sd_Kappa', 'Sn', 'sd_Sn', 'Sp', 'sd_Sp', 'R2', 'sd_R2')

###############Section 2: Testing Ntree range#############################
for (i in c(1:length(mf.sel))){
  message(paste0('Ntree range for ', mf.sel[i]))
  for (j in c(1:length(n.ntree))){
    df.train <- lst.all.train[[i]]
    df.temp <- f.CForestCV(df.train, cols = c(5:ncol(df.train)), 
                           n.ntree = n.ntree[j], n.mtry = NULL)
    # Fill performance matrix
    df.perf.ntree$Macrophyte[j] <- mf.sel[i]
    df.perf.ntree$ntree[j] <- n.ntree[j]
    df.perf.ntree$AUC[j] <- mean(df.temp$AUC)
    df.perf.ntree$sd_AUC[j] <- sd(df.temp$AUC)
    df.perf.ntree$Kappa[j] <- mean(df.temp$Kappa)
    df.perf.ntree$sd_Kappa[j] <- sd(df.temp$Kappa)
    df.perf.ntree$Sn[j] <- mean(df.temp$Sn)
    df.perf.ntree$sd_Sn[j] <- sd(df.temp$Sn)
    df.perf.ntree$Sp[j] <- mean(df.temp$Sp)
    df.perf.ntree$sd_Sp[j] <- sd(df.temp$Sp)
    df.perf.ntree$R2[j] <- mean(df.temp$R2)
    df.perf.ntree$sd_R2[j] <- sd(df.temp$R2)
  }
  lst.perf.ntree[[i]] <- df.perf.ntree
}
df.perf.ntree <- do.call('rbind', lst.perf.ntree)

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.perf.ntree$Macrophyte <- factor(df.perf.ntree$Macrophyte, mf.sel)
  p.NtreePerformance <- ggplot(df.perf.ntree, aes(ntree, AUC)) + 
    geom_vline(xintercept = 200, linetype = 'dashed', colour = 'grey70') + 
    geom_line() +
    geom_ribbon(aes(ymin = AUC-sd_AUC, ymax = AUC + sd_AUC), alpha = 0.2) + 
    scale_x_continuous('Number of trees (-)') + 
    scale_y_continuous('AUC (-)', limits = c(0.6, 1)) + 
    facet_grid(.~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          strip.text.x = element_text(face = 'italic'))
  plot(p.NtreePerformance)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S2_SettingsNtreePerformance.tiff'), 
         units = 'mm', width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.NtreePerformance)
    dev.off()
  }
}
if(b.save){
  write.table(df.perf.ntree, file = paste0(s.data, 'D_C6S2_SettingsNtreePerformance.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(n.ntree, lst.perf.ntree, i, j, df.temp, df.perf.ntree, p.NtreePerformance)

##############Part 3: Selection of repetitions (1 hour)###################
###############Section 1: In- and output##################################
n.rep <- 30
lst.perf.nrep <- list()
df.perf.nrep <- as.data.frame(matrix(nrow = n.rep, ncol = 12))
names(df.perf.nrep) <- c('Macrophyte', 'nrep', 'AUC', 'sd_AUC', 'Kappa', 
                         'sd_Kappa', 'Sn', 'sd_Sn', 'Sp', 'sd_Sp', 'R2', 'sd_R2')

###############Section 2: Testing repetitions#############################
for (i in c(1:length(mf.sel))){
  message(paste0('Repetitions for ', mf.sel[i]))
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.perf.temp <- foreach(n = 1:n.rep, .combine = 'rbind', 
                          .packages = c('party', 'PresenceAbsence'), 
                          .export = c('f.CForestCV', 'f.Performance')) %dopar% {
    df.train <- lst.all.train[[i]]
    df.temp <- f.CForestCV(df.train, cols = c(5:ncol(df.train)), n.ntree = 200, 
                           n.mtry = NULL, seed = n); df.temp$n <- n
    df.temp
  }
  stopCluster(cl)
  for (j in c(1:n.rep)){
    df.temp.sum <- f.SummaryPerf(df.perf.temp[df.perf.temp$n <= j, ])
    # Fill performance matrix
    df.temp <- df.temp.sum[df.temp.sum$Type == 'Rep', ]
    df.perf.nrep$Macrophyte[j] <- mf.sel[i]
    df.perf.nrep$nrep[j] <- j
    df.perf.nrep$AUC[j] <- df.temp$Mean[df.temp$Metric == 'AUC']
    df.perf.nrep$sd_AUC[j] <- df.temp$sd[df.temp$Metric == 'AUC']
    df.perf.nrep$Kappa[j] <- df.temp$Mean[df.temp$Metric == 'Kappa']
    df.perf.nrep$sd_Kappa[j] <- df.temp$sd[df.temp$Metric == 'Kappa']
    df.perf.nrep$Sn[j] <- df.temp$Mean[df.temp$Metric == 'Sn']
    df.perf.nrep$sd_Sn[j] <- df.temp$sd[df.temp$Metric == 'Sn']
    df.perf.nrep$Sp[j] <- df.temp$Mean[df.temp$Metric == 'Sp']
    df.perf.nrep$sd_Sp[j] <- df.temp$sd[df.temp$Metric == 'Sp']
    df.perf.nrep$R2[j] <- df.temp$Mean[df.temp$Metric == 'R2']
    df.perf.nrep$sd_R2[j] <- df.temp$sd[df.temp$Metric == 'R2']
  }
  lst.perf.nrep[[i]] <- df.perf.nrep
}
df.perf.nrep <- do.call('rbind', lst.perf.nrep)

###############Section 3: Plotting########################################
if(b.plot){
  df.perf.nrep$Macrophyte <- factor(df.perf.nrep$Macrophyte, mf.sel)
  p.NrepPerformance <- ggplot(df.perf.nrep, aes(nrep,AUC)) + 
    geom_vline(xintercept = 10, linetype = 'dashed', colour = 'grey70') + 
    geom_line() + 
    geom_ribbon(aes(ymin = AUC - sd_AUC, ymax = AUC + sd_AUC), alpha = 0.2) + 
    scale_x_continuous('Number of repetitions (-)') + 
    scale_y_continuous('AUC (-)', limits = c(0.7, 0.9)) + 
    facet_grid(.~Macrophyte, scales = 'free_y', switch = 'y', 
               labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          strip.text.x = element_text(face = 'italic'))
  plot(p.NrepPerformance)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C6S2_SettingsNrepPerformance.tiff'), 
         units = 'mm', width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.NrepPerformance)
    dev.off()
  }
}

if(b.save){
  write.table(df.perf.nrep, file = paste0(s.data, 'D_C6S2_SettingsNrepPerformance.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(n.rep, lst.perf.nrep, df.perf.temp, df.temp, df.perf.nrep, i, j, 
   p.NrepPerformance)