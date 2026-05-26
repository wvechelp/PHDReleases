##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 25/10/2019; Last changes: 25/10/2019
# See also Script C6S5
###############Section 2: Packages########################################
# install.packages('party') # For cForest modelling
# install.packages('PresenceAbsence') # For confusion matrix and metrics
# install.packages('reshape2') # For melting dataframes into 'long' mode
# install.packages('ggplot2') # For plotting
# install.packages('gridExtra')
# install.packages('ggpubr') 
# install.packages('doParallel')
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
  df.train <- rbind(df.pres.temp, df.abs.temp[which(df.abs.temp$PsA == 1), c(1:(ncol(df.abs.temp) - 1))])
  
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
      geom_hline(data = df.all[df.all$Type == 'Rep', ], aes(yintercept = Mean), colour = 'grey') +
      geom_hline(data = df.all[df.all$Type == 'Rep', ], aes(yintercept = Mean - sd), 
                 colour = 'grey', linetype = 'dotted') +
      geom_hline(data = df.all[df.all$Type == 'Rep', ], aes(yintercept = Mean + sd), 
                 colour = 'grey', linetype = 'dotted') +
      geom_point(data = df.all[df.all$Type=='CV', ], aes(n, Mean)) + 
      geom_pointrange(data = df.all[df.all$Type == 'CV', ], aes(n, Mean, ymin = Mean - sd, ymax = Mean + sd)) + 
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
f.CForest <- function(data, cols = c(1:ncol(data)), eval = NULL, 
                      n.ntree = 200, n.mtry = NULL, n.rep, n.cv = 5){
  # Define presences, absences and number of instances per fold
  df.pres.base <- data[data$Macrophyte >= 0.5, ]
  df.abs.base <- data[data$Macrophyte < 0.5, ]
  n.inst <- floor(min(nrow(df.pres.base), nrow(df.abs.base)) / n.cv)
  # Open different clusters for n
  cl <- makeCluster(availableCores() - 1); registerDoParallel(cl)
  df.out <- foreach(n=1:n.rep, .packages = c('party', 'PresenceAbsence'), 
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
                      controls = cforest_unbiased(ntree = n.ntree, mtry = floor(sqrt(length(cols)))))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, 
                            f.Performance(df.test, lab = 1, obs = (ncol(df.test) - 1), 
                                          pred = ncol(df.test))[[1]])
      if (!is.null(eval)){
        eval$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = eval))
        df.perf.eval <- rbind(df.perf.eval, 
                              f.Performance(eval, lab = 1, obs = (ncol(eval) - 1), 
                                            pred = ncol(eval))[[1]])
      }
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    t1 <- Sys.time()
    if (!is.null(eval)){
      df.perf.eval$k <- c(1:n.cv)
      df.perf.eval$n <- n
    }
    list(df.perf.temp, df.perf.eval, round(as.numeric(difftime(t1, t0, units = 'secs')), 4))
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.train <- df.perf.eval <- v.time <- c()
  for (i in c(1:n.rep)){
    df.perf.train <- rbind(df.perf.train, df.out[[i]][[1]])
    df.perf.eval <- rbind(df.perf.eval, df.out[[i]][[2]])
    v.time[i] <- df.out[[i]][[3]]
  }
  df.perf.train.sum <- f.SummaryPerf(df.perf.train)
  df.perf.eval.sum <- f.SummaryPerf(df.perf.eval)
  
  return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], 
              v.time, df.perf.eval, df.perf.eval.sum[df.perf.eval.sum$Type == 'Rep', c(1, 4, 5)]))
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
        cf <- cforest(Macrophyte~., data = df.train[, cols], controls = cforest_unbiased(ntree = n.ntree))  
      } else {
        cf <- cforest(Macrophyte~., data = df.train[, cols], 
                      controls = cforest_unbiased(ntree = n.ntree, mtry = n.mtry))
      }
      df.test$SI <- do.call('rbind', predict(cf, type = 'prob', newdata = df.test))
      df.perf.temp <- rbind(df.perf.temp, 
                            f.Performance(df.test, lab = 1, obs = (ncol(df.test) - 1), pred = ncol(df.test))[[1]])
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

###############Section 3: Creating species-specific data##################
lst.all <- list()
for (i in c(1:length(mf.list))){
  lst.all[[i]]<-df.all[, c(1:(mf.index - 1), which(names(df.all) == mf.list[i]))]
  names(lst.all[[i]])[ncol(lst.all[[i]])] <- 'Macrophyte'
}

###############Section 4: Variable removal################################
rm(i, df.all, mf.index)

##############Part 2: Data pre-processing (40 mins)#######################
###############Section 1: Threshold selection#############################
t.ol <- 3; t.psa <- 0.05; t.cor <- 0.7; t.imp <- 0.10
dir.create(paste0(s.data, 'DataTraining'))

###############Section 2: Pre-processing##################################
for (i in c(1:length(mf.list))){
  message(paste0('Pre-processing for ', mf.list[i]))
  df.temp <- lst.all[[i]]
  # Step 1: Removal of outliers
  v.up <- apply(df.temp[, c(5:24)], 2, 
                function(x) quantile(x, 0.75) + t.ol * (quantile(x, 0.75) - quantile(x, 0.25)))
  v.low <- apply(df.temp[, c(5:24)], 2, 
                 function(x) max(0, quantile(x, 0.25) - t.ol * (quantile(x, 0.75) - quantile(x, 0.25))))
  df.temp$OL <- 0
  for (j in c(1:ncol(df.temp[, c(5:24)]))){
    for (k in c(1:nrow(df.temp))){
      if(df.temp[k, (j + 4)] > v.up[j] | df.temp[k, (j + 4)] < v.low[j]){
        df.temp$OL[k] <- df.temp$OL[k] + 1
      }
    }
  }
  # Here, choice can be made which data is kept (0 -> each instance with outlier(s) removed)
  df.temp <- df.temp[which(df.temp$OL == 0), -ncol(df.temp)] 
  # Step 2: Removal of pseudoabsences
  df.temp <- f.RangeRemoval(df.temp, cols = c(5:24), quant = (1 - t.psa))
  # Step 3: Removal of correlated variables
  v.var <- names(df.temp[, c(5:24)])
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
        df.cor.temp$R2[v] <- cor(df.temp[, which(names(df.temp) == v.var[j])], 
                                 df.temp[, which(names(df.temp) == v.var[k])])
        v <- v + 1
      }
      df.corr <- rbind(df.corr, df.cor.temp)
    }
    df.corr <- df.corr[order(abs(df.corr$R2), decreasing = T), ]
    if(corr > t.cor){
      cor.var1 <- cor(df.temp[, which(names(df.temp) == df.corr$Var1[1])], df.temp$Macrophyte)
      cor.var2 <- cor(df.temp[, which(names(df.temp) == df.corr$Var2[1])], df.temp$Macrophyte)
      if (abs(cor.var1) >= abs(cor.var2)){
        v.var <- v.var[-which(v.var == df.corr$Var2[1])]
      } else {
        v.var <- v.var[-which(v.var == df.corr$Var1[1])]
      }
    }
    corr <- max(df.corr$R2)
  }
  df.temp <- df.temp[, c(1:4, which(names(df.temp) %in% v.var), ncol(df.temp))]
  # Step 4: Removal of redundant variables
  df.vi.temp <- f.CForestVI(df.temp, cols = c(5:ncol(df.temp)), n.ntree = 200, n.rep = 10)[[2]]
  v.var <- v.var[order(df.vi.temp$Mean, decreasing = F)][which(sort(df.vi.temp$Mean) / max(df.vi.temp$Mean) > t.imp)]
  df.temp <- df.temp[, c(c(1:4), which(names(df.temp) %in% v.var), ncol(df.temp))]
  # Step 5: Saving
  write.table(df.temp, file = paste0(s.data, 'DataTraining/D_C7S1_Data_', 
                                     sub(' ', '_', mf.list[i]), '.txt'), row.names = F, sep = ',')
}

###############Section 3: Variable removal################################
rm(t.ol, t.psa, t.cor, t.imp, df.temp, v.up, v.low, i, j, k, v.var, corr, 
   df.corr, df.cor.temp, cor.var1, cor.var2, v, df.vi.temp)
