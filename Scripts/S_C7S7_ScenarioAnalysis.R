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
  v.perf[1] <- auc(data[, c(lab,obs, pred)], st.dev = FALSE)
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
f.CForestScenario <- function(data, cols = c(1:ncol(data)), eval = NULL, 
                              n.ntree = 200, n.mtry = NULL, n.split = 0.2, 
                              n.leaf = 0.01, n.rep, n.cv = 5){
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
      }
    }
    df.perf.temp$k <- c(1:n.cv)
    df.perf.temp$n <- n
    list(df.perf.temp, df.si.all)
  }
  registerDoSEQ(); stopCluster(cl)
  df.perf.train <- df.si.all <- c()
  for (i in c(1:n.rep)){
    df.perf.train <- rbind(df.perf.train, df.out[[i]][[1]])
    if(!is.null(eval)){
      df.si.all <- cbind(df.si.all, apply(df.out[[i]][[2]], 1, mean))
    }
  }
  df.perf.train.sum <- f.SummaryPerf(df.perf.train)
  if(!is.null(eval)){
    eval$SI <- apply(df.si.all, 1, mean)
    eval$sd <- apply(df.si.all, 1, function(x) sd(x) / sqrt(length(x)))
    
    return(list(df.perf.train, df.perf.train.sum[df.perf.train.sum$Type == 'Rep', c(1, 4, 5)], eval))
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
df.chem <- df.all[, c(1:(mf.index - 1))]

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
rm(i, df.all, v.sample, mf.index, df.temp)

##############Part 2: Description of variables############################
###############Section 1: In- and output##################################
df.chem[, c(5:ncol(df.chem))] <- NA

###############Section 2: Combining data##################################
for (i in c(1:length(mf.list))){
  for (j in c(1:nrow(lst.all.train[[i]]))){
    n.ind.mp <- which(paste(df.chem$Mp, df.chem$Date) == paste(lst.all.train[[i]]$Mp[j], lst.all.train[[i]]$Date[j]))
    for (k in c(5:(ncol(lst.all.train[[i]])-1))){
      n.ind.var <- which(names(df.chem) == names(lst.all.train[[i]])[k])
      if(is.na(df.chem[n.ind.mp, n.ind.var])){
        df.chem[n.ind.mp, n.ind.var] <- lst.all.train[[i]][j, k]
      }
    }
  }
}

###############Section 3: Temporal trends#################################
df.time <- df.chem[, which(apply(df.chem, 2, function(x) sum(is.na(x))) != nrow(df.chem))]
df.time$Date <- do.call('rbind', strsplit(as.character(do.call('rbind', strsplit(as.character(df.chem$Date), ' '))[, 1]), '/'))[, 3]

################Subsection: Linear models for temporal trend
df.lm<-data.frame(matrix(nrow = length(names(df.time)[c(5:ncol(df.time))]),ncol = 7)); names(df.lm)<-c('Variable','Intercept','Coefficient','CI_L_Int','CI_U_Int','CI_L_Coeff','CI_U_Coeff')
df.lm$Variable<-names(df.time)[c(5:ncol(df.time))]
for (i in c(1:nrow(df.lm))){
  lm.temp<-lm(df.time[,which(names(df.time) == df.lm$Variable[i])]~as.numeric(as.character(df.time$Date)))
  df.lm$Intercept[i]<-lm.temp$coefficients[1]; df.lm$Coefficient[i]<-lm.temp$coefficients[2]
  ci.temp<-confint(lm.temp)
  df.lm$CI_L_Int[i]<-ci.temp[1,1]; df.lm$CI_U_Int[i]<-ci.temp[1,2]; df.lm$CI_L_Coeff[i]<-ci.temp[2,1]; df.lm$CI_U_Coeff[i]<-ci.temp[2,2]
}
################Subsection: Average value per year
df.time$Date<-as.factor(df.time$Date)
df.avg<-df.sd<-data.frame(matrix(nrow = length(levels(df.time$Date)),ncol = (ncol(df.time)-3)))
names(df.avg)<-names(df.sd)<-c('Year',names(df.time)[c(5:ncol(df.time))])
for (i in c(1:nrow(df.avg))){
  df.avg$Year[i]<-df.sd$Year[i]<-levels(df.time$Date)[i]
  df.temp<-df.time[df.time$Date == levels(df.time$Date)[i],]
  df.avg[i,c(2:ncol(df.avg))]<-apply(df.temp[,c(5:ncol(df.temp))],2,function(x) mean(x,na.rm = TRUE))
  df.sd[i,c(2:ncol(df.sd))]<-apply(df.temp[,c(5:ncol(df.temp))],2,function(x) sd(x,na.rm = TRUE))
}
df.trend<-cbind.data.frame(melt(df.avg,id.vars = 'Year',measure.vars = names(df.avg)[-1]),melt(df.sd,id.vars = 'Year',measure.vars = names(df.sd)[-1])$value)
names(df.trend)<-c('Year','Variable','Mean','sd')
###############Section 4: Plotting and saving#############################
df.chem.red <- df.chem[,which(apply(df.chem, 2, function(x) sum(is.na(x))) < nrow(df.chem))]
summary(df.chem.red[, c(5:ncol(df.chem.red))])
if(b.plot){
  df.chem.m <- melt(df.chem.red, id.vars = c('Mp', 'Date'), 
                    measure.vars = names(df.chem.red)[5:ncol(df.chem.red)])
  p.DataStatistics <- ggplot(df.chem.m) + 
    geom_density(aes(x = value), na.rm = T) + 
    scale_x_continuous('Variable value') + 
    scale_y_continuous('Variable density') +
    facet_wrap(~variable, scales = 'free', ncol = 5) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black'), 
          strip.background = element_blank(), 
          strip.placement = 'outside')
  plot(p.DataStatistics)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C7S7_VariableDensities.tiff'), units = 'mm', 
         width = 210, height = 120, res = 300, pointsize = 7)
    plot(p.DataStatistics)
    dev.off()
  }
}
if(b.plot){
  df.trend$Variable <- factor(df.trend$Variable)
  df.trend$Year <- as.numeric(as.character(df.trend$Year))
  p.DataTemporal <- ggplot(df.trend, aes(x = Year, y = Mean)) + 
    geom_ribbon(aes(ymin = Mean - sd, ymax = Mean + sd), alpha = 0.2) + 
    geom_point(size = 0.2) + 
    geom_smooth(method = 'lm', linetype = 'solid', colour = 'black', size = 0.7) + 
    scale_x_continuous('Year', breaks = c(1980, 1995, 2010)) + 
    scale_y_continuous('Value') +
    facet_wrap(~Variable, scales = 'free', ncol = 5, labeller = label_wrap_gen(width = 10)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.DataTemporal)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C7S7_VariableTemporalTrend.tiff'), units = 'mm', 
         width = 160, height = 210, res = 300, pointsize = 7)
    plot(p.DataTemporal)
    dev.off()
  }
}
if(b.save){
  write.table(df.chem, paste0(s.data, 'D_SI_C7S7_VariableDensities.txt'), 
              row.names = F, sep = ',')
  write.table(df.lm, paste0(s.data, 'D_SI_C7S7_LinearModelsTemporalTrend.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(i, j, k, n.ind.mp, n.ind.var, df.chem.red, df.chem.m, lm.temp, ci.temp, 
   df.avg, df.sd, df.temp, p.DataStatistics, p.DataTemporal)

##############Part 3: Scenario definition#################################
###############Section 1: In- and output##################################
v.time <- seq(2010, 2030, 1)
df.s1 <- as.data.frame(matrix(nrow = length(v.time), ncol = (ncol(df.time) - 2)))
names(df.s1) <- c('Scenario', 'Time', names(df.time)[c(5:ncol(df.time))])
df.s1$Time <- v.time
for (i in c(3:length(names(df.s1)))){
  n.ind.var <- which(names(df.time) == names(df.s1)[i])
  df.s1[, i] <- mean(df.time[, n.ind.var], na.rm = T)
}
df.s2 <- df.s3 <- df.s4 <- df.s5 <- df.s6 <- df.s1
df.s1$Scenario <- 'AVG-BAU'
df.s2$Scenario <- 'AVG-KEY'
df.s3$Scenario <- 'EXT-BAU'
df.s4$Scenario <- 'EXT-KEY'
df.s5$Scenario <- 'NUT-BAU'
df.s6$Scenario <- 'NUT-KEY'

###############Section 2: Scenarios#######################################
################Scenario 1: Mean start + Business As Usual################
for (i in c(3:ncol(df.s1))){
  n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s1)[i], ]$Mean
  n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == names(df.s1)[i], ]$Coefficient
  v.new <- seq(n.start, n.end, length.out = nrow(df.s1))
  df.s1[, i] <- ifelse(v.new < 0.001, 0.001, v.new)
}

################Scenario 2: Mean start + Focus on key variables###########
for (i in c(3:ncol(df.s2))){
  df.s2[, i] <- df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s2)[i], ]$Mean
}

# Temperature
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Temperature', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Temperature', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s2))
df.s2$Temperature <- v.new

# Nitrate
n.start <- df.s2$Nitrate[1]
n.end <- 0.5
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s2))
df.s2$Nitrate <- exp(v.new)

# Ammonium
n.start <- df.s2$Ammonium[1]
n.end <- 0.2
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s2))
df.s2$Ammonium <- exp(v.new)

# Oxygen
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Oxygen', ]$Mean
n.end <- 5
v.new <- seq(n.start, n.end, length.out = nrow(df.s2))
df.s2$Oxygen <- v.new

# pH
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'pH', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'pH', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s2))
df.s2$pH <- v.new

################Scenario 3: Extreme start + Business As Usual#############
for (i in c(3:ncol(df.s3))){
  n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s3)[i], ]$Mean + 
    2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s1)[i], ]$sd
  n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == names(df.s3)[i], ]$Coefficient
  v.new <- seq(n.start, n.end, length.out = nrow(df.s3))
  df.s3[, i] <- v.new
}

# Temperature, starting at regular mean
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Temperature', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Temperature', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s3))
df.s3$Temperature <- v.new

# Oxygen, starting at lower value
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Oxygen', ]$Mean - 
  1 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Oxygen', ]$sd
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Oxygen', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s3))
df.s3$Oxygen <- v.new

# pH, starting at regular mean
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'pH', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'pH', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s3))
df.s3$pH <- v.new

################Scenario 4: Extreme start + Focus on key variables########
for (i in c(3:ncol(df.s4))){
  df.s4[, i] <- df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s4)[i], ]$Mean + 
    2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s4)[i], ]$sd
}

# Temperature, starting at regular mean
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Temperature', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Temperature', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s4))
df.s4$Temperature <- v.new

# Nitrate
n.start <- df.s4$Nitrate[1]
n.end <- 0.5
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s4))
df.s4$Nitrate <- exp(v.new)

# Ammonium
n.start <- df.s4$Ammonium[1]
n.end <- 0.2
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s4))
df.s4$Ammonium <- exp(v.new)

# Oxygen, starting at lowest extreme
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Oxygen', ]$Mean - 
  1 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Oxygen', ]$sd
n.end <- 5
v.new <- seq(n.start, n.end, length.out = nrow(df.s4))
df.s4$Oxygen <- v.new

# pH, starting at regular mean
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'pH', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'pH', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s4))
df.s4$pH <- v.new

################Scenario 5: Nutrient enriched + Business-as-usual####
for (i in c(3:ncol(df.s5))){
  n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s5)[i], ]$Mean
  n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == names(df.s5)[i], ]$Coefficient
  v.new <- seq(n.start, n.end, length.out = nrow(df.s5))
  df.s5[, i] <- v.new
}

# Nitrate
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Nitrate', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Nitrate', ]$sd
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Nitrate', ]$Coefficient
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s5))
df.s5$Nitrate <- exp(v.new)

# Ammonium
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Ammonium', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Ammonium', ]$sd
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Ammonium', ]$Coefficient
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s5))
df.s5$Ammonium <- exp(v.new)

# Kjeldhal-nitrogen
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Kjeldahl.Nitrogen', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Kjeldahl.Nitrogen', ]$sd
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Kjeldahl.Nitrogen', ]$Coefficient
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s5))
df.s5$Kjeldahl.Nitrogen <- exp(v.new)

# Phosphorus, starting at extreme
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Phosphorus.total', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Phosphorus.total', ]$sd
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Phosphorus.total', ]$Coefficient
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s5))
df.s5$Phosphorus.total <- exp(v.new)

################Scenario 6: Nutrient enriched + Focus on key variables####
for (i in c(3:ncol(df.s6))){
  df.s6[, i] <- df.trend[df.trend$Year == 2010 & df.trend$Variable == names(df.s6)[i], ]$Mean
}

# Temperature, starting at regular mean
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Temperature', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'Temperature', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s6))
df.s6$Temperature <- v.new

# Nitrate
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Nitrate', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Nitrate', ]$sd
n.end <- 0.5
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s6))
df.s6$Nitrate <- exp(v.new)

# Ammonium
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Ammonium', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Ammonium', ]$sd
n.end <- 0.2
v.new <- seq(log(n.start), log(n.end), length.out = nrow(df.s6))
df.s6$Ammonium <- exp(v.new)

# Oxygen
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Oxygen', ]$Mean
n.end <- 5
v.new <- seq(n.start, n.end, length.out = nrow(df.s6))
df.s6$Oxygen <- v.new

# pH, starting at regular mean
n.start <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'pH', ]$Mean
n.end <- n.start + length(v.time) * df.lm[df.lm$Variable == 'pH', ]$Coefficient
v.new <- seq(n.start, n.end, length.out = nrow(df.s6))
df.s6$pH <- v.new

# Nitrite & Phosphorus, starting at extreme
df.s6$Kjeldahl.Nitrogen <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Kjeldahl.Nitrogen', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Kjeldahl.Nitrogen', ]$sd
df.s6$Phosphorus.total <- df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Phosphorus.total', ]$Mean + 
  2 * df.trend[df.trend$Year == 2010 & df.trend$Variable == 'Phosphorus.total', ]$sd

###############Section 3: Plotting and saving#############################
df.scen <- rbind(df.s1,df.s2,df.s3,df.s4,df.s5,df.s6)
if(b.plot){
  df.scen.m <- melt(df.scen, id.vars = c('Scenario', 'Time'), 
                    measure.vars = c('Temperature', 'Nitrate', 'Oxygen', 'Ammonium', 'pH'))
  p.ScenarioVariablePatterns <- ggplot(df.scen.m, aes(x = Time, y = value)) + 
    geom_line() + 
    scale_x_continuous('Time (years)', limits = c(2009, 2032), 
                       breaks = c(2010, 2020, 2030)) + 
    scale_y_continuous('') + 
    facet_grid(variable~Scenario, scales = 'free_y', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8))
  plot(p.ScenarioVariablePatterns)
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C7S7_ScenarioVariablePatterns.tiff'), units = 'mm', 
         width = 160, height = 120, res = 300, pointsize = 7)
    plot(p.ScenarioVariablePatterns)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.s1, df.s2, df.s3, df.s4, df.s5, df.s6, v.time, v.new)

##############Part 4: Scenario analysis (2 hours)#########################
###############Section 1: In- and output##################################
df.set <- read.table(paste0(s.data, 'D_C7S3_OptimisedSettings.txt'), 
                     header = T, sep = ',')
df.scen.si <- c()

###############Section 2: Modelling#######################################
for (i in c(1:length(mf.list))){
  message(paste0('Scenario analysis for ', mf.list[i]))
  index <- which(df.set$Macrophyte == mf.list[i])
  df.train <- lst.all.train[[i]]
  df.test <- df.scen
  lst.temp <- f.CForestScenario(df.train, cols = c(5:ncol(df.train)), 
                                eval = df.scen, n.rep = 10, 
                                n.ntree = df.set$Ntree[index], 
                                n.mtry = df.set$Mtry[index], 
                                n.split = df.set$Nsplit[index], 
                                n.leaf = df.set$Nleaf[index])
  # Store results
  lst.temp[[3]]$Macrophyte <- mf.list[i]
  df.scen.si <- rbind(df.scen.si, lst.temp[[3]])
}

###############Section 3: Plotting and saving#############################
if(b.plot){
  df.scen.si.s <- df.scen.si[which(df.scen.si$Macrophyte %in% mf.sel), ]
  df.scen.si.s$Macrophyte <- factor(df.scen.si.s$Macrophyte, mf.sel)
  df.scen.si.s$Scenario <- factor(df.scen.si.s$Scenario)
  p.ScenarioAnalysis <- ggplot(df.scen.si.s, aes(x = Time, y = SI, group = Macrophyte)) + 
    geom_ribbon(aes(ymin = SI - sqrt(10) * sd, ymax = SI + sqrt(10) * sd), alpha = 0.2) + 
    geom_line(aes(linetype = Macrophyte, colour = Macrophyte)) +
    scale_x_continuous(limits = c(2009, 2031), breaks = c(2010, 2020, 2030)) + 
    scale_y_continuous('Habitat Suitability Index (-)') + 
    scale_color_manual(values = c('grey30', 'grey30', 'black', 'grey50', 'grey50'), 
                       labels = c('P. australis', 'L. minor', 'C. demersum', 
                                  'M. aquatica', 'L. minuta')) + 
    scale_linetype_manual(values = c('dashed', 'solid', 'dotted', 'dashed', 'solid'), 
                          labels = c('P. australis', 'L. minor', 'C. demersum', 
                                     'M. aquatica', 'L. minuta')) + 
    facet_grid(.~Scenario) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          strip.background = element_blank(), 
          strip.placement = 'outside', 
          strip.text = element_text(size = 8), 
          legend.background = element_blank(), 
          legend.title = element_blank(), 
          legend.position = c(0.08, 0.15), 
          legend.text = element_text(face = 'italic', size = 7))
  plot(p.ScenarioAnalysis)
  if(b.save){
    tiff(paste0(s.figs, 'F_C7S7_ScenarioAnalysis.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.ScenarioAnalysis)
    dev.off()
  }
}

if(b.save){
  write.table(df.scen.si, paste0(s.data, 'D_C7S7_ScenarioAnalysis.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm(df.set, df.scen.si, index, lst.temp, p.ScenarioAnalysis)
