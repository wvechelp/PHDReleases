##############Part 0: General info########################################
###############Section 1: Location & info#################################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 03/07/2019; Last changes: 03/07/2019 
###############Section 2: Packages########################################
# install.packages('missForest') # For imputation
# install.packages('reshape2') # For melting
# install.packages('ggplot2') # For plotting
###############Section 3: Libraries#######################################
library(missForest)
library(reshape2)
library(ggplot2)
###############Section 4: Defining global variables#######################
s.data <- '../Data/'
s.figs <- '../Figures/'
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Imputation chemical data (10 mins)##################
###############Section 1: In- and output##################################
df.temp <- list()
w <- 1
df.temp[[w]] <- read.table(paste0(s.data, 'D_C6_Common_Chemical.txt'), 
                           header = T, sep = ',')
f_MD <- sum(is.na(df.temp[[w]])) / (nrow(df.temp[[w]]) * (ncol(df.temp[[w]])))
df.red.data <- as.data.frame(matrix(nrow = 1, ncol = 6))
names(df.red.data) <- c('w', 'Nvar', 'Ninst', 'Ndp', 'MD', 'f')
df.red.data[w, ] <- c(w, ncol(df.temp[[w]]), nrow(df.temp[[w]]), 
                      ncol(df.temp[[w]]) * nrow(df.temp[[w]]), 
                      sum(is.na(df.temp[[w]])), f_MD)

###############Section 2: Reduction to 50% MD#############################
f_lim <- 0.5
diff_new <- diff_old <- abs(f_lim - f_MD)
while (diff_new <= diff_old) {
  message(paste('Run ', w, sep = ''))
  ############Subsection: Removal of variable (can cause instance to be without information)
  df.var <- df.temp[[w]][, order(apply(df.temp[[w]], 2, function(x) sum(is.na(x))), decreasing = F)]
  df.var <- df.var[, -ncol(df.var)]
  empty.v <- which(apply(df.var, 1, function(x) sum(is.na(x))) == ncol(df.var[, -c(1, 2)]))
  if(length(empty.v) > 0){
    df.var <- df.var[-empty.v, ]
  }
  ############Subsection: Removal of instance (can cause variable to have no information left)
  df.inst <- df.temp[[w]][order(apply(df.temp[[w]][, -c(1, 2)], 1, function(x) sum(is.na(x))), decreasing = F), ]
  df.inst <- df.inst[-nrow(df.inst), ]
  empty.i <- which(apply(df.inst, 2, function(x) sum(is.na(x))) == nrow(df.inst))
  if(length(empty.i) > 0){
    df.inst <- df.inst[, -empty.i]
  }
  ############Subsection: selection of variable/instance to be removed####
  mis.var <- sum(is.na(df.var[, -c(1, 2)])) / (nrow(df.var) * ncol(df.var[, -c(1, 2)]))
  mis.inst <- sum(is.na(df.inst[, -c(1, 2)])) / (nrow(df.inst) * ncol(df.inst[, -c(1, 2)]))
  if (mis.var == min(mis.inst, mis.var) & ncol(df.var) > 4) { # Keeps at least 3 variables in the data, i.e. not selected if reduced data has only 2 variables
    df.temp[[w + 1]] <- df.var
    if(length(empty.v) > 0){ message(paste0('Removal of ', length(empty.v), ' instances without data'))}
  } else {
    df.temp[[w + 1]] <- df.inst
    if(length(empty.i) > 0){ message(paste0('Removal of ', length(empty.i), ' variables without data'))}
  }
  w <- w + 1
  diff_old <- diff_new
  f_MD <- sum(is.na(df.temp[[w]])) / (nrow(df.temp[[w]]) * (ncol(df.temp[[w]])))
  diff_new <- abs(f_lim - f_MD)
  df.red.data[w, ] <- c(w, ncol(df.temp[[w]]), nrow(df.temp[[w]]), 
                        ncol(df.temp[[w]]) * nrow(df.temp[[w]]), 
                        sum(is.na(df.temp[[w]])), f_MD)
}

###############Section 3: Imputation######################################
df.chem <- df.temp[[w - 1]]
df.chem.imp <- cbind(df.chem[, c(1, 2)], missForest(df.chem[, c(3:ncol(df.chem))], 
                                                    ntree = 100, maxiter = 10)$ximp)

###############Section 4: Plotting and saving#############################
if(b.plot){
  df.red.data.m <- melt(df.red.data[c(1:155), ], 
                        measure.vars = c('Nvar', 'Ninst'), id.vars = 'f')
  df.red.data.m$variable <- factor(df.red.data.m$variable, 
                                   levels = c('Nvar','Ninst'), 
                                   labels = c('Number of variables (-)', 
                                              'Number of instances (-)'))
  
  p.DataReduction <- ggplot(df.red.data.m, aes(x = 100 * f, y = value)) + 
    geom_point() + 
    scale_x_reverse('Fraction missing data (%)') + 
    scale_y_continuous('') + 
    facet_grid(variable~., scales = 'free_y', switch = 'y') + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(colour = 'black', size = 7),
          axis.title = element_text(size = 9),
          strip.background = element_blank(),
          strip.placement = 'outside',
          strip.text = element_text(size = 8))
  plot(p.DataReduction)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_SI_C6S1_DataReductionForImputation.tiff'), units = 'mm', 
         width = 160, height = 90, res = 300, pointsize = 7)
    plot(p.DataReduction)
    dev.off()
  }
}

if(b.save){
  write.table(df.red.data, paste0(s.data, 'D_C6S1_DataReductionTo50.txt'), 
              row.names = F, sep = ',')
  write.table(df.chem.imp, paste0(s.data, 'D_C6S1_Chemical_Imputed.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(f_MD, f_lim, diff_old, diff_new, w, df.var, empty.v, df.inst, empty.i, 
   mis.var, mis.inst, df.chem)

##############Part 2: Macrophyte selection (< 1 min)######################
###############Section 1: In- and output##################################
df.bio <- merge(df.chem.imp[, 1:2], read.table(paste0(s.data, 'D_C6_Common_Macrophytes.txt'), 
                                               header = T, sep = ','))
names(df.bio) <- gsub('[.]', ' ', names(df.bio))

###############Section 2: Selection based on prevalence###################
n.prev.lim <- 100
df.bio.prev <- as.data.frame(apply(df.bio[, c(3:ncol(df.bio))], 2, 
                                   function(x) sum(x == 'Present') / nrow(df.bio)))
names(df.bio.prev) <- 'Prevalence'
df.bio.prev$Macrophyte <- row.names(df.bio.prev)
row.names(df.bio.prev) <- c()
df.bio.prev <- df.bio.prev[, c('Macrophyte', 'Prevalence')]
df.bio.prev <- df.bio.prev[order(df.bio.prev$Prevalence, decreasing = T), ]
df.bio.sel <- df.bio.prev[df.bio.prev$Prevalence >= n.prev.lim / nrow(df.bio), ]

###############Section 3: Selection based on aquatic######################
df.bio.sel <- read.table(paste0(s.data, 'D_C6_MacrophyteSelection.txt'), 
                         header = T, sep = ',')
v.mf.sel <- as.character(df.bio.sel$Macrophyte[df.bio.sel$Aquatic == 'Yes'])
df.bio.new <- df.bio[, c(1, 2, which(names(df.bio) %in% v.mf.sel))]

###############Section 4: Plotting and saving#############################
if(b.save){
  write.table(df.bio.new, paste0(s.data, 'D_C6S1_Macrophytes_Selection.txt'), 
              row.names = F, sep = ',')
}

###############Section 5: Variable removal################################
rm(df.bio, n.prev.lim, df.bio.prev, df.bio.sel, v.mf.sel)

##############Part 3: Merging all data (5 secs)###########################
###############Section 1: In- and output##################################
df.bio.new <- read.table(paste0(s.data, 'D_C6S1_Macrophytes_Selection.txt'), 
                         header = T, sep = ',')
df.chem.imp <- read.table(paste0(s.data, 'D_C6S1_Chemical_Imputed.txt'), 
                          header = T, sep = ',')
df.loc <- read.table(paste0(s.data, 'D_C6_Common_LocationsInfo.txt'), 
                     header = T, sep = ',')

###############Section 2: Merging#########################################
df.all <- merge(merge(df.loc[, c(1, 2, 5, 6)], df.chem.imp), df.bio.new)

###############Section 3: Plotting and saving#############################
if(b.save){
  write.table(df.all, paste0(s.data, 'D_C6S1_AllData_ImputedAndMerged.txt'), 
              row.names = F, sep = ',')
}

###############Section 4: Variable removal################################
rm()