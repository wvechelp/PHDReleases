##############Part 0: General info########################################
###############Section 1: Location and info###############################
setwd(choose.dir()) # Might not work (bug), use CTRL + SHIFT + H
# Remarks: Last successful run: 01/02/2020; Last changes: 01/02/2020
###############Section 2: Packages########################################
# install.packages('ggplot2') # For plotting
# install.packages('ggpubr') # For merging plots
###############Section 3: Libraries#######################################
library(ggplot2)
library(ggpubr)
###############Section 4: Global variables################################
s.data <- '../Data/'
s.figs <- '../Figures/'
n.Vol <- 0.25 # Volume of test medium
dir.create('Temp')
###############Section 5: Plot and save###################################
b.plot <- T; b.save <- T

##############Part 1: Nutrients###########################################
###############Section 1: In- and output##################################
df.nut <- read.csv(paste0(s.data, 'D_C8_Graphs_Nutrients.csv'), 
                   header = T, sep = ',')

###############Section 2: Preparation#####################################
df.dat.p <- df.nut
names(df.dat.p)[1] <- 'Group'

df.dat.p$Group <- as.character(df.dat.p$Group)
df.dat.p$Group[df.dat.p$Group == 'Control'] <- c('Initial')
df.dat.p$Group <- factor(df.dat.p$Group, c('Initial', 'L. minor', 'L. minuta'))

df.dat.p$N4[df.dat.p$Group == 'Initial'] <- df.dat.p$N0[df.dat.p$Group == 'Initial']
df.dat.p$se_N4[df.dat.p$Group == 'Initial'] <- df.dat.p$se_N0[df.dat.p$Group == 'Initial']

df.dat.p$N4m[df.dat.p$Group == 'Initial'] <- df.dat.p$N0m[df.dat.p$Group == 'Initial']
df.dat.p$se_N4m[df.dat.p$Group == 'Initial'] <- df.dat.p$se_N0m[df.dat.p$Group == 'Initial']

df.dat.p$P4[df.dat.p$Group == 'Initial'] <- df.dat.p$P0[df.dat.p$Group == 'Initial']
df.dat.p$se_P4[df.dat.p$Group == 'Initial'] <- df.dat.p$se_P0[df.dat.p$Group == 'Initial']

df.dat.p$P4m[df.dat.p$Group == 'Initial'] <- df.dat.p$P0m[df.dat.p$Group == 'Initial']
df.dat.p$se_P4m[df.dat.p$Group == 'Initial']<-df.dat.p$se_P0m[df.dat.p$Group == 'Initial']

###############Section 3: Plotting and saving#############################
if(b.plot){
  # Plotting Nitrogen
  p.NitroBar <- ggplot(df.dat.p, aes(x = Concentration, group = Group)) + 
    geom_col(aes(y = N4m, fill = Group), colour = 'black', position = 'dodge') + 
    geom_errorbar(aes(ymin = N4m - sd_N4m, ymax = N4m + sd_N4m), 
                  width = 0.4, position = position_dodge(width = 0.9)) + 
    coord_cartesian(ylim = c(0, 20)) + 
    scale_x_discrete('') + 
    scale_y_continuous('Nitrogen mass (mg)') + 
    scale_fill_manual(values = c('black', 'grey30', 'grey60'), 
                      labels = c('Initial', expression(italic('L. minor')), 
                                 expression(italic('L. minuta')))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.title = element_blank(),
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.85), 
          legend.text = element_text(size = 8), 
          legend.key.size = unit(3, 'mm'), 
          legend.text.align = 0)
  plot(p.NitroBar)
  
  p.NitroDot <- ggplot(df.dat.p[df.dat.p$Group != 'Initial', ], 
                       aes(x = N0, y = DiffN, group = Group)) + 
    geom_errorbar(aes(ymin = DiffN - sd_DiffN, ymax = DiffN + sd_DiffN, 
                      colour = Group), width = 0.02 * 75) + 
    geom_errorbarh(aes(xmin = N0 - sd_N0, xmax = N0 + sd_N0, 
                       colour = Group), height = 0.02 * 4) + 
    geom_point(aes(colour = Group)) + 
    scale_colour_manual(values = c('grey30', 'grey60')) + 
    coord_cartesian(xlim = c(0, 75), ylim = c(0, 4)) + 
    scale_x_continuous(expression(Initial~concentration~(mg%.%L^{'-1'}))) + 
    scale_y_continuous('Nitrogen removed (mg)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.875), 
          legend.text = element_text(face = 'italic'), 
          legend.key.size = unit(3, 'mm'))
  plot(p.NitroDot)
  
  p.Nitrogen <- ggarrange(p.NitroBar, p.NitroDot, ncol = 2, labels = 'AUTO', 
                          label.x = 0.125, label.y = 0.98, 
                          font.label = list(size = 9), align = 'hv')
  plot(p.Nitrogen)
  
  # Plotting Phosphorus
  p.PhosphoBar <- ggplot(df.dat.p, aes(x = Concentration, group = Group)) + 
    geom_col(aes(y = P4m, fill = Group), colour = 'black', position = 'dodge') + 
    geom_errorbar(aes(ymin = P4m - sd_P4m, ymax = P4m + sd_P4m), 
                  width = 0.4, position = position_dodge(width = 0.9)) + 
    coord_cartesian(ylim = c(0, 6.5)) + 
    scale_x_discrete('') + 
    scale_y_continuous('Phosphorus mass (mg)') + 
    scale_fill_manual(values = c('black', 'grey30', 'grey60'), 
                      labels = c('Initial', expression(italic('L. minor')), 
                                 expression(italic('L. minuta')))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.85), 
          legend.text = element_text(size = 8), 
          legend.key.size = unit(3, 'mm'), 
          legend.text.align = 0)
  plot(p.PhosphoBar)
  
  p.PhosphoDot <- ggplot(df.dat.p[df.dat.p$Group != 'Initial', ], 
                         aes(x = P0, y = DiffP, group = Group)) + 
    geom_errorbar(aes(ymin = DiffP - sd_DiffP, ymax = DiffP + sd_DiffP, 
                      colour = Group), width = 0.02 * 25) + 
    geom_errorbarh(aes(xmin = P0 - sd_P0, xmax = P0 + sd_P0, 
                       colour = Group), height = 0.02 * 1.7) + 
    geom_point(aes(colour = Group)) + 
    scale_colour_manual(values = c('grey30', 'grey60')) + 
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 1.7)) + 
    scale_x_continuous(expression(Initial~concentration~(mg%.%L^{-1}))) + 
    scale_y_continuous('Phosphorus removed (mg)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.875), 
          legend.text = element_text(face = 'italic'), 
          legend.key.size = unit(3, 'mm'))
  plot(p.PhosphoDot)
  
  p.Phosphorus <- ggarrange(p.PhosphoBar, p.PhosphoDot, ncol = 2, 
                            labels = 'AUTO', label.x = 0.15, label.y = 0.98, 
                            font.label = list(size = 9), align = 'hv')
  plot(p.Phosphorus)
  
  # Plotting relative removal
  p.NitroRel <- ggplot(df.dat.p[df.dat.p$Group != 'Initial', ], 
                       aes(x = N0, y = RemN, group = Group)) + 
    coord_cartesian(xlim = c(0, 75), ylim = c(0, 110)) + 
    geom_errorbar(aes(ymin = RemN - sd_RemN, ymax = RemN + sd_RemN, 
                      colour = Group), width = 0.02 * 75) + 
    geom_errorbarh(aes(xmin = N0 - sd_N0, xmax = N0 + sd_N0, 
                       colour = Group), height = 0.02 * 110) + 
    geom_point(aes(colour = Group)) + 
    scale_colour_manual(values = c('grey30', 'grey60')) + 
    scale_x_continuous(expression(Initial~concentration~(mg%.%L^{-1}))) + 
    scale_y_continuous('Relative nitrogen removal (%)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.position = 'none')
  plot(p.NitroRel)
  
  p.PhosphoRel <- ggplot(df.dat.p[df.dat.p$Group != 'Initial', ], 
                         aes(x = P0, y = RemP, group = Group)) + 
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 110)) +
    geom_errorbar(aes(ymin = RemP - sd_RemP, ymax = RemP + sd_RemP, 
                      colour = Group), width = 0.02 * 25) + 
    geom_errorbarh(aes(xmin = P0 - sd_P0, xmax = P0 + sd_P0, 
                       colour = Group), height = 0.02 * 110) + 
    geom_point(aes(colour = Group)) + 
    scale_colour_manual(values = c('grey30', 'grey60')) + 
    scale_x_continuous(expression(Initial~concentration~(mg%.%L^{-1}))) + 
    scale_y_continuous('Relative phosphorus removal (%)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.9), 
          legend.text = element_text(face = 'italic'), 
          legend.key.size = unit(5, 'mm'))
  plot(p.PhosphoRel)
  
  p.Relative <- ggarrange(p.NitroRel, p.PhosphoRel, ncol = 2, labels = 'AUTO', 
                          label.x = 0.13, label.y = 0.98, 
                          font.label = list(size = 9), align = 'hv')
  plot(p.Relative)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C8S1_Article_NitrogenRemoval.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Nitrogen)
    dev.off()
    
    tiff(paste0(s.figs, 'F_C8S1_Article_PhosphorusRemoval.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Phosphorus)
    dev.off()
    
    tiff(paste0(s.figs, 'F_C8S1_Article_RelativeRemoval.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Relative)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.nut, df.dat.p, p.NitroBar, p.NitroDot, p.Nitrogen, p.PhosphoBar, 
   p.PhosphoDot, p.Phosphorus, p.NitroRel, p.PhosphoRel, p.Relative)

##############Part 2: Biomass#############################################
###############Section 1: In- and output##################################
df.bio <- read.csv(paste0(s.data, 'D_C8_Graphs_RGR.csv'), header = T, sep = ',')

###############Section 2: Preparation#####################################
df.dat.rep <- df.bio

###############Section 3: Plotting and saving#############################
if(b.plot){
  p.BioBar <- ggplot(df.dat.rep, aes(x = Concentration, y = Diff2, group = Series)) + 
    geom_col(aes(fill = Series), position = 'dodge') + 
    geom_errorbar(aes(ymin = Diff2 - sd_Diff2, ymax = Diff2 + sd_Diff2), 
                  position = position_dodge(width = 0.9), width = 0.4) + 
    scale_fill_manual(values = c('grey30', 'grey60')) + 
    scale_x_discrete('') + 
    scale_y_continuous('Increase in dry biomass (mg)') + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.position = 'none')
  plot(p.BioBar)
  
  p.RGRBar <- ggplot(df.dat.rep, aes(x = Concentration, y = RGR1, group = Series)) + 
    geom_col(aes(fill = Series), position = 'dodge') + 
    geom_errorbar(aes(ymin = RGR1 - sd_RGR1, ymax = RGR1 + sd_RGR1), 
                  position = position_dodge(width = 0.9), width = 0.4) + 
    scale_fill_manual(values = c('grey30', 'grey60')) + 
    scale_x_discrete('') + 
    scale_y_continuous(expression(Relative~Growth~Rate~(d^{-1})), limits = c(0, 1)) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 9), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.9), 
          legend.text = element_text(face = 'italic', size = 8), 
          legend.key.size = unit(3, 'mm'))
  plot(p.RGRBar)
  
  p.Biomass <- ggarrange(p.BioBar, p.RGRBar, ncol = 2, labels = 'AUTO', 
                         label.x = 0.18, label.y = 0.98, 
                         font.label = list(size = 9), align = 'hv')
  plot(p.Biomass)
  
  if(b.save){
    tiff(paste0(s.figs, 'F_C8S1_Article_BiomassIncrease.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.Biomass)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.bio, df.dat.rep, p.BioBar, p.RGRBar, p.Biomass)

##############Part 3: BBNR################################################
###############Section 1: In- and output##################################
df.bbnr <- read.csv(paste0(s.data, 'D_C8_Graphs_BBNR.csv'), header = T, sep = ',')

###############Section 2: Preparation#####################################
df.dat.rep <- df.bbnr

###############Section 3: Plotting and saving#############################
if(b.plot){
  p.BBNRNitrogen <- ggplot(df.dat.rep, aes(x = N0, y = BioN, group = Series)) + 
    coord_cartesian(xlim = c(0, 75), ylim = c(0, 70)) + 
    geom_errorbar(aes(ymin = BioN - sd_BioN, ymax = BioN + sd_BioN, 
                      colour = Series), width = 0.02 * 75) + 
    geom_errorbarh(aes(xmin = N0 - sd_N0, xmax = N0 + sd_N0, 
                       colour = Series), height = 0.02 * 70) + 
    geom_point(aes(colour = Series)) + 
    scale_colour_manual(values = c('grey30', 'grey60')) + 
    scale_x_continuous(expression(Initial~concentration~(mg%.%L^{-1}))) + 
    scale_y_continuous(expression(Biomass-based~N~removal~(mg%.%g^{-1}))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 8), 
          legend.position = 'none')
  plot(p.BBNRNitrogen)
  
  p.BBNRPhosphorus <- ggplot(df.dat.rep, aes(x = P0, y = BioP, group = Series)) + 
    coord_cartesian(xlim = c(0, 25), ylim = c(0, 40)) + 
    geom_errorbar(aes(ymin = BioP - sd_BioP, ymax = BioP + sd_BioP, 
                      colour = Series), width = 0.02 * 25) + 
    geom_errorbarh(aes(xmin = P0 - sd_P0, xmax = P0 + sd_P0, 
                       colour = Series), height = 0.02 * 35) + 
    geom_point(aes(colour = Series)) + 
    scale_colour_manual(values = c('grey30', 'grey60')) + 
    scale_x_continuous(expression(Initial~concentration~(mg%.%L^{-1}))) + 
    scale_y_continuous(expression(Biomass-based~P~removal~(mg%.%g^{-1}))) + 
    theme_bw() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(colour = 'black', size = 7), 
          axis.title = element_text(size = 8), 
          legend.title = element_blank(), 
          legend.background = element_blank(), 
          legend.position = c(0.8, 0.17), 
          legend.text = element_text(face = 'italic', size = 8))
  plot(p.BBNRPhosphorus)
  
  p.BBNR <- ggarrange(p.BBNRNitrogen, p.BBNRPhosphorus, ncol = 2, 
                      labels = 'AUTO', label.x = 0.15, label.y = 0.98, 
                      font.label = list(size = 9), align = 'hv')
  plot(p.BBNR)
  if(b.save){
    tiff(paste0(s.figs, 'F_C8S1_Article_BiomassNutrient.tiff'), units = 'mm', 
         width = 160, height = 60, res = 300, pointsize = 7)
    plot(p.BBNR)
    dev.off()
  }
}

###############Section 4: Variable removal################################
rm(df.bbnr, df.dat.rep, p.BBNRNitrogen, p.BBNRPhosphorus, p.BBNR)
