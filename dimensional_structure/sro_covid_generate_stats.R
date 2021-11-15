library(lme4)
library(lmerTest)
library(jtools)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tibble)
library(influence.ME)
library(ggpubr)
library(raincloudplots)
library(cowplot)
library(effectsize)
library(tidyverse)
library(ppcor)
library(grid)

setwd('/Users/Matilde/Documents/Experiments/Self_Regulation_Ontology-master_COVID/dimensional_structure')
source('./functions_r/functions.R')


# Load files and define list of variables   ----------------------------
rootDir<-paste (dirname(getwd()), '/Results/dimensional_structure/Covid_test', sep = "", collapse = NULL)
setwd(rootDir)
all_df<-read.csv('data_all_df_final.csv')
all_df_paired<-read.csv('data_all_paired_diff_final.csv')

tasks    <- c('Caution', 'PercResp', 'SpeededIP', 'StrategicIP') 
surveys  <- c('AGR', 'EMC', 'ERT', 'GDM', 'RS', 'RP' ,  'SS', 'SRT' )
wellbeing<- c('pss', 'lon',  'soc_supp') 
crisis   <- c('Negative.Mood',	'COVID.19.Worries',	'Stress.Life.Changes',	
              'Sleep.Time',	'Sleep.Hours',	'Physical.Exercise',	'Economic.Concern',	'Changes.Relationship',	'Media.Usage',	'General.Anxiety')



# Check test - retest reliability of factors ------------------------------
list_vars<- append(tasks,'sid')
list_vars<- append(list_vars,'time')
list_vars<- append(surveys,list_vars)

df <- all_df[,  c(list_vars)]
df_wide <-reshape(data = df, idvar = c("sid"), timevar = 'time',direction = "wide")
df_wide <- df_wide[ , !(names(df_wide ) %in% "sid")]

col_names = colnames(df_wide[,grepl(".covid", colnames(df_wide))])
col_names = gsub('.{6}$', '', col_names)

print('Pearsons correlation master vs. covid assessment for:')
for (n in 1:length(col_names)) { 
  print(col_names[n])
  x_var = paste(col_names[n],'.master', sep = "")
  y_var = paste(col_names[n],'.covid', sep = "")
  print(round(cor(df_wide[[x_var]], df_wide[[y_var]], method = c("pearson")),2))
}

# Preprocessing for data analysis -----------------------------------------
# Use Age, Gender, IQ, from master assessment even at baseline to be consistent 
# with two stage approach used for equivalence testing 

for (i in levels(all_df$sid)){
  print(i)
  temp<-all_df[all_df$sid==i,]
  all_df$master_age[all_df$sid ==i]<-temp$Age[1]
  all_df$master_ravens.score.pt[all_df$sid ==i]<-temp$ravens.score.pt[1]
  all_df$master_sex[all_df$sid ==i]<-temp$Sex[1]
}

#Scale within-subject
all_df$time_num <- ifelse(all_df$time =='master', 0, 1)
all_df$time_num.sc <- ifelse(all_df$time =='master', -.5, .5)

#Scale between subject
#Psych
all_df$AD<- scale(all_df$AD)
all_df$CIT<- scale(all_df$CIT)
all_df$SW<- scale(all_df$SW)

#Demo
all_df$Age <-scale(all_df$Age)
all_df$Sex<-scale(all_df$Sex)
all_df$ravens.score.pt<-scale(all_df$ravens.score.pt)

all_df$master_age<-scale(all_df$master_age)
all_df$master_sex<- scale(all_df$master_sex)
all_df$master_ravens.score.pt<-scale(all_df$master_ravens.score.pt)

#Mindset
all_df$mindset_stress.pt<-scale(all_df$mindset_stress.pt)
all_df$mindset_pandemic_catastrophe.pt<-scale(all_df$mindset_pandemic_catastrophe.pt)

master <- all_df[all_df$time =='master', ]
covid <- all_df[all_df$time =='covid', ]
total <- merge(master ,covid,by="sid")




# Self-Regulation TASKS ---------------------------------------------------
subDir ='tasks'
ifelse(!dir.exists(file.path(rootDir, subDir)), dir.create(file.path(rootDir,subDir)), FALSE)
vars=tasks

all_models<-create_empty_data_frame()
all_models_screened <-create_empty_data_frame()
all_models_conditioning_baseline<-create_empty_data_frame()
all_models_conditioning_baseline_screened<-create_empty_data_frame()
all_part_cor<-create_empty_par_cor()

all_models_cinf<-create_empty_data_cinf()
all_models_cinf_screened<-create_empty_data_cinf()
all_models_conditioning_baseline_cinf<-create_empty_data_cinf()
qq_plot= list(); qq_idx = 0 

mdl<- list()
fit<- list()

for (n in 1:length(vars)) {
  print(vars[n])
  #===============================================================
  # Run mixed model
  #===============================================================
  f<- paste(vars[n], "~ AD + CIT + SW + master_age + master_ravens.score.pt + master_sex + time_num.sc + AD*time_num.sc  + CIT*time_num.sc + 
          SW*time_num.sc + master_age*time_num.sc  + master_ravens.score.pt*time_num.sc + master_sex*time_num.sc   +(1|sid)")
  mdl[[n]]<-lmer(as.formula(f) , data = all_df)
  
  #================================================================
  # Store results from mixed model
  #================================================================
  res_table<- store_results_mixed_models(mdl[[n]], vars[n])
  colnames(res_table)[6] <- "Pr"
  all_models = rbind(all_models,  res_table)
  
  #================================================================
  # QQ Plot
  #================================================================
  qq_idx = qq_idx +1 
  qq_plot[[qq_idx]]<- make_qq_plot(mdl[[n]], vars[n])
  
  #================================================================
  # Fit on paired data to visualize interaction effect
  #================================================================
  f_paired <- paste(vars[n], "~ AD + CIT + SW + Age + ravens.score.pt + Sex")
  fit[[n]]<-lm(as.formula(f_paired), data =all_df_paired)
  
  #================================================================
  # Partial correlation for equivalence testing
  #================================================================
  part_cor = est_partcor_ci(fit[[n]], .9) 
  model = rep(c(vars[n]), each = length(part_cor$partial_correlation))
  partial_correlation  = c(part_cor$partial_correlation)
  variable_name = names(partial_correlation)
  variable_name = factor(variable_name, levels = names(part_cor$partial_correlation)[6:1])
  ci_low = c(part_cor$ci_low)
  ci_up = c(part_cor$ci_up)
  df_part_cor = data.frame(model, variable_name, partial_correlation, ci_low, ci_up)
  all_part_cor = rbind(all_part_cor,  df_part_cor)
  
  #================================================================
  # Conditioning on baseline
  #================================================================
  fu= paste(vars[n],'.y', sep = "")
  bs= paste(vars[n],'.x', sep = "")
  
  formula_conditioning_baseline<- paste(fu, "~", bs ,"+ Age.x + Sex.x + ravens.score.pt.x + AD.x + CIT.x + SW.x")
  m0<-lm(as.formula(formula_conditioning_baseline) , data = total)
  
  res_conditioning_baseline<- store_results_mixed_models(m0, vars[n])
  colnames(res_conditioning_baseline)[5] <- "Pr"
  all_models_conditioning_baseline = rbind(all_models_conditioning_baseline,  res_conditioning_baseline)
  
  
  #================================================================
  # Remove influential data 
  #================================================================
  inf_out<- get_influential_data(mdl[[n]])
  
  # Cook's distance and get id of influential data point
  cd_vals <- get_cooks_val(inf_out)
  influential_id <- get_influential_id(cd_vals)
  print(influential_id)
  
  #Remove influential cases
  all_df_screened<-all_df[!all_df$X %in% influential_id,]
  all_df_paired_screened <- all_df_paired[!all_df_paired$X %in% influential_id,]
  total_screened <- total[!total$X.x %in% influential_id,]
  
  #================================================================
  # Run mixed model on screened data
  #================================================================
  mdl_screened<-lmer(as.formula(f) , data = all_df_screened)
  
  #================================================================
  # Store results from mixed model on screened data
  #================================================================
  res_table_screened<- store_results_mixed_models(mdl_screened, vars[n])
  colnames(res_table_screened)[6] <- "Pr"
  all_models_screened = rbind(all_models_screened,  res_table_screened)
  
  #================================================================
  # Conditioning on baseline
  #================================================================
  m0_screened<-lm(as.formula(formula_conditioning_baseline) , data =  total_screened)
  
  res_conditioning_baseline_screened<- store_results_mixed_models(m0_screened, vars[n])
  colnames(res_conditioning_baseline_screened)[5] <- "Pr"
  all_models_conditioning_baseline_screened = rbind(all_models_conditioning_baseline_screened,  res_conditioning_baseline_screened)
  
  #================================================================
  # Get 95% CI
  #================================================================
  cinf_table <- store_conf_interval(mdl[[n]], vars[n], 0.95)
  all_models_cinf = rbind(all_models_cinf, cinf_table)
  
  cinf_table_screened <- store_conf_interval(mdl_screened, vars[n], 0.95)
  all_models_cinf_screened = rbind(all_models_cinf_screened, cinf_table_screened)
  
  cinf_table_conditioning_baseline <- store_conf_interval(m0, vars[n], 0.95)
  all_models_conditioning_baseline_cinf = rbind(all_models_conditioning_baseline_cinf, cinf_table_conditioning_baseline )
  
}

#================================================================
# Save Results 
#================================================================
filename_fit <-paste(rootDir,'/',subDir,'/', "results_mdl.rds", sep ='')
saveRDS(mdl, file = filename_fit)

filename_paired <-paste(rootDir,'/',subDir,'/', "results_fit.rds", sep ='')
saveRDS(fit, file = filename_paired)

#================================================================
# Save QQ plot
#================================================================
filename = paste(rootDir,'/',subDir,'/', "qq_plot.tiff", sep ='')
save_tiff(filename, 12, 3)
do.call(grid.arrange, c(qq_plot, ncol =4))
dev.off()

#================================================================
# Save CI 95%
#================================================================
write.csv(x=all_models_cinf, file = paste(rootDir,'/',subDir,'/', "results_lme4_cinf.csv", sep =''))
write.csv(x=all_models_cinf_screened, file = paste(rootDir,'/',subDir,'/', "results_lme4_screened_cinf.csv", sep =''))
write.csv(x=all_models_conditioning_baseline_cinf, file = paste(rootDir,'/',subDir,'/', "results_conditioning_baseline_cinf.csv", sep =''))

#================================================================
# Equivalence testing
#================================================================
partial_corr_plot<-make_partial_correlation_self_reg_plot(all_part_corr)
filename = paste(rootDir,'/',subDir,'/', "partial_correlation.tiff", sep ='')
save_tiff(filename, 12, 3)
partial_corr_plot
dev.off()

#================================================================
# FDR CORRECTION
#================================================================
all_models<-fdr_correction(all_models)
all_models_screened<-fdr_correction(all_models_screened)

all_models_conditioning_baseline<-fdr_correction(all_models_conditioning_baseline)
all_models_conditioning_baseline_screened<-fdr_correction(all_models_conditioning_baseline_screened)

write.csv(x=all_models, file = paste(rootDir,'/',subDir,'/', "results_lme4.csv", sep =''))
write.csv(x=all_models_screened, file = paste(rootDir,'/',subDir,'/', "results_lme4_screened.csv", sep =''))
write.csv(x=all_models_conditioning_baseline, file = paste(rootDir,'/',subDir,'/', "results_conditioning_baseline.csv", sep =''))
write.csv(x=all_models_conditioning_baseline_screened, file = paste(rootDir,'/',subDir,'/', "results_conditioning_baseline_screened.csv", sep =''))

#================================================================
# Figure Paper
#================================================================
id_model_caut   = match(c('Caution'),vars)
id_model_perc   = match(c('PercResp'),vars)
id_model_speed  = match(c('SpeededIP'),vars)
id_model_strat  = match(c('StrategicIP'),vars)

p1 <- plot_effect_plot(fit[[id_model_caut]], 'AD')
p2 <- plot_effect_plot(fit[[id_model_perc]], 'AD')
p3 <- plot_effect_plot(fit[[id_model_speed]], 'AD')
p4 <- plot_effect_plot(fit[[id_model_strat]], 'AD')

p5 <- plot_effect_plot(fit[[id_model_caut]], 'CIT')
p6 <- plot_effect_plot(fit[[id_model_perc]], 'CIT')
p7 <- plot_effect_plot(fit[[id_model_speed]], 'CIT')
p8 <- plot_effect_plot(fit[[id_model_strat]], 'CIT')

p9 <- plot_effect_plot(fit[[id_model_caut]],  'SW')
p10 <- plot_effect_plot(fit[[id_model_perc]], 'SW')
p11 <- plot_effect_plot(fit[[id_model_speed]], 'SW')
p12 <- plot_effect_plot(fit[[id_model_strat]], 'SW')

plot_figure<-cowplot::plot_grid(p1 + labs(title = "  Caution ")+
                                  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                        plot.title = element_textbox(hjust = 0.5, margin = margin(t = 5, b = 5)),
                                        axis.title.y = element_blank(), 
                                        axis.title.x = element_blank()),
                                p2 + labs(title = "Perc Resp")+
                                  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                        plot.title = element_textbox(hjust = 0.5, margin = margin(t = 5, b = 5)),
                                        axis.title.y = element_blank(), 
                                        axis.title.x = element_blank(), 
                                        #axis.text.x = element_blank(), 
                                        axis.text.y = element_blank()),
                                
                                p3 + labs(title = "Speeded IP")+
                                  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                        plot.title = element_textbox(hjust = 0.5, margin = margin(t = 5, b = 5)),
                                        axis.title.y = element_blank(), 
                                        axis.title.x = element_blank(), 
                                        #axis.text.x = element_blank(), 
                                        axis.text.y = element_blank()),
                                p4 + labs(title = "Strategic IP")+
                                  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                        plot.title = element_textbox(hjust = 0.5, margin = margin(t = 5, b = 5)),
                                        axis.title.y = element_blank(), 
                                        axis.title.x = element_blank(), 
                                        #axis.text.x = element_blank(), 
                                        axis.text.y = element_blank()),
                                #####
                                p5 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                           axis.title.y = element_blank(), 
                                           axis.title.x = element_blank(), 
                                ),
                                p6 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                           axis.title.y = element_blank(), 
                                           axis.title.x = element_blank(), 
                                           # axis.text.x = element_blank(), 
                                           axis.text.y = element_blank()),
                                p7 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                           axis.title.y = element_blank(), 
                                           axis.title.x = element_blank(), 
                                           #axis.text.x = element_blank(),
                                           axis.text.y = element_blank()),
                                p8 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                           axis.title.y = element_blank(), 
                                           axis.title.x = element_blank(), 
                                           #axis.text.x = element_blank(),
                                           axis.text.y = element_blank()),
                                
                                #####
                                p9 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                           axis.title.y = element_blank(), 
                                           axis.title.x = element_blank()),
                                p10 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                            axis.title.y = element_blank(), 
                                            axis.title.x = element_blank(), 
                                            axis.text.y = element_blank()),
                                p11 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                            axis.title.y = element_blank(), 
                                            axis.title.x = element_blank(), 
                                            axis.text.y = element_blank()),
                                p12 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
                                            axis.title.y = element_blank(), 
                                            axis.title.x = element_blank(), 
                                            axis.text.y = element_blank()),                     
                                ncol = 4)

y.grob <- textGrob(paste("\u0394", "cognitive factor, partial residuals",  "\n (Post \u2212 Pre onset COVID-19)"),
                   gp=gpar(fontface="bold", col="black", fontsize=20), rot=90)

x.grob <- textGrob("\n Severity of symptoms", 
                   gp=gpar(fontface="bold", col="black", fontsize=20))


filename = paste(rootDir,'/',subDir,'/', "figure_paper.tiff", sep ='')
save_tiff(filename,12, 9)
p_all<- grid.arrange(arrangeGrob(plot_figure, left = y.grob, bottom = x.grob))
dev.off()




# Self-Regulation SURVEYS ---------------------------------------------------
subDir ='surveys'
ifelse(!dir.exists(file.path(rootDir, subDir)), dir.create(file.path(rootDir,subDir)), FALSE)
vars = surveys

all_models<-create_empty_data_frame()
all_models_screened <-create_empty_data_frame()
all_models_conditioning_baseline<-create_empty_data_frame()
all_models_conditioning_baseline_screened<-create_empty_data_frame()
all_part_cor<-create_empty_par_cor()

all_models_cinf<-create_empty_data_cinf()
all_models_cinf_screened<-create_empty_data_cinf()
all_models_conditioning_baseline_cinf<-create_empty_data_cinf()
qq_plot= list(); qq_idx = 0 

mdl<- list()
fit<- list()

for (n in 1:length(vars)) {
  print(vars[n])
  #===============================================================
  # Run mixed model
  #===============================================================
  f<- paste(vars[n], "~ AD + CIT + SW + master_age + master_ravens.score.pt + master_sex + time_num.sc + AD*time_num.sc  + CIT*time_num.sc + 
          SW*time_num.sc + master_age*time_num.sc  + master_ravens.score.pt*time_num.sc + master_sex*time_num.sc   +(1|sid)")
  mdl[[n]]<-lmer(as.formula(f) , data = all_df)
  
  #================================================================
  # Store results from mixed model
  #================================================================
  res_table<- store_results_mixed_models(mdl[[n]], vars[n])
  colnames(res_table)[6] <- "Pr"
  all_models = rbind(all_models,  res_table)
  
  #================================================================
  # QQ Plot
  #================================================================
  qq_idx = qq_idx +1 
  qq_plot[[qq_idx]]<- make_qq_plot(mdl[[n]], vars[n])
  
  #================================================================
  # Fit on paired data to visualize interaction effect
  #================================================================
  f_paired <- paste(vars[n], "~ AD + CIT + SW + Age + ravens.score.pt + Sex")
  fit[[n]]<-lm(as.formula(f_paired), data =all_df_paired)
  
  #================================================================
  # Partial correlation for equivalence testing
  #================================================================
  part_cor = est_partcor_ci(fit[[n]], .9) 
  model = rep(c(vars[n]), each = length(part_cor$partial_correlation))
  partial_correlation  = c(part_cor$partial_correlation)
  variable_name = names(partial_correlation)
  variable_name = factor(variable_name, levels = names(part_cor$partial_correlation)[6:1])
  ci_low = c(part_cor$ci_low)
  ci_up = c(part_cor$ci_up)
  df_part_cor = data.frame(model, variable_name, partial_correlation, ci_low, ci_up)
  all_part_cor = rbind(all_part_cor,  df_part_cor)
  
  #================================================================
  # Conditioning on baseline
  #================================================================
  fu= paste(vars[n],'.y', sep = "")
  bs= paste(vars[n],'.x', sep = "")
  
  formula_conditioning_baseline<- paste(fu, "~", bs ,"+ Age.x + Sex.x + ravens.score.pt.x + AD.x + CIT.x + SW.x")
  m0<-lm(as.formula(formula_conditioning_baseline) , data = total)
  
  res_conditioning_baseline<- store_results_mixed_models(m0, vars[n])
  colnames(res_conditioning_baseline)[5] <- "Pr"
  all_models_conditioning_baseline = rbind(all_models_conditioning_baseline,  res_conditioning_baseline)
  
  
  #================================================================
  # Remove influential data 
  #================================================================
  inf_out<- get_influential_data(mdl[[n]])
  
  # Cook's distance and get id of influential data point
  cd_vals <- get_cooks_val(inf_out)
  influential_id <- get_influential_id(cd_vals)
  print(influential_id)
  
  #Remove influential cases
  all_df_screened<-all_df[!all_df$X %in% influential_id,]
  all_df_paired_screened <- all_df_paired[!all_df_paired$X %in% influential_id,]
  total_screened <- total[!total$X.x %in% influential_id,]
  
  #================================================================
  # Run mixed model on screened data
  #================================================================
  mdl_screened<-lmer(as.formula(f) , data = all_df_screened)
  
  #================================================================
  # Store results from mixed model on screened data
  #================================================================
  res_table_screened<- store_results_mixed_models(mdl_screened, vars[n])
  colnames(res_table_screened)[6] <- "Pr"
  all_models_screened = rbind(all_models_screened,  res_table_screened)
  
  #================================================================
  # Conditioning on baseline
  #================================================================
  m0_screened<-lm(as.formula(formula_conditioning_baseline) , data =  total_screened)
  
  res_conditioning_baseline_screened<- store_results_mixed_models(m0_screened, vars[n])
  colnames(res_conditioning_baseline_screened)[5] <- "Pr"
  all_models_conditioning_baseline_screened = rbind(all_models_conditioning_baseline_screened,  res_conditioning_baseline_screened)
  
  #================================================================
  # Get 95% CI
  #================================================================
  cinf_table <- store_conf_interval(mdl[[n]], vars[n], 0.95)
  all_models_cinf = rbind(all_models_cinf, cinf_table)
  
  cinf_table_screened <- store_conf_interval(mdl_screened, vars[n], 0.95)
  all_models_cinf_screened = rbind(all_models_cinf_screened, cinf_table_screened)
  
  cinf_table_conditioning_baseline <- store_conf_interval(m0, vars[n], 0.95)
  all_models_conditioning_baseline_cinf = rbind(all_models_conditioning_baseline_cinf, cinf_table_conditioning_baseline )
  
}

#================================================================
# Save Results 
#================================================================
filename_fit <-paste(rootDir,'/',subDir,'/', "results_mdl.rds", sep ='')
saveRDS(mdl, file = filename_fit)

filename_paired <-paste(rootDir,'/',subDir,'/', "results_fit.rds", sep ='')
saveRDS(fit, file = filename_paired)

#================================================================
# Save QQ plot
#================================================================
filename = paste(rootDir,'/',subDir,'/', "qq_plot.tiff", sep ='')
save_tiff(filename, 12, 6)
do.call(grid.arrange, c(qq_plot, ncol =4))
dev.off()

#================================================================
# Save CI 95%
#================================================================
write.csv(x=all_models_cinf, file = paste(rootDir,'/',subDir,'/', "results_lme4_cinf.csv", sep =''))
write.csv(x=all_models_cinf_screened, file = paste(rootDir,'/',subDir,'/', "results_lme4_screened_cinf.csv", sep =''))
write.csv(x=all_models_conditioning_baseline_cinf, file = paste(rootDir,'/',subDir,'/', "results_conditioning_baseline_cinf.csv", sep =''))

#================================================================
# Equivalence testing
#================================================================
partial_corr_plot<-make_partial_correlation_self_reg_plot(all_part_corr)
filename = paste(rootDir,'/',subDir,'/', "partial_correlation.tiff", sep ='')
save_tiff(filename, 12, 6)
partial_corr_plot
dev.off()

#================================================================
# FDR CORRECTION
#================================================================
all_models<-fdr_correction(all_models)
all_models_screened<-fdr_correction(all_models_screened)

all_models_conditioning_baseline<-fdr_correction(all_models_conditioning_baseline)
all_models_conditioning_baseline_screened<-fdr_correction(all_models_conditioning_baseline_screened)

write.csv(x=all_models, file = paste(rootDir,'/',subDir,'/', "results_lme4.csv", sep =''))
write.csv(x=all_models_screened, file = paste(rootDir,'/',subDir,'/', "results_lme4_screened.csv", sep =''))
write.csv(x=all_models_conditioning_baseline, file = paste(rootDir,'/',subDir,'/', "results_conditioning_baseline.csv", sep =''))
write.csv(x=all_models_conditioning_baseline_screened, file = paste(rootDir,'/',subDir,'/', "results_conditioning_baseline_screened.csv", sep =''))




# Stats model on wellbeing ------------------------------------------------
subDir ='wellbeing'
ifelse(!dir.exists(file.path(rootDir, subDir)), dir.create(file.path(rootDir,subDir)), FALSE)
vars = wellbeing

all_models<-create_empty_data_frame()
all_models_screened <-create_empty_data_frame()

all_models_cinf<-data.frame(Predictors=character(0), `2.5 %`=numeric(0), `97.5 %`=numeric(0))
all_part_cor<-create_empty_par_cor()


qq_plot= list(); qq_idx = 0 
paired_plot = list(); paired_idx = 0
est_plot=list(); est_idx = 0 

mdl<- list()
fit<- list()

for (n in 1:length(vars)) {
  print(vars[n])
  #================================================================
  # Run mixed model
  #================================================================
  f<- paste(vars[n], "~ mindset_stress.pt + mindset_pandemic_catastrophe.pt + AD + CIT + SW + master_age + master_sex + time_num.sc +
            mindset_stress.pt*time_num.sc + 
            mindset_pandemic_catastrophe.pt*time_num.sc+ 
            AD *time_num.sc+ 
            CIT*time_num.sc + 
            SW *time_num.sc+ 
            master_age *time_num.sc +
            master_sex *time_num.sc + 
            (1|sid)")
  mdl[[n]]<-lmer(as.formula(f) , data = all_df)
  
  
  #================================================================
  # Store results from mixed model
  #================================================================
  res_table<- store_results_mixed_models(mdl[[n]], vars[n])
  colnames(res_table)[6] <- "Pr"
  all_models = rbind(all_models,  res_table)
  
  #================================================================
  # Get 95% CI
  #================================================================
  cinf_table <- store_conf_interval(mdl[[n]], vars[n], 0.95)
  all_models_cinf = rbind(all_models_cinf, cinf_table)
  
  #================================================================
  # QQ Plot
  #================================================================
  qq_idx = qq_idx +1 
  qq_plot[[qq_idx]]<- make_qq_plot(mdl[[n]], vars[n])
  
  #================================================================
  # Add values predicted from model 
  #================================================================
  all_df<-predict_from_model(all_df, mdl[[n]], vars[n])
  
  #================================================================
  # Plot paired values predicted 
  #================================================================
  paired_idx = paired_idx+1
  paired_plot[[paired_idx]]<- make_paired_ggplot(all_df, vars[n],  vars[n])
  
  #================================================================
  # Fit on paired data to visualize interaction effect
  #================================================================
  f_paired <- paste(vars[n], "~ mindset_stress.pt+ mindset_pandemic_catastrophe.pt+ AD + CIT + SW + Age + Sex")
  fit[[n]]<-lm(as.formula(f_paired), data =all_df_paired)
  
  #================================================================
  # Partial correlation for equivalence testing
  #================================================================
  part_cor = est_partcor_ci(fit[[n]], .9) 
  model = rep(c(vars[n]), each = length(part_cor$partial_correlation))
  partial_correlation  = c(part_cor$partial_correlation)
  variable_name = names(partial_correlation)
  variable_name = factor(variable_name, levels = names(part_cor$partial_correlation)[7:1])
  ci_low = c(part_cor$ci_low)
  ci_up = c(part_cor$ci_up)
  df_part_cor = data.frame(model, variable_name, partial_correlation, ci_low, ci_up)
  all_part_cor = rbind(all_part_cor,  df_part_cor)
  
  #================================================================
  # Remove influential data 
  #================================================================
  inf_out<- get_influential_data(mdl[[n]])
  #plot(inf_out, which = "cook")
  
  # Cook's distance and get id of influential data point
  cd_vals <- get_cooks_val(inf_out)
  influential_id <- get_influential_id(cd_vals)
  print(influential_id)
  
  #Remove influential cases
  all_df_screened<-all_df[!all_df$X %in% influential_id,]
  all_df_paired_screened <- all_df_paired[!all_df_paired$X %in% influential_id,]
  
  #================================================================
  # Run mixed model on screened data
  #================================================================
  mdl_screened<-lmer(as.formula(f) , data = all_df_screened)
  
  #================================================================
  # Store results from mixed model on screened data
  #================================================================
  res_table_screened<- store_results_mixed_models(mdl_screened, vars[n])
  colnames(res_table_screened)[6] <- "Pr"
  all_models_screened = rbind(all_models_screened,  res_table_screened)
  
  #================================================================
  # Fit on paired data screened to visualize interaction effect
  #================================================================
  fit_screened<-lm(as.formula(f_paired), data =all_df_paired_screened )
  
}

#================================================================
# Save Results 
#================================================================
filename_fit <-paste(rootDir,'/',subDir,'/', "results_mdl.rds", sep ='')
saveRDS(mdl, file = filename_fit)

filename_paired <-paste(rootDir,'/',subDir,'/', "results_fit.rds", sep ='')
saveRDS(fit, file = filename_paired)



#================================================================
# Save QQ plot
#================================================================
filename = paste(rootDir,'/',subDir,'/', "qq_plot.tiff", sep ='')
save_tiff(filename, 9, 3)
do.call(grid.arrange, c(qq_plot, ncol =3))
dev.off()



#================================================================
# Save CI 95%
#================================================================
write.csv(x=all_models_cinf, file = paste(rootDir,'/',subDir,'/', "results_lme4_cinf.csv", sep =''))


#================================================================
# Equivalence testing
#================================================================
partial_corr_plot<-make_partial_correlation_wellbeing_plot(all_part_cor)
filename = paste(rootDir,'/',subDir,'/', "partial_correlation.tiff", sep ='')
save_tiff(filename, 9, 3)
partial_corr_plot
dev.off()

#================================================================
# FDR CORRECTION
#================================================================
all_models<-fdr_correction(all_models)
all_models_screened<-fdr_correction(all_models_screened)
write.csv(x=all_models, file = paste(rootDir,'/',subDir,'/', "results_lme4.csv", sep =''))
write.csv(x=all_models_screened, file = paste(rootDir,'/',subDir,'/', "results_lme4_screened.csv", sep =''))


for (n in 1:length(vars)) {
  print(vars[n])
  est_idx = est_idx+1
  selection_statement = "Predictors=='AD' | Predictors=='CIT' | Predictors=='SW'"
  est_plot[[est_idx ]]<- make_model_estimate_plot(all_models, vars[n],  vars[[n]], selection_statement)
}

filename = paste(rootDir,'/',subDir,'/', "figure_paper_summary_wellbeing.tiff", sep ='')
save_tiff(filename, 12, 8)
ggdraw() +
  draw_plot(do.call("grid.arrange", c(paired_plot, ncol=3)), 0, 0.5, 1, 0.5)+
  draw_plot(do.call("grid.arrange", c(est_plot, ncol=3)) ,x = 0, y = 0, width = 1, height = 0.5)+ 
  draw_plot_label(c("A", "B", "C", "D", "E", "F"), c(0 ,0, 0.35, 0.35, 0.68, 0.68), c(0.99, 0.5, 0.99,0.5,  0.99, 0.5), size = 10)
dev.off()



# Stats model on CRISIS factors -------------------------------------------
subDir ='crisis/'
ifelse(!dir.exists(file.path(rootDir, subDir)), dir.create(file.path(rootDir,subDir)), FALSE)
vars = crisis

all_models<-create_empty_data_frame()
all_models_screened <-create_empty_data_frame()
all_models_cinf<-create_empty_data_cinf()
qq_plot= list(); qq_idx = 0 

for (n in 1:length(vars)) {
  print(vars[n])
  
  #================================================================
  # Run mixed model
  #================================================================
  f<- paste(vars[n], "~  mindset_stress.pt + mindset_pandemic_catastrophe.pt+ AD + CIT + SW + Age + Sex")
  mdl[[n]]<-lm(as.formula(f) , data = all_df)
  
  #================================================================
  # Store results from mixed model
  #================================================================
  res_table<- store_results_mixed_models(mdl[[n]], vars[n])
  colnames(res_table)[5] <- "Pr"
  all_models = rbind(all_models,  res_table)
  
  #================================================================
  # Get 95% CI
  #================================================================
  cinf_table <- store_conf_interval(mdl[[n]], vars[n], 0.95)
  all_models_cinf = rbind(all_models_cinf, cinf_table)
  
  #================================================================
  # QQ Plot
  #================================================================
  qq_idx = qq_idx +1 
  qq_plot[[qq_idx]]<- make_qq_plot(mdl[[n]], vars[n])
  
}

#================================================================
# Save Results 
#================================================================
filename_fit <-paste(rootDir,'/',subDir,'/', "results_mdl.rds", sep ='')
saveRDS(mdl, file = filename_fit)


#================================================================
# Save QQ plot
#================================================================
filename = paste(rootDir,'/',subDir,'/', "qq_plot.tiff", sep ='')
save_tiff(filename, 9, 3)
do.call(grid.arrange, c(qq_plot, ncol =5))
dev.off()

#================================================================
# Save CI 95%
#================================================================
write.csv(x=all_models_cinf, file = paste(rootDir,'/',subDir,'/', "results_lm_cinf.csv", sep =''))

#================================================================
# FDR CORRECTION
#================================================================
all_models<-fdr_correction(all_models)
write.csv(x=all_models, file = paste(rootDir,'/',subDir,'/', "results_lm.csv", sep =''))

