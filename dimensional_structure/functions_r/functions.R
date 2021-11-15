
#=================================================
# Create empty dataframe
#=================================================
create_empty_data_frame <- function(){

  all_models<-data.frame(Predictors=character(0),
                         Estimate=numeric(0),
                         `Std. Error `=integer(0),
                         df = integer(0),
                         `t value` = integer(0),
                         `Pr(>|t|)`  = integer(0),
                         model  = character(0)  )
  return(all_models)
}

#================================================
# Theme for plots
#================================================
my_theme<- theme_minimal() +
  theme(
    panel.grid.major = ggplot2::element_line(colour = "#e0e0e0", size = 0.25),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(colour = "#cccccc", size = 0.5),
    axis.line = ggplot2::element_line(size = 1, colour = "#cccccc"),
    axis.text = ggplot2::element_text(size = 8),
    axis.title = ggplot2::element_text(size = 10),
    axis.ticks.length=unit(0.2, "cm"),
    strip.text.x = element_text(size = 10),
    plot.margin = ggplot2::margin(t = 10, b = 10, l = 10, r = 15),
    plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'),
  )



#================================================
# Function to identify influential data plot
#influence() is the workhorse function of the influence.ME package.
#Based on a priorly estimated mixed effects regression model (estimated using lme4),
#the influence() function iteratively modifies the mixed effects model to neutralize
#the effect a grouped set of data has on the parameters, and which returns returns
#the fixed parameters of these iteratively modified models. These are used to compute measures of influential data.
#================================================
get_influential_data<-function(mdl){
  inf_out = influence(mdl,  "sid")
  return(inf_out)
}

#=================================================
#Function to get cook's distance
#Cook’s Distance is a measure indicating to what extent model parameters are influenced by (a set of)
#influential data on which the model is based. This function computes the Cook’s distance based on the
#information returned by the influence() function.
#=================================================
get_cooks_val<-function(inf_out){
  cd_vals = cooks.distance(inf_out)
  cd_vals <- tibble::rownames_to_column(as.data.frame(cd_vals), "sid")
  return(cd_vals)
}

#=================================================
#Function to get influential data points
#=================================================
get_influential_id<-function(cd_vals){
  influential_id <- cd_vals[cd_vals$V1> (3*mean(cd_vals$V1)),'sid']
  return(influential_id)
}

#=================================================
# Store mixed results
#=================================================
store_results_mixed_models<- function(mdl, var){
  res_table<- as.data.frame(round((summary(mdl)$coefficients),2))
  res_table<- tibble::rownames_to_column(res_table, "Predictors")
  res_table$model<-var
  return(res_table)
}

#=================================================
# Store confidence interval
#=================================================
store_conf_interval<- function(mdl, var, level){
  #Compute confidence interval
  cinf <- confint(mdl,level = level)
  #Create table
  cint_table<- as.data.frame(round(cinf,3))
  cint_table<- tibble::rownames_to_column( cint_table, "Predictors")
  cint_table$model<-var
  return(cint_table)
}

#=================================================
# Create empty dataframe
#=================================================
create_empty_data_frame <- function(){

  all_models<-data.frame(Predictors=character(0),
                         Estimate=numeric(0),
                         `Std. Error `=integer(0),
                         df = integer(0),
                         `t value` = integer(0),
                         `Pr(>|t|)`  = integer(0),
                         model  = character(0)  )
  return(all_models)
}

create_empty_data_cinf <- function(){
df<- data.frame(Predictors=character(0), `2.5 %`=numeric(0), `97.5 %`=numeric(0))
return(df)}


#=================================================
# Create empty par correlation
#=================================================
create_empty_par_cor<-function(){

  all_part_cor<-data.frame(model = character(0),
                           variable_name=character(0),
                           partial_correlation=numeric(0),
                           ci_low=numeric(0),
                           ci_up=numeric(0))
  return(all_part_cor)
}

#=================================================
# fdr_correction
#=================================================
fdr_correction<-function(summary_models){

  #add columns for fdr corrected p values
  summary_models<-summary_models %>% add_column(p.fdr = NA)

  preds= unique(summary_models$Predictors)
  for (pred in 1:length(preds)) {
    print(pred)
    p_values<-summary_models[summary_models$Predictors==preds[pred],'Pr']
    summary_models[summary_models$Predictors==preds[pred],'p.fdr']=  round(p.adjust(round(p_values,2), method = 'fdr', n = length(p_values)),2)
  }
  summary_models$New<- ifelse(summary_models$p.fdr<0.001,"***",
                              ifelse (summary_models$p.fdr<0.01,"**",
                                      ifelse(summary_models$p.fdr<0.05,"*",
                                             ifelse(summary_models$`Pr(>|t|)`> 0.05," ", ""))))

  return(summary_models)

}

#=================================================
# predict from model
#=================================================
predict_from_model<-function(df, model, DV){

  new_var <-paste(DV, '_predicted',  sep = "")
  df[,new_var]<-predict(model, newdata = df)
  return (df)
}

#=================================================
# prepost
#=================================================
make_paired_ggplot<-function(df,  DV, extended_name){

  new_var <-paste(DV, '_predicted',  sep = "")
  scaleFUN <- function(x) sprintf("%.1f", x)

  if ( DV =='pss') {
    ext = extended_name
    col = "coral"
  } else if ( DV =='lon') {
    ext = extended_name
    col = "coral2"
  } else if (DV =='soc_supp') {
    ext = extended_name
    col = "coral4"
  }

  data_pre = df %>% filter(time_num==0)
  data_post = df %>% filter(time_num==1)

  df_1x1 <- data_1x1(
    array_1 =  data_pre[[new_var]],
    array_2 =  data_post[[new_var]],
    jit_distance = .08,
    jit_seed = 321)

  plot<- raincloud_1x1_repmes(
    data = df_1x1,
    colors = (c(col, col)),
    fills =  (c(col, col)),
    line_color = col,
    line_alpha = .3,
    size = 1,
    alpha = .6,
    align_clouds = FALSE) +
    theme_bw(base_size = 20)+
    scale_x_continuous(breaks = 1:2, labels = c("Pre onset \n COVID-19", "Post onset \n COVID-19"))+
    scale_y_continuous(labels=scaleFUN, limits = c(-3,3))+
    labs( y = 'Predicted value') +
    labs( x = " ") +
    ggtitle(ext)

  return(plot)
}



make_partial_correlation_self_reg_plot <-function(all_df){

 plot<-  ggplot(all_part_cor,
         aes(x = variable_name, y = partial_correlation,
             ymin = ci_low, ymax = ci_up))+
  ylim(-1, 1)+
  # scale_x_discrete(limits=all_omega2$Predictors)+
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept =-.1,  color = "red", linetype = 'dotted')+
  geom_hline(yintercept = .1,  color = "red", linetype = 'dotted')+
  geom_hline(yintercept =-.3,  color = "red")+
  geom_hline(yintercept = .3,  color = "red")+
  coord_flip()+
  facet_grid(. ~ model)+
  theme_minimal() +
  theme(
    panel.grid.major = ggplot2::element_line(colour = "#e0e0e0", size = 0.25),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(colour = "#cccccc", size = 0.5),
    axis.line = ggplot2::element_line(size = 1, colour = "#cccccc"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    axis.ticks.length=unit(0.2, "cm"),
    strip.text.x = element_text(size = 14),
    plot.margin = ggplot2::margin(t = 10, b = 10, l = 10, r = 15),
    plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'),
  ) +
  xlab("")+
  scale_x_discrete(labels=c("AD" =expression("AD : Time" [(pre/post)]),
                            "CIT"=expression( "CIT : Time" [(pre/post)]),
                            "SW" =expression("SW : Time" [(pre/post)]),
                            "Age" =expression("Age : Time" [(pre/post)]),
                            "ravens.score.pt" =expression("IQ : Time" [(pre/post)]),
                            "Sex" =expression("Gender: Time" [(pre/post)])))+
  ylab("Partial Correlation")

return(plot)
}


#=================================================
# make plot estimates
#=================================================
make_model_estimate_plot<- function (summary_models, DV, extended_name, selection_statement){

  if ( DV =='pss') {
    ext = extended_name[1]
    col = "coral"
  } else if ( DV =='lon') {
    ext = extended_name[2]
    col = "coral2"
  } else if (DV =='soc_supp') {
    ext = extended_name[3]
    col = "coral4"
  } else {
    ext = extended_name
    col =c("darkslateblue",  "darkred",  "goldenrod3")
  }

  data_for_plot <- summary_models%>% filter(model==DV)

  plot<- ggplot(data=data_for_plot %>% filter(eval(rlang::parse_expr(selection_statement)) ),
                aes(x=Predictors, y=Estimate, fill = Predictors))+

    geom_hline(yintercept=0,size=1, colour = "#cccccc")+
    geom_bar(stat="identity", size = 0.6, color = col, position = "dodge", width=1, fill=col, alpha = .3)+
    geom_errorbar(aes(x=Predictors, ymin=Estimate-`Std. Error`, ymax=Estimate+`Std. Error`),
                  width = 0.1, size=0.6, color = col)+
    labs(title=" ", x=" ", y = "Regression coefficient") +
    ylim(c(-1,1))+
    # ggtitle(ext)+
    theme_bw(base_size = 21)+
    theme(legend.position = "none")+
    geom_text(aes(label = New, x=Predictors, y = ifelse(Estimate > 0, Estimate+`Std. Error` + 0.02, Estimate-`Std. Error` -  0.15)),
              position = position_dodge(width = 1), hjust = 0.5,
              color = 'black', size = 6)

  return(plot)
}


#=================================================
# make qqplot
#=================================================
make_qq_plot<-function(mdl, DV){
  temp <-as.data.frame(resid(mdl))
  plot <- ggplot(temp, aes(sample = resid(mdl)))+stat_qq() + stat_qq_line() + ggtitle(DV)
  return(plot)
}

#=================================================
# save tiff
#=================================================
save_tiff <- function (name_figure, width, heigth){
  tiff(file = name_figure, width = width, height =heigth, units = "in", res = 300)
}


#=================================================
# Function for estimating partial correlations and CIs from linear regression
# output.  Doesn't really need to be written to use linear regression output
# so if you'd like me to change it to just take in a matrix of data, let me know
#=================================================

est_partcor_ci = function(lm_model_output, ci_bound){
  # Estimate partial correlations and standard CI based on Fisher's
  # Z transformation.
  dat = lm_model_output$model
  df = lm_model_output$df.resid
  pcor_est = pcor(dat)$estimate[1,-c(1)]
  fzt_pcor = atanh(pcor_est)
  ub_fzt = fzt_pcor + qnorm(1 - (1-ci_bound)/2)/sqrt(df-1)
  lb_fzt = fzt_pcor - qnorm(1 - (1-ci_bound)/2)/sqrt(df-1)
  ub_pcor = (exp(2*ub_fzt) - 1)/(exp(2*ub_fzt) + 1)
  lb_pcor = (exp(2*lb_fzt) - 1)/(exp(2*lb_fzt) + 1)
  out = list()
  out$partial_correlation = pcor_est
  out$ci_low = lb_pcor
  out$ci_up = ub_pcor
  return(out)
}


element_textbox <- function(...) {
  el <- element_text(...)
  class(el) <- c("element_textbox", class(el))
  el
}

element_grob.element_textbox <- function(element, ...) {
  text_grob <- NextMethod()
  rect_grob <- element_grob(calc_element("strip.background", theme_bw()))

  ggplot2:::absoluteGrob(
    grid::gList(
      element_grob(calc_element("strip.background", theme_bw())),
      text_grob
    ),
    height = grid::grobHeight(text_grob),
    width = grid::unit(1, "npc")
  )
}

plot_effect_plot<-function( fit, pred){
  if ( pred =='AD') {
    col = "#58A1CF"
  } else if ( pred =='CIT') {
    col = "#3787C0"
  } else if (pred =='SW') {
    col = "#1B69AF"
  }
  plot<-effect_plot(fit, pred = !! pred, interval = FALSE,
                    partial.residuals = TRUE, main.title = '',
                    colors = col , point.size = 2, point.alpha = 0.4)+
    ylim(-4,3)+xlim(-3,3)+
    theme_bw(base_size = 20)
  return(plot)
}


make_partial_correlation_wellbeing_plot<-function(all_part_cor){

 plot<-  ggplot(all_part_cor,
         aes(x = variable_name, y = partial_correlation,
             ymin = ci_low, ymax = ci_up))+
  ylim(-1, 1)+
  # scale_x_discrete(limits=all_omega2$Predictors)+
  geom_errorbar(width = 0.2) +
  geom_hline(yintercept =-.1,  color = "red", linetype = 'dotted')+
  geom_hline(yintercept = .1,  color = "red", linetype = 'dotted')+
  geom_hline(yintercept =-.3,  color = "red")+
  geom_hline(yintercept = .3,  color = "red")+
  coord_flip()+
  facet_grid(. ~ model)+
  theme_minimal() +
  theme(
    panel.grid.major = ggplot2::element_line(colour = "#e0e0e0", size = 0.25),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_line(colour = "#cccccc", size = 0.5),
    axis.line = ggplot2::element_line(size = 1, colour = "#cccccc"),
    axis.text = ggplot2::element_text(size = 12),
    axis.title = ggplot2::element_text(size = 14),
    axis.ticks.length=unit(0.2, "cm"),
    strip.text.x = element_text(size = 14),
    plot.margin = ggplot2::margin(t = 10, b = 10, l = 10, r = 15),
    plot.title = element_text(hjust = 0.5, size = 10, face = 'bold'),
  ) +
  xlab("")+
  scale_x_discrete(labels=c("mindset_stress.pt" =expression( "Mindset stress : Time" [(pre/post)]),
                            "mindset_pandemic_catastrophe.pt" =expression("Mindset pandemic : Time" [(pre/post)]),
                            "AD" =expression("AD : Time" [(pre/post)]),
                            "CIT"=expression( "CIT : Time" [(pre/post)]),
                            "SW" =expression("SW : Time" [(pre/post)]),
                            "Age" =expression("Age : Time" [(pre/post)]),
                            "Sex" =expression("Gender: Time" [(pre/post)])))+
  ylab("Partial Correlation")

return(plot)
}
