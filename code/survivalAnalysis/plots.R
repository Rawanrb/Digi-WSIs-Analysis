setwd("/results/") # the folder where data available
getwd()

library('Hmisc')
library('GGally')
library("forestmodel")
library("survival")
library(plyr)
library("dplyr")
library("ggpubr")
library("lattice")
library("Formula")
library(ggplot2)
library(magrittr)
require("survival")
library("survminer")
library(rms)
library(Gmisc)
library(Greg)
library(splines)
library(smoothHR)
library(psych)


# --------------------- Random Forest ---------------------- #
random_forest <-function(){
  model <- coxph( Surv(time, event), #~Feature Names,
                  data = dataset )
  ggforest(model)
}
# --------------------- Linear Regression ---------------------- #

linear_regression <-function(dataset, feature_one_list, feature_two_list,x_label, y_label,file_name="",save_plot=FALSE){
  model <- lm(feature_one_list~feature_two_list, data=dataset)
  print(summary(model))


  #create plot to visualize fitted linear regression model
  p <- ggplot(dataset,aes(feature_one_list, feature_two_list)) + geom_point() + geom_smooth(method='lm')+labs(x = x_label, y =y_label)
  p

  if(save_plot){
      pdf(file = file_name)
      print(p,newpage = FALSE)
      dev.off()
  }

}


# --------------------- Box Plot ---------------------- #
box_plot<-function(dataset, feature_one, feature_two,x_label, y_label,file_name="",save_plot=FALSE){


  p <- boxplot(feature_one~feature_two,# here you write the feature column names in the csv file
  data=dataset,
  xlab=x_label,
  ylab=y_label,
  col="gray",
  border="black"
  )
  p

  if(save_plot){
    pdf(file = file_name)
    print(p,newpage = FALSE)
    dev.off()
  }
}
# --------------------- Correlation ---------------------- #
correlation_method <-function(dataset, feature_one, feature_two,x_label, y_label,file_name = '',save_plot=FALSE){
  p <- ggscatter(dataset, x = feature_one, y = feature_two,
               add = "reg.line", conf.int = TRUE,
               cor.coef = FALSE, cor.method = "spearman",cor.coef.size = 6, xlim=c(5, 75))
  p <- p+theme(axis.text = element_text(size = 14, color = "black", face = "bold"), axis.title.x = element_text(size = 14, color = "black", face = "bold"),legend.text = element_text(size = 14, color = "black", face = "bold"),
                                                     axis.title.y = element_text(size = 14, color = "black", face = "bold"))
  p <- p+labs(x = x_label, y =y_label)

  if(save_plot){
    pdf(file = file_name)
    print(p,newpage = FALSE)
    dev.off()
  }

}

#--------------------- Forest Plot ---------------------
forest_plot<-function(dataset,save_plot=FALSE,file_name=""){

  model <- coxph( Surv(time, event) ,#~Feature Names,
                  data = dataset )
  ggforest(model)
  p <- ggforest(model)
  print(p)
  if (save_plot){
    forest_plot(model)
    ggsave(file_name, plot=p, width=7.0, height=3.0)}

}

# --------------------- Non-linear correlation between features (Loop over features or marker) ---------------------- #
rcs_plotHR<-function(dataset,save_plot=FALSE, file_name=""){


  fit.cph <- cph(Surv(time, event)  ~ rcs(feature_name,3)+score, data=dataset)
  anova(fit.cph)

  m <- cph(Surv(time, event)   , data=dataset)#~Feature Names,

  p <- plotHR(m, term=1, plot.bty="l", xlim=c(0, 0.1),col.term = "#DF76EE",col.se = "#0F5129",xlab = "")

  if (save_plot){
    pdf(file = file_name)   # The directory you want to save the file in
    print(p,newpage = FALSE)
    dev.off()
  }
}

