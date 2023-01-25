#
setwd("..../..../....")
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

  #print(p)

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
# --------------------- Correlation (Loop over features or marker) ---------------------- #
multi_correlation<-function(save_csv=FALSE,show_plot=FALSE,file_name='',target_col=1,start_cols=1, end_cols=1){
  ############### with Bonferroni Correction ##########################
  threshold <- end_cols-start_cols
  correlation_markers <- data.frame(matrix(ncol = 3, nrow = 0))
  for (n in seq(start_cols,end_cols, by=1)){

    col_one <- dataset[[n]]

    col_two <- dataset[[target_col]]

    try({
      res <- cor.test(col_one,col_two,
                         method = "spearman",exact=FALSE)
      if (res$p.value*threshold < 0.05){
        print(n)
        print(colnames(dataset)[n])

        correlation_markers[nrow(correlation_markers) + 1,] <-c(colnames(dataset)[n],(res$p.value*threshold),res$estimate)
      }
      if (show_plot){

        feature_one <- colnames(dataset)[n]
        feature_two <- colnames(dataset)[target_col]

        ggscatter(dataset, x = feature_one, y = feature_two,
                    add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "spearman",
                   xlab = col_one, ylab = col_two)
    }
    })
  }

  print(correlation_markers)
  print(colnames(dataset)[target_col])

  if (save_csv){
    write.csv(correlation_markers,paste('../../../..', file_name, '.csv'))
  }
}
rcs_plotHR<-function(dataset,save_plot=FALSE, file_name=""){


  fit.cph <- cph(Surv(time, event)  ~ rcs(feature_name,3)+score, data=dataset)
  anova(fit.cph)

  m <- cph(Surv(time, event)   , data=dataset)#~Feature Names,

  p.val <- 1- pchisq( (fit.cph$loglik[2]- m$loglik[2]), 2 )
  print(p.val)
  p <- plotHR(m, term=1, plot.bty="l", xlim=c(0, 0.1),col.term = "#DF76EE",col.se = "#0F5129",xlab = "")
  print(p)

  if (save_plot){
    pdf(file = file_name)   # The directory you want to save the file in
    print(p,newpage = FALSE)
    dev.off()
  }
}
#--------------------- The data csv file ----------------------#
dataset <- read.csv(file=".../.../.../...", header=TRUE, sep=",",dec = "")

feature_one <- '...'
feature_two <- '...'

x_label <- "..."
y_label <- "..."

linear_regression(dataset, feature_one=feature_one, feature_two=feature_two,x_label=x_label, y_label=y_label)
correlation_method(dataset, feature_one=feature_one, feature_two=feature_two, x_label=x_label, y_label=y_label, file_name = "../../..", save_plot=TRUE)
box_plot(dataset, feature_one=feature_one, feature_two=feature_two,x_label=x_label, y_label=y_label,file_name= "../../..")
forest_plot(dataset,file_name = "/../../...")
rcs_plotHR(dataset,file_name="/../../...")
multi_correlation(save=FALSE,show_plot=TRUE,file_name='',target_col=0,start_cols=0, end_cols=0)
ICC(dataset)
