library(survival)
library(survminer)
library(dplyr)
library(dynpred)
library(Hmisc)
library("psych")
library("harmonicmeanp")

best_threshold_extraction <- function(feature, time, event, no_quantiles = 100) {

  testrange<-seq(0, 0.99, len=no_quantiles)
  p <- sapply(testrange, function(q){
    dat <- data.frame(x=feature>unname(quantile(feature, q, na.rm=T)),S=Surv(time,event))
    test <- survdiff(Surv(time,event) ~ x, data=dat, rho=1)
    p.val <- 1 - pchisq(test$chisq, length(test$n) - 1)
    p.val})

  # get best threshold
  threshold <-c()
  for (i in seq(1, length(p), by=1)){
    if (p[i] <0.05){
      q <- testrange[i]
      threshold<-c(threshold,unname(quantile(feature, q, na.rm=T)))
    }
  }
  return(threshold)
}


######################################## Best Threshold ####################################################

best_threshold_method <-function(train_,test_test,methods,y,x){

  best_threshold <- mean(train_[[x]])
  p_value_test <- 1.0

  try({for (i in methods){
    print(i)
    if (i == "median"){
      threshold <- median(train_[[x]])
    }
    else if(i =="mean"){
      threshold <- mean(train_[[x]])
    }
    else if(i =="best_threshold"){
      thresholds <- best_threshold_extraction(feature = train_[, x], time = train_$time, event = train_$event)

      try({for (i in thresholds){
        threshold <- i

        train_ <- train_ %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))

        train_$score <- factor(train_$score)

        train_data <- fit_data_train(train_)

        test_test <- test_test %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))
        test_test$score <- factor(test_test$score)
        if ( nlevels(test_test$score)>1){
          test_data <- fit_data_train(test_test)


          if ((test_data[3] < p_value_test & train_data[3] < 0.05 & test_data[3]<0.05) ){

            print(paste0(i," of threshold method is taken instead of current one!"))
            list_data <- c(train_data[3],train_data[2],train_data[1],test_data[3],test_data[2],test_data[1])
            print(list_data)
            p_value_test <- test_data[3]
            best_threshold <- threshold
            print(best_threshold)


          }}
      }
      })}
    else if(i =="sd"){
      threshold <-sd(train_[[x]])

    }
    else if(i =="surv_cut"){
      res.cut <- surv_cutpoint(train_, time = "time", event = "event",
                               variables = y)
      threshold <- res.cut$cutpoint$cutpoint

    }
    else if (i =="mode"){
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      threshold <-getmode(train_[[x]])
    }

    train_ <- train_ %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))

    train_$score <- factor(train_$score)

    train_data <- fit_data_train(train_)

    test_test <- test_test %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))

    test_test$score <- factor(test_test$score)

    if ( nlevels(test_test$score)>1){
      test_data <- fit_data_train(test_test)

      if ((test_data[3] < p_value_test & train_data[3] < 0.05 & test_data[3]<0.05) ){
        print(paste0(i," is taken instead of current one!"))
        list_data <- c(train_data[3],train_data[2],train_data[1],test_data[3],test_data[2],test_data[1])
        print(list_data)
        p_value_test <- test_data[3]
        best_threshold <- threshold

      }}
  }})

  return (best_threshold)
}


################### fitting the survival model ######################################
fit_data_train <- function(train1){

  surv_object <- Surv(time = train1$time, event = train1$event)

  fit.coxph <- coxph(surv_object ~ train1$score,
                     data = train1)

  coxph.sum <- summary(fit.coxph)

  hazard.ratio <- signif(coxph.sum$conf.int[1],2)
  LogRank <- signif(coxph.sum$sctest['pvalue'],3)

  c.index <- coxph.sum$concordance[1]

  return(c(c.index,hazard.ratio,LogRank))
}


################ C-index raw data #################
calculate_c_index_raw_data <- function(train, test, feature) {
  surv_object <- Surv(time = train$time, event = train$event)
  fit.coxph <- coxph(surv_object ~ get(feature), data = train)
  coxph.sum <- summary(fit.coxph)
  c.train <- coxph.sum$concordance[1]


  surv_object <- Surv(time = test$time, event = test$event)
  fit.coxph <- coxph(surv_object ~ get(feature), data = test)
  coxph.sum <- summary(fit.coxph)
  c.test <- coxph.sum$concordance[1]

  return (c(c.train, c.test))
}

################### Survival data with discovery and validation set ###############################
calculate_survival_data_with_two_cohort<-function(train_file, test_file, x,methods,group_categories){


  train <- read.csv(file=gsub(" ", "", train_file), header=TRUE, sep=",")
  test <- read.csv(file=gsub(" ", "",test_file), header=TRUE, sep=",")

  y <- colnames(train)[x]

  print(y)

  print("The two indices with raw data!")
  print(calculate_c_index_raw_data(train, test, y))

  ########################################discovery with threshold############################

  threshold <- best_threshold_method(train,test,methods,y,x)
  print("Threshold:")
  print(threshold)

  train <- train %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
  train$score <- factor(train$score)

  train_data <- fit_data_train(train)


  ######################################## test ####################################################
  test <- test %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
  test$score <- factor(test$score)
  test_data <- fit_data_train(test)

  splots <- list()
  splots[[1]]<-ggsurvplot(survfit(Surv(time = train$time, event = train$event) ~ train$score, data = train), data = train,conf.int = FALSE, risk.table = TRUE, ,pval = FALSE,legend.title = "",
                          legend.labs = c(group_categories[2],group_categories[1]),xlab="Time (months)",
                          font.tickslab = c(14, "plain"),
                          font.x = c(14, "bold"),
                          font.y = c(14, "bold"), ylim=c(0.25, 1),xlim=c(0, 120),tables.y.text = FALSE ) #risk.table.height=.35
  splots[[1]]$plot <- splots[[1]]$plot +
    theme(legend.text = element_text(size = 14, color = "black", face = "bold"))
  splots[[1]]$table <- splots[[1]]$table + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                 axis.ticks.x=element_blank())

  splots[[2]]<-ggsurvplot(survfit(Surv(time = test$time, event = test$event) ~ test$score, data = test), data = test,conf.int = FALSE, risk.table = TRUE,pval = FALSE,
                          legend.title = "",legend.labs = c(group_categories[2],group_categories[1]),xlab="Time (months)",
                          font.tickslab = c(14, "plain"),
                          font.x = c(14, "bold"),
                          font.y = c(14, "bold"), ylim=c(0.25, 1),xlim=c(0, 120),tables.y.text = FALSE )#risk.table.height=.35
  splots[[2]]$plot <- splots[[2]]$plot + theme(legend.text = element_text(size = 14, color = "black", face = "bold"))
  splots[[2]]$table <- splots[[2]]$table + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
                                                 axis.ticks.x=element_blank())


  arrange_ggsurvplots(splots, print = TRUE,
                      ncol = 2, nrow = 1, risk.table.height = 0.4)


  list_data <- c(train_data[3],train_data[2],train_data[1],test_data[3],test_data[2],test_data[1])

  print(list_data)
  print(colnames(train)[x])


}

calculate_survival_data_train_valid_with_CV<-function(x,runs,methods,group_categories,file_init_train,file_init_valid ){

  p.values.train <-c()
  HRs.train <-c()
  c.indices.train <- c()

  p.values.test <-c()
  HRs.test <-c()
  c.indices.test <- c()

  for (n in seq(0, (runs-1), by=1)){

    train <- read.csv(file=gsub(" ", "", paste(c("/../../",file_init_train,n,".csv"),collapse = "")), header=TRUE, sep=",")
    test <- read.csv(file=gsub(" ", "",paste(c("/../../",file_init_valid,n,".csv"),collapse = "")), header=TRUE, sep=",")
    ########################################discovery with threshold############################
    y <- colnames(train)[x]
    print(n)
    print(y)

    threshold <- best_threshold_method(train,test,methods,y,x)
    print("threshold")
    print(threshold)

    train <- train %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
    train$score <- factor(train$score)

    train_data <- fit_data_train(train)


    ######################################## test ####################################################
    test <- test %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
    test$score <- factor(test$score)

    test_data <- fit_data_train(test)

    p.values.train <- c(p.values.train,train_data[3])

    HRs.train <-c(HRs.train,train_data[2])

    c.indices.train <-c(c.indices.train,train_data[1])

    p.values.test <- c(p.values.test,test_data[3])
    HRs.test <-c(HRs.test,test_data[2])

    c.indices.test <-c(c.indices.test,test_data[1])


  }

  print("Mean of C-indices:")

  print(mean(c.indices.train))
  print(mean(c.indices.test))
  print("SD of C-indices:")
  print(sd(c.indices.train))
  print(sd(c.indices.test))

  print("Train p.values:")
  print(p.values.train)
  print("p.values means:")
  print(mean(p.values.train))
  print("p.values median:")
  print(median(p.values.train))
  print("p.values 2*median:")
  print(2*median(p.values.train))
  print("p.values Harmonic Mean:")
  print(p.hmp(p.values.train))

  print("Test p.values:")
  print(p.values.test)
  print("p.values means:")
  print(mean(p.values.test))
  print("p.values median:")
  print(median(p.values.test))
  print("p.values 2*median:")
  print(2*median(p.values.test))
  print("p.values Harmonic Mean:")
  print(p.hmp(p.values.test))


  print("Mean of HRs:")
  print(paste0("train:"))
  print((HRs.train))
  print(paste0("valid:"))
  print((HRs.test))

  print(paste0("train:"))
  print((mean(HRs.train)))
  print(paste0("valid:"))
  print((mean(HRs.test)))

  print(y)

}