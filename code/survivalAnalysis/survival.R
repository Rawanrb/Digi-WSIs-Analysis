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



best_threshold_method_train_valid_test <-function(train_,valid_,test_test,methods,y,x){

  try({for (i in methods){
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

        valid_ <- valid_ %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))
        valid_$score <- factor(valid_$score)

        test_test <- test_test %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))
        test_test$score <- factor(test_test$score)

        if ( nlevels(test_test$score)>1 & nlevels(valid_$score)>1){
          valid_data <- fit_data_train(valid_)

          test_data <- fit_data_train(test_test)

          if ((test_data[3] < 0.05 & train_data[3] < 0.05 &valid_data[3] < 0.05) ){
            print(paste0(i," of threshold method is taken instead of current one!"))
            best_threshold <- threshold
            print(best_threshold)
          }
        }}
      })}
    else if(i =="sd"){
      threshold <-sd(train_[[x]])
    }
    else if(i =="surv_cut"){
      res.cut <- surv_cutpoint(train_, time = "time", event = "event", variables = y)
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

    valid_ <- valid_ %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))
    valid_$score <- factor(valid_$score)



    test_test <- test_test %>% mutate(score = ifelse(get(y) <=threshold, "low", "high"))
    test_test$score <- factor(test_test$score)


    if ( nlevels(test_test$score)>1 & nlevels(valid_$score)>1){
      valid_data <- fit_data_train(valid_)
      test_data <- fit_data_train(test_test)

      if ((test_data[3] < 0.05 & train_data[3] < 0.05 &valid_data[3] < 0.05) ){
        print(paste0(i," of threshold method is taken instead of current one!"))
        best_threshold <- threshold

      }
    }
  }})

  return (best_threshold)
}

################### fitting the survival model ######################################
fit_data_train <- function(train1){

  surv_object <- Surv(time = train1$time, event = train1$event)

  fit.coxph <- coxph(surv_object ~ train1$score,
                     data = train1)

  #print(summary(fit.coxph))
  coxph.sum <- summary(fit.coxph)

  hazard.ratio <- signif(coxph.sum$conf.int[1],2)
  LogRank <- signif(coxph.sum$sctest['pvalue'],3)

  CIlower <- signif(coxph.sum$conf.int[,"lower .95"], 2)
  CIupper <- signif(coxph.sum$conf.int[,"upper .95"], 2)

  c.index <- coxph.sum$concordance[1]
  #print(coxph.sum$concordance)

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

calculate_survival_data_train_valid_test_with_CV<-function(x,runs,methods,group_categories,file_init_train,file_init_valid,file_init_test ){

  p.values.train <-c()
  HRs.train <-c()
  c.indices.train <- c()

  p.values.valid <-c()
  HRs.valid <-c()
  c.indices.valid <- c()

  p.values.test <-c()
  HRs.test <-c()
  c.indices.test <- c()

  for (n in seq(0, (runs-1), by=1)){

    train <- read.csv(file=gsub(" ", "", paste(c("/../../",file_init_train,n,".csv"),collapse = "")), header=TRUE, sep=",")
    valid <- read.csv(file=gsub(" ", "",paste(c("/../../",file_init_valid,n,".csv"),collapse = "")), header=TRUE, sep=",")
    test <- read.csv(file=gsub(" ", "",paste(c("/../../",file_init_test,n,".csv"),collapse = "")), header=TRUE, sep=",")
    ########################################discovery with threshold############################
    y <- colnames(train)[x]
    print(n)
    print(y)

    try({

      threshold <- best_threshold_method_train_valid_test(train,valid,test,methods,y,x)


      train <- train %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
      train$score <- factor(train$score)

      train_data <- fit_data_train(train)


      ######################################## valid ####################################################
      valid <- valid %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
      valid$score <- factor(valid$score)

      valid_data <- fit_data_train(valid)

      ######################################## test ####################################################
      test <- test %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
      test$score <- factor(test$score)

      test_data <- fit_data_train(test)

      p.values.train <- c(p.values.train,train_data[3])

      HRs.train <-c(HRs.train,train_data[2])

      c.indices.train <-c(c.indices.train,train_data[1])

      ###############################

      p.values.valid <- c(p.values.valid,valid_data[3])

      HRs.valid <-c(HRs.valid,valid_data[2])

      c.indices.valid <-c(c.indices.valid,valid_data[1])
      ###############################

      p.values.test <- c(p.values.test,test_data[3])

      HRs.test <-c(HRs.test,test_data[2])

      c.indices.test <-c(c.indices.test,test_data[1])

    })
  }

  print("Mean of C-indices:")
  print(paste0("train:"))
  print(mean(c.indices.train))
  print(paste0("valid:"))
  print(mean(c.indices.valid))
  print(paste0("test:"))
  print(mean(c.indices.test))
  print("SD of C-indices:")
  print(paste0("train:"))
  print(sd(c.indices.train))
  print(paste0("valid:"))
  print(sd(c.indices.valid))
  print(paste0("test:"))
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

  print("Valid p.values:")
  print(p.values.valid)
  print("p.values means:")
  print(mean(p.values.valid))
  print("p.values median:")
  print(median(p.values.valid))
  print("p.values 2*median:")
  print(2*median(p.values.valid))
  print("p.values Harmonic Mean:")
  print(p.hmp(p.values.valid))

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
  print(mean(HRs.train))
  print(paste0("valid:"))
  print(mean(HRs.valid))
  print(paste0("test:"))
  print(mean(HRs.test))

  print(y)

}

calculate_survival_data_with_feature_combinations<-function(set, methods,group_categories,train_file,test_file,csv_file_name='',combs_number=3){

  combinations_values <- data.frame(matrix(ncol = 7, nrow = 0))
  combinations <-c()


  n <- length(set)

  if (combs_number == 2){
    sprintf("Number of combinations:%4d", n^2)
    for(i in seq_along(set)){
      for(j in seq_along(set)){
        combinations<-c(combinations,paste(set[i],set[j]))
      }
    }
  }
  #print(combinations)
  if (combs_number == 3){
    sprintf("Number of combinations:%4d", n^3)
    for(i in seq_along(set)){
      for(j in seq_along(set)){
        for(k in seq_along(set)){
          combinations<-c(combinations,paste(set[i], set[j], set[k]))

        }
      }
    }}
  if (combs_number == 4){
    sprintf("Number of combinations:%4d", n^4)
    for(i in seq_along(set)){
      for(j in seq_along(set)){
        for(k in seq_along(set)){
          for(d in seq_along(set)){
            combinations<-c(combinations,paste(set[i], set[j], set[k],set[d]))
          }
        }
      }
    }
  }

  for (n in seq(1, length(combinations), by=1)){
    train <- read.csv(file=gsub(" ", "", train_file), header=TRUE, sep=",")
    test <- read.csv(file=gsub(" ", "",test_file), header=TRUE, sep=",")

    featurescombines <- combinations[n]

    features <- as.list(strsplit(featurescombines, " ")[[1]])

    train <- train %>% mutate(combinations = 0)

    for (i in 2:length(features)) {
      train <- train %>% mutate(combinations = get('combinations')+get(toString(features[i])))

    }
    #train <- train %>% mutate(combinations = get(toString(features[1]))/get('combinations'))
    train <- train %>% mutate(combinations =  ifelse(get('combinations') >0,get(toString(features[1]))/get('combinations'),0))

    test <- test %>% mutate(combinations = 0)
    for (i in 2:length(features)) {
      test <- test %>% mutate(combinations = get('combinations')+get(toString(features[i])))
    }

    #test <- test %>% mutate(combinations = get(toString(features[1]))/get('combinations'))
    test <- test %>% mutate(combinations =  ifelse(get('combinations') >0,get(toString(features[1]))/get('combinations'),0))


    y <- 'combinations'
    x <- length(train)


    ########################################discovery with threshold############################

    try({

      threshold <- best_threshold_method(train,test,methods,y,x)


      train <- train %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
      train$score <- factor(train$score)

      train_data <- fit_data_train(train)


      ######################################## test ####################################################
      test <- test %>% mutate(score = ifelse(get(y) <=threshold, group_categories[1], group_categories[2]))
      test$score <- factor(test$score)

      test_data <- fit_data_train(test)


      combinations_values[nrow(combinations_values) + 1,] <-c(featurescombines,c(train_data[3]),c(train_data[2]),
                                                              c(train_data[1]),c(test_data[3]),c(test_data[2]),c(test_data[1]))


      colnames(combinations_values) <- c('ID','c_indices_train','hr_value_train','p_value_train','c_indices_test','hr_value_test','p_value_test')

      write.csv(combinations_values,gsub(" ", "", paste('/../../',csv_file_name,'.csv')))

    })
  }

}

calculate_survival_data_with_feature_combinations_CV<-function(set, methods,group_categories,csv_file_name,folds_threshold=10,combs_number=3,runs=10,file_init_train,file_init_valid)
{
  combinations_values <- data.frame(matrix(ncol = 13, nrow = 0))
  combinations <-c()

  n <- length(set)
  if (combs_number ==1){
    for(i in seq_along(set)){
      combinations<-c(combinations,set[i])
    }
  }

  if (combs_number == 2){
    sprintf("Number of combinations:%4d", n^2)
    for(i in seq_along(set)){
      for(j in seq_along(set)){
        combinations<-c(combinations,paste(set[i],set[j]))
      }
    }
  }
  if (combs_number == 3){
    sprintf("Number of combinations:%4d", n^3)
    for(i in seq_along(set)){
      for(j in seq_along(set)){
        for(k in seq_along(set)){
          combinations<-c(combinations,paste(set[i], set[j], set[k]))

        }
      }
    }}
  if (combs_number == 4){
    sprintf("Number of combinations:%4d", n^4)
    for(i in seq_along(set)){
      for(j in seq_along(set)){
        for(k in seq_along(set)){
          for(d in seq_along(set)){
            print(combinations)
            combinations<-c(combinations,paste(set[i], set[j], set[k],set[d]))
          }
        }
      }
    }
  }



  for (n in seq(1, length(combinations), by=1)){
    featurescombines <- combinations[n]
    print(featurescombines)

    features <- as.list(strsplit(featurescombines, " ")[[1]])
    print(features)
    p.values.train <-c()
    HRs.train <-c()
    c.indices.train <- c()

    p.values.test <-c()
    HRs.test <-c()
    c.indices.test <- c()

    try({
      for (n in seq(0, (runs-1), by=1)){


        train <- read.csv(file=gsub(" ", "", paste(c("/../../",file_init_train,n,".csv"),collapse = "")), header=TRUE, sep=",")
        test <- read.csv(file=gsub(" ", "",paste(c("/../../",file_init_valid,n,".csv"),collapse = "")), header=TRUE, sep=",")


        train <- train %>% mutate(combinations = 0)

        for (i in 2:length(features)) {
          train <- train %>% mutate(combinations = get('combinations')+get(toString(features[i])))

        }

        train <- train %>% mutate(combinations =  ifelse(get('combinations') >0,get(toString(features[1]))/get('combinations'),0))



        test <- test %>% mutate(combinations = 0)
        for (i in 2:length(features)) {
          test <- test %>% mutate(combinations = get('combinations')+get(toString(features[i])))
        }

        test <- test %>% mutate(combinations =  ifelse(get('combinations') >0,get(toString(features[1]))/get('combinations'),0))

        y <- 'combinations'
        x<-length(train)


        ########################################discovery with threshold############################



        threshold <- best_threshold_method(train,test,methods,y,x)


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


      }})

    #print(c.indices)

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
    #print(p.hmp(p.values.train))

    print("Test p.values:")
    print(p.values.test)
    print("p.values means:")
    print(mean(p.values.test))
    print("p.values median:")
    print(median(p.values.test))
    print("p.values 2*median:")
    print(2*median(p.values.test))
    print("p.values Harmonic Mean:")
    #print(p.hmp(p.values.test))


    print("Mean of HRs:")
    print(mean(HRs.train))
    print(mean(HRs.test))
    try(
      if(length(p.values.train)>=folds_threshold){



        combinations_values[nrow(combinations_values) + 1,] <-c(featurescombines,c(mean(c.indices.train)),c(mean(c.indices.test)),
                                                                c(sd(c.indices.train)),c(sd(c.indices.test)),c(mean(HRs.train)),c(mean(HRs.test)),
                                                                c(toString(p.values.train)),c(median(p.values.train)),c(2*median(p.values.train)),
                                                                c(toString(p.values.test)), c(median(p.values.test)),c(2*median(p.values.test)))
        colnames(combinations_values) <- c('ID','mean(c.indices.train)','mean(c.indices.test)',
                                           'sd(c.indices.train)','sd(c.indices.test)','mean(HRs.train)','mean(HRs.test)',
                                           'p.values.train','median(p.values.train)','2*median(p.values.train)',
                                           ' p.values.test',' median(p.values.test)','2*median(p.values.test))')

        write.csv(combinations_values,gsub(" ", "", paste('/../../',csv_file_name,'_',folds_threshold,'_',combs_number,'.csv')))

      })


  }

}

calculate_survival_data_with_two_cohort_cases_removal<-function(train_file, test_file, x,methods,group_categories,test_remove=TRUE, train_remove=FALSE){



  ########################################discovery with threshold############################
  if (train_remove){ data <- train}
  if (test_remove){data<-test}
  for (n in seq(1, nrow(data), by=1)){

    try({train <- read.csv(file=gsub(" ", "", train_file), header=TRUE, sep=",")
      y <- colnames(train)[x]

      print(y)
      test <- read.csv(file=gsub(" ", "",test_file), header=TRUE, sep=",")

      print(n)
      print("The two indices with raw data!")
      if (train_remove){ train <- train[-c(n), ]}
      if (test_remove){test <- test[-c(n), ]}

      print(calculate_c_index_raw_data(train, test, y))


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


      list_data <- c(train_data[3],train_data[2],train_data[1],test_data[3],test_data[2],test_data[1])

      print(list_data)
      print(colnames(train)[x])
    })}

}

