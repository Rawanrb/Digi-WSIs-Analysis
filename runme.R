source("/SurvivalAnalysis/survival.R")

train_file <-  paste("/../..")
test_file <- paste("/../..")

train <- read.csv(file=gsub(" ", "", train_file), header=TRUE, sep=",")
test <- read.csv(file=gsub(" ", "",test_file), header=TRUE, sep=",")
methods <- c("median","mean","sd","surv_cut","mode","best_threshold")

group_categories<- c()
x<-0
# calculate_survival_data_with_two_cohort(train_file, test_file, x, methods,group_categories)
# calculate_survival_data_train_valid_test_with_CV(x=x,runs=10,methods,group_categories=group_categories,file_init_train="train_",file_init_valid="valid_",file_init_test="test_")
# calculate_survival_data_train_valid_with_CV(x=x,runs=10,methods,group_categories=group_categories,file_init_train="train_",file_init_valid="valid_")
# calculate_survival_data_with_feature_combinations_CV(set, methods,group_categories,folds_threshold=7,csv_file_name='..',combs_number=3,runs=10,file_init_train="train_",file_init_valid="valid_")
# calculate_survival_data_with_two_cohort_cases_removal(train_file, test_file, x, methods,group_categories)


