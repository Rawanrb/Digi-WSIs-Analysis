source("/SurvivalAnalysis/survival.R")
source("/SurvivalAnalysis/plots.R")


#--------------------- The code to use the function in survival.R file ----------------------#

#the name of csv files to be read
train_file <-  paste("/results/??.csv")
test_file <- paste("/results/??.csv")

train <- read.csv(file=gsub(" ", "", train_file), header=TRUE, sep=",")
test <- read.csv(file=gsub(" ", "",test_file), header=TRUE, sep=",")

#the threshold types to be used to divide the data into high and low risk
#the same threshold will used for training and validation sets
methods <- c("median","mean","sd","surv_cut","mode","best_threshold")

#the name to be used for risk high and low values
group_categories<- c()

#the position of the feature in the csv file
x<-0
calculate_survival_data_with_two_cohort(train_file, test_file, x, methods,group_categories)
# calculate_survival_data_train_valid_test_with_CV(x=x,runs=10,methods,group_categories=group_categories,file_init_train="train_",file_init_valid="valid_",file_init_test="test_")
# calculate_survival_data_train_valid_with_CV(x=x,runs=10,methods,group_categories=group_categories,file_init_train="train_",file_init_valid="valid_")
# calculate_survival_data_with_feature_combinations_CV(set, methods,group_categories,folds_threshold=7,csv_file_name='..',combs_number=3,runs=10,file_init_train="train_",file_init_valid="valid_")
# calculate_survival_data_with_two_cohort_cases_removal(train_file, test_file, x, methods,group_categories)


#--------------------- The code to use the function in plots.R file ----------------------#
dataset <- read.csv(file="/results/??.csv", header=TRUE, sep=",",dec = "")

# the name of the used features in the csv file
feature_one <- '...'
feature_two <- '...'

# the x and y labels to be used for plots
x_label <- "..."
y_label <- "..."

linear_regression(dataset, feature_one=feature_one, feature_two=feature_two,x_label=x_label, y_label=y_label)
# correlation_method(dataset, feature_one=feature_one, feature_two=feature_two, x_label=x_label, y_label=y_label, file_name = "/results/??", save_plot=TRUE)
# box_plot(dataset, feature_one=feature_one, feature_two=feature_two,x_label=x_label, y_label=y_label,file_name= "/results/??")
# forest_plot(dataset,file_name = "/results/??")
# rcs_plotHR(dataset,file_name="/results/??")