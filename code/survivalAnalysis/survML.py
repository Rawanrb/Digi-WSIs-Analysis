from sksurv.ensemble import RandomSurvivalForest
from sklearn.model_selection import ShuffleSplit, GridSearchCV

from sksurv.metrics import concordance_index_censored
from sksurv.svm import FastSurvivalSVM
import numpy as np


###################################### Survival Random Forest#######################################
def survRF(X, y,X_test, y_test,feature_name = '',df=None,df_test=None,save_predictions=False):

    estimator_RSF = RandomSurvivalForest().fit(X, y)

    ###################################### Predicting Random Survival Forest#######################################


    c_index_train = estimator_RSF.score(X, y)
    c_index_test = estimator_RSF.score(X_test, y_test)

    ####### with different hyperparameter ########

    if save_predictions:
        df[feature_name] = estimator_RSF.predict(X)
        df_test[feature_name] = estimator_RSF.predict(X_test)
        return c_index_train, c_index_test, df, df_test


    return c_index_train,c_index_test

######################################  survival SVM #######################################
def survSVM(X, y,X_test, y_test,feature_name = '',df=None,df_test=None,save_predictions=False):
    estimator = FastSurvivalSVM()
    def score_survival_model(model, X, y):
        prediction = model.predict(X)
        result = concordance_index_censored(y['Status'], y['time'], prediction)
        return result[0]

    param_grid = {'alpha': 2. ** np.arange(-12, 13, 2)}
    cv = ShuffleSplit(n_splits=10, test_size=0.5, random_state=0)
    gcv = GridSearchCV(estimator, param_grid, scoring=score_survival_model,
                 refit=False,
                   cv=cv)
    gcv = gcv.fit(X, y)

    estimator.set_params(**gcv.best_params_)
    estimator.fit(X, y)


    ###################################### Predicting Survival SVM #######################################

    c_index_train = estimator.score(X, y)
    c_index_test = estimator.score(X_test, y_test)

    if save_predictions:
        df[feature_name] = estimator.predict(X)

        df_test[feature_name] = estimator.predict(X_test)
        return c_index_train, c_index_test, df, df_test


    return c_index_train, c_index_test

def combinations_list(features):
    from itertools import combinations
    output = sum([list(map(list, combinations(features, i))) for i in range(len(features) + 1)], [])
    final_list = [item for item in output if len(item)>1]
    return final_list

def survival_ml_combinations(df,df_test,train_file,valid_file, combinations_list,type,save=False,save_folds = False, i =0):
    # #############################################  Train Data ########################################
    # # to convert event values to true and false
    c_indices_lists = []

    try:
        for feature_cols in combinations_list:
            try:
                print(feature_cols)

                # to convert event values to true and false
                df['event_value'] = df['event'].apply(lambda x: True if x == 1 else False)
                X = df.loc[:, feature_cols]


                status = np.array(df.loc[:, ["event_value"]])
                time = np.array(df.loc[:, ["time"]])


                y = np.zeros(len(X), dtype={'names': ('Status', 'time'),
                                            'formats': ('bool', 'float')})

                # to convert the array from 2D to 1D
                y['Status'] = status.reshape(-1)
                y['time'] = time.reshape(-1)

                # ###################################################### Test Data ###########################################
                # # to convert event values to true and false
                df_test['event_value'] = df_test['event'].apply(lambda x: True if x == 1 else False)
                X_test = df_test.loc[:, feature_cols]


                status = np.array(df_test.loc[:, ["event_value"]])

                time = np.array(df_test.loc[:, ["time"]])



                y_test = np.zeros(len(X_test), dtype={'names': ('Status', 'time'),
                                                      'formats': ('bool', 'float')})

                # to convert the array from 2D to 1D
                y_test['Status'] = status.reshape(-1)
                y_test['time'] = time.reshape(-1)

                if type == 'svm':
                    c_index_train, c_index_test,df ,df_test = survSVM(X, y, X_test, y_test, feature_name=str(feature_cols)+'_svm', df=df, df_test=df_test, save_predictions=True)
                    if save:
                        df.to_csv("/../" + train_file + '_svm_'+train_file+'_train.csv')
                        df_test.to_csv(
                            "/../" + valid_file + '_svm_'+train_file+'_train.csv')
                    if save_folds:
                        df.to_csv(
                            "/../" + "train_"+str(i)+'.csv')
                        df_test.to_csv(
                            "/../" + "valid_"+str(i)+'.csv')
                else:
                    c_index_train, c_index_test,df, df_test = survRF(X, y,X_test, y_test, feature_name=str(feature_cols) + '_rsf', df=df, df_test=df_test, save_predictions=True)
                    if save:
                        df.to_csv(
                            "/../" + train_file + '_rsf_' + train_file + '_train.csv')
                        df_test.to_csv(
                            "/../" + valid_file + '_rsf_' + train_file + '_train.csv')
                    if save_folds:
                        df.to_csv(
                            "/../" + "train_"+str(i)+'.csv')
                        df_test.to_csv(
                            "/../" + "valid_"+str(i)+'.csv')


                print([feature_cols,c_index_train, c_index_test])
                c_indices_lists.append([c_index_train, c_index_test])
            except:
                pass
    except:
        pass
    return c_indices_lists #sorted(c_indices_lists, key = lambda x: x[2])

