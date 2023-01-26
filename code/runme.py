import survivalAnalysis.survML as survML

if __name__ == '__main__':
    from sklearn.model_selection import train_test_split

    #
    # trainRSFAvergedCindex = 0
    # testRSFAvergedCindex = 0
    # trainSurvSVMAvergedCindex = 0
    # testSurvSVMAvergedCindex = 0
    # N = len(dataset)
    # for i in range(0,10):
    #     index_train, index_test = train_test_split(range(N), test_size=0.5,
    #                                                stratify=dataset['event'].values , shuffle = True)
    #     df = dataset.loc[index_train].reset_index(drop=True)
    #     df_test = dataset.loc[index_test].reset_index(drop=True)
    #
    #     #############################################  Train Data ########################################
    #     # to convert event values to true and false
    #     df['event_value'] = df['event'].apply(lambda x: True if x == 2 else False)
    #     X = df.loc[:, feature_cols]
    #
    #     status = np.array(df.loc[:, ["event_value"]])
    #     time = np.array(df.loc[:, ["time"]])
    #
    #     y = np.zeros(len(X), dtype={'names': ('Status', 'time'),
    #                                 'formats': ('bool', 'float')})
    #
    #     # to convert the array from 2D to 1D
    #     y['Status'] = status.reshape(-1)
    #     y['time'] = time.reshape(-1)
    #
    #     ###################################################### Test Data ###########################################
    #     # to convert event values to true and false
    #     df_test['event_value'] = df_test['event'].apply(lambda x: True if x == 2 else False)
    #     X_test = df_test.loc[:, feature_cols]
    #
    #     status = np.array(df_test.loc[:, ["event_value"]])
    #     time = np.array(df_test.loc[:, ["time"]])
    #
    #     y_test = np.zeros(len(X_test), dtype={'names': ('Status', 'time'),
    #                                           'formats': ('bool', 'float')})
    #
    #     # to convert the array from 2D to 1D
    #     y_test['Status'] = status.reshape(-1)
    #     y_test['time'] = time.reshape(-1)
    #
    #     c_index_train, c_index_test, df, df_test = survSVM(X, y, feature_name='svm_'+str(i), df=df, df_test=df_test, save_predictions=True)
    #
    #     print("Training SVM-Ranked C-Index:")
    #     print(c_index_train)
    #     trainSurvSVMAvergedCindex += c_index_train
    #
    #     print("Testing SVM-Ranked C-Index:")
    #     print(c_index_test)
    #     testSurvSVMAvergedCindex += c_index_test
    #     feature_col = ['ID','time','event','svm_'+str(i)]
    #     X_save = df.loc[:, feature_col]
    #     X_test_save =df_test.loc[:, feature_col]
    #     X_save.to_csv(param.param.survival_data_path + 'train_svm_'+str(i)+'.csv')
    #     X_test_save.to_csv(param.param.survival_data_path + 'valid_svm_'+str(i)+'.csv')
    #
    #     c_index_train, c_index_test, df, df_test = survRF(X, y, feature_name='rsf_'+str(i), df=df, df_test=df_test, save_predictions=True)
    #
    #     print("Training Default RSF C-Index:")
    #     print(c_index_train)
    #     trainRSFAvergedCindex += c_index_train
    #
    #     print("Testing Default RSF C-Index:")
    #     print(c_index_test)
    #     testRSFAvergedCindex += c_index_test
    #
    #     feature_col = ['ID', 'time', 'event', 'rsf_' + str(i)]
    #     X_save = df.loc[:, feature_col]
    #     X_test_save = df_test.loc[:, feature_col]
    #     X_save.to_csv(param.param.survival_data_path + 'train_rsf_' + str(i) + '.csv')
    #     X_test_save.to_csv(param.param.survival_data_path + 'valid_rsf_' + str(i) + '.csv')
    #
    # print([trainRSFAvergedCindex/10, testRSFAvergedCindex/10,trainSurvSVMAvergedCindex/10,testSurvSVMAvergedCindex/10])

    # train_file = 'centroid_features_tcga'
    # valid_file = 'centroid_features_tnbc'

    #
    # df = read_csv(param.param.survival_data_path + train_file + '.csv', error_bad_lines=False)
    # df_test = read_csv(param.param.survival_data_path + valid_file + '.csv', error_bad_lines=False)
    #
    # combinations_list = [feature_cols]#combinations_list(feature_cols)
    #
    # c_indices_lists = survival_ml_combinations(df, df_test, combinations_list)

    # import pandas as pd
    # column_names = ["feature","train_cindex_svm","test_cindex_svm"]
    # df = pd.DataFrame(c_indices_lists, columns=column_names)
    # df.to_csv(param.param.survival_data_path +'combinations_c_index_svm_tcga.csv')

    #################### Many folds ml averaged report #########################
    # import pandas as pd
    # train_file = 'uncentroid_tnbc_survival'
    # dataset = pd.read_csv(param.param.survival_data_path + train_file + '.csv', error_bad_lines=False)
    # N = len(dataset)
    # print(dataset.columns.get_indexer(feature_cols))
    # for i in range(0, 10):
    #     index_train, index_test = train_test_split(range(N), test_size=0.5,
    #                                                stratify=dataset['event'].values, shuffle=True)
    #     df = dataset.loc[index_train].reset_index(drop=True)
    #     df_test = dataset.loc[index_test].reset_index(drop=True)
    #
    #     #combinations_lists = combinations_list(feature_cols)
    #     combinations_lists = [['age', 'PHD_num', 'grade', 's_num', 'ax_num']]
    #     train_file ="train_clinical_"+str(i)
    #     valid_file = "valid_clinical_"+str(i)
    #     c_indices_lists = survival_ml_combinations(df, df_test, combinations_lists)
    #
    #
    #     if i == 0:
    #         c_indices_lists_averaged = c_indices_lists
    #     else:
    #         for item,i in zip(c_indices_lists,range(len(c_indices_lists))):
    #             for itemx,x in  zip(item,range(len(item))):
    #
    #                 c_indices_lists_averaged[i][x] +=itemx
    #
    # import statistics

    # print(statistics.stdev(c_index_test))
    #
    # c_indices_lists_averaged_final = []
    #
    # for item,x in zip(combinations_lists,range(len(combinations_lists))):
    #
    #     c_indices_lists_averaged_final.append([item,c_indices_lists_averaged[x][1]/10,c_indices_lists_averaged[x][2]/10])
    #

    # column_names = ["feature", "train_cindex_svm", "test_cindex_svm"]
    # df = pd.DataFrame(c_indices_lists_averaged_final, columns=column_names)
    # df.to_csv(param.param.survival_data_path + 'combinations_c_index_svm_just_clinical_feature.csv')

    ########### To calculate the survival ml score ###############

    # feature_cols = ['digiTAS','sTILs','axillary_nodes_flag','age','TAS_scores_max','']
    # combinations_lists = combinations_list(feature_cols)
    # combinations_lists = [['tumour_size','axillary_nodes_flag','Age']]
    combinations_lists = [['axillary_nodes', 'Digi_sTILs']]

    # train_file = 'AUBC'
    # valid_file = 'TCGA'

    #
    valid_file = 'TAS_scores_tnbc_m20_d2_o7_'
    train_file = 'TAS_scores_tcga_m20_d2_o7_'
    #
    #
    df = read_csv("/Users/rawan/Documents/TNBC_Project/tnbc/results/survival_data/" + train_file + '.csv')
    df_test = read_csv("/Users/rawan/Documents/TNBC_Project/tnbc/results/survival_data/" + valid_file + '.csv')
    #
    c_indices_lists = survival_ml_combinations(df, df_test, combinations_lists, type='svm', save=True)

    print(c_indices_lists)
    print(combinations_lists)
    # train_file = "TAS_scores_tnbc_m20_d2_o7_"
    # df = read_csv("/Users/rawan/Documents/TNBC_Project/tnbc/results/survival_data/" + train_file + '.csv')
    # for i in range(0, 10):
    #     index_train, index_test = train_test_split(range(429), test_size=0.5,
    #                                                stratify=df['event'].values, shuffle=True)
    #     df_ = df.loc[index_train].reset_index(drop=True)
    #     df_test_ = df.loc[index_test].reset_index(drop=True)
    #
    #     train_file = 'train_'+str(i)
    #     valid_file = 'valid_'+str(i)
    #
    #     c_indices_lists = survival_ml_combinations(df_, df_test_, combinations_lists,type='svm',save_folds = True, i =i)
    #
    #