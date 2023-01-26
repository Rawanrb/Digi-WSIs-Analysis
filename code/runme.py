import survivalAnalysis.survML as survML
from pandas import read_csv

if __name__ == '__main__':
    ########### To calculate the survival ml score ###############

    combinations_lists = [[]]

    valid_file = '...'
    train_file = '...'


    df = read_csv("/results/" + train_file + '.csv')
    df_test = read_csv("/results/" + valid_file + '.csv')

    c_indices_lists = survML.survival_ml_combinations(df, df_test, combinations_lists, type='svm', save=True)
