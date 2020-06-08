'''
Model pipeline
'''

import sys
import math
import itertools as it
import pandas as pd
import numpy as np
from sklearn import datasets, metrics, utils, base, preprocessing
from sklearn import neighbors, linear_model, tree, svm, ensemble, naive_bayes
from sklearn.model_selection import train_test_split
#from sklearn.utils.fixes import signature
#import model_specs as ms

#############################################
# LOAD, CLEAN, PRODUCE METADATA AND CHARTS
#############################################
def load_data(filename, dtypes=None):
    '''
    Load data in a CSV to a Pandas dataframe

    Inputs: filename (str), the file path to the input CSV
      dtypes (dict), dictionary of column name to input type (default None)
    Returns: pandas data frame
    '''
    df = pd.read_csv(filename, dtype=dtypes)
    return df


def generate_metadata(dataframe):
    '''
    Generate a metadata data frame with information about column types
    and null values.

    Inputs: dataframe (pd df), the data frame you want to generate
      a summary of
    Returns: pandas data frame with metadata
    '''
    cols = list(dataframe.columns)
    meta = pd.DataFrame(cols, columns=["colname"])

    # Create column that shows data type of each column
    meta.loc[:, "type"] = meta["colname"].apply(lambda x:
        type(dataframe[x].iloc[0]))

    # Create column that shows percent of null values in each column
    meta.loc[:, "pct_null"] = meta["colname"].apply(lambda x:
        dataframe[x].isna().sum() / len(dataframe))

    return meta


def generate_summary(dataframe, outliers=True):
    '''
    Return summary statistics of numeric variables

    Inputs: dataframe (pd df), the dataframe with numeric variables
        to summarize
      outliers (Bool), True if to include outliers and False if to
        exclude (based on IQR)

    Returns: pandas data frame with summary statistics
    '''
    meta = generate_metadata(dataframe)
    numeric = []
    # Make a list of numeric columns in the input dataframe
    for row in meta.iterrows():
        if np.issubdtype(row[1]["type"], np.number):
            numeric.append(row[1]["colname"])

    if not numeric:
        return None

    # Return summary of numeric variables while excluding outlier values
    if not outliers:
        cols = ['colname', 'mean', 'median', 'min', 'max', 'std_dev', 'count']
        summary = pd.DataFrame(columns=cols)
        for var in numeric:
            varsum = pd.DataFrame([var], columns=["colname"])
            outliers = identify_outliers(dataframe, var)
            df = dataframe[~outliers]
            varsum.loc[:, "mean"] = varsum["colname"].apply(lambda x:
                df[x].mean())
            varsum.loc[:, "median"] = varsum["colname"].apply(lambda x:
                df[x].median())
            varsum.loc[:, "min"] = varsum["colname"].apply(lambda x: 
                min(df[x]))
            varsum.loc[:, "max"] = varsum["colname"].apply(lambda x:
                max(df[x]))
            varsum.loc[:, "std_dev"] = varsum["colname"].apply(lambda x:
                df[x].std())
            varsum.loc[:, "count"] = varsum["colname"].apply(lambda x:
                len(df[x]))
            summary = pd.concat([summary, varsum])

    # Return summary of numeric variables while including outlier values
    else:
        summary = pd.DataFrame(numeric, columns=["colname"])
        summary.loc[:, "mean"] = summary["colname"].apply(lambda x:
            dataframe[x].mean())
        summary.loc[:, "median"] = summary["colname"].apply(lambda x:
            dataframe[x].median())
        summary.loc[:, "min"] = summary["colname"].apply(lambda x: 
            min(dataframe[x]))
        summary.loc[:, "max"] = summary["colname"].apply(lambda x:
            max(dataframe[x]))
        summary.loc[:, "std_dev"] = summary["colname"].apply(lambda x:
            dataframe[x].std())
        summary.loc[:, "count"] = summary["colname"].apply(lambda x:
            len(dataframe[x]))

    return summary


def output_numeric_vars(dataframe):
    '''
    Output only the numeric fields of input dataframe.

    Inputs: dataframe (pandas dataframe)
    Returns: dataframe containing only the numeric variables
    '''
    meta = generate_metadata(dataframe)
    numeric = []
    for row in meta.iterrows():
        if np.issubdtype(row[1]["type"], np.number):
            numeric.append(row[1]["colname"])

    return_df = dataframe[numeric]
    return return_df


def identify_outliers(dataframe, colname):
    '''
    Determine whether individual observations are outliers using IQR

    Inputs: dataframe (pandas dataframe)
      colname (str), the column for which to analyze outliers

    Returns: pandas series showing truth value (True/False) for whether
      observation at given index location in "dataframe" is an outlier 
      for variable "colname"
    '''
    q1 = dataframe[colname].quantile(0.25)
    q3 = dataframe[colname].quantile(0.75)
    iqr = q3 - q1
    outliers = (dataframe[colname] > (q3 + 1.5*iqr)) | \
        (dataframe[colname] < (q1 - 1.5*iqr))
    return outliers


def return_outliers(dataframe, colname):
    '''
    Return full data frame of outliers along a particular attribute

    Inputs: dataframe (pandas dataframe)
      colname (str), the column for which to analyze outliers

    Returns: pandas dataframe consisting of observations that have outlier
      (unusually high/low) values for the given colname
    '''
    idx = identify_outliers(dataframe, colname)
    return_df = dataframe[idx]
    return return_df



##############################################
# SPLIT, DISCRETIZE, DUMMIFY, IMPUTE, SCALE
##############################################
def create_random_splits(
    dataframe, features, target, test_size, random_state=1000):
    '''
    Create train-test split from master dataset for use in learning and testing
    ML model.

    Inputs:
      dataframe (pandas df)
      features (list of features)
      target (str, variable for which to predict outcome)

    Returns:
      Train and test df's for feature (x) and classification label (y) variables
    '''
    x = dataframe[features]
    y = dataframe[target]
    x_train, x_test, y_train, y_test = train_test_split(
        x, y, test_size=test_size, random_state=random_state)
    return x_train, x_test, y_train, y_test


def create_date_splits_manual(
    dataframe, features, target, date_col, train_dates, test_dates, convert=False):
    '''
    Create train-test split based on input dates for use in learning and testing
    ML model. 

    Inputs:
      dataframe (pandas df): dataframe of all input data
      features (list of str): column names of feature/predictor vars
      target (str): column name of target variable to label/predict
      date_col (str): name of date column in dataset to separate date sets on
      train_dates (tuple of str): takes format ('mm-dd-yyyy', 'mm-dd-yyyy') to
        delineate the training data period
      test_dates (tuple of str): takes format ('mm-dd-yyyy', 'mm-dd-yyyy') to
        delineate the testing data period
      convert (bool, default False): if True, convert date column using 
        pd.to_datetime() string method

    Returns:
      Train and test df's for feature (x) and classification label (y) variables
    '''
    # Convert string date column to datetime type
    if convert:
        dataframe['date_use'] = pd.to_datetime(dataframe[date_col])
    else:
        dataframe['date_use'] = dataframe[date_col]

    # Unpack training and testing start/end dates
    train_start, train_end = train_dates
    test_start, test_end = test_dates
    
    # Create date filter to apply for training set
    train_filter = (dataframe['date_use'] >= train_start) & (dataframe['date_use'] <= train_end)
    train_df = dataframe[train_filter]

    # Create date filter to apply for testing set
    test_filter = (dataframe['date_use'] >= test_start) & (dataframe['date_use'] <= test_end)
    test_df = dataframe[test_filter]

    # Extract train and test datasets for features and target
    x_train = train_df[features]
    y_train = train_df[target]
    x_test = test_df[features]
    y_test = test_df[target]

    return x_train, x_test, y_train, y_test


def create_date_splits_auto(
    dataframe, features, target, date_col, train_start, 
    train_length, interval, test_length, convert=False):
    '''
    Create train-test split based on input dates for use in learning and testing
    ML model. 

    Inputs:
      dataframe (pandas df): dataframe of all input data
      features (list of str): column names of feature/predictor vars
      target (str): column name of target variable to label/predict
      date_col (str): name of date column in dataset to separate date sets on
      train_start (str): start date for test data
      train_length (int): number of days for train period
      interval (int): number of days between end of train and beginning of test
      test_length (int): number of days for test period
      convert (bool, default False): if True, convert date column using 
        pd.to_datetime() string method

    Returns:
      Train and test df's for feature (x) and classification label (y) variables
    '''
    # Convert string date column to datetime type
    if convert:
        dataframe['date_use'] = pd.to_datetime(dataframe[date_col])
    else:
        dataframe['date_use'] = dataframe[date_col]

    # Automatically calculate train/test start/end dates based on parameters
    train_start = pd.to_datetime(train_start)
    train_end = train_start + pd.DateOffset(train_length)
    test_start = train_end + pd.DateOffset(interval)
    test_end = test_start + pd.DateOffset(test_length)
    
    # Create date filter to apply to training set
    train_filter = (dataframe['date_use'] >= train_start) & (dataframe['date_use'] <= train_end)
    train_df = dataframe[train_filter]
    
    # Create date filter to apply to testing set
    test_filter = (dataframe['date_use'] >= test_start) & (dataframe['date_use'] <= test_end)
    test_df = dataframe[test_filter]

    # Extract train/test splits for features and target
    x_train = train_df[features]
    y_train = train_df[target]
    x_test = test_df[features]
    y_test = test_df[target]

    return x_train, x_test, y_train, y_test


def fill_na_values(dataframe, colname, how="median"):
    '''
    Fill NaN or missing values from a numeric column with either the
    median or mean of the nonmissing values from that column.

    Inputs: dataframe (pandas df)
      colname (str): the column name for which to impute missing variables
      how (str): "median" or "mean" to tell pandas which measure of central
        tendency to use. Default is "median", if something other than
        "median" or "mean" is entered, NaN's are filled with 0's.

    Returns: updated pandas df
    '''
    if how == "median":
        fill = dataframe[colname].median(axis=0)
    elif how == "mean":
        fill = dataframe[colname].mean(axis=0)
    else:
        fill = 0
    dataframe[colname] = np.where(dataframe[colname] == np.NaN, fill, dataframe[colname])
    #dataframe[colname].fillna(value=fill, inplace=True, axis=0)
    return dataframe


def discretize_continuous(dataframe, colname, bins, labels):
    '''
    Discretize continuous variables by placing them into discrete "buckets".

    Inputs: dataframe (pandas df)
      colname (str): column to discretize
      bins (list): list of bucket boundaries
      labels (list): labels for categories

    Returns: updated pandas df with select continuous variables converted to categorical
    '''
    newcol = colname + "_cat"
    dataframe[newcol] = pd.cut(
        dataframe[colname], bins=bins, labels=labels, include_lowest=True, right=False)
    return dataframe


def dummify_categorical(dataframe, colnames):
    '''
    Convert categorical variables to dummy variables. Use this to prepare
    encoded data to be used in learning/testing ML model.

    Inputs: dataframe (pandas df)
      colnames (list): column names of categorical variables to convert
        to dummies

    Returns:
      encoded pandas df with categorical variables converted to numeric dummies
    '''
    dummies = pd.get_dummies(dataframe[colnames], drop_first=False)
    return_df = dataframe.drop(columns=colnames)
    return_df = return_df.merge(dummies, how="inner", left_index=True, right_index=True)
    return return_df


def scaler(dataframe, colname):
    '''
    Apply min-max scaling to continuous column 'colname' in dataframe 'dataframe.'

    Inputs:
      dataframe (pd df): name of dataframe containing column to scale
      colname (str): name of column to scale

    Returns:
      newcol (pd series): scaled column
    '''
    minimum = min(dataframe[colname])
    maximum = max(dataframe[colname])
    newcol = dataframe[colname].apply(lambda x: (x-minimum)/(maximum-minimum))
    return newcol


def prepare_dfs(x_df, categorical_cols, continuous_cols):
    '''
    Combine imputation, dummification, scaling of a feature dataset.

    Inputs:
      x_df (pd df): pandas dataframe of feature data
      categorical_cols (list of str): list of categorical columns in 
        the feature data
      continuous_cols (list of str): list of continuous columns in
        the feature data
      all_categorical_cols (list of str): list of all dummified
        categorical columns (all possible permutations of column names
        after conversion to binary cols)
    '''
    # Impute continuous variables, dummify categoricals
    na_cols = x_df.columns[x_df.isna().any()].tolist()
    for col in na_cols:
        if col in categorical_cols:
            x_df[col].fillna("Unknown", inplace=True, axis=0)
        elif col in continuous_cols:
            x_df[col].fillna(x_df[col].median, inplace=True, axis=0)
    return_df = dummify_categorical(x_df, categorical_cols)

    # Scale continuous columns
    for col in continuous_cols:
        return_df[col] = scaler(return_df, col)

    return return_df


def convert_to_array(xtrain_df, xtest_df):
    '''
    Convert feature training and test sets to np arrays in order to pass
    them to model fit method.

    Inputs:
      xtrain_df, xtest_df (pd dfs): pandas dataframes of feature training
        and test data, respectively

    Returns:
      train_array, test_array (arrays): arrays of train/test feature data
    '''
    # If a column does not appear in either training or testing data (i.e.
    # it's a categorical value that didn't appear in one of the dfs), create
    # a column for it and assign all values to 0
    for col in xtrain_df.columns:
        if col not in xtest_df.columns:
            xtest_df.loc[:, col] = 0
    for col in xtest_df.columns:
        if col not in xtrain_df.columns:
            xtrain_df.loc[:, col] = 0

    # Convert to array and return
    train_array = xtrain_df.values
    train_array = train_array.reshape(len(xtrain_df), len(xtrain_df.columns))
    test_array = xtest_df.values
    test_array = test_array.reshape(len(xtest_df), len(xtest_df.columns))

    return train_array, test_array




###################
# FIT AND EVALUATE
###################
def get_pred_and_actual(xtest, ytest, target, pred_scores, threshold):
    '''
    RENAME 'TARGET' IN MERGED DF TO WHATEVER THE TARGET VAR ACTUALLY IS
    Have to enter the name of the target variable that's being predicted
    as the target parameter
    '''
    # Reset index on test outcomes
    ytest_reset = ytest.reset_index()

    # Send predicted scores to a df and name column proba
    pred_scores_binary = [x[1] for x in pred_scores]
    pred_scores_frame = pd.Series(pred_scores_binary).to_frame()
    pred_scores_frame.rename(columns={0:'proba'}, inplace=True)

    # Merge actual outcomes to predicted probability,
    # sort by descending predicted proba
    merged = ytest_reset.merge(
        pred_scores_frame, how='inner', left_index=True, right_index=True)
    merged.sort_values(by='proba', ascending=False, inplace=True)

    # Assign prediction = 1 to top threshold % of values
    merged['rank'] = range(len(merged))
    merged['predicted'] = np.where(
        merged['rank']<math.floor(len(merged)*threshold), 1, 0)

    # Return lists of corresponding actuals and predictions
    return list(merged[target]), list(merged['predicted'])


def model_loop(
    X_train, y_train, X_test, y_test, train_date, test_date, target, model_dict):
    '''
    Loop through all the models listed in the input model_dict using
    all train-test splits specified in the model_specs module. Calculate
    model metrics at all specified thresholds and output a summary CSV
    including model metadata, parameters, and performance.

    Inputs:
      X_train (pd df): dataframe of feature training data
      y_train (pd df): dataframe of target training data
      X_test (pd df): dataframe of feature testing data
      y_test (pd df): dataframe of target testing data
      train_date (str): string indicating interval of training data
      test_date (str): string indicating interval of testing data
      target (str): name of target variable
      model_dict (dict): dictionary of models including parameters,
        train-test dates, and actual data

    Returns:
      Outputs summary CSV of model metadata, metrics, performance
    '''
    # Create dataframe shell that will be filled by model statistics
    summary = pd.DataFrame(columns=[
        'model','train_date','test_date','parameters',
        'threshold','baseline','accuracy','precision','recall','f1','auc'])

    # Turn model parameter dictionary into single-level dictionaries with all 
    # possible permutations of parameters
    keys = model_dict['params'].keys()
    values = (model_dict['params'][key] for key in keys)
    combinations = [dict(zip(keys, combination)) for combination in it.product(*values)]

    # Set baseline accuracy
    baseline = 1 - (len(y_test) - sum(y_test)) / len(y_test)

    # Fit, train and add model statistics to summary for all parameter combos
    i = 1
    for params in combinations:
        print("\t Fitting model ", i, " of ", len(combinations), " in this category.")
        # Create and fit model, save parameter values to be appended to
        # summary dataset
        model = base.clone(model_dict['model'])
        model.set_params(**params)
        param_print = str(params)
        model.fit(X_train, y_train)

        # Predict on test set using fitted model, calculate evaluation metrics
        # for all specified threshold levels
        pred_scores = model.predict_proba(X_test)
        for threshold in ms.thresholds:
            actual, predicted = get_pred_and_actual(
                X_test, y_test, target, pred_scores, threshold)
            accuracy = calculate_accuracy_at_threshold(actual, predicted)
            precision = calculate_precision_at_threshold(actual, predicted)
            recall = calculate_recall_at_threshold(actual, predicted)
            f1 = metrics.f1_score(actual, predicted)
            auc = metrics.roc_auc_score(actual, predicted)

            # Add line to summary dataframe
            summary.loc[len(summary)] = [model_dict['name'], 
                train_date, test_date, param_print, threshold, baseline,
                accuracy, precision, recall, f1, auc]
        i += 1
    return summary


def run_all_models(
    X_train, y_train, X_test, y_test, train_date, test_date, target, model_list, sort_column):
    '''
    Wrapper function for the model_loop() function that iterates through a list
    of models specified in the model_specs util file.

    Inputs:
      X_train (array): feature training set as a numpy array
      y_train (array or series): target training set
      X_test (array): feature testing set as a numpy array
      y_test (array or series): target testing set
      train_date, test_date (str): string representation of train/test dates
        to populate summary table
      target (str): target variable for prediction
      model_list (list): list of models to iterate through in the loop
      sort_column (str): name of metric column by which to sort (descending)
        for final output summary df

    Returns:
      summary_dfs_final (pd df): dataframe summarizing model parameters, train/test
        split, and evaluation metrics
    '''
    summary_dfs_dict = {}
    for idx, model in enumerate(model_list):
        print("Running model type ", idx+1, " of ", len(model_list), " : ", model['name'])
        # Store model information in a dict of summary info
        summary_dfs_dict[model['name']] = model_loop(
            X_train, y_train, X_test, y_test, train_date, test_date, target, model)
    # Concatenate all models with info into a final summary df
    summary_dfs_final = pd.concat(summary_dfs_dict.values())
    # Sort summary df by chosen evaluation metric descending to optimize
    summary_dfs_final.sort_values(by=sort_column, ascending=False, inplace=True)
    return summary_dfs_final



def calculate_accuracy_at_threshold(true_labels, pred_labels):
    '''
    Calculates accuracy score for given true_labels and pred_labels

    Inputs:
      true_labels (list): list of testing data for target class variable
        (actual values)
      pred_labels (list): list of predicted values from feature
        testing data

    Returns:
      (float): accuracy score
    '''
    tn, fp, fn, tp = metrics.confusion_matrix(true_labels, pred_labels).ravel()
    return 1.0 * (tp + tn) / (tn + fp + fn + tp)


def calculate_precision_at_threshold(true_labels, pred_labels):
    '''
    Calculates precision for given true_labels and pred_labels

    Inputs:
      true_labels (list): list of testing data for target class variable
        (actual values)
      pred_labels (list): list of predicted values from feature
        testing data

    Returns:
      (float): precision (proportion of identified positives that
        were in fact positives)
    '''
    _, fp, _, tp = metrics.confusion_matrix(true_labels, pred_labels).ravel()
    return 1.0 * tp / (fp + tp)


def calculate_recall_at_threshold(true_labels, pred_labels):
    '''
    Calculates recall for given true_labels and pred_labels

    Inputs:
      true_labels (list): list of testing data for target class variable
        (actual values)
      pred_labels (list): list of predicted values from feature
        testing data

    Returns:
      (float): recall (proportion of all true positives that 
        were identified as such)
    '''
    _, _, fn, tp = metrics.confusion_matrix(true_labels, pred_labels).ravel()
    return 1.0 * tp / (fn + tp)


def calculate_roc_curve(ytest, pred_labels):
    '''
    Calculate the false positive rate (fpr) and true positive rate (tpr)
    for plotting of the ROC curve, as well as the total area under
    the curve (roc_auc)

    Inputs:
      model: trained model
      xtest (df): dataframe of predictors
      ytest (df/list): list of observed test points
      threshold (float): threshold at which to cut off predicted positives

    Returns:
      fpr (array): false positives (x-points of ROC)
      tpr (array): true positives (y-points of ROC)
      roc_auc (float): area under the curve 
    '''
    fpr, tpr, _ = metrics.roc_curve(ytest, pred_labels)
    roc_auc = metrics.auc(fpr, tpr)
    return fpr, tpr, roc_auc

