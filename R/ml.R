#' Reads and prepares a dataset
#'
#' Reads a dataset from a CSV-file and prepares it for machine learning operations
#'
#' @param csvFilename name of the CSV-File
#' @param depColumn the column name that contains the dependent variable
#' @param missingDataFun function to replace NA entries or NULL to skip NA correction
#' @param transformFactors set to true to convert character columns into factors
#' @param splitRatio the amount of data that should go into the training set (80\% = 0.8)
#' @param seed if results should be done consistently random, enter a seed number here, otherwise NULL
#' @param featureScaling set to true to automatically apply feature scaling to every numeric column
#' @return list with 2 datasets: training_set and test_set
#' @examples
#' ml.prepare(ml.testset())
#' @import utils
#' @export
ml.read <- function(csvFilename, depColumn, missingDataFun = mean, transformFactors = TRUE, splitRatio = 0.8, seed=NULL, featureScaling = FALSE)
{
  dataset <- utils::read.csv(csvFilename)
  resultset <- ml.prepare(dataset, depColumn, missingDataFun, transformFactors, splitRatio, seed, featureScaling)
  return(resultset)
}

#' Prepares a dataset
#'
#' Prepares a dataset for machine learning operations
#'
#' @param dataset the dataset to prepare
#' @param depColumn the column name that contains the dependent variable
#' @param missingDataFun function to replace NA entries or NULL to skip NA correction
#' @param transformFactors set to true to convert character columns into factors
#' @param splitRatio the amount of data that should go into the training set (80\% = 0.8)
#' @param seed if results should be done consistently random, enter a seed number here, otherwise NULL
#' @param featureScaling set to true to automatically apply feature scaling to every numeric column
#' @return list with 2 datasets: training_set and test_set
#' @examples
#' ml.prepare(ml.testset())
#' @export
ml.prepare <- function(dataset, depColumn, missingDataFun = mean, transformFactors = TRUE, splitRatio = 0.8, seed=NULL, featureScaling = FALSE)
{
  # check and correct columns for missing data
  if (!is.null(missingDataFun))
  {
    dataset <- ml.pre.na.correct(dataset,missingDataFun)
  }

  # convert categorical data
  if (transformFactors)
  {
    dataset <- ml.pre.factors.correct.data.frame(dataset)
  }

  # split the set
  resultset <- ml.pre.split(dataset, depColumn, splitRatio, seed)

  # apply feature scaling
  if (featureScaling)
  {
    resultset <- ml.pre.scaling(resultset)
  }

  return(resultset)
}

#' #########################################################
#' ############# PRE PROCESSING 1 - NA VALUES ##############
#' #########################################################

#' Identifies NA columns
#'
#' Retrieves columns of a dataset that contains NA entries.
#'
#' @param dataset dataset to investigate
#' @return vector of column names that contain NA entries
#' @examples
#' ml.pre.na.columns(ml.testset())
#' @export
ml.pre.na.columns <- function(dataset)
{
  dataset.missing <- c()
  for (i in names(dataset))
  {
    if (any(is.na(dataset[,i])))
    {
      dataset.missing <- c(dataset.missing,i)
    }
  }
  return(dataset.missing)
}

#' Replaces NA values
#'
#' Replaces NA values of a column with the value of a predefined method, such as mean.
#'
#' @param vec column for NA replacement (or data.frame)
#' @param fun function to determine the replacement value. Is applied only on entries != NA.
#' @return same column vector but with replaced NA values
#' @examples
#' ml.pre.na.correct(ml.testset()$disp,mean)
#' @export
ml.pre.na.correct <- function(vec, fun=mean)
{
  if (class(vec) == "data.frame")
  {
    return(ml.pre.na.correct.data.frame(vec,fun))
  }
  x.without.na <- na.omit(vec)
  replace = ave(na.omit(vec), FUN = fun)
  return(ifelse(is.na(vec), replace, vec))
}

#' Replaces NA values(data.frame)
#'
#' Replaces all NA values in a data.frame with the value of a predefined method, such as the mean.
#'
#' @param dataset dataset to apply the replacement on
#' @param fun function to determine the replacement value. Is applied only on entries != NA.
#' @return dataset with corrected NA values
#' @examples
#' ml.pre.na.correct.data.frame(ml.testset(),mean)
#' @export
ml.pre.na.correct.data.frame <- function(dataset, fun)
{
  for (i in ml.pre.na.columns(dataset))
  {
    dataset[,i] <- ml.pre.na.correct(dataset[,i],fun)
  }
  return(dataset)
}


#' #########################################################
#' ############## PRE PROCESSING 2 - FACTORS ###############
#' #########################################################

#' Identifies Factor columns
#'
#' Retrieves columns of a dataset that are strings and should be converted to factors.
#'
#' @param dataset dataset to investigate
#' @return vector of column names that contain strings
#' @examples
#' ml.pre.factors.columns(ml.testset())
#' @export
ml.pre.factors.columns <- function(dataset)
{
  dataset.character <- c()
  for (i in names(dataset))
  {
    if (any(is.character(dataset[,i])))
    {
      dataset.character <- c(dataset.character,i)
    }
  }
  return(dataset.character)
}

#' Converts strings to factors
#'
#' Converts all string values of a column to factor values.
#'
#' @param vec column for string conversion (or data.frame)
#' @return same column vector but with converted string values
#' @examples
#' ml.pre.factors.correct(ml.testset()$dep)
#' @export
ml.pre.factors.correct <- function(vec)
{
  if (class(vec) == "data.frame")
  {
    return(ml.pre.factors.correct.data.frame(vec))
  }
  uni <- unique(vec)
  return(factor(vec, levels = uni, labels = seq(1,length(uni))))
}

#' Converts strings to factors(data.frame)
#'
#' Converts all string values in a data.frame to factor values.
#'
#' @param dataset dataset to apply the conversion on
#' @return dataset with corrected factor values
#' @examples
#' ml.pre.factors.correct.data.frame(ml.testset())
#' @export
ml.pre.factors.correct.data.frame <- function(dataset)
{
  for (i in ml.pre.factors.columns(dataset))
  {
    dataset[,i] <- ml.pre.factors.correct(dataset[,i])
  }
  return(dataset)
}


#' #########################################################
#' ########### PRE PROCESSING 3 - SPLIT DATASET ############
#' #########################################################

#' Splits a dataset
#'
#' Splits a dataset into training_set and test_set.
#'
#' @param dataset dataset to split
#' @param depVariable the dependent variable of the dataset (on which basis the split is done)
#' @param splitRatio ratio of the training_set size ]0,1]
#' @param seed the seed for the random number generator (or NULL for no change of the seed)
#' @return list with 2 datasets: training_set and test_set
#' @examples
#' ml.pre.split(ml.testset(),"dep")
#' @import caTools
#' @export
ml.pre.split <- function(dataset, depVariable, splitRatio=0.8, seed=123)
{
  if (!is.null(seed))
  {
    set.seed(seed)
  }
  split = caTools::sample.split(dataset[,depVariable], SplitRatio = splitRatio)
  return(list(
    training_set=subset(dataset, split == TRUE),
    test_set=subset(dataset, split == FALSE)
  ))
}

#' #########################################################
#' ########## PRE PROCESSING 4 - FEATURE SCALING ###########
#' #########################################################

#' Scales columns' values
#'
#' Applies Feature Scaling to columns
#'
#' @param training_set the training set or list(training_set=...,test_set=...)
#' @param test_set the test set or NULL
#' @param columns the columns that should be scaled as vector or NULL to scale all numeric columns
#' @return list with 2 datasets: training_set and test_set
#' @examples
#' ml.pre.scaling(ml.pre.split(ml.testset(),"dep"))
#' @export
ml.pre.scaling <- function(training_set, test_set=NULL, columns=NULL)
{
  if (class(training_set)=="list")
  {
    test_set = training_set[["test_set"]]
    training_set = training_set[["training_set"]]
  }
  if (is.null(columns))
  {
    columns <- c();
    for (i in names(training_set))
    {
      if (any(is.numeric(training_set[,i])))
      {
        columns <- c(columns,i)
      }
    }
  }

  train_center <-colMeans(training_set[,columns],  na.rm=T)
  train_scale <- sapply(training_set[,columns], FUN=function(col) sd(col, na.rm=T))
  training_set[,columns] <- scale(training_set[,columns])
  test_set[,columns] <- scale(test_set[,columns], center=train_center, scale=train_scale)
  return(list(
    training_set=training_set,
    test_set=test_set
  ))
}


#' #########################################################
#' ######################### TESTS #########################
#' #########################################################

#' Testset for ML functions
#'
#' Creates a testset based on mtcars for this methods to test.
#' Contains a column 'dep' that describes the following relationship:
#' if cyl == 4 OR qsec < 16,
#' then ml.testset()$dep = 'class1',
#' otherwise ml.testset()$dep = 'class2'
#'
#' @return data.frame object for testing
#' @examples
#' ml.testset()
#' @import datasets
#' @export
ml.testset <- function()
{
  testset <- datasets::mtcars
  testset$disp[3] <- NA
  testset$cyl[4] <- NA
  selector <- (datasets::mtcars$cyl == 4) | (datasets::mtcars$qsec < 16)
  testset[,"dep"] <- ifelse(selector,"class1","class2")
  return(testset)
}
