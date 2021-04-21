# LIBRARY LOADING AND MEMORY SETUP####
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gsubfn)) install.packages("gsubfn", repos = "http://cran.us.r-project.org")

#Setting up larger memory allowance, for more demanding models and systems with limited memory 
gc()
memory.limit(size=100000)

# HELPER FUNCTIONS DEFINITION ####
## Function get_movielens ####
# Extracts the base data into a dataframe named 'movielens' from the grouplens website
get_movielens <- function() {
  # Note: this process could take a couple of minutes
  
  # MovieLens 10M dataset:
  # https://grouplens.org/datasets/movielens/10m/
  # http://files.grouplens.org/datasets/movielens/ml-10m.zip
  
  dl <- tempfile()
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)  
  
  movies <- as.data.frame(movies) 
  
  names(movies) <- c("movieId","title","genres" )
  
  movies <- movies %>% mutate(movieId = as.numeric(movieId),
                                             title = as.character(title),
                                             genres = as.character(genres))
  
  movielens <- left_join(ratings, movies, by = "movieId")
  
  rm(dl,ratings,movies)
  
  return(movielens)
}

## Function createMovieLensDataPartition ####
# Splits the 'movielens' dataframe into a training set named 'edx' and
#a test set named 'validation'
#if remove_sparse is TRUE, the test set will be filtered to only have ratings for
#movies and users which are also present in the training set. remove_sparse being TRUE
# will produce the same data set as outlined in the problem set code.
createMovieLensDataPartition <- function(movielens_data,p,remove_sparse) {
  test_index <- createDataPartition(y = movielens_data$rating, 
                                    times = 1, 
                                    p = p, 
                                    list = FALSE)
  training_set <- movielens_data[-test_index,]
  
  if(remove_sparse) {
    # Make sure userId and movieId in validation set are also in training set
    temp <- movielens_data[test_index,]
    test_set <- temp %>%
      semi_join(training_set, by = "movieId") %>%
      semi_join(training_set, by = "userId")
    # Add rows removed from validation set back into training set
    removed <- anti_join(temp, test_set)
    training_set <- rbind(training_set, removed)
  } else {
    test_set <- movielens_data[test_index,]
  }
  
  #returning a list containing training set and test set data frames
  return(list("training_set" = training_set, "test_set" = test_set))  
}

## Function createMovieLensFolds ####
# Provides the index required to split a 'movielens_data' dataframe, containing the same type of data as the movielens dataframe,
#into k non overlapping folds. Each fold index would define a test set, while the remaining indices would be the training set.
#if remove_sparse is TRUE, each test set will be filtered to only have ratings for
#movies and users which are also present in the training set, then the corresponding index be modified accordingly.
#if remove_sparse is FALSE, this function is equivalent to just applying caret package createFolds to movielens_data.
createMovieLensFolds <- function(movielens_data,k,remove_sparse) {
  test_index_list <- createFolds(y = movielens_data$rating,
                                 k = k,
                                 list=TRUE)
  
  for (name in names(test_index_list)) {
    test_index <- unlist(test_index_list[name])
    movielens_data <- movielens_data %>% 
      mutate(row_index=1:n())
    training_set <- movielens_data[-test_index,]
    
    if(remove_sparse) {
      # Make sure userId and movieId in validation set are also in training set
      temp <- movielens_data[test_index,]
      test_set <- temp %>%
        semi_join(training_set, by = "movieId") %>%
        semi_join(training_set, by = "userId")
      # Add rows removed from validation set back into training set
      removed <- anti_join(temp, test_set)
      training_set <- rbind(training_set, removed)
    } else {
      test_set <- movielens_data[test_index,]
    }
    #storing updated test_index (in case it was modified by the removing of sparse entries)
    test_index <- test_set$row_index
    
    test_index_list[[name]] <- test_index
  }
  return(test_index_list)
}

## Function get_RMSE ####
# Calculates the Root Mean Squared Error between two arrays
get_RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Function get_RMSE_folds ####
#Provides the Cross validation RMSEs for all folds as named list, with names created using the folds names 
#Inputs :
#test_index_folds: named list containing the indices for each fold
#movielens_data: dataframe containing the data from movielens to be partitioned using the folds - this assumes that the movielens_data 
#is the same dataframe that test_index_folds was extracted from.
#Input get_pred_func: function giving the prediction of ratings for a test set test_set considering a training set training_set assumed to be
#of the movielens_data dataframe type. Each model below will have its own get_pred_func.
#Returns: 
#RMSE_folds, the RMSEs for each fold as a named list.
get_RMSE_folds <- function(test_index_folds,movielens_data,get_pred_func) {
  RMSE_folds <- list()
  for (name in names(test_index_folds)) {
    #the training set is the values at indices defined by all folds but the one considered
    training_set <- movielens_data[-test_index_folds[[name]],]
    #the test set is made of the values at indices defined by the current fold
    test_set <- movielens_data[test_index_folds[[name]],]
    #getting RMSE between actual ratings in test_set and those predicted by get_pred_func (model)
    RMSE_named_fold <- get_RMSE(test_set$rating,
                               get_pred_func(training_set,test_set))
    #adding the RMSE to output named list, with name built from fold name
    RMSE_folds[[paste("RMSE",name,sep="_")]] <- RMSE_named_fold
  }
  return(RMSE_folds)
}

## Function get_pred_fold ####
#Provides the prediction vector for the named fold and movielens data
get_pred_fold <- function(test_index_folds,foldname,movielens_data,get_pred_func) {
    #the training set is the values at indices defined by all folds but the one considered
    training_set <- movielens_data[-test_index_folds[[foldname]],]
    #the test set is made of the values at indices defined by the current fold
    test_set <- movielens_data[test_index_folds[[foldname]],]
    #getting model prediction
    pred_fold <- get_pred_func(training_set,test_set)
  return(pred_fold)
}

## Function get_worst_fold_best_method ####
#Determines the current "worst fold" considering results saved so far
#We want to use a single lambda from training data, to be used in the final model. To achieve this in the context of a 10 Folds cross 
#validation, we consider for each method (candidate target method for end model), the fold for which the RMSE 
#is the highest ("worst fold"). We then apply regularisation to that fold and determine which lambda minimises the RMSE, We then 
#use that lambda for all folds to determine the RMSE. 

#Another possibility would be to regularise all folds using the same lambda, picking the lambda that minimises the average RMSE. 
#This would takes much longer to run because it requires 10 times as many calculations. We could also regularise all folds individually 
#to obtain 10 lambdas. this will take 10 times as long as function 1. Unless the models chosen do not perform to the desired targets, 
#we will not explore those methods, so as to speed up the results generation. 

#getting the name of fold for which highest RMSE using the best method (lowest average RMSE) in order to excluse the possibility, 
#as far as possible, that the RMSE be high because of the method being too inadequate (therefore making the notion of worst fold less reliable)
#during the study it will be important to verify that the worst fold thus determined changes or not by re-running that function on the best model known 
#at that point of the study.

#Inputs:
#rmse_folds_results data frame containing the results obtained so far on the various models considered
#Outputs:
#name of the worst fold
get_worst_fold_best_method <- function(rmse_folds_results) {
  worst_fold_best_method <- rmse_folds_results %>%
    gather("Folds","RMSE",-c(method,do_remove_sparse)) %>%
    group_by(method) %>%
    mutate(mean_RMSE=mean(RMSE)) %>%
    ungroup() %>%
    filter(mean_RMSE == min(mean_RMSE)) %>% #picking best method
    filter(RMSE == max(RMSE)) %>% #picking worst fold
    pull(Folds) %>%
    str_remove("RMSE_") 
  return (worst_fold_best_method)
}

## Function get_pred_worstfold ####
#prediction on the worst fold
get_pred_worstfold <- function(rmse_folds_results,test_index_folds,movielens_data,get_pred_func) {
  worst_fold_best_method <- get_worst_fold_best_method(rmse_folds_results)
  pred_worstfold <- get_pred_fold(test_index_folds,worst_fold_best_method,movielens_data,get_pred_func)
  return (pred_worstfold)
}

## Function check_RMSE_worstfold ####
check_RMSE_worstfold <- function(rmse_folds_results,test_index_folds,movielens_data,get_pred_func,verbose = FALSE) {
  #getting worst fold best method
  worst_fold_best_method <- get_worst_fold_best_method(rmse_folds_results)
  rating_worstfold <- movielens_data[test_index_folds[[worst_fold_best_method]],]$rating
  pred_worstfold <- get_pred_fold(test_index_folds,worst_fold_best_method,movielens_data,get_pred_func)
  RMSE_worstfold <- get_RMSE(true_ratings = rating_worstfold,predicted_ratings = pred_worstfold)
  if (verbose) {
    print(paste("RMSE =",RMSE_worstfold,"for worst fold = ",worst_fold_best_method))
  } 
  return(RMSE_worstfold)
}
## Function get_userboundaries_trainingset ####
#Provides a data frame that gives the minimum and maximum rating for each userId in trainingset. 
#this can be used in modelling to apply adjustment to predictions which are outside of that range.
get_userboundaries_trainingset <- function(training_set) {
  user_pred_boundaries <- training_set %>%
    group_by(userId,rating) %>%
    summarise(count = n()) %>%
    mutate(totalcount = sum(count)) %>%
    select(-count) %>%
    filter(rating == min(rating) | rating == max(rating)) %>%
    mutate(rating_boundary = case_when(rating == min(rating)~"minimum_rating",
                                       TRUE ~ "maximum_rating")) %>%
    pivot_wider(names_from = rating_boundary,values_from = rating) %>%
    mutate(maximum_rating = if_else(is.na(maximum_rating),minimum_rating,maximum_rating))
}

## Function Find_Get_Val_Local_Minimum_Golden ####
#Finds the minimum (if it exists) of another function over an interval using golden section search
#this method was chosen because it only requires one extra evaluation each loop 
#after the 4 initial evaluations. This is useful when the function to minimise Get_Val is computationally costly to run
#Input: 
#X_Min (X_Max) the minimum (resp maximum) value of X between which to look for a value minimsing Get_Val
#Get_Val: function to minimise
#Precision: the precision required to determine that the minimising X has been reached. 
#verbose: whether the function will output function evaluation details 
#with_details: determines whether the search steps will be saved in a dataframe, if FALSE the function
# will just return the minimising X
#Returns:
#Minimising X value or Dataframe with search steps (depending on with_details input value)
Find_Get_Val_Local_Minimum_Golden <- function(X_Min,X_Max,
                                              Get_Val,Precision,verbose = TRUE,with_details = FALSE,
                                              max_Nb_Attempt = 1,PrecisionType = "Val") {
  
  failed_Attempt_Nb <- 0
  
  #this is a modified version of the Get_Val function to report 
  #during calculation on the values estimated
  Get_Val_Verbose <- function(X,verbose) {
    OutputVal <- Get_Val(X)
    if (verbose) {
      print(paste("X = ",X))
      print(paste("Val_X =",OutputVal))    
    }
    return(OutputVal)
  }
  #golden ratio
  Phi <- (1+sqrt(5))/2 
  
  Get_Progress_Check <- function(
    PrecisionType,
    X_Left,
    X_Right,
    Val_X_Left,
    Val_X_Right
    ) {
    print("Get_Progress_Check")
    #difference in value between the current values at X_left and X_Right
    if (PrecisionType == "Val") {
      #difference in value between the current values at X_left and X_Right
      progress_check <- Val_X_Right-Val_X_Left
    } else {
      #difference in value between the search X X_left and X_Right
      progress_check <- X_Right-X_Left 
    }
  }
  
  Initialise_Search <- function(
    X_Min,X_Max,
    Val_X_Min,Val_X_Max
    ) {
    print("Initialise_Search")
    #calculating X_Left and X_Right to be located at golden ratio distance from X_Min and X_Max
    X_Left <- (X_Max + Phi*X_Min)/(1+Phi)
    X_Right <- X_Min + Phi*(X_Left - X_Min)
    
    #evaluating the values of function Get_Val at the 4 locations, saving them to reassign later
    #this calls Get_Val a minimum of 4 times at the start of the search
    Val_X_Left <- Get_Val_Verbose(X_Left,verbose)
    Val_X_Right <- Get_Val_Verbose(X_Right,verbose)
    if (missing(Val_X_Min)) {Val_X_Min <- Get_Val_Verbose(X_Min,verbose)}
    if (missing(Val_X_Max)) {Val_X_Max <- Get_Val_Verbose(X_Max,verbose)}
    
    #testing that the values at X_Min, X_Left, X_Right and X_Max are compatible with the 
    #presence of a local minimum.
    #test_local_minimimum <- (min(Val_X_Left,Val_X_Right) < min(Val_X_Min,Val_X_Max)) & (max(Val_X_Left,Val_X_Right) < max(Val_X_Min,Val_X_Max)) #ensuring that a local minimum can be found
    test_exists_minimum <- min(Val_X_Left,Val_X_Right) < min(Val_X_Min,Val_X_Max)
    test_only_minimum <- max(Val_X_Left,Val_X_Right) < max(Val_X_Min,Val_X_Max)
    test_local_minimimum <-   test_exists_minimum & test_only_minimum 
    
    # if (PrecisionType == "Val") {
    #   #difference in value between the current values at X_left and X_Right
    #   progress_check <- Val_X_Right-Val_X_Left
    # } else {
    #   #difference in value between the search X X_left and X_Right
    #   progress_check <- X_Right-X_Left 
    # }
    progress_check <- Get_Progress_Check(PrecisionType,
                                         X_Left,
                                         X_Right,
                                         Val_X_Left,
                                         Val_X_Right)
    
    return(list(X_Left,X_Right,
                Val_X_Left,Val_X_Right,Val_X_Min,Val_X_Max,
                test_only_minimum,test_exists_minimum,test_local_minimimum,
                progress_check))
  }
  
  list[X_Left,X_Right,
       Val_X_Left,Val_X_Right,Val_X_Min,Val_X_Max,
       test_only_minimum,test_exists_minimum,test_local_minimimum,
       progress_check] <- Initialise_Search(
         X_Min,X_Max
         )
  
  # print(list(X_Left,X_Right,
  #            Val_X_Left,Val_X_Right,Val_X_Min,Val_X_Max,
  #            test_only_minimum,test_exists_minimum,test_local_minimimum,
  #            progress_check))
  
  #setting the output to the one yielding the lowest RMSE
  if (Val_X_Right-Val_X_Left>0) {
    Output <- X_Left
  } else {
    Output <- X_Right
  } 
  
  #we are still searching if the difference in value is greater than required precision
  Searching <- abs(progress_check) > Precision
  
  
  step <- 0
  
  #data frame to save search results in
  SearchRecord <- data.frame(name = character(),
                             X = numeric(), 
                             Val = numeric(),
                             test_local_minimum = logical(), 
                             Searching = logical(),
                             step = numeric())
  
  #saving results in SeachRecord dataframe, only if with_details input is TRUE
  if (with_details) {
    step <- step + 1
    SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Min", X = X_Min, Val = Val_X_Min, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
    SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Left", X = X_Left, Val = Val_X_Left, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
    SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Right", X = X_Right, Val = Val_X_Right, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
    SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Max", X = X_Max, Val = Val_X_Max, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
  }
  
  
  while(Searching) {
    #testing that the values found between current values of X_Max and X_Min are
    #compatible with finding a minimum, if not the function will exist and return NA
    if (test_local_minimimum) {
      if (Val_X_Left < Val_X_Right) {
        print("minimum is between X_Min and X_Right")
        #updating X_Max to X_Right
        X_Max <- X_Right
        Val_X_Max <- Val_X_Right
        #updating X_Right to X_Left
        X_Right <- X_Left
        Val_X_Right <- Val_X_Left
        #calculating new X_Left using golden ratio and calculating corresponding new Val_X_Left
        X_Left <- (X_Max + Phi*X_Min)/(1+Phi)
        Val_X_Left <- Get_Val_Verbose(X_Left,verbose)
      } else {
        print("minimum is between X_Left and X_Max")
        #updating X_Min to X_Left
        X_Min <- X_Left
        Val_X_Min <- Val_X_Left
        #updating X_Left to X_Right
        X_Left <- X_Right
        Val_X_Left <- Val_X_Right
        #calculating new X_Right using golden ratio and calculating corresponding new Val_X_Right
        X_Right <- X_Min + Phi*(X_Left - X_Min)
        Val_X_Right <- Get_Val_Verbose(X_Right,verbose)
      } 
      #update progress,local minimum test and precision reached check
      test_exists_minimum <- min(Val_X_Left,Val_X_Right) < min(Val_X_Min,Val_X_Max)
      test_only_minimum <- max(Val_X_Left,Val_X_Right) < max(Val_X_Min,Val_X_Max)
      test_local_minimimum <-   test_exists_minimum & test_only_minimum 
      # if (PrecisionType == "Val") {
      #   #difference in value between the current values at X_left and X_Right
      #   progress_check <- Val_X_Right-Val_X_Left
      # } else {
      #   #difference in value between the search X X_left and X_Right
      #   progress_check <- X_Right-X_Left 
      # }
      progress_check <- Get_Progress_Check(PrecisionType,
                                           X_Left,
                                           X_Right,
                                           Val_X_Left,
                                           Val_X_Right)
      Searching <- abs(progress_check) >= Precision
      #saving progress in SearchRecord dataframe
      if (with_details) {
        step <- step + 1
        SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Min", X = X_Min, Val = Val_X_Min, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
        SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Left", X = X_Left, Val = Val_X_Left, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
        SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Right", X = X_Right, Val = Val_X_Right, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
        SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Max", X = X_Max, Val = Val_X_Max, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
      }
      #udpating searching boundaries
      if (progress_check>0) {
        Output <- X_Left
      } else {
        Output <- X_Right
      }
    } else {
      #this attempt failed
      failed_Attempt_Nb <- failed_Attempt_Nb +1
      if (failed_Attempt_Nb < max_Nb_Attempt & !test_exists_minimum) {
        print(paste("local minimum test failed at attempt",failed_Attempt_Nb,"of",max_Nb_Attempt,"allowed."))
        print("Attempting minimum search again between X_Min and X_left")
        #it is possible that an existing minimum was missed, for instance if the search interval was too large
        #we reset the search interval to be between X)Min and X_Left
        X_Max <- X_Left
        Val_X_Max <- Val_X_Left
        
        #Initialising the search, #=providing Val_Xmin and Val_XMax as they have already been evaluated
        list[X_Left,X_Right,
             Val_X_Left,Val_X_Right,Val_X_Min,Val_X_Max,
             test_only_minimum,test_exists_minimum,test_local_minimimum,
             progress_check] <- Initialise_Search(
               X_Min,X_Max,
               Val_X_Min, Val_X_Max
               ) 
        
        if (with_details) {
          step <- step + 1
          SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Min", X = X_Min, Val = Val_X_Min, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
          SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Left", X = X_Left, Val = Val_X_Left, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
          SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Right", X = X_Right, Val = Val_X_Right, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
          SearchRecord <- SearchRecord %>% bind_rows(bind_cols(name = "X_Max", X = X_Max, Val = Val_X_Max, test_local_minimimum = test_local_minimimum,Searching = Searching, step = step))
        }
        # print(list(X_Left,X_Right,
        #            Val_X_Left,Val_X_Right,Val_X_Min,Val_X_Max,
        #            test_only_minimum,test_exists_minimum,test_local_minimimum,
        #            progress_check))
      } else{
        print("local minimum test failed permanently, returning NA")
        Searching <- FALSE
        Output <- NA
      }
    }  
    
  }
  
  #if with_details is TRUE we return the output along with the searchrecord dataframe
  if (with_details) {
    return(list(Output = Output, SearchRecord = SearchRecord))
  } else (
    return(Output)
  )
}

## Function save_input_data ####
#Function to save the static input data, which will not change throughout the script once created
save_input_data <- function() {
  filename <- "Data/Input.RData"
  candidateobjectlist <- c("edx", 
                  "validation" , 
                  "test_index_folds" , 
                  "release_years_ratings", 
                  "timestamp_day_rating",
                  "do_remove_sparse",  
                  "worst_fold_best_method")  
  objectlist <- c()
  for (object in candidateobjectlist) {
    if (exists(object)) {
      objectlist <- c(objectlist,object)
    }
  }
  save(file=filename, list = objectlist)
  print(paste("file",filename,"saved."))
}

## Function save_output_data ####
#Function to save all intermediary  output results in  RData file, with name subset with savenum
#it outputs savenum increased with 1, which allows to have incremental saves throughout the script run 
# by calling #savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) 
# this allows to save time in case of a crash by resuming the run from the last place it crashed 
# (loading results already produced)
save_output_data <- function(savenum,save_output,objectlist) {
  if (save_output) {
    filenameroot <- "Data/Output_"
    filename_numbered <- paste0(filenameroot,stringr::str_pad(savenum, 3, pad = "0"),".RData") #paste0("Data/Output_",str_pad(savenum, 3, pad = "0"),".RData")
    filename_latest <- paste0(filenameroot,"Latest",".RData")
    if(missing(objectlist)) {
      objectlist <- ls(name = .GlobalEnv)
    }
    #excluding large static input data objects
    objectlist <- objectlist[objectlist != "edx" & 
                               objectlist != "validation" &
                               objectlist != "test_index_folds" &
                               objectlist != "release_years_ratings" &
                               objectlist != "timestamp_day_rating"]

    #excluding loading logic objects so they do not get overwritten upon loading
    objectlist <- objectlist[objectlist != "load_input_data" & 
                               objectlist != "load_output_data"]
    save(file=filename_numbered, list = objectlist)
    print(paste("file",filename_numbered,"saved."))
    savenum <- savenum + 1
    save(file=filename_latest, list = objectlist)
    print(paste("file",filename_latest,"saved."))
  }
  return (savenum)
}


# RUNNING OR LOADING LOGIC ####

# this section sets up whether the data will be loaded from a RData file,
# or will be calculated from scratch. 
# We differentiate between Input Data, which is the data from movielens, 
# without further analysis just arranged in a usable way; and Output Data 
# which is the data produced from the analysis typically the models run and 
# corresponding RMSE results. 

# Even if the option to load the data is chosen, it is advised to read through 
# the code that produces it as it contains explanation as to how the input and output data
# were produced. 

# By Default we will load the data if it is available in the project folder. Otherwise, it
# will be recreated. You can hard set load_input_data and load_output_data so the code
# will run even if the RData files are available. 

#input data loaded if available
load_input_data <- file.exists("Data/Input.RData") 
#output data loaded if available
#This tells us that the whole of the run in section "OUTPUT DATA CREATION" is available and ready to be loaded
load_output_data <- file.exists("Data/Output_020.RData") 
savecount <- 1
savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save 

# INPUT DATA CREATION (data set creation and cleaning)####
# This section produces or loads the input data to the analysis 
if (load_input_data) {
  # load input data from RData file (if it was generated in a previous run)
  load("Data/Input.RData")
} else {
  # this section creates input data from scratch and contains the code used to generate
  # the data in the Input.RData file  
  # it should run in an indicative duration of 4.456573 mins
  ## DATA SETS CREATION ####
  run_input_data_start_time <- Sys.time()
  
  #Loading a workspace containing only the movielens dataframe, it it exists.
  #This allow to save the time time loading it from the web 
  if(file.exists("movielens.RData")) {
    load("movielens.RData")  
  } else {
    #if the movielens dataframe does not exist in the workspace, it is created 
    #using the function previously defined
    #Note: this process could take a couple of minutes
    movielens <- get_movielens()
    # Saving into a file for faster future running
    save(movielens,file="movielens.RData")
  }
  
  #Extracting training set ('edx') and test set ('validation') dataframes using the function
  #extract_edx_valiration previously defined #with remove_sparse TRUE so the validation
  #dataframe will not contain ratings for movies or users which are not present in
  #the edx data frame
  set.seed(1, sample.kind="Rounding")
  #createMovieLensDataPartition returns a named list containing edx and validation dataframes
  temp <- createMovieLensDataPartition(movielens_data = movielens,
                                       p= 0.1,
                                       remove_sparse = TRUE)
  edx <- temp$training_set
  validation <- temp$test_set
  rm(temp)
  rm(movielens)
  
  #Split edx in training and testing set 10 times
  #this is so lambda can be optimised without using the validation data set
  do_remove_sparse <- TRUE
  test_index_folds <- createMovieLensFolds(movielens_data = edx,
                                           k=10,
                                           remove_sparse = do_remove_sparse) 
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  ## DATA CLEANING AND METHODS####
  ### Individual genres extraction ####
  
  #the genres field is a string made of the concatenation of individual genres
  #separated by the symbol "|"
  
  gc()
  
  #we create the full list of genres for the edx data set, to speed up extraction later on
  
  indgenres_movie_summary <- edx %>% 
    select(movieId,genres) %>%
    unique() %>%
    mutate(indgenres = strsplit(genres, "\\|")) %>% #split string into a list
    unnest(indgenres)  %>%#one line per genre
    group_by(indgenres) %>%
    mutate(nb_movies = n())
  
  #removing genres with less than 50 movies
  indgenres_relevant <- indgenres_movie_summary %>%
    filter(nb_movies >50) %>% 
    select(movieId,indgenres)
  
  ### Release year extraction ####
  
  #the movie release year is contained as part of the movie title
  #it is located within brackets at the end of the title string
  #it is extracted by data wrangling using regex pattern \\((\\d{4})\\)$"
  
  release_years <- edx %>%
    select(movieId,title) %>%
    unique() %>%
    mutate(release_year = as.numeric(str_match(string = title, pattern = "\\((\\d{4})\\)$")[,2])) %>%
    select(movieId,release_year)  
  
  release_years_ratings <- edx %>%
    left_join(release_years,by="movieId") %>%
    select(movieId,release_year,rating)
  
  mu <- mean(edx$rating)
  
  release_years_ratings_summary <- release_years_ratings %>%
    group_by(release_year) %>%
    summarise(nb_ratings = n(),
              nb_movies = n_distinct(movieId),
              dev_mean_rating =mean(rating)-mu,
              mean_rating =mean(rating),
              sd_rating = sd(rating)
    ) %>%
    ungroup() %>%
    mutate(rating_per_movie = nb_ratings/nb_movies)
  
  p_ry_ratingsvsmovie <- release_years_ratings_summary %>%
    pivot_longer(names_to = "metric",cols = c(rating_per_movie,nb_movies,dev_mean_rating)) %>%
    ggplot(aes(release_year,value,fill=metric)) +
    geom_col() +
    facet_grid(metric~.,scales = "free")
  print(p_ry_ratingsvsmovie) 
  
  ### Timestamp extraction ####
  timestamp_day_rating <- edx %>%
    select(timestamp) %>%
    unique() %>%
    mutate(time_rating = as.character(round_date(as_datetime(timestamp),"day")))
  
  ### Regularisation implementation####
  #We consider the effect of regularisation to the model. In order to increase how much the model extends to unseen data, it is important
  #not to overfit the model the training data. This can happen when considering effects for categories with a small number of observations 
  #and extreme values. By penalising groups with a small number of observations it is possible to limit that effect. 
  #Without outlining in details the maths, the model is modified by considering not the simple average of the deviation of the rating compared
  # to the current model, but the deviation of the absolute sum of the deviations divided by the number of observation n plus a penalty parameter
  # lambda. For lambda = 0, we are back to the non regularised model. When lambda is non null, the categories with a small n
  # will have their effect penalised. For categories with a large n, the effect of lambda will be smaller, and the contribution will be mostly
  # unchanged. 
  
  #The RMSE function of lambda is expected to reach a minimum for the optimal lambda. In order to obtain that value, 
  #several methods are possible. 
  #One could pick lambda values in a range for which RMSE is calculated, and the lambda for which the smallest value is reached
  #is kept. This however requires trial and error to reach the desired value and precision. 
  #Instead, in order to reach the desired precision in a more automated way, a function was written to find the minimum. See "FUNCTIONS DEFINITION"
  #above, function "Find_Get_Val_Local_Minimum_Golden" 
  
  #Furthermore instead of calculating a lambda for each of the folds, we only determine a lambda for the "worst fold" that is the fold that gives
  # the highest RMSE before regularisation. This choice is because all other aspects being equal, the requirement for regularisation would explain
  # partly the higher RMSE for the worst fold, therefore it appears conservative that this be used to determine a lambda to be used in the final model.
  # one could also determine the optimum lambda for each fold and then an average be used however because the determination of the optimum lambda is 
  #computationally expensive, a single determination was used. 
  run_input_data_end_time <- Sys.time()
  run_input_data_duration <- run_input_data_end_time - run_input_data_start_time 
  print(run_input_data_duration) #Time difference of 
  #saving produced input data in Input.RData for future retrieval
  save_input_data()
}

# OUTPUT DATA CREATION (data exploration, prediction models training and runs)####
# This section produces or loads the output data from the analysis
# It will not run if you already have the Output018.RData file.
# This section contains the explanation on how data was produced which can be found 
# in the code and comments 
if (load_output_data) {
  #This will load the successive models RMSE output data, providing the same data
  # as would be produced running all the models below
  load("Data/Output_020.RData")
  savecount <- 21
} else {
  ## DATA EXPLORATION ####
  nb_users <- length(unique(edx$userId))
  nb_movies <- length(unique(edx$movieId))
  nb_observations_edx <- nrow(edx)
  nb_observations_validation <- nrow(validation)
  nb_observations_tot <- nb_observations_edx + nb_observations_validation
  edx_varnames <- names(edx)

  
  p_ratings_spread <- edx %>%
    group_by(rating) %>%
    summarise(nb_rating = n()) %>%
    ggplot(aes(rating,nb_rating)) +
    geom_col()
  
  wholeratingperc_spread <- edx %>%
    mutate(is_whole_rating = ((10*rating)%%10 == 0) ) %>%
    group_by(userId) %>%
    summarise(perc_whole_rating = sum(is_whole_rating)/n()) %>%
    ungroup() 
  
  p_wholeratingperc_spread <- wholeratingperc_spread %>%
    ggplot(aes(perc_whole_rating)) +
    geom_histogram()
  
  typeraters_spread <- wholeratingperc_spread  %>%
    group_by(userId) %>%
    summarise(whole_rating_type = case_when(
      perc_whole_rating == 1.0 ~ "whole_rating_only",
      perc_whole_rating == 0.0  ~ "partial_rating_only",
      TRUE ~ "mix_whole_partial"
    )
    ) %>%
    ungroup() %>%
    group_by(whole_rating_type) %>%
    summarise(nb_of_type = n()) #44763 users give only whole rating, 25109 with mix and 6 only with exclusive partial rating
  
  typeraters_spread <- wholeratingperc_spread  %>%
    group_by(userId) %>%
    summarise(whole_rating_type = case_when(
      perc_whole_rating == 1.0 ~ "whole_rating_only",
      perc_whole_rating == 0.0  ~ "partial_rating_only",
      TRUE ~ "mix_whole_partial"
    )
    ) %>%
    ungroup() %>%
    group_by(whole_rating_type) %>%
    summarise(nb_of_type = n()) #44763 users give only whole rating, 25109 with mix and 6 only with exclusive partial rating
  
  preferenceraters_spread <- wholeratingperc_spread  %>%
    group_by(userId) %>%
    summarise(rating_preference = case_when(
      perc_whole_rating > 0.5 ~ "whole_rating",
      perc_whole_rating <=  0.5 ~ "partial_rating",
      TRUE ~ "error"
    )
    ) %>%
    ungroup() %>%
    group_by(rating_preference) %>%
    summarise(nb_of_type = n()) #44763 users give only whole rating, 25109 with mix and 6 only with exclusive partial rating
  
  #TODO attempt to round to closest whole rating the predicted ratings for users that have so far only ever given whole ratings
  #TODO potentially refine to round to 0.5 for those who have gone 0.5 more and whole for those who have gone whole more
  
  p_indgenres_spread <- indgenres_movie_summary %>%
    ungroup() %>%
    select(indgenres,nb_movies) %>%
    unique() %>%
    mutate(indgenres = reorder(indgenres,nb_movies)) %>%
    ggplot(aes(indgenres,nb_movies)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

  ## MODELS COMPARISON ####
  #This section will generate RMSE for successive models, from scratch and contains the code
  #that has generated the Output_020.RData file.
  #Each subsection will incrementally save the output data in file Output_xxx.RData where xxx 
  #is an integer starting 1 reaching 18. This allows for intermediate results to be saved
  #in case of a crash during a run, so the data can be reloaded and output data run resumed to 
  #save time. 
  #The models take an indicative time of 5.718817 hours to run. 
  # It is advised to first run the script without any change which should load the 
  # Ouput_018.RData file contained in the Data folder. Once the data is loaded,
  # specific parts of the code can be re-ran.
  run_output_data_start_time <- Sys.time()
  #saving produced input data in Input.RData for future retrieval
  ### Model: naive average ####
  
  #we predict that each new rating is the average of the ratings for
  #all previously rated movies by all users
  get_pred_mean <- function(training_set,test_set) {
    return(mean(training_set$rating))
  }
  
  #empty dataframe to contain the RMSE results 
  if (!exists("rmse_folds_results")) {
    rmse_folds_results <- data.frame()
  }
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mean
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="average",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_mean))))

  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  ### Model: average by user ####
  
  #we predict that each new rating for a given user is the average
  #of the ratings for all previously rated movies by that same user
  #practically we calculate the average deviation (or effect) of rating
  #for each user, then predict the overall average plus that effect will
  #be that user rating for each new movie
  get_pred_user <- function(training_set,test_set) {
    mu <- mean(training_set$rating)
    effect_user <- training_set %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu)) %>%
      ungroup()
    pred_mean_user <- test_set %>%
      left_join(effect_user,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_u) %>%
      pull(pred)
    return(pred_mean_user)
  }

  # RMSE on worst fold
  check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_user,
                       verbose = TRUE)
  #RMSE = 0.979776212801374 for worst fold =  Fold01
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mean_user
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="user",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_user))))
  
  
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  ### Model: average by movie ####
  
  #we predict that each new rating for a given movie is the average
  #of the ratings for that same movie by all other users
  #practically we calculate the average deviation (or effect) of rating
  #for each movie, then predict the overall average plus that effect will
  #be that movie rating for each new user
  get_pred_movie <- function(training_set,test_set) {
    mu <- mean(training_set$rating)
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup()
    pred_mean_movie <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i) %>%
      pull(pred)
    return(pred_mean_movie)
  }
  
  # To conservatively evaluate the new model, we get the RMSE of that model applied on the fold that gave the highest RMSE using the 
  # best model (lowest average RMSE) considered before that new model
  # RMSE on worst fold
  RMSE_movie <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_movie,
                       verbose = TRUE)
  #RMSE = 0.944699640916886 for worst fold =  Fold01
  
  ### Model: average by movie, with Rounding to nearest allowed value ####

  #We saw in the data exploration section that only possible ratings are 0.5-5 in increments of 0.5. The question then is whether for a model that
  #gives results which are not within those actually allowed values, will there be an improvement by rounding to the nearest allowed value?
  
  #We use the wholeratingperc_spread derived in the data exploration section but applying the extraction
  #to the training_set as it is the only known data. Then if a user has more whole rating given than a set minimum percentage provided 
  #as function parameter (min_perc_whole_rating) then rounding to nearest whole rating is applied. 
  #By setting min_perc_whole_rating to 1.0 for instance,we can apply nearest whole rating rounding only to users that have so far only given whole ratings.
  
  #We also place a further control that such a rounding should only occur if the difference between the initial prediction to the nearest whole rating is 
  #lower than parameter max_diff_pred_whole provided as input to the function. This allows for instance if set to 0.1 to only round to nearest whole rating the
  #predicitions which are quite close to a whole rating (while setting to 0.5 would result in all non whole predictions to be rounded).
  #the latest parameter round_val allows to choose to which nearest increment to round. If set to 1, if will round to nearest whole rating, for instance but could
  #be set to 0.5 to round to nearest half rating.
  get_pred_mean_movie_rounded_mpwr_mdpw <- function(training_set,test_set,
                                          min_perc_whole_rating = 1.0,
                                          max_diff_pred_whole = 0.5,
                                          round_val = 1.0) {
    
    wholeratingperc_spread_training_set <- training_set  %>%
      mutate(is_whole_rating = ((10*rating)%%10 == 0) ) %>%
      group_by(userId) %>%
      summarise(perc_whole_rating = sum(is_whole_rating)/n()) %>%
      ungroup() 
    
    mu <- mean(training_set$rating)
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    pred_mean_movie <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i) %>%
      left_join(wholeratingperc_spread) %>%
      mutate(pred = case_when(perc_whole_rating>=min_perc_whole_rating & abs(round(pred,0)-pred) < max_diff_pred_whole ~ round(pred/round_val)*round_val,
                              TRUE ~ pred)) %>% 
      pull(pred)
    return(pred_mean_movie)
  }
  
  #we user the model with min_perc_whole_rating = 1.0,  max_diff_pred_whole = 0.1, and   round_val = 1.0 that is, rounding  to the nearest whole rating,only for
  #users who only gave whole ratings, if the rating is within 0.1 or less of a whole rating. 
  get_pred_mean_movie_rounded <- function(training_set,test_set) {
    return(get_pred_mean_movie_rounded_mpwr_mdpw(training_set,test_set,
                                                 min_perc_whole_rating = 1.0,
                                                 max_diff_pred_whole = 0.1,
                                                 round_val = 1.0))
  }

  # RMSE on worst fold
  RMSE_movie_rounded <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_mean_movie_rounded,
                       verbose = TRUE)
  #RMSE = 0.944882070377347 for worst fold =  Fold01
  
  #Rounding does NOT improve results (result without rounding was 0.9446996), even in the conservative case considered.
  #other types of systematic rounding were considered (rounding to nearest 0.5, other distance from nearest allowed value) which 
  # also did not improve results. 
  
  
  #we can try to see why there was no improvement overall, by looking at which predictions were improved and which were not and by how much
  worst_fold_best_method <- get_worst_fold_best_method(rmse_folds_results)
  comp_mean_movie <- data.frame(pred_mean_movie = get_pred_worstfold(rmse_folds_results,test_index_folds,edx,get_pred_movie),
                                pred_mean_movie_rounded = get_pred_worstfold(rmse_folds_results,test_index_folds,edx,get_pred_mean_movie_rounded),
                                actual_rating =  edx[test_index_folds[[worst_fold_best_method]],]$rating) %>%
    mutate(sqdev = (pred_mean_movie - actual_rating)^2) %>%
    mutate(sqdev_rounded = (pred_mean_movie_rounded - actual_rating)^2) %>%
    mutate(sqdev_diff = sqdev-sqdev_rounded) %>%
    mutate(sqdev_improved = case_when(sqdev_diff > 0 ~"better",
                                      sqdev_diff < 0 ~"worse",
                                      TRUE~ "same")) %>%
    mutate(rmse_all = sqrt(mean(sqdev))) %>% 
    mutate(mse_all = mean(sqdev)) %>% 
    mutate(rmse_all_rounded = sqrt(mean(sqdev_rounded))) %>%
    mutate(mse_all_rounded = mean(sqdev_rounded))
  
  comp_mean_movie_summary <- comp_mean_movie %>%
    group_by(sqdev_improved,rmse_all,mse_all,rmse_all_rounded,mse_all_rounded) %>%
    summarise(count = n(),
              rmse = sqrt(mean(sqdev)),
              rmse_rounded = sqrt(mean(sqdev_rounded)),
              mse = mean(sqdev),
              mse_rounded = mean(sqdev_rounded)
              )  %>%
    ungroup() %>%
    mutate(Dmse = mse_rounded - mse) %>%
    mutate(weighted_Dmse = count*Dmse)
  
  # we see that although the predictions are improved in circa 3 times more cases than worsened, the worsening in terms of RMSE adds up to more than 
  # the improvement for the cases where it is improved. 
  # this can be seen looking at weighted mses for each type of change (worse, same, better)
  # Further work on this could include checking whether the users/movies for which it is improved, non overlapping with the ones for which it is 
  # worsened, to the point that it would be possible to discriminate a subset of users/movies on which to apply rounding or not and still achieve improvement.
  # However to do this, we would need to create a further partition of one of the folds of the edx to create new unseen data on which to test this. This is 
  # because to pick a method based on information obtained from the actual ratings, that information needs not to be be in the validation set.
  
  comp_mean_movie_category_summary <- data.frame(pred_mean_movie = get_pred_worstfold(rmse_folds_results,test_index_folds,edx,get_pred_movie),
                                              pred_mean_movie_rounded = get_pred_worstfold(rmse_folds_results,test_index_folds,edx,get_pred_mean_movie_rounded)) %>%
    bind_cols(edx[test_index_folds[[worst_fold_best_method]],]) %>%
    rename(actual_rating = rating) %>%
    mutate(sqdev = (pred_mean_movie - actual_rating)^2) %>%
    mutate(sqdev_rounded = (pred_mean_movie_rounded - actual_rating)^2) %>%
    mutate(sqdev_diff = sqdev-sqdev_rounded) %>%
    mutate(sqdev_improved = case_when(sqdev_diff > 0 ~"better",
                                      sqdev_diff < 0 ~"worse",
                                      TRUE~ "same")) %>%
    mutate(rmse_all = sqrt(mean(sqdev))) %>% 
    mutate(mse_all = mean(sqdev)) %>% 
    mutate(rmse_all_rounded = sqrt(mean(sqdev_rounded))) %>%
    mutate(mse_all_rounded = mean(sqdev_rounded)) %>%
    group_by(userId) %>%
    mutate(nb_ratings = n()) %>%
    group_by(userId,sqdev_improved) %>%
    mutate(nb_ratings_improvecat = n()) %>%
    mutate(improve_category = case_when(nb_ratings == nb_ratings_improvecat ~ "single",
                                        TRUE ~ "mixed")) %>%
    ungroup() %>%
    group_by(improve_category,sqdev_improved) %>%
    summarise(count_category = n(),
              mse = mean(sqdev),
              mse_rounded = mean(sqdev_rounded),
              Dmse = mse_rounded - mse,
              weighted_Dmse = count_category*Dmse
              )  %>%
    ungroup() %>%
    mutate(contribution = weighted_Dmse/sum(count_category))
  
  # As can be seen from the summary table, even if we could come up with a cross validated set of users for which rounding improve result, at most we could 
  # improve the result by 4e-05 (likely, the subset would be smaller hence impact also smaller), so we will not be making that effort.
  
  # As this model does not improve RMSE, the results will not be added to the rmse_folds_results dataframe and we will NOT apply systematic rounding in the
  # models going forward.

  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  ### Model: average by movie, with User Boundary Rounding ####
  
  #Is there a minimum and maximum rating or more generally set of possible values that is specific enough to a user that prediction can be improved
  #by setting those boundaries as the only possible values for boundaries / rounding ?
  #By looking at the minimum and maximum rating given so far by each user, we apply a simpler rule which is that if a prediction is lower than the 
  #lowest rating given so far by the user, that lower boundary is predicted instead. Similarly, if a prediction is higher than the highest rating given so far, 
  #the prediction is changed to be equal to that higher boundary. This is based on the assumption that each user will have their idea of what is a 
  #maximum and minimum rating, keeping to this rather than the full range.
  get_pred_mean_movie_rub <- function(training_set,test_set) {
    
    #getting minimum and maximum boundaries for ratings given by each user in the trainingset
    user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    
    mu <- mean(training_set$rating)
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    pred_mean_movie <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i) %>%
      #adding information on the boundaries
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      #adjust any new rating lower (resp higher) than minimum (resp maximum)
      #rating given thus far by user, to that minimum (resp maximum) value
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>% 
      pull(pred)
    return(pred_mean_movie)
  }
  
  # RMSE on worst fold
  RMSE_movie_userboundary <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_mean_movie_rub,
                       verbose = TRUE)
  #RMSE = 0.943558937204152 for worst fold =  Fold01
  #user based boundary rounding DOES improve results (result without rounding was 0.9446996).
  #Therefore, in all further models, we will apply user boundaries rounding. 
  #For conciseness we will not specify it in model descriptions
  
    
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mean_movie_rub
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_mean_movie_rub))))
 
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  ### Model: average by movie then user ####
  
  #we predict that the rating for movie i, user u is t the sum of
  #the movie effect and the user effect as recalculated using the baseline
  #with movie effect included 
  get_pred_movie_user <- function(training_set,test_set) {
    mu <- mean(training_set$rating)
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu-b_i)) %>%
      ungroup() 
    pred_mean_movie_user <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u) %>%
      pull(pred)
    return(pred_mean_movie_user)
  }
  
  # RMSE on worst fold
  RMSE_movie_user <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_movie_user,
                       verbose = TRUE)
  #RMSE = 0.866792775519839 for worst fold =  Fold01
  
  ### Model: average by movie then user with Global Boundary Rounding ####
  
  # Because there are now two effects incorporated the b_u effect can result in predictions outside 
  # of the possible values which as we have seen in the data exploration section, is in the [0.5,5] range
  # in increments of 0.5. This would happen for instance if the average rating over all movies was high
  # then a particular user has consistently lower than average ratings. For a movie that is on average
  # itself with a low rating, both the b_u and b_i would be negative with magnitude high enough to 
  # result in a prediction outside of the range. 
  pred_movie_user_worstfold <- get_pred_worstfold(rmse_folds_results,test_index_folds,edx,get_pred_movie_user)
  nb_pred_total <- length(pred_movie_user_worstfold)
  nb_pred_outofrange <- length(pred_movie_user_worstfold[pred_movie_user_worstfold > 5 | pred_movie_user_worstfold <0.5])
  rm(pred_movie_user_worstfold)
  print(nb_pred_outofrange)
  #2161 predicted ratings are indeed out of the actual possible range for ratings.
  
  # To improve on this, for models with more than one effect, we can change any prediction that is lower 
  # then 0.5 to 0.5 and any prediction that is higher than 5 to 5. This should improve the overall RMSE
  # because any prediction that is outside of the possible range will be improved by the operation. 
  get_pred_movie_user_ab <- function(training_set,test_set) {
    mu <- mean(training_set$rating)
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu-b_i)) %>%
      ungroup() 
    pred_mean_movie_user_ab <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>% #rounding up to 0.5 if prediction is below 0.5
      mutate(pred = if_else(pred > 5,5,pred)) %>% #rounding down to 5 if prediction is higher than 5
      pull(pred)
    return(pred_mean_movie_user_ab)
  }
  
  # RMSE on worst fold
  RMSE_movie_user_globalboundary <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_movie_user_ab,
                       verbose = TRUE)
  #RMSE = 0.866596639311349 for worst fold =  Fold01
  #this confirms improvement compared to the case without adjustment for allowed boundaries (0.8667928)
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  ### Model: average by movie then user with Global Boundary Rounding followed by User Boundary Rounding ####
  #we also apply user based boundaries adjustment and see if there is further improvement or if 
  #the two adjustments are mutually exclusive
  get_pred_movie_user_ab_rub <- function(training_set,test_set) {
    #getting minimum and maximum boundaries for ratings given by each user in the trainingset
    user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    
    mu <- mean(training_set$rating)
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu-b_i)) %>%
      ungroup() 
    pred_mean_movie_user <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>% #rounding up to 0.5 if prediction is below 0.5
      mutate(pred = if_else(pred > 5,5,pred)) %>% #rounding down to 5 if prediction is higher than 5
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating ~ minimum_rating, #applying minimum user boundary
                              pred > maximum_rating ~ maximum_rating, #applying maximum user boundary
                              TRUE ~ pred)) %>%
      pull(pred)
    return(pred_mean_movie_user)
  }
  
  # RMSE on worst fold
  RMSE_movie_user_globalthenuserboundary <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_movie_user_ab_rub,
                       verbose = TRUE)
  #RMSE = 0.866507592226623 for worst fold =  Fold01
  #further improvement on doing only allowed global boundaries adjustment (0.8665966)
  
  #In all models below with more than 1 effect we will therefore apply in succession two adjustments : overall allowed boundaries adjustment and 
  #user based boundary adjustment. It is clear that for users that have already submitted ratings, only applying the second adjustment will result
  #in the same improvement since the minimum and maximum rating given by a user are within the allowed ranges. However this will not be the case for 
  #new users with no submitted ratings so far. For all models incorporating more than one effect that is not user based, we will apply both effect. 
  
  #Again for conciseness we will not specify it in the model description.
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mean_movie_user_ab_rub
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_movie_user_ab_rub))))
  
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  ### Model: average by movie then user then genres ####
  #we predict that the rating for movie i, user u is the sum of
  #the following effects in that order movie effect, user effect and 
  #movie genres effect, each applied on the previous baseline.
  #we model the genres by taking directly the series of genres as a unique genre
  get_pred_movie_user_genres <- function(training_set,test_set) {
    #getting minimum and maximum boundaries for ratings given by each user in the trainingset
    user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    
    mu <- mean(training_set$rating)
    
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu-b_i)) %>%
      ungroup() 
    
    effect_movie_user_genres <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      group_by(userId,genres) %>%
      summarise(b_g = mean(rating-mu-b_i-b_u)) %>%
      ungroup() 
    
    pred_mean_movie_user_genres <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      left_join(effect_movie_user_genres,by = c("userId","genres")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_g) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>% #rounding up to 0.5 if prediction is below 0.5
      mutate(pred = if_else(pred > 5,5,pred)) %>% #rounding down to 5 if prediction is higher than 5
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating ~ minimum_rating, #applying minimum user boundary
                              pred > maximum_rating ~ maximum_rating, #applying maximum user boundary
                              TRUE ~ pred)) %>%
      pull(pred)
    
    return(pred_mean_movie_user_genres)
  }
  
  # RMSE on worst fold
  check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_movie_user_genres,
                       verbose = TRUE)
  #RMSE = 0.914505595301922 for worst fold =  Fold01
  #this is a worse model than movie+user model (0.8665076)
  
  #Possible reasons for this being the case is the large number of genres with a small number of observations
  
  # Because the model does not improve results, it will not be added to the results data frame. 

  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save

  ### Model: movie regularised ####
  
  #we apply regularisation to the movie only model, using the parameter l_m (movie effet lambda) to 
  #penalise movies with a small number of ratings and improve how well the model generalise to new data
  #prediction function
  get_pred_m_reg <- function(training_set,
                             test_set,
                             user_pred_boundaries_training_set,
                             l_m 
                             ) {
    
    if (missing(user_pred_boundaries_training_set)) {
      print("extracting user_pred_boundaries_training_set")
      user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    } else {
      print("using user_pred_boundaries_training_set provided as input")
    }

    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie_reg <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = sum(rating-mu)/(n()+l_m)) %>%
      ungroup() 
    
    print("prediction")
    pred_mean_movie_reg <- test_set %>%
      left_join(effect_movie_reg,by="movieId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>% 
      pull(pred)
    return(pred_mean_movie_reg)
  }
  
  # We define a function giving the RMSE of the movie only regularised model for the worst fold and a given value of Lambda.
  # This function will then be used to find the optimal lambda minimising the RMSE on that fold. 
  # To speed up the process, we extract the dataframe countaining the minimum and maximum rating for each user in 
  # the training set, user_pred_boundaries_training_set using the function get_userboundaries_trainingset. We then 
  # provide it as an input to get_pred_m_reg, as it will  be the same for each evaluation of get_pred_m_reg, thereby 
  # saving time in the minimizing process.
  worst_fold_best_method <- get_worst_fold_best_method(rmse_folds_results)
  user_pred_boundaries_training_set_worstfold <- get_userboundaries_trainingset(edx[-test_index_folds[[worst_fold_best_method]],][,c(1,3)])
  save_input_data()
  get_RMSE_worst_fold_m_reg_lm <- function(l) {
    # training_set <- edx[-test_index_folds[[worst_fold_best_method]],c(1,2,3)]
    # test_set <- edx[test_index_folds[[worst_fold_best_method]],c(1,2,3)]
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_m_reg(training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3)],
                                       test_set = edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3)],
                                       user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                                       l_m = l
                                       ))
    return(OutRMSE)
  }
  
  precision_min_RMSE <-  1e-06 
  #precision to which RMSE minimum is considered reached, chosen based on the target
  #RMSE having precision of 1e-5
  
  #The lambda minimising this function is found by applying the minimum finding function to our worst fold RMSE function.
  #Here we search between lambda = 0 and lambda = 10, with a precision on RMSE of precision_min_RMSE.
  #By setting the with_details parameter to TRUE, we can plot the minimum finding process.
  Details_Lambda_Min_RMSE_worst_fold_m <- Find_Get_Val_Local_Minimum_Golden(0,10,
                                                                            get_RMSE_worst_fold_m_reg_lm, 
                                                                            precision_min_RMSE,TRUE,TRUE) 
  #promise already under evaluation: recursive default argument reference or earlier problems? 
  #or
  # Error in Get_Progress_Check() : object 'X_Right' not found 
  #the number of function evaluation to find minimum with precision_min_RMSE precision
  nb_eval <- length(unique(Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord$X)) 
  print(nb_eval) 
  #6 evaluations only, proving the benefit of the method for finding minimum indeed the closest would have been 
  #2.4 which if evaluated between 0 and 10 by increments of 0.1 would have required a minimum of 25 evaluations (if
  #stopping at the point where the function starts increasing again) and at maximum 100 evaluations, to reach a similar
  #result with slightly less precision. 
  RMSE_movie_reg <- min(Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord$Val)

  #Systematic Lambda Search
  RMSE_movie_extensive_minimum_search <- data.frame()
  for (lambda in seq(0,10,0.1)) {
    RMSE_movie_extensive_minimum_search <- RMSE_movie_extensive_minimum_search %>%
      bind_rows(data.frame(Lambda = lambda, RMSE = get_RMSE_worst_fold_m_reg_lm(lambda)))
  }
  #Visualisations of systematic lambda search
  RMSE_movie_extensive_minimum_search %>%
    ggplot(aes(Lambda, RMSE)) +
    geom_line() #+ geom_point()
  
  Lambda_Min_RMSE_worst_fold_m_systematic <- RMSE_movie_extensive_minimum_search %>% 
    filter(RMSE == min(RMSE)) %>%
    pull(Lambda)
  
  RMSE_movie_systematic <- min(RMSE_movie_extensive_minimum_search$RMSE)
  
  Comp_SearchMethods <- Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord %>%
    select(X,Val) %>%
    rename(Lambda = X, RMSE = Val) %>%
    mutate(Category = "Golden_Section_Search") %>%
    bind_rows(data.frame(RMSE_movie_extensive_minimum_search,Category  = "Extensive_Search"))
  
  Comp_SearchMethods %>%
    ggplot(aes(Lambda, RMSE,colour = Category )) +
    geom_line(size = 1) + geom_point()
  
  ggplot(Comp_SearchMethods%>% filter(Category  == "Extensive_Search"), aes(Lambda, RMSE,colour = Category ,shape = Category )) + 
    geom_line() +
    geom_point(size = 1) +
    geom_point(data = Comp_SearchMethods%>% filter(Category  == "Golden_Section_Search"),size = 2,stroke = 2)+
    geom_point(data = Comp_SearchMethods%>% filter(Category  == "Golden_Section_Search") %>% filter(RMSE == min(RMSE)) %>% mutate(Category  = "Golden_Section_Minimum"),size = 2,stroke = 2)+
    geom_point(data = Comp_SearchMethods%>% filter(Category  == "Extensive_Search") %>% filter(RMSE == min(RMSE)) %>% mutate(Category  = "Extensive_Minimum"),size = 2,stroke = 2)
  
  #Visualisations of lambda search
  #search steps visualisations - using facets
  Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord %>%
    mutate(step = as.character(step)) %>%
    ggplot(aes(X, Val,group = step)) +
    geom_line(aes(colour = step)) +
    geom_point(alpha = 0.5) + facet_grid(step~.) +
    theme(legend.position = c(0.8, 0.2))
  
  #search steps visualisations - using dodged line plots
  Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord %>%
    mutate(step = as.character(step)) %>%
    ggplot(aes(X, Val)) +
    geom_line(#position = position_dodge(width = 0.5),
              size = 1,
              aes(colour = step)) +
    geom_point() +
    theme(legend.position = c(0.8, 0.2))
  
  #search steps visualisations - line and point plot
  Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord %>%
    ggplot(aes(X, Val)) +
    geom_point() +
    geom_line()
  
  #However, one must be careful that the appropriate interval search is chosen. Indeed if the search interval is increased
  #to the point that a minimum does not appear in the golden section search, then we might miss the minimum. Such 
  #a case happens in the previous search if using the interval [0,20].
  
  #To allow for that the search has been modified to allow further attempts between the first two location picked, as 
  #there might still be a minimum.By setting the max_Nb_Attempt parameter to a value greater than 1, the function will 
  #attempt to find a minimum between the the first two locations. 
  
  #one attempt only, minimum missed
  Details_Lambda_Min_RMSE_worst_fold_m_LargerIntervalMISSED <- Find_Get_Val_Local_Minimum_Golden(0,20,
                                                                                                 get_RMSE_worst_fold_m_reg_lm,
                                                                                                 precision_min_RMSE,TRUE,TRUE) 
  
  #2 attempts allowed, the minimum is found between the initial two first values
  Details_Lambda_Min_RMSE_worst_fold_m_LargerIntervalFOUND <- Find_Get_Val_Local_Minimum_Golden(0,20,
                                                                                                get_RMSE_worst_fold_m_reg_lm,
                                                                                                precision_min_RMSE,TRUE,TRUE,
                                                                                                max_Nb_Attempt = 2)
  
  LargerInterval_SearchRecordComparison <- bind_rows(
    Details_Lambda_Min_RMSE_worst_fold_m_LargerIntervalMISSED$SearchRecord %>% mutate(case = "max_Nb_Attempt =1, Minimum Missed"),
    Details_Lambda_Min_RMSE_worst_fold_m_LargerIntervalFOUND$SearchRecord %>% mutate(case = "max_Nb_Attempt =2, Minimum Found"))
  
  #Comparing the two cases
  p_LargerInterval_SearchRecordComparison <-  LargerInterval_SearchRecordComparison %>% 
    mutate(step = as.character(step)) %>%
    ggplot(aes(X, Val,group = step)) +
    geom_line(aes(colour = step)) +
    geom_point(alpha = 0.5) + facet_grid(step~case) +
    theme(legend.position = c(0.8, 0.2))
  
  print(p_LargerInterval_SearchRecordComparison)
  
  #minimizing lambda
  Lambda_Min_RMSE_worst_fold_m <- Details_Lambda_Min_RMSE_worst_fold_m$Output 
  print(Lambda_Min_RMSE_worst_fold_m) 
  #2.36068
  #minimum RMSE reached
  print(min(Details_Lambda_Min_RMSE_worst_fold_m$SearchRecord$Val)) 
  #0.94350736 which is worse than the movie + user model (0.866793)
  #Because this model is not as good as the movie + user model, it will not be added to the results data frame.
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  ### Model: Model: movie then user regularised ####
  
  #we predict that the rating for movie i, user u is t the sum of
  #the movie effect and the user effect as recalculated using the baseline
  #with movie effect included and applying regularisation
  
  #we allow for two different parameters lambda for the movie and user effect, so we can check
  #whether this makes a difference in the accuracy of the predictions
  get_pred_m_u_reg <- function(training_set,test_set,
                               user_pred_boundaries_training_set,
                               l_m,l_u) {
    if (missing(user_pred_boundaries_training_set)) {
      print("extracting user_pred_boundaries_training_set")
      user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    } else {
      print("using user_pred_boundaries_training_set provided as input")
    }
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie_reg <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i = sum(rating-mu)/(n()+l_m)) %>%
      ungroup() 
    
    print("user effect")
    effect_movie_user_reg <- training_set %>%
      left_join(effect_movie_reg,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating-mu-b_i)/(n()+l_u)) %>%
      ungroup() 
    
    print("prediction")
    pred_mean_movie_user_reg <- test_set %>%
      left_join(effect_movie_reg,by="movieId") %>%
      left_join(effect_movie_user_reg,by="userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>% 
      pull(pred)
    return(pred_mean_movie_user_reg)
  }
  
  #Function giving the RMSE of the worst fold for a given value of Lambda
  #First we consider the case with a unique lambda,  l_m = l_u = l 
  #For speed of execution, only the column containing the data to be used (movieId, userId and rating) is provided
  #As with the movie regularised model, we supply the user_pred_boundaries_training_set parameter for speed of execution
  get_RMSE_worst_fold_mu_reg<- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_m_u_reg(training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3)],
                                         test_set = edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3)],
                                         user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                                         l,l #l_m=l_u = l single lambda for both movie and user effects
                                         ))
    return(OutRMSE)
  }
  
  #The lambda minimising this function is found by applying the minimum finding function to our worst fold RMSE function
  #Here we search between lambda = 0 and lambda = 10, with a precision on RMSE of precision_min_RMSE
  #By setting the with_details parameter to TRUE, we can observe the minimum finding process
  Details_Lambda_Min_RMSE_worst_fold_mu <- Find_Get_Val_Local_Minimum_Golden(0,10,
                                                                             get_RMSE_worst_fold_mu_reg, 
                                                                             precision_min_RMSE,TRUE,TRUE) 
  
  Lambda_Min_RMSE_worst_fold_mu <- Details_Lambda_Min_RMSE_worst_fold_mu$Output #4.72136
  print(Lambda_Min_RMSE_worst_fold_mu) #4.72136
  print(min(Details_Lambda_Min_RMSE_worst_fold_mu$SearchRecord$Val)) 
  #0.8660161, it is an improvement on the movie + user model (0.8665076)
  
  #Minimum finding observations
  #number of function evaluation to find minimising lambda
  nb_eval <- length(unique(Details_Lambda_Min_RMSE_worst_fold_mu$SearchRecord$X)) 
  print(nb_eval) #8 evaluations only, compared to 48 evaluations minimum to get a result of 4.7 for lambda
  
  #Visualisations of lambda search
  #search steps visualisations - using facets
  Details_Lambda_Min_RMSE_worst_fold_mu$SearchRecord %>%
    mutate(step = as.character(step)) %>%
    ggplot(aes(X, Val,group = step)) +
    geom_line(aes(colour = step)) +
    geom_point(alpha = 0.5) + facet_grid(step~.) +
    theme(legend.position = c(0.8, 0.2))
  
  #search steps visualisations - using dodged line plots
  Details_Lambda_Min_RMSE_worst_fold_mu$SearchRecord %>%
    mutate(step = as.character(step)) %>%
    ggplot(aes(X, Val)) +
    geom_line(#position = position_dodge(width = 0.5),
      size = 1,
      aes(colour = step)) +
    geom_point() +
    theme(legend.position = c(0.8, 0.2))
  
  #search steps visualisations - line and point plot
  Details_Lambda_Min_RMSE_worst_fold_mu$SearchRecord %>%
    ggplot(aes(X, Val)) +
    geom_point() +
    geom_line()
  
  # We attempt applying different lambdas for movie and user effects
  # we set the lambda for movie to be the one determined for the movie only regularised model
  # then we keep the lambda for user effect variable, obtaining a monovariate function to 
  # minimise
  get_RMSE_worst_fold_m_u_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_m_u_reg(training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3)],
                                         test_set = edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3)],
                                         user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                                         l_m = Lambda_Min_RMSE_worst_fold_m,
                                         l))
    return(OutRMSE)
  }
  
  Details_Lambda_Min_RMSE_worst_fold_m_u <- Find_Get_Val_Local_Minimum_Golden(0,10,
                                                                              get_RMSE_worst_fold_m_u_reg,
                                                                              precision_min_RMSE,TRUE,TRUE) 
  
  Lambda_Min_RMSE_worst_fold_m_u <- Details_Lambda_Min_RMSE_worst_fold_m_u$Output 
  print(Lambda_Min_RMSE_worst_fold_m_u) #4.72136 
  #same lambda for user effect as unique lambda for movie+user
  print(min(Details_Lambda_Min_RMSE_worst_fold_m_u$SearchRecord$Val)) 
  #0.8660269 RMSE for the multiple lambda is slightly worse 
  #therefore for the movie+user model regularised we will keep the single lambda version. 
  
  #saving the information on comparing single vs lambda for regularised models
  SingleVsMultipleLambdaComp <- data.frame(method= "movie+user",
                                           SingleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_mu$SearchRecord$Val),
                                           MultipleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_m_u$SearchRecord$Val)
                                           )
  #prediction function, using the minimising lambda (Lambda_Min_RMSE_worst_fold_reg_mu) previously obtained
  #and keeping user_pred_boundaries_training_set unset so it is re-extracted for each training_set
  get_pred_mu_reg_trained <- function(training_set, test_set) {
    return(get_pred_m_u_reg(training_set = training_set[,c(1,2,3)],
                            test_set = test_set[,c(1,2,3)],
                            l_m = Lambda_Min_RMSE_worst_fold_mu,
                            l_u = Lambda_Min_RMSE_worst_fold_mu))
  }
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mu_reg
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user reg",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_mu_reg_trained))))
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  ### Model: movie then user then genres regularised #### 
  
  #we predict that the rating for movie i, user u is t the sum of
  #the following effects in that order movie effect, user effect and 
  #movie genres effect, each applied on the previous baseline 
  #However the effect from genres is applied not to the overall average of users
  #but for each individual user. This approach is justified as genres preference 
  #is likely to be stronger for individual user, rather than for the overall users, 
  #so the effect should be greater and therefore greater accuracy reached
  #however the genres can be a quite specific combination that would give raise 
  #to issues of generalisation - making regularisation a necessity to see
  #a valuable improvement.
  #in order to speed up the search for the minimising lambda, we set a minimum number
  #of ratings and a minimum number of movies for a given genre, which will reduce the 
  #size of the join (at the expense of overall RMSE reduction which is fine at the lambda
  #determination stage)
  
  get_pred_m_u_g_reg <- function(training_set,test_set,
                                 user_pred_boundaries_training_set=NA,
                                 l_m,l_u,l_g,
                                 min_nb_movies=1,min_nb_ratings=0) {
    
    if (missing(user_pred_boundaries_training_set)) {
      print("extracting user_pred_boundaries_training_set")
      user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    } else {
      print("using user_pred_boundaries_training_set provided as input")
    }
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i =sum(rating-mu)/(n()+l_m)) %>%
      ungroup() 
    
    print("user effect")
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating-mu-b_i)/(n()+l_u)) %>%
      ungroup() 
    
    print("genres per user effect")
    effect_movie_user_genres_peruser <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      group_by(genres) %>%
      filter(n_distinct(movieId)>min_nb_movies) %>%
      filter(n() > min_nb_ratings) %>%
      ungroup() %>%
      group_by(userId,genres) %>%
      summarise(b_g = sum(rating-mu-b_i-b_u)/(n()+l_g)) %>%
      ungroup() 
    
    print("prediction")
    pred_mean_movie_user_genres_peruser_reg <- test_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      left_join(effect_movie_user_genres_peruser, by=c("userId","genres")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_g) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>%
      pull(pred)
    
    return(pred_mean_movie_user_genres_peruser_reg)
  }
  
  #single lambda applied to movie user and genres
  get_RMSE_worst_fold_mug_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_m_u_g_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3,6)],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3,6)],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = l,
                          l_u = l,
                          l_g = l,
                          min_nb_movies = 300, 
                          min_nb_ratings = 1))
    return(OutRMSE)
  }
  Details_Lambda_Min_RMSE_worst_fold_mug <- Find_Get_Val_Local_Minimum_Golden(0,20,get_RMSE_worst_fold_mug_reg ,1e-06,TRUE,TRUE)  
  
  print(length(unique(Details_Lambda_Min_RMSE_worst_fold_mug$SearchRecord$X))) #11 evaluations to find minimum
  
  Lambda_Min_RMSE_worst_fold_mug <- Details_Lambda_Min_RMSE_worst_fold_mug$Output
  print(Lambda_Min_RMSE_worst_fold_mug) #8.06045
  
  #the trained function is get_RMSE_worst_fold_m_u_g_reg with the lambda as obtained by minimizing the RMSE on the worst fold
  #and keeping user_pred_boundaries_training_set unset so it is re-extracted for each training_set
  #and decreasing min_nb_movies to defaults 1 for best precision (default values)
  get_pred_mug_reg_trained <- function(training_set,test_set) {
    get_pred_m_u_g_reg(training_set = training_set,
                       test_set = test_set,
                       l_m = Lambda_Min_RMSE_worst_fold_mug,
                       l_u = Lambda_Min_RMSE_worst_fold_mug,
                       l_g = Lambda_Min_RMSE_worst_fold_mug
    )
  }
  
  # RMSE on worst fold
  RMSE_mug_reg_SingleLambda <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_mug_reg_trained,
                       verbose = TRUE)
  #RMSE = 0.857317830930791 for worst fold =  Fold01
  #this is a clear improvement from the non regularised case (0.9200688) and better than best model so far, 
  # movie + user regularised (0.8660161)
  
  #separate lambdas: we attempt again to use previous results and apply the new effect as its separate lambda.
  #as the single lambda movie + user gave the best results, we keep that part the same, leaving l_g (lambda for genre effect)
  #as the single variable for the RMSE function to minimise.
  get_RMSE_worst_fold_mu_g_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_m_u_g_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3,6)],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3,6)],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = Lambda_Min_RMSE_worst_fold_mu,
                          l_u = Lambda_Min_RMSE_worst_fold_mu,
                          l_g = l,
                          min_nb_movies = 300,
                          min_nb_ratings = 0))
    return(OutRMSE)
  }
  Details_Lambda_Min_RMSE_worst_fold_mu_g <- Find_Get_Val_Local_Minimum_Golden(0,20,get_RMSE_worst_fold_mu_g_reg ,1e-06,TRUE,TRUE)  
  Lambda_Min_RMSE_worst_fold_mu_g <- Details_Lambda_Min_RMSE_worst_fold_mu_g$Output
  print(Lambda_Min_RMSE_worst_fold_mu_g) #12.36068 
  
  #the trained function is get_RMSE_worst_fold_m_u_g_reg with the lambda as obtained by minimizing the RMSE on the worst fold
  #keeping l_m and l_u equal to the value found for the movie+user regularised model and l_g the minimising lambda 
  #Lambda_Min_RMSE_worst_fold_mu_g
  #and decreasing min_nb_movies to defaults 1 for best precision
  get_pred_mu_g_reg_trained <- function(training_set,test_set) {
    get_pred_m_u_g_reg(training_set = training_set,
                       test_set = test_set,
                       l_m = Lambda_Min_RMSE_worst_fold_mu,
                       l_u = Lambda_Min_RMSE_worst_fold_mu,
                       l_g = Lambda_Min_RMSE_worst_fold_mu_g
    )
  }
  
  # RMSE on worst fold on trained model - multiple lambdas
  RMSE_mu_g_reg_MultipleLambda <- check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_mu_g_reg_trained,
                       verbose = TRUE)
  #RMSE = 0.857696501332263 for worst fold =  Fold01
  #this is worse than single lambda value (0.8573178) so we will keep the model with a single lambda.
  
  #saving the information on comparing single vs lambda for regularised models
  SingleVsMultipleLambdaComp <- SingleVsMultipleLambdaComp  %>% 
    bind_rows(
      data.frame(method= "movie+user+genres",
                 SingleLambdaRMSE = RMSE_mug_reg_SingleLambda,
                 MultipleLambdaRMSE = RMSE_mu_g_reg_MultipleLambda
                 )
      )

  #adding the results by applying get_RMSE_folds with get_pred_func = get_RMSE_mug_reg_trained
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user+genres reg",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_mug_reg_trained)))) 
  
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save

  ### Model: movie then user then individual genres ####
  
  #we now consider the effect of individual genres
  get_pred_mu_indgenres <- function(training_set,test_set) {
    
    print("extracting user_pred_boundaries_training_set")
    user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie <- training_set %>%
      select(movieId,rating) %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    
    print("user effect")
    effect_movie_user <- training_set %>%
      select(movieId,userId,rating) %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu-b_i)) %>%
      ungroup() 
    
    indgenres_relevant_training <- indgenres_relevant %>% 
      filter(movieId %in% training_set$movieId)
    
    print("invidual genre effect")
    effect_movie_user_indgenres <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      right_join(indgenres_relevant_training) %>%
      group_by(userId,indgenres)  %>%
      summarise(b_indg = mean(rating-mu-b_i-b_u))  %>%
      ungroup() 
    
    print("prediction")
    pred_mean_movie_user_indgenres <- test_set %>%
      select(movieId,userId,rating,genres) %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      #get the indgenres for the test set data
      mutate(indgenres = strsplit(genres, "\\|")) %>% #split string into a list
      unnest(indgenres) %>% #one line per genre
      left_join(effect_movie_user_indgenres,by=c("userId","indgenres")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      group_by(userId,genres) %>%
      mutate(b_g = mean(b_indg)) %>% #the effect of a genre is the average of the individual genres effects
      ungroup() %>%
      select(-c(indgenres,b_indg)) %>%
      unique() %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_g) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>%
      pull(pred)
    
    return(pred_mean_movie_user_indgenres)
  }
  
  # RMSE on worst fold
  check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_mu_indgenres,
                       verbose = TRUE)
  #RMSE = 0.851451518686382 for worst fold =  Fold01
  #this is a non regularised model, performing better than movie + users + genres regularised (0.8573178)
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mu_indgenres
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user+indgenres",
                        as.data.frame(get_RMSE_folds(test_index_folds,
                                                     edx,
                                                     get_pred_mu_indgenres)))) 
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  ### Model: movie then user then indgenres for all users then indgenres for ind users ####
  #we now consider the effect of individual genres but instead of applying the effect directly for each user, 
  #we first consider the effect of individual genres on the whole user population, the assumption to test being that 
  #for genres that have not been rated by an individual user, the "all user" individual genre effect would give a better
  #precision than defaulting to no effect at all. 
  get_pred_mu_indgenres_inc <- function(training_set,test_set) {
    
    print("extracting user_pred_boundaries_training_set")
    user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie <- training_set %>%
      select(movieId,rating) %>%
      group_by(movieId) %>%
      summarise(b_i = mean(rating-mu)) %>%
      ungroup() 
    
    print("user effect")
    effect_movie_user <- training_set %>%
      select(movieId,userId,rating) %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = mean(rating-mu-b_i)) %>%
      ungroup() 
    
    indgenres_relevant_training <- indgenres_relevant %>% 
      filter(movieId %in% training_set$movieId)
    
    print("invidual genre effect for all users")
    effect_movie_user_indgenres_allusers <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      right_join(indgenres_relevant_training) %>%
      group_by(indgenres)  %>%
      summarise(b_indg_au = mean(rating-mu-b_i-b_u)) %>%
      ungroup() 
    
    print("invidual genre effect individual users")
    effect_movie_user_indgenres_indusers <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      right_join(indgenres_relevant_training) %>%
      left_join(effect_movie_user_indgenres_allusers, by="indgenres") %>%
      group_by(userId,indgenres)  %>%
      summarise(b_indg = mean(rating-mu-b_i-b_u-b_indg_au)) %>%
      ungroup() 
    
    print("prediction")
    # indgenres_relevant_test <- indgenres_relevant %>% 
    #   filter(movieId %in% test_set$movieId)
    pred_mean_movie_user_indgenres <- test_set %>%
      select(movieId,userId,rating,genres) %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      #get the indgenres for the test set data
      mutate(indgenres = strsplit(genres, "\\|")) %>% #split string into a list
      unnest(indgenres) %>% #one line per genre
      left_join(effect_movie_user_indgenres_allusers,by="indgenres") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      group_by(genres) %>%
      mutate(b_g_au = mean(b_indg_au)) %>%
      ungroup() %>%
      left_join(effect_movie_user_indgenres_indusers,by=c("userId","indgenres")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      group_by(userId,genres) %>%
      mutate(b_g = mean(b_indg)) %>%
      ungroup() %>%
      select(-c(indgenres,b_indg,b_indg_au)) %>%
      unique() %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_g_au+b_g) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) 
    
    pred_mean_movie_user_indgenres <- pred_mean_movie_user_indgenres %>%
      pull(pred)
    
    return(pred_mean_movie_user_indgenres)
  }
  
  # RMSE on worst fold 
  check_RMSE_worstfold(rmse_folds_results = rmse_folds_results ,
                       test_index_folds = test_index_folds,
                       movielens_data = edx,
                       get_pred_func = get_pred_mu_indgenres_inc,
                       verbose = TRUE)
  #RMSE = 0.851455312306009 for worst fold =  Fold01
  #this is slightly worse than the simple movie +user + individual genres (0.851451518686382)
  # therefore we will not be adding the results to the results dataframe.

  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  
  ### Model: movie+user+indgenres reg  ####
  
  #we predict that the rating for movie i, user u is t the sum of
  #the following effects in that order movie effect, user effect and 
  #individual movie genres effect, each applied on the previous baseline,
  #with regularisation.
  #this requires wrangling of the movies genre from single field
  
  get_pred_muig_reg <- function(training_set,test_set,
                                user_pred_boundaries_training_set,
                                l_m,l_u,l_g
                                ) {
    
    if (missing(user_pred_boundaries_training_set)) {
      print("extracting user_pred_boundaries_training_set")
      user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    } else {
      print("using user_pred_boundaries_training_set provided as input")
    }
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie <- training_set %>%
      select(movieId,rating) %>%
      group_by(movieId) %>%
      summarise(b_i = sum(rating-mu)/(n()+l_m)) %>%
      ungroup() 
    
    print("user effect")
    effect_movie_user <- training_set %>%
      select(movieId,userId,rating) %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating-mu-b_i)/(n()+l_u)) %>%
      ungroup() 
    
    indgenres_relevant_training <- indgenres_relevant %>% 
      filter(movieId %in% training_set$movieId)
    
    print("invidual genre effect")
    effect_movie_user_indgenres <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      right_join(indgenres_relevant_training) %>%
      group_by(userId,indgenres)  %>%
      summarise(b_indg = sum(rating-mu-b_i-b_u)/(n()+l_g)) %>%
      ungroup()  
    
    print("prediction")
    # indgenres_relevant_test <- indgenres_relevant %>% 
    #   filter(movieId %in% test_set$movieId)
    pred_mean_movie_user_indgenres_reg <- test_set %>%
      select(movieId,userId,rating,genres) %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      #get the indgenres for the test set data
      mutate(indgenres = strsplit(genres, "\\|")) %>% #split string into a list
      unnest(indgenres) %>% #one line per genre
      left_join(effect_movie_user_indgenres,by=c("userId","indgenres")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      group_by(userId,genres) %>%
      mutate(b_g = mean(b_indg)) %>%
      ungroup() %>%
      select(-c(indgenres,b_indg)) %>%
      unique() %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_g) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>%
      pull(pred)
    
    return(pred_mean_movie_user_indgenres_reg)
  }
  
  #single lambda applied to movie user and genres
  get_RMSE_worst_fold_muig_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_muig_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3,6)],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3,6)],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = l,
                          l_u = l,
                          l_g = l))
    return(OutRMSE)
  }
  Details_Lambda_Min_RMSE_worst_fold_muig <- Find_Get_Val_Local_Minimum_Golden(0,10,get_RMSE_worst_fold_muig_reg ,1e-06,TRUE,TRUE)  
  
  print(length(unique(Details_Lambda_Min_RMSE_worst_fold_muig$SearchRecord$X))) #11 evaluations to find minimum
  
  Lambda_Min_RMSE_worst_fold_muig <- Details_Lambda_Min_RMSE_worst_fold_muig$Output
  print(Lambda_Min_RMSE_worst_fold_muig) #2.229124
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  #multiple lambdas - we attempt getting to a lower RMSE by using minimising lambda
  #obtained for simpler model (movie + user) and varying only the lambda for the
  #additional effect (individual genre)
  #automatic search for lambda
  get_RMSE_worst_fold_mu_ig_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_muig_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3,6)],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3,6)],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = Lambda_Min_RMSE_worst_fold_mu,
                          l_u = Lambda_Min_RMSE_worst_fold_mu,
                          l_g = l))
    return(OutRMSE)
  }
  
  Details_Lambda_Min_RMSE_worst_fold_mu_ig <- Find_Get_Val_Local_Minimum_Golden(0,5,get_RMSE_worst_fold_mu_ig_reg ,1e-06,TRUE,TRUE)  
  
  print(length(unique(Details_Lambda_Min_RMSE_worst_fold_mu_ig$SearchRecord$X))) #8 evaluations to find minimum
  
  Lambda_Min_RMSE_worst_fold_mu_ig <- Details_Lambda_Min_RMSE_worst_fold_mu_ig$Output
  print(Lambda_Min_RMSE_worst_fold_mu_ig) #2.082039
  
  #check which lambda choice is better (i.e. allowing for lower RMSE)
  print(min(Details_Lambda_Min_RMSE_worst_fold_muig$SearchRecord$Val)) #0.8496666 for single lambda
  print(min(Details_Lambda_Min_RMSE_worst_fold_mu_ig$SearchRecord$Val)) #0.8495601 for multiple lambdas
  #results are slightly better using a different lambda for individual genre effect, 
  #so we use this model will be kept and added to the results
  
  #saving the information on comparing single vs lambda for regularised models
  SingleVsMultipleLambdaComp <- SingleVsMultipleLambdaComp  %>% 
    bind_rows(
      data.frame(method= "movie+user+indgenres",
                 SingleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_muig$SearchRecord$Val),
                 MultipleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_mu_ig$SearchRecord$Val)
      )
    )
  
  #the trained function is get_pred_muig_reg with the lambdas as obtained by minimizing the RMSE on the worst fold
  #and keeping user_pred_boundaries_training_set unset so it is re-extracted for each training_set 
  get_pred_muig_reg_trained <- function(tr,te) {
    get_pred_muig_reg(training_set = tr,
                      test_set = te,
                      l_m = Lambda_Min_RMSE_worst_fold_mu,
                      l_u = Lambda_Min_RMSE_worst_fold_mu,
                      l_g = Lambda_Min_RMSE_worst_fold_mu_ig 
                      )
  }
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mean_movie_user_indgenres
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user+indgenres reg",
                        as.data.frame(get_RMSE_folds(test_index_folds = test_index_folds,
                                                     movielens_data = edx[,c(1,2,3,6)],
                                                     get_pred_func = get_pred_muig_reg_trained
                                                     ))))
 
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  ### Model: movie+user+indgenres+releaseyear reg  ####
  
  #we predict that the rating for movie i, user u is t the sum of
  #the following effects in that order movie effect, user effect and 
  #movie genres effect and release year effect, each applied on the previous baseline
  get_pred_muigy_reg <- function(training_set,test_set,
                                  user_pred_boundaries_training_set,
                                  l_m,l_u,l_g,l_y) {
    
    if (missing(user_pred_boundaries_training_set)) {
      print("extracting user_pred_boundaries_training_set")
      user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    } else {
      print("using user_pred_boundaries_training_set provided as input")
    }
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i =sum(rating-mu)/(n()+l_m)) %>%
      ungroup() 
    
    print("user effect")
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating-mu-b_i)/(n()+l_u)) %>%
      ungroup() 
    
    indgenres_relevant_training <- indgenres_relevant %>% 
      filter(movieId %in% training_set$movieId)
    
    print("invidual genre effect")
    effect_movie_user_indgenres <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      right_join(indgenres_relevant_training) %>%
      group_by(userId,indgenres)  %>%
      mutate(b_indg = sum(rating-mu-b_i-b_u)/(n()+l_g)) %>%
      select(userId, indgenres,genres,b_indg) %>%
      ungroup() 
    
    print("genre effect")
    effect_movie_user_genres <- effect_movie_user_indgenres %>%
      group_by(userId,genres) %>%
      summarise(b_g = mean(b_indg)) %>%
      ungroup()  
    
    print("invidual genre effect (cont)")
    effect_movie_user_indgenres <- effect_movie_user_indgenres %>%
      select(-genres) %>%
      unique()
    
    print("release year per user effect")
    effect_movie_user_releaseyear_peruser <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      left_join(effect_movie_user_genres,by=c("userId","genres")) %>%
      left_join(release_years, by = "movieId") %>%
      group_by(userId,release_year) %>%
      summarise(b_y = sum(rating-mu-b_i-b_u-b_g)/(n()+l_y)) %>%
      ungroup() 
    
    
    print("prediction")
    # indgenres_relevant_test <- indgenres_relevant %>% 
    #   filter(movieId %in% test_set$movieId)
    pred_mean_movie_user_year_peruser_reg <- test_set %>%
      select(movieId,userId,rating,genres) %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      #get the indgenres for the test set data
      mutate(indgenres = strsplit(genres, "\\|")) %>% #split string into a list
      unnest(indgenres) %>% #one line per genre
      left_join(effect_movie_user_indgenres,by=c("userId","indgenres")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      group_by(userId,genres) %>%
      mutate(b_g = mean(b_indg)) %>%
      ungroup() %>%
      select(-c(indgenres,b_indg)) %>%
      unique() %>%
      left_join(release_years, by = "movieId") %>%
      left_join(effect_movie_user_releaseyear_peruser, by=c("userId","release_year")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_g+b_y) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>%
      pull(pred)
    
    
    return(pred_mean_movie_user_year_peruser_reg)
  }

  #single lambda applied to movie user and genres
  get_RMSE_worst_fold_muigy_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_muigy_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3,5,6)],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3,5,6)],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = l,
                          l_u = l,
                          l_y = l,
                          l_g = l
                          ))
    return(OutRMSE)
  }
  Details_Lambda_Min_RMSE_worst_fold_muigy <- Find_Get_Val_Local_Minimum_Golden(0,20,get_RMSE_worst_fold_muigy_reg ,1e-06,TRUE,TRUE)  
  Lambda_Min_RMSE_worst_fold_muigy   <- Details_Lambda_Min_RMSE_worst_fold_muigy$Output
  print(Lambda_Min_RMSE_worst_fold_muigy) #12.09757
  
  #attempt to separate lambda for release year using lambdas previously obtained for movie+users and indgenres
  get_RMSE_worst_fold_peruser_reg_mu_ig_y <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_muigy_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],][,c(1,2,3,5,6)],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]][,c(1,2,3,5,6)],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = Lambda_Min_RMSE_worst_fold_mu,
                          l_u = Lambda_Min_RMSE_worst_fold_mu,
                          l_g = Lambda_Min_RMSE_worst_fold_mu_ig,
                          l_y =l
                        ))
    return(OutRMSE)
  }
  
  
  Details_Lambda_Min_RMSE_worst_fold_mu_ig_y <- Find_Get_Val_Local_Minimum_Golden(20,
                                                                                  45,
                                                                                  get_RMSE_worst_fold_peruser_reg_mu_ig_y,
                                                                                  precision_min_RMSE, TRUE, TRUE)  
  Lambda_Min_RMSE_worst_fold_mu_ig_y  <- Details_Lambda_Min_RMSE_worst_fold_mu_ig_y$Output
  print(Lambda_Min_RMSE_worst_fold_mu_ig_y) #28.15595

  #check which better
  min(Details_Lambda_Min_RMSE_worst_fold_muigy$SearchRecord$Val) #0.8495612
  min(Details_Lambda_Min_RMSE_worst_fold_mu_ig_y$SearchRecord$Val) #0.8464639
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  #for this model having separate lambdas gives lower RMSE so the model with different lambda is added
  #to the results
  
  #saving the information on comparing single vs lambda for regularised models
  SingleVsMultipleLambdaComp <- SingleVsMultipleLambdaComp  %>% 
    bind_rows(
      data.frame(method= "movie+user+indgenres+releaseyear",
                 SingleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_muigy$SearchRecord$Val),
                 MultipleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_mu_ig_y$SearchRecord$Val)
      )
    )
  
  #the trained function is get_pred_muigy_reg with the multiple lambdas obtained by minimizing the RMSE 
  #on the worst fold and keeping user_pred_boundaries_training_set unset so it is re-extracted for each training_set
  get_pred_mu_ig_y_reg_trained <- function(training_set,test_set) 
  {
    get_pred_muigy_reg(
      training_set = training_set,
      test_set = test_set,
      l_m = Lambda_Min_RMSE_worst_fold_mu,
      l_u = Lambda_Min_RMSE_worst_fold_mu,
      l_g = Lambda_Min_RMSE_worst_fold_mu_ig,
      l_y = Lambda_Min_RMSE_worst_fold_mu_ig_y
    )
  }
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mu_ig_y_reg_trained
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user+indgenres+releaseyear reg",
                        as.data.frame(get_RMSE_folds(test_index_folds = test_index_folds,
                                                     movielens_data = edx,
                                                     get_pred_func = get_pred_mu_ig_y_reg_trained
                                                     ))))    

  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  ### Model: movie+user+indgenres+releaseyear reg+timestamp ####
  
  #we predict that the rating for movie i, user u is t the sum of
  #the following effects in that order movie effect, user effect and 
  #date of rating effect
  
  get_pred_muigyt_reg <- function(training_set,test_set,
                                  user_pred_boundaries_training_set,
                                  l_m,l_u,l_g,l_y,l_t) {
    
    if (missing(user_pred_boundaries_training_set)) {
      print("extracting user_pred_boundaries_training_set")
      user_pred_boundaries_training_set <- get_userboundaries_trainingset(training_set)
    } else {
      print("using user_pred_boundaries_training_set provided as input")
    }
    
    print("mu")
    mu <- mean(training_set$rating)
    
    print("movie effect")
    effect_movie <- training_set %>%
      group_by(movieId) %>%
      summarise(b_i =sum(rating-mu)/(n()+l_m)) %>% 
      ungroup()
    
    print("user effect")
    effect_movie_user <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      group_by(userId) %>%
      summarise(b_u = sum(rating-mu-b_i)/(n()+l_u)) %>% 
      ungroup()
    
    indgenres_relevant_training <- indgenres_relevant %>% 
      filter(movieId %in% training_set$movieId)
    
    print("invidual genre effect")
    effect_movie_user_indgenres <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      right_join(indgenres_relevant_training) %>%
      group_by(userId,indgenres)  %>%
      mutate(b_indg = sum(rating-mu-b_i-b_u)/(n()+l_g)) %>%
      select(userId, indgenres,genres,b_indg) %>% 
      ungroup()
    
    print("genre effect")
    effect_movie_user_genres <- effect_movie_user_indgenres %>%
      group_by(userId,genres) %>%
      summarise(b_g = mean(b_indg)) %>% 
      ungroup()
    
    print("invidual genre effect (cont)")
    effect_movie_user_indgenres <- effect_movie_user_indgenres %>%
      select(-genres) %>%
      unique()
    
    print("release year per user effect")
    effect_movie_user_releaseyear_peruser <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      left_join(effect_movie_user_genres,by=c("userId","genres")) %>%
      left_join(release_years, by = "movieId") %>%
      group_by(userId,release_year) %>%
      summarise(b_y = sum(rating-mu-b_i-b_u-b_g)/(n()+l_y)) %>% 
      ungroup()
    
    print("date per user effect")
    effect_movie_user_week_peruser <- training_set %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      left_join(release_years, by = "movieId") %>%
      left_join(effect_movie_user_releaseyear_peruser, by=c("userId","release_year")) %>%
      left_join(timestamp_day_rating,by = "timestamp") %>%
      group_by(userId,time_rating) %>%
      summarise(b_t = sum(rating-mu-b_i-b_u)/(n()+l_t)) %>%
      ungroup()
    
    
    print("prediction")
    # indgenres_relevant_test <- indgenres_relevant %>% 
    #   filter(movieId %in% test_set$movieId)
    pred_mean_movie_user_time_peruser_reg <- test_set %>%
      select(movieId,userId,rating,genres,timestamp) %>%
      left_join(effect_movie,by="movieId") %>%
      left_join(effect_movie_user,by="userId") %>%
      #get the indgenres for the test set data
      mutate(indgenres = strsplit(genres, "\\|")) %>% #split string into a list
      unnest(indgenres) %>% #one line per genre
      left_join(effect_movie_user_indgenres,by=c("userId","indgenres")) %>% 
      group_by(userId,genres) %>%
      mutate(b_g = mean(b_indg)) %>%
      ungroup() %>%
      select(-c(indgenres,b_indg)) %>%
      unique() %>%
      left_join(release_years, by = "movieId") %>%
      left_join(effect_movie_user_releaseyear_peruser, by=c("userId","release_year")) %>%
      left_join(timestamp_day_rating,by = "timestamp") %>%
      left_join(effect_movie_user_week_peruser,by=c("userId","time_rating")) %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = mu+b_i+b_u+b_t+b_g+b_y) %>%
      mutate(pred = if_else(pred < 0.5,0.5,pred)) %>%
      mutate(pred = if_else(pred > 5,5,pred)) %>%
      left_join(user_pred_boundaries_training_set,by = "userId") %>%
      mutate_all(~replace(., is.na(.), 0)) %>%
      mutate(pred = case_when(pred < minimum_rating  ~ minimum_rating,
                              pred > maximum_rating  ~ maximum_rating,
                              TRUE ~ pred)) %>%
      pull(pred)
    
    
    return(pred_mean_movie_user_time_peruser_reg)
  }
  
  #single lambda applied to movie user and genres
  get_RMSE_worst_fold_muigyt_reg <- function(l) {
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_muigyt_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = l,
                          l_u = l,
                          l_g = l,
                          l_y = l,
                          l_t = l
                        ))
    return(OutRMSE)
  }
  Details_Lambda_Min_RMSE_worst_fold_muigyt <- Find_Get_Val_Local_Minimum_Golden(0,45,get_RMSE_worst_fold_muigyt_reg ,1e-06,TRUE,TRUE,3)  
  Lambda_Min_RMSE_worst_fold_muigyt   <- Details_Lambda_Min_RMSE_worst_fold_muigyt$Output
  print(Lambda_Min_RMSE_worst_fold_muigyt) #14.45458

  #get separate lambda for time stamp
  get_RMSE_worst_fold_peruser_reg_mu_ig_y_t <- function(l) {
    training_set <- edx[-test_index_folds[[worst_fold_best_method]],c(1,2,3,4,5,6)]
    test_set <- edx[test_index_folds[[worst_fold_best_method]],c(1,2,3,4,5,6)]
    OutRMSE <- get_RMSE(edx[test_index_folds[[worst_fold_best_method]]]$rating,
                        get_pred_muigyt_reg(
                          training_set = edx[-test_index_folds[[worst_fold_best_method]],],
                          test_set= edx[test_index_folds[[worst_fold_best_method]]],
                          user_pred_boundaries_training_set = user_pred_boundaries_training_set_worstfold,
                          l_m = Lambda_Min_RMSE_worst_fold_mu,
                          l_u = Lambda_Min_RMSE_worst_fold_mu,
                          l_g = Lambda_Min_RMSE_worst_fold_mu_ig,
                          l_y = Lambda_Min_RMSE_worst_fold_mu_ig_y,
                          l_t = l
                        ))
    return(OutRMSE)
  }
  
  
  Details_Lambda_Min_RMSE_worst_fold_mu_ig_y_t <- Find_Get_Val_Local_Minimum_Golden(0,
                                                                                    45,
                                                                                    get_RMSE_worst_fold_peruser_reg_mu_ig_y_t,
                                                                                    precision_min_RMSE, TRUE, TRUE)  
  Lambda_Min_RMSE_worst_fold_mu_ig_y_t  <- Details_Lambda_Min_RMSE_worst_fold_mu_ig_y_t$Output
  print(Lambda_Min_RMSE_worst_fold_mu_ig_y_t) #15.63859
  
  #comparison single and multiple lambda
  min(Details_Lambda_Min_RMSE_worst_fold_muigyt$SearchRecord$Val) #0.8437321
  min(Details_Lambda_Min_RMSE_worst_fold_mu_ig_y_t$SearchRecord$Val) #0.8402347
  #results are better using multiple lambdas so this will be kept and added to the results
  
  #saving the information on comparing single vs lambda for regularised models
  SingleVsMultipleLambdaComp <- SingleVsMultipleLambdaComp  %>% 
    bind_rows(
      data.frame(method= "movie+user+indgenres+releaseyear+timestamp",
                 SingleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_muigyt$SearchRecord$Val),
                 MultipleLambdaRMSE = min(Details_Lambda_Min_RMSE_worst_fold_mu_ig_y_t$SearchRecord$Val)
      )
    )
  
  #the trained function is get_pred_muigyt_reg with the multiple lambdas obtained by minimizing the RMSE 
  #on the worst fold and keeping user_pred_boundaries_training_set unset so it is re-extracted for each training_set
  get_pred_mu_ig_y_t_reg_trained <- function(training_set,test_set) 
  {
    get_pred_muigyt_reg(
      training_set = training_set,
      test_set= test_set,
      l_m = Lambda_Min_RMSE_worst_fold_mu,
      l_u = Lambda_Min_RMSE_worst_fold_mu,
      l_g = Lambda_Min_RMSE_worst_fold_mu_ig,
      l_y = Lambda_Min_RMSE_worst_fold_mu_ig_y,
      l_t = Lambda_Min_RMSE_worst_fold_mu_ig_y_t
    )
  }
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  
  #adding the results by applying get_RMSE_folds with get_pred_func = get_pred_mu_ig_y_t_reg_trained
  rmse_folds_results <- rmse_folds_results %>%  
    bind_rows(bind_cols(do_remove_sparse = do_remove_sparse,
                        method="movie+user+indgenres+releaseyear+timestamp reg",
                        as.data.frame(get_RMSE_folds(test_index_folds = test_index_folds,
                                                     movielens_data = edx,
                                                     get_pred_func = get_pred_mu_ig_y_t_reg_trained
                                                     )))) 
  
  savecount <- save_output_data(savenum = savecount, save_output = !load_output_data) #incremental save
  run_output_data_end_time <- Sys.time()
  run_output_data_duration <- run_output_data_end_time - run_output_data_start_time 
  print(run_output_data_duration) #Time difference of 5.718817 hours
}

# SUMMARY OF RESULTS (visualisations of models RMSE performance) ####
if (file.exists("Data/Output_021.RData")) {
  load("Data/Output_021.RData")
  savecount <- savecount + 1
} else {
  #tidy version of the rmse folds results dataframe, ready for plotting
  rmse_folds_tidy <- rmse_folds_results %>%
    gather("Folds","RMSE",-c(method,do_remove_sparse)) %>%
    group_by(method) %>%
    arrange(desc(RMSE)) %>%
    ungroup()
  
  #Summary table for all models
  rmse_folds_results_summary <- rmse_folds_tidy %>%
    group_by(method,do_remove_sparse) %>%
    summarise(mean_RMSE = mean(RMSE),
              median_RMSE = median(RMSE),
              max_RMSE = max(RMSE),
              sd_RMSE = sd(RMSE)) %>%
    arrange(desc(median_RMSE))
  
  #Target RMSEs for grading, to be drawn as horizontal lines on RMSE plots
  rmse_target_25 <- 0.86490
  rmse_target_20 <- 0.86500
  rmse_target_15 <- 0.86550
  rmse_target_10 <- 0.90000
  
  #Plot of all models as boxplot 
  p_rmse_bestmethods_boxplot <- rmse_folds_tidy %>%
    filter(RMSE<1.53) %>%
    mutate(method=reorder(method,-RMSE)) %>%
    mutate(Folds = reorder(Folds,-RMSE,FUN = max)) %>%
    ggplot() +
    geom_boxplot(aes(
      x=method,
      y=RMSE
      ,color=method
    )) + 
    geom_point(
      aes(x=method,
          y=RMSE,
          shape = method,
          color=method
      )
      ,size=1
      
    ) +
    scale_shape_manual(values=seq(20)) +
    geom_hline(yintercept = rmse_target_10,color="red",size = 1.05) +
    geom_hline(yintercept = rmse_target_15,color="orange",size = 1.05) +
    geom_hline(yintercept = rmse_target_20,color="blue",size = 1.05) +
    geom_hline(yintercept = rmse_target_25,color="green",size = 1.05) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  print(p_rmse_bestmethods_boxplot)
  
  ##Plot of all models as boxplot, keeping only the best models (rmse < 0.88)
  p_rmse_bestmethods_boxplot_zoom <- rmse_folds_tidy %>%
    filter(RMSE<0.88) %>%
    mutate(method=reorder(method,-RMSE)) %>%
    mutate(Folds = reorder(Folds,-RMSE,FUN = max)) %>%
    ggplot() +
    geom_boxplot(aes(
      x=method,
      y=RMSE
      ,color=method
    )) + 
    geom_point(
      aes(x=method,
          y=RMSE,
          shape = method,#shape = Folds,
          color=method
      )
      ,size=1
    ) +
    scale_shape_manual(values=seq(14)) +
    geom_hline(yintercept = rmse_target_15,color="orange",size = 1.05) +
    geom_hline(yintercept = rmse_target_20,color="blue",size = 1.05) +
    geom_hline(yintercept = rmse_target_25,color="green",size = 1.05) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  print(p_rmse_bestmethods_boxplot_zoom)
  
  #Plot showing that the rmse order of folds is in large kept between models
  p_rmse_bestmethods_pointline <- rmse_folds_tidy %>%
    filter(RMSE<0.884) %>%
    mutate(method=reorder(method,-RMSE)) %>%
    mutate(Folds = reorder(Folds,-RMSE,FUN = max)) %>%
    ggplot(aes(x = method,
               y = RMSE,
               group=Folds,
               shape = Folds)) + 
    geom_line() + 
    geom_point(aes(colour=method),size = 3) +
    scale_shape_manual(values=seq(10)) +
    geom_hline(yintercept = rmse_target_15,color="orange",size = 1.05) +
    geom_hline(yintercept = rmse_target_20,color="blue",size = 1.05) +
    geom_hline(yintercept = rmse_target_25,color="green",size = 1.05) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) 
  print(p_rmse_bestmethods_pointline)
  
  #savecount <- save_output_data(savenum = savecount, save_output = TRUE)
  save_output_data(savenum = savecount, 
                   save_output = TRUE,
                   objectlist = c(
                     "rmse_folds_tidy",
                     "rmse_folds_results_summary",
                     "rmse_target_10",
                     "rmse_target_15",
                     "rmse_target_20",
                     "rmse_target_25",
                     "p_rmse_bestmethods_boxplot",
                     "p_rmse_bestmethods_boxplot_zoom",
                     "p_rmse_bestmethods_pointline")
  )
}

# FINAL PREDICTION RMSE (applying best models to unseen data) ####
if (file.exists("Data/Output_022.RData")) {
  load("Data/Output_022.RData")
  savecount <- savecount + 1
  } else {
  # we now apply the best models as determined using the training data, to the edx and validation sets. As this is 
  # is unseen data, we cannot totally predict what the result will be however because 10 folds cross validation was
  # used and models were tuned until the target RMSE were reached for all folds and with a significant margin, 
  # we can have reasonable confidence that the target RMSEs should be met on that unseen data. 
  
  #best model with regularisation
  start_time <- Sys.time()
  final_pred_RMSE <-get_RMSE(validation$rating,
                             get_pred_muigyt_reg(
                               training_set = edx,
                               test_set= validation,
                               l_m = Lambda_Min_RMSE_worst_fold_mu,
                               l_u = Lambda_Min_RMSE_worst_fold_mu,
                               l_g = Lambda_Min_RMSE_worst_fold_mu_ig,
                               l_y = Lambda_Min_RMSE_worst_fold_mu_ig_y,
                               l_t = Lambda_Min_RMSE_worst_fold_mu_ig_y_t
                             )) 
  end_time <- Sys.time()
  duration <- end_time-start_time
  print(final_pred_RMSE)
  print(duration)
  #Time difference of 5.321855 mins
  #0.8428221 : the target RMSE of 0.8649 is reached. 
  
  #best model with no regularisation for speed
  start_time <- Sys.time()
  final_pred_fast_RMSE <-get_RMSE(validation$rating,
                                  get_pred_mu_indgenres(edx,validation
                                  )) 
  end_time <- Sys.time()
  duration_fast <- end_time-start_time
  print(final_pred_fast_RMSE)
  print(duration_fast)
  #Time difference of 1.198538 mins, about 3 times faster
  #0.8492771 : the target RMSE of 0.8649 is reached. 
  savecount <- save_output_data(savenum = savecount, 
                                save_output = TRUE,
                                objectlist <- c("final_pred_RMSE",
                                                "duration",
                                                "final_pred_fast_RMSE",
                                                "duration_fast")
                                )
  }