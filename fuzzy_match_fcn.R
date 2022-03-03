library(dplyr)
library(pbmcapply)
library(stringdist)
library(magrittr)

#inputs: two data frames target_df and new_df.  outputs, dataframe with matches
fuzzyMatch <- function (a, b) {
  
  # calculate a jaccard dissimilarity matrix 
  distance <- stringdistmatrix(a,b,method = 'jw', p = 0.1, useBytes = T)
  
  # find the closest match for each
  match <- apply(distance, 1, which.min)
  
  # find how far away these were
  dists <- apply(distance, 1, min)
  
  # return these as a two-column matrix
  return (cbind(match = match,
                distance = dists))
  
}

apply_fuzzy_match <- function(df_a, col_in_a, df_b, col_in_b, max.dist){

  # df_a = Unmatched_Titles
  # col_in_a = "Match_Name"
  # df_b = unique_titles_in_raw_coding
  # col_in_b = "Match_Name"
  # max.dist = 0.2
  
  df_a_var <- c(col_in_a)
  df_b_var <- c(col_in_b)
  
  df_a_column <- df_a %>% dplyr::select(dplyr::all_of(df_a_var)) %>% pull()
  df_b_column <- df_b %>% dplyr::select(dplyr::all_of(df_b_var)) %>% pull()
  
  #apply matching algorithm
  matches <- pbmclapply(X = df_a_column, FUN = function(X, b) fuzzyMatch(X,b), b=df_b_column)
  indx <- sapply(matches, length)
  match_df <- as.data.frame(do.call(rbind,lapply(matches, `length<-`,max(indx))))
  
  #update the data frame
  a_frame <- df_a %>%
    dplyr::select(dplyr::all_of(df_a_var)) 

  b_frame <- df_b %>% 
    dplyr::select(dplyr::all_of(df_b_var)) %>%
    slice(match_df[,"match"]) %>%
    dplyr::bind_cols(match_df[,"match"]) %>%
    rename("Match_Row_In_B" = 2) %>%
    dplyr::bind_cols(match_df[,"distance"]) %>%
    rename("JW_Distance" = 3)
  
  a_frame %<>%
    dplyr::bind_cols(b_frame) %>% 
    filter(JW_Distance <= max.dist )

  names(a_frame) <- c(
    c(paste0("A_",col_in_a)), c(paste0("B_",col_in_b)), "Row_Match_From_DF_B", "JW_Distance"
  )
  
  return(a_frame)
}


