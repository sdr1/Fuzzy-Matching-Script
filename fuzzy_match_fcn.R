#### Ultimate Fuzzy Matching Algorithm ###
# outputs a data frame that can be used to translate from one data frame to another

library(dplyr)
library(pbmcapply)
library(stringdist)


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

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

process_columns <- function(df, col){
  col_of_df <- select(df, col)
  col_of_df[,1] <- iconv(col_of_df[,1], to = "UTF-8-MAC")
  unique_values <- unique(col_of_df[,1])
  unique_values[is.na(unique_values)] <- "NOTHING"
  return(unique_values)
}

firm_ending_regex = ",\\s{1}inc$|inc\\.$|[Ii][Nn][Cc]\\.{1}$|[Cc][Oo]$|LLC$|LLP$|,\\s{1}LLP$|, Inc.$|INC$|inc$| & Co$|Company$"

#target is what you're trying to match
#new_frame is the frame you're trying to match to the old frame

apply_fuzzy_match <- function(target_df_col, target_df, new_df_col, new_df, max.dist){
  # get unique columns
  new_df_orgs <- process_columns(new_df,new_df_col)
  target_df_orgs <- process_columns(target_df,target_df_col)
  
  #apply matching algorithm
  matches <- pbmclapply(X = new_df_orgs, FUN = function(X, b) fuzzyMatch(X,b), b=target_df_orgs)
  indx <- sapply(matches, length)
  match_df <- as.data.frame(do.call(rbind,lapply(matches, `length<-`,max(indx))))
  
  #update the data frame
  updated_match_df <- as.data.frame(cbind(target_df_orgs[match_df[,"match"]], match_df[,"match"], new_df_orgs, match_df[,"distance"]), stringsAsFactors = F)
  names(updated_match_df) <- c(target_df_col,"Row_Match",new_df_col, "Dist")

  updated_match_df$Dist <- as.numeric(updated_match_df$Dist)
  
  #good matches go in frame for merging
  good_matches <-   subset(updated_match_df, updated_match_df$Dist<=max.dist)

  #bad matches get processed to get rid of some common endings (inc, corp, etc...) then get another shot
  bad_matches <- updated_match_df[updated_match_df$Dist>max.dist,]
    
  #get rid of endings with regex above
  bad_matches$new_new_df_col <- trim(gsub(pattern = firm_ending_regex,  replacement = "", ignore.case = T, x = bad_matches[,3]))
  new_target_df_col <- trim(gsub(pattern = firm_ending_regex,  replacement = "", ignore.case = T, x = target_df_orgs))
    
  #only do the rematch with values that have changed (otherwise just wasting processor power)
  bad_matches <- bad_matches[bad_matches[,3] != bad_matches[,5],]
    
  #apply matching algorithm
  matches_2 <- pbmclapply(X = bad_matches[,5], FUN = function(X, b) fuzzyMatch(X,b), b=new_target_df_col)
   
  if(length(matches_2)==0){
      full_matches <- data.frame(good_matches[,c(1:4)], stringsAsFactors = F)
      cat("No additional matches", sep = "\n")
      return(good_matches)
    } else {
      cat("Removing endings leads to new results")
      #update data frame
      indx <- sapply(matches_2, length)
      
      match_df_2 <- as.data.frame(do.call(rbind,lapply(matches_2, `length<-`,max(indx))))
      
      #do the same thing we did above
      #update the data frame
      
      updated_match_df_2 <- as.data.frame(cbind(target_df_orgs[match_df_2[,"match"]], match_df_2[,"match"], bad_matches[,3], match_df_2[,"distance"]), stringsAsFactors = F)
      
      names(updated_match_df_2) <- c(target_df_col,"Row_Match",new_df_col, "Dist")
      
      good_matches_2 <- updated_match_df_2[updated_match_df_2$Dist<=max.dist,]
      
      full_matches <- as.data.frame(rbind(good_matches[,1:4], good_matches_2[,c(1:4)]))
      
      return(full_matches)
    }
}
  
#### re



