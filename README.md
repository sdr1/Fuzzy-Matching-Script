# Fuzzy-Matching-Script
This script implements a fuzzy matching algorithm in R.  It produces a dataframe with the closest match for each entry in a dataframe and the distance between the input and the match.  I recommend a threshold of 0.075 but you can alter this based on your own particular needs.

I find it particularly useful in matching commenter names in the study of regulations.  For example, the script can tell you that Goldman Sachs and Goldman Sachs & Co are the same organization while JP Morgan and Morgan Stanley are not.
