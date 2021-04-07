#' Fixes the classes of the columns of a dataset
#' @export
#' @import dplyr
#' @importFrom plyr mapvalues
#' @title Data wrangling
#' @param df a dataframe
#' @param col_cat a vector of strings. The column you want to rename the factor (if you do)
#' @param old_cat A list of vectors of strings. The label of the factors you want to rename
#' @param new_cat A list of vectors of strings. The new label you want to create
treat_class=function(df,col_cat=NULL,old_cat=list(c('0','1')),new_cat=list(c('NO','YES'))){
  #require(dplyr)
  #require(plyr)

  old_class=unlist(lapply(df,class))
  cat("BEFORE TREATMENT:\n\n")
  print(head(as_tibble(df[,-1])))

  if(sum(grepl('char',old_class))==ncol(df)){
    aux=unname(unlist(lapply(df,function(x){sum(grepl("\\D", x))})))
    aux2=which(aux==0)
    df=df %>% mutate_at(vars(aux2), as.integer)

    aux=unname(unlist(lapply(df,function(x){sum(grepl("[^A-Za-z]", x))})))
    aux2=which(aux==0)
    df=df %>% mutate_at(vars(aux2), as.factor)

    aux=unname(unlist(lapply(df,function(x){sum(!grepl("0$|1$", x))})))
    aux2=which(aux==0)
    df=df %>% mutate_at(vars(aux2), as.factor)
  }
  if(!is.null(col_cat) & length (old_cat)==1 & length (new_cat)==1){
    for(i in 1: length(col_cat)){
      df[col_cat[i]]=mapvalues(unname(unlist(df[col_cat[i]])), from = old_cat[[1]], to = new_cat[[1]])
    }}

  else if (!is.null(col_cat) & length (old_cat)==1 & length (new_cat)>1){
    for(i in 1: length(col_cat)){
      df[col_cat[i]]=mapvalues(unname(unlist(df[col_cat[i]])), from = old_cat[[1]], to = new_cat[[i]])
    }}

  else if (!is.null(col_cat) & length (old_cat)>1 & length (new_cat)>1){
    for(i in 1: length(col_cat)){
      df[col_cat[i]]=mapvalues(unname(unlist(df[col_cat[i]])), from = old_cat[[i]], to = new_cat[[i]])
    }}

  #print(unlist(lapply(df,class)))
  cat("\n\nAFTER  TREATMENT:\n\n")
  print(head(as_tibble(df[,-1])))
  #print(head(df))
  return(df)

}

