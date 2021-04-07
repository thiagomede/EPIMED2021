#' Prints, excludes or replaces NA values from a dataset
#' @export
#' @import ggplot2
#' @importFrom utils head
#' @import naniar
#' @import dplyr
#' @title Data wrangling
#' @param df a dataframe
#' @param view_only logical. If TRUE, doesn't change the dataframe
#' @param plot_na logical. If TRUE, plots the number of NA values by column
#' @param df_na logical. If TRUE, prints all the rows of the original dataframe that contains NA values
#' @param exclude_na logical. If TRUE, the rows wich cointains NA values are deleted
#' @param col_exclude character. The name of the column which contains the NA values you want to exclude
#' @param replace_na logical. If TRUE, the NA values will be replaced instead of deleted
#' @param col_replace character. The name of the column which contains the NA value you want to replace
#' @param val_replace A vector with the values you want to replace the NA observations(if you do)
treat_na=function(df,view_only=F,plot_na=T,df_na=T,exclude_na=F,col_exclude=NULL,
                  replace_na=F,col_replace=NULL,val_replace=NULL){
  #require('ggplot2')
  #require('naniar')
  #require('dplyr')

  if(!is.null(col_exclude) & !exclude_na){
    exclude_na=T
    warning('col_exclude was not NULL, so exclude_na was set to TRUE')
  }

  if((!is.null(col_replace) & !is.null(val_replace)) & !replace_na){
    replace_na=T
    warning('col_replace and var_replace were not NULL, so replace_na was set to TRUE')
  }

  if(replace_na & (is.null(col_replace)|is.null(val_replace)))stop('replace_na is set to TRUE. you have to set var_replace and col_replace')

  if(exclude_na & is.null(col_exclude))stop('exclude_na is set to TRUE. you have to set col_exclude')

  if(exclude_na & replace_na){
    exclude_na=F
    warning('You cant replace and exclude a NA value. exclude_na parameter set to FALSE')}

  if (plot_na){g=gg_miss_var(df) + labs(y = "Number of missing observations")}

  if (df_na){new_df <- df[rowSums(is.na(df)) > 0,]}

  if(df_na & !plot_na)print(new_df)else if(!df_na & plot_na)print(g)else if (df_na & plot_na)print(list(new_df,g))

  if(exclude_na){
    col_exclude=ensym(col_exclude)
    df=df %>% filter(!is.na(!!col_exclude))}

  if (replace_na){
    aux=which(is.na(df[col_replace]))
    df[aux,col_replace]=val_replace}

  if(!view_only)return(df)
}

