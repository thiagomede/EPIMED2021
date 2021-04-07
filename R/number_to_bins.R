#' Transforms a numerical column from a dataset in a categorical column
#' @export
#' @import ggplot2
#' @import gridExtra
#' @title Data wrangling
#' @param df a dataframe
#' @param cols a vector of strings with the name of the columns with the numerical data
#' @param bins a list of numerical vector containing the limits of the bins for each variable
number_to_bins<-function(df,cols=c('Age','UnitLengthStay'),
                         bins=list(c(0,10,20,40,60,70,80,110),c(0,1,2,3,7,15,200))){

  #require('ggplot2')
  #require('gridExtra')

  aux_list1=list()
  aux_list2=list()
  for (i in 1:length(bins)){
    cols2=sym(cols[i])
    bins2=unlist(bins[[i]])
    aux_col=paste0(cols[i],'_bin')
    aux_col2=sym(aux_col)
    aux_lab=paste0(bins2[-length(bins2)],'_to_',bins2[-1])
    df[aux_col]=cut(unlist(df[cols[i]]),breaks = bins2,include.lowest = T,
                    labels = aux_lab)
    aux_list1[[i]]=ggplot(df,aes(x=!!cols2))+
      geom_histogram(bins = 100,fill='white',col='black')+
      theme_classic()+
      ggtitle('Histogram of numeric data')+
      theme(axis.text.x = element_text(size=8),
            plot.title = element_text(size=10),
            axis.title.x = element_text(size=8))+
      ylab('')

    #print(aux_list1[[i]])

    aux_list2[[i]]=ggplot(df,aes(x=!!aux_col2))+
      geom_bar(fill='white',col='black')+
      theme_classic()+
      ggtitle('Barplot of categorical data')+
      theme(axis.text.x = element_text(size=5),
            plot.title = element_text(size=10),
            axis.title.x = element_text(size=8))+
      ylab('')

    #print(aux_list2[[i]])
  }

  grid.arrange(
    aux_list1[[1]],
    aux_list2[[1]],
    aux_list1[[2]],
    aux_list2[[2]],
    nrow = 2,
    ncol = 2,
    top = "Numerical to categorial transformation"

  )

  return(df)
}
