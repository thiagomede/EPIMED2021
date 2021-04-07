#' Creates barplots for 2 or 3 variables
#' @export
#' @import ggplot2
#' @title Barplots
#' @param df a dataframe
#' @param var_plot character. the variable for the x-axis
#' @param var_label character. the variable for the y-axis
#' @param var_grid character. the variable you will use to split the graph (if you do)
graph_plot=function(df,var_plot,var_label, var_grid=NULL){

  #require(ggplot2)
  x2=sym(var_plot)
  y2=sym(var_label)
  if(!is.null(var_grid))z2=sym(var_grid)

  if(is.null(var_grid)){

    a=ggplot(df,aes(x=!!x2,fill=!!y2))+geom_bar(position = 'fill',color='black')+
      labs(title=paste('Relation Between ',var_plot,' and ',var_label))+
      theme(axis.text.x = element_text(size=6))+ylab('Prop.')+
      scale_fill_discrete(name = var_label)+
      theme_classic()+
      theme(axis.text.x = element_text(size=6),
            axis.title.x = element_text(size=8))+
      scale_fill_manual(values = c("azure3","darkred"))
    return(a)}else{

      a=ggplot(df,aes(x=!!x2,fill=!!y2))+geom_bar(position = 'fill',color='black')+
        labs(title=paste('Relation Between ',var_plot,' and ',var_label),
             subtitle = paste('Filtered by ',var_grid))+
        ylab('Prop.')+
        scale_fill_discrete(name = var_label)+
        theme_classic()+scale_fill_manual(values = c("azure3","darkred"))+
        theme(axis.text.x = element_text(size=6,angle = 90),
              axis.title.x = element_text(size=8))+
        facet_wrap(as.formula(paste('~', var_grid)))
      return(a)
    }

}
