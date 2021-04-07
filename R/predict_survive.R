#' Trains, tests and plots predictions a random forest model
#' @export
#' @import ranger
#' @importFrom stats as.formula predict rbinom
#' @title Classification Model
#' @param df a dataframe
#' @param df_new_pacients a dataframe with new data you want to predict the outcome
#' @param prop_train numerical between 0 and 1. The proportion of the original dataset you will use to train the model
#' @param seed integer. the seed you want to use to replicate the same results
#' @param features a vector of characters. the features you will use in your model
#' @param col_weight a vector of strings. The columns on which rows you will apply the case.weights of the random forest model
#' @param val_weight a vector of strings. The values of the rows that identifies where to apply the case.weights
#' @param acc_weight a numerical vector. The case.weights you will apply
predict_survive=function(df,df_new_pacients=NULL,prop_train=0.8,seed=9238,
                         features=c('Age_bin','Gender','UnitLengthStay_bin','IsArterialHypertension'),
                         col_weight=c('UnitDischargeCode','Age_bin','Age_bin','UnitLengthStay_bin'),
                         val_weight=c('D','0_to_10','10_to_20','15_to_200'),
                         acc_weight=c(.25,.25,.25,.25)){

  #require('ranger')
  list_result=list()
  aux_form=paste('UD_CODE','~',paste(features,collapse = '+'))

  set.seed(seed)
  aux_train=sample(x = 1:nrow(df),size =round(prop_train*nrow(df)),replace=F)
  treino=df[aux_train,]
  teste=df[-aux_train,]

  aux_weight=rep(1,nrow(treino))

  for(i in 1:length(col_weight)){
    aux_weight[which(treino[col_weight[i]]==val_weight[i])]=aux_weight[which(treino[col_weight[i]]==val_weight[i])]+acc_weight[i]
  }


  m2=ranger(as.formula(aux_form),data=treino,
            seed=seed,probability = T,importance = 'impurity',
            case.weights  = aux_weight)


  pred=predict(m2,teste,probability=T)


  list_result[[1]]=m2$variable.importance


  teste3=teste

  teste3$PROB=pred$predictions[,1]
  teste3$PROB_BIN=NA
  teste3['PROB_BIN']=cut(unlist(teste3['PROB']),breaks = seq(0,1,0.1),include.lowest = T)
  g=graph_plot(teste3,var_plot = 'PROB_BIN',var_label = 'UD_CODE')


  list_result[[2]]=g

  aux_teste=table(teste3$PROB_BIN,teste3$UD_CODE)
  aux_teste=data.frame(PROB_ALIVE=c(.05,.15,.25,.35,.45,.55,.65,.75,.85,.95),A=aux_teste[1:10],D=aux_teste[11:20])
  aux_teste$SOMA=aux_teste$A+aux_teste$D
  aux_teste=aux_teste[which(aux_teste$SOMA>0),]

  aux_bin=list()
  for(i in 1:nrow(aux_teste)){
    aux_bin[[i]]=rbinom(10000,size = aux_teste$SOMA[i],prob = aux_teste$PROB[i])
  }

  aux_teste$PROB_MENOR=NA
  aux_teste$PROB_MAIOR=NA
  for( i in 1:length(aux_bin)){
    aux_teste$PROB_MENOR[i]=table(aux_bin[[i]]>=(aux_teste$A[i]))[2]/10000
    aux_teste$PROB_MAIOR[i]=table(aux_bin[[i]]<=(aux_teste$A[i]))[2]/10000
  }
  aux_teste[is.na(aux_teste)]=0.999
  aux2=rep(0,nrow(aux_teste))
  aux2=aux2+(aux_teste$PROB_MENOR<0.1)+(aux_teste$PROB_MAIOR<0.1)
  aux_teste$BINOM=aux2

  aux_teste$MODEL_FIT=NA
  for (i in 1:nrow(aux_teste)){
    aux_teste$MODEL_FIT[i]=if(aux_teste$PROB_MAIOR[i]<0.05){
      'OVERESTIMATED'}else if(aux_teste$PROB_MENOR[i]<0.05){
        'UNDERESTIMATED'} else 'OK'
  }

  list_result[[3]]=aux_teste[,c(1,2,3,8)]
  names(list_result)=c('Variable_Importance','Test_Predictions_Plot','Model_Fit')
  if (!is.null(df_new_pacients)){
    pred2=predict(m2,df_new_pacients,probability=T)
    df_new_pacients$PROB_ALIVE=pred2$predictions[,1]
    list_result[[4]]=df_new_pacients
    names(list_result)[4]='Predictions_new_pacients'
  }
  return(list_result)
}
