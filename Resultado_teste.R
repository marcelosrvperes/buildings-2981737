Resultados_teste <- function(data){
  MAE <- round(caret::MAE(data[,1],data[,2]),2)
  RMSE <- round(caret::RMSE(data[,1],data[,2]),2)
  R2 <- round(caret::R2(data[,1],data[,2]),2)
  Resultado <- cbind(MAE,RMSE,R2)
  Resultado  
}

