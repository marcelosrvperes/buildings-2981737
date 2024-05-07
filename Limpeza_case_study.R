

Limpeza2 <- function(data1){

  data4 <- data1
  data4$P_m <- round(data1$L_m*cos(data1$I_GRAUS*pi/180),2)
  data5 <- data4 %>% select(1:ncol(data4),-I_GRAUS,-L_m)
  
  data6 <- data5[apply(data5!=0, 1, all),]
  
  data6
  
}
