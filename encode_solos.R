encode_solos <- function(data_completa){

  encodePM1 <- as.numeric(data_completa$NUM_SOLO==1)
  
  encodePM2 <- as.numeric(data_completa$NUM_SOLO==2)
  
  encodePM3 <- as.numeric(data_completa$NUM_SOLO==3)
  
  encodePM4 <- as.numeric(data_completa$NUM_SOLO==4)
  
  encodePM5 <- as.numeric(data_completa$NUM_SOLO==5)
  
  encodePM6 <- as.numeric(data_completa$NUM_SOLO==6)  
  
  data_completa$S1 <- encodePM1
  
  data_completa$S2 <- encodePM2
  
  data_completa$S3 <- encodePM3
  
  data_completa$S4 <- encodePM4
  
  data_completa$S5 <- encodePM5 #- nenhum tipo encontrado
  
  data_completa$S6 <- encodePM6
  
  # data_completa <- data_completa %>% select(-S5)
  
  data_Encoding <- data_completa
  
  data_Encoding
}

