Processo_Total <- function(data1){
  
  out <- lapply(data1, function(x) length(unique(x)))
  want <- which(!out > 1)
  unlist(want)
  data2 <- data1[,-(unlist(want))]
  
  a1 <- data2
  a1$teste <- ifelse(a1$L_m%%1==0,a1$L_m,NA)
  a1 <- na.exclude(a1)
  data3 <- a1 %>% select(1:(ncol(a1)-1))
  
  data4 <- data3 
  data4$P_m <- round(data3$L_m*cos(data3$I_GRAUS*pi/180),2)
  data5 <- data4 %>% select(1:ncol(data4),-I_GRAUS,-L_m)
  
  data6 <- data5[apply(data3!=0, 1, all),]
  
  df <- data6

  df <- df %>% group_by(TORRE, PE) %>% arrange(TORRE) %>%
    mutate(Nh1 = (rollmean(NSPTponta, k = 1,fill = NA)/6)) %>%
    mutate(Nh2 = (rollmean(NSPTponta, align = "right", k = 2,fill = NA)/3)) %>% 
    mutate(Nh3 = (rollmean(NSPTponta, align = "right", k = 3,fill = NA)/2)) %>% 
    mutate(Nh4 = (rollmean(NSPTponta, align = "right", k = 4,fill = NA)/(6/4))) %>% 
    mutate(Nh5 = (rollmean(NSPTponta, align = "right", k = 5,fill = NA)/(6/5))) %>% 
    mutate(Nh6 = rollmean(NSPTponta, align = "right", k = 6,fill = NA))
  
  df$Nh <- ifelse(is.na(df$Nh6), df$Nh5, df$Nh6)
  df$Nh <- ifelse(is.na(df$Nh), df$Nh4, df$Nh)
  df$Nh <- ifelse(is.na(df$Nh), df$Nh3, df$Nh)
  df$Nh <- ifelse(is.na(df$Nh), df$Nh2, df$Nh)
  df$Nh <- ifelse(is.na(df$Nh), df$Nh1, df$Nh)  
  df$Nh <- round(df$Nh,2)
  df <- df[,!colnames(df) %in% c("Nh1","Nh2","Nh3","Nh4","Nh5","Nh6")]
  
  df2 <- df
  
  df3 <- df2 %>% group_by(TORRE, PE) %>% arrange(TORRE) %>% 
    mutate( Nfacum= ifelse(CONTADOR < 6,0,lag(cumsum(NSPTponta), n=6, default=0))) %>%   
    ungroup() %>% arrange(TORRE, PE)
  
  df3$Nfacum <- round(df3$Nfacum,2)  
  
  data_fict <- df3
  
  df1 <- data_fict %>% 
    group_by(TORRE,PE) %>%
    mutate(N1 = NSPTponta[1],
           N2 = NSPTponta[2],
           N3 = NSPTponta[3],
           N4 = NSPTponta[4]) %>%   
    ungroup() %>% arrange(TORRE, PE)
df1
}