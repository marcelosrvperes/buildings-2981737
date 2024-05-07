ficticio <- function(data_fict){
  df1 <- data_fict %>% mutate(N1 = NSPTtip[1])
  
  df1 <- mutate(df1,N2 = NSPTtip[2])
  
  df1 <- mutate(df1,N3 = NSPTtip[3])
  
  df1 <- mutate(df1,N4 = NSPTtip[4])

  df1
}