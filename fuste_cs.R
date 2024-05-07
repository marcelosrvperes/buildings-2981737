fuste_cs <- function(data1){
  data1 <- data1 %>% 
    mutate( Nfuste= ifelse(P_m< 7,0,lag(cummean(NSPTtip), n=6, default=0))) %>%   
    ungroup() 
}

