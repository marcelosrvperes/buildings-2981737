unir_bd <- function(bd1,bd2){
  fundir <- merge(bd1,bd2, by=c("CONTADOR","TORRE")) %>% ungroup()
  
  fundir
}


