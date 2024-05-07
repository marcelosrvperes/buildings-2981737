numero_solos <- function(NSPT_Torres){
  num_solos <- NSPT_Torres %>%
    mutate(
      NUM_SOLO = case_when(
        SOIL=="Clay" ~1,
        SOIL=="Clayey sand" ~2,
        SOIL=="Clayey silt" ~3,
        SOIL=="Clayey, fine sand" ~4,
        SOIL=="Clayey, fine sand -SC" ~4,
        SOIL=="Clayey, medium sand" ~4,
        SOIL=="Fine sand" ~5,
        SOIL=="Lateritic level" ~6,
        SOIL=="Med, sand" ~5,
        SOIL=="Sandy clay" ~6,
        SOIL=="Silty clay" ~ 3,
        SOIL=="Silty, fine sand" ~2)) %>% ungroup()
  
  colnames(num_solos) <- c("CONTADOR", "TORRE", "SOLO","N_SOLO","NUM_SOLO")
  
  data_solo1 <- num_solos %>% 
    mutate( 
      SOLO = case_when(
        NUM_SOLO==1~"Argila",
        NUM_SOLO==2~"Silte arenoso",
        NUM_SOLO==3~"Silte argiloso",
        NUM_SOLO==4~"Areia argilosa",
        NUM_SOLO==5~"Areia",
        NUM_SOLO==6~"Argila arenosa")) %>% ungroup()
  
  data_solo2 <- data_solo1 %>% select(-4)
  
  data_solo2
}
  
 
