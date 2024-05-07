PM <- function(df){
  avg.last.6 <- function (x) if (length(x-1) < 6) rep(NA, length(x-1)) else rollmeanr(x, 6, fill = NA)  
  
  res6 <- df %>% 
    mutate(NSPThelix=round(avg.last.6(NSPTtip),2)) %>%        ## 3.
    ungroup()        ## 4.
  
  avg.last.5 <- function (x) if (length(x-1) < 5) rep(NA, length(x-1)) else rollmeanr(x, 5, fill = NA)  
  
  res5 <- df %>% 
    mutate(NSPThelix=round(avg.last.5(NSPTtip),2)) %>%        ## 3.
    ungroup()        ## 4.
  
  avg.last.4 <- function (x) if (length(x-1) < 4) rep(NA, length(x-1)) else rollmeanr(x, 4, fill = NA)  
  
  res4 <- df %>%
    mutate(NSPThelix=round(avg.last.4(NSPTtip),2)) %>%        ## 3.
    ungroup()        ## 4.
  
  avg.last.3 <- function (x) if (length(x-1) < 3) rep(NA, length(x-1)) else rollmeanr(x, 3, fill = NA) 
  
  res3 <- df %>%
    mutate(NSPThelix=round(avg.last.3(NSPTtip),2)) %>%        ## 3.
    ungroup()        ## 4.
  
  avg.last.2 <- function (x) if (length(x-1) < 2) rep(NA, length(x-1)) else rollmeanr(x, 2, fill = NA)
  
  res2 <- df %>%    
    mutate(NSPThelix=round(avg.last.2(NSPTtip),2)) %>%        
    ungroup()        
  
  df$NSPThelix <- ifelse(is.na(res6$NSPThelix), res5$NSPThelix, res6$NSPThelix)
  df$NSPThelix <- ifelse(is.na(df$NSPThelix), res4$NSPThelix, df$NSPThelix)
  df$NSPThelix <- ifelse(is.na(df$NSPThelix), res3$NSPThelix, df$NSPThelix)
  df$NSPThelix <- ifelse(is.na(df$NSPThelix), res2$NSPThelix, df$NSPThelix)
  df$NSPThelix <- ifelse(is.na(df$NSPThelix), 0, df$NSPThelix)  
  
  df
}