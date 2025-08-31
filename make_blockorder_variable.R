make_blockorder_variable <- function(df_orig){
  
  df_orig$block_order <- NA 
  
  df <- df_orig %>%
    group_by(id) %>%
    mutate(block_order = case_when(
      first(str_trim(as.character(condition))) == "book"  ~ "bsp",
      first(str_trim(as.character(condition))) == "phone" ~ "pbs",
      first(str_trim(as.character(condition))) == "still" ~ "spb",
      TRUE ~ NA_character_
    )) %>%
    ungroup()

  df
  
}
