# This function is used to re classify the larger groups in the zoo data
# into their smaller groups and create new rows for each sub class

break_out_groups = function(df, catagories, name){
  
  # Match the sub groups to the groups and make the duplicate entries
  df_temp = catagories %>% 
    left_join(df, by = name) %>% 
    select(-all_of(name)) %>% 
    na.omit() %>% 
    rename(group = sub_group)
  
  # get the group names in a list to use in a filter
  used_groups = select(catagories, all_of(name)) %>% 
    distinct() %>% 
    as.list()
  
  # filter out the entries that are super groups and join to the 
  # groups used data
  df_out = df %>% 
    filter(!(group %in% used_groups[[1]])) %>% 
    bind_rows(df_temp)
  
  return(df_out)
}