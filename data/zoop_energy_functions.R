##### Description #####
# This is functions that put out zooplankton energy densities based on a file path 

##### Libraries #####
library(tidyverse)

select = dplyr::select

##### Functions #####
# convert wet to dry or vise versa 
convert_zoop_energy_density = function(ratio_df,
                                       energy_df,
                                       convert_to){
  converted_df_1 = ratio_df %>% 
    na.omit() %>% 
    right_join(energy_df, by = "group") 
  if(convert_to == "dry"){
    converted_df_2 = converted_df_1 %>% 
      mutate(energy_density_j_per_g_dry_mass =
             energy_density_j_per_g_wet_mass/dry_to_wet_ratio) %>% 
    select(-energy_density_j_per_g_wet_mass, -dry_to_wet_ratio) 
  } else if(convert_to == "wet"){
    converted_df_2 = converted_df_1 %>% 
      mutate(energy_density_j_per_g_wet_mass =
               energy_density_j_per_g_dry_mass*dry_to_wet_ratio) %>% 
      select(-energy_density_j_per_g_dry_mass, -dry_to_wet_ratio) 
  } else{
    stop("Please select 'wet' or 'dry' for convert_to")
  }
  
  return(converted_df_2)
  
}

# Load the data
load_zoop_energy_density = function(foulder){
  file_name_start = paste0(foulder,"/zoo_energy_")
  file_names = as.list(c( "dry", "wet", "digestibility", "ratio"))
  data = file_names %>% 
    map(~na.omit(read.csv(paste0(file_name_start, .x, ".csv"))) %>% 
          mutate_if(is.character, str_trim)) %>% 
    set_names(file_names)
  
  return(data)
}

# This is a function you can just run to get the symmary df
get_zoop_energy_density = function(foulder){
  data = load_zoop_energy_density(foulder) 
  
  summary_ratio = data$ratio %>% 
    group_by(group) %>% 
    summarize(dry_to_wet_ratio = mean(dry_to_wet_ratio))
  
  converted_wet = convert_zoop_energy_density(ratio_df = summary_ratio,
                                              energy_df = data$dry,
                                              convert_to = "wet") %>% 
    bind_rows(data$wet)
  
  wet_summary = converted_wet %>% 
    group_by(group) %>% 
    summarise(mean_energy_density = mean(energy_density_j_per_g_wet_mass),
              energy_density_sd = sd(energy_density_j_per_g_wet_mass))
  print(wet_summary)
  
  return(wet_summary)
  
}
