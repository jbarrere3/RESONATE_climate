#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_plot.R  
#' @description R script containing all functions relative to data
#               visualisation
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Function to plot the relation between pet and tmean for all sites together
#' @param data.clim climatic data per spatial point formatted
#' @param year.in Year for which to plot the data
#' @param file.out Name of the file to save, including path
plot.tmean.pet.allsites = function(data.clim, year.in, file.out){
  
  # Start by creating output directory if needed
  create_dir_if_needed(file.out)
  
  # Make the plot
  plot.out = data.clim %>%
    left_join(data.frame(month = c(1:12), month.name = month.abb), 
              by = "month") %>%
    mutate(month.name = factor(month.name, levels = month.abb)) %>%
    filter(year == year.in) %>%
    ggplot(aes(x = tmean, y = pet, color = month.name)) + 
    geom_point() + 
    facet_wrap(~ cs) + 
    scale_color_manual(values = colorRampPalette(c("blue", "orange", "royalblue"))(12))
  
  # Save the plot
  ggsave(file.out, plot.out, width = 20, height = 15, units = "cm", dpi = 600, 
         bg = "white")
  
  # Return the name of the file saved
  return(file.out)
  
}


#' Function to plot relation between tmean and pet per month
#' @param data.clim climate data per spatial point formatted
#' @param dir.out Directory whete to save all files
plot.per.month = function(data.clim, dir.out){
  
  # Create the output directoyr if it doesn't exist
  create_dir_if_needed(paste0(dir.out, "/test"))
  
  # Loop on all months of the year
  for(i in 1:12){
    
    # Make plot for month i
    plot.i = data.clim %>%
      mutate(year = as.character(year)) %>%
      filter(month == i) %>%
      ggplot(aes(x = tmean, y = pet, color = year)) + 
      geom_point(alpha = 0.5) + 
      facet_wrap(~ cs, scales = "free") +
      ggtitle(month.abb[i])
    
    # Save plot for month i
    ggsave(paste0(dir.out, "/", letters[i], "_", month.abb[i], ".jpg"), plot.i, 
           width = 20, height = 15, units = "cm", dpi = 600, bg = "white")
  }
  
  # Return the list of all files saved
  return(paste0(dir.out, "/", letters[c(1:12)], "_", month.abb, ".jpg"))
}

#' Function to plot relation between tmean and pet per site
#' @param data.clim climate data per spatial point formatted
#' @param dir.out Directory whete to save all files
plot.per.site = function(data.clim, dir.out){
  
  # Create the output directoyr if it doesn't exist
  create_dir_if_needed(paste0(dir.out, "/test"))
  
  # vector fo case studies
  cs.all = unique(data.clim$cs)
  
  # correct name of cs
  cs.all.cor = gsub("\\ ", "", cs.all)
  cs.all.cor = gsub("\\/", "", cs.all.cor)
  
  # Loop on all months of the year
  for(i in 1:length(cs.all)){
    
    # Make plot for month i
    plot.i = data.clim %>%
      mutate(year = as.character(year)) %>%
      filter(cs == cs.all[i]) %>%
      ggplot(aes(x = tmean, y = pet, group = interaction(ID.point, year), 
                 color = year)) + 
      geom_line() +
      ggtitle(cs.all[i])
    
    # Save plot for month i
    ggsave(paste0(dir.out, "/", cs.all.cor[i], ".jpg"), plot.i, 
           width = 20, height = 15, units = "cm", dpi = 600, bg = "white")
  }
  
  # Return the list of all files saved
  return(paste0(dir.out, "/", cs.all.cor, ".jpg"))
}


#' Function to plot the comparison of pet calculated with chelsa or WC
#' @param data.wc.pet worldclim data with pet calculated
#' @param data.chelsa climate data extracted from chelsa
#' @param file.out Name of the file to save, including path
plot_pet_chelsa_vs_wc = function(data.wc.pet, data.chelsa, file.out){
  
  # Start by creating output directory if needed
  create_dir_if_needed(file.out)
  
  # Make the plot
  plot.out = data.wc.pet %>%
    dplyr::select(cs, ID.point, month, month.name, pet_WC = pet_monthly) %>%
    left_join((data.chelsa %>% dplyr::select(cs, ID.point, month, pet_CH = pet)), 
              by = c("cs", "ID.point", "month")) %>%
    mutate(month.name = factor(month.name, levels = month.abb)) %>%
    ggplot(aes(x = pet_CH, y = pet_WC, color = month.name)) + 
    geom_point() + 
    facet_wrap( ~ cs) + 
    geom_abline(slope = 1, intercept = 0) + 
    scale_color_manual(values = colorRampPalette(c("blue", "orange", "royalblue"))(12))
  
  # Save the plot
  ggsave(file.out, plot.out, width = 20, height = 15, units = "cm", dpi = 600, 
         bg = "white")
  
  # Return the name of the file saved
  return(file.out)
  
}

#' Function to plot the comparison of average pet calculated with chelsa or WC
#' @param data.wc.pet worldclim data with pet calculated
#' @param data.chelsa climate data extracted from chelsa
#' @param file.out Name of the file to save, including path
plot_pet_chelsa_vs_wc_mean = function(data.wc.pet, data.chelsa, file.out){
  
  # Start by creating output directory if needed
  create_dir_if_needed(file.out)
  
  # Make the plot
  plot.out = data.wc.pet %>%
    dplyr::select(cs, ID.point, month, month.name, pet_WC = pet_monthly) %>%
    left_join((data.chelsa %>% dplyr::select(cs, ID.point, month, pet_CH = pet)), 
              by = c("cs", "ID.point", "month")) %>%
    mutate(month.name = factor(month.name, levels = month.abb)) %>%
    group_by(cs, month, month.name) %>%
    summarize(pet_CH_mean = mean(pet_CH, na.rm = TRUE), 
              pet_CH_lwr = quantile(pet_CH, 0.025, na.rm = TRUE), 
              pet_CH_upr = quantile(pet_CH, 0.975, na.rm = TRUE), 
              pet_WC_mean = mean(pet_WC, na.rm = TRUE), 
              pet_WC_lwr = quantile(pet_WC, 0.025, na.rm = TRUE), 
              pet_WC_upr = quantile(pet_WC, 0.975, na.rm = TRUE)) %>%
    ggplot(aes(x = pet_CH_mean, y = pet_WC_mean, color = month.name)) + 
    geom_point() + 
    geom_errorbarh(aes(xmin = pet_CH_lwr, xmax = pet_CH_upr), height = 0) + 
    geom_errorbar(aes(ymin = pet_WC_lwr, ymax = pet_WC_upr), width = 0) + 
    facet_wrap( ~ cs) + 
    geom_abline(slope = 1, intercept = 0) + 
    scale_color_manual(values = colorRampPalette(c("blue", "orange", "royalblue"))(12))
  
  # Save the plot
  ggsave(file.out, plot.out, width = 20, height = 15, units = "cm", dpi = 600, 
         bg = "white")
  
  # Return the name of the file saved
  return(file.out)
  
}