#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name _targets.R  
#' @description R script to launch the target pipeline
#' @author Julien BARRERE
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Options and packages ----------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load targets
library(targets)
# Load functions
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))
# install if needed and load packages
packages.in <- c("dplyr", "ggplot2", "tidyr", "sf", "targets", "rnaturalearth", 
                 "data.table", "terra", "elevatr")
for(i in 1:length(packages.in)){
  if(!(packages.in[i] %in% rownames(installed.packages()))){
    install.packages(packages.in[i])
  }
}  
# Targets options
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")
tar_option_set(packages = packages.in,
               memory = "transient")
future::plan(future::multisession, workers = 6)
set.seed(2)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Targets workflow --------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(
  # Load and read coord data file
  tar_target(coord_file, "data/coordinates_Resonate.csv", format = "file"), 
  tar_target(coord, (fread(coord_file) %>% mutate(ID.point = c(1:dim(.)[1])))),
  tar_target(coord_Z, add_elevation_to_coord(coord)),
  
  # WORLDCLIM climate files
  tar_target(dir.wc, "data/worldclim", format = "file"),
  tar_target(data.wc, extract_wordlclim(coord_Z, dir.wc)), 
  
  # ISIMIP climate files
  # - Directory where data are stored
  tar_target(dir.isimip, "data/isimip", format = "file"), 
  # - All combinations of ssp scenarios and time period of data
  tar_target(df.ssp.date, expand.grid(
    date = c("2015_2020", "2021_2030", "2031_2040", "2041_2050", "2051_2060", 
             "2061_2070", "2071_2080", "2081_2090", "2091_2100"),
    ssp = c("ssp126", "ssp370", "ssp585"))), 
  # - Vector of same length as df.ssp.date (will be useful for branching)
  tar_target(ID.ssp.date, c(1:dim(df.ssp.date)[1])),
  # - Extract future climate 
  tar_target(future_clim_isimip, get_future_clim_isimip(
    dir.isimip, data.wc, df.ssp.date$ssp[ID.ssp.date], 
    df.ssp.date$date[ID.ssp.date]), pattern = map(ID.ssp.date), iteration = "list"), 
  # - Compile data in one data frame
  tar_target(future_clim_all, compile_future_clim_all(future_clim_isimip)), 
  # - Average across each case study
  tar_target(future_clim, summarize_future_clim(future_clim_all)), 
  # - Export future climate file
  tar_target(future_clim_file_all, write_on_disk(
    future_clim_all, "output/future_clim_all.csv"), format = "file"), 
  tar_target(future_clim_file, write_on_disk(
    future_clim, "output/future_clim.csv"), format = "file")
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### - Compare chelsa pet vs PET calculated directly with isimip and wordlclim
  
  # ISIMIP
  # -- Extract ISIMIP daily data
  # tar_target(data.isimip.daily, extract_isimip(coord, isimip_files)),
  # 
  
  # CHELSA
  # # -- Download chelsa data
  # tar_target(data.dl.chelsa, to_download_CHELSA(
  #   var.in = c("tas", "pet", "pr", "tasmax", "tasmin"), 
  #   month.in = c(paste0("0", c(1:9)), as.character(c(10:12))), 
  #   year.in = c(2011), dir.out = "data/climate")), 
  # tar_target(chelsa_files, download_CHELSA(data.dl.chelsa), format = "file"), 
  # # -- Extract chelsa data per coordinates
  # tar_target(data.chelsa.raw, extract_chelsa(data.dl.chelsa, chelsa_files, coord)), 
  # tar_target(data.chelsa, format.data.chelsa(data.chelsa.raw)), 
  
  # Plot the comparison between pet with wc or chelsa
  # tar_target(fig_wc_vs_chelsa, plot_pet_chelsa_vs_wc(
  #   data.wc.pet, data.chelsa, "output/fig/chelsa_vs_wc.jpg"), format = "file"), 
  # tar_target(fig_wc_vs_chelsa_mean, plot_pet_chelsa_vs_wc_mean(
  #   data.wc.pet, data.chelsa, "output/fig/chelsa_vs_wc_mean.jpg"), format = "file"),
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
  
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ### - Plot the relation between tmean and pet for year 2011 with chelsa data
  
  # # -- For all sites
  # tar_target(fig_allsites, plot.tmean.pet.allsites(
  #   data.chelsa, year.in = "2011", file.out = "output/fig/allsites.jpg"), 
  #   format = "file"), 
  
  # # -- Per month 
  # tar_target(fig_permonth, plot.per.month(data.chelsa, "output/fig/permonth"), 
  #            format = "file"), 
  
  # # -- Per case study
  # tar_target(fig_percs, plot.per.site(data.chelsa, "output/fig/percs"), 
  #            format = "file")
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  
  
)

