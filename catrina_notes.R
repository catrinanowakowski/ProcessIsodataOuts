## Catrina notes
# https://sahirbhatnagar.com/rpkg/#step-1-check-working-directory

pacman::p_load(sinew)
sinew::makeOxyFile("R/functions/Lab_Std_plots_N.R")

usethis::use_data_raw()


usethis::use_data("C:/Users/EcoGeoChemLab/porcess_isodat_outs/ProcessIsodataOuts/data/test_run/220118_203559__AA8_N_R1_Nowakowski_400ulst 50% split - 100ul (1 ul inj)-0000.dxf", internal = TRUE)

usethis::use_data("C:/Users/EcoGeoChemLab/porcess_isodat_outs/ProcessIsodataOuts/data/test_run/220118_203559__AA8_N_R1_Nowakowski_400ulst 50% split - 100ul (1 ul inj)-0000.dxf", AA8_N_R1)

#######################
## Need to add a function for the test aa std runs 
## Would be good to also have a function that does the AAstds alone 

## eventualy add a trophic position function ect... 

devtools::build_readme()
