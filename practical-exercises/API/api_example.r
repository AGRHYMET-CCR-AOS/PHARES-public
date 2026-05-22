############################################################
# R script for accessing a restricted PHARES APIs with a key
############################################################

####################
#  Settings
base_path <- "D:/CCR_AOS/PROJETS/TransfertFANFAR/Activites2026/Training3/repositories/PHARES-public/practical-exercises/API"
# Define your working directory, where downloaded files will be stored
  home_dir <- "D:/CCR_AOS/ACTIVITES/AGRHYMET/2026/DCEM/Dev/bulletin"      

# Define the path and name of the API key file (JSON)
  key_path_json <- file.path(Sys.getenv("HOME"),"phares","phares-fastapi-was-key",fsep = "\\")

# Define the path and name of the script containing the code for executing the API request with a JSON Key file
  api_request_script_name <- file.path(base_path,"api_request_with_key.R")  

# Define target_url, i.e. the API endpoint that you want to use
  # List of available APIs: https://github.com/AGRHYMET-CCR-AOS/PHARES-public/wiki/APIs-for-data-access
  # Each API then lists the endpoints available on the API main page. Examples for https://api-was.phares-agrhymet.cilss.int/
    # https://api-was.phares-agrhymet.cilss.int/api/storage/list
    # https://api-was.phares-agrhymet.cilss.int/api/storage/latest/list/{zone_name}
    # https://api-was.phares-agrhymet.cilss.int/api/storage/latest/download/{zone_name}

#target_url <- "https://api-was.phares-agrhymet.cilss.int/api/storage/list"
target_url <- "https://api-was.phares-agrhymet.cilss.int/api/storage/latest/list/wa-hype1.2_hydrogfd3.2_ecoper_noINSITU"
target_url <- "https://api-was.phares-agrhymet.cilss.int/api/storage/latest/download/wa-hype1.2_hydrogfd3.2_ecoper_noINSITU"

# Define client ID (no need to change normally)
client_id  <- "89447740177-oa9r2j411ugm9cu6k15qjsi3jkiao2kp.apps.googleusercontent.com" 

#######################
# Run the API request

system2(
  command = "Rscript",
  args = c(api_request_script_name, target_url, client_id, home_dir, key_path_json)
)
