# --- Configuration ---
# home_dir <- Sys.getenv("HOME")
# if (home_dir == "") home_dir <- Sys.getenv("USERPROFILE")
# key_path_json <- file.path(home_dir, "phares", ".phares-was-fastapi-key")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4) stop("Usage: ./api_request.R <URL> <IAP_CLIENT_ID>")
target_url <- args[1]
client_id  <- args[2]
home_dir  <- args[3]
key_path_json  <- args[4]

#!/usr/bin/env Rscript
library(httr)
library(jsonlite)
library(openssl)

# --- SSL FIX ---
set_config(config(ssl_verifypeer = 0L)) 

# URL-safe Base64 Helper
b64u <- function(x) {
  if (is.list(x)) x <- jsonlite::toJSON(x, auto_unbox = TRUE)
  if (is.character(x)) x <- charToRaw(x)
  res <- openssl::base64_encode(x)
  res <- gsub("\\+", "-", res)
  res <- gsub("/", "_", res)
  res <- gsub("=+$", "", res)
  return(res)
}

get_iap_token <- function(kp_path, audience) {
  cat("[*] Reading JSON key...\n")
  info <- jsonlite::fromJSON(kp_path)
  
  cat("[*] Parsing Private Key (OpenSSL 3.x compatible)...\n")
  # read_key is the modern replacement for read_pem
  # We pass it the raw character string directly
  priv_key <- openssl::read_key(info$private_key)
  
  cat("[*] Minting JWT...\n")
  header <- list(alg = "RS256", typ = "JWT", kid = info$private_key_id)
  payload <- list(
    iss = info$client_email,
    sub = info$client_email,
    aud = "https://oauth2.googleapis.com/token",
    target_audience = audience,
    iat = as.integer(Sys.time()),
    exp = as.integer(Sys.time()) + 3600
  )
  
  token_base <- paste(b64u(header), b64u(payload), sep = ".")
  
  # Step 4: Signing
  # We use the raw vector of the string to avoid 'file must be connection'
  # and we use the key object we just created
  sig <- openssl::signature_create(
    data = charToRaw(token_base),
    hash = openssl::sha256,
    key = priv_key
  )
  
  final_jwt <- paste(token_base, b64u(sig), sep = ".")
  
  cat("[*] Exchanging for Token...\n")
  res <- POST(
    "https://oauth2.googleapis.com/token",
    body = list(
      grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer",
      assertion = final_jwt
    ), 
    encode = "form"
  )
  
  if (status_code(res) != 200) stop(content(res, "text"))
  return(content(res)$id_token)
}

tryCatch({
  id_token <- get_iap_token(key_path_json, client_id)
  cat("[+] Token obtained.\n")
  
  response <- GET(target_url, add_headers(Authorization = paste("Bearer", id_token)))
  
  if (status_code(response) == 200) {
    # File handling
    content_type <- headers(response)[["content-type"]]
    cd <- headers(response)[["content-disposition"]]
    
    if (!is.null(cd) || !grepl("json|text", content_type)) {
      save_name <- if(!is.null(cd)) file.path(home_dir,gsub('.*filename=',"",cd)) else file.path(home_dir,"downloaded_file")
      writeBin(content(response, "raw"), save_name)
      cat("[+] Saved to:", save_name, "\n")
    } else {
      cat(prettify(content(response, "text", encoding = "UTF-8")), "\n")
    }
  } else {
    cat("[-] API Error:", status_code(response), "\n")
    cat(prettify(content(response, "text")), "\n")
  }
}, error = function(e) {
  message("\n[FATAL ERROR] ", e$message)
})
