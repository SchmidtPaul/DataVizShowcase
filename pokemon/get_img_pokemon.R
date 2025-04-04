download_files <- function(names, urls, output_folder) {
  # Create output directory if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)
  
  # Loop through each file
  for (i in seq_along(names)) {
    name <- names[i]
    url <- urls[i]
    destination <- file.path(output_folder, paste0(name, ".png"))
    
    # Skip if file already exists
    if (file.exists(destination)) {
      next
    }
    
    # Add https: prefix if URL starts with //
    if (grepl("^//", url)) {
      url <- paste0("https:", url)
    }
    
    # Download the file
    tryCatch({
      download.file(url, destination, mode = "wb", quiet = TRUE)
    }, error = function(e) {
      warning("Failed to download ", name, ": ", e$message)
    })
  }
}

# tmp <- dat %>% slice(1:3)
# 
# # Usage example:
# # Download icons
# download_files(
#   names = tmp$pokemon,
#   urls = tmp$url_icon,
#   output_folder = "icon"
# )