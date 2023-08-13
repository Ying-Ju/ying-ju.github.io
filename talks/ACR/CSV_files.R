# The CSV_files() function is designed to perform a series of tasks related to downloading, extracting, and combining CSV files from a given URL.

CSV_files <- function(each_url){
  # Get the file name
  download_path <- sub("^.*/", "", each_url)
  
  # Download the file and save it to the working directory
  GET(each_url, write_disk(download_path, overwrite=TRUE))
  
  # Unzip the Files
  temp_dir <- tempdir() # the path to the current temporary directory
  unzip(download_path, exdir = temp_dir)
  
  # List the CSV Files
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names=TRUE)
  
  # Read the CSV Files
  data_list <- lapply(csv_files, read.csv)
  
  # Combine the Data from multiple CSV files
  combined_data <- bind_rows(data_list)
  
  # Optional: Clean Up
  unlink(download_path)
  
  return(combined_data)
}
