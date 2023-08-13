# The JSON_files function is designed to download a compressed JSON file (with a .gz extension) from a given URL, decompress it, and then read the JSON content into R.
JSON_files <- function(each_link){
  # Get the file name
  download_path <- sub("^.*/", "", each_link)
  
  # Download the file and save it to the working directory
  GET(each_link, write_disk(download_path, overwrite=TRUE))
  
  # Remove .gz in the end of the file name
  decompressed_file <- sub("\\.gz$", "", download_path)
  
  # Decompress the Gzip File
  gunzip(download_path, destname = decompressed_file, overwrite = TRUE)
  
  # Read the JSON File
  instancesfile <- stream_in(file(decompressed_file))
  
  
  return(instancesfile)
}