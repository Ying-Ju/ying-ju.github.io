# The get_CSV() function is designed to perform a series of tasks related to downloading, extracting, and combining CSV files from a given URL.
get_CSV <- function(URL){
  # Get the file name
  file_name <- sub("^.*/", "", URL)
  
  # Download the file and save it to the working directory
  GET(URL, write_disk(file_name, overwrite=TRUE))
  
  # Unzip the Files
  temp_dir <- tempdir() # the path to the current temporary directory
  unzip(file_name, exdir = temp_dir)
  
  # List the CSV Files
  csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names=TRUE)
  
  # Read the CSV Files
  data_list <- lapply(csv_files, read.csv)
  
  # Combine the Data from multiple CSV files
  combined_data <- bind_rows(data_list)
  
  # Optional: Clean Up
  unlink(file_name)
  
  return(combined_data)
}



# The get_JSON() function is designed to download a compressed JSON file (with a .gz extension) from a given URL, decompress it, and then read the JSON content into R.
get_JSON <- function(URL){
  # Get the file name
  file_name <- sub("^.*/", "", URL)
  
  # Download the file and save it to the working directory
  GET(URL, write_disk(file_name, overwrite=TRUE))
  
  # Remove .gz in the end of the file name
  decompressed_file <- sub("\\.gz$", "", file_name)
  
  # Decompress the Gzip File
  gunzip(file_name, destname = decompressed_file, overwrite = TRUE)
  
  # Read the JSON File
  instancesfile <- stream_in(file(decompressed_file))
  
  return(instancesfile)
}



# The get_faculty() function extracts information about faculty members from a given URL.
# Specifically, it retrieves the names, positions, and profile links of faculty members in the Department of Mathematics at the University of Dayton.
get_faculty <- function(URL){
  # Read the HTML content of the provided URL
  page <- read_html(URL)
  
  # Extract faculty names using the specific xpath, which targets links with a class starting with 'profile-listing__copy-header'
  faculty <- page %>%
    html_elements(xpath = "//a[starts-with(@class, 'profile-listing__copy-header')]") %>%
    html_text()
  
  # Extract faculty positions using the specific xpath, which targets paragraphs with a class starting with 'profile-listing__copy'
  position <- page %>%
    html_elements(xpath = "//p[starts-with(@class, 'profile-listing__copy')]") %>%
    html_text()
  
  # Extract all links that include the "/directory/artssciences/mathematics/" pattern
  # Then, make them full URLs by prefixing with the main university domain
  all_links <- page %>%
    html_elements(xpath = "//a") %>%
    html_attr("href") %>%
    .[str_detect(., "/directory/artssciences/mathematics/")] %>%
    .[c(1:(2*length(faculty)))] %>%
    unique() %>%
    paste0("https://udayton.edu", .)
  
  # Combine faculty names, positions, and links into a data frame
  df <- data.frame(faculty, position, all_links)
  
  # Introduce a random delay between 15 and 25 seconds to mimic human browsing and avoid potential rate-limiting
  Sys.sleep(sample(c(15,25),1))
  
  # Return the data frame containing the extracted information
  return(df)
}



# The get_individual() function extracts details such as the name, degree, profile, and research areas from a given URL of a faculty member in the Department of Mathematics at the University of Dayton.
get_individual <- function(URL){
  # Read the HTML content of the provided URL
  page <- read_html(URL)
  
  # Extract the name by targeting the text inside the h2 element
  name <- page %>%
    html_element(xpath = "//h2") %>%
    html_text()
  
  # Extract degrees by identifying list items within a div element with class starting with 'wysiwyg'
  # Only include the list items that contain a period and comma, usually found in degree information
  degree <- page %>%
    html_elements(xpath = "//div[starts-with(@class, 'wysiwyg')]") %>%
    html_elements(xpath = ".//li") %>%
    html_text() %>%
    .[str_detect(., "\\., ")]
  
  # Determine the length of the degree list
  k <- length(degree)
  
  # If there are degrees found
  if (k != 0) {
    
    # Concatenate the degrees into a single string, separated by semicolons
    degree <- degree %>%
      paste(., collapse="; ")
    
    # Extract the paragraph within a 'div' element with class starting with 'wysiwyg'
    # If no such paragraph is found, set profile to "NA"
    profile <- page %>%
      html_elements(xpath = "//div[starts-with(@class, 'wysiwyg')]") %>%
      html_elements(xpath = ".//p") %>%
      html_text() %>%
      .[5] %>%
      { if (. == "" || length(.) == 0 || is.na(.)) NA_character_ else . }
    
    # Extract the research details
    # If no such paragraph is found, set research to "NA"
    research <- page %>%
      html_elements(xpath = "//div[starts-with(@class, 'wysiwyg')]") %>%
      html_elements(xpath = ".//li") %>%
      html_text() %>%
      .[-c(1:k)] %>%
      paste(., collapse=", ") %>%
      { if (. == "" || length(.) == 0 || is.na(.)) NA_character_ else . }
    
  } else {
    # If no degrees found, set all fields to "0"
    k <- 0
    degree <- NA; profile <- NA; research <- NA;
  }
  
  # Combine the extracted name, degree, profile, and research into a data frame
  df <- data.frame(name, degree, profile, research)
  
  # Introduce a random delay between 3 and 15 seconds to mimic human browsing
  Sys.sleep(sample(c(3,15),1))
  
  # Return the data frame containing the extracted information
  return(df)
}