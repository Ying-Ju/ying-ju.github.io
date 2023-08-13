## The get_individual() function extracts details such as the name, degree, profile, and research areas from a given URL of a faculty member in the Department of Mathematics at the University of Dayton.

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
    
    # Extract the 5th paragraph within a 'div' element with class starting with 'wysiwyg'
    # If no such paragraph is found, set profile to "0"
    profile <- page %>%
      html_elements(xpath = "//div[starts-with(@class, 'wysiwyg')]") %>%
      html_elements(xpath = ".//p") %>%
      html_text() %>%
      .[5] %>%
      if_else(length(.) == 0, "0", .)
    
    # Extract the research details, excluding the degrees
    research <- page %>%
      html_elements(xpath = "//div[starts-with(@class, 'wysiwyg')]") %>%
      html_elements(xpath = ".//li") %>%
      html_text() %>%
      .[-c(1:k)] %>%
      paste(., collapse=", ") %>%
      if_else(length(.) == 0, "0", .)
    
  } else {
    # If no degrees found, set all fields to "0"
    k <- 0
    degree <- "0"; profile <- "0"; research <- "0";
  }
  
  # Combine the extracted name, degree, profile, and research into a data frame
  df <- data.frame(name, degree, profile, research)
  
  # Introduce a random delay between 3 and 15 seconds to mimic human browsing
  Sys.sleep(sample(c(3,15),1))
  
  # Return the data frame containing the extracted information
  return(df)
}
