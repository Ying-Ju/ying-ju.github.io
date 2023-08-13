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