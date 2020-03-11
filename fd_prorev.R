#'This function will add a note to each customer review in FD,
#'suggesting a possible match for the author of the review.
#'
#'It takes two arguments as input:
#' - day: a string/date in the format "yyyy-mm-dd"
#' - contacts: a string including the path of the list of contacts from FD
#'
#'The function will update all the reviews created the day.
#'
#'For each review where possible matching contacts are detected, the function
#'will write the emails which possible correspond to the author. If more than
#'10 emails are found, no suggestion will be returned but the message that
#'not suggestions could be found.
#'
#'It is recommended to run this once at the beginning of each day, to ensure
#'that every review sent from AppBot night time is targeted.


## INPUT example
# day <- "yyyy-mm-dd"

prorev <- function(day){
  require(googledrive, quietly = TRUE)
  require(httr, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(openxlsx, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(readr, quietly = TRUE)
  
  ## Set the different arguments or parameters:
  
  FD_api <- "z6L822MqF9kZ2K3D4J8c"
  apikey <- Sys.getenv("fd_api") # constant
  tag <- "appbot" # user input, default NULL
  group_id <- 19000105695 # constant
  created_at <- day # user input
  languages <- c("english", "german", "italian", "french", "spanish")
  appbot_os <- read.csv("appbot_os.csv", header = T, sep=",", stringsAsFactors = F)
  devices <- read.csv("devices.csv", header = TRUE)

  contacts <- FD_contacts(day) ## updating contacts from new ones in FD
  
  ## Parts of the curl
  
  ini_url <- "https://alk.freshdesk.com/api/v2/search/tickets?query=\""
  space <- "%20AND%20"
  single_quote <- "%27"
  tag_url <- paste0("tag:", single_quote, tag, single_quote)  
  group_url <- paste0("group_id:", group_id)
  created_url <- paste0("created_at:>", single_quote, created_at, single_quote)
  page_url <- "\"&page="
  
  ## Building the call across numerous tickets and pages
  
  page_num <- 1
  full_url <- paste0(ini_url,
                     tag_url, space, 
                     group_url, space,
                     created_url,
                     page_url, page_num)
  
  requests <- GET(full_url, authenticate(apikey, "X", type = "basic"))
  
  ## storing the content of the call in a list including all requests
  req_content <- content(requests)
  req_list <- req_content[[1]]
  
  num_req <- length(req_content[[1]])
  while (num_req == 30) {
    page_num <- page_num + 1
    full_url <- paste0(ini_url,
                       tag_url, space, 
                       group_url, space,
                       created_url,
                       page_url, page_num)
    
    requests <- GET(full_url, authenticate(apikey, "X", type = "basic"))
    req_content <- content(requests)
    req_list <- append(req_list, req_content[[1]])
    num_req <- length(req_content[[1]])
  }
  
  ## create an empty dataframe to store all requests
  requests_df <- data.frame(matrix(ncol = 12, nrow = 0))
  colnames(requests_df) <- c(
    "Ticket",
    "Description",
    "Name",
    "Email",
    "Device_os",
    "Devicename",
    "Product",
    "Version",
    "Map Data",
    "Tags",
    "Language",
    "Key_Author")
  
  
  # populate the dataframe
  for (i in 1:length(req_list)) {
    
    tags_ls <- req_list[i][[1]]$tags
    requests_df[i, 10] <- paste(tags_ls, collapse = ", ") # tags
    
    langue <- grep(paste0(languages , collapse="|"), tags_ls, value=T) # language
    requests_df[i, 11] <-  if(length(langue)!=0){str_to_title(langue)}else{"English"} #language
    
    requests_df[i, 1] <- req_list[i][[1]]$id
    
    requests_df[i, 2] <- req_list[i][[1]]$description_text %>%
      str_replace_all("\r", "")
    
    author <- req_list[i][[1]]$description %>%
      str_extract("by\\s.*?\\<br\\>") %>%       ## extracting the author name
      str_replace("by\\s", "") %>%              ## trimming left
      str_replace( "\\<br\\>", "") %>%          ## trimming right
      iconv("latin1", "ASCII//TRANSLIT")     ## converting to unicode
    requests_df[i, 3] <- if(length(author)==0){""}else{author}
    
    if(grepl("os-version", requests_df[i, 10])){
      
      os_version <- requests_df[i, 10] %>%
        str_extract("os-version_.\\d") %>%       ## extracting the device OS
        str_replace("os-version_", "") %>%            ## trimming left
        str_replace(", ", "")       ## trimming right
      
      requests_df[i, 5] <- appbot_os$Platform[which(appbot_os$os.version==os_version)]
    }else if(grepl("googleplay", requests_df[i, 10])){
      requests_df[i, 5] <- "Android"
    }else{
      requests_df[i, 5] <- "iOS"}
    
    
    dev_data <- requests_df[i, 10] %>%
      str_extract("device_.*?[[:punct:]]|device_.*?$") %>%       ## extracting the device name
      str_replace("device_", "^")  %>%                           ## trimming left
      str_replace(",|$", "$")                                  ## trimming right
   
    if(length(dev_data)>0){
      devMan <- devices[grep(dev_data, devices$Model.Code, ignore.case = TRUE),'Manufacturer']
      devMod <- devices[grep(dev_data, devices$Model.Code, ignore.case = TRUE),'Model.Name']
      requests_df[i, 6] <- paste(devMan, devMod, collapse = " ")
    }else{requests_df[i, 6] <- "NA"}
    
    
    # building suggestion for the author name
    author <- author %>% strsplit('[[:space:]]')   
    num_words <- length(author[[1]])              ## number of words included in the author
    name_str <- strsplit(author[[1]][1], '*')
    first <- paste0('[', toupper(name_str[[1]][1]),tolower(name_str[[1]][1]), ']')
    if(num_words>1){
      surname_str <- strsplit(author[[1]][num_words],'*')
      second <- paste0('[', toupper(surname_str[[1]][1]),tolower(surname_str[[1]][1]), ']')
      last <- paste0(surname_str[[1]][2:length(surname_str[[1]])], collapse = '')
      key_author <- paste0('^',first, '.*?',second,'.*?', last)
    }
    else{ 
      last <- paste0(name_str[[1]][2:(length(name_str[[1]])-1)], collapse = '')
      key_author <- paste0('^',first, '.*?', last) }
    
    nick <- c(contacts$Email[grep(key_author, contacts$Email)], contacts$Email[grep(key_author, contacts$Full.name)])
    
    nicks <-  paste(unique(nick), collapse=", ")
    
    if(0 < length(nick) & length(nick) < 11){
      note_msg <- "See if any of these emails match with the author of the review: "
      requests_df[i, 12] <- paste0(note_msg, nicks)                   
    }else{
      requests_df[i, 12] <- "No suggestions for the author email."
    }
    
  }
  
  # scripting put calls with all the values to be added in FD
  curl_note <- 'curl -v -u z6L822MqF9kZ2K3D4J8c:X -H \"Content-Type: application/json\" -X POST -d "{\\\"body\\\":\\\"'
  curl_y <- '\\\"}" https://alk.freshdesk.com/api/v2/tickets/'
  curl_prop <- 'curl -v -u z6L822MqF9kZ2K3D4J8c:X -H \"Content-Type: application/json\" -X PUT -d "{\\\"custom_fields\\\":{'
  curl_x <- '}}" https://alk.freshdesk.com/api/v2/tickets/'
  
  for (i in 1:nrow(requests_df)) {
    ##
    name_txt <-
      paste0("\\\"name\\\":\\\"", requests_df[i, 3], '\\\"')
    
    ## Device OS
    
    
    if (grepl("iOS", requests_df[i, 5])) {
      os_txt <-
        paste0(
          '\\\"device_platform\\\":\\\"',
          "iOS",
          '\\\"',
          ",",
          '\\\"device_os\\\":\\\"',
          "iOS",
          '\\\"'
        )
    } else if (grepl("Android", requests_df[i, 5])) {
      os_txt <-
        paste0(
          '\\\"device_platform\\\":\\\"',
          "Android",
          '\\\"',
          ",",
          '\\\"device_os\\\":\\\"',
          "Android",
          '\\\"'
        )
    }else {
      os_txt <-
        paste0(
          '\\\"device_platform\\\":\\\"',
          "Android",
          '\\\"',
          ",",
          '\\\"device_os\\\":\\\"',
          "Android",
          '\\\"',
          ",",
          "\\\"mobile_device\\\":\\\"",
          requests_df[i, 5],
          '\\\"'
        )
    }
    
    
    
    ## device model
    device_txt <-
      paste0("\\\"device_make_and_model\\\":\\\"", requests_df[i, 6], '\\\"')
    ## language
    langue_txt <-
      paste0("\\\"language\\\":\\\"", requests_df[i, 11], '\\\"')
    
    ## Product
    if (grepl("truckgps", requests_df[i, 10])) {
      product_txt <- paste0("\\\"product\\\":\\\"", "Truck GPS", '\\\"')
    } else if (grepl("truck.gps", requests_df[i, 10])) {
      product_txt <- paste0("\\\"product\\\":\\\"", "Truck GPS", '\\\"')
    } else{
      product_txt <- paste0("\\\"product\\\":\\\"", "GPS", '\\\"')
    }
    
    
    ## CoPilot Version
    if (grepl("v10.9", requests_df[i, 10])) {
      version_txt <- paste0("\\\"app_version\\\":\\\"", "V10.9", '\\\"')
    }else if (grepl("v10.10", requests_df[i, 10])) {
      version_txt <- paste0("\\\"app_version\\\":\\\"", "V10.10", '\\\"')
    }else if (grepl("v10.7", requests_df[i, 10])) {
      version_txt <- paste0("\\\"app_version\\\":\\\"", "V10.7", '\\\"')
    } else if (grepl("v10.4", requests_df[i, 10])) {
      version_txt <- paste0("\\\"app_version\\\":\\\"", "V10", '\\\"')
    } else if (grepl("v10.14", requests_df[i, 10])) {
      version_txt <- paste0("\\\"app_version\\\":\\\"", "V10.14", '\\\"')
    } else{
      version_txt <- paste0("\\\"app_version\\\":\\\"", "V10.11", '\\\"')
    }
    
    
    call_fields <-
      paste(name_txt,
            os_txt,
            device_txt,
            langue_txt,
            product_txt,
            version_txt,
            sep = ",")
    url_prop <-
      paste0(curl_prop, call_fields, curl_x, requests_df[i, 1])
    shell(url_prop)
    
    url_note <- 
      paste0(curl_note, requests_df[i, 12], curl_y, requests_df[i, 1], "/notes")
    shell(url_note)
    
  }
}
