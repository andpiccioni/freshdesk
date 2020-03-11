#' This function add a tag to the tickets in FD.
#' Inputs are the tag and a fd_df.
#'

tagging <- function(tag, data_set){
  
  require(httr, quietly = TRUE)
  require(stringr, quietly = TRUE)
  require(magrittr, quietly = TRUE)
  require(openxlsx, quietly = TRUE)
  require(dplyr, quietly = TRUE)
  require(readr, quietly = TRUE)
  
  ## Set the different arguments or parameters:
  
  apikey <- Sys.getenv("fd_api") # constant
  # tag <- 'never_used'
  # data_set <- never_used

  ## Parts of the PUT
  for(i in 1:nrow(data_set)){
    ticket_id <- data_set$Ticket[i]
    main_problem_type <- data_set$`1st Problem Type`[i]
    specific_issue <- as.character(data_set$`1st Issue Type`[i])
    cf_2nd_problem_type <- data_set$`2nd Problem Type`[i]
    cf_2nd_specific_issue <- as.character(data_set$`2nd Issue Type`[i])
    mtags <- data_set$Tags[i]
    
    tags_a <- mtags %>%
      str_replace_all(",", '\\\\\\\",\\\\\\\"')
    
    tags <- paste('[\\\"', tags_a,'\\\",\\\"', tag, '\\\"]')
    
    curl_1 <- 'curl -v -u z6L822MqF9kZ2K3D4J8c:X -H \"Content-Type: application/json\" -X PUT -d "{\\\"tags\\\":'
    curl_2 <- ',\\\"custom_fields\\\":{\\\"main_problem_type\\\":\\\"'
    curl_3 <- '\\\",\\\"specific_issue\\\":\\\"'
    curl_4 <- '\\\",\\\"cf_2nd_problem_type\\\":\\\"'
    curl_5 <- '\\\",\\\"cf_2nd_specific_issue\\\":\\\"'
    curl_6 <- '\\\"}}" https://alk.freshdesk.com/api/v2/tickets/'
    
    url_prop <-
      paste0(curl_1, tags, curl_2, main_problem_type, curl_3, specific_issue, curl_4, cf_2nd_problem_type, curl_5, cf_2nd_specific_issue, curl_6, ticket_id)
    shell(url_prop)
  }
  
}

