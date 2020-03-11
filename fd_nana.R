#'This function will look for any requests with "NA" as first problem type
#'and will create a spreadsheet with the ticket number and the assignee.
#'The function should exclude the merged ticket, which have a specific tag.

NANA <- function(day, nana_qu){
  ## Libraries
  require(httr)
  require(stringr)
  require(magrittr)
  require(openxlsx)
  require(dplyr)
  require(purrr)
  apikey <- Sys.getenv("fd_api")
  
  agents_name <- c('Amit', 
                 'Andrea', 
                 'Carina', 
                 'Chris', 
                 'Emillia', 
                 'Michel',
                 'Nadja', 
                 'Rachid',
                 'Vera')
  
  agents_id <- c(19000532180, 
                 19000445734, 
                 19000562176, 
                 19012915923, 
                 19014387275, 
                 19000532181,
                 19000532183, 
                 19000532182,
                 19011097494)
  
  agents_email <- c('amit_gujral@trimble.com', 
                      'andrea_piccioni@trimble.com', 
                      'carina_pamminger@trimble.com', 
                      'chris_goodwin@trimble.com', 
                      'emillia_nlandu@trimble.com', 
                      'michel_diomande@trimble.com',
                      'nadja_mbomson@trimble.com', 
                      'rachid_boukerche@trimble.com',
                      'vera_boteva@trimble.com')
  
  agents <- as.data.frame(cbind(agents_id,agents_name,agents_email), stringsAsFactors = FALSE)
  
  ini_curl <-
    "https://alk.freshdesk.com/api/v2/search/tickets?"
  query_curl <- "query=\"group_id:19000105695%20AND%20(status:4%20OR%20status:5)%20AND%20created_at:>%27"
  fin_curl <- "%27\"&page="
  page_num <- 1
  full_curl <- paste0(ini_curl, query_curl, day, fin_curl, page_num)
  req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
  
  req_content <- content(req)  ## storing the content of the call
  tckt_list <- req_content[[1]]
  
  rev_descr <- length(req_content[[1]])
  while (rev_descr == 30) {
    page_num <- page_num + 1
    full_curl <- paste0(ini_curl, query_curl, day, fin_curl, page_num)
    req <- GET(full_curl, authenticate(apikey, "X", type = "basic"))
    req_content <- content(req)
    tckt_list <- append(tckt_list, req_content[[1]])
    rev_descr <- length(req_content[[1]])
  }
  cess <- tckt_list%>%map_lgl(is.list) ## to resolve the 301 line
  ## from here it changes to get the more details
  descr_txt <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(descr_txt) <- c(
    "Ticket",
    "Subject",
    "P_type",
    "Assignee",
    "Tags"
  )

  
  for (i in seq_along(which(cess=='TRUE'))) {
    if(class(tckt_list[i][[1]][['responder_id']]) == 'numeric'){
      descr_txt[i, 1] <- tckt_list[i][[1]][['id']]
      descr_txt[i, 2] <- tckt_list[i][[1]][['subject']]
      if(length(tckt_list[i][[1]][['custom_fields']][['main_problem_type']]) > 0){
        descr_txt[i, 4] <- 
          agents$agents_name[agents$agents_id==tckt_list[i][[1]][['responder_id']]]
        ##descr_txt[i, 4] <- tckt_list[i][[1]][['responder_id']]
        descr_txt[i, 3] <- tckt_list[i][[1]][['custom_fields']][['main_problem_type']]
      }else{descr_txt[i, 3] <- ''} 
      if(length(tckt_list[i][[1]][['tags']]) > 0){
        descr_txt[i, 5] <- tckt_list[i][[1]][['tags']]
      }
    }
  }
  
  na_tbl <- descr_txt %>%
    filter(P_type == "NA") %>%
    filter(!grepl("Merged", Tags)) %>%
    filter(!grepl("merged", Tags)) 
  
  rich_num <- nrow(na_tbl)
  nana_mess <- paste0("Found",' ', rich_num,' ', "requests missing the Problem Type. Do you want to notify the assignees (Y/N)?")
  nana_qu <- readline(nana_mess)
  if(toupper(nana_qu) == 'Y'){
    # adding a note
    curl_start <- 'curl -v -u z6L822MqF9kZ2K3D4J8c:X -H "Content-Type: application/json" -X POST -d "{\\\"body\\\":\\\"'
    curl_msg <- "Please, select the Problem type and the Issue type that apply. Thank you! "
    curl_x <- '\\\"}" https://alk.freshdesk.com/api/v2/tickets/'
    curl_end<- "/notes"
    
    
    for(j in seq_along(na_tbl)){
      ticket_id <- na_tbl[j, 1]
      user_id <- na_tbl[j, 2]
      url <-
        paste0(curl_start, curl_msg, curl_x, ticket_id, curl_end)
      
      shell(url)
    }
    print(na_tbl)
  }else if(toupper(nana_qu) == 'N'){
    # provide table only
    print(na_tbl)
  }
}
  
 
  
