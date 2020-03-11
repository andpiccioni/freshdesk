

  ## Libraries
  require(httr)
  require(stringr)
  require(magrittr)
  require(openxlsx)
  require(dplyr)
  apikey <- Sys.getenv("fd_api")
  
  # Features Request
  Feat_Req <- "https://alk.freshdesk.com/api/v2/discussions/forums/19000126774/topics?page="
  page_num <- 1
  feats_curl <- paste0(Feat_Req, page_num)
  feats_req <- GET(feats_curl, authenticate(apikey, "X", type = "basic"))
  feats_content <- content(feats_req)  ## storing the list of topics
  feats_list <- feats_content[[1]]
  
  feats_descr <- length(feats_content)
  while (feats_descr == 30) {
    page_num <- page_num + 1
    feats_curl <- paste0(Feat_Req, page_num)
    feats_req <- GET(feats_curl, authenticate(apikey, "X", type = "basic"))
    feats_content <- content(feats_req)  ## storing the list of topics
    feats_list <- append(feats_list, feats_content[[1]])
    feats_descr <- length(feats_content)
  }
  
  # Tips and Tricks
  T_n_T <- "https://alk.freshdesk.com/api/v2/discussions/forums/19000126775/topics"
  tips_req <- GET(T_n_T, authenticate(apikey, "X", type = "basic"))
  tips_content <- content(tips_req)  ## storing the list of topics
  tiptop_list <- tips_content[[1]]
  
  
  
  # Features Request + Tips and Tricks
  forum_topics <- append(feats_list, tiptop_list)
  
  ## from here it changes to get the more details
  forum_stats <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(forum_stats) <- c(
    "Id",
    "Title",
    "Votes",
    "Comments",
    "Hits"
  )
  cess <- seq(1, length(forum_topics), by = 16)
  
  for (j in 1:6) {
    
    i <- j*16
    forum_stats[i, 1] <- forum_topics[[i]]
    forum_stats[i, 2] <- forum_topics[[i+1]]
    forum_stats[i, 3] <- forum_topics[[i+8]]
    forum_stats[i, 4] <- forum_topics[[i+10]]
    forum_stats[i, 5] <- forum_topics[[i+15]]
  }
  
  na_tbl <- descr_txt %>%
    filter(P_type == "NA") %>%
    filter(!grepl("Merged", Tags))
  
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
 
  
