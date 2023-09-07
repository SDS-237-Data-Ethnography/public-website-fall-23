print_readings <- function(perusall_link, reading_df, required_flag){
  reading_list <- c()

  reading_keys <- reading_df %>% 
    select(reading, chapter) %>%
    filter(!is.na(reading)) %>%
    mutate(chapter = ifelse(is.na(chapter), "", paste(chapter, ",")))
  
  if(nrow(reading_keys > 0)) {
    
    reading_list <- sapply(reading_keys$reading, function(x) {
      
      paste0(
        capture.output(print(bib[x], .opts = list(style = "markdown"))), 
        collapse = " ")
    })
    if(required_flag == TRUE) {
      cat(paste("", fa(name ="book"), " ", reading_keys$chapter, reading_list," [Read in Perusall](", perusall_link, ")"), sep = '  \n')
    }
    else {
      cat(paste("", fa(name ="book"), " ", reading_keys$chapter, reading_list), sep = '  \n')
    }
  }
  else{
  }
}

print_writing_assignments <- function(assignment_df){
  assignment_names <- assignment_df %>% 
    select(writing_assignment) %>% 
    drop_na() %>%
    pull()
  
  if(length(assignment_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="file-alt"), "   ", assignment_names), sep = "  \n")
  }
}

print_project_assignments <- function(assignment_df){
  assignment_names <- assignment_df %>% 
    select(project_assignment) %>% 
    drop_na() %>%
    pull()
  
  if(length(assignment_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="user-friends"), "   ", assignment_names), sep = "  \n")
  }
}

print_links <- function(links_df){
  links_names <- links_df %>% 
    select(link) %>% 
    drop_na() %>%
    pull()
  
  if(length(links_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="link"), "   ", links_names), sep = "  \n")
    cat("\n")
  }
}

print_codes <- function(codes_df){
  codes_names <- codes_df %>% 
    select(code) %>% 
    drop_na() %>%
    pull()
  
  if(length(codes_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="code"), "   ", codes_names), sep = "  \n")
    cat("\n")
  }
}

print_slides <- function(slides_df){
  slides_names <- slides_df %>% 
    select(slides) %>% 
    drop_na() %>%
    pull()
  
  if(length(slides_names > 0)) {
    cat("\n")
    cat(paste("", fa(name ="images"), "   ", slides_names), sep = "  \n")
    cat("\n")
  }
}

# print_announcements <- function(announcement_df){
#   announcement_names <- announcement_df %>% 
#     select(announcement) %>% 
#     drop_na() %>%
#     pull()
#   
#   if(length(announcement_names > 0)) {
#     cat("\n")
#     cat(paste("", fa(name ="bullhorn"), "   ", announcement_names), sep = "  \n")
#   }
# }

print_due_today <- function(perusall_link, topic){
  this_week_required_reading <- required_reading %>% filter(topic_abbr == topic) 
  this_week_project_assignments <- project_assignments %>% filter(topic_abbr == topic)
  this_week_writing_assignments <- writing_assignments %>% filter(topic_abbr == topic)
  print_readings(perusall_link, this_week_required_reading, required_flag = TRUE)
  print_project_assignments(this_week_project_assignments)
  print_writing_assignments(this_week_writing_assignments)
}

print_further_reading <- function(perusall_link, topic){
  this_week_slides <- slides %>% filter(topic_abbr == topic)
  print_slides(this_week_slides)
  this_week_codes <- codes %>% filter(topic_abbr == topic) 
  print_codes(this_week_codes)
  this_week_links <- links %>% filter(topic_abbr == topic) 
  print_links(this_week_links)
  this_week_optional_reading <- optional_reading %>% filter(topic_abbr == topic) 
  print_readings(perusall_link, this_week_optional_reading, required_flag = FALSE)
}

# print_today_announcements <- function(topic){
#   this_week_announcement <- announcements %>% filter(topic_abbr == topic) 
#   print_announcements(this_week_announcement)
# }