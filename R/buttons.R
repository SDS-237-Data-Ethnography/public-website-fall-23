create_standards_button <- function(text, type, size){
  if(type == "literacy"){
    bs_button(toupper(text), "warning", size)
  }
  else if(type == "method"){
    bs_button(toupper(text), "success", size)
  }
  else if(type == "competency"){
    bs_button(toupper(text), "info", size)
  }
  else if(type == "critical-thinking"){
    bs_button(toupper(text), "primary", size)
  }
}

print_standards_buttons <- function(standards, size = "small"){
  
  if(nrow(standards) > 0){
    for (i in 1:nrow(standards)){
      cat(paste("", create_standards_button(standards$standard[i], 
                                            standards$type[i], size), " "), sep = " ")
    }
  }
}

print_this_week_standards_buttons <- function(topic_name){
  this_week_standards <- topic_standards %>% 
    filter(topic_abbr == topic_name) %>%
    select(standard_id, standard, type) %>%
    filter(!is.na(standard_id)) %>%
    arrange(standard_id)
  
  print_standards_buttons(this_week_standards)
  
}