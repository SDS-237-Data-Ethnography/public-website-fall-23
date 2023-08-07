print_calendar <- function(major_due_dates, annotation_due_dates, m) {

  major_due_dates_m <- major_due_dates %>% filter(month == m)
  annotation_due_dates_m <- annotation_due_dates %>% filter(month == m)
  
  print(calendR(year = 2023, 
          month = m,       # Year and month
          start = "M",     # Start the week on Sunday
          text = major_due_dates_m$assignment, 
          text.pos = major_due_dates_m$day,       # Days of the month where to put the texts 
          text.size = 2,               # Font size of the text
          text.col = 1,
          special.days = annotation_due_dates_m$day,
          special.col = "lightblue",
          title.size = 10,
          font.family = "sans",             # Font family of all the texts                 
          weeknames = c("M", "T", "W", "T", "F", "S", "S")))

}