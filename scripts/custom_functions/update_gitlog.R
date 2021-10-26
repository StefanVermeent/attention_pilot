update_gitlog <- function(repo_url) {
  
  library(tidyverse)
  library(openxlsx)
  
  
  # Create Updated Git log
  log_format_options <- c(datetime = "cd", commit = "h", author = "an", subject = "s")
  option_delim <- "\t"
  log_format   <- glue("%{log_format_options}") %>% glue_collapse(option_delim)
  log_options  <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S"')
  log_cmd      <- glue(str_c('git -C ', here(), ' log {log_options}'))
  log_cmd
  
  gitlog <- system(log_cmd, intern = TRUE) %>%
    str_split_fixed(option_delim, length(log_format_options)) %>%
    as_tibble() %>%
    mutate(Milestone = "") %>%
    setNames(c("Date", "Commit", "Author", "Description", "Milestone")) %>%
    select(Milestone, everything()) %>%
    add_row(.before = 1, Milestone = "Preregistration 1 Pilot Study", Date = "323434", Commit = "d3kk2", Description = "Fake commit for prereg") %>%
    add_row(.before = 20, Milestone = "Deviation from Preregistration 1", Date = "323434", Commit = "j6jh6", Description = "Fake commit for prereg2") %>%
    add_row(.before = 47, Milestone = "Accepted at Nature", Date = "323434", Commit = "dgaffae", Description = "Accepted Manuscript!!1!") %>%
    mutate(
      Commit = glue(str_c(
        "HYPERLINK(\"",
        str_c(repo_url, "/tree/", "{Commit}"),
        "\", \"",
        "{Commit}",
        "\")"
      ))
    )
  
  
  # Identify rows that contain Milestones (e.g., (deviations from) Preregistrations, major manuscript updates, etc.)
  # All other rows containing intermediate commits are flagged to be collapsed in the xlsx document.
  milestone_rows = which(gitlog$Milestone != "")
  
  rows_to_collapse = list()
  
  for (i in 1:length(milestone_rows)) {
    
    if (i == length(milestone_rows)) {
      rows_to_collapse[[i]] <- seq(milestone_rows[i]+2, nrow(gitlog)+1, 1)
    } else {
      
      this_milestone = milestone_rows[i]
      next_milestone = milestone_rows[i+1]
      
      rows_to_collapse[[i]] <- seq(this_milestone+2, next_milestone, 1)
    }
  }
  
  class(gitlog$Commit) <- "formula"
  
  # Create and write workbook
  wb <- createWorkbook()
  
  addWorksheet(wb, "Overview")
  
  setColWidths(wb, 1, cols = c(1,2,3,4), widths = c(40, 19, 50, 50))
  
  hyperlink <- createStyle(textDecoration = "underline", fontColour = "blue")
  
  # Collapse commits under each Project Milestone
  for (groups in rows_to_collapse) {
    groupRows(wb=wb, sheet=1, rows=groups, hidden=TRUE)
  }
  
  # Style hyperlinks
  addStyle(wb=wb, sheet=1, style=hyperlink, rows = 2:nrow(gitlog), cols=1)
  addStyle(wb=wb, sheet=1, style=hyperlink, rows = 2:nrow(gitlog), cols=3)
  
  writeData(wb, sheet = 1, gitlog)
  
  saveWorkbook(wb, "Preregistrations_and_Milestones.xlsx", TRUE)
  
}