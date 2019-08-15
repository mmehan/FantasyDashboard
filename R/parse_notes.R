parse_notes <- function(filename) {
  lines <- readLines(filename)
  lines = lines[!(grepl("ago", lines) & nchar(lines) < 25)]
  starts <- grep("^[0-9]", lines)
  notes <- lapply(starts, function(i) {
    if (nchar(lines[i]) < 60 & nchar(lines[i+1]) > 60)
      data.frame(col1 = lines[i], Notes = lines[i+1], stringsAsFactors = F)
  }) %>% 
    bind_rows() %>% 
    extract(col1, c("trash", "Name", "Trash2"), "([0-9]*[.] *)(.*) - (.*)")  %>% 
    select(-contains("trash")) %>% 
    mutate(Name = str_trim(gsub("RB$|WR$|QB$|TE$", "", Name))) %>% 
    mutate(Notes = gsub('"', "", Notes)) %>% 
    rename(Player = Name)
  
  
  notes
}

# parse_notes("data/player notes.csv") %>% 
#   print