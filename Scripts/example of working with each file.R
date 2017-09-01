files <- list.files("examples") %>%
  map( ~ list(
    name = str_replace(.x, ".xls", ""),
    file_path =  glue("examples/{.x}")
  )) %>%
  set_names(map_chr(., "name"))

# do to each!



ls <- files %>%
  map( ~ process_sheet(.x))


x$working_tbl$Original

y <- pluck(x, "working_tbl", "Original")
# get names
