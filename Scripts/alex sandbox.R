# This function is for the creation of a standard project folder

library(lsp)

UserDir <- "C:/Users/User"
path <- "Documents/Primer_test"
n <- "test_3"

proj_primer <- function(UserDir = NULL, proj_path = NULL, proj_name = NULL) {

  lsp_files <- paste(UserDir, "Documents/packages/lsp/Report Template/", sep = "/")

  project_path <- paste(UserDir, proj_path, proj_name, sep = "/")
  print(project_path)
  if (dir.exists(project_path)) {
    stop("This project has already been created.")
  }

  #Create the project_path
  dir.create(project_path)

  #Create the subdirectory for the css folder
  projpath_css <- paste(project_path, "css", sep = "/")
  dir.create(projpath_css)

  #Copy the YSIcss.css file from the lsp project to the css directory
  file.copy(from = paste(lsp_files, "YSIcss.css", sep = "/"), to = projpath_css, overwrite = FALSE)

  #Create the subdirectory for the data folder
  projpath_data <- paste(project_path, "data", sep = "/")
  dir.create(projpath_data)

  #Create the subdirectory for the extdata folder
  projpath_extdata <- paste(project_path, "extdata", sep = "/")
  dir.create(projpath_extdata)

  #Create the subdirectory for the imgs folder
  projpath_imgs <- paste(project_path, "imgs", sep = "/")
  dir.create(projpath_imgs)

  #Copy the YSI.png file from the lsp project to the imgs directory
  file.copy(from = paste(lsp_files, "YSI.png", sep = "/"), to = projpath_imgs, overwrite = FALSE)

  #Create the subdirectory for the literature folder
  projpath_literature <- paste(project_path, "literature", sep = "/")
  dir.create(projpath_literature)


}


proj_primer(UserDir, path, n)

