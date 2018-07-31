
#' Initiate New Project Folders
#'
#' @name proj_primer
#' @param UserDir Character vector of the user's primary drive dile pather, e.g C:/User/Users
#' @param proj_path Character vector of the directory where the new project will be housed, e.g Documents/R/Projects
#' @param proj_name Character vector of the name of the new project you are creating
#' @return Creates a new set of directories in the desired location with included YSIcss.css and YSI.png files
#' @export
#'

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
