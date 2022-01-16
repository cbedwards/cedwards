#' Project directory maker
#' 
#' This function creates my preferred directory structure and key files for new research projects
#' In addition to folders, this creates:
#'   - a blank datasources.txt file in the `1_raw_data`, to keep track of where data files come from
#'   - a blank funs.R file in `3_scripts`
#'   - a template file for recommended reviewers (`Reviewers.docx`) in `ms` -- this makes life easier when resubmitting
#'   - a template code map file (`Code map.txt`) for mapping out what files go where, with reminder text on how to generate a directory tree.
#' @param filepath For identifying the root directory of the new project. Leave as '.' if the current working directory is the new project.
#' @keywords organization
#' @export
#' @examples 
#' skeleton_maker("C:/repos/unicorn-pop-dynamics")
#' @import officer
#' @import dplyr

skeleton_maker = function(filepath='.'){
  ## folder structure
  dir.create((paste0(filepath, "/1_raw_data")))
  file.create((paste0(filepath, "/1_raw_data/datasources.txt"))) #file to track where the data files are coming from
  dir.create((paste0(filepath, "/2_data_wrangling")))
  dir.create((paste0(filepath, "/3_scripts")))
  dir.create((paste0(filepath, "/3_scripts/figures")))
  dir.create((paste0(filepath, "/3_scripts/simulations")))
  dir.create((paste0(filepath, "/3_scripts/archive")))
  file.create((paste0(filepath, "/3_scripts/funs.R"))) #file to store generic R functions
  dir.create((paste0(filepath, "/4_res")))
  dir.create((paste0(filepath, "/5_figs")))
  dir.create((paste0(filepath, "/ms")))
  
  ## create recommended reviewers template
  rev.template = officer::read_docx()
  rev.template = rev.template %>%  officer::body_add_par("Firstname")
  rev.template = rev.template %>%  officer::body_add_par("Lastname")
  rev.template = rev.template %>%  officer::body_add_par("Email")
  rev.template = rev.template %>%  officer::body_add_par("Institute")
  rev.template = rev.template %>%  officer::body_add_par("")
  rev.template = rev.template %>%  officer::body_add_par("Firstname")
  rev.template = rev.template %>%  officer::body_add_par("Lastname")
  rev.template = rev.template %>%  officer::body_add_par("Email")
  rev.template = rev.template %>%  officer::body_add_par("Institute")
  rev.template = rev.template %>%  officer::body_add_par("")
  rev.template = rev.template %>%  officer::body_add_par("Firstname")
  rev.template = rev.template %>%  officer::body_add_par("Lastname")
  rev.template = rev.template %>%  officer::body_add_par("Email")
  rev.template = rev.template %>%  officer::body_add_par("Institute")
  rev.template = rev.template %>%  officer::body_add_par("")
  rev.template = rev.template %>%  officer::body_add_par("Firstname")
  rev.template = rev.template %>%  officer::body_add_par("Lastname")
  rev.template = rev.template %>%  officer::body_add_par("Email")
  rev.template = rev.template %>%  officer::body_add_par("Institute")
  rev.template = rev.template %>%  officer::body_add_par("")
  rev.template = rev.template %>%  officer::body_add_par("Firstname")
  rev.template = rev.template %>%  officer::body_add_par("Lastname")
  rev.template = rev.template %>%  officer::body_add_par("Email")
  rev.template = rev.template %>%  officer::body_add_par("Institute")
  print(rev.template, (paste0(filepath, "/ms/Reviewers.docx")))
  
  ## create code map template
  cat("This directory contains the data and analysis scripts used in [XXXX]\n",
      file=(paste0(filepath, "/Code map.txt")),sep="\n")
  cat("To avoid any potential copyright issues, figures are not included. However, published figures were generated from the scripts in this directory (sometimes with tweaking in Adobe Illustrator afterwards)\n\n",file="Code map.txt",append=TRUE)
  cat("The key scripts are [list and describe]\n\n",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
  cat("Additional details\n\n",
      file=(paste0(filepath, "/Code map.txt")), append=TRUE)
  cat("Overview of results files, including description of columns\n\n",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
  cat("Directory Structure:",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
  cat("[To generate tree from windows machine, open commandline and navigate to root of project, then run\n",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
  cat("tree /F /A >tree.txt\n",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
  cat("e.g. this: https://gist.github.com/search?l=Batchfile&q=user%3Acbedwards\n",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
  cat("Note that some of the content in tree.txt should be trimmed - don't need to keep the structure of the project subfolder, for example.]\n",
      file=(paste0(filepath, "/Code map.txt")),append=TRUE)
}