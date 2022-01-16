#' Change Directory
#' 
#' Wrapper for setwd() using unix command form. 
#' 
#' @param filepath Filepath to set as current working directory.
#' @export
#' @keywords wrapper

cd = function(filepath){
  setwd(filepath)
}