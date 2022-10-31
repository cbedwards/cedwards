#' Change Directory
#'
#' Wrapper for setwd() using unix command form.
#'
#' @param filepath Filepath to set as current working directory.
#' @keywords wrapper
#' @export

cd = function(filepath){
  setwd(filepath)
}
