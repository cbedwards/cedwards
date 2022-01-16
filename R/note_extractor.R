#' Extract notes from R scripts
#'
#' Searches a text file (including R and Rmarkdown) for lines with a specific marker
#' (default, `'#note'`), and saves those lines to a new text file. Useful
#' for writing notes in a long analysis or simulation script. Example usecase:
#' my analysis script of 35+ species of butterflies -- writing notes for each species that has problematic fit diagnostics.
#' While this can be done in a separate file, leaving the notes in the script allows for natural updating.
#' note_extractor makes it easy to access these notes.
#'
#' @param infile Name of the file to search through for notes
#' @param outfile Name of the file notes will be saved to. Default is `NULL`, and in this case the outfile will be set to the infile name, but with an ending of `'-notes.txt`
#' @param marker String used to identify comment lines that have notes. Defaults to `#%note`.
#' @export


note_extractor = function(infile,
                          outfile = NULL,
                          marker = '#%note'){
  #define outfile based on infile, if outfile name isn't given
  if(is.null(outfile)){
    out = strsplit(test, "[.]")
    outfile = paste0(out[[1]][-length(out[[1]])],"-notes.txt")
  }
  con = file(infile, "r")
  char.store = c(paste0("Notes extracted from ", infile, "\n"))
  while ( TRUE ) {
    line = readLines(con, n = 1, skipNul = T)
    if ( length(line) == 0 ) {
      break
    }
    temp = (grep(paste0(marker, '.*$'), line , value = T))
    if(length(temp)>0){char.store=c(char.store,temp)}
  }
  close(con)
  char.clean = gsub(paste0('.*',marker),"",char.store)
  cat(char.clean, file=outfile, sep="\n\n")
}
