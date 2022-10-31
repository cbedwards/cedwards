#' Saves ggplot and documentation
#'
#' Simplify plotting for larger projects by simultaneously saving the plot, documentation of the plot,
#'  and the ggplot object itself as an RDS file.
#'
#' @param filename Name of the file (can include filepath) WITHOUT FILETYPE. Function will automatically save pdf and jpg filetypes.
#' @param description Description of the figure generated to save as metadata.
#' It is useful to treat this as a prototype figure caption, but with any additional details on methods, specific parameters, etc.
#' @param gfig Figure, defaults to the last plot displayed.
#' @param from.description Optional additional description of where the figures were generated - the scripts and project.
#' @param is.knitted If this function is being used in a knitted document, the knitr package can automatically save the script name in the description. Default is `FALSE`.
#' @param width Width of the saved figure; default is 12 inches.
#' @param height Height of the saved figure; default is 8 inches.
#' @param res Resolution of the saved figure; default is 300 dpi.
#' @param units Units for height and with; default is inches. Options include "cm", "mm", and "px"
#' @param ... additional ggplot2 arguments
#'
#' @return none
#' @import ggplot2
#' @import knitr
#' @export
#'
#' @examples
#' library(ggplot2)
#' x= 1:10
#' y = x + rnorm(10)
#' dat = data.frame(x, y)
#' gp = ggplot(data = dat, aes(x=x, y=y))+
#'   geom_point()
#' gfig_saver(filename="testfig",
#'            description=c("This is an example figure of X = Y + noise",
#'                          "note that this is just for demonstration"),
#'            gfig = gp)


gfig_saver=function(filename, #name of figure file to save as WITHOUT SUFFIX
                    description, #vector of strings, each will be put in its own line of meta file
                    gfig = last_plot(),
                    from.description = NULL, #information on what generated this figure -- project name and script name.
                    is.knitted = FALSE, # UPDATE TO HANDLE THIS
                    ##  Note: generating file is defined in the function, date and time is automatically added.
                    ##default figure info:
                    width=12,
                    height=8,
                    res=300,
                    units="in",
                    ...
){
  ## save meta file
  if(is.knitted){from.description = c(from.description, paste("from", knitr::current_input())) }
  cat(c(description,
        "",
        from.description,
        "RDS file with same name contains ggplot object used to generate this figure.",
        as.character(Sys.time())),
      sep="\n",
      file=paste0(filename,"_meta.txt")
  )
  #save figure as jpg (change code here for other figure types)
  ggsave(filename=paste0(filename,".jpg"),
         plot=gfig,
         device="jpeg",
         dpi=res,
         width=width, height=height, units=units,
         ...
  )
  ggsave(filename=paste0(filename,".pdf"),
         plot=gfig,
         device="pdf",
         dpi=res,
         width=width, height=height, units=units,
         ...
  )
  #save ggplot object as RDS file, for easy manipulation later
  saveRDS(object = gfig, file=paste0(filename,".RDS"))
}
