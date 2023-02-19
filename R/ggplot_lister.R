#' Function for creating sequential (or branching) ggplots
#'
#' For lectures or presentations, it can often be helpful to show a sequential series of plots.
#' A common case: show the raw data, show the raw data and the fitted model, then
#' show the raw data and the fitted model with an overlay of some sort of additional information.
#' This process can become somewhat tedious for complex buildups, even with the relative streamlining
#' ggplot provides. This function streamlines that process, and `ggplot_list_saver`
#' further streamlines the workflow by also automatically saving the generated figures.
#'
#' @param gglist A list of ggplot layers, augmented with whole numbers to specify commands. First item in the list must be a `ggplot()` call.
#' `0` specifies printing, negative integers specify removing the previous `n` layers before continuing. Each item MUST be either a `ggplot()` call, ggplot layer, `0`, or negative whole number.
#' @param niceties A single ggplot layer or a list of ggplot layers that should be applied to all plots, and
#' are applied last. Common use case would be a list of axis labels, plot title, and theme. Defaults to NULL; in this case, no layers added.
#' Note that this formulation is merely for convenience -- these layers can also be specified in gglist, but would generally need to be added before the first `0` entry.
#'
#' @details The core functionality comes from the `gglist` argument. Layers are added sequentially; whenever
#' a 0 is encountered in the list, `ggplot_lister()` saves the plot up to that point as a new entry in the output list. In some
#' cases, it can be useful to backtrack ("Okay, but what if we fit the data with a smoothing spline instead of a linear regression?").
#' Rather than make separate calls to `ggplot_lister()`, you can backtrack by adding negative integers to the list.
#' Adding negative value `n` removes the last `n` layers in the list before continuing.
#'
#' For figure sets without backtracking, a simple workflow is to develop the final figure with all
#' components before using `ggplot_lister()`. Then simply copy the `ggplot()` call and the entire
#' collection of layers into a list for the `gglist` argument, replacing `+` with commas. Move any purely cosmetic
#' layers (axis labels, themes, etc) to a separate list for `niceties`. Then, in the `gglist` argument, add new `0`
#' entries to the list after each layer for which you want a new plot.
#'
#' @return List of ggplots, in order of creation.
#' @export
#' @import ggplot2
#'
#' @examples
#' library(tidyverse)
#' dat = data.frame(y = runif(30), x = runif(30))
#' out = ggplot_lister(gglist = list(ggplot(data = dat, aes(x = x, y = y)),
#'                                   geom_point(), 0,
#'                                   geom_path(),0, -1, geom_smooth(method = 'lm')),
#'                     niceties = list(xlab("turtle"), ylab("cow")))
#'
#' out = ggplot_lister(gglist = list(ggplot(data = dat, aes(x = x, y = y)),
#'                                   geom_point(), 0,
#'                                   geom_path(),0,
#'                                   geom_point(aes(x = x+.2), col = 'blue')),
#'                     niceties = list(xlab("turtle"), ylab("cow")))

ggplot_lister = function(gglist,
                         niceties = NULL #
){
  gglist.cur = gglist
  #For convenience, presuming that the final layer added should be
  #followed by a zero
  if(!is.numeric(gglist.cur[[length(gglist.cur)]])){
    gglist.cur[[length(gglist.cur)+1]] = 0
  }
  ## need to pull out initial ggplot call, since we have to
  ## have that separate from the list
  plot.init = gglist.cur[[1]]
  gglist.cur = gglist.cur[-1]


  ind.instru = which(sapply(gglist.cur, is.numeric))
  instru = do.call(c, gglist.cur[ind.instru])
  stopifnot(all(is.numeric(instru)),
            all(instru <= 0))
  list.plots = NULL

  for(i in 1:length(ind.instru)){
    cur.ind = min(which(sapply(gglist.cur, is.numeric)))
    cur.instru = gglist.cur[[cur.ind]]
    if(cur.instru == 0){
      list.plots[[length(list.plots)+1]]=
        plot.init + gglist.cur[1:(cur.ind-1)] + niceties
      gglist.cur = gglist.cur[-cur.ind]
    }else{
      gglist.cur = gglist.cur[-((cur.ind + cur.instru):cur.ind)]
    }
  }
  return(list.plots)
}

#' Create and save sequential (or branching) ggplot figures
#'
#' @inheritParams ggplot_lister
#' @param base.name Name for plots (numbers will be added) without filetype.
#' @param filetype suffix for file type, including period. Default is ".jpg"
#' @param save.path Path to save files to. Default is current working directory.
#' @param suffix Optional suffix to separate base name from plot numbers. Default is "-V". Set to "" to ignore.
#' @param ... Additional arguments passed to ggsave. Likely choices: width, height, units, dpi. See `?ggsave` for details.
#'
#' @return Logical
#' @export
#'
#' @examples
#'
#' library(tidyverse)
#' dat = data.frame(y = runif(30), x = runif(30))
#' # ggplot_list_saver(gglist = list(ggplot(data = dat, aes(x = x, y = y)),
#' #   geom_point(), 0,
#' #   geom_path(),0, -1, geom_smooth(method = 'lm')),
#' #   niceties = list(xlab("turtle"), ylab("cow"), theme_bw()),
#' #   base.name = "testfig",
#' #   save.path = "."
#' # )

ggplot_list_saver = function(gglist,
                             base.name,
                             filetype = ".jpg",
                             save.path = ".",
                             suffix = "-V",
                             niceties = NULL,
                             ...
){
  fig.list = ggplot_lister(gglist, niceties)
  for(i in 1:length(fig.list)){
    ggsave(filename = paste0(base.name, suffix,
                             sprintf("%03d",i), filetype),
           fig.list[[i]],
           path = save.path,
           ...)
  }
  return(TRUE)
}

