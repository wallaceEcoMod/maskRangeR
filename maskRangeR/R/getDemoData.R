#==================================================================================
#==================================================================================
#' @title Downloads biolocial data to use in vignette
#'
#' @description Canned datasets of real case studies for examples to follow along with the vignette
#' @details The function will download the data from a github repository as a zipped folder and will unzip it in the destination path specifies by 'path'
#' See Examples.
#' @param path character string representing the file pathway
# @examples
#'
#'
# @return
#' @author Peter Galante <pgalante@@amnh.org>,
#' @author Cecina Babich Morrow <cbabichmorrow@@amnh.org>,
#' @note 
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export


getDemoData <- function(path){
  download.file("https://github.com/Pgalante/WorkingCode/raw/master/demoData.zip",destfile = paste0(path, "/test.zip"))
  unzip(paste0(path, "/test.zip"))
}

path = "/home/pgalante/Projects/Garbage"


getDemoData(path = path)
