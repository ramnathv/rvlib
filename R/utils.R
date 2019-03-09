#' Get an HTML diff
#'
#' @export
#' @importFrom rvest html_node
#' @importFrom xml2 read_html
#' @importFrom htmltools tags tagList singleton HTML browsable
#' @examples
#' rv_get_diff("x <- 10", "x <- 20")
rv_get_diff <- function(x, y, as_char = FALSE, add_css = TRUE){
  if (is.na(x) || x == "" || is.na(y) || y == "" || x == y){
    return(tags$pre(tags$code()))
  }
  td <- tempdir()
  withr::with_dir(td, {
    cat(x, file = 'file1.txt')
    cat(y, file = 'file2.txt')
    my_diff <- system(
      'diff -u file1.txt file2.txt | diff2html -i stdin -o stdout',
      intern = TRUE
    )
    diff_html <- my_diff %>%
      paste(collapse = "\n") %>%
      read_html()

    diff_body <- diff_html %>%
      html_node('.d2h-file-diff') %>%
      as.character()

    css <- diff_html %>%
      html_node('style') %>%
      as.character()

    my_diff <- tagList(
      tags$div(HTML(diff_body)),
      if (add_css) singleton(HTML(css)) else NULL
    )
  })
  if (as_char){
    as.character(as.tags(my_diff))
  } else {
    browsable(my_diff)
  }
}
