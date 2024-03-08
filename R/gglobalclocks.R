gglobalclocks <- function(data = NULL){
  
  ggplot2::ggplot(data) + 
  list(ggplot2::theme_void(),
       ggplot2::coord_polar(),
       ggplot2::scale_x_continuous(limits = c(0,1)),
       ggplot2::scale_y_continuous(limits = c(0,1.3)))
  
}
