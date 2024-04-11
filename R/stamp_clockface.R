stamp_clockface <- function(){
  
    list(ggplot2::geom_text(data = data.frame(x = 1:12, y = 1), 
            ggplot2::aes(label = x, x = x/12, y = y, xend = NULL, 
                yend = NULL, color = NULL, local_time_hm = NULL),
            show.legend = F),
          ggplot2::annotate(geom = "segment", x = 0, xend = 1, y = 1.2, yend = 1.2)
         
    )
  
}
