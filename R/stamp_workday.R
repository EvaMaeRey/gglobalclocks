create_day_schedule_df <- function(){
  
  data.frame(time_start = 
                                hms::as_hms(c("00:00:00","07:00:00", "09:00:00", "17:00:00","21:00:00")), 
                              time_end = 
                                hms::as_hms(c("07:00:00","09:00:00", "17:00:00", "21:00:00","24:00:00")),
                              stance = c("avoid","awake", "business","awake","avoid"))
}


stamp_workday <- function(show.legend = T){  
  
  ggplot2::geom_rect(data = create_day_schedule_df(),
            ggplot2::aes(xmin = time_start,
                xmax = time_end, 
                x = NULL,
                fill = stance,
                ymin = -Inf,
                ymax = Inf,
                y = NULL,
                color = NULL),
           alpha = .5, show.legend = show.legend) 
}
