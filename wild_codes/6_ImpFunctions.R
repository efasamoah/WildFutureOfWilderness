
gm_mean <- function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}

ntile_na <- function(x,n)
{
  notna <- !is.na(x)
  out <- rep(NA_real_,length(x))
  out[notna] <- ntile(x[notna],n)
  return(out)
}

theme_sleek <- function(base_size = 11, base_family = "") {
  half_line <- base_size/2
  theme_light(base_size = base_size, base_family = base_family) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.length = unit(half_line / 2.2, "pt"),
      strip.background = element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "grey30"),
      strip.text.y = element_text(colour = "grey30"),
      axis.text = element_text(colour = "black"),
      axis.title = element_text(colour = "black"),
      legend.title = element_text(colour = "grey30", size = rel(0.9)),
      panel.border = element_rect(fill = NA, colour = "grey70", size = 0.5),
      legend.key.size = unit(0.9, "lines"),
      legend.text = element_text(size = rel(0.7), colour = "grey30"),
      legend.key = element_rect(colour = NA, fill = NA),
      legend.background = element_rect(colour = NA, fill = NA),
      plot.title = element_text(colour = "grey30", size = rel(1)),
      plot.subtitle = element_text(colour = "grey30", size = rel(.85))
    )
}


# Import resTime function from VoCC package
resTime<-function(pg, vel, areapg = NA){
  RT <- suppressWarnings(data.table(ID = sp::getSpPPolygonsIDSlots(pg)))
  RT[, v := raster::extract(vel, pg, small=TRUE, fun=mean, na.rm=TRUE)]
  # If not provided, calculate the area of the polygon
  if(all(is.na(areapg))){
    areapg <- geosphere::areaPolygon(pg)/1000000              # area polygon in km2
  }
  RT[, d := 2*sqrt((areapg/pi))]
  RT[, resTim := abs(d/v)]
  return(RT[])
}
