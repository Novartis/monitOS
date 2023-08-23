#' Title
#'
#' @param study TBD
#'
#' @return
#' @export
#'
#' @examples
use_cases <- function(study){

  return(switch(study,
        Polarix1 = polarix1(),
        Polarix2 = polarix2(),
        MonarchE = MonarchE(),
        Leda = Leda(),
        YTB323 = YTB323()))

}


# Theoretical design
polarix1 <- function(){

  events <- c(134, 178)
  bounds <- monitOS::bounds(events = events)
  return(list(thres1=exp(bounds$lhr_con),
              events=events,
              thres2=NULL,
              hrs=c(0.7, 1, 1.1, 1.5, 2)))

}

# Actual trial
polarix2 <- function(){

  events <- c(110, 125, 131)
  bounds <- monitOS::bounds(events = events)
  return(list(thres1=exp(bounds$lhr_con),
              events=events,
              thres2=NULL,
              hrs=c(0.7, 1, 1.1, 1.5, 2)))

}


MonarchE <- function(){

  events <- c(76, 186, 330, 650)
  bounds <- monitOS::bounds(events =events)
  return(list(thres1=exp(bounds$lhr_con),
              thres2=NULL,
              events=events,
              hrs=c(0.7, 1, 1.1, 1.5, 2)))

}


Leda <- function(){

  events <- c(22, 34)
  bounds <- monitOS::bounds(events =events, delta_imax = log(1.333))
  return(list(thres1=exp(bounds$lhr_con),
              thres2=NULL,
              events=events,
              hrs=c(0.5, 0.7, 1, 1.1, 1.5, 2)))

}

YTB323 <- function(){

  events <- c(36, 52)
  bounds <- monitOS::bounds(events =events, delta_imax = log(1.5))
  return(list(thres1=exp(bounds$lhr_con),
              thres2=NULL,
              events=events,
              hrs=c(0.5, 0.7, 1, 1.1, 1.5, 2)))

}
