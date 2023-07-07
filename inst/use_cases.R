#' Specify case studies for shiny app.
#'
#' @param study String. Provide study name.
#'
use_cases <- function(study){

  return(switch(study,
        Polarix1 = polarix1(),
        Polarix2 = polarix2(),
        MonarchE = MonarchE(),
        Leda = Leda(),
        YTB323 = YTB()))

}


# Theoretical design
polarix1 <- function(){

  events <- c(134, 178)
  thresh1 <- exp(monitOS::bounds(events = events))
  hrs <- c(0.5, 0.7, 1, 1.1, 1.5, 2)
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=hrs))

}

# Actual trial
polarix2 <- function(){

  events <- c(110, 125, 131)
  thresh1 <- exp(monitOS::bounds(events = events))
  hrs <- c(0.5, 0.7, 1, 1.1, 1.5, 2)
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=hrs))

}


MonarchE <- function(){

  events <- c(76, 186, 330, 650)
  thresh1 <- exp(monitOS::bounds(events = events))
  hrs <- c(0.5, 0.7, 1, 1.1, 1.5, 2)
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=hrs))

}


Leda <- function(){

  events <- c(22, 34)
  thresh1 <- exp(monitOS::bounds(events =events))
  hrs <- c(0.5, 0.7, 1, 1.1, 1.5, 2)
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=hrs))

}

YTB <- function(){

  events <- c(36, 52)
  thresh1 <- exp(monitOS::bounds(events = events,
                                 delta_imax = log(1.5)))
  hrs <- c(0.5, 0.7, 1, 1.1, 1.5, 2)
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=hrs))

}
