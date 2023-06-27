#' Title
#'
#' @param study
#'
#' @return
#' @export
#'
#' @examples
use_cases <- function(study){

  return(switch(study,
        polarix1 = polarix1(),
        polarix2 = polarix2(),
        MonarchE = MonarchE(),
        Leda = Leda()))

}


# Theoretical design
polarix1 <- function(){

  events <- c(134, 178)
  thresh1 <- exp(monitOS::bounds(events = events))
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=c(thresh1$lhr_alt[1],
                    1 - 0.5 * (1 -exp(thresh1$lhr_alt[1])),
                    0,
                    1.1,
                    1.5,
                    2)))

}

# Actual trial
polarix2 <- function(){

  events <- c(110, 125, 131)
  thresh1 <- exp(monitOS::bounds(events = events))
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=c(thresh1$lhr_alt[1],
                    1 - 0.5 * (1 -exp(thresh1$lhr_alt[1])),
                    0,
                    1.1,
                    1.5,
                    2)))

}


MonarchE <- function(){

  events <- c(76, 186, 330, 650)
  thresh1 <- exp(monitOS::bounds(events = events))
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=c(thresh1$lhr_alt[1],
                    1 - 0.5 * (1 -exp(thresh1$lhr_alt[1])),
                    0,
                    1.1,
                    1.5,
                    2)))

}


Leda <- function(){

  events <- c(22, 34)
  thresh1 <- exp(monitOS::bounds(events =events))
  return(list(thresh1=thresh1$lhr_con,
              events=events,
              hrs=c(thresh1$lhr_alt[1],
                    1 - 0.5 * (1 -exp(thresh1$lhr_alt[1])),
                    0,
                    1.1,
                    1.5,
                    2)))

}
