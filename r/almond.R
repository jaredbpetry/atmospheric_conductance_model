#' Almond yield
#'
#' Function to calculate the maximum, minimum and mean Almond Yield anomaly for a input time series
#'
#' @param df The input to this function should be a dataframe that includes monthly aggregated data, with the minimum temperatures column named "tmin_c" and the precipitation column named "precip".
#'
#' @return Maximum, minimum and mean Almond Yield anomaly for a input time series (ton/acre)
#'
#' @export

yield_fun <- function(df){
  
  # Transform df variables into vectors
  Tmin <- df %>% 
    filter(month %in% 2) %>% 
    pull(tmin_c)
  P <- df %>% 
    filter(month %in% 1) %>% 
    pull(precip)
  
  # It may be necessary to filter the maximum temperature if other crops functions are included
  # Tmax <- df %>% 
  #   filter(month %in% 2) %>% 
  #   pull(tmax_c)
  
  # Compute almond yield based on the variables of interest
  almond_yield <- (-0.015*Tmin) - (0.0046*(Tmin^2)) - (0.07*P) + (0.0043*(P^2)) + 0.28
  
  # Return max, min and mean values
  return(data.frame(max = max(almond_yield),
                    min = round(min(almond_yield),3),
                    mean = mean(almond_yield)))
}