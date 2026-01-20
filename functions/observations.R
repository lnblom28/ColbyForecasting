
read_observations = function(scientificname = "Phocoena phocoena",
                             minimum_year = 1970, individual_count = 10000,
                             remove_missing = c("eventDate", "individualCount", "year", "month"),
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = read_obis(scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb))
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }

   # x = x |>
    if(individual_count >= 1)
    {
      x = x |>
        filter(individualCount <= individual_count)
      
      x = x |>
        filter(!is.na(individualCount))
    }
    
  
  if ("eventDate" %in% remove_missing){
    x = x |>
      filter(!is.na(eventDate))
  }
  
  if ("individualCount" %in% remove_missing){
    x = x |>
      filter(!is.na(individualCount))
  }
  if ("year" %in% remove_missing){
    x = x |>
      filter(!is.na(year))
  }
  if ("month" %in% remove_missing){
    x = x |>
      filter(!is.na(month))
  }
   
  return(x)
}
