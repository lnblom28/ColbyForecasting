
read_observations = function(scientificname = "Phocoena phocoena",
                             minimum_year = 1970, individual_count,
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
    #  filter(!is.na(individualCount), !is.na(eventDate), !is.na(year))
    if(individual_count >= 1)
    {
      x = x |>
        filter(individualCount <= individual_count)
      
      x = x |>
        filter(!is.na(individualCount))
      print("a")
    }
  
  return(x)
}
