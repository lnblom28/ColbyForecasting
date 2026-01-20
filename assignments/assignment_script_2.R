source("setup.R")

buoys = gom_buoys()
coast = read_coastline()
db = brickman_database()

db = db |> 
  filter(scenario == "PRESENT", interval == "mon")
covars = read_brickman(db)

x = read_model_input(scientificname = "Phocoena phocoena")

result = x |> 
  group_by(month) |> 
  group_map(
    function(rows, key){
      first = slice(rows, 1)
      last = slice(rows, nrow(rows))
      r = bind_rows(first, last)
      cv = slice(covars, "month", rows$month[1])
      vals = extract_brickman(cv, r, form = "wide") |>
        select(-.id)
      #r = bind_cols(r, vals)
      return(vals)
    }, .keep = TRUE
  ) |>
  bind_rows()



#x = extract_brickman(covars, buoys, form = "wide")
#thinned_obs
#janP = obsbkg |> 
#  filter(class == "presence", month == "Jan")
#janB = obsbkg |> 
#  filter(class == "background", month == "Jan")


