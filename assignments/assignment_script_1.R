source("setup.R")

buoys = gom_buoys()
coast = read_coastline()
db = brickman_database()

buoys = buoys |> 
  filter(id == "M01")

db = db |> 
  filter(scenario == "RCP45", interval == "mon", year == "2055")
covars = read_brickman(db)
x = extract_brickman(covars, buoys, form = "wide")

ggplot(data = x,
       mapping = aes(x = month, y = SST)) +
  geom_point() + 
  labs(title = "Temp difference at buoy M01")

x = x |>
  mutate(month = factor(month, levels = month.abb))

ggplot(data = x,
       mapping = aes(x = month, y = SST)) +
  geom_point() + 
  labs(y = "SST (C)", 
       title = "RCP4.5 2055 SST at buoy M01")

ggsave("images/M01_SST.png")
