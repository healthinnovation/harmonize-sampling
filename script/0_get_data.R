library(tidyverse)
library(sf)
library(ggspatial)
library(rgee)
library(innovar)
ee_Initialize(user = "antony.barja@upch.pe")
cp <- st_read("../raw_data/villages.gpkg",layer = "cp_5km") %>%
  select(IDCCPP_17,NOMCCPP_17) %>% 
  rename(
    codigo = IDCCPP_17,
    village = NOMCCPP_17
  )

cp_ee <- cp %>% 
  st_transform(32718) %>%
  st_buffer(dist = 5000) %>%
  st_transform(4326) %>%
  sf_as_ee()

# Precipiation -----------------
pp <- get_climate(
  from = "2021-01-01",
  to = "2021-12-31",
  by = "year",
  band = "pr",
  fun = "mean",
  region = cp_ee)

# Runoff  ----------------------
ro <- get_climate(
  from = "2021-01-01",
  to = "2021-12-31",
  by = "year",
  band = "ro",
  fun = "mean",
  region = cp_ee)

# Soil moisture ---------------
soil <- get_climate(
  from = "2021-01-01",
  to = "2021-12-31",
  by = "year",
  band = "soil",
  fun = "mean",
  region = cp_ee)

# Tmmx -------------------------
tmax <- get_climate(
  from = "2021-01-01",
  to = "2021-12-31",
  by = "year",
  band = "tmmx",
  fun = "mean",
  region = cp_ee)

# Tmmn -------------------------
tmin <- get_climate(
  from = "2021-01-01",
  to = "2021-12-31",
  by = "year",
  band = "tmmn",
  fun = "mean",
  region = cp_ee)

# Global Human Modification ------
ghm <- get_ghm(region = cp_ee,fun = "mean")

# Evapotranspiration -------------
etp <- get_etp(
  from = "2021-01-01",
  to = "2021-12-31",
  band = "ET",
  region = cp_ee,
  fun = "mean"
)
etp <- etp |> 
  pivot_longer(!c("codigo","village"),names_to = "year",values_to = "ETP") |> 
  mutate(year = gsub("ET","",year) %>% substr(.,1,4) %>% as.numeric()) |> 
  group_by(codigo,village) |> 
  summarise(ETP = mean(ETP))

# Humidity -----------------------
humidity <- get_fldas(
  from = "2021-01-01",
  to = "2021-12-31",
  by = "year",
  band = "Qair_f_tavg",
  region = cp_ee,
  fun = "mean")

# Population ----------------------
pop <- get_pop(
  from = "2020-01-01",
  to = "2020-12-31",
  region = cp_ee,
  fun = "mean"
)

# Modeling spatial data ---------------------------------------------------
m1 <- left_join(
  cp %>% select(codigo),
  etp,
  "codigo"
  )

m2 <- left_join(
  m1,
  ghm %>% select(-village),
  "codigo"
  )

m3 <- left_join(
  m2,
  humidity %>% select(-village),
  "codigo"
)

m4 <- left_join(
  m3,
  pop %>% select(-village),
  "codigo"
)

m5 <- left_join(
  m4,
  pp %>% select(-village),
  "codigo"
)

m6 <- left_join(
  m5,
  ro %>% select(-village),
  "codigo"
)

m7 <- left_join(
  m6,
  soil %>% select(-village),
  "codigo"
)

m8 <- left_join(
  m7,
  tmax %>% select(-village),
  "codigo"
)

dbtotal <- left_join(
  m8,
  tmin %>% select(-village),
  "codigo"
)

names(dbtotal) <- c(
  "codcp","village","etp",
  "ghm","humidity","pop","pp","runnof",
  "soil","tmmx","tmmn","geom")

write_csv(dbtotal %>% mutate(lat = st_coordinates(geom)[,2],lon = st_coordinates(geom)[,1]) %>% st_set_geometry(NULL),"../processed_data/db_variables.csv")
write_sf(dbtotal,"../processed_data/db_variables.gpkg")