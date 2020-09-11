

load(file = "prep_data/TWmap_raw_data.RData")
usethis::use_data(dict_admin, overwrite = T)
usethis::use_data(bord_nation, overwrite = T)
usethis::use_data(bord_county, overwrite = T)
usethis::use_data(bord_region, overwrite = T)
usethis::use_data(bord_town, overwrite = T)
usethis::use_data(bord_urban, overwrite = T)
usethis::use_data(bord_seg, overwrite = T)


moh2moi <- structure(dict_admin$town_moi, names = dict_admin$town_moh)

ind <- !duplicated(dict_admin$city_moi)
moi2reg <- structure(dict_admin$region[ind], names = dict_admin$city_moi[ind])


require(dplyr)
pop <- readRDS("prep_data/TBDC_population_template.RDS")
TWpop_town <- pop %>%
  mutate(
    age = purrr::map(strsplit(AgeGroup, ","), 1) %>% substr(2,3) %>% as.integer(),
    sex = ifelse(Sex == "male" , 1, 0),
    town = moh2moi[Town],
    year= Year
  ) %>%
  select(age, sex, town, year, population)%>%
  group_by(age, sex, town, year)%>%
  summarise(population = sum(population)) %>%
  ungroup()


TWpop_city <- TWpop_town%>%
  mutate(city = substr(town, 1, 5))%>%
  group_by(age, sex, city, year)%>%
  summarise(population = sum(population)) %>%
  ungroup()

TWpop_region <- TWpop_city%>%
  mutate(region = moi2reg[city])%>%
  group_by(age, sex, region, year)%>%
  summarise(population = sum(population)) %>%
  ungroup()

all(!is.na(TWpop_region$population))

usethis::use_data(TWpop_town, overwrite = T)
usethis::use_data(TWpop_city, overwrite = T)
usethis::use_data(TWpop_region, overwrite = T)


### cause outline

dict_cause <- read.csv("prep_data/cause_outline_to_name_chn.csv", stringsAsFactors = F)

colnames(dict_cause)[7] <- "cause_name_ch"
usethis::use_data(dict_cause, overwrite = T)



####
load(file = "raw-data/TWmap_demo_data.RData")
attri_death_pm25 <- df[,c(7,1:6)]

usethis::use_data(attri_death_pm25, overwrite = T)


daly_B01 <- read.csv("raw-data/DALY_B01.csv", stringsAsFactors = F)%>%
  filter(year == 2015)
daly_B01 <- daly_B01 %>%
  mutate(cause_l3 = substr(cause_l4, 1,7),
         town = formatC(town, width = 8, format = "d", flag = "0")) %>%
  select(age, sex, town, year, cause_l3, yld_count,yll_count,daly_count, population)
daly_B01 <- daly_B01 %>%
  group_by(sex, town, year, cause_l3)%>%
  summarise(daly_count = sum(daly_count),
            yld_count = sum(yld_count),
            yll_count = sum(yll_count),
            population = sum(population))%>%
  ungroup()
usethis::use_data(daly_B01, overwrite = T)
