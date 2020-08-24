

load(file = "raw-data/TWmap_raw_data.RData")


usethis::use_data(dict_admin, overwrite = T)
usethis::use_data(bord_nation, overwrite = T)
usethis::use_data(bord_county, overwrite = T)
usethis::use_data(bord_region, overwrite = T)
usethis::use_data(bord_town, overwrite = T)
usethis::use_data(bord_urban, overwrite = T)
usethis::use_data(bord_seg, overwrite = T)


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
