
### 0. Installation
devtools::install_github("dachuwu/TWmap")
require(TWmap)
require(sf)
require(dplyr)

### 1. Preparation
View(dict_admin) # dictionary
View(attri_death_pm25) # demo dataset


moh2moi <- structure(dict_admin$town_moi, names = dict_admin$town_moh)
df <- attri_death_pm25 %>%
  mutate(town.moi = moh2moi[towncode_new])



### 1. county-specific measures (continuous scale)
df <- df %>%
  dplyr::mutate(county = substr(town.moi, 1, 5))%>%
  dplyr::group_by(county)%>%
  dplyr::summarise(xv = sum(all_att_death))%>%
  dplyr::ungroup()

TWmap::twmap_static(geo.code = df$county, x = df$xv,
                    geo.level = "county", x.name = "all_att_death")



### 2. county-specific measures (discrete scale)
df <- df%>%
  mutate(
    xv_d = cut(xv, breaks = seq(0, 700, 100))
  )

TWmap::twmap_static(geo.code = df$county, x = df$xv_d,
                    geo.level = "county", x.name = "all_att_death")


### 3. township-specific measures (continuous scale)
df <- daly_B01 %>%
  filter(sex == 0, cause_l3 == "B.01.07")%>%
  mutate(xv = 1E5*daly_count/population)

TWmap::twmap_static(geo.code = df$town, x = df$xv,
                    geo.level = "town", x.name = "YLD rate per 100,000")


### 3. township-specific measures (discrete scale)
df <- df %>%
  mutate(
    xv_d = cut(xv, breaks = c(0,  1000, 2000, 3000, 4000, Inf),
               labels = c("0~1000", "1000~2000", "2000~3000","3000~4000", "4000+"))
  )

TWmap::twmap_static(geo.code = df$town, x = df$xv_d,
                    geo.level = "town", x.name = "YLD rate per 100,000")



### 4. misc



