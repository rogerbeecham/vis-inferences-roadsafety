Data processing
================

## Introduction

This document describes download and processing activities for the main
STATS19 crash data that underpins our data analysis.

Please cite:

Beecham, R. and Lovelace, R. *A framework for inserting
visually-supported inferences into geographical analysis workflow:
application to road crash analysis*. DOI: []().

## Setup

### Required libraries

If you do not have them, the required packages can be installed
individually with `install.packages(<package_name>)`. Core packages are
imported into the session with `library(<package_name>)`. Occasional use
of packages is made with the `<package-name>::<function-name>()` syntax
so as to avoid polluting the workspace.

``` r
pkgs <- c("tidyverse","sf","stats19", "trafficalmr", "here")
# If not already installed.
# install.packages(pkgs)
# Core packages
library(tidyverse)              # Bundle of packages for data manipulation. 
library(sf)                     # For working with geospatial data.
```

## Download data

Pre-cleaned crash data between 2009-2019 are downloaded using functions
from the [`stats19`](https://docs.ropensci.org/stats19/) R package and
vehicle types and speed limit category variables recoded using functions
from [`trafficcalmr`]().

``` r
# Download
years <- 2009:2019
crashes_all <- stats19::get_stats19(year=years, type="accident")
casualties_all <- stats19::get_stats19(year=years, type="Casualty")
vehicles_all <-  stats19::get_stats19(year=years, type="Vehicle")     
# Recode
casualties_all <- casualties_all %>% 
  mutate(casualty_type=trafficalmr::tc_recode_casualties(casualty_type))
vehicles_all <- vehicles_all %>% 
  mutate(vehicle_type=trafficalmr::tc_recode_vehicle_type(vehicle_type))
crashes_all <- crashes_all %>% 
  mutate(speed_limit=trafficalmr::tc_recode_speeds_uk(speed_limit))
```

## Data processing

Our analysis focuses on pedestrian casualties and we are interested in
both the vehicles and drivers involved (as well as the crash
characteristics). The three tables – `crashes`, `vehicles` and
`casualties` – must be linked, with some simplifications and assumptions
made. Some pedestrian crashes involve several vehicles, but ideally we
would like to associate a single `vehicle` with a single pedestrian
`casualty`. Our approach here is that for pedestrian crashes involving
several vehicles, we find the largest vehicle type involved and assign
this as *the* vehicle involved. However, it is also possible to have
several vehicles of the same type involved in a crash. Within the
`vehicles_all` table is a `vehicle reference` variable, which takes
values from *1* to *n* distinguishing each distinct vehicle involved in
the crash of a particular type. We assume that `vehicle reference`*=1*
is the principal vehicle of that type involved in the crash. So single
vehicles are linked to single casualties based first on the largest
vehicle involved and then on `vehicle reference`. This is demonstrated
below.

``` r
# Identify all pedestrian casualties.
ped_veh <- crashes_all %>%
  left_join(casualties_all %>%
              select(
                accident_index, age_of_casualty,sex_of_casualty, casualty_type,
                casualty_imd_decile, casualty_severity, pedestrian_location,
                pedestrian_movement, casualty_reference)
            )  %>%
  filter(casualty_type=="Pedestrian")
# Create an ordered factor of vehicle types.
vehicles_all %>% select(vehicle_type) %>% distinct() %>% pull()
# Manually set order, filter on largest vehicle and then vehicle_reference.
veh_orders <- c("Bicycle", "Motorcycle","Car", "Taxi", "Other", "Van", "Bus", "HGV")e
temp_ped_veh <- vehicles_all %>%
  semi_join(ped_veh) %>%
  mutate(vehicle_type=factor(vehicle_type, levels = veh_orders, ordered = TRUE)) %>%
  group_by(accident_index) %>%
  mutate(largest_vehicle = max(vehicle_type)) %>%
  filter(vehicle_type==largest_vehicle) %>%
  mutate(unique_vehicle=min(vehicle_reference)) %>%
  filter(vehicle_reference==min(vehicle_reference))

ped_veh <- ped_veh %>% left_join(temp_ped_veh)
rm(temp_ped_veh)
```

We serialize and write the main crash data file out as an
[`.fst`](https://www.fstpackage.org/) file for future use.

``` r
# Write out as .fst file to data folder.
if(!dir.exists(here::here("data"))) dir.create(here::here("data"))
fst::write_fst(ped_veh, here::here("data", "ped_veh.fst"))
```
