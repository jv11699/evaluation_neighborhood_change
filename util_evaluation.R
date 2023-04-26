library(areal)
library(data.table)
library(ggbeeswarm)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(leaflet)
library(lubridate)
library(plotly)
library(readr)
library(rmapshaper)
library(sf)
library(stringr)
library(tidycensus)
library(tidyverse)
library(tigris)
library(ggspatial)
library(Hmisc)
library(tmap)
library(ggthemes)
library(spdep)
library(spatialreg)
library(dtplyr)
library(scales)
library(plyr)
library(stargazer)
theme_set(theme_bw(14))
options(tigris_use_cache = TRUE)

two_sd_scale <- function(d){
  return( (d-mean(d[!is.na(d)]))/(2*sd(d[!is.na(d)])))
}


YOUR_CENSUS_KEY = "PUT_YOUR_KEY_HERE"

model_names_list <-  c("pred_baseline_citywide"= "Baseline: Citywide Average", 
    "pred_rf_reg"= "Random Forest Regression", 
    "pred_rf_reg_std"= "Random Forest (Standardized Outcome)", 
    "pred_rf_class"= "Random Forest Classifier", 
    "pred_all_years"= "Baseline: Avg. All Years", 
    "pred_all_years_std"= "Baseline: Avg. All Years (Standardized)", 
    "pred_previous_year"= "Baseline: Previous Year's Value", 
    "pred_previous_year_std"= "Baseline: Previous Year's Value (Standardized)")


get_buffalo_census<- function(year, 
                              size ="block group", 
                              fn=get_acs,
                              vars=c(tpopr = "B03002_001", 
                                     nhwhite = "B03002_003", nhblk = "B03002_004", 
                                     nhasn = "B03002_006", hisp = "B03002_012",
                                     tothhs = "B11001_001",
                                     inc="B19013_001E",
                                     inc_total="B19025_001")){
  census_api_key(YOUR_CENSUS_KEY)
  ny.tracts <- fn(geography = size, 
                       year = year,
                       variables = vars, 
                       state = "NY",
                       county="Erie",
                       geometry = TRUE)
  buff.city <- filter(places(state = "NY", year = ifelse(year < 2011, 2011,year), cb = TRUE), NAME == "Buffalo")
  buff.metro.tracts.int <- st_join(ny.tracts, buff.city, 
                                   join = st_within, left=FALSE)
  metro_tracts_census <- spread(buff.metro.tracts.int[, 
                                                      c("geometry","estimate",
                                                        "GEOID.x","variable")], 
                                "variable","estimate")
  return(metro_tracts_census)
  
}


get_geo_res <- function(cell_size, year_val = -1){
  
  # look at results files
  res <-rbindlist(lapply(Sys.glob(paste0("results/res_*_",cell_size,"_20*")),fread),fill=T)
  if(year_val != -1){
    res <- res[year == year_val]
  }
  print(paste0("results/cellbounds_",cell_size,".csv"))
  shp <- fread(paste0("results/cellbounds_",cell_size,".csv"))
  shp[, cell_id := paste0(cell_x,",",cell_y)]
  

  results_shapefile <- merge(shp,res,by="cell_id")
  print(nrow(res))
  print(nrow(results_shapefile))
  lst_results <- lapply(1:nrow(results_shapefile), function(x){
    ## create a matrix of coordinates that also 'close' the polygon
    res <- matrix(c(results_shapefile$x0[x], results_shapefile$y0[x],
                    results_shapefile$x1[x], results_shapefile$y0[x],
                    results_shapefile$x1[x], results_shapefile$y1[x],
                    results_shapefile$x0[x], results_shapefile$y1[x],
                    results_shapefile$x0[x], results_shapefile$y0[x])  ## need to close the polygon
                  , ncol =2, byrow = T
    )
    ## create polygon objects
    st_polygon(list(res))
    
  })
  return(st_sf(results_shapefile,crs=3857, poly=st_sfc(lst_results)))
}


gen_error_analysis <- function(){
  if(file.exists("error_analysis_data.csv")){
    return(fread("error_analysis_data.csv"))
  }
  dr <- data.table()
  r <- unique(res$year)
  r <- r[r > 2012]
  for(year in r){
    buffalo_acs_data <- get_buffalo_census(year,size = ifelse(year > 2012, "block group","tract"))
    for(spatial in unique(res$cell_size)){
      final_results_geo_full <- get_geo_res(spatial,year)
      print(spatial)
      for(timespan in unique(res[year==year]$window_size)){
        print(timespan)
        final_results_geo <- final_results_geo_full[final_results_geo_full$window_size == timespan,]
        if(nrow(final_results_geo) == 0){
          next
        }
        vars <- c("hisp", "nhblk", "nhwhite","tpopr","inc_total")
        geo_census <- aw_interpolate(st_transform(final_results_geo,26915) , 
                                     tid = cell_id, 
                                     source = st_transform(buffalo_acs_data,26915), 
                                     sid = GEOID.x, 
                                     weight = "total", 
                                     output = "sf", 
                                     extensive = vars)
        geo_census$abs_err <- geo_census$pred_rf_reg - geo_census$y
        geo_census <- st_transform(geo_census,4326)
        seab<-poly2nb(geo_census, queen=T)
        seaw<-nb2listw(seab, style="W", zero.policy = TRUE)
        geo_census$prop_blk <- two_sd_scale(with(geo_census,nhblk/tpopr))
        geo_census$prop_hisp <- two_sd_scale(with(geo_census,hisp/tpopr))
        geo_census$log_inc <- two_sd_scale(with(geo_census,log(inc_total/tpopr)))
        geo_census$scaled_y <-  two_sd_scale(geo_census$y)
        m1 <- lagsarlm(abs_err~prop_blk+prop_hisp+prop_blk+log_inc+scaled_y, 
                       data=geo_census,
                       zero.policy = TRUE,
                       listw=seaw)
        
        m2 <- lagsarlm(y~prop_blk+prop_hisp+prop_blk+log_inc, 
                       data=geo_census,
                       zero.policy = TRUE,
                       listw=seaw)
        dr <- rbind(dr,
                    data.table(year=year,spatial=spatial,timespan=timespan,
                               var=names(coef(m1))[3:6],
                               val=coef(m1)[3:6], se=summary(m1)$rest.se[2:5],
                               mod="m1")
        )
        dr <- rbind(dr,
                    data.table(year=year,spatial=spatial,timespan=timespan,
                               var=names(coef(m2))[3:5],
                               val=coef(m2)[3:5], se=summary(m2)$rest.se[2:4],
                               mod="m2")
        )
      }
    }
  }
  write.csv(dr, "error_analysis_data.csv",row.names=F)
  return(dr)
}