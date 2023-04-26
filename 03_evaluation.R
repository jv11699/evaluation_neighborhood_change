library(ggridges)
library(scales)
library(ggspatial)
library(plyr)
library(StatRank)
library(Hmisc)
source("util.R")


### data stats
filt <- fread("filtered_data.csv")
filt[, date:= ymd_hms(`Actual Date`)]
filt[, amount := parse_number(consideration)]
filt[, year := year(date)]

filt <- filt[year(date) >= 2000 | year(date) > 2019]

sum(filt$`doc type`%in% c('D1A','DEED','D1B','D1BU'))/nrow(filt)

filt <- filt[`doc type`%in% c('D1A','DEED','D1B','D1BU')]
nrow(filt[amount <=5])/nrow(filt)
filt <- filt[amount > 5]
nrow(filt)

buff.city <- filter(places(state = "NY", 
                           year =2020, cb = TRUE), NAME == "Buffalo")
buff_blocks <- st_join(blocks(state = "NY", county = "Erie", year = 2020), buff.city, 
                               join = st_within, left=FALSE)
BLOCK_MEAN_SIZE <- as.double(mean(st_area(buff_blocks))/(1000*1000))
BG_MEAN_SIZE <- as.double(mean(st_area(get_buffalo_census(2019)$geometry))/(1000*1000))
TRACT_MEAN_SIZE <- as.double(mean(st_area(get_buffalo_census(2019,"tract")$geometry))/(1000*1000))

CELL_AREA_AXIS_LABEL <- expression(Cell~Area~{"("}~a^2~{")"}~{" in Sq. KM"})
buffalo_acs_2019 <- get_buffalo_census(2019)

p1 <- ggplot() + annotation_map_tile(zoomin = -1)  + geom_sf(data=buffalo_acs_2019, aes(fill =tpopr), 
                         color=NA,alpha=.6)  +
  scale_fill_viridis_c("Total\nPopulation", option = "plasma") + 
  theme_map(15) 
p2 <- ggplot() + annotation_map_tile(zoomin = -1)+ geom_sf(data=buffalo_acs_2019, aes(fill =nhwhite/tpopr), 
                                                           color=NA,alpha=.8)  +
  scale_fill_viridis_c("Percent\nWhite", option = "plasma",labels=percent) + 
  theme_map(15) 

#setnames(buffalo_acs_2019, "B19013_001", "inc")
p3 <- ggplot() + annotation_map_tile(zoomin = -1) + geom_sf(data=buffalo_acs_2019, aes(fill =log(inc)), 
                                                           color=NA,alpha=.8)  +
  scale_fill_viridis_c("Log(Median\nIncome)", option = "plasma",labels=trans_format("exp",label_dollar(drop0trailing = TRUE,accuracy=100))) + 
  theme_map(15) 
ggsave("img/fig1.pdf",
       grid.arrange(p1,p2,p3,nrow=1),
       h=6,w=15)


cell_size_map = data.frame()
for(x in c(10,12,16,20,24,28,32,36,40,44,48,60)){
  print(x)
  results_geo <- get_geo_res(x)
  cell_size_map <- rbind(cell_size_map,
                         data.frame(cell_size=x,
                                    a_2=st_area(results_geo$poly[1])/(1000*1000)))
}


# load in final data
res <-rbindlist(lapply(Sys.glob("results/res_*"),fread),fill=T)
res <- merge(res, cell_size_map)
res[, cell_size_fact := factor(round(a_2, 2), sort(unique(round(a_2, 2))))]

res[, window_size_lab := factor(window_size, levels=c("-year","-2year","-3year"),
                                labels=c("1 Year", "2 Years", "3 Years"))]
p_y <- ggplot(res, aes(x = y, y = cell_size_fact,fill=window_size_lab)) + 
  geom_density_ridges(alpha=.3) +
  scale_x_continuous("Percent of Homes Sold", limits = c(0, .6),labels=percent) +
  scale_y_discrete(CELL_AREA_AXIS_LABEL, 
                   expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  scale_fill_discrete("Prediction\nWindow\nSize") +
  theme_ridges( center_axis_labels = TRUE)


  
p_homecount <- ggplot(res, aes(x = `target:numberofhomessold`, y = cell_size_fact,fill=window_size_lab)) + 
  geom_density_ridges(alpha=.3) +
  scale_x_continuous("Number of Homes Sold", limits = c(0, 75)) +
  scale_y_discrete(CELL_AREA_AXIS_LABEL, expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  scale_fill_discrete("Prediction\nWindow\nSize") +
  theme_ridges( center_axis_labels = TRUE)
ggsave("img/fig2.pdf",
       grid.arrange(p_homecount,p_y,nrow=1),
       h=6,w=12)

res[, median(`target:numberofhomessold`), by=cell_size_fact]
res[, mean(y), by=cell_size_fact]
res[, sum(y==0)/.N, by=cell_size_fact]


######### GEO ###############
results_geo <- get_geo_res(32)
p4 <- ggplot()  + annotation_map_tile(zoomin = -1) + 
  geom_sf(data=results_geo[results_geo$window_size == "-year" & results_geo$year > 2006,], 
          aes(fill =ifelse(y_std > .5, "Top", ifelse(y_std < -.5, "Bottom","Bottom"))), 
          color=NA,alpha=.8) + scale_fill_manual(values=c("white","red")) + 
  facet_wrap(~year,nrow=3) +theme_map(15)+ theme(legend.position="none")
ggsave("img/fig3.pdf",
       p4,
       h=8,w=8)




####### RESULTS ############

model_res <- grep("^pred_",names(res),value = T)

# continuous preds
ml =melt(res,
         id = c("cell_id","year","cell_size_fact","y","y_std",
                "y_binary","total_number_of_houses","window_size"), 
         measure= model_res)

ml[, mod_name := mapvalues(variable, names(model_names_list), as.vector(model_names_list))]

outcome_data <- ml[!grepl("std",variable) & !grepl("class",variable),
                   list(rmse= sqrt(mean((y-value)^2)),
                        ndcg= Evaluation.NDCG(1:.N, .SD[order(value)]$y),
                        rankcor = cor(value,y, method="kendall")
                   ),
                   by=.(cell_size_fact,mod_name,year, window_size)]
setnames(outcome_data, c("rmse","ndcg","rankcor"),
         c("RMSE","NDCG","Kendall's Tau"))
outcome_data[, window_size := mapvalues(window_size, c("-year",'-2year','-3year'), c("1 Year", "2 Years","3 Years"))]

p_res1 <- ggplot(melt(outcome_data, id= c("cell_size_fact","mod_name","window_size"),
                      measure=c("RMSE","NDCG","Kendall's Tau")), 
                 aes(as.double(as.character(cell_size_fact)), value,color=mod_name,fill=mod_name))+
  stat_smooth(method='gam') +
  facet_grid(variable~window_size,scales="free_y") + 
  labs(x=CELL_AREA_AXIS_LABEL) +
  geom_vline(xintercept = TRACT_MEAN_SIZE, color="black", linetype="dotted",size=1.2) +
  geom_vline(xintercept = BLOCK_MEAN_SIZE, color="black", linetype="dotted",size=1.2) +
  geom_vline(xintercept = BG_MEAN_SIZE, color="black", linetype="dotted",size=1.2) +
  scale_color_discrete("Model") +
  scale_fill_discrete("Model") +
  theme(legend.position = "bottom") + 
  guides(color=guide_legend(nrow=2,byrow=TRUE))
p_res1
ggsave("img/pres1.pdf",p_res1, h=7,w=10)

outcome_data[, mod_name_fin:= factor(mod_name,
                                     levels=rev(c("Random Forest Regression",
                                              "Baseline: Previous Year's Value",
                                              "Baseline: Avg. All Years",
                                              "Baseline: Citywide Average")))]
p_res2 <- ggplot(melt(outcome_data, 
                      id= c("cell_size_fact","mod_name_fin","window_size"),
                      measure=c("RMSE","NDCG","Kendall's Tau"))[, 
                              as.list(smean.cl.boot(value)), 
                              by=.(variable,mod_name_fin)],
                 aes(mod_name_fin,Mean,ymin=Lower,ymax=Upper,
                     color=ifelse(mod_name_fin == "Random Forest Regression","red","black"))) + 
  geom_pointrange() + coord_flip() +
  labs(y="Mean Value of Metric",x="Model") + 
  facet_wrap(~variable, scales="free_x",nrow=1)+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  scale_color_manual(guide="none", values=c("black","red")); p_res2
ggsave("img/pres2.pdf",p_res2, h=3,w=10)



### ERROR ANALYSIS

error_analysis_data <- gen_error_analysis()

error_analysis_data[, window_size_lab := factor(timespan, levels=c("-year","-2year","-3year"),
                             labels=c("1 Year", "2 Years", "3 Years"))]

error_analysis_data[, mod_name := ifelse(mod == "m2","Outcome:\n% Homes Sold",
                                         "Outcome:\nPrediction Error")]

error_analysis_data[, var_name := mapvalues(var, c("prop_hisp","log_inc","scaled_y","prop_blk"), c("% Hispanic residents",
                                            "Log(Mean Income)",
                                            "% Homes Sold",
                                            "% Black residents")) ]
error_analysis_data <- merge(error_analysis_data,cell_size_map, by.x="spatial",by.y="cell_size")

error_plt <- ggplot(error_analysis_data[mod == "m1"], 
       aes(spatial,val,color=window_size_lab, fill=window_size_lab)) +
  facet_wrap(~var_name,scales="free_y") + 
  geom_hline(yintercept = 0, size=1.2,linetype="dashed") + 
  stat_smooth(method="gam") +
  scale_x_continuous(CELL_AREA_AXIS_LABEL) + 
  scale_color_discrete("Prediction\nWindow\nSize") +
  scale_fill_discrete("Prediction\nWindow\nSize") +
  ylab("Regression Coefficient Magnitude")
ggsave("img/error_plt.pdf",error_plt,h=6,w=8)




### MODEL EXPLORATION 
shap <- fread("df_with_shap.csv")
library(ggbeeswarm)
### First Shap plot

mt <- melt(shap[, names(shap)[grepl("shap_",names(shap))],with=F])
top <-  as.character(mt[,mean(abs(value)),by=variable][order(-V1)][1:10]$variable)
top_df <- data.table()
for(t in top){
  top_df <- rbind(top_df,
                  shap[, c(t,sub("shap_","",t)),
                       with=F][,v := t],
                  use.names=F
  )
}
setnames(top_df, c("shap","feature_val","v"))

factor_names = c("shap_spatiallag-order:6_1yearsago_type:percentages" = "% Sold (Avg.\n6 Nearest Cells,\nLast Year)",
                 "shap_value-2yearsago_type:median" = "Median Price\n(This cell, 2 years ago)",
                 "shap_total_number_of_houses" = "Total N\n(This Cell)",
                 "shap_value-1yearsago_type:transactions" = "N Transactions\n(Last Year, This Cell)",
                 "shap_spatiallag-order:3_1yearsago_type:transactions" = "N Transactions\n(Last Year, Avg. 3 Nearest Cells)",
                 "shap_value-1yearsago_type:percentages" = "% Sold\n(This Cell, Last Year)",
                 "shap_lisacategory_1yearsago_type:percentages" = "LISA Value % Sold\n(Last Year)",
                 "shap_value-3yearsago_type:median" = "Median Price (This cell, 3 Years Ago)",
                 "shap_value-1yearsago_type:median" = "Median Price (This cell, Last Year)",
                 "shap_spatiallag-order:27_3yearsago_type:transactions" = "N Transactions\n(Avg. of 27 Nearest Cells, 3 years ago")
top_df[, name := mapvalues(v, names(factor_names), as.vector(factor_names))]
top_df[, name := factor(name, levels=as.vector(factor_names))]

library(scales)

top_df[, rescaled_feature := rescale(feature_val,c(0,1)), by=name]


p_res <- ggplot(top_df[, as.data.table(sample_n(.SD, 10000)), by=name], 
                aes(rev(name), shap,color=rescaled_feature)) + 
  geom_quasirandom(varwidth=T) + 
  scale_color_gradientn("Feature Value\n(Rescaled 0-1)",
                        colors=c("blue","grey","orange", "red")) + 
  theme(legend.position="none") +
  ylab("SHAP Value")+
  xlab("Top 25 Most Important Predictors")+
  theme_classic(14)+
  theme(axis.text.x=element_blank())+
  coord_flip() +
  geom_hline(yintercept = 0, color='black',size=1.2)
ggsave("img/shaply.jpg",p_res,h=6.,w=7.)
