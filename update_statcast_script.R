#### daily update of Statcast database

# load required packages

require(pacman)
p_load(DBI, dplyr, magrittr, RSQLite, RMySQL, ggplot2, reshape2, scales)

# update using custom function

update_statcast <- readRDS("/Users/williampetti/statcast_database/update_statcast_function.RDS")

update_statcast()
save.image()  

#################

require(pacman)
p_load(DBI, dplyr, magrittr, RSQLite, RMySQL)

statcast_db <- src_sqlite("statcast.db.sqlite3", create = FALSE)

pitchfx <- statcast_db %>% 
  tbl("pitchfx") %>%
  collect()

statcast <- statcast_db %>% 
  tbl("statcast") %>%
  collect()

joined <- left_join(pitchfx, statcast, by = c("pitch_id", "game_pk", "game_date", "batter", "pitcher"))

joined_hit <- filter(joined, type.x == "X")

joined_hit <- mutate(joined_hit, angle_range = cut(hit_angle, breaks = seq(-90, 90, by = 10)), exit_velo_range = cut(hit_speed, breaks = seq(0, 130, by = 5)))

joined_hit <- filter(joined_hit, !events %in% c("Bunt Lineout", "Sac Bunt", "Bunt Pop Out", "Sacrifice Bunt DP", "Bunt Groundout"), Position != "P")

joined_hit_null <- filter(joined_hit, hit_angle == 0, hit_speed == 0, hit_distance_sc == 0)

joined_hit <- setdiff(joined_hit, joined_hit_null)

angle_table_2015 <- with(filter(joined_hit, game_year.x == "2015"), table(angle_range)) %>% prop.table() %>% round(3)

angle_table_2016 <- with(filter(joined_hit, game_year.x == "2016"), table(angle_range)) %>% prop.table() %>% round(3)

compare <- as.data.frame(cbind(angle_table_2016, angle_table_2015))
compare$angle_range <- row.names(compare)
names(compare) <- c("Launch Angle 2016", "Launch Angle 2015", "angle_range")
write.csv(compare, "compare.csv", row.names = TRUE)

compare_melt <- melt(compare)
compare_melt$angle_range <- gsub("\\[", "(", compare_melt$angle_range)
compare_melt$angle_range <- gsub("\\]", ")", compare_melt$angle_range)
compare_melt$angle_range <- as.factor(compare_melt$angle_range)
# names(compare) <- c("2016", "2015", "angle_range")
range_level <- c("(-90,-80)", "(-80,-70)", "(-70,-60)", "(-60,-50)", "(-50,-40)", "(-40,-30)", "(-30,-20)", "(-20,-10)", "(-10,0)", "(0,10)", "(10,20)", "(20,30)", "(30,40)", "(40,50)", "(50,60)", "(60,70)", "(70,80)", "(80,90)") 
# levels(compare_melt$angle_range) <- range_level
compare_melt$angle_range <- factor(compare_melt$angle_range, levels = range_level)


# clip <- pipe("pbcopy", "w")                       
# write.table(compare, file = clip, row.names = TRUE)           
# close(clip)

tab_condensed <- c("#006BA4", "#C85200")

ggplot(joined_hit, aes(hit_angle, ..density.., group = as.factor(game_year.x))) + geom_density(aes(fill = as.factor(game_year.x)), adjust = 2.5, alpha = .5) + scale_fill_manual(values = tab_condensed) + theme_bp_grey()

ggplot(compare_melt, aes(angle_range, value, group = as.factor(variable))) + geom_bar(aes(fill = as.factor(variable)), position = "dodge", stat = "identity", alpha = .60) + geom_text(aes(label = paste(value*100, "%")), fontface = "bold", position=position_dodge(width=0.9), vjust=-0.25, size = 3.5) + scale_fill_manual(values = tab_condensed, "Season") + scale_y_continuous(labels=percent) + xlab("\nLaunch Angle Range in Degrees") + ylab("\nFrequency of Launch Angle Range\n") + ggtitle("Frequency of Launch Angle on Batted Balls by Season\n") + theme_bp_grey() + theme(legend.position = "bottom")

ggsave("launch_angle_fre_yr.png", scale = 1.2, width = 14, height = 8.5, units = "in")

# subset to 95+ mph

joined_hit_95 <- filter(joined_hit, start_speed >= 95)

angle_table_2015_95 <- with(filter(joined_hit_95, game_year.x == "2015"), table(angle_range)) %>% prop.table() %>% round(3)

angle_table_2016_95 <- with(filter(joined_hit_95, game_year.x == "2016"), table(angle_range)) %>% prop.table() %>% round(3)

compare_95 <- as.data.frame(cbind(angle_table_2016_95, angle_table_2015_95))
compare_95$angle_range <- row.names(compare_95)
names(compare_95) <- c("Launch Angle 2016", "Launch Angle 2015", "angle_range")

compare_melt_95 <- melt(compare_95)
compare_melt_95$angle_range <- gsub("\\[", "(", compare_melt_95$angle_range)
compare_melt_95$angle_range <- gsub("\\]", ")", compare_melt_95$angle_range)
compare_melt_95$angle_range <- as.factor(compare_melt_95$angle_range)
# names(compare) <- c("2016", "2015", "angle_range")
range_level <- c("(-90,-80)", "(-80,-70)", "(-70,-60)", "(-60,-50)", "(-50,-40)", "(-40,-30)", "(-30,-20)", "(-20,-10)", "(-10,0)", "(0,10)", "(10,20)", "(20,30)", "(30,40)", "(40,50)", "(50,60)", "(60,70)", "(70,80)", "(80,90)") 
# levels(compare_melt$angle_range) <- range_level
compare_melt_95$angle_range <- factor(compare_melt_95$angle_range, levels = range_level)

ggplot(compare_melt_95, aes(angle_range, value, group = as.factor(variable))) + geom_bar(aes(fill = as.factor(variable)), position = "dodge", stat = "identity", alpha = .60) + geom_text(aes(label = paste(value*100, "%")), fontface = "bold", position=position_dodge(width=0.9), vjust=-0.25, size = 3.5) + scale_fill_manual(values = tab_condensed, "Season") + scale_y_continuous(labels=percent) + xlab("\nLaunch Angle Range in Degrees") + ylab("\nFrequency of Launch Angle Range\n") + ggtitle("\nFrequency of Launch Angle on Batted Balls by Season (>= 95mph Exit Velo)\n") + theme_bp_grey() + theme(legend.position = "bottom")

ggsave("launch_angle_fre_yr_95.png", scale = 1.2, width = 14, height = 8.5, units = "in")

##### control for hitters in both years, with minimum number of batted balls (450)

batters <- joined_hit %>% group_by(batter, game_year.x) %>% summarise(count = n()) %>% dcast(., batter ~ game_year.x, value.var = "count")
batters_min <- filter(batters, `2016` >= 100, `2015` >= 300)
joined_hit_conseq <- filter(joined_hit, batter %in% batters_min$batter)

angle_table_2015_conseq <- with(filter(joined_hit_conseq, game_year.x == "2015"), table(angle_range)) %>% prop.table() %>% round(3)

angle_table_2016_conseq <- with(filter(joined_hit_conseq, game_year.x == "2016"), table(angle_range)) %>% prop.table() %>% round(3)

compare_conseq <- as.data.frame(cbind(angle_table_2016_conseq, angle_table_2015_conseq))
compare_conseq$angle_range <- row.names(compare_conseq)
names(compare_conseq) <- c("Launch Angle 2016", "Launch Angle 2015", "angle_range")
write.csv(compare_conseq, "compare_consec.csv")
anova_yr_batter <- aov(hit_angle~factor(game_year.x), data = joined_hit_conseq)
summary(anova_yr_batter)
joined_hit_conseq %>% group_by(game_year.x) %>% summarise(mean(hit_angle))

compare_melt_conseq <- melt(compare_conseq)
compare_melt_conseq$angle_range <- gsub("\\[", "(", compare_melt_conseq$angle_range)
compare_melt_conseq$angle_range <- gsub("\\]", ")", compare_melt_conseq$angle_range)
compare_melt_conseq$angle_range <- as.factor(compare_melt_conseq$angle_range)
# names(compare) <- c("2016", "2015", "angle_range")
range_level <- c("(-90,-80)", "(-80,-70)", "(-70,-60)", "(-60,-50)", "(-50,-40)", "(-40,-30)", "(-30,-20)", "(-20,-10)", "(-10,0)", "(0,10)", "(10,20)", "(20,30)", "(30,40)", "(40,50)", "(50,60)", "(60,70)", "(70,80)", "(80,90)") 
# levels(compare_melt$angle_range) <- range_level
compare_melt_conseq$angle_range <- factor(compare_melt_conseq$angle_range, levels = range_level)

ggplot(compare_melt_conseq, aes(angle_range, value, group = as.factor(variable))) + geom_bar(aes(fill = as.factor(variable)), position = "dodge", stat = "identity", alpha = .60) + geom_text(aes(label = paste(value*100, "%")), fontface = "bold", position=position_dodge(width=0.9), vjust=-0.25, size = 3.5) + scale_fill_manual(values = tab_condensed, "Season") + scale_y_continuous(labels=percent) + xlab("\nLaunch Angle Range in Degrees") + ylab("\nFrequency of Launch Angle Range\n") + ggtitle("\nFrequency of Launch Angle on Batted Balls by Season (>= 95mph Exit Velo)\n") + theme_bp_grey() + theme(legend.position = "bottom")

ggsave("launch_angle_fre_yr_consec.png", scale = 1.2, width = 14, height = 8.5, units = "in")



require(pacman)
p_load(DBI, dplyr, magrittr, RSQLite, RMySQL)

source("https://gist.githubusercontent.com/BillPetti/cf1e082b5e580b3b7209/raw/28b62dddfd1e494b2cf9d10c4ef79d68e6033a42/variable_list.R")

# create SQLite database

statcast_db <- src_sqlite("statcast.db.sqlite3", create = TRUE)

# load FanGraphs database authorization 

fg_database_values <- c("/Users/williampetti/bpAuthsCons/fg_dbname.rda", "/Users/williampetti/bpAuthsCons/fg_username.rda", "/Users/williampetti/bpAuthsCons/fg_password.rda", "/Users/williampetti/bpAuthsCons/fg_host.rda")

lapply(fg_database_values, load, .GlobalEnv)

# pull Statcast data from FanGraphs database through 2016-06-30

con <- src_mysql(dbname = fg_dbname, user = fg_username, password = fg_password, host = fg_host, port = 3306)

savant_data <- con %>% 
  tbl("gd_savant") %>% 
  filter(game_date < "2016-07-03") %>%
  left_join(tbl(con, "playerid_lookup"), by = c("batter" = "mlbamid")) %>%
  left_join(tbl(con, "player_info"), by = "playerid") %>%
  collect()

gc()

savant_data_vars <- variable_list(savant_data)

savant_data <- savant_data %>%
  arrange(game_date) %>%
  as.data.frame()



