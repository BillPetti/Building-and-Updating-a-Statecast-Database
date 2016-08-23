#### create a database that updates daily with Statcast and PITCHf/x data, pulled from the FanGraphs database

## 8-23-2016

# load required packages

require(pacman)
p_load(DBI, dplyr, magrittr, RSQLite, RMySQL)

source("https://gist.githubusercontent.com/BillPetti/cf1e082b5e580b3b7209/raw/28b62dddfd1e494b2cf9d10c4ef79d68e6033a42/variable_list.R")

# create SQLite database

statcast_db <- src_sqlite("statcast.db.sqlite3", create = TRUE)

# load FanGraphs database authorization 

fg_database_values <- c("/Users/williampetti/bpAuthsCons/fg_dbname.rda", "/Users/williampetti/bpAuthsCons/fg_username.rda", "/Users/williampetti/bpAuthsCons/fg_password.rda", "/Users/williampetti/bpAuthsCons/fg_host.rda")

lapply(fg_database_values, load, .GlobalEnv)

# pull Statcast data from FanGraphs database through 2016-08-16

con <- src_mysql(dbname = fg_dbname, user = fg_username, password = fg_password, host = fg_host, port = 3306)

savant_data <- con %>% 
  tbl("gd_savant") %>% 
  filter(game_date < "2016-07-07") %>%
  left_join(tbl(con, "playerid_lookup"), by = c("batter" = "mlbamid")) %>%
  left_join(tbl(con, "player_info"), by = "playerid") %>%
  collect(n = Inf)

gc()

savant_data_vars <- variable_list(savant_data)

# savant_data$game_date %<>% as.Date("%Y-%m-%d")
# savant_data$BirthDate %<>% as.Date("%Y-%m-%d")
# savant_data$DeathDate %<>% as.Date("%Y-%m-%d")
# savant_data$LastGame %<>% as.Date("%Y-%m-%d")
# savant_data$Debut %<>% as.Date("%Y-%m-%d")

savant_data <- savant_data %>%
  arrange(game_date) %>%
  as.data.frame()

savant_data$hit_type <- with(savant_data, ifelse(type == "X" & grepl("Single", savant_data$events), 1,
                                                 ifelse(type == "X" & grepl("Double", savant_data$events), 2,
                                                        ifelse(type == "X" & grepl("Triple", savant_data$events), 3, 
                                                               ifelse(type == "X" & grepl("Home Run", savant_data$events), 4, NA)))))

savant_data$hit <- with(savant_data, ifelse(type == "X" & grepl("Single", savant_data$events), 1,
                                            ifelse(type == "X" & grepl("Double", savant_data$events), 1,
                                                   ifelse(type == "X" & grepl("Triple", savant_data$events), 1, 
                                                          ifelse(type == "X" & grepl("Home Run", savant_data$events), 1, NA)))))

savant_data$fieldingTeam <- with(savant_data, ifelse(inning_top_bottom == "bot", away_team, home_team))

savant_data_vars <- variable_list(savant_data)

pitchfx <- savant_data %>%
  select(-(hit_distance_sc:release_extension))

statcast <- savant_data %>%
  select(pitch_id, game_date, game_year, batter, pitcher, game_pk, type, hit_distance_sc:release_extension, hit, hit_type, home_team, fieldingTeam)

# copy data to database

dbWriteTable(statcast_db$con, "pitchfx", pitchfx)
dbWriteTable(statcast_db$con, "statcast", statcast)

rm(statcast_db)

# print latest date in data base

print_latest_date <- function() {
  statcast_db <- src_sqlite("statcast.db.sqlite3", create = FALSE)
  
  statcast_db %>% 
    tbl("statcast") %>% 
    select(game_date) %>% 
    distinct() %>% 
    collect() %>% 
    tail(n = 1)
}

print_latest_date()
  
update_statcast_db <- function() {
    setwd("/Users/williampetti/statcast_database")
    db <- src_sqlite("statcast.db.sqlite3", create = FALSE)
    lastdate <- db %>% tbl("pitchfx") %>% select(game_date) %>% distinct() %>% collect(n = Inf)
    lastdate <- lastdate %>% tail(n = 1)
    lastdate <- paste0(lastdate[1,1])
    
    rm(db)
    
    fg_database_values <- c("/Users/williampetti/bpAuthsCons/fg_dbname.rda", "/Users/williampetti/bpAuthsCons/fg_username.rda", "/Users/williampetti/bpAuthsCons/fg_password.rda", "/Users/williampetti/bpAuthsCons/fg_host.rda")
    
    lapply(fg_database_values, load, .GlobalEnv)
    
    con <- src_mysql(dbname = fg_dbname, user = fg_username, password = fg_password, host = fg_host, port = 3306)
    
    df <- con %>% 
      tbl("gd_savant") %>% 
      filter(game_date > lastdate) %>%
      left_join(tbl(con, "playerid_lookup"), 
                by = c("batter" = "mlbamid")) %>% 
      left_join(tbl(con, "player_info"), by = "playerid") %>%
      collect(n = Inf)
    
    rm(con)
    
    gc()
    
    df <- df %>%
      arrange(game_date) %>%
      as.data.frame()
    
    df$hit_type <- with(df, ifelse(type == "X" & grepl("Single", df$events), 1,
                                   ifelse(type == "X" & grepl("Double", df$events), 2,
                                          ifelse(type == "X" & grepl("Triple", df$events), 3, 
                                                 ifelse(type == "X" & grepl("Home Run", df$events), 4, NA)))))
    
    df$hit <- with(df, ifelse(type == "X" & grepl("Single", df$events), 1,
                              ifelse(type == "X" & grepl("Double", df$events), 1,
                                     ifelse(type == "X" & grepl("Triple", df$events), 1, 
                                            ifelse(type == "X" & grepl("Home Run", df$events), 1, NA)))))
    
    df$fieldingTeam <- with(df, ifelse(inning_top_bottom == "bot", away_team, home_team))
    
    pitchfx2 <- df %>%
      select(-(hit_distance_sc:release_extension))
    
    statcast2 <- df %>%
      select(pitch_id, game_date, game_year, batter, pitcher, game_pk, type, hit_distance_sc:release_extension, hit, hit_type, home_team, fieldingTeam)
    
    db <- src_sqlite("statcast.db.sqlite3", create = FALSE)
    
    dbWriteTable(db$con, "pitchfx", pitchfx2, append = TRUE, overwrite = FALSE, row.names = FALSE)
    
    dbWriteTable(db$con, "statcast", statcast2, append = TRUE, overwrite = FALSE, row.names = FALSE)
    
    # print latest date in data base
    
    print_latest_date <- function() {
      statcast_db <- src_sqlite("statcast.db.sqlite3", create = FALSE)
      
      statcast_db %>% 
        tbl("pitchfx") %>% 
        select(game_date) %>% 
        distinct() %>% 
        collect(n = Inf) %>% 
        tail(n = 1)
    }
    
    print_latest_date()
    
}
  
saveRDS(update_statcast_db, "update_statcast_function.RDS")
  
saveRDS(print_latest_date, "print_latest_statcast_date.RDS")
  
###### build a Statcast databse by scraping Baseball Savant
  
  # function to scrape and format data from BaseballSavant.com
  
  scrape_statcast_savant <- function(start_date, end_date, type = "batter") {
    url <- paste0("https://baseballsavant.mlb.com/statcast_search/csv?all=true&hfPT=&hfZ=&hfGT=R%7C&hfPR=&hfAB=&stadium=&hfBBT=&hfBBL=&hfC=&season=all&player_type=", type, "&hfOuts=&pitcher_throws=&batter_stands=&start_speed_gt=&start_speed_lt=&perceived_speed_gt=&perceived_speed_lt=&spin_rate_gt=&spin_rate_lt=&exit_velocity_gt=&exit_velocity_lt=&launch_angle_gt=&launch_angle_lt=&distance_gt=&distance_lt=&batted_ball_angle_gt=&batted_ball_angle_lt=&game_date_gt=", start_date, "&game_date_lt=", end_date, "&team=&position=&hfRO=&home_road=&hfInn=&min_pitches=0&min_results=0&group_by=name&sort_col=pitches&sort_order=desc&min_abs=0&xba_gt=&xba_lt=&px1=&px2=&pz1=&pz2=&type=details&")
    x <- read.csv(url)
    x[x=="null"] <- NA
    x$game_pk <- as.character(x$game_pk) %>% as.numeric()
    x$on_1b <- as.character(x$on_1b) %>% as.numeric()
    x$on_2b <- as.character(x$on_2b) %>% as.numeric()
    x$on_3b <- as.character(x$on_3b) %>% as.numeric()
    x$hit_distance_sc <- as.character(x$hit_distance_sc) %>% as.numeric()
    x$hit_speed <- as.character(x$hit_speed) %>% as.numeric()
    x$hit_angle <- as.character(x$hit_angle) %>% as.numeric()
    x
  }
  
  test <- scrape_statcast_savant("2016-04-06", "2016-04-10", type = "pitcher")
  
  require(pacman)
  p_load(DBI, dplyr, magrittr, RSQLite, RMySQL)
  
  source("https://gist.githubusercontent.com/BillPetti/cf1e082b5e580b3b7209/raw/28b62dddfd1e494b2cf9d10c4ef79d68e6033a42/variable_list.R")
  
  # create SQLite database
  
  statcast_savant_db <- src_sqlite("statcast.savant.db.sqlite3", create = TRUE)
  
  # collect initial data from savant
  
  savant_scraped <- scrape_statcast_savant("2016-03-20", "2016-07-15")
  