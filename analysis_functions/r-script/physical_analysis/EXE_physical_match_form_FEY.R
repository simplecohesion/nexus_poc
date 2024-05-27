####################
# source("azureml_utils.R")
source("../library/feyenoord_utils.R")
#####################
## Function in depth
fun_physical_match_form_FEY <- function(benchmark_path, game_data_path, gata_path,
                                        chyron_hego_path, ch_competition_data_path,
                                        competition_name, team_name,
                                        output_path) {
  ## Load the data for the benchmark data frame
  ## Load benchmarks
  # DIRS <- list()
  # DIRS$Fysieke_data <- "/Users/folkert.boer@feyenoord.nl/Library/CloudStorage/OneDrive-FeyenoordRotterdam/Medical & Performance Feyenoord 1/Performance/Data"
  # benchmark.list <- list.files(path = "/Users/folkert.boer@feyenoord.nl/Library/CloudStorage/OneDrive-FeyenoordRotterdam/Medical & Performance Feyenoord 1/Performance/Data", pattern ='*Match Volumes.xlsx')
  benchmark.list <- benchmark_path
  # sheet_names1 = excel_sheets(benchmark.list)
  benchmark.list <- read_excel_sheet(
    sheet_name = "Info",
    path = benchmark.list,
    skip = 1
  )
  ##
  AV_benchmark.list <- benchmark.list[, c(17:30)]
  colnames(AV_benchmark.list) <- c(
    "Player", "av_Total.Distance", "av_HI.Distance", "av_HIR", "av_SP.Distance", "av_Sprints",
    "av_Acc", "av_HI_Acc", "av_Dec", "av_HI_Dec", "x90_Played", "av_Max.Speed", "Position", "F1_player"
  )
  AV_benchmark.list$av_SP_Perc <- round(as.numeric(AV_benchmark.list$av_SP.Distance) / as.numeric(AV_benchmark.list$av_HI.Distance), digits = 2)
  AV_benchmark.list <- AV_benchmark.list %>% mutate_at(c(
    "av_Total.Distance", "av_HI.Distance", "av_HIR", "av_SP.Distance", "av_Sprints",
    "av_Acc", "av_HI_Acc", "av_Dec", "av_HI_Dec", "av_Max.Speed"
  ), as.numeric)
  AV_benchmark.list <- dplyr::filter(AV_benchmark.list, F1_player == "yes", Position %nin% 1)
  ##
  MAX_benchmark.list <- benchmark.list[, c(5:15)]
  colnames(MAX_benchmark.list) <- c(
    "Player", "M_Total.Distance", "M_HI.Distance", "M_HIR", "M_SP.Distance", "M_Sprints",
    "M_Max.Speed", "M_Acc", "M_HI_Acc", "M_Dec", "M_HI_Dec"
  )
  MAX_benchmark.list <- dplyr::filter(MAX_benchmark.list, Player %in% AV_benchmark.list$Player)
  ##
  sd_benchmark.list <- benchmark.list[, c(33:42)]
  colnames(sd_benchmark.list) <- c(
    "Player", "sd_Total.Distance", "sd_HI.Distance", "sd_HIR", "sd_SP.Distance", "sd_Sprints",
    "sd_Acc", "sd_HI_Acc", "sd_Dec", "sd_HI_Dec"
  )
  sd_benchmark.list <- dplyr::filter(sd_benchmark.list, Player %in% AV_benchmark.list$Player)
  ##
  benchmark.list <- AV_benchmark.list %>%
    dplyr::full_join(MAX_benchmark.list) %>%
    dplyr::full_join(sd_benchmark.list) %>%
    as.data.frame()
  benchmark.list$Position <- ifelse(benchmark.list$Player == "Bart Nieuwkoop", 2, benchmark.list$Position)
  ##
  # zero_minutes <- which(benchmark.list$x90_Played == 0)
  for (i in 1:nrow(benchmark.list)) {
    benchmark.list$av_Total.Distance[i] <- ifelse(is.na(benchmark.list$av_Total.Distance[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_Total.Distance, na.rm = TRUE),
      benchmark.list$av_Total.Distance[i]
    )
    benchmark.list$av_HI.Distance[i] <- ifelse(is.na(benchmark.list$av_HI.Distance[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_HI.Distance, na.rm = TRUE),
      benchmark.list$av_HI.Distance[i]
    )
    benchmark.list$av_SP.Distance[i] <- ifelse(is.na(benchmark.list$av_SP.Distance[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_SP.Distance, na.rm = TRUE),
      benchmark.list$av_SP.Distance[i]
    )
    benchmark.list$av_Sprints[i] <- ifelse(is.na(benchmark.list$av_Sprints[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_Sprints, na.rm = TRUE),
      benchmark.list$av_Sprints[i]
    )
    benchmark.list$av_Acc[i] <- ifelse(is.na(benchmark.list$av_Acc[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_Acc, na.rm = TRUE),
      benchmark.list$av_Acc[i]
    )
    benchmark.list$av_HI_Acc[i] <- ifelse(is.na(benchmark.list$av_HI_Acc[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_HI_Acc, na.rm = TRUE),
      benchmark.list$av_HI_Acc[i]
    )
    benchmark.list$av_Dec[i] <- ifelse(is.na(benchmark.list$av_Dec[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_Dec, na.rm = TRUE),
      benchmark.list$av_Dec[i]
    )
    benchmark.list$av_HI_Dec[i] <- ifelse(is.na(benchmark.list$av_HI_Dec[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_HI_Dec, na.rm = TRUE),
      benchmark.list$av_HI_Dec[i]
    )
    benchmark.list$av_SP_Perc[i] <- ifelse(is.na(benchmark.list$av_SP_Perc[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_SP_Perc, na.rm = TRUE),
      benchmark.list$av_SP_Perc[i]
    )
    benchmark.list$av_Max.Speed[i] <- ifelse(is.na(benchmark.list$av_Max.Speed[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$av_Max.Speed, na.rm = TRUE),
      benchmark.list$av_Max.Speed[i]
    )
    ###########
    benchmark.list$sd_Total.Distance[i] <- ifelse(is.na(benchmark.list$sd_Total.Distance[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_Total.Distance, na.rm = TRUE),
      benchmark.list$sd_Total.Distance[i]
    )
    benchmark.list$sd_HI.Distance[i] <- ifelse(is.na(benchmark.list$sd_HI.Distance[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_HI.Distance, na.rm = TRUE),
      benchmark.list$sd_HI.Distance[i]
    )
    benchmark.list$sd_SP.Distance[i] <- ifelse(is.na(benchmark.list$sd_SP.Distance[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_SP.Distance, na.rm = TRUE),
      benchmark.list$sd_SP.Distance[i]
    )
    benchmark.list$sd_Sprints[i] <- ifelse(is.na(benchmark.list$sd_Sprints[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_Sprints, na.rm = TRUE),
      benchmark.list$sd_Sprints[i]
    )
    benchmark.list$sd_Acc[i] <- ifelse(is.na(benchmark.list$sd_Acc[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_Acc, na.rm = TRUE),
      benchmark.list$sd_Acc[i]
    )
    benchmark.list$sd_HI_Acc[i] <- ifelse(is.na(benchmark.list$sd_HI_Acc[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_HI_Acc, na.rm = TRUE),
      benchmark.list$sd_HI_Acc[i]
    )
    benchmark.list$sd_Dec[i] <- ifelse(is.na(benchmark.list$sd_Dec[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_Dec, na.rm = TRUE),
      benchmark.list$sd_Dec[i]
    )
    benchmark.list$sd_HI_Dec[i] <- ifelse(is.na(benchmark.list$sd_HI_Dec[i]) == TRUE,
      mean(dplyr::filter(benchmark.list, Position == benchmark.list$Position[i], Player != benchmark.list$Player[i])$sd_HI_Dec, na.rm = TRUE),
      benchmark.list$sd_HI_Dec[i]
    )
  }
  ##
  benchmark.list[, c(2:34)] <- lapply(benchmark.list[, c(2:34)], as.numeric)
  colnames(benchmark.list)[13] <- "Position.nr"
  ##################################################
  ##################################################
  ## Load the physical match data
  # file.list <- list.files(path = "/Users/folkert.boer@feyenoord.nl/Library/CloudStorage/OneDrive-FeyenoordRotterdam/Medical & Performance Feyenoord 1/Performance/Data", pattern ='*Match Volumes.xlsx')
  file.list <- game_data_path
  # sheet_names = excel_sheets(file.list)
  df.list <- read_excel_sheet(
    sheet_name = "Data",
    path = file.list,
    skip = 0
  )
  df.list2 <- read_excel_sheet(
    sheet_name = "%SpeedZoneData",
    path = file.list,
    skip = 0
  )
  ##
  physicalMatch_database_teamALL <- dplyr::bind_rows(df.list, .id = "id") %>%
    as.data.frame() %>%
    dplyr::filter(Season == "2023-2024", Player %in% c(benchmark.list$Player, "Givairo Read", "Jaden Slory"))

  physicalMatch_database_team2 <- dplyr::bind_rows(df.list2, .id = "id") %>%
    as.data.frame() %>%
    dplyr::filter(Season == "2023-2024", Player %in% c(benchmark.list$Player, "Givairo Read", "Jaden Slory")) %>%
    dplyr::select(-c(id, Date, WeekType, Phase, Position, Sessiontype, `Day Code`, Role, DataSource, Duration, Competition))

  physicalMatch_database_team <- merge(physicalMatch_database_teamALL, physicalMatch_database_team2, by = c("Player", "Week", "Match", "Season"))

  ##
  physicalMatch_database_teamF1 <- physicalMatch_database_team
  # physicalMatch_database_teamF1 <- dplyr::filter(physicalMatch_database_team, Team == "FEY", Team_sub == "-")
  # team_matches <- unique(physicalMatch_database_teamF1$Match)
  for (i in 1:nrow(physicalMatch_database_teamF1)) {
    physicalMatch_database_teamF1$Match_number[i] <- ifelse(is.na(as.numeric(gsub("[^0-9]", "", physicalMatch_database_teamF1$Match[i]))), physicalMatch_database_team$Competition[i],
      as.numeric(gsub("[^0-9]", "", physicalMatch_database_teamF1$Match[i]))
    )
  }
  physicalMatch_database_teamF1 <- dplyr::filter(physicalMatch_database_teamF1, Position %nin% "Goalkeeper") # %>%
  # dplyr::select(Date, Week, Phase, Player, Position, Match, Competition, Role,
  #               Duration, `Total Distance`, `HI Distance`, `Sprint Distance`, `HI Accelerations`, `HI Decelerations`, `Top Speed`, Match_number)

  physicalMatch_database_teamF1$`Sprint Percentage` <- physicalMatch_database_teamF1$`Sprint Distance` / physicalMatch_database_teamF1$`HI Distance`
  physicalMatch_database_teamF1[, c(16:62)] <- lapply(physicalMatch_database_teamF1[, c(16:62)], as.numeric)
  ## Set the OP values based on the AVERAGE MATCH VALUES of a player
  for (i in 1:nrow(physicalMatch_database_teamF1)) {
    physicalMatch_database_teamF1$zScore_TD[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$`Total Distance`[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_Total.Distance,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_Total.Distance,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    physicalMatch_database_teamF1$zScore_HI_TD[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$`HI Distance`[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_HI.Distance,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_HI.Distance,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    physicalMatch_database_teamF1$zScore_SP_TD[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$`Sprint Distance`[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_SP.Distance,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_SP.Distance,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    physicalMatch_database_teamF1$zScore_SP[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$Sprints[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_Sprints,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_Sprints,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    physicalMatch_database_teamF1$zScore_ACC[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$Accelerations[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_Acc,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_Acc,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    # physicalMatch_database_teamF1$SP_Perc[i] <- calc_z_scores(value = physicalMatch_database_teamF1$[i],
    #                                                           mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_Sprints,
    #                                                           sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_Sprints)
    physicalMatch_database_teamF1$zScore_HI_ACC[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$`HI Accelerations`[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_HI_Acc,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_HI_Acc,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    physicalMatch_database_teamF1$zScore_DEC[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$Decelerations[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_Dec,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_Dec,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    physicalMatch_database_teamF1$zScore_HI_DEC[i] <- calc_z_scores(
      value = physicalMatch_database_teamF1$`HI Decelerations`[i],
      mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_HI_Dec,
      sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_HI_Dec,
      playing_minutes = physicalMatch_database_teamF1$Duration[i]
    )
    # physicalMatch_database_teamF1$Max_SPEED[i] <- calc_z_scores(value = physicalMatch_database_teamF1$`Top Speed`[i],
    #                                                             mean = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$av_Sprints,
    #                                                             sd = dplyr::filter(benchmark.list, Player == physicalMatch_database_teamF1$Player[i])$sd_Sprints)
  }
  # Add some proportion values for the plot later
  for (i in 1:nrow(physicalMatch_database_teamF1)) {
    physicalMatch_database_teamF1$Prop_TD[i] <- ifelse((1 + physicalMatch_database_teamF1$OP_TD[i]) > 1, 1, (1 + physicalMatch_database_teamF1$OP_TD[i]))
  }
  colnames(physicalMatch_database_teamF1)[5] <- "Position.name"
  ###
  df_total <- merge(physicalMatch_database_teamF1, benchmark.list, by = "Player")
  ###
  # Select data for the spefific match filtered by input_Match_number
  # m_data <- dplyr::filter(df_total, Match == "B-SF. FEY - FC Groningen") %>%
  m_data <- dplyr::filter(df_total, Date == last(df_total$Date), Competition == competition_name) %>% # , Duration> 45)
    dplyr::select(
      Player, Duration, `Total Distance`, `HI Distance`, `Sprint Distance`, Sprints, Accelerations, `HI Accelerations`, Decelerations, `HI Decelerations`, `Top Speed`, `Distance 80+`,
      M_Total.Distance, M_HI.Distance, M_SP.Distance, M_Sprints, M_Max.Speed, M_Acc, M_HI_Acc, M_Dec, M_HI_Dec,
      av_Total.Distance, av_HI.Distance, av_SP.Distance, av_Sprints, av_Max.Speed, av_Acc, av_HI_Acc, av_Dec, av_HI_Dec,
      sd_Total.Distance, sd_HI.Distance, sd_SP.Distance, sd_Sprints, sd_Acc, sd_HI_Acc, sd_Dec, sd_HI_Dec,
      zScore_TD, zScore_HI_TD, zScore_SP_TD, zScore_SP, zScore_ACC, zScore_HI_ACC, zScore_DEC, zScore_HI_DEC, M_Max.Speed
    )
  m_data[, c(3:5, 12:16, 22:25, 27:30)] <- round(m_data[, c(3:5, 12:16, 22:25, 27:30)], digits = 0)
  m_data[, c(11, 17, 26)] <- round(m_data[, c(11, 17, 26)], digits = 1)
  m_data <- m_data[order(m_data$Player), ]
  row.names(m_data) <- NULL
  m_data[is.na(m_data)] <- 0
  colnames(m_data) <- c(
    "Name", "Playing Time", "Total Distance", "HI Distance", "SP Distance", "Sprints", "Accelerations", "HI Acc", "Decelerations", "HI Dec",
    "Top Speed", "Distance 80+",
    "max_Total.Distance", "max_HI.Distance", "max_SP.Distance", "max_Sprints", "max_Max.Speed", "max_Acc", "max_HI_Acc", "max_Dec", "max_HI_Dec",
    "av_Total.Distance", "av_HI.Distance", "av_SP.Distance", "av_Sprints", "av_Max.Speed", "av_Acc", "av_HI_Acc", "av_Dec", "av_HI_Dec",
    "sd_Total.Distance", "sd_HI.Distance", "sd_SP.Distance", "sd_Sprints", "sd_Acc", "sd_HI_Acc", "sd_Dec", "sd_HI_Dec",
    "zScore TD", "zScore HId", "zScore SPd", "zScore SP", "zScore Acc", "zScore HI Acc", "zScore Dec", "zScore HI Dec"
  )

  ##################################################
  ##################################################
  # Add tracking data In Possession and Out of Possession HI Distances > above 19.8 km/h from GaTa platform
  if (input_competition == "Champions League") {
    # DIRS$GaTa_data <- "/Users/folkert.boer@feyenoord.nl/Documents/Feyenoord/SportsDynamic/Raw Data/"
    #
    # GaTa_data <- list.files(path = DIRS$GaTa_data, pattern ='*game-stats.csv')
    GaTa_data <- gata_path
    GaTa_data <- read.csv(GaTa_data[1], sep = ";") %>% as.data.frame()

    GaTa_data$Player <- ifelse(GaTa_data$Player == "None Igor Paixao", "Igor Paixao", GaTa_data$Player)
    GaTa_players <- GaTa_data %>%
      dplyr::filter(Metric.name == "Playing time") %>%
      dplyr::filter(Value > 0, Team.name == team_name)
    GaTa_players <- GaTa_players$Player
    GaTa_data <- dplyr::filter(GaTa_data, Player %in% GaTa_players, Metric.name %in% c("Distance covered - HI Distance (>19.8 km/h) - In possession", "Distance covered - HI Distance (>19.8 km/h) - Out of possession"))

    if (nrow(GaTa_data) > 0) {
      players <- unique(GaTa_data$Player)
      df_tracking_match <- create_empty_table(num_rows = length(players), num_cols = 3)
      colnames(df_tracking_match) <- c("Name", "HId IP", "HId OP")
      for (i in 1:length(players)) {
        df_tracking_match$Name[i] <- players[i]
        df_tracking_match$`HId IP`[i] <- sum(dplyr::filter(GaTa_data, Player == players[i], Metric.name %in% c("Distance covered - HI Distance (>19.8 km/h) - In possession"))$Value, na.rm = TRUE)
        df_tracking_match$`HId OP`[i] <- sum(dplyr::filter(GaTa_data, Player == players[i], Metric.name %in% c("Distance covered - HI Distance (>19.8 km/h) - Out of possession"))$Value, na.rm = TRUE)
      }
      for (i in 1:nrow(df_tracking_match)) {
        df_tracking_match$`HId IP%`[i] <- ifelse(df_tracking_match$`HId IP`[i] == 0, 0, df_tracking_match$`HId IP`[i] / sum(df_tracking_match$`HId IP`[i] + df_tracking_match$`HId OP`[i]))
        df_tracking_match$`HId OP%`[i] <- ifelse(df_tracking_match$`HId OP`[i] == 0, 0, df_tracking_match$`HId OP`[i] / sum(df_tracking_match$`HId IP`[i] + df_tracking_match$`HId OP`[i]))
      }
    } else {
      players <- unique(m_data$Name)
      df_tracking_match <- create_empty_table(num_rows = length(players), num_cols = 5)
      colnames(df_tracking_match) <- c("Name", "HId IP", "HId OP", "HId IP%", "HId OP%")
      df_tracking_match$Name <- unique(m_data$Name)
      df_tracking_match$`HId IP` <- "-"
      df_tracking_match$`HId OP` <- "-"
      df_tracking_match$`HId IP%` <- "-"
      df_tracking_match$`HId OP%` <- "-"
    }

    df_tracking_match$Name <- ifelse(df_tracking_match$Name == "C. Stengs", "Calvin Stengs", df_tracking_match$Name)
  } else {
    ## Add tracking data In Possession and Out of Possession HI Distances > above 19.8 km/h
    physical_ERE_2324_total <- loadRData(chyron_hego_path)
    CH_competition_data_2324 <- loadRData(ch_competition_data_path)
    # load("/Users/folkert.boer@feyenoord.nl/Dropbox/ChyronHego data/2023-2024/Physical data/physical_data_Eredivisie_2324.RData")
    # load("/Users/folkert.boer@feyenoord.nl/Dropbox/ChyronHego data/Data Bases/CH_competition_data_2324.RData")
    ##

    match_ids <- dplyr::filter(
      CH_competition_data_2324, Ronde == as.numeric(dplyr::filter(df_total, Date == last(df_total$Date))$Match_number[1]),
      ThuisTeam == team_name | UitTeam == team_name
    )$MatchID
    df_phy <- dplyr::filter(
      physical_ERE_2324_total, match_id == match_ids, TeamName == team_name,
      physical.parameter %in% c("Net Distance (20.0 - 25.0 km/h) - ATT", "Net Distance (20.0 - 25.0 km/h) - DEF", "Net Distance (> 25.0 km/h) - ATT", "Net Distance (> 25.0 km/h) - DEF")
    )
    df_phy$player.name <- ifelse(df_phy$player.name == "Dávid Hancko", "David Hancko",
      ifelse(df_phy$player.name == "Javairô Dilrosun", "Javairo Dilrosun",
        ifelse(df_phy$player.name == "Santiago Giménez", "Santiago Gimenez",
          ifelse(df_phy$player.name == "Igor Guilherme Barbosa da Paixão", "Igor Paixao",
            ifelse(df_phy$player.name == "Patrik Wålemark", "Patrik Walemark",
              ifelse(df_phy$player.name == "Danilo Pereira da Silva", "Danilo Pereira",
                ifelse(df_phy$player.name == "Orkun Kökcü", "Orkun Kokcu",
                  ifelse(df_phy$player.name == "Marcos López", "Marcos Lopez",
                    df_phy$player.name
                  )
                )
              )
            )
          )
        )
      )
    )

    if (nrow(df_phy) > 0) {
      players <- unique(df_phy$player.name)
      df_tracking_match <- create_empty_table(num_rows = length(players), num_cols = 3)
      colnames(df_tracking_match) <- c("Name", "HId IP", "HId OP")
      for (i in 1:length(players)) {
        df_tracking_match$Name[i] <- players[i]
        df_tracking_match$`HId IP`[i] <- sum(dplyr::filter(df_phy, player.name == players[i], physical.parameter %in% c("Net Distance (20.0 - 25.0 km/h) - ATT", "Net Distance (> 25.0 km/h) - ATT"))$Total_p90, na.rm = TRUE)
        df_tracking_match$`HId OP`[i] <- sum(dplyr::filter(df_phy, player.name == players[i], physical.parameter %in% c("Net Distance (20.0 - 25.0 km/h) - DEF", "Net Distance (> 25.0 km/h) - DEF"))$Total_p90, na.rm = TRUE)
      }
      for (i in 1:nrow(df_tracking_match)) {
        df_tracking_match$`HId IP%`[i] <- ifelse(df_tracking_match$`HId IP`[i] == 0, 0, df_tracking_match$`HId IP`[i] / sum(df_tracking_match$`HId IP`[i] + df_tracking_match$`HId OP`[i]))
        df_tracking_match$`HId OP%`[i] <- ifelse(df_tracking_match$`HId OP`[i] == 0, 0, df_tracking_match$`HId OP`[i] / sum(df_tracking_match$`HId IP`[i] + df_tracking_match$`HId OP`[i]))
      }
    } else {
      players <- unique(m_data$Name)
      df_tracking_match <- create_empty_table(num_rows = length(players), num_cols = 5)
      colnames(df_tracking_match) <- c("Name", "HId IP", "HId OP", "HId IP%", "HId OP%")
      df_tracking_match$Name <- unique(m_data$Name)
      df_tracking_match$`HId IP` <- " "
      df_tracking_match$`HId OP` <- " "
      df_tracking_match$`HId IP%` <- " "
      df_tracking_match$`HId OP%` <- " "
    }
  }

  ##################################################
  ##################################################
  ### Create overall data frame to visualize
  df_final <- merge(m_data, df_tracking_match, by = "Name")
  ### Add teams total on top
  df_totals_match <- create_empty_table(num_rows = 1, num_cols = length(df_final))
  colnames(df_totals_match) <- colnames(df_final)
  df_totals_match$Name <- "Team Total"
  df_totals_match$`Playing Time` <- ""
  df_totals_match$`Total Distance` <- sum(df_final$`Total Distance`) + 5000
  df_totals_match$`HI Distance` <- sum(df_final$`HI Distance`)
  df_totals_match$`SP Distance` <- sum(df_final$`SP Distance`)
  df_totals_match$Sprints <- sum(df_final$Sprints)
  df_totals_match$Accelerations <- sum(m_data$Accelerations)
  df_totals_match$`HI Acc` <- sum(m_data$`HI Acc`)
  df_totals_match$Decelerations <- sum(m_data$Decelerations)
  df_totals_match$`HI Dec` <- sum(m_data$`HI Dec`)
  df_totals_match$`Top Speed` <- round(max(df_final$`Top Speed`), digits = 1)
  df_totals_match$`Distance 80+` <- round(sum(df_final$`Distance 80+`), digits = 0)
  df_totals_match$`HId IP%` <- round(mean(df_final$`HId IP%`), digits = 2)
  df_totals_match$`HId OP%` <- round(mean(df_final$`HId OP%`), digits = 2)
  df_totals_match[, c(13:38)] <- ""

  m_data <- rbind(df_totals_match, df_final)
  m_data[, c(3:28)] <- lapply(m_data[, c(3:28)], as.numeric)
  m_data[, "Top Speed"] <- round(m_data[, "Top Speed"], digits = 1)
  m_data[, "Distance 80+"] <- round(m_data[, "Distance 80+"], digits = 0)
  m_data$`Playing Time` <- as.numeric(m_data$`Playing Time`)
  m_data$prop_TD <- 0
  m_data$prop_HID <- 0
  m_data$prop_SPD <- 0
  m_data$prop_SPR <- 0
  m_data$prop_ACC <- 0
  m_data$prop_HIACC <- 0
  m_data$prop_DEC <- 0
  m_data$prop_HIDEC <- 0
  m_data$prop_TopSpeed <- 0

  # Add % of parameters
  for (i in 2:nrow(m_data)) {
    m_data$prop_TD[i] <- as.numeric(ifelse(m_data$max_Total.Distance[i] == 0, 0, (((m_data$`Total Distance`[i] - m_data$max_Total.Distance[i]) / m_data$max_Total.Distance[i]) + 1)))
    m_data$prop_HID[i] <- as.numeric(ifelse(m_data$max_HI.Distance[i] == 0, 0, (((m_data$`HI Distance`[i] - m_data$max_HI.Distance[i]) / m_data$max_HI.Distance[i]) + 1)))
    m_data$prop_SPD[i] <- as.numeric(ifelse(m_data$max_SP.Distance[i] == 0, 0, (((m_data$`SP Distance`[i] - m_data$max_SP.Distance[i]) / m_data$max_SP.Distance[i]) + 1)))
    m_data$prop_SPR[i] <- as.numeric(ifelse(m_data$max_Sprints[i] == 0, 0, (((m_data$Sprints[i] - m_data$max_Sprints[i]) / m_data$max_Sprints[i]) + 1)))
    m_data$prop_ACC[i] <- as.numeric(ifelse(m_data$max_Acc[i] == 0, 0, (((m_data$Accelerations[i] - m_data$max_Acc[i]) / m_data$max_Acc[i]) + 1)))
    m_data$prop_HIACC[i] <- as.numeric(ifelse(m_data$max_HI_Acc[i] == 0, 0, (((m_data$`HI Acc`[i] - m_data$max_HI_Acc[i]) / m_data$max_HI_Acc[i]) + 1)))
    m_data$prop_DEC[i] <- as.numeric(ifelse(m_data$max_Dec[i] == 0, 0, (((m_data$Decelerations[i] - m_data$max_Dec[i]) / m_data$max_Dec[i]) + 1)))
    m_data$prop_HIDEC[i] <- as.numeric(ifelse(m_data$max_HI_Dec[i] == 0, 0, (((m_data$`HI Dec`[i] - m_data$max_HI_Dec[i]) / m_data$max_HI_Dec[i]) + 1)))
    m_data$prop_TopSpeed[i] <- as.numeric(ifelse(m_data$max_Max.Speed[i] == 0, 0, (((m_data$`Top Speed`[i] - m_data$max_Max.Speed[i]) / m_data$max_Max.Speed[i]) + 1)))
  }
  # Add z-scores
  for (i in 1:nrow(m_data)) {
    m_data$`zScore TD`[i] <- ifelse(m_data$`zScore TD`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore TD`[i])
    m_data$`zScore HId`[i] <- ifelse(m_data$`zScore HId`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore HId`[i])
    m_data$`zScore SPd`[i] <- ifelse(m_data$`zScore SPd`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore SPd`[i])
    m_data$`zScore SP`[i] <- ifelse(m_data$`zScore SP`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore SP`[i])
    m_data$`zScore Acc`[i] <- ifelse(m_data$`zScore Acc`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore Acc`[i])
    m_data$`zScore HI Acc`[i] <- ifelse(m_data$`zScore HI Acc`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore HI Acc`[i])
    m_data$`zScore Dec`[i] <- ifelse(m_data$`zScore Dec`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore Dec`[i])
    m_data$`zScore HI Dec`[i] <- ifelse(m_data$`zScore HI Dec`[i] < 0 & m_data$`Playing Time`[i] < 90, 0, m_data$`zScore HI Dec`[i])
  }
  for (i in 1:nrow(m_data)) {
    m_data$`zScore TD`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore TD`[i])
    m_data$`zScore HId`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore HId`[i])
    m_data$`zScore SPd`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore SPd`[i])
    m_data$`zScore SP`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore SP`[i])
    m_data$`zScore Acc`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore Acc`[i])
    m_data$`zScore HI Acc`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore HI Acc`[i])
    m_data$`zScore Dec`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore Dec`[i])
    m_data$`zScore HI Dec`[i] <- ifelse(m_data$`Playing Time`[i] < 60, 0, m_data$`zScore HI Dec`[i])
  }
  m_data[, c(49, 50)] <- round(m_data[, c(49, 50)], digits = 2)
  m_data[, c(6:10)] <- round(m_data[, c(6:10)], digits = 0)
  m_data[2:nrow(m_data), 2] <- round(as.numeric(m_data[2:nrow(m_data), 2]), digits = 0)
  m_data[1, 2] <- ""
  ##################################################
  ##################################################
  # Output files; Final Plot and add columns for Personal Bests, Coloring and Team Best
  final_plot <- m_data[, c(1:12, 49, 50)]
  # Personal Best
  PB_df <- m_data[, c(1, 2, 51:59)]
  PB_df[, 3:11][PB_df[, 3:11] <= 1] <- 0
  PB_df[, 3:11][PB_df[, 3:11] > 1] <- 1
  PB_df <- PB_df %>% dplyr::rename_with(.cols = matches("prop_"), ~ paste0(.x, "_PB"))
  # Coloring
  Color_df <- m_data[, c(1, 2, 39:46)]
  for (i in 3:length(Color_df)) {
    Color_df[, i] <- ifelse(Color_df[, i] > 0.5 & Color_df[, i] <= 1, "#F6EF4D",
      ifelse(Color_df[, i] > 1 & Color_df[, i] <= 1.5, "#FFA500",
        ifelse(Color_df[, i] > 1, "#FF4F4F",
          ifelse(Color_df[, i] < -1, "#96ECDC", "white")
        )
      )
    )
  }
  Color_df <- Color_df %>% dplyr::rename_with(.cols = matches("zScore"), ~ paste0(.x, "_color"))
  # Team Best
  TB_df <- m_data[-1, c(1:12)]
  # Function to convert column values to 0 and 1, with 1 indicating the maximum value
  convert_to_binary <- function(x) {
    ifelse(x == max(x, na.rm = TRUE), 1, 0)
  }
  # Apply the function to all columns in the data frame
  TB_df <- TB_df %>%
    dplyr::mutate(across(3:12, convert_to_binary))
  TB_df <- TB_df %>% dplyr::rename_with(.cols = 3:12, ~ paste0(.x, "_TB"))
  ###
  game_data_physical_data <- final_plot %>%
    dplyr::left_join(PB_df) %>%
    dplyr::left_join(Color_df) %>%
    dplyr::left_join(TB_df)

  ##################################################
  ##################################################
  ## Making the visualisation
  ## Set colors for analysis
  lowOrange <- "#F6EF4D"
  customOrange <- "#FFA500"
  customRed <- "#FF4F4F"
  ##
  customGreen <- "#71CA97"
  custimLGreen <- "#96ECDC"
  customGrey <- "#ECECEC"
  customWhite <- "white"

  ## Two scaling functions for visualisation
  unit.scale <- function(x) (x - min(x)) / (max(x) - min(x))
  unit.scale2 <- function(x) (x / max(x))
  manual.scale <- function(x) ifelse((x - min(x)) / (max(x) - min(x)) < 0.5, 0.5, (x - min(x)) / (max(x) - min(x)))

  improvement_formatter <- formatter("span",
    style = x ~ style(color = "black"),
    x ~ color_bar(ifelse(`OP TD` > 0.4, customRed, ifelse(`OP TD` < -0.4, customRed, customGrey))),
    x ~ icontext(ifelse(`Total Distance` > `av_Total.Distance`, "arrow-up", "arrow-down"), `Total Distance`)
  )
  ## Output of the tabel by using formattable
  df_table <- formattable(m_data[, c(1:12, 49, 50)],
    align = c("l"),
    width = c(1.2, rep(0.1, length(m_data) - 1)),
    list(
      `Name` = formatter("span", style = function(x) style(color = "grey", font.weight = "regular")),
      formattable::color_text(row = 1, color = "black", font.weight = "bold"),
      formattable::area(col = 3, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore TD`[2:nrow(m_data)] > 0.5 & m_data$`zScore TD`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore TD`[2:nrow(m_data)] > 1 & m_data$`zScore TD`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore TD`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore TD`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 4, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore HId`[2:nrow(m_data)] > 0.5 & m_data$`zScore HId`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore HId`[2:nrow(m_data)] > 1 & m_data$`zScore HId`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore HId`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore HId`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 5, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore SPd`[2:nrow(m_data)] > 0.5 & m_data$`zScore SPd`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore SPd`[2:nrow(m_data)] > 1 & m_data$`zScore SPd`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore SPd`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore SPd`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 6, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore SP`[2:nrow(m_data)] > 0.5 & m_data$`zScore SP`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore SP`[2:nrow(m_data)] > 1 & m_data$`zScore SP`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore SP`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore SP`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 7, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore Acc`[2:nrow(m_data)] > 0.5 & m_data$`zScore Acc`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore Acc`[2:nrow(m_data)] > 1 & m_data$`zScore Acc`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore Acc`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore Acc`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 8, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore HI Acc`[2:nrow(m_data)] > 0.5 & m_data$`zScore HI Acc`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore HI Acc`[2:nrow(m_data)] > 1 & m_data$`zScore HI Acc`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore HI Acc`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore SP`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 9, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore Dec`[2:nrow(m_data)] > 0.5 & m_data$`zScore Dec`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore Dec`[2:nrow(m_data)] > 1 & m_data$`zScore Dec`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore Dec`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore Dec`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = 10, row = 2:nrow(m_data)) ~ color_bar(ifelse(m_data$`zScore HI Dec`[2:nrow(m_data)] > 0.5 & m_data$`zScore HI Dec`[2:nrow(m_data)] <= 1, lowOrange,
        ifelse(m_data$`zScore HI Dec`[2:nrow(m_data)] > 1 & m_data$`zScore HI Dec`[2:nrow(m_data)] <= 1.5, customOrange,
          ifelse(m_data$`zScore HI Dec`[2:nrow(m_data)] > 1, customRed,
            ifelse(m_data$`zScore HI Dec`[2:nrow(m_data)] < -1, custimLGreen, customWhite)
          )
        )
      ), fun = unit.scale2),
      formattable::area(col = c(13:14), row = 1:nrow(m_data)) ~ function(x) formattable::percent(x, digits = 0),
      formattable::area(col = 11, row = 2:nrow(m_data)) ~ color_bar(customGrey, fun = unit.scale2),
      formattable::area(col = 12, row = 2:nrow(m_data)) ~ color_bar(customGrey, fun = unit.scale2),
      formattable::area(col = 13, row = 2:nrow(m_data)) ~ color_bar(customGrey, fun = unit.scale2),
      formattable::area(col = 14, row = 2:nrow(m_data)) ~ color_bar(customGrey, fun = unit.scale2)
    )
  )
  # df_table
  df_table_widget <- as.htmlwidget(df_table)
  htmlwidgets::saveWidget(df_table_widget, paste0(output_path, "game_data_physical_data.html"))
  # Export tot .json file
  jsonlite::write_json(game_data_physical_data, path = paste0(output_path, "game_data_physical_data.json"))
}


#########################################################################################################
#########################################################################################################
