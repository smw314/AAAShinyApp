ml_pbp <- read.csv("/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/df_pbp.csv")

fastball <- c("Four-Seam Fastball", "Sinker", "Fastball", "Two-Seam Fastball")
breaking_ball <- c("Cutter", "Curveball", "Slider", "Sweeper", "Knuckle Curve",
                   "Eephus", "Slurve", "Slow Curve")
offspeed <- c("Changeup", "Splitter", "Forkball", "Knuckle Ball")

proj <- ml_pbp %>% 
  select("pitchData.coordinates.vX0", "pitchData.coordinates.vY0", 
         "pitchData.coordinates.vZ0", "pitchData.coordinates.aX", 
         "pitchData.coordinates.aY", "pitchData.coordinates.aZ",
         "details.type.code", "pitchData.startSpeed", "pitchData.coordinates.x0",
         "pitchData.coordinates.z0", "pitchData.coordinates.y0", 
         "pitchData.breaks.spinDirection", "pitchData.breaks.spinRate",
         "pitchData.extension", "pitchData.coordinates.pfxX",
         "pitchData.coordinates.pfxZ", "matchup.pitchHand.code")

ivb <- as.data.frame(predict(xgb_pfx_z, data.matrix(proj)))
hb <- as.data.frame(predict(xgb_pfx_x, data.matrix(proj)))

ml_pbp <- cbind(ml_pbp, ivb, hb)

colnames(ml_pbp)[172:173] <- c("ivb", "hb")

y0 <- 50
yf <- (17/12)

ml_pbp <- ml_pbp %>% 
  filter(type == "pitch") %>% 
  mutate(pitching_team = ifelse(about.halfInning == "top", home_team, away_team),
         parent_team = ifelse(about.halfInning == "top", home_parentOrg_name, away_parentOrg_name),
         parent_team = paste("(", parent_team, ")", sep = ""),
         team = paste(pitching_team, parent_team),
         oz = ifelse(pitchData.zone > 9, 1, 0),
         swstr = ifelse(details.description == "Swinging Strike", 1, 0),
         cs = ifelse(details.description == "Called Strike", 1, 0),
         swing = ifelse(details.description == "In play, out(s)" | 
                          details.description == "In play, no out" |
                          details.description == "Foul" | 
                          details.description == "Swinging Strike" | 
                          details.description == "In play, run(s)", 1, 0),
         oz_swing = ifelse(oz > 0 & swing > 0, 1, 0),
         whiff = ifelse(swstr > 0 & swing > 0, 1, 0),
         bip = ifelse(grepl("In play", details.description), 1, 0),
         gb = ifelse(hitData.trajectory == "ground_ball", 1, 0),
         hard = ifelse(hitData.launchSpeed > 94.9, 1, 0),
         batting_team = ifelse(about.halfInning == "top", away_team, home_team),
         batting_parent_team = ifelse(about.halfInning == "top", away_parentOrg_name, home_parentOrg_name),
         batting_parent_team = paste("(", batting_parent_team, ")", sep = ""),
         batting_team = paste(batting_team, batting_parent_team),
         vy_f = -1*(sqrt((pitchData.coordinates.vY0^2)-(2*pitchData.coordinates.aY*(y0-yf)))),
         t = (vy_f - pitchData.coordinates.vY0)/pitchData.coordinates.aY,
         vz_f = pitchData.coordinates.vZ0 + (pitchData.coordinates.aZ*t),
         VAA = -1*(atan(vz_f/vy_f)*(180/pi)),
         filter = paste(matchup.pitcher.fullName, team, sep = ", "),
         sweet_spot = ifelse(hitData.launchAngle > 7.9 & hitData.launchAngle < 32.1, 1, 0),
         la_recorded = ifelse(is.na(hitData.launchAngle), 0, 1),
         fb = ifelse(details.type.description %in% fastball, 1, 0),
         fb_swstr = ifelse(fb > 0 & swstr > 0, 1, 0),
         bb = ifelse(details.type.description %in% breaking_ball, 1, 0),
         bb_swstr = ifelse(bb > 0 & swstr > 0, 1, 0),
         off = ifelse(details.type.description %in% offspeed, 1, 0),
         off_swstr = ifelse(off > 0 & swstr > 0, 1, 0))

ml_pbp <- ml_pbp %>% 
  mutate(iz = ifelse(pitchData.zone > 9, 0, 1),
         iz_contact = ifelse((details.description == "In play, out(s)" |
                               details.description == "In play, no out" |
                               details.description == "Foul" |
                               details.description == "In play, run(s)") & iz > 0, 1, 0),
         iz_swing = ifelse((details.description == "In play, out(s)" |
                              details.description == "In play, no out" |
                              details.description == "Swinging Strike" |
                              details.description == "Foul" |
                              details.description == "In play, run(s)") & iz > 0, 1, 0))


pitcher_data <- ml_pbp %>% 
  group_by(matchup.pitcher.fullName, team, details.type.description, filter, matchup.pitcher.id) %>% 
  summarise(n = n(),
            velo = mean(pitchData.startSpeed, na.rm = TRUE),
            ivb = mean(ivb, na.rm = TRUE),
            hb = mean(hb, na.rm = TRUE),
            spinrate = mean(pitchData.breaks.spinRate, na.rm = TRUE),
            spindirection = mean(pitchData.breaks.spinDirection, na.rm = TRUE),
            vaa = mean(VAA, na.rm = TRUE),
            avg_ev = mean(hitData.launchSpeed, na.rm = TRUE),
            oz = 100* sum(oz)/n,
            iz = 100-oz,
            swstr = 100*(sum(swstr)/n),
            whiff_pct = 100*(sum(whiff)/sum(swing, na.rm = TRUE)),
            csw = 100*((sum(cs) + sum(whiff))/n),
            gb = 100*(sum(gb, na.rm = TRUE)/sum(bip))) %>% 
  filter(details.type.description != "NA") %>% 
  mutate(across(where(is.numeric), round, 1))

pitcher_data <- pitcher_data %>% 
  mutate(league = "NA",
         team_short = gsub("\\s*\\([^\\)]+\\)","",as.character(team)))

for (i in 1:nrow(pitcher_data)) {
  if (pitcher_data$team_short[i] == "Jupiter Hammerheads" | pitcher_data$team_short[i] == "Palm Beach Cardinals" 
      | pitcher_data$team_short[i] == "St. Lucie Mets"| pitcher_data$team_short[i] == "Bradenton Marauders" | 
      pitcher_data$team_short[i] == "Clearwater Threshers" | pitcher_data$team_short[i] == "Dunedin Blue Jays" | 
      pitcher_data$team_short[i] == "Fort Myers Mighty Mussels" |
      pitcher_data$team_short[i] == "Lakeland Flying Tigers" | pitcher_data$team_short[i] == "Tampa Tarpons"
      | pitcher_data$team_short[i] == "Daytona Tortugas") {
    pitcher_data$league[i] <- "FSL"
  }
  if (pitcher_data$team_short[i] == "Albuquerque Isotopes" | pitcher_data$team_short[i] == "El Paso Chihuahuas" 
      | pitcher_data$team_short[i] == "Round Rock Express"| pitcher_data$team_short[i] == "Las Vegas Aviators" | 
      pitcher_data$team_short[i] == "Oklahoma City Dodgers" | pitcher_data$team_short[i] == "Sugar Land Space Cowboys" | 
      pitcher_data$team_short[i] == "Reno Aces" | pitcher_data$team_short[i] == "Sacramento River Cats" 
      | pitcher_data$team_short[i] == "Salt Lake Bees" | pitcher_data$team_short[i] == "Tacoma Rainiers") {
    pitcher_data$league[i] <- "PCL"
  }
  if (pitcher_data$team_short[i] == "Buffalo Bisons" | pitcher_data$team_short[i] == "Charlotte Knights" 
      | pitcher_data$team_short[i] == "Columbus Clippers" | pitcher_data$team_short[i] == "Durham Bulls" | 
      pitcher_data$team_short[i] == "Gwinnett Stripers" | pitcher_data$team_short[i] == "Indianapolis Indians" | 
      pitcher_data$team_short[i] == "Jacksonville Jumbo Shrimp" | pitcher_data$team_short[i] == "Lehigh Valley IronPigs" 
      | pitcher_data$team_short[i] == "Norfolk Tides" | pitcher_data$team_short[i] == "Rochester Red Wings" |
      pitcher_data$team_short[i] == "Scranton/Wilkes-Barre RailRiders" | pitcher_data$team_short[i] == "Syracuse Mets" 
      | pitcher_data$team_short[i] == "Toledo Mud Hens" | pitcher_data$team_short[i] == "Worcester Red Sox" | 
      pitcher_data$team_short[i] == "Louisville Bats" | pitcher_data$team_short[i] == "Nashville Sounds" | 
      pitcher_data$team_short[i] == "Memphis Redbirds" | pitcher_data$team_short[i] == "Omaha Storm Chasers" 
      | pitcher_data$team_short[i] == "Iowa Cubs" | pitcher_data$team_short[i] == "St. Paul Saints") {
    pitcher_data$league[i] <- "INT"
  }
}

pitcher_data <- pitcher_data %>% 
  mutate(y = ifelse(spindirection < 210, (spindirection/30) + 6, (spindirection/30) - 6),
         hour = floor(y),
         minute = round((y %% hour) *60),
         spindirection = ifelse(minute < 10, paste(hour, minute, sep=":0"),
                       paste(hour, minute, sep = ":"))) %>% 
  select(-y, -hour, -minute)


pitcher_data <- merge(pitcher_data, chadwick, by.x = "matchup.pitcher.id", by.y = "key_mlbam")

pitcher_data <- pitcher_data %>% 
  select(matchup.pitcher.fullName, age, team, league, everything()) %>% 
  select(-matchup.pitcher.id)

pitcher_data <- pitcher_data %>% 
  select(-team_short)

barrel <- ml_pbp %>% 
  select(hitData.launchAngle, hitData.launchSpeed)

colnames(barrel) <- c("launch_angle", "launch_speed")

barrel <- code_barrel(barrel)

barrel <- barrel %>% select(barrel)

ml_pbp <- cbind(ml_pbp, barrel)

batter_data <- ml_pbp %>% 
  group_by(matchup.batter.fullName, batting_team, matchup.batter.id) %>% 
  summarise(n = n(),
            bip = sum(bip, na.rm = TRUE),
            max_ev = max(hitData.launchSpeed, na.rm = TRUE),
            ev_95 = quantile(hitData.launchSpeed, 0.95, na.rm = TRUE),
            av_ev = mean(hitData.launchSpeed, na.rm = TRUE),
            la_swspt = 100* sum(sweet_spot, na.rm = TRUE)/sum(la_recorded, na.rm = TRUE),
            sd_la = sd(hitData.launchAngle, na.rm = TRUE),
            oz_swing = 100 * (sum(oz_swing, na.rm = TRUE)/sum(oz, na.rm = TRUE)),
            iz_contact = 100* (sum(iz_contact, na.rm = TRUE)/sum(iz_swing, na.rm = TRUE)),
            swing_rate = 100 * (sum(swing, na.rm = TRUE)/n),
            swstr = 100*(sum(swstr)/n),
            whiff = 100*(sum(whiff, na.rm = TRUE)/sum(swing, na.rm = TRUE)),
            hard = 100*(sum(hard, na.rm = TRUE)/sum(bip, na.rm = TRUE)),
            barrel = 100*sum(barrel, na.rm = TRUE)/sum(!is.na(barrel)),
            fb_rate = 100*sum(fb)/n(),
            fb_swstr = 100*sum(fb_swstr)/sum(fb),
            bb_rate = 100*sum(bb)/n(),
            bb_swstr = 100*sum(bb_swstr)/sum(bb),
            off_rate = 100*sum(off)/n(),
            off_swstr = 100*sum(off_swstr)/sum(off))%>%
  mutate(across(where(is.numeric), round, 1))

batter_data <- batter_data %>% 
  mutate(league = "NA",
         team_short = gsub("\\s*\\([^\\)]+\\)","",as.character(batting_team)))

for (i in 1:nrow(batter_data)) {
  if (batter_data$team_short[i] == "Jupiter Hammerheads" | batter_data$team_short[i] == "Palm Beach Cardinals" 
      | batter_data$team_short[i] == "St. Lucie Mets"| batter_data$team_short[i] == "Bradenton Marauders" | 
      batter_data$team_short[i] == "Clearwater Threshers" | batter_data$team_short[i] == "Dunedin Blue Jays" | 
      batter_data$team_short[i] == "Fort Myers Mighty Mussels" |
      batter_data$team_short[i] == "Lakeland Flying Tigers" | batter_data$team_short[i] == "Tampa Tarpons"
      | batter_data$team_short[i] == "Daytona Tortugas") {
    batter_data$league[i] <- "FSL"
  }
  if (batter_data$team_short[i] == "Albuquerque Isotopes" | batter_data$team_short[i] == "El Paso Chihuahuas" 
      | batter_data$team_short[i] == "Round Rock Express"| batter_data$team_short[i] == "Las Vegas Aviators" | 
      batter_data$team_short[i] == "Oklahoma City Dodgers" | batter_data$team_short[i] == "Sugar Land Space Cowboys" | 
      batter_data$team_short[i] == "Reno Aces" | batter_data$team_short[i] == "Sacramento River Cats" 
      | batter_data$team_short[i] == "Salt Lake Bees" | batter_data$team_short[i] == "Tacoma Rainiers") {
    batter_data$league[i] <- "PCL"
  }
  if (batter_data$team_short[i] == "Buffalo Bisons"  | batter_data$team_short[i] == "Charlotte Knights" 
      | batter_data$team_short[i] == "Columbus Clippers" | batter_data$team_short[i] == "Durham Bulls" | 
      batter_data$team_short[i] == "Gwinnett Stripers" | batter_data$team_short[i] == "Indianapolis Indians" | 
      batter_data$team_short[i] == "Jacksonville Jumbo Shrimp" | batter_data$team_short[i] == "Lehigh Valley IronPigs" 
      | batter_data$team_short[i] == "Norfolk Tides" | batter_data$team_short[i] == "Rochester Red Wings" |
      batter_data$team_short[i] == "Scranton/Wilkes-Barre RailRiders" | batter_data$team_short[i] == "Syracuse Mets" 
      | batter_data$team_short[i] == "Toledo Mud Hens" | batter_data$team_short[i] == "Worcester Red Sox" | 
      batter_data$team_short[i] == "Louisville Bats" | batter_data$team_short[i] == "Nashville Sounds" | 
      batter_data$team_short[i] == "Memphis Redbirds" | batter_data$team_short[i] == "Omaha Storm Chasers" 
      | batter_data$team_short[i] == "Iowa Cubs" | batter_data$team_short[i] == "St. Paul Saints") {
    batter_data$league[i] <- "INT"
  }
}

batter_data <- merge(batter_data, chadwick, by.x = "matchup.batter.id", by.y = "key_mlbam")

batter_data <- batter_data %>% 
  select(matchup.batter.fullName, age, batting_team, league, everything()) %>% 
  select(-matchup.batter.id, -team_short)


write_csv(ml_pbp, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/pbp.csv")
write_csv(pitcher_data, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/pitcher_data.csv")
write_csv(batter_data, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/batter_data.csv")


pitcher_data_2 <- ml_pbp %>%
  group_by(matchup.pitcher.fullName, details.type.description, game_date) %>% 
  summarise(filter = filter,
            pfx_x = mean(hb, na.rm = TRUE),
            pfx_z = mean(ivb, na.rm = TRUE),
            velo = mean(pitchData.startSpeed, na.rm = TRUE),
            spin = mean(pitchData.breaks.spinRate, na.rm = TRUE)) %>% 
  distinct()

pitcher_data_3 <- ml_pbp %>% 
  group_by(filter, game_date, details.type.description) %>%
  summarise(n = n()) %>% 
  ungroup() %>%
  group_by(filter, game_date) %>%
  mutate(pitch_percentage = n / sum(n) * 100) %>% 
  select(-n)

write_csv(pitcher_data_2, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/pitcher_data_2.csv")
write_csv(pitcher_data_3, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/pitcher_data_3.csv")

pitch_data <- ml_pbp %>% 
  select(filter, details.type.description, hb, ivb, pitchData.coordinates.pX, 
         pitchData.coordinates.pZ)

write_csv(pitch_data, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/pitch_data.csv")

bbe_data <- ml_pbp %>% 
  select(matchup.batter.fullName, hitData.coordinates.coordX, hitData.coordinates.coordY,
         hitData.launchSpeed, hitData.launchAngle)

write_csv(bbe_data, "/Users/samwirth/Downloads/aaa_statcast/aaa_statcast/aaa_statcast/bbe_data.csv")


pitching_teams <- as.data.frame(unique(ml_pbp$team))
write_csv(pitching_teams, 'team.csv')
batting_teams <- as.data.frame(unique(ml_pbp$batting_team))
write_csv(batting_teams, 'batting_team.csv')
batter_list <- as.data.frame(unique(ml_pbp$matchup.batter.fullName))
write_csv(batter_list, 'batter_names.csv')
pitcher_list <- as.data.frame(unique(ml_pbp$matchup.pitcher.fullName))
write_csv(pitcher_list, 'pitcher_names.csv')
