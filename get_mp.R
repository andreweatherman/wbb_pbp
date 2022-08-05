#### ------- get minutes played function
get_mp <- function(minutes, include_ot=FALSE) {
  seconds <- lubridate::as.duration(minutes*60)
  # pull master sub list
  master_sub <- readr::read_rds('clt_master_sub.rds')
  # if reg. only
  if(include_ot == FALSE) {
    # filter to inputted time
    data <- master_sub %>%
      mutate(in_regulation = case_when(quarter <= 4 ~ real_time,
                                       TRUE ~ in_regulation)) %>%
      filter(in_regulation <= seconds)
    # remove times when a player is subbed out at the time requested
    data <- data[!(data$in_regulation == as.duration(seconds) & data$sub == 'out'),]

    # add row if player is in game at the start time
    row_loop <- as.list(data %>% select(row))[[1]]
    add_data <- purrr::map_dfr(.x=row_loop[-1],
                               .f = function(x) {
                                 if(master_sub[x-1,]$row %!in% row_loop & subset(data, row==x)$pid == master_sub[x-1,]$pid & subset(data, row==x)$game_id == master_sub[x-1,]$game_id & master_sub[x-1,]$sub == 'in') {
                                   master_sub[x-1,] %>%
                                     mutate(quarter = if(minutes>=31) 1 else if(between(minutes, 21, 30)) 2  else if(between(minutes, 11, 20)) 3  else if(between(minutes, 0, 10)) 4,
                                            real_time = as.duration(seconds),
                                            in_regulation = as.duration(seconds),
                                            duration = if(minutes==40) lubridate::as.duration(600) else lubridate::as.duration((minutes-floor(minutes/10)*10)*60))
                                 }
                                 else {
                                   tibble()
                                 }
                               }
    )
    # add row for players who play the entire game
    data <- dplyr::bind_rows(data, add_data) %>%
      arrange(row)
    # remove any times where the below sub time is greater than above (error)
    data <- data %>% group_by(player, game_id) %>% filter(row_number() == 1 | in_regulation <= lag(in_regulation)) %>% ungroup()

    # calculate minutes
    game_min <- data %>%
      group_by(player, game_id) %>%
      mutate(
        i = row_number(),
        new_val = ifelse(i %% 2 == 1, in_regulation - lead(in_regulation), 0)
      ) %>%
      summarize(
        seconds=sum(new_val),
        minutes = lubridate::seconds_to_period(seconds),
      ) %>%
      arrange(desc(seconds)) %>%
      ungroup()

    # return minutes
    return(game_min)
  }
}
