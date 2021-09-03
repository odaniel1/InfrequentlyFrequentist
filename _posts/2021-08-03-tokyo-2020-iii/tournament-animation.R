library(tidyverse)
library(gganimate)
library(fst)
library(infreqthemes)

source("~/Documents/R Projects/track-cycling/R/functions/forecasting.R")

olympic_rounds <- read_rds('~/Documents/R Projects/track-cycling/_targets/objects/fcst_rounds_Men')
strength_draw <- read_rds('~/Documents/R Projects/track-cycling/_targets/objects/fcst_strength_draws_Men') %>% filter(.draw == 1)  %>% mutate(time = 0)
tournament_draw <- forecast_tournament(strength_draw, olympic_rounds, samples = 1, accumulate = TRUE, gold_only = FALSE) %>% distinct()

rounds_long <- olympic_rounds %>%
  select(round, round_no, match_no, starts_with("rider_code")) %>%
  gather(rider_no, round_code, starts_with("rider_code"))

tournament_draw  <- tournament_draw %>%
  mutate(
    rider_surname = if_else(
      rider == "TJON EN FA JAIR", "TJON EN FA",
      str_match(rider, "(.*?) ")[,2]),
  )

# combine rounds/draw information
draw_rounds <- tournament_draw %>%
  select(rider_surname, round_code) %>%
  left_join(rounds_long) %>%
  group_by(rider_surname) %>%
  arrange(round_no) %>%
  # derived fields used for plotting
  mutate(
    # whether the rider is seeded 1st/2nd/3rd in the match
    rider_no = str_extract(rider_no, '\\d') %>% as.numeric,
    
    # what round they go on to (NA if out of competition)
    next_round = lead(round,1),
    next_next_round = lead(round,2),

    # the number/id of the next match, and whether they're seeded
    # 1st/2nd/3rd
    next_match_no = lead(match_no,1),
    next_rider_no = lead(rider_no,1)
  ) %>%
  ungroup()

# data set to derive the coordinates for each rider in each frame
# of the animation
rider_coords <- draw_rounds %>%
  group_by(round) %>%
  mutate(
    # frame showing riders in pairings for matches
    x_1 = rider_no/2 - 3/4,
    y_1 = min(match_no) - match_no,
    
    # frame moving the qualifiers of each match to the right side of the plot
    x_2 =
      if_else(round != 'Semifinals',
        case_when(
          is.na(next_round) ~ x_1,
          str_detect(next_round, 'Repechage') ~ x_1,
          TRUE ~ 2 - 1/4
        ),
        case_when(
          next_match_no != min(next_match_no) ~ x_1,
          TRUE ~ 2 - 1/4
        )
      ),
    
    y_2 = y_1,
    
    # frame moving those yet to qualify into position for repechage matches
    x_3 = case_when(
      !str_detect(next_round, 'Repechage') ~ x_2,
      TRUE ~  -1/4 + 1/2 * (next_rider_no - 1)  
    ),
    
    y_3 = case_when(
      !str_detect(next_round, 'Repechage') ~ y_2,
      TRUE ~ min(next_match_no) - next_match_no
    ),
    
    # frame moving the qualifiers from the repechage to the right side of the plot
    x_4 = 
      if_else(round != 'Semifinals',
        case_when(
          x_2 != x_1 ~ x_2,
          is.na(next_next_round) ~ x_3,
          TRUE ~ 2 - 1/4
        ),
        2 - 1/4
      ),
    
    y_4 = 
      if_else(round != 'Semifinals',
        case_when(
          x_2 != x_1 ~ y_2,
          is.na(next_next_round) ~ y_3,
          TRUE ~ min(y_2) + min(next_match_no) - next_match_no -1
        ),
        case_when(
          x_2 != x_1 ~ y_2,
          TRUE ~ y_2 - 2
        )
      ),
    
    # frame removing riders who did not qualify
    x_5 = case_when(
      x_4 == 2 - 1/4 ~ x_4,
      TRUE ~ NA_real_
    ),
    
    y_5 = case_when(
      x_4 == 2 - 1/4 ~ y_4,
      TRUE ~ NA_real_
    )
  ) %>%
  ungroup()


# repechages are picked up in the frame sequence above, so we don't need rows for 
# this data.
rider_coords <- rider_coords %>%
  filter(!is.na(round_no), !str_detect(round, 'Repechage')) %>%
  select(rider_surname, round, round_no, starts_with("x_"), starts_with("y_"))

# convert to long format so that there is a row per x/y pairing
rider_coords_long <- rider_coords %>%
  gather(coord_frame, value,starts_with("x_"), starts_with("y_")) %>%
  separate(coord_frame, c("coord", "subframe"), "_") %>%
  # these frames don't change from the previous so remove
  filter(
    !(round == "Quarterfinals" & subframe %in% c(3,4)),
    !(round == "Semifinals" & subframe %in% c(3,5)),
    !(round == "Finals" & subframe > 1)
  ) %>%
  # create a frame number that determines the animation order
  mutate(frame_no = round_no + (as.numeric(subframe)-1)/5) %>%
  # widen data to have a column for x/y values
  spread(coord, value)

# we are missing frames to show the eventual medal positions;
# this adds two more frames: one with the riders, and a second
# with text saying the medal position
podium_coords <- tournament_draw %>%
  filter(round_code %in% c("Gold", "Silver", "Bronze")) %>%
  crossing(frame_no = 1:2) %>%
  transmute(
    rider_surname,
    round_no = 10,
    frame_no = max(rider_coords_long$frame_no) + frame_no,
    medal = if_else(frame_no == max(frame_no), round_code, NA_character_),
    x = 1.75,
    y = case_when(round_code == "Gold" ~ -1,
                  round_code == "Silver" ~ -3,
                  round_code == "Bronze" ~ -5
        ),
    round = 'Finals'
  )

# combine data, and add a field for the qualifier text to appear on the plot
# this is Qualifiers except in the final round when it becomes Medalists.
rider_coords_long <- bind_rows(rider_coords_long, podium_coords) %>%
  mutate(
    qualifier_text = if_else(round != "Finals", "Qualifiers", "Medalists")
  )

p_body <- ggplot(rider_coords_long) + 
  # shaded column for qualifer names
  annotate(
    "rect", 
    xmin = 1.65, xmax = 2.3, ymin = -Inf, ymax = 0.5,
    alpha = 0.4, fill = infreq_palette["green"]
  ) +
  # qualifying header
  geom_text(
    aes(x = 2, y = 1,  label = qualifier_text),
    size = 7, color = infreq_palette["darkblue"]
  ) +
  # round header
  geom_text(
    aes(x=0, y= 1, label = round),
    size = 7, color = infreq_palette["darkblue"]
  ) +
  # athlete names
  geom_text(
    aes(x,y,label=rider_surname),
    hjust=0, size = 4.5, color = infreq_palette["darkblue"]
  ) +
  # medal text (for final frame only)
  geom_text(
    aes(x = 1.75, y = y - 0.65,  label = medal),
    hjust = 0,size = 4.5, color = infreq_palette["orange"]
  ) +
  # remove all axis elements,
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  # fix plot range
  coord_cartesian(xlim = c(-0.6,2.5)) +
  # animate by frame_no
  transition_states(frame_no,transition_length = 1, state_length = 2)

tournament_body <- animate(p_body,nframes = 12 * 33, height=600, width = 800)

anim_save('_posts/2021-08-03-tokyo-2020-iii/img/tournament_body.gif')


p_prev <-
  ggplot(rider_coords_long %>% filter(round_no >= 5) %>% mutate(x = if_else(x > 1.5, 1.25, x))) + 
  # shaded column for qualifer names
  annotate(
    "rect", 
    xmin = 1.2, xmax = 1.7, ymin = -Inf, ymax = 0.5,
    alpha = 0.4, fill = infreq_palette["green"]
  ) +
  # qualifying header
  geom_text(
    aes(x = 1.4, y = 1,  label = qualifier_text),
    size = 4, color = infreq_palette["darkblue"]
  ) +
  # round header
  geom_text(
    aes(x=0, y= 1, label = round),
    size = 4, color = infreq_palette["darkblue"]
  ) +
  # athlete names
  geom_text(
    aes(x,y,label=rider_surname),
    hjust=0, size = 3, color = infreq_palette["darkblue"]
  ) +
  # medal text (for final frame only)
  geom_text(
    aes(x = 1.25, y = y - 0.65,  label = medal),
    hjust = 0,size = 3, color = infreq_palette["orange"]
  ) +
  # remove all axis elements,
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  # fix plot range
  coord_cartesian(xlim = c(-0.3,1.7)) +
  # animate by frame_no
  transition_states(frame_no,transition_length = 1, state_length = 2)

tournament_prev <- animate(p_prev, nframes = 12 * 12, height=220, width = 400)

anim_save('_posts/2021-08-03-tokyo-2020-iii/img/tournament_prev.gif')
