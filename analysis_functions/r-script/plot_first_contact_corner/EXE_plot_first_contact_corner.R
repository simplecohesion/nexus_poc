####################
source("./lib/feyenoord_utils.R")
####################
## Function in depth
fun_plot_first_contact_corner <- function(input_team_name, output_path) {
  # Extract de xG data from the event data
  xg <- events %>%
    # filtering to only shots and grabbing the xG of each shot for joining up to the passes
    dplyr::filter(type.name == "Shot") %>%
    dplyr::select(
      id = shot.key_pass_id,
      shot.statsbomb_xg
    )

  # Set corner events
  corners <- events %>%
    # filtering to only corner passes
    dplyr::filter(pass.type.name == "Corner") %>%
    # joining up the xG from earlier to the corner passes, so now it's xG Assisted
    dplyr::left_join(xg) %>%
    # denoting which side of the pitch the corner came from
    dplyr::mutate(
      direction = ifelse(location.y < 40, "Corners from Left", "Corners from Right"),
      # denoting which zone the corner was played to
      short = ifelse(pass.length < 30 & (pass.technique.name != "Inswinging" & pass.technique.name != "Outswinging" | is.na(pass.technique.name)), TRUE, NA),
      deeper = ifelse(pass.end_location.x < 114, TRUE, NA),
      sixyardbox = ifelse(pass.end_location.x >= 114 & pass.end_location.y >= 30 & pass.end_location.y <= 50, TRUE, NA),
      # denoting whether corner ended near post/far post/middle
      endpoint = ifelse((direction == "Corners from Left" & pass.end_location.y >= 30 & pass.end_location.y <= 37) | (direction == "Corners from Right" & pass.end_location.y >= 43 & pass.end_location.y <= 50), "Near Post",
        ifelse((direction == "Corners from Left" & pass.end_location.y >= 43 & pass.end_location.y <= 50) | (direction == "Corners from Right" & pass.end_location.y >= 30 & pass.end_location.y <= 37), "Far Post",
          ifelse(pass.end_location.y >= 38 & pass.end_location.y <= 42, "Middle", NA)
        )
      )
    )

  shot.xg_left <- sum(dplyr::filter(xg, id %in% dplyr::filter(
    events, team.name == input_team_name,
    pass.type.name == "Corner", pass.assisted_shot_id != "<NA>",
    location.y < 40
  )$id)$shot.statsbomb_xg, na.rm = TRUE)
  shot.xg_right <- sum(dplyr::filter(xg, id %in% dplyr::filter(
    events, team.name == input_team_name,
    pass.type.name == "Corner", pass.assisted_shot_id != "<NA>",
    location.y > 40
  )$id)$shot.statsbomb_xg, na.rm = TRUE)
  #
  chart <- corners %>%
    # cutting the pitch up into bins for the heatmap later
    dplyr::mutate(
      xbin = cut(pass.end_location.x, breaks = seq(from = 0, to = 120, by = (120 / 40)), include.lowest = TRUE),
      ybin = cut(pass.end_location.y, breaks = seq(from = 0, to = 80, by = (80 / 30)), include.lowest = TRUE)
    ) %>%
    # grouping by team and which side of the pitch the cross came from
    dplyr::group_by(team.name, direction) %>%
    # summing the total number of corners and the xG/shots/goals from each side
    dplyr::mutate(
      corners = n(),
      xg = sum(shot.statsbomb_xg, na.rm = TRUE),
      shots = sum(pass.shot_assist == TRUE, na.rm = TRUE),
      goals = sum(pass.goal_assist == TRUE, na.rm = TRUE)
    ) %>%
    # doing the same as previously but now for each bin
    dplyr::group_by(team.name, direction, xbin, ybin) %>%
    dplyr::summarise(
      corners = max(corners),
      bin_corners = n(),
      bin_pct = bin_corners / corners,
      xg = max(xg),
      shots = max(shots),
      goals = max(goals),
      pass.end_location.x = median(pass.end_location.x),
      pass.end_location.y = median(pass.end_location.y),
    ) %>%
    dplyr::ungroup() %>%
    # filtering to a team of choice
    dplyr::filter(team.name == input_team_name)

  labels_data <- corners %>%
    dplyr::group_by(team.name, direction) %>%
    # summing how many of each type of corner the team played and then getting that as a % of total corners
    dplyr::summarise(
      corners = n(),
      six_yard_box = sum(sixyardbox == TRUE, na.rm = TRUE),
      six_yard_box_pct = six_yard_box / corners,
      deeper = sum(deeper == TRUE, na.rm = TRUE),
      deeper_pct = deeper / corners,
      short = sum(short == "TRUE", na.rm = TRUE),
      short_pct = short / corners,
      far_post = sum(endpoint == "Far Post", na.rm = TRUE),
      far_post_pct = far_post / corners,
      middle = sum(endpoint == "Middle", na.rm = TRUE),
      middle_pct = middle / corners,
      near_post = sum(endpoint == "Near Post", na.rm = TRUE),
      near_post_pct = near_post / corners
    ) %>%
    # filtering to our same team of choice
    dplyr::filter(team.name == input_team_name)

  cornercolours <- c(
    "#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51",
    "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195",
    "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2",
    "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd"
  )

  corners_viz_left <- ggplot() +
    # overlaying the heatmap
    geom_bin2d(
      data = subset(chart, direction == "Corners from Left"), aes(x = pass.end_location.x, y = pass.end_location.y, fill = bin_pct, group = bin_pct),
      binwidth = c((120 / 40), (80 / 30)), position = "identity", alpha = 0.8, show.legend = F
    ) +
    # drawing the pitch
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 120.5, ymin = 36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = -0.5, ymin = 36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6) +
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6) +
    theme(
      rect = element_blank(),
      line = element_blank()
    ) +
    annotate("point", x = 12, y = 40, colour = "black", size = 1.05) +
    # add penalty spot right
    annotate("point", x = 108, y = 40, colour = "black", size = 1.05) +
    annotate("path",
      colour = "black", size = 0.6,
      x = 60 + 10 * cos(seq(0, 2 * pi, length.out = 2000)),
      y = 40 + 10 * sin(seq(0, 2 * pi, length.out = 2000))
    ) +
    # add centre spot
    annotate("point", x = 60, y = 40, colour = "black", size = 1.05) +
    annotate("path",
      x = 12 + 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), size = 0.6,
      y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), col = "black"
    ) +
    annotate("path",
      x = 107.9 - 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), size = 0.6,
      y = 40 - 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), col = "black"
    ) +
    # drawing dashed lines on the pitch to denote the middle
    geom_segment(data = subset(chart, direction == "Corners from Left"), aes(x = 122, xend = 95, y = 38, yend = 38), linetype = "dashed") +
    geom_segment(data = subset(chart, direction == "Corners from Left"), aes(x = 122, xend = 95, y = 42, yend = 42), linetype = "dashed") +
    # side labels
    geom_text(
      data = subset(labels_data, direction == "Corners from Left"), aes(x = 114, y = 71, label = paste("Six Yard Box:", scales::percent(six_yard_box_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", size = 3.5, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(labels_data, direction == "Corners from Left"), aes(x = 104, y = 71, label = paste("Deeper:", scales::percent(deeper_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", size = 3.5, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(labels_data, direction == "Corners from Left"), aes(x = 94, y = 9, label = paste("Short:", scales::percent(short_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", size = 3.5, check_overlap = TRUE
    ) +
    # rotated labels
    geom_text(
      data = labels_data, aes(x = 86, y = 40, label = paste("Middle:", scales::percent(middle_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", angle = 90, size = 4, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(unique(labels_data), direction == "Corners from Left"),
      aes(x = 86, y = 30, label = paste("Near Post:", scales::percent(near_post_pct, accuracy = 0.1))), fontface = "bold", family = "Helvetica", angle = 90, size = 4, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(unique(labels_data), direction == "Corners from Left"),
      aes(x = 86, y = 50, label = paste("Far Post:", scales::percent(far_post_pct, accuracy = 0.1))), fontface = "bold", family = "Helvetica", angle = 90, size = 4, check_overlap = TRUE
    ) +
    # the 'title' and headline stats
    geom_text(
      data = subset(chart, direction == "Corners from Left"),
      aes(
        x = 131, y = 40,
        label = paste(direction, "\n", "Total Corners:", corners, "\n", "Goals:", goals, "|", "Shots:", shots, "|", "xG:", round(shot.xg_left, 2))
      ),
      fontface = "bold", family = "Helvetica", size = 4, check_overlap = TRUE
    ) +
    # drawing a red circle to show which side the corner is being taken from
    geom_point(data = subset(chart, direction == "Corners from Left"), aes(x = 120, y = ifelse(direction == "Corners from Left", 0, 80)), shape = 21, stroke = 2.1, colour = "#DC2228", size = 8) +
    # theming
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.caption = element_text(size = 13),
      plot.title.position = "plot",
      legend.title = element_text(size = 17, family = "Helvetica"),
      legend.text = element_text(size = 15, family = "Helvetica"),
      legend.key.size = unit(1, "cm"),
      legend.direction = "horizontal",
      legend.position = c(0.22, 0.28),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.spacing.x = unit(0.7, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      aspect.ratio = c(80 / 100),
      strip.text = element_blank(),
      strip.background = element_blank()
    ) +
    # applying the colours to the heatmap
    scale_fill_gradientn(
      colours = rev(cornercolours), labels = scales::percent_format(accuracy = 1),
      name = "% Difference In Retention\nvs League Average"
    ) +
    scale_y_continuous(expand = expansion(mult = .075)) +
    # flipping the pitch so it's vertical
    coord_flip(xlim = c(60, 135), clip = "off")

  corners_viz_right <- ggplot() +
    # overlaying the heatmap
    geom_bin2d(
      data = subset(chart, direction == "Corners from Right"), aes(x = pass.end_location.x, y = pass.end_location.y, fill = bin_pct, group = bin_pct),
      binwidth = c((120 / 40), (80 / 30)), position = "identity", alpha = 0.8, show.legend = F
    ) +
    # drawing the pitch
    annotate("rect", xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 120, xmax = 120.5, ymin = 36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("rect", xmin = 0, xmax = -0.5, ymin = 36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
    annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6) +
    annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6) +
    theme(
      rect = element_blank(),
      line = element_blank()
    ) +
    annotate("point", x = 12, y = 40, colour = "black", size = 1.05) +
    # add penalty spot right
    annotate("point", x = 108, y = 40, colour = "black", size = 1.05) +
    annotate("path",
      colour = "black", size = 0.6,
      x = 60 + 10 * cos(seq(0, 2 * pi, length.out = 2000)),
      y = 40 + 10 * sin(seq(0, 2 * pi, length.out = 2000))
    ) +
    # add centre spot
    annotate("point", x = 60, y = 40, colour = "black", size = 1.05) +
    annotate("path",
      x = 12 + 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), size = 0.6,
      y = 40 + 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), col = "black"
    ) +
    annotate("path",
      x = 107.9 - 10 * cos(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), size = 0.6,
      y = 40 - 10 * sin(seq(-0.3 * pi, 0.3 * pi, length.out = 30)), col = "black"
    ) +
    # drawing dashed lines on the pitch to denote the middle
    geom_segment(data = chart, aes(x = 122, xend = 95, y = 38, yend = 38), linetype = "dashed") +
    geom_segment(data = chart, aes(x = 122, xend = 95, y = 42, yend = 42), linetype = "dashed") +
    # side labels
    geom_text(
      data = subset(labels_data, direction == "Corners from Right"), aes(x = 114, y = 71, label = paste("Six Yard Box:", scales::percent(six_yard_box_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", size = 3.5, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(labels_data, direction == "Corners from Right"), aes(x = 104, y = 71, label = paste("Deeper:", scales::percent(deeper_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", size = 3.5, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(labels_data, direction == "Corners from Right"), aes(x = 94, y = 71, label = paste("Short:", scales::percent(short_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", size = 3.5, check_overlap = TRUE
    ) +
    # rotated labels
    geom_text(
      data = labels_data, aes(x = 86, y = 40, label = paste("Middle:", scales::percent(middle_pct, accuracy = 0.1))),
      fontface = "bold", family = "Helvetica", angle = 90, size = 4, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(unique(labels_data), direction == "Corners from Right"),
      aes(x = 86, y = 30, label = paste("Far Post:", scales::percent(far_post_pct, accuracy = 0.1))), fontface = "bold", family = "Helvetica", angle = 90, size = 4, check_overlap = TRUE
    ) +
    geom_text(
      data = subset(unique(labels_data), direction == "Corners from Right"),
      aes(x = 86, y = 50, label = paste("Near Post:", scales::percent(near_post_pct, accuracy = 0.1))), fontface = "bold", family = "Helvetica", angle = 90, size = 4, check_overlap = TRUE
    ) +
    # the 'title' and headline stats
    geom_text(
      data = subset(chart, direction == "Corners from Right"),
      aes(
        x = 131, y = 40,
        label = paste(direction, "\n", "Total Corners:", corners, "\n", "Goals:", goals, "|", "Shots:", shots, "|", "xG:", round(shot.xg_right, 2))
      ),
      fontface = "bold", family = "Helvetica", size = 4, check_overlap = TRUE
    ) +
    # drawing a red circle to show which side the corner is being taken from
    geom_point(data = subset(chart, direction == "Corners from Right"), aes(x = 120, y = ifelse(direction == "Corners from Left", 0, 80)), shape = 21, stroke = 2.1, colour = "#DC2228", size = 8) +
    # theming
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      plot.caption = element_text(size = 13),
      plot.title.position = "plot",
      legend.title = element_text(size = 17, family = "Helvetica"),
      legend.text = element_text(size = 15, family = "Helvetica"),
      legend.key.size = unit(1, "cm"),
      legend.direction = "horizontal",
      legend.position = c(0.22, 0.28),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.spacing.x = unit(0.7, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      aspect.ratio = c(80 / 100),
      strip.text = element_blank(),
      strip.background = element_blank()
    ) +
    # applying the colours to the heatmap
    scale_fill_gradientn(
      colours = rev(cornercolours), labels = scales::percent_format(accuracy = 1),
      name = "% Difference In Retention\nvs League Average"
    ) +
    scale_y_continuous(expand = expansion(mult = .075)) +
    # flipping the pitch so it's vertical
    coord_flip(xlim = c(60, 135), clip = "off")

  corner_takers_left <- corners %>%
    # grouping by player, team and the direction the corner was taken from, then summing how many they took
    dplyr::group_by(player.name, player.nickname, team.name, direction) %>%
    dplyr::summarise(taken = n()) %>%
    # grouping by team and getting the top 5 corner takers from each side
    dplyr::group_by(team.name, direction) %>%
    dplyr::slice_max(taken, n = 5, with_ties = F) %>%
    # filtering to the left-sided corners for the following plot
    dplyr::filter(team.name == input_team_name & direction == "Corners from Left")

  takers_chart_left <- ggplot() +
    # drawing the bars for the bar chart
    geom_bar(
      data = corner_takers_left, aes(x = taken, y = reorder(player.nickname, taken)),
      width = 0.7, stat = "identity", fill = "#DC2228", colour = "black"
    ) +
    # writing how many corners the player took on the bars
    geom_text(data = corner_takers_left, aes(x = ifelse(taken == 1, taken - 0.5, taken - 1), y = reorder(player.nickname, taken), label = taken), colour = "white", fontface = "bold", family = "Helvetica", size = 5, check_overlap = TRUE) +
    # titles
    geom_text(data = corner_takers_left, aes(x = max(taken) / 2, y = 6, label = paste("Top 5 Takers,", direction)), family = "Helvetica", fontface = "bold", size = 5, check_overlap = TRUE) +
    # theming
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 13, margin = unit(c(3, 0, 0, 0), "mm"), family = "Helvetica"),
      axis.text.x = element_text(size = 13, family = "Helvetica"),
      axis.text.y = element_text(size = 13, family = "Helvetica"),
      legend.title = element_text(size = 17, family = "Helvetica"),
      legend.text = element_text(size = 15, family = "Helvetica"),
      legend.key.size = unit(0.7, "cm"),
      legend.direction = "vertical",
      legend.position = c(0.8, 0.8),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.spacing.x = unit(1.7, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      strip.text = element_blank(),
      aspect.ratio = c(65 / 100),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      strip.background = element_blank()
    ) +
    # removing blank space along the x axis
    scale_x_continuous(limits = c(0, max(corner_takers_left$taken)), name = "Taken") +
    coord_cartesian(ylim = c(1, 6), clip = "off")

  corner_takers_right <- corners %>%
    # grouping by player, team and the direction the corner was taken from, then summing how many they took
    dplyr::group_by(player.name, player.nickname, team.name, direction) %>%
    dplyr::summarise(taken = n()) %>%
    # grouping by team and getting the top 5 corner takers from each side
    dplyr::group_by(team.name, direction) %>%
    dplyr::slice_max(taken, n = 5, with_ties = F) %>%
    # filtering to the right-sided corners for the following plot
    dplyr::filter(team.name == input_team_name & direction == "Corners from Right")

  takers_chart_right <- ggplot() +
    # drawing the bars for the bar chart
    geom_bar(
      data = corner_takers_right, aes(x = taken, y = reorder(player.nickname, taken)),
      width = 0.7, stat = "identity", fill = "#DC2228", colour = "black"
    ) +
    # writing how many corners the player took on the bars
    geom_text(data = corner_takers_right, aes(x = ifelse(taken == 1, taken - 0.5, taken - 1), y = reorder(player.nickname, taken), label = taken), colour = "white", fontface = "bold", family = "Helvetica", size = 5, check_overlap = TRUE) +
    # titles
    geom_text(data = corner_takers_right, aes(x = max(taken) / 2, y = 6, label = paste("Top 5 Takers,", direction)), family = "Helvetica", fontface = "bold", size = 5, check_overlap = TRUE) +
    # theming
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 13, margin = unit(c(3, 0, 0, 0), "mm"), family = "Helvetica"),
      axis.text.x = element_text(size = 13, family = "Helvetica"),
      axis.text.y = element_text(size = 13, family = "Helvetica"),
      legend.title = element_text(size = 17, family = "Helvetica"),
      legend.text = element_text(size = 15, family = "Helvetica"),
      legend.key.size = unit(0.7, "cm"),
      legend.direction = "vertical",
      legend.position = c(0.8, 0.8),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.spacing.x = unit(1.7, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      strip.text = element_blank(),
      aspect.ratio = c(65 / 100),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      strip.background = element_blank()
    ) +
    # removing blank space along the x axis
    scale_x_continuous(limits = c(0, max(corner_takers_right$taken)), name = "Taken") +
    coord_cartesian(ylim = c(1, 6), clip = "off")

  # getting player nicknames for joining up to the top pass recipients data
  nicknames <- events %>%
    dplyr::select(pass.recipient.id = player.id, player.nickname) %>%
    dplyr::distinct(pass.recipient.id, player.nickname)

  corner_first_contact_left <- corners %>%
    # filtering short corners and incomplete passes out
    dplyr::filter(is.na(pass.outcome.name) & is.na(short)) %>%
    # grouping by recipient, team and direction of corner and summing how many corners each player received
    dplyr::group_by(pass.recipient.id, pass.recipient.name, team.name, direction) %>%
    dplyr::summarise(first_contacts = n()) %>%
    # grouping by team and direction of corner then filtering to the top 5 recipients
    dplyr::group_by(team.name, direction) %>%
    dplyr::arrange(direction, -first_contacts) %>%
    dplyr::filter(row_number() <= 5) %>%
    dplyr::ungroup() %>%
    # joining up nicknames
    dplyr::left_join(nicknames) %>%
    # filtering to our team of choice and only corners from the left
    dplyr::filter(team.name == input_team_name & direction == "Corners from Left")

  first_contacts_chart_left <- ggplot() +
    # drawing the bars for the bar chart
    geom_bar(
      data = unique(corner_first_contact_left), aes(x = first_contacts, y = reorder(player.nickname, first_contacts)),
      width = 0.7, stat = "identity", fill = "#DC2228", colour = "black"
    ) +
    # writing how many corners the player received on the bars
    geom_text(data = corner_first_contact_left, aes(x = ifelse(first_contacts == 1, first_contacts - 0.5, first_contacts - 1), y = reorder(player.nickname, first_contacts), label = first_contacts), colour = "white", fontface = "bold", family = "Helvetica", size = 5, check_overlap = TRUE) +
    # titles
    geom_text(data = corner_first_contact_left, aes(x = max(first_contacts) / 2, y = 6, label = paste("Top 5 Point of First Contact,", direction, "(Short Corners Excluded)")), family = "Helvetica", fontface = "bold", size = 5, check_overlap = TRUE) +
    # theming
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 13, margin = unit(c(3, 0, 0, 0), "mm"), family = "Helvetica"),
      axis.text.x = element_text(size = 13, family = "Helvetica"),
      axis.text.y = element_text(size = 13, family = "Helvetica"),
      legend.title = element_text(size = 17, family = "Helvetica"),
      legend.text = element_text(size = 15, family = "Helvetica"),
      legend.key.size = unit(0.7, "cm"),
      legend.direction = "vertical",
      legend.position = c(0.8, 0.8),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.spacing.x = unit(1.7, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      strip.text = element_blank(),
      aspect.ratio = c(65 / 100),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      strip.background = element_blank()
    ) +
    # removing blank space along the x axis
    scale_x_continuous(
      limits = c(0, max(corner_first_contact_left$first_contacts)), name = "First Contacts",
      breaks = c(0, max(corner_first_contact_left$first_contacts) / 2, max(corner_first_contact_left$first_contacts))
    ) +
    coord_cartesian(ylim = c(1, 6), clip = "off")

  corner_first_contact_right <- corners %>%
    # filtering short corners and incomplete passes out
    dplyr::filter(is.na(pass.outcome.name) & is.na(short)) %>%
    # grouping by recipient, team and direction of corner and summing how many corners each player received
    dplyr::group_by(pass.recipient.id, pass.recipient.name, team.name, direction) %>%
    dplyr::summarise(first_contacts = n()) %>%
    # grouping by team and direction of corner then filtering to the top 5 recipients
    dplyr::group_by(team.name, direction) %>%
    dplyr::arrange(direction, -first_contacts) %>%
    dplyr::filter(row_number() <= 5) %>%
    dplyr::ungroup() %>%
    # joining up nicknames
    dplyr::left_join(nicknames) %>%
    # filtering to our team of choice and only corners from the left
    dplyr::filter(team.name == input_team_name & direction == "Corners from Right")

  first_contacts_chart_right <- ggplot() +
    # drawing the bars for the bar chart
    geom_bar(
      data = unique(corner_first_contact_right), aes(x = first_contacts, y = reorder(player.nickname, first_contacts)),
      width = 0.7, stat = "identity", fill = "#DC2228", colour = "black"
    ) +
    # writing how many corners the player received on the bars
    geom_text(data = corner_first_contact_right, aes(x = ifelse(first_contacts == 1, first_contacts - 0.5, first_contacts - 1), y = reorder(player.nickname, first_contacts), label = first_contacts), colour = "white", fontface = "bold", family = "Helvetica", size = 5, check_overlap = TRUE) +
    # titles
    geom_text(data = corner_first_contact_right, aes(x = max(first_contacts) / 2, y = 6, label = paste("Top 5 Point of First Contact,", direction, "(Short Corners Excluded)")), family = "Helvetica", fontface = "bold", size = 5, check_overlap = TRUE) +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 13, margin = unit(c(3, 0, 0, 0), "mm"), family = "Helvetica"),
      axis.text.x = element_text(size = 13, family = "Helvetica"),
      axis.text.y = element_text(size = 13, family = "Helvetica"),
      legend.title = element_text(size = 17, family = "Helvetica"),
      legend.text = element_text(size = 15, family = "Helvetica"),
      legend.key.size = unit(0.7, "cm"),
      legend.direction = "vertical",
      legend.position = c(0.8, 0.8),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = NA),
      legend.key = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      panel.spacing.x = unit(1.7, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      strip.text = element_blank(),
      aspect.ratio = c(65 / 100),
      plot.margin = grid::unit(c(0, 0, 0, 0), "mm"),
      strip.background = element_blank()
    ) +
    # removing blank space along the x axis
    scale_x_continuous(
      limits = c(0, max(corner_first_contact_right$first_contacts)), name = "First Contacts",
      breaks = c(0, max(corner_first_contact_right$first_contacts) / 2, max(corner_first_contact_right$first_contacts))
    ) +
    coord_cartesian(ylim = c(1, 6), clip = "off")

  # patching our charts together in the correct order
  fc_final_plot <- wrap_plots(takers_chart_left | corners_viz_left | first_contacts_chart_left) / wrap_plots(takers_chart_right | corners_viz_right | first_contacts_chart_right) +
    # writing the title/subtitle and giving them theming
    plot_annotation(
      title = paste0(input_team_name, " - ", input_season_name),
      subtitle = "Goals, Shots and xG data refer to first point of contact from corner",
      theme = theme(
        plot.title = element_text(size = 22, family = "Helvetica", face = "bold"),
        plot.subtitle = element_text(size = 20, family = "Helvetica", face = "bold")
      )
    )
  ## OUTPUT
  ggsave(fc_final_plot,
    filename = paste0(
      output_path, "/First_Contact_Corners_", input_team_name,
      ".png"
    ), # bg = "transparent",
    width = 28, height = 12, units = "in"
  )
}

########################################################################################################
########################################################################################################
