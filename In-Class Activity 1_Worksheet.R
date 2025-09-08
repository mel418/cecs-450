################################################################################
# In-Class Exercise: Reshaping & Visualizing Data with ggplot2 (Walking Dead)
# Course: (fill in)
# Timebox: ~35–45 minutes
#
# What you'll practice
# 1) Building a data.frame and adding columns
# 2) Reshaping wide → long with reshape2::melt
# 3) Making histograms, density overlays, vertical reference lines
# 4) Line/point plots; aesthetics: color, linetype, shape, group
# 5) Scales, labels, themes, facets
#
# Rules
# - Only type where it says: ### YOUR CODE HERE ###
# - After each TASK, run the code and use the CHECK to verify results.
################################################################################


######################################
# 0) Setup & Data
######################################

# TASK 0.1 — Load packages you’ll need.
#   - ggplot2
#   - (later) reshape2
# HINT: library(<pkg>)
library(ggplot2)
library(reshape2)

# TASK 0.2 — Start with a clean workspace.
# HINT: rm(list = ls())
rm(list = ls())

# TASK 0.3 — Create the 'kills' data.frame with 8 character columns (Rick … Maggie),
#            each containing 6 numbers (some are NA). Do NOT add Season yet.
# HINT: data.frame(Rick = c( … ), Morgan = c( … ), … )
kills <- data.frame(
  Rick = c(36, 23, 59, 36, 36, 66),
  Morgan = c(3, NA, 2, NA, 5, 33),
  Glen = c(4, 10, 22, 29, 73, 44),
  Daryl = c(8, 19, 41, 68, 43, 41),
  Carl = c(0, 17, 18, 17, 8, 2),
  Michonne = c(NA, 1, 29, 46, 35, 42),
  Carol = c(0, 0, 8, 10, 37, 29),
  Maggie = c(NA, 2, 16, 32, 9, 12)
)

# CHECK 0 — Print kills and confirm it has 6 rows × 8 columns and some NAs.
print(kills)
dim(kills)

# TASK 0.4 — Add a Season column: values 1 through 6.
# HINT: kills$Season <- 1:6
kills$Season <- 1:6

# CHECK 0.4 — View kills; Season should be the 9th column.
print(kills)


######################################
# 1) Reshape wide → long with reshape2::melt
######################################

# TASK 1.1 — Load reshape2.
library(reshape2)

# TASK 1.2 — Melt kills into long format with:
#   id.vars      = "Season"
#   variable.name= "Character"
#   value.name   = "nKills"
# Save as kills.long.
# HINT: melt(kills, id.vars = "Season", variable.name = "Character", value.name = "nKills")
kills.long <- melt(kills, id.vars = "Season", variable.name = "Character", value.name = "nKills")

# CHECK 1.2 — Print kills.long. It should have 6*8 = 48 rows and 3 columns.
print(kills.long)

# TASK 1.3 — Repeat the melt but only for the first 3 characters (columns 1:3).
# Save (overwriting) kills.long.
# HINT: measure.vars = colnames(kills)[1:3]
kills.long <- melt(kills, id.vars = "Season", 
                   measure.vars = colnames(kills)[1:3],
                   variable.name = "Character", 
                   value.name = "nKills")

# CHECK 1.3 — Print kills.long; now it should have 6*3 = 18 rows.
print(kills.long)

# (OPTIONAL) Undo the restriction and melt again for all 8 characters for the
# remainder of the exercise.
kills.long <- melt(kills, id.vars = "Season", variable.name = "Character", value.name = "nKills")


######################################
# 2) Histograms (univariate)
######################################

# TASK 2.1 — Basic histogram of nKills from kills.long.
# HINT: ggplot(kills.long, aes(x = nKills)) + geom_histogram()
ggplot(kills.long, aes(x = nKills)) + geom_histogram()

# TASK 2.2 — Customize color, fill, and binwidth (col="red", fill="blue", binwidth=10).
ggplot(kills.long, aes(x = nKills)) + 
  geom_histogram(col="red", fill="blue", binwidth=10)

# TASK 2.3 — Use fixed breaks: breaks = seq(5, 85, 13.5), fill = "orange", alpha = 0.25.
ggplot(kills.long, aes(x = nKills)) + 
  geom_histogram(breaks = seq(5, 85, 13.5), fill = "orange", alpha = 0.25)

# TASK 2.4 — Plot density on y-axis (aes(y = ..density..)) with the same bins.
ggplot(kills.long, aes(x = nKills, y = ..density..)) + 
  geom_histogram(breaks = seq(5, 85, 13.5), fill = "orange", alpha = 0.25)


######################################
# 3) Layering: histogram + density + vertical line
######################################

# TASK 3.1 — Make a histogram (density scaled) and add a density curve layer.
# HINT: + geom_density(col = "red", size = 2)
ggplot(kills.long, aes(x = nKills, y = ..density..)) + 
  geom_histogram(breaks = seq(5, 85, 13.5), fill = "orange", alpha = 0.25) +
  geom_density(col = "red", size = 2)

# TASK 3.2 — Add a vertical dashed line at the median of nKills (ignore NAs).
# Steps:
#   a) Compute medval <- median(kills.long$nKills, na.rm = TRUE)
#   b) Add + geom_vline(xintercept = medval, col="purple", linetype="dashed", size=1.5)
medval <- median(kills.long$nKills, na.rm = TRUE)
ggplot(kills.long, aes(x = nKills, y = ..density..)) + 
  geom_histogram(breaks = seq(5, 85, 13.5), fill = "orange", alpha = 0.25) +
  geom_density(col = "red", size = 2) +
  geom_vline(xintercept = medval, col="purple", linetype="dashed", size=1.5)

# CHECK 3 — Do you see the purple dashed line near the middle of the density?
print(paste("The median number of kills is:", medval))


######################################
# 4) Season vs. nKills: separate simple plots
######################################

# TASK 4.1 — Line plot of Season (x) vs nKills (y).
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line()

# TASK 4.2 — Point plot of Season vs nKills.
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_point()


######################################
# 5) Season vs. nKills: groups & aesthetics
######################################

# TASK 5.1 — Line plot grouping by Character (so each character has a separate line).
# HINT: aes(group = Character)
ggplot(kills.long, aes(x = Season, y = nKills, group = Character)) + geom_line()

# TASK 5.2 — Same line plot but color lines by Character.
ggplot(kills.long, aes(x = Season, y = nKills, color = Character)) + geom_line()

# TASK 5.3 — Same line plot but map linetype to Character.
ggplot(kills.long, aes(x = Season, y = nKills, linetype = Character)) + geom_line()


######################################
# 6) Global vs. mapped aesthetics
######################################

# TASK 6.1 — Map color to Character inside aes(...).
ggplot(kills.long, aes(x = Season, y = nKills, color = Character)) + geom_line()

# TASK 6.2 — Set a fixed color for all lines (e.g., "green") outside aes(...).
ggplot(kills.long, aes(x = Season, y = nKills)) + geom_line(color = "green")

# TASK 6.3 — Group by Character but keep a fixed color of "green".
ggplot(kills.long, aes(x = Season, y = nKills, group = Character)) + geom_line(color = "green")


######################################
# 7) Derived variable (Gender) + shapes + manual palette + title
######################################

# TASK 7.1 — Create a Gender column:
#   - M for Rick, Morgan, Glen, Daryl, Carl
#   - F for everyone else (Michonne, Carol, Maggie)
# HINT: ifelse(kills.long$Character %in% c("Rick","Morgan","Glen","Daryl","Carl"), "M","F")
kills.long$Gender <- ifelse(kills.long$Character %in% c("Rick","Morgan","Glen","Daryl","Carl"), "M", "F")
head(kills.long, 12)  # Look at first 12 rows to see both Rick (M) and Morgan (M)


# TASK 7.2 — Plot Season vs nKills:
#   - Lines grouped by Character, color by Gender
#   - Points with shape mapped to Character and color to Gender
#   - Use scale_shape_manual(values = 1:8)
ggplot(kills.long, aes(x = Season, y = nKills)) +
  geom_line(aes(group = Character, color = Gender)) +
  geom_point(aes(shape = Character, color = Gender)) +
  scale_shape_manual(values = 1:8)

# TASK 7.3 — Repeat with thinner lines or bigger points (size=4) to see the effect.
ggplot(kills.long, aes(x = Season, y = nKills)) +
  geom_line(aes(group = Character, color = Gender), size = 0.5) +
  geom_point(aes(shape = Character, color = Gender), size = 4) +
  scale_shape_manual(values = 1:8)

# TASK 7.4 — Make a histogram of nKills; add a title and axis labels via labs().
#   title = "Walking Dead S1–S6", x = "# of Kills", y = "Frequency"
ggplot(kills.long, aes(x = nKills)) +
  geom_histogram(binwidth = 10) +
  labs(title = "Walking Dead S1–S6", x = "# of Kills", y = "Frequency")


######################################
# 8) Manual color scales + legend labels
######################################

# TASK 8.1 — Season vs nKills line plot colored by Gender and grouped by Character.
ggplot(kills.long, aes(x = Season, y = nKills, color = Gender, group = Character)) + 
  geom_line()

# TASK 8.2 — Apply a manual color scale with values = c("green","black").
ggplot(kills.long, aes(x = Season, y = nKills, color = Gender, group = Character)) + 
  geom_line() +
  scale_color_manual(values = c("green", "black"))

# TASK 8.3 — Keep the same colors but control legend order with breaks = c("M","F").
ggplot(kills.long, aes(x = Season, y = nKills, color = Gender, group = Character)) + 
  geom_line() +
  scale_color_manual(values = c("green", "black"), breaks = c("M", "F"))

# TASK 8.4 — Add labels for legend entries: labels = c("Men","Women").
ggplot(kills.long, aes(x = Season, y = nKills, color = Gender, group = Character)) + 
  geom_line() +
  scale_color_manual(values = c("green", "black"), breaks = c("M", "F"), labels = c("Men", "Women"))


######################################
# 9) Shapes + filled histograms by Gender
######################################

# TASK 9.1 — Season vs nKills with:
#   - lines (group=Character, color=Gender)
#   - points (shape=Character, color=Gender)
#   - scale_shape_manual(values = c(1,2,5,3,4,16,17,18))
#   - scale_colour_manual(values = c("green","black"))
ggplot(kills.long, aes(x = Season, y = nKills)) +
  geom_line(aes(group = Character, color = Gender)) +
  geom_point(aes(shape = Character, color = Gender)) +
  scale_shape_manual(values = c(1,2,5,3,4,16,17,18)) +
  scale_colour_manual(values = c("green","black"))

# TASK 9.2 — Histogram of nKills filled by Gender, col = "yellow", binwidth = 10.
ggplot(kills.long, aes(x = nKills, fill = Gender)) +
  geom_histogram(col = "yellow", binwidth = 10)

# TASK 9.3 — Same histogram but manually set fill colors to c("blue","green").
ggplot(kills.long, aes(x = nKills, fill = Gender)) +
  geom_histogram(col = "yellow", binwidth = 10) +
  scale_fill_manual(values = c("blue", "green"))

# TASK 9.4 — Same histogram but position = "dodge" to separate bars.
ggplot(kills.long, aes(x = nKills, fill = Gender)) +
  geom_histogram(col = "yellow", binwidth = 10, position = "dodge") +
  scale_fill_manual(values = c("blue", "green"))


######################################
# 10) Axis scales & boxplots
######################################

# TASK 10.1 — Season vs nKills line plot with:
#   scale_x_continuous(limits = c(0.5, 7.5), breaks = 1:6, labels = c(1:5, "six"))
ggplot(kills.long, aes(x = Season, y = nKills, color = Gender, group = Character)) + 
  geom_line() +
  scale_x_continuous(limits = c(0.5, 7.5), breaks = 1:6, labels = c(1:5, "six"))
  
# TASK 10.2 — Boxplot of nKills by Gender.
### YOUR CODE HERE ###

# TASK 10.3 — (Optional) Rename the x-axis categories to "Women" and "Men" via
#   scale_x_discrete(labels = c("Women","Men"))
### YOUR CODE HERE ###



######################################
# 11) Themes, titles, and legend positioning
######################################

# TASK 11.1 — Histogram with custom theme:
#   - axis.text size 18, color "green", angle 45
#   - y-axis title family "mono", size 24
### YOUR CODE HERE ###

# TASK 11.2 — Season vs nKills lines with color = Gender and a custom legend title
#   via scale_colour_discrete(name = "New Title"), and legend positioned at (0.15, 0.85).
### YOUR CODE HERE ###

# TASK 11.3 — Season vs nKills lines with the entire y-axis removed (title, text, ticks).
#   HINT: theme(axis.title.y = element_blank(),
#               axis.text.y  = element_blank(),
#               axis.ticks.y = element_blank())
### YOUR CODE HERE ###



######################################
# 12) Columns + facets
######################################

# TASK 12.1 — Look at the range of nKills (rounded) ignoring NAs.
# HINT: round(range(kills.long$nKills, na.rm = TRUE))
### YOUR CODE HERE ###

# TASK 12.2 — Make a column chart (geom_col) of nKills by Character:
#   - y-axis 0 to 75 with breaks at 0, 25, 50, 75
#   - x-axis text rotated 90 degrees
#   - facet by Season with 2 columns
# HINT: scale_y_continuous(limits = c(0, 75), breaks = c(0, 25, 50, 75))
#       theme(axis.text.x = element_text(angle = 90))
#       facet_wrap(~Season, ncol = 2)
### YOUR CODE HERE ###



################################################################################
# Stretch Goals (if you finish early)
# A) Replace reshape2::melt with tidyr::pivot_longer and compare.
# B) Compute per-Gender median nKills and add per-Gender vertical lines.
# C) Try a minimal theme: + theme_minimal() and adjust text sizes.
################################################################################