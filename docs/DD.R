library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(scales)  # Format numbers with functions like comma(), percent(), and dollar()
library(modelsummary)  # Create side-by-side regression tables

## injury_raw <- read_csv("data/injury.csv")

injury_raw <- read_csv(here::here("docs", "data", "injury.csv"))

injury <- injury_raw %>%
  filter(ky == 1) %>%
  # The syntax for rename is `new_name = original_name`
  rename(duration = durat, log_duration = ldurat,
         after_1980 = afchnge)

ggplot(data = injury, aes(x = duration)) +
  # binwidth = 8 makes each column represent 2 months (8 weeks)
  # boundary = 0 make it so the 0-8 bar starts at 0 and isn't -4 to 4
  geom_histogram(binwidth = 8, color = "white", boundary = 0) +
  facet_wrap(vars(highearn))

ggplot(data = injury, mapping = aes(x = log_duration)) +
  geom_histogram(binwidth = 0.5, color = "white", boundary = 0) +
  # Uncomment this line if you want to exponentiate the logged values on the
  # x-axis. Instead of showing 1, 2, 3, etc., it'll show e^1, e^2, e^3, etc. and
  # make the labels more human readable
  # scale_x_continuous(labels = trans_format("exp", format = round)) +
  facet_wrap(vars(highearn))

ggplot(data = injury, mapping = aes(x = log_duration)) +
  geom_histogram(binwidth = 0.5, color = "white", boundary = 0) +
  facet_wrap(vars(after_1980))

ggplot(injury, aes(x = factor(highearn), y = log_duration)) +
  geom_point(size = 0.5, alpha = 0.2) +
  stat_summary(geom = "point", fun = "mean", size = 5, color = "red") +
  facet_wrap(vars(after_1980))

ggplot(injury, aes(x = factor(highearn), y = log_duration)) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(vars(after_1980))

plot_data <- injury %>%
  # Make these categories instead of 0/1 numbers so they look nicer in the plot
  mutate(highearn = factor(highearn, labels = c("Low earner", "High earner")),
         after_1980 = factor(after_1980, labels = c("Before 1980", "After 1980"))) %>%
  group_by(highearn, after_1980) %>%
  summarize(mean_duration = mean(log_duration),
            se_duration = sd(log_duration) / sqrt(n()),
            upper = mean_duration + (1.96 * se_duration),
            lower = mean_duration + (-1.96 * se_duration))

ggplot(plot_data, aes(x = highearn, y = mean_duration)) +
  geom_pointrange(aes(ymin = lower, ymax = upper),
                  color = "darkgreen", size = 1) +
  facet_wrap(vars(after_1980))

ggplot(plot_data, aes(x = after_1980, y = mean_duration, color = highearn)) +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1) +
  # The group = highearn here makes it so the lines go across categories
  geom_line(aes(group = highearn))

diffs <- injury %>%
  group_by(after_1980, highearn) %>%
  summarize(mean_duration = mean(log_duration),
            # Calculate average with regular duration too, just for fun
            mean_duration_for_humans = mean(duration))
diffs

before_treatment <- diffs %>%
  filter(after_1980 == 0, highearn == 1) %>%
  pull(mean_duration)

before_control <- diffs %>%
  filter(after_1980 == 0, highearn == 0) %>%
  pull(mean_duration)

after_treatment <- diffs %>%
  filter(after_1980 == 1, highearn == 1) %>%
  pull(mean_duration)

after_control <- diffs %>%
  filter(after_1980 == 1, highearn == 0) %>%
  pull(mean_duration)

diff_treatment_before_after <- after_treatment - before_treatment
diff_treatment_before_after

diff_control_before_after <- after_control - before_control
diff_control_before_after

diff_diff <- diff_treatment_before_after - diff_control_before_after
diff_diff

ggplot(diffs, aes(x = as.factor(after_1980),
                  y = mean_duration,
                  color = as.factor(highearn))) +
  geom_point() +
  geom_line(aes(group = as.factor(highearn))) +
  # If you use these lines you'll get some extra annotation lines and
  # labels. The annotate() function lets you put stuff on a ggplot that's not
  # part of a dataset. Normally with geom_line, geom_point, etc., you have to
  # plot data that is in columns. With annotate() you can specify your own x and
  # y values.
  annotate(geom = "segment", x = "0", xend = "1",
           y = before_treatment, yend = after_treatment - diff_diff,
           linetype = "dashed", color = "grey50") +
  annotate(geom = "segment", x = "1", xend = "1",
           y = after_treatment, yend = after_treatment - diff_diff,
           linetype = "dotted", color = "blue") +
  annotate(geom = "label", x = "1", y = after_treatment - (diff_diff / 2),
           label = "Program effect", size = 3)

# Here, all the as.factor changes are directly in the ggplot code. I generally
# don't like doing this and prefer to do that separately so there's less typing
# in the ggplot code, like this:
#
# diffs <- diffs %>%
#   mutate(after_1980 = as.factor(after_1980), highearn = as.factor(highearn))
#
# ggplot(diffs, aes(x = after_1980, y = avg_durat, color = highearn)) +
#   geom_line(aes(group = highearn))

model_small <- lm(log_duration ~ highearn + after_1980 + highearn * after_1980,
                  data = injury)
tidy(model_small)

# Convert industry and injury type to categories/factors
injury_fixed <- injury %>%
  mutate(indust = as.factor(indust),
         injtype = as.factor(injtype))

model_big <- lm(log_duration ~ highearn + after_1980 + highearn * after_1980 +
                  male + married + age + hosp + indust + injtype + lprewage,
                data = injury_fixed)
tidy(model_big)

# Extract just the diff-in-diff estimate
diff_diff_controls <- tidy(model_big) %>%
  filter(term == "highearn:after_1980") %>%
  pull(estimate)

## modelsummary(list("Simple" = model_small, "Full" = model_big))

gm <- modelsummary::gof_map %>%
  mutate(omit = !(raw %in% c("nobs", "r.squared", "adj.r.squared")))

modelsummary(list("Simple" = model_small, "Full" = model_big),
             gof_map = gm, stars = TRUE) %>%
  kableExtra::row_spec(7, background = "#F6D645") %>%
  kableExtra::row_spec(36, extra_css = "border-bottom: 1px solid")
