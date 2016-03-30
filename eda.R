library(magrittr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
library(ggplot2)
library(cowplot)
library(purrr)
library(GGally) # install.packages("GGally")

events <- dplyr::tbl_df(as.data.frame(readr::read_rds("data/phrase_boost_test_EL.rds")))
events %<>% keep_where(session_id != "")
events$action_id <- NULL
events %<>% keep_where(date > "2016-03-14")
nonzero_session_ids <- events %>%
  group_by(session_id) %>%
  summarize(any_results = any(results_returned > 0)) %>%
  keep_where(any_results) %>%
  { .$session_id }
clickthroughed_session_ids <- events %>%
  group_by(session_id) %>%
  summarize(clickthrough = any(action %in% "visitPage")) %>%
  keep_where(clickthrough) %>%
  { .$session_id }

dir.create("figures")

p <- events %>%
  keep_where(!is.na(results_returned) & results_returned > 0) %>%
  dplyr::distinct(date, session_id, test_group) %>%
  group_by(date, group = test_group) %>%
  summarize(sessions = n()) %>%
  ggplot(aes(x = date, y = sessions, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a (%d %b)") +
  labs(title = "Sampling of test groups over time") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("sampling.png", plot = p, path = "figures", width = 8, height = 4)

## Clickthroughs
ctr_by_group <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id) %>%
  summarize(group = head(test_group, 1),
            clickthrough = any(action %in% "visitPage")) %>%
  group_by(group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough))
ctr_daily_by_group <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(date, session_id) %>%
  summarize(group = head(test_group, 1),
            clickthrough = any(action %in% "visitPage")) %>%
  group_by(date, group) %>%
  summarize(ctr = sum(clickthrough)/length(clickthrough))
p <- ggplot(data = ctr_daily_by_group, aes(x = date, y = ctr, color = group)) +
  geom_hline(data = ctr_by_group, aes(yintercept = ctr, color = group), linetype = "dashed") +
  geom_line(size = 1.5) + geom_point(size = 2) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a (%d %b)") +
  scale_y_continuous("Clickthrough rate", labels = scales::percent_format()) +
  geom_text(aes(label = sprintf("%.1f%%", 100*ctr)), color = "black", fontface = "bold",
            vjust = -1) +
  geom_text(data = ctr_by_group, aes(x = lubridate::ymd("2016-03-15"),
                                     y = ctr, color = group,
                                     label = sprintf("%.1f%% overall", 100*ctr)),
            vjust = -1, hjust = -0.05) +
  labs(title = "Proportions of sessions where user clicked on a result") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("daily_ctr.png", p, path = "figures", width = 10, height = 6)

## Number of clickthroughs
ctr_counts_by_group <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id) %>%
  summarize(group = head(test_group, 1),
            clickthroughs = sum(action %in% "visitPage")) %>%
  group_by(group, clickthroughs) %>%
  summarize(sessions = n())
p <- ctr_counts_by_group %>%
  mutate(clickthroughs = ifelse(clickthroughs < 4, clickthroughs, "4+")) %>%
  group_by(group, clickthroughs) %>%
  summarize(sessions = sum(sessions)) %>%
  group_by(Group = group) %>%
  mutate(prop = sessions/sum(sessions)) %>%
  ggplot(aes(x = clickthroughs, y = prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("Proportion of sessions", labels = scales::percent_format()) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), position = position_dodge(width = 1), vjust = -1) +
  labs(title = "How number of results opened per session varies by group",
       x = "Number of pages visited per session") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("pages_visited.png", p, path = "figures", width = 12, height = 6)

## Time to first clickthrough
time_to_clickthrough <- events %>%
  keep_where(session_id %in% nonzero_session_ids & session_id %in% clickthroughed_session_ids) %>%
  group_by(session_id) %>%
  arrange(ts) %>%
  summarize(Group = head(test_group, 1),
            Landing = head(ts, 1),
            `First clickthrough` = (function(x, y) {
              return(head(y[which(x == "visitPage")], 1))
            })(action, ts)) %>%
  mutate(`Time to first clickthrough` = as.integer(difftime(`First clickthrough`, `Landing`, units = "secs"))) %>%
  select(-c(`session_id`, `Landing`, `First clickthrough`)) %>%
  keep_where(`Time to first clickthrough` <= 120 & `Time to first clickthrough` > 0)
# Discard sessions where it took >2min or 0s to click on a result
p1 <- time_to_clickthrough %>%
  ggplot(aes(y = `Time to first clickthrough`, x = Group, fill = Group)) +
  geom_violin(trim = FALSE, adjust = 2, draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(y = "Time to first clickthrough (seconds)", title = "Time to first clickthrough") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
p2 <- time_to_clickthrough %>%
  ggplot(aes(y = log(`Time to first clickthrough`), x = Group, fill = Group)) +
  geom_violin(trim = FALSE, adjust = 2, draw_quantiles = c(0.25, 0.5, 0.75)) +
  labs(y = "Seconds to first clickthrough (log-transformed)", title = "Log-transformed time to clickthrough") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
(p <- plot_grid(p1, p2, ncol = 2))
ggsave("first_clickthrough.png", p, path = "figures", width = 12, height = 6)

## Number of searches per session
searches_by_group <- events %>%
  group_by(session_id) %>%
  summarize(Group = head(test_group, 1),
            SERPs = sum(action %in% "searchResultPage")) %>%
  group_by(Group, SERPs) %>%
  summarize(sessions = n()) %>%
  keep_where(SERPs > 0)
p <- searches_by_group %>%
  group_by(Group) %>%
  mutate(SERPs = ifelse(SERPs < 10, SERPs, "10+")) %>%
  group_by(Group, SERPs) %>%
  summarize(sessions = sum(sessions)) %>%
  group_by(Group) %>%
  mutate(prop = sessions/sum(sessions)) %>%
  ggplot(aes(x = factor(SERPs, levels = c(1:9, "10+")), y = prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("Proportion of sessions within group", labels = scales::percent_format()) +
  geom_text(aes(label = sprintf("%.1f", 100*prop), color = Group),
            position = position_dodge(width = 1), vjust = -1, fontface = "bold") +
  labs(title = "Number of search engine result pages (SERPs) opened per session",
       x = "Number of SERPs opened (searches performed)") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("searches.png", p, path = "figures", width = 12, height = 6)
rm(p1, p2, p)

## Position of first & second clicked results
first_click <- events %>%
  keep_where(!is.na(result_position)) %>%
  group_by(session_id) %>%
  arrange(ts) %>%
  summarize(group = head(test_group, 1),
            position = head(result_position, 1)) %>%
  group_by(group, position) %>%
  summarize(sessions = n())
p <- first_click %>%
  mutate(position = ifelse(position < 11, position, "11+")) %>%
  group_by(Group = group, position) %>%
  summarize(sessions = sum(sessions)) %>%
  mutate(prop = sessions/sum(sessions)) %>%
  ggplot(aes(x = factor(position, levels = c(1:10, "11+")), y = prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous("Proportion of sessions within group", labels = scales::percent_format()) +
  geom_text(aes(label = sprintf("%.1f", 100*prop), color = Group),
            position = position_dodge(width = 1), vjust = -1, fontface = "bold") +
  labs(title = "First clicked result's position on search engine result page",
       x = "Position of the first clicked result on SERP") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("first_position.png", p, path = "figures", width = 12, height = 6)

clicked_position <- events %>%
  keep_where(!is.na(result_position)) %>%
  group_by(session_id) %>%
  arrange(ts) %>%
  summarize(group = head(test_group, 1),
            first = head(result_position, 1),
            second = tail(head(result_position, 2), 1)) %>%
  tidyr::gather(click, position, -c(1,2)) %>%
  group_by(group, click, position) %>%
  summarize(sessions = n())

p <- clicked_position %>%
  mutate(position = ifelse(position < 11, position, "11+")) %>%
  group_by(Group = group, click, position) %>%
  summarize(sessions = sum(sessions)) %>%
  mutate(prop = sessions/sum(sessions)) %>%
  ungroup %>%
  mutate(click = factor(click, labels = c("First clicked result", "Second clicked result"))) %>%
  ggplot(aes(x = factor(position, levels = c(1:10, "11+")), y = prop, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~click, nrow = 2) +
  scale_y_continuous("Proportion of sessions within group", labels = scales::percent_format()) +
  geom_text(aes(label = sprintf("%.1f", 100*prop), color = Group),
            position = position_dodge(width = 1), vjust = -1, fontface = "bold") +
  labs(title = "Position of 1st and 2nd clicked results on search engine result page",
       x = "Position of the clicked result on SERP") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("clicked_positions.png", p, path = "figures", width = 10, height = 12)

## Session duration
session_lengths <- events %>%
  group_by(date, session_id) %>%
  arrange(ts) %>%
  summarize(group = head(test_group, 1),
            session_length = as.integer(difftime(tail(ts, 1), head(ts, 1), units = "secs"))) %>%
  ungroup
p <- session_lengths %>%
  keep_where(session_length > 0) %>%
  ggplot(aes(x = session_length, fill = group)) +
  geom_density(alpha = 0.5, adjust = 2) +
  scale_x_log10("Session Length (on log-10 scale)") +
  labs(title = "Distribution of log10-scaled session lengths", y = "Density") +
  ggthemes::theme_tufte(base_family = "Gill Sans") +
  theme(legend.position = "bottom", panel.grid = element_line(color = "black", size = 0.1))
ggsave("session_duration.png", p, path = "figures", width = 10, height = 6)

next_checkin <- function(x) {
  return(sapply(x, function(y) {
    checkins <- c(0, 10, 20, 30, 40, 50, 60, 90, 120, 150, 180, 210, 240, 300, 360, 420)
    idx <- which(checkins > y)
    if (length(idx) == 0) idx <- 16 # length(checkins) = 16
    return(checkins[min(idx)])
  }))
}

last_one_of <- function(x) {
  return(rev(x)[1])
}

one_of <- function(x) {
  return(x[sample.int(length(x), 1)])
}

## Time spent on pages
set.seed(0)
page_visits <- events %>%
  keep_where(!is.na(checkin)) %>%
  group_by(session_id) %>%
  arrange(ts) %>%
  summarize(group = head(test_group, 1),
            `page visits` = length(checkin),
            `median page visit check-in` = median(checkin),
            `random page visit check-in` = one_of(checkin),
            `last page visit check-in` = last_one_of(checkin)) %>%
  mutate(`random page visit next check-in` = next_checkin(`random page visit check-in`),
         `random page visit status` = ifelse(`random page visit check-in` == 420, 0, 3),
         `last page visit next check-in` = next_checkin(`last page visit check-in`),
         `last page visit status` = ifelse(`last page visit check-in` == 420, 0, 3))

surv <- survival::Surv(time = page_visits$`last page visit check-in`,
                       time2 = page_visits$`last page visit next check-in`,
                       event = page_visits$`last page visit status`,
                       type = "interval")
Group <- page_visits$group
fit <- survival::survfit(surv ~ Group)
GGally::ggsurv(fit) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Time (s)", title = "Time spent on last visited page in each search session")
