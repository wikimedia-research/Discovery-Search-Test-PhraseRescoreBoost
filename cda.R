library(magrittr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
library(ggplot2)
library(cowplot)

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

clickthrough_by_group <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id) %>%
  summarize(group = head(test_group, 1),
            clickthrough = any(action %in% "visitPage")) %>%
  group_by(group, clickthrough) %>%
  summarize(n = n()) %>%
  tidyr::spread(clickthrough, n)
clickthrough_by_group <- set_rownames(as.matrix(clickthrough_by_group[1:2, 2:3]), clickthrough_by_group$group)[2:1, 2:1]

library(bandit) # install.packages('bandit')
best_binomial_bandit(clickthrough_by_group[, 1], margin.table(clickthrough_by_group, 1))
significance_analysis(clickthrough_by_group[, 1], margin.table(clickthrough_by_group, 1))
# summarize_metrics(margin.table(clickthrough_by_group, 1)[1], successes = clickthrough_by_group[1, 1])

library(BCDA) # devtools::install_local('~/Documents/Projects/R Packages/BCDA')
test_indepen(clickthrough_by_group)
ci_prop_diff_tail(clickthrough_by_group)
ci_relative_risk(clickthrough_by_group)

library(testr) # devtools::install_github('bearloga/testr')
post <- beta_binomial_ab_test(clickthrough_by_group[, 1], margin.table(clickthrough_by_group, 1),
                              conf.level = 0.05, groups = rownames(clickthrough_by_group))
gg <- plot(post, limits = c(0.25, 0.35), title = "Phrase Rescore Boost 1 A/B Test: Clickthrough Rates")
gg + ggthemes::theme_tufte(base_family = "Gill Sans") + theme(panel.grid = element_line(color = "black"))
