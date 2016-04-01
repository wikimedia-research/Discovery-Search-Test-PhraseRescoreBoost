library(magrittr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
library(ggplot2)
library(cowplot)

format_confint <- function(ci, digits = 2, units = "") {
  if (units == "%") {
    units <- paste0(units, units)
  }
  type <- switch(typeof(ci[1]), "character" = "s", "double" = "f", "integer" = "i")
  a <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), ci[1])
  b <- sprintf(paste0("%", ifelse(type == "character", "s", paste0(".", digits, type)), units), ci[2])
  return(paste0("(", a, ", ", b, ")"))
}

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

library(testr) # devtools::install_github('bearloga/testr')
post <- beta_binomial_ab_test(clickthrough_by_group[, 1], margin.table(clickthrough_by_group, 1),
                              conf.level = 0.05, groups = rownames(clickthrough_by_group))
gg <- plot(post, limits = c(0.25, 0.35), title = "Phrase Rescore Boost 1 A/B Test: Clickthrough Rates")
gg + ggthemes::theme_tufte(base_family = "Gill Sans") + theme(panel.grid = element_line(color = "black"))

library(BCDA) # devtools::install_local('~/Documents/Projects/R Packages/BCDA')
bcda_summary <- function(data, knitr = TRUE) {
  temp <- list(`Test of Independence with Bayes Factor` = BCDA::test_indepen(data)$Interpretation,
               `Test Group - Control Group` = format_confint(100*BCDA::ci_prop_diff_tail(data), 3, "%"),
               `Relative Risk ("times more/less likely")` = format_confint(BCDA::ci_relative_risk(data), 3)) %>%
    do.call(rbind, .) %>% as.data.frame %>% set_names("95% Bayesian C.I.")
  if (knitr) {
    return(knitr::kable(temp, align = "c", caption = "Comparison of Test Group vs Control Group (Baseline)"))
  } else {
    return(temp)
  }
}

bcda_summary(clickthrough_by_group)

ks.test(time_to_clickthrough$`Time to first clickthrough`[time_to_clickthrough$Group == "phraseBoostEq1"],
        time_to_clickthrough$`Time to first clickthrough`[time_to_clickthrough$Group == "baseline"])
ks.test(log10(time_to_clickthrough$`Time to first clickthrough`[time_to_clickthrough$Group == "phraseBoostEq1"]),
        log10(time_to_clickthrough$`Time to first clickthrough`[time_to_clickthrough$Group == "baseline"]))

## Comparing discrete distributions...

kullback_leibler_divergence <- function(reference_distribution, test_distribution) {
  reference_distribution[reference_distribution == 0] <- 1e-7
  test_distribution[test_distribution == 0] <- 1e-7
  return(sum(reference_distribution * log(reference_distribution/test_distribution)))
}

bootstrap_kl <- function(reference_distribution, test_distribution, nsims = 1000) {
  observed_kl <- kullback_leibler_divergence(reference_distribution, test_distribution)
  bootstrapped_kl <- vapply(1:nsims, function(i) {
    new_ref <- sample(reference_distribution, length(reference_distribution), replace = TRUE)
    new_test <- sample(test_distribution, length(test_distribution), replace = TRUE)
    return(kullback_leibler_divergence(new_ref, new_test))
  }, 0.0)
  return(list(observed = observed_kl, pval = sum(bootstrapped_kl >= observed_kl)/nsims))
}

first_click <- events %>%
  keep_where(!is.na(result_position)) %>%
  group_by(session_id) %>%
  arrange(ts) %>%
  summarize(group = head(test_group, 1),
            position = head(result_position, 1))

observed_kl <- first_click %>%
  group_by(group, position) %>%
  summarize(sessions = n()) %>%
  tidyr::spread(group, sessions, fill = 0) %>%
  tidyr::gather(group, sessions, -position) %>%
  {
    P <- .$sessions[.$group == "baseline"]; P <- P/sum(P)
    Q <- .$sessions[.$group == "phraseBoostEq1"]; Q <- Q/sum(Q)
    kullback_leibler_divergence(P, Q)
  }

bootstrapped_kl <- vapply(1:1000, function(i) {
  bootstrapped_sample <- first_click
  bootstrapped_sample$position[bootstrapped_sample$group == "baseline"] %<>% sample(replace = TRUE)
  bootstrapped_sample$position[bootstrapped_sample$group == "phraseBoostEq1"] %<>% sample(replace = TRUE)
  bootstrapped_sample %<>%
    group_by(group, position) %>%
    summarize(sessions = n()) %>%
    tidyr::spread(group, sessions, fill = 0) %>%
    tidyr::gather(group, sessions, -position)
  P <- bootstrapped_sample$sessions[bootstrapped_sample$group == "baseline"]; P <- P/sum(P)
  Q <- bootstrapped_sample$sessions[bootstrapped_sample$group == "phraseBoostEq1"]; Q <- Q/sum(Q)
  return(kullback_leibler_divergence(P, Q))
}, 0.0)

bootstrap_test <- function(observed_statistic, bootstapped_statistics, conf_level = 0.95) {
  # see http://www.stat.cmu.edu/~cshalizi/402/lectures/08-bootstrap/lecture-08.pdf
  alpha <- 1 - conf_level
  p_val <- (sum(bootstapped_statistics >= observed_statistic)+1)/(length(bootstapped_statistics)+1)
  ci_lower <- 2*observed_statistic - quantile(bootstapped_statistics, 1-alpha/2)
  ci_upper <- 2*observed_statistic - quantile(bootstapped_statistics, alpha/2)
  return(list(pval = p_val, ci = unname(c(ci_lower, ci_upper))))
}

observed_kl
bootstrap_test(observed_kl, bootstrapped_kl)
format_confint(bootstrap_test(observed_kl, bootstrapped_kl)$ci, 3)

searches_by_group <- events %>%
  group_by(session_id) %>%
  summarize(group = head(test_group, 1),
            SERPs = sum(action %in% "searchResultPage")) %>%
  keep_where(SERPs > 0)

observed_kl <- searches_by_group %>%
  group_by(group, SERPs) %>%
  summarize(sessions = n()) %>%
  tidyr::spread(group, sessions, fill = 0) %>%
  tidyr::gather(group, sessions, -SERPs) %>%
  {
    P <- .$sessions[.$group == "baseline"]; P <- P/sum(P)
    Q <- .$sessions[.$group == "phraseBoostEq1"]; Q <- Q/sum(Q)
    kullback_leibler_divergence(P, Q)
  }

bootstrapped_kl <- vapply(1:1000, function(i) {
  bootstrapped_sample <- searches_by_group
  bootstrapped_sample$SERPs[bootstrapped_sample$group == "baseline"] %<>% sample(replace = TRUE)
  bootstrapped_sample$SERPs[bootstrapped_sample$group == "phraseBoostEq1"] %<>% sample(replace = TRUE)
  bootstrapped_sample %<>%
    group_by(group, SERPs) %>%
    summarize(sessions = n()) %>%
    tidyr::spread(group, sessions, fill = 0) %>%
    tidyr::gather(group, sessions, -SERPs)
  P <- bootstrapped_sample$sessions[bootstrapped_sample$group == "baseline"]; P <- P/sum(P)
  Q <- bootstrapped_sample$sessions[bootstrapped_sample$group == "phraseBoostEq1"]; Q <- Q/sum(Q)
  return(kullback_leibler_divergence(P, Q))
}, 0.0)

plot(density(bootstrapped_kl, adjust = 3))
abline(v = observed_kl, lty = "dashed")
observed_kl
bootstrap_test(observed_kl, bootstrapped_kl)
format_confint(bootstrap_test(observed_kl, bootstrapped_kl)$ci, 3)

ctr_counts_by_group <- events %>%
  keep_where(session_id %in% nonzero_session_ids) %>%
  group_by(session_id) %>%
  summarize(group = head(test_group, 1),
            clickthroughs = sum(action %in% "visitPage"))

observed_kl <- ctr_counts_by_group %>%
  group_by(group, clickthroughs) %>%
  summarize(sessions = n()) %>%
  tidyr::spread(group, sessions, fill = 0) %>%
  tidyr::gather(group, sessions, -clickthroughs) %>%
  {
    P <- .$sessions[.$group == "baseline"]; P <- P/sum(P)
    Q <- .$sessions[.$group == "phraseBoostEq1"]; Q <- Q/sum(Q)
    kullback_leibler_divergence(P, Q)
  }

bootstrapped_kl <- vapply(1:1000, function(i) {
  bootstrapped_sample <- ctr_counts_by_group
  bootstrapped_sample$clickthroughs[bootstrapped_sample$group == "baseline"] %<>% sample(replace = TRUE)
  bootstrapped_sample$clickthroughs[bootstrapped_sample$group == "phraseBoostEq1"] %<>% sample(replace = TRUE)
  bootstrapped_sample %<>%
    group_by(group, clickthroughs) %>%
    summarize(sessions = n()) %>%
    tidyr::spread(group, sessions, fill = 0) %>%
    tidyr::gather(group, sessions, -clickthroughs)
  P <- bootstrapped_sample$sessions[bootstrapped_sample$group == "baseline"]; P <- P/sum(P)
  Q <- bootstrapped_sample$sessions[bootstrapped_sample$group == "phraseBoostEq1"]; Q <- Q/sum(Q)
  return(kullback_leibler_divergence(P, Q))
}, 0.0)

plot(density(bootstrapped_kl, adjust = 3))
abline(v = observed_kl, lty = "dashed")
observed_kl
bootstrap_test(observed_kl, bootstrapped_kl)
format_confint(bootstrap_test(observed_kl, bootstrapped_kl)$ci, 3)

session_lengths <- events %>%
  group_by(date, session_id) %>%
  arrange(ts) %>%
  summarize(group = head(test_group, 1),
            session_length = as.integer(difftime(tail(ts, 1), head(ts, 1), units = "secs"))) %>%
  ungroup %>%
  keep_where(session_length > 0)

session_lengths_smaller <- dplyr::sample_frac(session_lengths, 0.75)
ks.test(session_lengths_smaller$session_length[session_lengths_smaller$group == "phraseBoostEq1"],
        session_lengths_smaller$session_length[session_lengths_smaller$group == "baseline"])
# hm...seems like the really large sample size makes the statistic significant, let's try bootstrap?

observed_D <- suppressWarnings(ks.test(session_lengths$session_length[session_lengths$group == "phraseBoostEq1"],
                                       session_lengths$session_length[session_lengths$group == "baseline"])$statistic['D'])

bootstrapped_D <- vapply(1:1000, function(i) {
  bootstrapped_sample <- session_lengths
  bootstrapped_sample$session_length[bootstrapped_sample$group == "baseline"] %<>% sample(replace = TRUE)
  bootstrapped_sample$session_length[bootstrapped_sample$group == "phraseBoostEq1"] %<>% sample(replace = TRUE)
  return(unname(suppressWarnings(ks.test(bootstrapped_sample$session_length[bootstrapped_sample$group == "phraseBoostEq1"],
                                         bootstrapped_sample$session_length[bootstrapped_sample$group == "baseline"]
                                         )$statistic['D'])))
}, 0.0)

plot(density(bootstrapped_D, adjust = 3))
abline(v = observed_D, lty = "dashed")
observed_D
bootstrap_test(observed_D, bootstrapped_D)
format_confint(bootstrap_test(observed_D, bootstrapped_D)$ci, 3)
