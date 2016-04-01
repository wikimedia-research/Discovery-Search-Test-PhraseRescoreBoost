# SELECT event_autocompleteType, event_source, event_subTest, COUNT(1) AS n FROM TestSearchSatisfaction2_15357244 WHERE LEFT(timestamp, 8) = '20160320' GROUP BY event_autocompleteType, event_source, event_subTest;
# +------------------------+--------------+----------------+--------+
# | event_autocompleteType | event_source | event_subTest  | n      |
# +------------------------+--------------+----------------+--------+
# | NULL                   | autocomplete | NULL           |  20921 | ?
# | NULL                   | autocomplete | phraseBoostEq1 |  15009 | ?
# | NULL                   | fulltext     | NULL           |  35825 | *
# | NULL                   | fulltext     | phraseBoostEq1 |  26360 | *
# | comp_suggest           | autocomplete | NULL           | 194302 |
# | comp_suggest           | autocomplete | phraseBoostEq1 | 143212 |
# | namespace,comp_suggest | autocomplete | NULL           |    241 |
# | namespace,comp_suggest | autocomplete | phraseBoostEq1 |    147 |
# | namespace,prefix       | autocomplete | NULL           |      1 |
# | namespace,prefix       | autocomplete | phraseBoostEq1 |      1 |
# | prefix                 | autocomplete | NULL           |    510 |
# | prefix                 | autocomplete | phraseBoostEq1 |    182 |
# | unknown                | autocomplete | NULL           |    102 |
# | unknown                | autocomplete | phraseBoostEq1 |     80 |
# +------------------------+--------------+----------------+--------+

start_date <- as.Date("2016-03-14")
end_date <- as.Date("2016-03-22")
# This test does not effect autocomplete (prefix or comp suggest)
#   and does not have any effect on zero results rate.
cat("Fetching EL data from", as.character(start_date), "to", as.character(end_date), "...\n")
events <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching EL data from", as.character(date), "\n")
  data <- wmf::build_query("SELECT LEFT(timestamp, 8) AS date,
                            timestamp AS ts,
                            userAgent AS user_agent,
                            event_mwSessionId AS session_id,
                            event_subTest AS test_group,
                            event_pageViewId AS page_id,
                            event_action AS action,
                            event_checkin AS checkin,
                            event_position AS result_position,
                            event_hitsReturned AS results_returned",
                           date = date,
                           table = "TestSearchSatisfaction2_15357244",
                           conditionals = "event_autocompleteType IS NULL AND event_action IN('searchResultPage', 'visitPage', 'checkin') AND event_source = 'fulltext'")
  return(data)
}))
cat("...done! Doing some date/datetime post-processing...")
library(magrittr)
events$date %<>% lubridate::ymd()
events$ts %<>% lubridate::ymd_hms()
cat("done.\n")

cat("Processing user agents to do spider filtering...")
events$device <- uaparser::parse_agents(events$user_agent, fields = "device")
events <- events[events$device != "Spider", ]
events <- events[, setdiff(names(events), c("user_agent", "device"))]
events$test_group[is.na(events$test_group)] <- "baseline"
cat("done.\n")

cat("Removing excess check-ins...")
events$action_id <- ifelse(events$action == "searchResultPage", 0, 1)
events$checkin[events$action == "visitPage"] <- 0
events <- data.table::as.data.table(events[order(events$session_id, events$action_id, events$page_id, events$checkin), ])
# For every page visit's check-ins, only keep the last check-in:
events <- events[!duplicated(events, fromLast = TRUE, by = c("session_id", "page_id", "action")), ]
cat("done.")

cat("Gathering last check-in data...\n")
library(progress)
pb <- progress_bar$new(total = nrow(events))
for (i in 2:nrow(events)) {
  pb$tick()
  if (events$action[i-1] == "visitPage" & events$action[i] == "checkin") {
    events$checkin[i-1] <- events$checkin[i]
  }
}; rm(i, pb)
events <- events[events$action != "checkin", ]
cat("All done!")

# users <- events[!duplicated(events, fromLast = FALSE, by = "session_id"), ]
# with(users, prop.table(table(date, test_group))) # baseline: 0.520127, phraseBoostEq1: 0.479873

cat("Writing data (", nrow(events), " events) to disk...", sep = "")
readr::write_rds(events, "~/phrase_boost_test_EL_v2.rds", "gz")
cat("done.\n")

## Locally:
# $> mkdir ~/Documents/Projects/Discovery\ Tests/Phrase\ Rescore\ Boost/data
# $> scp stat2:/home/bearloga/phrase_boost_test_EL_v2.rds ~/Documents/Projects/Discovery\ Tests/Phrase\ Rescore\ Boost/data/
