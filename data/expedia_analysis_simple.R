library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(infer)

df <- read_csv("search_data_student.csv")

search_data <-
    df %>%
    select(srch_id, date_time, prop_id, prop_country_id,
           treatment = random_bool,
           booked = booking_bool,
           clicked = click_bool,
           prop_starrating,
           prop_review_score,
           prop_brand_bool,
           prop_location_score1,
           price_usd,
           promotion_flag, 
           position
           #gross_bookings_usd
           )

write_csv(search_data, "expedia_experiment_simple.csv")


# Descriptives?
# (i)
nrow(search_data)
# (ii)
search_data %>%
    select(srch_id) %>%
    n_distinct()
# (iii)
search_data %>%
    select(prop_id) %>%
    n_distinct()
# (iv)
search_data %>%
    select(prop_country_id) %>%
    n_distinct()

search_data %>%
    group_by(srch_id, treatment) %>%
    count() %>%
    group_by(treatment) %>%
    count()

# booking made
# --- Aggregate the Data --- #
# Run this code before continuing to 
# Questions 6 through 10.
sessions_data <-
    search_data %>%
    group_by(srch_id) %>%
    summarise(
        session_date = max(date(date_time)),
        treatment = as.factor(max(treatment)),
        booking_made = as.factor(max(booked)),
        booking_made_1 = max(booked),
        hotels_viewed = sum(clicked),
        has_clicked   = as.factor(max(clicked))#,
        #total_spent = max(gross_bookings_usd)
    ) %>%
    ungroup() %>%
    mutate(more_than_two_clicks = 
               if_else(hotels_viewed > 2, TRUE, FALSE)
    )

# Bookings
sessions_data %>%
    ggplot(aes(fill=as.factor(booking_made), x=as.factor(treatment))) + 
    geom_bar(position="fill") +
    ylab("Proportion") +
    xlab("Treatment Group") + 
    scale_fill_manual(name = "Booking Made",
                      labels = c("True",
                                 "False"),
                      values = c("1" ="#E69F00", "0" ="#999999")) +
    theme_bw() +
    theme(legend.position = "bottom") + 
    scale_x_discrete(labels=c("0" = "Control Group", "1" = "Randomized Rankings"))


# bookings, what test? prop
prop_test(sessions_data, booking_made ~ treatment, alternative = "greater")

# clicks?
sessions_data %>%
    group_by(treatment) %>%
    summarise(hotels_viewed = mean(hotels_viewed))

t_test(sessions_data, hotels_viewed ~ treatment, alternative = "greater")


# as regressions
booking_reg <- lm(as.numeric(booking_made_1) ~ treatment, data = sessions_data)
summary(booking_reg)

viewed_reg <- lm(hotels_viewed ~ treatment, data = sessions_data)
summary(viewed_reg)

