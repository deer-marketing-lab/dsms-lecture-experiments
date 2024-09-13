
library(readr)
library(lubridate)
library(dplyr)

search_data <- read_csv("../data/expedia_experiment_simple.csv")
  
search_data %>%
  head(15)


## ---- echo = TRUE, message = FALSE-----------------------------------------------------------------------------------------------------------------

# (i)
nrow(search_data)
# (ii)
search_data %>% select(srch_id) %>% n_distinct()
# (iii)
search_data %>% select(prop_id) %>% n_distinct()
# (iv)
search_data %>% select(prop_country_id) %>% n_distinct()


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
search_data %>%
    group_by(srch_id, treatment) %>%
    count() %>%
    group_by(treatment) %>%
    count()


## ---- message = FALSE, echo = FALSE, fig.align = 'center'------------------------------------------------------------------------------------------
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
library(ggplot2)
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


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
library(infer)

prop_test(sessions_data, booking_made ~ treatment, alternative = "two.sided")



## ---- echo = FALSE, fig.align = "center", out.width="65%"------------------------------------------------------------------------------------------
url <- "https://miro.medium.com/max/852/1*iM4wTvvEgVmFVqbaip2--Q.png"
knitr::include_graphics(url)


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
# what are the means?
sessions_data %>%
    group_by(treatment) %>%
    summarise(hotels_viewed = mean(hotels_viewed))

#Now test
t_test(sessions_data, hotels_viewed ~ treatment, alternative = "two.sided")



## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
booking_reg <- lm(as.numeric(booking_made_1) ~ treatment, 
                  data = sessions_data)
summary(booking_reg)


## ---- echo = FALSE---------------------------------------------------------------------------------------------------------------------------------
viewed_reg <- lm(hotels_viewed ~ treatment, 
                  data = sessions_data)
summary(viewed_reg)



