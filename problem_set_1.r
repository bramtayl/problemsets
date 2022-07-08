library(car)
library(dplyr)
library(purrr)
library(readr)
library(sandwich)
library(lmtest)

commute_binary = 
    read_csv("brandon/commute_binary.csv") %>%
    rename(
        individual_id = id,
        commute_mode = mode,
        drive_time_minutes = time.car,
        drive_cost_dollars = cost.car,
        bus_time_minutes = time.bus,
        bus_cost_dollars = cost.bus,
        gas_price_dollars = price_gas,
        daily_snow_inches = snowfall,
        age_years = age,
        income_thousand_dollas = income
    ) %>%
    mutate(
        construction = as.logical(construction),
        bus_detour = as.logical(bus_detour),
        drives = commute_mode == "car"
    )

# 1
## a
linear_model = lm(
    drives ~ drive_cost_dollars + drive_time_minutes + bus_time_minutes,
    commute_binary
)
### i
coeftest(linear_model)
vcovHC(linear_model)
### ii
commute_binary =
    commute_binary %>%
    mutate(
        linear_model_prediction = predict(linear_model, commute_binary),
    )
### iii
commute_binary =
    commute_binary %>%
    mutate(
        linear_model_feasible = 
            0 <= linear_model_prediction & linear_model_prediction <= 1
    )
sum(!(commute_binary$linear_model_feasible))
# 2
## a
logistic_model =
    glm(
        drives ~ drive_cost_dollars + drive_time_minutes + bus_time_minutes,
        commute_binary,
        family = "binomial"
    )
summary(logistic_model)

marginal_effect_summary = function (data, model, variable_name) {
    marginal_effects = 
        coef(model)[[variable_name]] * data[[variable_name]]
    quantile(marginal_effects) %>%
    as.list %>%
    as_tibble %>%
    mutate(mean = mean(marginal_effects))  
}

full_join(
    tibble(
        model_name = c("linear_model", "logisitic_model"),
        model = list(linear_model, logistic_model)
    ),
    tibble(
        variable_name = c(
            "drive_cost_dollars",
            "drive_time_minutes",
            "bus_time_minutes"
        )
    ),
    by = character()
) %>%
    group_by(model_name, variable_name) %>%
    summarize(marginal_effect_summary(commute_binary, model, variable_name))

