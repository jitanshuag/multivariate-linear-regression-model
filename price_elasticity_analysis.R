## Multivariate Regression Analysis for Price Elasticity

library(tidyverse)
library(broom)
library(car)
library(lmtest)
library(sandwich)

## Import data
data <- read_csv('pricing_data.csv')

## Log transformation for elasticity
data <- data %>%
  mutate(
    log_qty = log(qty),
    log_price = log(unit_price)
  )

## Multivariate regression model
model <- lm(
  log_qty ~ log_price + freight_price + weekend + holiday + product_category_name,
  data = data
)

## Model summary
summary(model)

## Multicollinearity check
vif(model)

## Heteroscedasticity test
bptest(model)

## Robust standard errors
coeftest(model, vcov = vcovHC(model, type = 'HC1'))

## Robust confidence intervals
robust_se <- sqrt(diag(vcovHC(model, type = 'HC1')))

robust_results <- tidy(model) %>%
  mutate(
    robust_se = robust_se,
    conf.low = estimate - 1.96 * robust_se,
    conf.high = estimate + 1.96 * robust_se
  )

## Coefficient plot
ggplot(robust_results %>% filter(term != '(Intercept)'),
       aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  coord_flip() +
  labs(
    title = 'Regression Coefficients with Robust 95% Confidence Intervals',
    x = 'Variables',
    y = 'Coefficient Estimate'
  )

ggsave('robust_coefficients_plot.png', width = 8, height = 6)
