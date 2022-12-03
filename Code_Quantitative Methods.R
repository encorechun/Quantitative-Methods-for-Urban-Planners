#install and load package('readxl')
#install and load package('kableExtra')
#install and load package('ggplot2')
#install and load package('dplyr')
#install and load package('treemapify')
#install and load package('rstatix')

install.packages('readxl')
library(readxl)
install.packages('kableExtra')
library(kableExtra)
install.packages('ggplot2')
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('treemapify')
library(treemapify)
install.packages('rstatix')
library(rstatix)

#0. Import Excel file into R
airbnb <- read_excel('/Users/encorechun/Library/CloudStorage/OneDrive-HarvardUniversity/School/SES 5215 Analytic Methods of Urban Planning Quantitative/Week_01/SES5215_Assignment_1_Chunfeng_Yang_Cleaned.xls')
#view entire dataset
airbnb

#1. Visualizing the distribution of a continuous variable
#(1) plotting histogram
ggplot(airbnb) +
  geom_histogram(aes(x = NEAR_DIST_Subway_Station),
                 color = "white",
                 fill = "#660FFC",
                 bins = 100) +
  theme_minimal()
#(2)plotting A jittered (or one-dimensional) scatter plot
ggplot(airbnb) +
  geom_point(aes(x = price, y = 0),
             position = "jitter",
             size = 0.5,
             alpha = 0.5) +
  scale_y_continuous(name = "",
                     breaks = c()) +
  theme_minimal()

#2. Visualizing the relationship between two continuous variables
#(1)Scatter Plotting
ggplot(airbnb) +
  geom_point(aes(x = NEAR_DIST_Gallery, y = price),
             color = "purple",
             size = 0.1,
             alpha = 0.2) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_minimal()
#(2)Grid Overplotting
ggplot(airbnb) +
  geom_bin_2d(aes(x = price, 
                  y = NEAR_DIST_Gallery)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  scale_fill_viridis_c() +
  theme_minimal() 
#(3)Hexagonal Overplotting
ggplot(airbnb) +
  geom_hex(aes(x = price,
               y = NEAR_DIST_Gallery)) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  scale_fill_viridis_c() +
  theme_minimal()

#3. Decribing continuous variables
# (1) calculate average/mean
mean(airbnb$price)
# (2) calculate median
median(airbnb$price)
# (3) calculate standard deviation
sd(airbnb$price)
# (4) calculate percentile values
quantile(airbnb$price, probs = c(0.25, 0.5, 0.75))
# (5) get a summary of a variable’s range, interquartile range, mean, and median
summary(airbnb$price)

#4. Summarizing a categorical variable
airbnb %>%
  group_by(room_type) %>%
  summarise(number = n()) %>%
  mutate(proportion = number / length(airbnb$room_type)) 

#5. Visualizing proportions
#(1) bar chart plotting
airbnb %>%
  group_by(room_type) %>%
  summarise(number = n()) %>%
  mutate(proportion = number / length(airbnb$room_type)) %>%
  ggplot() +
    geom_bar(aes(x = room_type, y = proportion),
             stat = "identity") +
    scale_y_continuous(breaks = breaks <- seq(0, 1, by = 0.1),
                       labels = paste0(breaks * 100, "%")) +
    theme_minimal()
#(2) tree map plotting
airbnb %>%
  group_by(room_type) %>%
  summarise(number = n()) %>%
  mutate(proportion = number / length(airbnb$room_type)) %>%
  ggplot() +
  geom_treemap(aes(area = proportion, fill = room_type)) +
  geom_treemap_text(aes(area = proportion, label = room_type),
                    color = "white") +
  scale_fill_brewer(type = "qual", 
                    guide = "none") 

#6. Confidence intervals for means and proportions
#(1) get a 90-percent confidence interval for the average continuous variable
t.test(airbnb$price, conf.level = 0.9)
# you can be 90 percent confident that average price for the full airbnb listing would be between 183.7 and 187.4 minutes.
#(2) get a 90-percent confidence interval for the average continuous variable that falls into a category
t.test(airbnb$room_type == "Private room", conf.level = 0.9)
# you can be 90 percent confident that the share of the full airbnb listing that is private room is between 36 percent and 37 percent.
#(3) Average values within categories

# price_by_room_type <- airbnb %>%
#   group_by(room_type) %>%
#   get_summary_stats(price, type = "mean_ci") %>%
#   mutate(ci_low = mean - ci,
#          ci_hi = mean + ci)
# price_by_room_type  %>%
#   kable(digits = 0)

price_by_room_type <- airbnb %>%
  t_test(price ~ room_type, detailed = TRUE, conf.level = 0.9)

price_by_room_type  %>%
  kable(digits = c(rep(0, 15), 3, 0)) %>%
  scroll_box(width = "75%")
#(4) Error Bars
ggplot(price_by_room_type) +
  geom_col(aes(x = room_type, y = mean)) +
  geom_errorbar(aes(x = room_type,
                    ymin = ci_low,
                    ymax = ci_hi),
                colour = "Purple",
                width = 0.2) +
  scale_y_continuous(name = "Average price",
                     breaks = breaks <- seq(0, 250,
                                            by = 20),
                     labels =
                       paste0("$",
                              prettyNum(breaks,
                                        big.mark = ","))) +
  scale_x_discrete(name = "Usual room type of an Airbnb listing") +
  theme_minimal()
#(5) Correlation
cor.test(log(airbnb$price), 
         log(airbnb$NEAR_DIST_Gallery))

#7. Estimating a regression model
#(1) create a regression model
test_model <- lm(`price` ~ `NEAR_DIST_Gallery`,
                 data = airbnb)
#(2) interpretting a regression model
summary(test_model)
#coefficient:-0.109511
#p-value:<2e-16 
#r square:0.03759

#8. Regression with a categorical predictor
room_types <- room_types %>%
  mutate(room_type = fct_infreq(room_type))
type_model <- lm(price ~ room_type,
                 data = airbnb)

summary(type_model)

#9. Multivariate Regression
model <- lm(price ~ NEAR_DIST_Gallery + NEAR_DIST_Subway_Station + room_type,
            data = airbnb)

summary(model)
