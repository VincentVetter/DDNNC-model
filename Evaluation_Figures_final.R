library(ggplot2)
library(dplyr)

ES_DDNNC <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/es_ddnnc.csv", header = TRUE)
ES_LASSO <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/es_lassoBS.csv", header = FALSE)
ES_CGNN <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/es_cgmES.csv", header = FALSE)
ES <- data.frame(DDNNC = ES_DDNNC$ES, LASSO = ES_LASSO$V1, CGNN = ES_CGNN$V1)

VS_half_DDNNC <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/vs2_ddnnc.csv", header = TRUE)
VS_half_LASSO <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/vs2_lassoBS.csv", header = FALSE)
VS_half_CGNN <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/vs2_cgmES.csv", header = FALSE)
VS_half <- data.frame(DDNNC = VS_half_DDNNC$VS_half, LASSO = VS_half_LASSO$V1, CGNN = VS_half_CGNN$V1)

VS_whole_DDNNC <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/vs1_ddnnc.csv", header = TRUE)
VS_whole_LASSO <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/vs1_lassoBS.csv", header = FALSE)
VS_whole_CGNN <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/vs1_cgmES.csv", header = FALSE)
VS_whole <- data.frame(DDNNC = VS_whole_DDNNC$VS_whole, LASSO = VS_whole_LASSO$V1, CGNN = VS_whole_CGNN$V1)

# On and Off Peak figures: (on-peak hours (8-19) and off-peak hours (0-7,20-23)) ----------------------------------------------------------------------------

rows_per_day <- 24
num_days <- 200
on_peak_range <- 9:20
day_indices <- rep(1:num_days, each = rows_per_day)
hour_indices <- rep(1:rows_per_day, num_days)
on_peak_rows <- which(hour_indices %in% on_peak_range)
off_peak_rows <- which(!(hour_indices %in% on_peak_range))

ES_On_Peak <- ES[on_peak_rows, ]
VS_half_On_Peak <- VS_half[on_peak_rows, ]
VS_whole_On_Peak <- VS_whole[on_peak_rows, ]

ES_Off_Peak <- ES[off_peak_rows, ]
VS_half_Off_Peak <- VS_half[off_peak_rows, ]
VS_whole_Off_Peak <- VS_whole[off_peak_rows, ]


#Energy Score:
mean_DDNNC_on_peak <- mean(ES_On_Peak$DDNNC)
mean_DDNNC_off_peak <- mean(ES_Off_Peak$DDNNC)

mean_LASSO_on_peak <- mean(ES_On_Peak$LASSO)
mean_LASSO_off_peak <- mean(ES_Off_Peak$LASSO)

mean_CGNN_on_peak <- mean(ES_On_Peak$CGNN)
mean_CGNN_off_peak <- mean(ES_Off_Peak$CGNN)

data <- data.frame(
  Model = rep(c("DDNNC", "LASSO_bootstrap", "CGNN"), each = 2),
  Energy_Score = c(mean_DDNNC_on_peak, mean_DDNNC_off_peak,
                   mean_LASSO_on_peak, mean_LASSO_off_peak,
                   mean_CGNN_on_peak, mean_CGNN_off_peak),
  Peak_Off_Peak = rep(c("on-peak", "off-peak"), times = 3)
)

ggplot(data, aes(x = Model, y = Energy_Score, color = Peak_Off_Peak, group = Peak_Off_Peak)) +
  geom_point(size = 4) +  # Plot the points
  geom_line(size = 1, linetype = "dashed") +   # Connect the points with lines
  labs(x = "Model", y = "Energy Score") +
  scale_color_manual(values = c("on-peak" = "darkgreen", "off-peak" = "darkred")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),  # Increase size of X axis title
    axis.title.y = element_text(size = 14),  # Increase size of Y axis title
    axis.text.x = element_text(size = 12),   # Increase size of X axis labels
    axis.text.y = element_text(size = 12),   # Increase size of Y axis labels
    legend.text = element_text(size = 12)    # Increase size of legend text
  )

# Variogram score half
mean_DDNNC_on_peak <- mean(VS_half_On_Peak$DDNNC)
mean_DDNNC_off_peak <- mean(VS_half_Off_Peak$DDNNC)

mean_LASSO_on_peak <- mean(VS_half_On_Peak$LASSO)
mean_LASSO_off_peak <- mean(VS_half_Off_Peak$LASSO)

mean_CGNN_on_peak <- mean(VS_half_On_Peak$CGNN)
mean_CGNN_off_peak <- mean(VS_half_Off_Peak$CGNN)

data <- data.frame(
  Model = rep(c("DDNNC", "LASSO_bootstrap", "CGNN"), each = 2),
  Energy_Score = c(mean_DDNNC_on_peak, mean_DDNNC_off_peak,
                   mean_LASSO_on_peak, mean_LASSO_off_peak,
                   mean_CGNN_on_peak, mean_CGNN_off_peak),
  Peak_Off_Peak = rep(c("on-peak", "off-peak"), times = 3)
)

ggplot(data, aes(x = Model, y = Energy_Score, color = Peak_Off_Peak, group = Peak_Off_Peak)) +
  geom_point(size = 4) +  # Plot the points
  geom_line(size = 1, linetype = "dashed") +   # Connect the points with lines
  labs(x = "Model", y = "Variogram Score, p = 0.5") +
  scale_color_manual(values = c("on-peak" = "darkgreen", "off-peak" = "darkred")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),  # Increase size of X axis title
    axis.title.y = element_text(size = 14),  # Increase size of Y axis title
    axis.text.x = element_text(size = 12),   # Increase size of X axis labels
    axis.text.y = element_text(size = 12),   # Increase size of Y axis labels
    legend.text = element_text(size = 12)    # Increase size of legend text
  )

# Variogram score whole
mean_DDNNC_on_peak <- mean(VS_whole_On_Peak$DDNNC)
mean_DDNNC_off_peak <- mean(VS_whole_Off_Peak$DDNNC)

mean_LASSO_on_peak <- mean(VS_whole_On_Peak$LASSO)
mean_LASSO_off_peak <- mean(VS_whole_Off_Peak$LASSO)

mean_CGNN_on_peak <- mean(VS_whole_On_Peak$CGNN)
mean_CGNN_off_peak <- mean(VS_whole_Off_Peak$CGNN)

data <- data.frame(
  Model = rep(c("DDNNC", "LASSO_bootstrap", "CGNN"), each = 2),
  Energy_Score = c(mean_DDNNC_on_peak, mean_DDNNC_off_peak,
                   mean_LASSO_on_peak, mean_LASSO_off_peak,
                   mean_CGNN_on_peak, mean_CGNN_off_peak),
  Peak_Off_Peak = rep(c("on-peak", "off-peak"), times = 3)
)

ggplot(data, aes(x = Model, y = Energy_Score, color = Peak_Off_Peak, group = Peak_Off_Peak)) +
  geom_point(size = 4) +  # Plot the points
  geom_line(size = 1, linetype = "dashed") +   # Connect the points with lines
  labs(x = "Model", y = "Variogram Score, p = 1") +
  scale_color_manual(values = c("on-peak" = "darkgreen", "off-peak" = "darkred")) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.title.x = element_text(size = 14),  # Increase size of X axis title
    axis.title.y = element_text(size = 14),  # Increase size of Y axis title
    axis.text.x = element_text(size = 12),   # Increase size of X axis labels
    axis.text.y = element_text(size = 12),   # Increase size of Y axis labels
    legend.text = element_text(size = 12)    # Increase size of legend text
  )

# Hourly figures -----------------------------------------------------------------------------------------

ES$hour <- rep(0:23, times = 200)
VS_half$hour <- rep(0:23, times = 200)
VS_whole$hour <- rep(0:23, times = 200)

ES_hourly_dfs <- split(ES, ES$hour)
VS_half_hourly_dfs <- split(VS_half, VS_half$hour)
VS_whole_hourly_dfs <- split(VS_whole, VS_whole$hour)

ES_hour_0 <- ES_hourly_dfs[[1]]
ES_hour_1 <- ES_hourly_dfs[[2]]
ES_hour_2 <- ES_hourly_dfs[[3]]
ES_hour_3 <- ES_hourly_dfs[[4]]
ES_hour_4 <- ES_hourly_dfs[[5]]
ES_hour_5 <- ES_hourly_dfs[[6]]
ES_hour_6 <- ES_hourly_dfs[[7]]
ES_hour_7 <- ES_hourly_dfs[[8]]
ES_hour_8 <- ES_hourly_dfs[[9]]
ES_hour_9 <- ES_hourly_dfs[[10]]
ES_hour_10 <- ES_hourly_dfs[[11]]
ES_hour_11 <- ES_hourly_dfs[[12]]
ES_hour_12 <- ES_hourly_dfs[[13]]
ES_hour_13 <- ES_hourly_dfs[[14]]
ES_hour_14 <- ES_hourly_dfs[[15]]
ES_hour_15 <- ES_hourly_dfs[[16]]
ES_hour_16 <- ES_hourly_dfs[[17]]
ES_hour_17 <- ES_hourly_dfs[[18]]
ES_hour_18 <- ES_hourly_dfs[[19]]
ES_hour_19 <- ES_hourly_dfs[[20]]
ES_hour_20 <- ES_hourly_dfs[[21]]
ES_hour_21 <- ES_hourly_dfs[[22]]
ES_hour_22 <- ES_hourly_dfs[[23]]
ES_hour_23 <- ES_hourly_dfs[[24]]


VS_half_hour_0 <- VS_half_hourly_dfs[[1]]
VS_half_hour_1 <- VS_half_hourly_dfs[[2]]
VS_half_hour_2 <- VS_half_hourly_dfs[[3]]
VS_half_hour_3 <- VS_half_hourly_dfs[[4]]
VS_half_hour_4 <- VS_half_hourly_dfs[[5]]
VS_half_hour_5 <- VS_half_hourly_dfs[[6]]
VS_half_hour_6 <- VS_half_hourly_dfs[[7]]
VS_half_hour_7 <- VS_half_hourly_dfs[[8]]
VS_half_hour_8 <- VS_half_hourly_dfs[[9]]
VS_half_hour_9 <- VS_half_hourly_dfs[[10]]
VS_half_hour_10 <- VS_half_hourly_dfs[[11]]
VS_half_hour_11 <- VS_half_hourly_dfs[[12]]
VS_half_hour_12 <- VS_half_hourly_dfs[[13]]
VS_half_hour_13 <- VS_half_hourly_dfs[[14]]
VS_half_hour_14 <- VS_half_hourly_dfs[[15]]
VS_half_hour_15 <- VS_half_hourly_dfs[[16]]
VS_half_hour_16 <- VS_half_hourly_dfs[[17]]
VS_half_hour_17 <- VS_half_hourly_dfs[[18]]
VS_half_hour_18 <- VS_half_hourly_dfs[[19]]
VS_half_hour_19 <- VS_half_hourly_dfs[[20]]
VS_half_hour_20 <- VS_half_hourly_dfs[[21]]
VS_half_hour_21 <- VS_half_hourly_dfs[[22]]
VS_half_hour_22 <- VS_half_hourly_dfs[[23]]
VS_half_hour_23 <- VS_half_hourly_dfs[[24]]


VS_whole_hour_0 <- VS_whole_hourly_dfs[[1]]
VS_whole_hour_1 <- VS_whole_hourly_dfs[[2]]
VS_whole_hour_2 <- VS_whole_hourly_dfs[[3]]
VS_whole_hour_3 <- VS_whole_hourly_dfs[[4]]
VS_whole_hour_4 <- VS_whole_hourly_dfs[[5]]
VS_whole_hour_5 <- VS_whole_hourly_dfs[[6]]
VS_whole_hour_6 <- VS_whole_hourly_dfs[[7]]
VS_whole_hour_7 <- VS_whole_hourly_dfs[[8]]
VS_whole_hour_8 <- VS_whole_hourly_dfs[[9]]
VS_whole_hour_9 <- VS_whole_hourly_dfs[[10]]
VS_whole_hour_10 <- VS_whole_hourly_dfs[[11]]
VS_whole_hour_11 <- VS_whole_hourly_dfs[[12]]
VS_whole_hour_12 <- VS_whole_hourly_dfs[[13]]
VS_whole_hour_13 <- VS_whole_hourly_dfs[[14]]
VS_whole_hour_14 <- VS_whole_hourly_dfs[[15]]
VS_whole_hour_15 <- VS_whole_hourly_dfs[[16]]
VS_whole_hour_16 <- VS_whole_hourly_dfs[[17]]
VS_whole_hour_17 <- VS_whole_hourly_dfs[[18]]
VS_whole_hour_18 <- VS_whole_hourly_dfs[[19]]
VS_whole_hour_19 <- VS_whole_hourly_dfs[[20]]
VS_whole_hour_20 <- VS_whole_hourly_dfs[[21]]
VS_whole_hour_21 <- VS_whole_hourly_dfs[[22]]
VS_whole_hour_22 <- VS_whole_hourly_dfs[[23]]
VS_whole_hour_23 <- VS_whole_hourly_dfs[[24]]

# ES
mean_DDNNC_hour_1 <- mean(ES_hour_0$DDNNC)
mean_DDNNC_hour_2 <- mean(ES_hour_1$DDNNC)
mean_DDNNC_hour_3 <- mean(ES_hour_2$DDNNC)
mean_DDNNC_hour_4 <- mean(ES_hour_3$DDNNC)
mean_DDNNC_hour_5 <- mean(ES_hour_4$DDNNC)
mean_DDNNC_hour_6 <- mean(ES_hour_5$DDNNC)
mean_DDNNC_hour_7 <- mean(ES_hour_6$DDNNC)
mean_DDNNC_hour_8 <- mean(ES_hour_7$DDNNC)
mean_DDNNC_hour_9 <- mean(ES_hour_8$DDNNC)
mean_DDNNC_hour_10 <- mean(ES_hour_9$DDNNC)
mean_DDNNC_hour_11 <- mean(ES_hour_10$DDNNC)
mean_DDNNC_hour_12 <- mean(ES_hour_11$DDNNC)
mean_DDNNC_hour_13 <- mean(ES_hour_12$DDNNC)
mean_DDNNC_hour_14 <- mean(ES_hour_13$DDNNC)
mean_DDNNC_hour_15 <- mean(ES_hour_14$DDNNC)
mean_DDNNC_hour_16 <- mean(ES_hour_15$DDNNC)
mean_DDNNC_hour_17 <- mean(ES_hour_16$DDNNC)
mean_DDNNC_hour_18 <- mean(ES_hour_17$DDNNC)
mean_DDNNC_hour_19 <- mean(ES_hour_18$DDNNC)
mean_DDNNC_hour_20 <- mean(ES_hour_19$DDNNC)
mean_DDNNC_hour_21 <- mean(ES_hour_20$DDNNC)
mean_DDNNC_hour_22 <- mean(ES_hour_21$DDNNC)
mean_DDNNC_hour_23 <- mean(ES_hour_22$DDNNC)
mean_DDNNC_hour_24 <- mean(ES_hour_23$DDNNC)

mean_LASSO_hour_1 <- mean(ES_hour_0$LASSO)
mean_LASSO_hour_2 <- mean(ES_hour_1$LASSO)
mean_LASSO_hour_3 <- mean(ES_hour_2$LASSO)
mean_LASSO_hour_4 <- mean(ES_hour_3$LASSO)
mean_LASSO_hour_5 <- mean(ES_hour_4$LASSO)
mean_LASSO_hour_6 <- mean(ES_hour_5$LASSO)
mean_LASSO_hour_7 <- mean(ES_hour_6$LASSO)
mean_LASSO_hour_8 <- mean(ES_hour_7$LASSO)
mean_LASSO_hour_9 <- mean(ES_hour_8$LASSO)
mean_LASSO_hour_10 <- mean(ES_hour_9$LASSO)
mean_LASSO_hour_11 <- mean(ES_hour_10$LASSO)
mean_LASSO_hour_12 <- mean(ES_hour_11$LASSO)
mean_LASSO_hour_13 <- mean(ES_hour_12$LASSO)
mean_LASSO_hour_14 <- mean(ES_hour_13$LASSO)
mean_LASSO_hour_15 <- mean(ES_hour_14$LASSO)
mean_LASSO_hour_16 <- mean(ES_hour_15$LASSO)
mean_LASSO_hour_17 <- mean(ES_hour_16$LASSO)
mean_LASSO_hour_18 <- mean(ES_hour_17$LASSO)
mean_LASSO_hour_19 <- mean(ES_hour_18$LASSO)
mean_LASSO_hour_20 <- mean(ES_hour_19$LASSO)
mean_LASSO_hour_21 <- mean(ES_hour_20$LASSO)
mean_LASSO_hour_22 <- mean(ES_hour_21$LASSO)
mean_LASSO_hour_23 <- mean(ES_hour_22$LASSO)
mean_LASSO_hour_24 <- mean(ES_hour_23$LASSO)

mean_CGNN_hour_1 <- mean(ES_hour_0$CGNN)
mean_CGNN_hour_2 <- mean(ES_hour_1$CGNN)
mean_CGNN_hour_3 <- mean(ES_hour_2$CGNN)
mean_CGNN_hour_4 <- mean(ES_hour_3$CGNN)
mean_CGNN_hour_5 <- mean(ES_hour_4$CGNN)
mean_CGNN_hour_6 <- mean(ES_hour_5$CGNN)
mean_CGNN_hour_7 <- mean(ES_hour_6$CGNN)
mean_CGNN_hour_8 <- mean(ES_hour_7$CGNN)
mean_CGNN_hour_9 <- mean(ES_hour_8$CGNN)
mean_CGNN_hour_10 <- mean(ES_hour_9$CGNN)
mean_CGNN_hour_11 <- mean(ES_hour_10$CGNN)
mean_CGNN_hour_12 <- mean(ES_hour_11$CGNN)
mean_CGNN_hour_13 <- mean(ES_hour_12$CGNN)
mean_CGNN_hour_14 <- mean(ES_hour_13$CGNN)
mean_CGNN_hour_15 <- mean(ES_hour_14$CGNN)
mean_CGNN_hour_16 <- mean(ES_hour_15$CGNN)
mean_CGNN_hour_17 <- mean(ES_hour_16$CGNN)
mean_CGNN_hour_18 <- mean(ES_hour_17$CGNN)
mean_CGNN_hour_19 <- mean(ES_hour_18$CGNN)
mean_CGNN_hour_20 <- mean(ES_hour_19$CGNN)
mean_CGNN_hour_21 <- mean(ES_hour_20$CGNN)
mean_CGNN_hour_22 <- mean(ES_hour_21$CGNN)
mean_CGNN_hour_23 <- mean(ES_hour_22$CGNN)
mean_CGNN_hour_24 <- mean(ES_hour_23$CGNN)

hour <- 1:24

mean_DDNNC <- c(mean_DDNNC_hour_1, mean_DDNNC_hour_2, mean_DDNNC_hour_3, mean_DDNNC_hour_4, mean_DDNNC_hour_5, mean_DDNNC_hour_6, mean_DDNNC_hour_7, mean_DDNNC_hour_8, mean_DDNNC_hour_9, mean_DDNNC_hour_10, mean_DDNNC_hour_11, mean_DDNNC_hour_12, mean_DDNNC_hour_13, mean_DDNNC_hour_14, mean_DDNNC_hour_15, mean_DDNNC_hour_16, mean_DDNNC_hour_17, mean_DDNNC_hour_18, mean_DDNNC_hour_19, mean_DDNNC_hour_20, mean_DDNNC_hour_21, mean_DDNNC_hour_22, mean_DDNNC_hour_23, mean_DDNNC_hour_24)
mean_LASSO <- c(mean_LASSO_hour_1, mean_LASSO_hour_2, mean_LASSO_hour_3, mean_LASSO_hour_4, mean_LASSO_hour_5, mean_LASSO_hour_6, mean_LASSO_hour_7, mean_LASSO_hour_8, mean_LASSO_hour_9, mean_LASSO_hour_10, mean_LASSO_hour_11, mean_LASSO_hour_12, mean_LASSO_hour_13, mean_LASSO_hour_14, mean_LASSO_hour_15, mean_LASSO_hour_16, mean_LASSO_hour_17, mean_LASSO_hour_18, mean_LASSO_hour_19, mean_LASSO_hour_20, mean_LASSO_hour_21, mean_LASSO_hour_22, mean_LASSO_hour_23, mean_LASSO_hour_24)
mean_CGNN <- c(mean_CGNN_hour_1, mean_CGNN_hour_2, mean_CGNN_hour_3, mean_CGNN_hour_4, mean_CGNN_hour_5, mean_CGNN_hour_6, mean_CGNN_hour_7, mean_CGNN_hour_8, mean_CGNN_hour_9, mean_CGNN_hour_10, mean_CGNN_hour_11, mean_CGNN_hour_12, mean_CGNN_hour_13, mean_CGNN_hour_14, mean_CGNN_hour_15, mean_CGNN_hour_16, mean_CGNN_hour_17, mean_CGNN_hour_18, mean_CGNN_hour_19, mean_CGNN_hour_20, mean_CGNN_hour_21, mean_CGNN_hour_22, mean_CGNN_hour_23, mean_CGNN_hour_24)

data <- data.frame(
  Hour = rep(hour, 3),
  Energy_Score = c(mean_DDNNC, mean_LASSO, mean_CGNN),
  Method = rep(c("DDNN", "LASSO_bootstrap", "CGNN"), each = 24)
)

ggplot(data, aes(x = Hour, y = Energy_Score, color = Method, group = Method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("DDNN" = "#008080", "LASSO_bootstrap" = "#800080", "CGNN" = "#FFA500")) +
  labs(x = "Hour", y = "Energy Score") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),  # Increase size of X axis title
        axis.title.y = element_text(size = 14),  # Increase size of Y axis title
        axis.text.x = element_text(size = 12),   # Increase size of X axis labels
        axis.text.y = element_text(size = 12),   # Increase size of Y axis labels
        legend.text = element_text(size = 12)    # Increase size of legend text
        )

# VS half

mean_DDNNC_hour_1 <- mean(VS_half_hour_0$DDNNC)
mean_DDNNC_hour_2 <- mean(VS_half_hour_1$DDNNC)
mean_DDNNC_hour_3 <- mean(VS_half_hour_2$DDNNC)
mean_DDNNC_hour_4 <- mean(VS_half_hour_3$DDNNC)
mean_DDNNC_hour_5 <- mean(VS_half_hour_4$DDNNC)
mean_DDNNC_hour_6 <- mean(VS_half_hour_5$DDNNC)
mean_DDNNC_hour_7 <- mean(VS_half_hour_6$DDNNC)
mean_DDNNC_hour_8 <- mean(VS_half_hour_7$DDNNC)
mean_DDNNC_hour_9 <- mean(VS_half_hour_8$DDNNC)
mean_DDNNC_hour_10 <- mean(VS_half_hour_9$DDNNC)
mean_DDNNC_hour_11 <- mean(VS_half_hour_10$DDNNC)
mean_DDNNC_hour_12 <- mean(VS_half_hour_11$DDNNC)
mean_DDNNC_hour_13 <- mean(VS_half_hour_12$DDNNC)
mean_DDNNC_hour_14 <- mean(VS_half_hour_13$DDNNC)
mean_DDNNC_hour_15 <- mean(VS_half_hour_14$DDNNC)
mean_DDNNC_hour_16 <- mean(VS_half_hour_15$DDNNC)
mean_DDNNC_hour_17 <- mean(VS_half_hour_16$DDNNC)
mean_DDNNC_hour_18 <- mean(VS_half_hour_17$DDNNC)
mean_DDNNC_hour_19 <- mean(VS_half_hour_18$DDNNC)
mean_DDNNC_hour_20 <- mean(VS_half_hour_19$DDNNC)
mean_DDNNC_hour_21 <- mean(VS_half_hour_20$DDNNC)
mean_DDNNC_hour_22 <- mean(VS_half_hour_21$DDNNC)
mean_DDNNC_hour_23 <- mean(VS_half_hour_22$DDNNC)
mean_DDNNC_hour_24 <- mean(VS_half_hour_23$DDNNC)

mean_LASSO_hour_1 <- mean(VS_half_hour_0$LASSO)
mean_LASSO_hour_2 <- mean(VS_half_hour_1$LASSO)
mean_LASSO_hour_3 <- mean(VS_half_hour_2$LASSO)
mean_LASSO_hour_4 <- mean(VS_half_hour_3$LASSO)
mean_LASSO_hour_5 <- mean(VS_half_hour_4$LASSO)
mean_LASSO_hour_6 <- mean(VS_half_hour_5$LASSO)
mean_LASSO_hour_7 <- mean(VS_half_hour_6$LASSO)
mean_LASSO_hour_8 <- mean(VS_half_hour_7$LASSO)
mean_LASSO_hour_9 <- mean(VS_half_hour_8$LASSO)
mean_LASSO_hour_10 <- mean(VS_half_hour_9$LASSO)
mean_LASSO_hour_11 <- mean(VS_half_hour_10$LASSO)
mean_LASSO_hour_12 <- mean(VS_half_hour_11$LASSO)
mean_LASSO_hour_13 <- mean(VS_half_hour_12$LASSO)
mean_LASSO_hour_14 <- mean(VS_half_hour_13$LASSO)
mean_LASSO_hour_15 <- mean(VS_half_hour_14$LASSO)
mean_LASSO_hour_16 <- mean(VS_half_hour_15$LASSO)
mean_LASSO_hour_17 <- mean(VS_half_hour_16$LASSO)
mean_LASSO_hour_18 <- mean(VS_half_hour_17$LASSO)
mean_LASSO_hour_19 <- mean(VS_half_hour_18$LASSO)
mean_LASSO_hour_20 <- mean(VS_half_hour_19$LASSO)
mean_LASSO_hour_21 <- mean(VS_half_hour_20$LASSO)
mean_LASSO_hour_22 <- mean(VS_half_hour_21$LASSO)
mean_LASSO_hour_23 <- mean(VS_half_hour_22$LASSO)
mean_LASSO_hour_24 <- mean(VS_half_hour_23$LASSO)

mean_CGNN_hour_1 <- mean(VS_half_hour_0$CGNN)
mean_CGNN_hour_2 <- mean(VS_half_hour_1$CGNN)
mean_CGNN_hour_3 <- mean(VS_half_hour_2$CGNN)
mean_CGNN_hour_4 <- mean(VS_half_hour_3$CGNN)
mean_CGNN_hour_5 <- mean(VS_half_hour_4$CGNN)
mean_CGNN_hour_6 <- mean(VS_half_hour_5$CGNN)
mean_CGNN_hour_7 <- mean(VS_half_hour_6$CGNN)
mean_CGNN_hour_8 <- mean(VS_half_hour_7$CGNN)
mean_CGNN_hour_9 <- mean(VS_half_hour_8$CGNN)
mean_CGNN_hour_10 <- mean(VS_half_hour_9$CGNN)
mean_CGNN_hour_11 <- mean(VS_half_hour_10$CGNN)
mean_CGNN_hour_12 <- mean(VS_half_hour_11$CGNN)
mean_CGNN_hour_13 <- mean(VS_half_hour_12$CGNN)
mean_CGNN_hour_14 <- mean(VS_half_hour_13$CGNN)
mean_CGNN_hour_15 <- mean(VS_half_hour_14$CGNN)
mean_CGNN_hour_16 <- mean(VS_half_hour_15$CGNN)
mean_CGNN_hour_17 <- mean(VS_half_hour_16$CGNN)
mean_CGNN_hour_18 <- mean(VS_half_hour_17$CGNN)
mean_CGNN_hour_19 <- mean(VS_half_hour_18$CGNN)
mean_CGNN_hour_20 <- mean(VS_half_hour_19$CGNN)
mean_CGNN_hour_21 <- mean(VS_half_hour_20$CGNN)
mean_CGNN_hour_22 <- mean(VS_half_hour_21$CGNN)
mean_CGNN_hour_23 <- mean(VS_half_hour_22$CGNN)
mean_CGNN_hour_24 <- mean(VS_half_hour_23$CGNN)

hour <- 1:24

mean_DDNNC <- c(mean_DDNNC_hour_1, mean_DDNNC_hour_2, mean_DDNNC_hour_3, mean_DDNNC_hour_4, mean_DDNNC_hour_5, mean_DDNNC_hour_6, mean_DDNNC_hour_7, mean_DDNNC_hour_8, mean_DDNNC_hour_9, mean_DDNNC_hour_10, mean_DDNNC_hour_11, mean_DDNNC_hour_12, mean_DDNNC_hour_13, mean_DDNNC_hour_14, mean_DDNNC_hour_15, mean_DDNNC_hour_16, mean_DDNNC_hour_17, mean_DDNNC_hour_18, mean_DDNNC_hour_19, mean_DDNNC_hour_20, mean_DDNNC_hour_21, mean_DDNNC_hour_22, mean_DDNNC_hour_23, mean_DDNNC_hour_24)
mean_LASSO <- c(mean_LASSO_hour_1, mean_LASSO_hour_2, mean_LASSO_hour_3, mean_LASSO_hour_4, mean_LASSO_hour_5, mean_LASSO_hour_6, mean_LASSO_hour_7, mean_LASSO_hour_8, mean_LASSO_hour_9, mean_LASSO_hour_10, mean_LASSO_hour_11, mean_LASSO_hour_12, mean_LASSO_hour_13, mean_LASSO_hour_14, mean_LASSO_hour_15, mean_LASSO_hour_16, mean_LASSO_hour_17, mean_LASSO_hour_18, mean_LASSO_hour_19, mean_LASSO_hour_20, mean_LASSO_hour_21, mean_LASSO_hour_22, mean_LASSO_hour_23, mean_LASSO_hour_24)
mean_CGNN <- c(mean_CGNN_hour_1, mean_CGNN_hour_2, mean_CGNN_hour_3, mean_CGNN_hour_4, mean_CGNN_hour_5, mean_CGNN_hour_6, mean_CGNN_hour_7, mean_CGNN_hour_8, mean_CGNN_hour_9, mean_CGNN_hour_10, mean_CGNN_hour_11, mean_CGNN_hour_12, mean_CGNN_hour_13, mean_CGNN_hour_14, mean_CGNN_hour_15, mean_CGNN_hour_16, mean_CGNN_hour_17, mean_CGNN_hour_18, mean_CGNN_hour_19, mean_CGNN_hour_20, mean_CGNN_hour_21, mean_CGNN_hour_22, mean_CGNN_hour_23, mean_CGNN_hour_24)

data <- data.frame(
  Hour = rep(hour, 3),
  Energy_Score = c(mean_DDNNC, mean_LASSO, mean_CGNN),
  Method = rep(c("DDNN", "LASSO_bootstrap", "CGNN"), each = 24)
)

ggplot(data, aes(x = Hour, y = Energy_Score, color = Method, group = Method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("DDNN" = "#008080", "LASSO_bootstrap" = "#800080", "CGNN" = "#FFA500")) +
  labs(x = "Hour", y = "Variogram Score with p = 0.5") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),  # Increase size of X axis title
        axis.title.y = element_text(size = 14),  # Increase size of Y axis title
        axis.text.x = element_text(size = 12),   # Increase size of X axis labels
        axis.text.y = element_text(size = 12),   # Increase size of Y axis labels
        legend.text = element_text(size = 12)    # Increase size of legend text
  )

# VS whole

mean_DDNNC_hour_1 <- mean(VS_whole_hour_0$DDNNC)
mean_DDNNC_hour_2 <- mean(VS_whole_hour_1$DDNNC)
mean_DDNNC_hour_3 <- mean(VS_whole_hour_2$DDNNC)
mean_DDNNC_hour_4 <- mean(VS_whole_hour_3$DDNNC)
mean_DDNNC_hour_5 <- mean(VS_whole_hour_4$DDNNC)
mean_DDNNC_hour_6 <- mean(VS_whole_hour_5$DDNNC)
mean_DDNNC_hour_7 <- mean(VS_whole_hour_6$DDNNC)
mean_DDNNC_hour_8 <- mean(VS_whole_hour_7$DDNNC)
mean_DDNNC_hour_9 <- mean(VS_whole_hour_8$DDNNC)
mean_DDNNC_hour_10 <- mean(VS_whole_hour_9$DDNNC)
mean_DDNNC_hour_11 <- mean(VS_whole_hour_10$DDNNC)
mean_DDNNC_hour_12 <- mean(VS_whole_hour_11$DDNNC)
mean_DDNNC_hour_13 <- mean(VS_whole_hour_12$DDNNC)
mean_DDNNC_hour_14 <- mean(VS_whole_hour_13$DDNNC)
mean_DDNNC_hour_15 <- mean(VS_whole_hour_14$DDNNC)
mean_DDNNC_hour_16 <- mean(VS_whole_hour_15$DDNNC)
mean_DDNNC_hour_17 <- mean(VS_whole_hour_16$DDNNC)
mean_DDNNC_hour_18 <- mean(VS_whole_hour_17$DDNNC)
mean_DDNNC_hour_19 <- mean(VS_whole_hour_18$DDNNC)
mean_DDNNC_hour_20 <- mean(VS_whole_hour_19$DDNNC)
mean_DDNNC_hour_21 <- mean(VS_whole_hour_20$DDNNC)
mean_DDNNC_hour_22 <- mean(VS_whole_hour_21$DDNNC)
mean_DDNNC_hour_23 <- mean(VS_whole_hour_22$DDNNC)
mean_DDNNC_hour_24 <- mean(VS_whole_hour_23$DDNNC)

mean_LASSO_hour_1 <- mean(VS_whole_hour_0$LASSO)
mean_LASSO_hour_2 <- mean(VS_whole_hour_1$LASSO)
mean_LASSO_hour_3 <- mean(VS_whole_hour_2$LASSO)
mean_LASSO_hour_4 <- mean(VS_whole_hour_3$LASSO)
mean_LASSO_hour_5 <- mean(VS_whole_hour_4$LASSO)
mean_LASSO_hour_6 <- mean(VS_whole_hour_5$LASSO)
mean_LASSO_hour_7 <- mean(VS_whole_hour_6$LASSO)
mean_LASSO_hour_8 <- mean(VS_whole_hour_7$LASSO)
mean_LASSO_hour_9 <- mean(VS_whole_hour_8$LASSO)
mean_LASSO_hour_10 <- mean(VS_whole_hour_9$LASSO)
mean_LASSO_hour_11 <- mean(VS_whole_hour_10$LASSO)
mean_LASSO_hour_12 <- mean(VS_whole_hour_11$LASSO)
mean_LASSO_hour_13 <- mean(VS_whole_hour_12$LASSO)
mean_LASSO_hour_14 <- mean(VS_whole_hour_13$LASSO)
mean_LASSO_hour_15 <- mean(VS_whole_hour_14$LASSO)
mean_LASSO_hour_16 <- mean(VS_whole_hour_15$LASSO)
mean_LASSO_hour_17 <- mean(VS_whole_hour_16$LASSO)
mean_LASSO_hour_18 <- mean(VS_whole_hour_17$LASSO)
mean_LASSO_hour_19 <- mean(VS_whole_hour_18$LASSO)
mean_LASSO_hour_20 <- mean(VS_whole_hour_19$LASSO)
mean_LASSO_hour_21 <- mean(VS_whole_hour_20$LASSO)
mean_LASSO_hour_22 <- mean(VS_whole_hour_21$LASSO)
mean_LASSO_hour_23 <- mean(VS_whole_hour_22$LASSO)
mean_LASSO_hour_24 <- mean(VS_whole_hour_23$LASSO)

mean_CGNN_hour_1 <- mean(VS_whole_hour_0$CGNN)
mean_CGNN_hour_2 <- mean(VS_whole_hour_1$CGNN)
mean_CGNN_hour_3 <- mean(VS_whole_hour_2$CGNN)
mean_CGNN_hour_4 <- mean(VS_whole_hour_3$CGNN)
mean_CGNN_hour_5 <- mean(VS_whole_hour_4$CGNN)
mean_CGNN_hour_6 <- mean(VS_whole_hour_5$CGNN)
mean_CGNN_hour_7 <- mean(VS_whole_hour_6$CGNN)
mean_CGNN_hour_8 <- mean(VS_whole_hour_7$CGNN)
mean_CGNN_hour_9 <- mean(VS_whole_hour_8$CGNN)
mean_CGNN_hour_10 <- mean(VS_whole_hour_9$CGNN)
mean_CGNN_hour_11 <- mean(VS_whole_hour_10$CGNN)
mean_CGNN_hour_12 <- mean(VS_whole_hour_11$CGNN)
mean_CGNN_hour_13 <- mean(VS_whole_hour_12$CGNN)
mean_CGNN_hour_14 <- mean(VS_whole_hour_13$CGNN)
mean_CGNN_hour_15 <- mean(VS_whole_hour_14$CGNN)
mean_CGNN_hour_16 <- mean(VS_whole_hour_15$CGNN)
mean_CGNN_hour_17 <- mean(VS_whole_hour_16$CGNN)
mean_CGNN_hour_18 <- mean(VS_whole_hour_17$CGNN)
mean_CGNN_hour_19 <- mean(VS_whole_hour_18$CGNN)
mean_CGNN_hour_20 <- mean(VS_whole_hour_19$CGNN)
mean_CGNN_hour_21 <- mean(VS_whole_hour_20$CGNN)
mean_CGNN_hour_22 <- mean(VS_whole_hour_21$CGNN)
mean_CGNN_hour_23 <- mean(VS_whole_hour_22$CGNN)
mean_CGNN_hour_24 <- mean(VS_whole_hour_23$CGNN)

hour <- 1:24

mean_DDNNC <- c(mean_DDNNC_hour_1, mean_DDNNC_hour_2, mean_DDNNC_hour_3, mean_DDNNC_hour_4, mean_DDNNC_hour_5, mean_DDNNC_hour_6, mean_DDNNC_hour_7, mean_DDNNC_hour_8, mean_DDNNC_hour_9, mean_DDNNC_hour_10, mean_DDNNC_hour_11, mean_DDNNC_hour_12, mean_DDNNC_hour_13, mean_DDNNC_hour_14, mean_DDNNC_hour_15, mean_DDNNC_hour_16, mean_DDNNC_hour_17, mean_DDNNC_hour_18, mean_DDNNC_hour_19, mean_DDNNC_hour_20, mean_DDNNC_hour_21, mean_DDNNC_hour_22, mean_DDNNC_hour_23, mean_DDNNC_hour_24)
mean_LASSO <- c(mean_LASSO_hour_1, mean_LASSO_hour_2, mean_LASSO_hour_3, mean_LASSO_hour_4, mean_LASSO_hour_5, mean_LASSO_hour_6, mean_LASSO_hour_7, mean_LASSO_hour_8, mean_LASSO_hour_9, mean_LASSO_hour_10, mean_LASSO_hour_11, mean_LASSO_hour_12, mean_LASSO_hour_13, mean_LASSO_hour_14, mean_LASSO_hour_15, mean_LASSO_hour_16, mean_LASSO_hour_17, mean_LASSO_hour_18, mean_LASSO_hour_19, mean_LASSO_hour_20, mean_LASSO_hour_21, mean_LASSO_hour_22, mean_LASSO_hour_23, mean_LASSO_hour_24)
mean_CGNN <- c(mean_CGNN_hour_1, mean_CGNN_hour_2, mean_CGNN_hour_3, mean_CGNN_hour_4, mean_CGNN_hour_5, mean_CGNN_hour_6, mean_CGNN_hour_7, mean_CGNN_hour_8, mean_CGNN_hour_9, mean_CGNN_hour_10, mean_CGNN_hour_11, mean_CGNN_hour_12, mean_CGNN_hour_13, mean_CGNN_hour_14, mean_CGNN_hour_15, mean_CGNN_hour_16, mean_CGNN_hour_17, mean_CGNN_hour_18, mean_CGNN_hour_19, mean_CGNN_hour_20, mean_CGNN_hour_21, mean_CGNN_hour_22, mean_CGNN_hour_23, mean_CGNN_hour_24)

data <- data.frame(
  Hour = rep(hour, 3),
  Energy_Score = c(mean_DDNNC, mean_LASSO, mean_CGNN),
  Method = rep(c("DDNN", "LASSO_bootstrap", "CGNN"), each = 24)
)

ggplot(data, aes(x = Hour, y = Energy_Score, color = Method, group = Method)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("DDNN" = "#008080", "LASSO_bootstrap" = "#800080", "CGNN" = "#FFA500")) +
  labs(x = "Hour", y = "Variogram Score with p = 1") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.x = element_text(size = 14),  # Increase size of X axis title
        axis.title.y = element_text(size = 14),  # Increase size of Y axis title
        axis.text.x = element_text(size = 12),   # Increase size of X axis labels
        axis.text.y = element_text(size = 12),   # Increase size of Y axis labels
        legend.text = element_text(size = 12)    # Increase size of legend text
  )

# Weekly figures --------------------------------------------------------------------------------------------
num_rows_per_week <- 7 * 24  # 7 days * 24 hours = 168 rows

ES_weekly_dfs <- split(ES, ceiling(seq_len(nrow(ES)) / num_rows_per_week))
VS_half_weekly_dfs <- split(VS_half, ceiling(seq_len(nrow(VS_half)) / num_rows_per_week))
VS_whole_weekly_dfs <- split(VS_whole, ceiling(seq_len(nrow(VS_whole)) / num_rows_per_week))

ES_week_1 <- ES_weekly_dfs[[1]]
ES_week_2 <- ES_weekly_dfs[[2]]
ES_week_3 <- ES_weekly_dfs[[3]]
ES_week_4 <- ES_weekly_dfs[[4]]
ES_week_5 <- ES_weekly_dfs[[5]]
ES_week_6 <- ES_weekly_dfs[[6]]
ES_week_7 <- ES_weekly_dfs[[7]]
ES_week_8 <- ES_weekly_dfs[[8]]
ES_week_9 <- ES_weekly_dfs[[9]]
ES_week_10 <- ES_weekly_dfs[[10]]
ES_week_11 <- ES_weekly_dfs[[11]]
ES_week_12 <- ES_weekly_dfs[[12]]
ES_week_13 <- ES_weekly_dfs[[13]]
ES_week_14 <- ES_weekly_dfs[[14]]
ES_week_15 <- ES_weekly_dfs[[15]]
ES_week_16 <- ES_weekly_dfs[[16]]
ES_week_17 <- ES_weekly_dfs[[17]]
ES_week_18 <- ES_weekly_dfs[[18]]
ES_week_19 <- ES_weekly_dfs[[19]]
ES_week_20 <- ES_weekly_dfs[[20]]
ES_week_21 <- ES_weekly_dfs[[21]]
ES_week_22 <- ES_weekly_dfs[[22]]
ES_week_23 <- ES_weekly_dfs[[23]]
ES_week_24 <- ES_weekly_dfs[[24]]
ES_week_25 <- ES_weekly_dfs[[25]]
ES_week_26 <- ES_weekly_dfs[[26]]
ES_week_27 <- ES_weekly_dfs[[27]]
ES_week_28 <- ES_weekly_dfs[[28]]

VS_half_week_1 <- VS_half_weekly_dfs[[1]]
VS_half_week_2 <- VS_half_weekly_dfs[[2]]
VS_half_week_3 <- VS_half_weekly_dfs[[3]]
VS_half_week_4 <- VS_half_weekly_dfs[[4]]
VS_half_week_5 <- VS_half_weekly_dfs[[5]]
VS_half_week_6 <- VS_half_weekly_dfs[[6]]
VS_half_week_7 <- VS_half_weekly_dfs[[7]]
VS_half_week_8 <- VS_half_weekly_dfs[[8]]
VS_half_week_9 <- VS_half_weekly_dfs[[9]]
VS_half_week_10 <- VS_half_weekly_dfs[[10]]
VS_half_week_11 <- VS_half_weekly_dfs[[11]]
VS_half_week_12 <- VS_half_weekly_dfs[[12]]
VS_half_week_13 <- VS_half_weekly_dfs[[13]]
VS_half_week_14 <- VS_half_weekly_dfs[[14]]
VS_half_week_15 <- VS_half_weekly_dfs[[15]]
VS_half_week_16 <- VS_half_weekly_dfs[[16]]
VS_half_week_17 <- VS_half_weekly_dfs[[17]]
VS_half_week_18 <- VS_half_weekly_dfs[[18]]
VS_half_week_19 <- VS_half_weekly_dfs[[19]]
VS_half_week_20 <- VS_half_weekly_dfs[[20]]
VS_half_week_21 <- VS_half_weekly_dfs[[21]]
VS_half_week_22 <- VS_half_weekly_dfs[[22]]
VS_half_week_23 <- VS_half_weekly_dfs[[23]]
VS_half_week_24 <- VS_half_weekly_dfs[[24]]
VS_half_week_25 <- VS_half_weekly_dfs[[25]]
VS_half_week_26 <- VS_half_weekly_dfs[[26]]
VS_half_week_27 <- VS_half_weekly_dfs[[27]]
VS_half_week_28 <- VS_half_weekly_dfs[[28]]

VS_whole_week_1 <- VS_whole_weekly_dfs[[1]]
VS_whole_week_2 <- VS_whole_weekly_dfs[[2]]
VS_whole_week_3 <- VS_whole_weekly_dfs[[3]]
VS_whole_week_4 <- VS_whole_weekly_dfs[[4]]
VS_whole_week_5 <- VS_whole_weekly_dfs[[5]]
VS_whole_week_6 <- VS_whole_weekly_dfs[[6]]
VS_whole_week_7 <- VS_whole_weekly_dfs[[7]]
VS_whole_week_8 <- VS_whole_weekly_dfs[[8]]
VS_whole_week_9 <- VS_whole_weekly_dfs[[9]]
VS_whole_week_10 <- VS_whole_weekly_dfs[[10]]
VS_whole_week_11 <- VS_whole_weekly_dfs[[11]]
VS_whole_week_12 <- VS_whole_weekly_dfs[[12]]
VS_whole_week_13 <- VS_whole_weekly_dfs[[13]]
VS_whole_week_14 <- VS_whole_weekly_dfs[[14]]
VS_whole_week_15 <- VS_whole_weekly_dfs[[15]]
VS_whole_week_16 <- VS_whole_weekly_dfs[[16]]
VS_whole_week_17 <- VS_whole_weekly_dfs[[17]]
VS_whole_week_18 <- VS_whole_weekly_dfs[[18]]
VS_whole_week_19 <- VS_whole_weekly_dfs[[19]]
VS_whole_week_20 <- VS_whole_weekly_dfs[[20]]
VS_whole_week_21 <- VS_whole_weekly_dfs[[21]]
VS_whole_week_22 <- VS_whole_weekly_dfs[[22]]
VS_whole_week_23 <- VS_whole_weekly_dfs[[23]]
VS_whole_week_24 <- VS_whole_weekly_dfs[[24]]
VS_whole_week_25 <- VS_whole_weekly_dfs[[25]]
VS_whole_week_26 <- VS_whole_weekly_dfs[[26]]
VS_whole_week_27 <- VS_whole_weekly_dfs[[27]]
VS_whole_week_28 <- VS_whole_weekly_dfs[[28]]
