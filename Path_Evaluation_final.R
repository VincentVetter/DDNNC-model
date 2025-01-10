library(scoringRules)
library(arrow)

setwd("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code")

mu = 39.966422871197395
sigma = 19.256267738598343

data_used <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/data_used.csv", header = FALSE)
normalized_prices <- data_used[13081:20064, 103:112]
true_prices <- normalized_prices * sigma + mu

path_forecast8 <- read_csv_arrow("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/path_forecast8.csv", col_names = FALSE)
dim(path_forecast8)
path_forecast8[1,1]
path_forecast8[36*24,1]
path_forecast <- path_forecast8 * sigma + mu



ES <- vector(mode = "numeric", length = 36*24)
VS_half <- vector(mode = "numeric", length = 36*24)
VS_whole <- vector(mode = "numeric", length = 36*24)


weight <- matrix(1/100, nrow = 10, ncol = 10)

for (day in 0:35) {
  for (hour in 0:23) {
    y_true <- unlist(true_prices[(day*24+hour+1)+(37+37+37+36+36+36+36)*24,])
    y_pred <- as.matrix(path_forecast[(day*24*10+hour*10+1):(day*24*10+hour*10+10),])
    
    ES[day*24+hour+1] <- es_sample(y_true,y_pred)
    VS_half[day*24+hour+1] <- vs_sample(y_true,y_pred,w_vs = weight, p = 0.5)
    VS_whole[day*24+hour+1] <- vs_sample(y_true,y_pred,w_vs = weight, p = 1)
  }
}


write.csv(data.frame(ES), "ES8.csv", row.names = FALSE)
write.csv(data.frame(VS_half), "VS_half8.csv", row.names = FALSE)
write.csv(data.frame(VS_whole), "VS_whole8.csv", row.names = FALSE)

# Merging the results:

ES1 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES1.csv", header = TRUE)
ES2 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES2.csv", header = TRUE)
ES3 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES3.csv", header = TRUE)
ES4 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES4.csv", header = TRUE)
ES5 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES5.csv", header = TRUE)
ES6 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES6.csv", header = TRUE)
ES7 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES7.csv", header = TRUE)
ES8 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/ES8.csv", header = TRUE)

ES <- c(ES1$ES1, ES2$ES, ES3$ES, ES4$ES, ES5$ES, ES6$ES, ES7$ES, ES8$ES)
ES <- ES[(length(ES) - 4799):length(ES)]
write.csv(data.frame(ES), "es_ddnnc.csv", row.names = FALSE)


VS_half1 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half1.csv", header = TRUE)
VS_half2 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half2.csv", header = TRUE)
VS_half3 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half3.csv", header = TRUE)
VS_half4 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half4.csv", header = TRUE)
VS_half5 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half5.csv", header = TRUE)
VS_half6 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half6.csv", header = TRUE)
VS_half7 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half7.csv", header = TRUE)
VS_half8 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_half8.csv", header = TRUE)

VS_half <- c(VS_half1$VS_half1, VS_half2$VS_half, VS_half3$VS_half, VS_half4$VS_half, VS_half5$VS_half, VS_half6$VS_half, VS_half7$VS_half, VS_half8$VS_half)
VS_half <- VS_half[(length(VS_half) - 4799):length(VS_half)]
write.csv(data.frame(VS_half), "vs2_ddnnc.csv", row.names = FALSE)


VS_whole1 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole1.csv", header = TRUE)
VS_whole2 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole2.csv", header = TRUE)
VS_whole3 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole3.csv", header = TRUE)
VS_whole4 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole4.csv", header = TRUE)
VS_whole5 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole5.csv", header = TRUE)
VS_whole6 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole6.csv", header = TRUE)
VS_whole7 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole7.csv", header = TRUE)
VS_whole8 <- read.csv("C:/Users/v_vet/OneDrive/Dokumente/Seminar Topics in Econometrics/Code/VS_whole8.csv", header = TRUE)

VS_whole <- c(VS_whole1$VS_whole1, VS_whole2$VS_whole, VS_whole3$VS_whole, VS_whole4$VS_whole, VS_whole5$VS_whole, VS_whole6$VS_whole, VS_whole7$VS_whole, VS_whole8$VS_whole)
VS_whole <- VS_whole[(length(VS_whole) - 4799):length(VS_whole)]
write.csv(data.frame(VS_whole), "vs1_ddnnc.csv", row.names = FALSE)
