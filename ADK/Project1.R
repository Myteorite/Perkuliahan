#3.5
#1
# Unduh data dan simpan ke data frame
url <- "https://users.stat.ufl.edu/~aa/cat/data/Crabs.dat"
crab_data <- read.table(url, header = FALSE)
head(crab_data)

#A
crab <- data.frame(
  weight = as.numeric(crab_data$V4),
  y_original = as.numeric(crab_data$V2)
)

# Buat variabel biner (misalnya: apakah y_original > 0)
crab$y <- ifelse(crab$y_original > 0, 1, 0)
crab

# Pastikan tidak ada NA
crab <- na.omit(crab)

# Cek nilai y
head(crab$y)

# Model regresi linier
lpm_model <- lm(y ~ weight, data = crab)
summary(lpm_model)

# Prediksi nilai y untuk weight = 5.20
new_weight <- data.frame(weight = 5.20)
predict(lpm_model, newdata = new_weight)

#B
# Model regresi logistik
logit_model <- glm(y ~ weight, data = crab, family = binomial(link = "logit"))
summary(logit_model)

# Prediksi probabilitas
print(predict(logit_model, newdata = new_weight, type = "response"), digits = 10)

#3.6
y <- c(5,18,19,25,7,7,2); n <- c(6,21,20,36,17,18,3)
x <- c(1,2,3,4,5,6,7)
fit <- glm(y/n ~ x, family=binomial(link=logit), weights=n)
summary(fit)

#3.7
