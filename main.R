#renv::init()

# Read the file
dados <- read.csv("Dados.csv")
dados_df <- data.frame(dados)
dados_df

# Identify the factors
power <- factor(dados_df$Power..W.)
speed <- factor(dados_df$Scanning.speed..mm.s.)

# Descriptive statistics
mean_H1 <- mean(dados_df$H1)
max_H1 <- max(dados_df$H1)
min_H1 <- min(dados_df$H1)

mean_H2 <- mean(dados_df$H2)
max_H2 <- max(dados_df$H2)
min_H2 <- min(dados_df$H2)

mean_W2 <- mean(dados_df$W2)
max_W2 <- max(dados_df$W2)
min_W2 <- min(dados_df$W2)

# Create a data frame to each response
stat_H1 <- data.frame(mean_H1, max_H1, min_H1)
colnames(stat_H1) <- c("mean [um]", "max [um]", "min [um]")

stat_H2 <- data.frame(mean_H2, max_H2, min_H2)
colnames(stat_H2) <- c("mean [um]", "max [um]", "min [um]")

stat_W2 <- data.frame(mean_W2, max_W2, min_W2)
colnames(stat_W2) <- c("mean [um]", "max [um]", "min [um]")

# Combine in one data frame with all the responses
descrip_stat <- rbind(stat_H1, stat_H2, stat_W2)
rownames(descrip_stat) <- c('H1', 'H2', 'W2')
print(descrip_stat)

# Evaluate the influence of the factors in each response
# ANOVA H1
model_H1 <- aov(dados_df$H1 ~ power * speed)
summary(model_H1)

# ANOVA H2
model_H2 <- aov(dados_df$H2 ~ power * speed)
summary(model_H2)

# ANOVA W2
model_W2 <- aov(dados_df$W2 ~ power * speed)
summary(model_W2)

# Make some graphs
plot(dados_df$H1 ~ power)

# Boxplot
png("boxplot.png", width = 2200, height = 1300, res = 300)
par(mfrow = c(1, 1), family = "serif")
boxplot(dados_df$H1, dados_df$H2, dados_df$W2, xaxt = "n")
axis(1, at = seq(1, 3, by = 1), labels = c("H1", "H2", "W2"))
title(main = "Boxplot of the responses", cex.main = 2, font.main = 3, line = 0.5)
title(ylab = bquote("Value [um]"), line = 2.5)
dev.off()

# QQ-Norm
png("qq-plot_H1.png", width = 2200, height = 1300, res = 300)
par(mfrow = c(1, 1), family = "serif")
qqnorm(residuals(model_H1))
qqline(residuals(model_H1))
dev.off()

png("qq-plot_H2.png", width = 2200, height = 1300, res = 300)
par(mfrow = c(1, 1), family = "serif")
qqnorm(residuals(model_H2))
qqline(residuals(model_H2))
dev.off()

png("qq-plot_W2.png", width = 2200, height = 1300, res = 300)
par(mfrow = c(1, 1), family = "serif")
qqnorm(residuals(model_W2))
qqline(residuals(model_W2))
dev.off()

# Visual residual analysis
png("four_diagnosis_plots.png", width = 2200, height = 1300, res = 300)
par(mfrow = c(2, 2), family = "serif")
plot(model_H2)
dev.off()

# Check residuals normality with Shapiro Wilk Test
shapiro.test(residuals(model_H1))

# Extra
# Visualize the response related to factors' interaction
interaction.plot(power, speed, dados_df$H2)

# Density, residuals, and histograms
install.packages("ggpubr")
library("ggpubr")
ggdensity(dados_df$H2, xlab = "H2")
ggqqplot(dados_df$H2)
hist(dados_df$H1, breaks = 5)

# Check responses normality distribution
shapiro.test(dados_df$H1)
shapiro.test(dados_df$H2)
