file_path <- "C:/Users/quinn/Downloads/ST 625 - Group Project(New Data).csv"
data <- read.csv(file_path)

#nice color pallette, thanks https://htmlcolorcodes.com/
plot_colors <- c(lightblue = "#87CEEB", darkorange = "#FA8072", beige = "#F5DEB3",
                 lightpurple = "#D8BFD8", lightgreen = "#90EE90", lightred = "#F08080",
                 orange = "#FFA500", pink="#FFB6C1", mediumblue="#6495ED")


#directory to save plots
plot_dir <- "plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir)
  print("created plot directory")
} else {
  print("plot directory already exists")
}

response_var <- "R.PA"

data$LgF <- ifelse(data$Lg == "NL", 1, 0)
print("LgF")
data$BatsRight <- ifelse(data$Bats == "Right", 1, 0)
print("Bats")
data$ThrowsRight <- ifelse(data$Throws == "Right", 1, 0)
print("Throws")

data$BirthplaceF <- as.factor(data$Birthplace)
print("BirthplaceF")

png(filename = file.path(plot_dir, "categorical_distributions.png"), width = 1920, height = 1080)
par(mfrow=c(1, 3), cex.main = 2.0, cex.lab = 2.0, cex.axis = 1.3, mar = c(5, 5, 4, 2) + 0.1) # Increased margins slightly too


hist_league <- hist(data$LgF, main="Distribution of League", xlab="League (1=NL, 0=AL)",
     col=plot_colors["lightblue"], breaks=2, probability = TRUE)

hist_bats <- hist(data$BatsRight, main="Distribution of Bats", xlab="Bats (1=Right, 0=Left)",
     col=plot_colors["lightred"], breaks=2, probability = TRUE)

hist_throws <- hist(data$ThrowsRight, main="Distribution of Throws", xlab="Throws (1=Right, 0=Left)",
     col=plot_colors["lightgreen"], breaks=2, probability = TRUE)

dev.off()





#univariate distribution plots
png(filename = file.path(plot_dir, "univariate_distributions.png"), width = 1920, height = 1080)
#par(mfrow=c(1, 3))
par(mfrow=c(1, 3), cex.main = 2.0, cex.lab = 2.0, cex.axis = 1.3, mar = c(5, 5, 4, 2) + 0.1) # Increased margins slightly too



hist_age <- hist(data$Age, main="Distribution of Player Age", xlab="Age (years)",
     col=plot_colors["lightblue"], breaks=25, probability = TRUE)



#distribution of weight
hist_weight <- hist(data$Weight, main="Distribution of Player Weight", xlab="Weight (lbs)",
     col=plot_colors["lightred"], breaks=25, probability = TRUE)

#distribution of height
hist_height <- hist(data$Height, main="Distribution of Player Height", xlab="Height (inches)",
     col=plot_colors["lightgreen"], breaks=25, probability = TRUE)

dev.off()



