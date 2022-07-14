max_bitrate_target <- 192.0

#input initial raw values
raw_aac_target <- list(c(80.0, 3.01), c(96.0, 3.94),
                        c(112.0, 4.33), c(128.0, 4.66),
                        c(144.0, 4.85), c(96.0, 4.40),
                        c(64.0, 4.77125), c(192.0, 4.94))
raw_opus_target <- list(c(80.0, 3.40), c(96.0, 4.02),
                        c(112.0, 4.33), c(128.0, 4.61),
                        c(96.0, 4.65), c(64.0, 4.535),
                        c(80.0, 4.585), c(128.0, 4.775),
                        c(64.0, 4.77125), c(192, 5.0))
raw_mp3_target <- list(c(128.0, 4.24), c(96.0, 4.555),
                        c(192.0, 4.46), c(128.0, 4.51))
raw_aac_actual <- list(c(75.6, 3.01), c(96.8, 3.94),
                        c(112.7, 4.66), c(142.2, 4.85),
                        c(104.0, 4.40), c(70.70, 4.77125))
raw_opus_actual <- list(c(85.8, 3.40), c(103.04, 4.02),
                        c(119.3, 4.33), c(136.8, 4.61),
                        c(107.0, 4.65), c(68.3, 4.99875))
raw_mp3_actual <- list(c(136.0, 4.24), c(143.0, 4.51))

#normalize all values to account for different bitrates
#(function used: score * (max_bitrate/x)
normalize_data <- function(data) {
  return_vector <- c()

  for (i in data) {
    factor <- max_bitrate_target / i[1]
    return_vector <- c(return_vector, i[2] * factor)
  }

  return(return_vector)
}

normalized_aac_target <- normalize_data(raw_aac_target)
normalized_opus_target <- normalize_data(raw_opus_target)
normalized_mp3_target <- normalize_data(raw_mp3_target)

normalized_aac_actual <- normalize_data(raw_aac_actual)
normalized_opus_actual <- normalize_data(raw_opus_actual)
normalized_mp3_actual <- normalize_data(raw_mp3_actual)

#calculate means and standard deviation
mean_aac_target <- mean(normalized_aac_target)
mean_opus_target <- mean(normalized_opus_target)
mean_mp3_target <- mean(normalized_mp3_target)

deviation_aac_target <- sd(normalized_aac_target)
deviation_opus_target <- sd(normalized_opus_target)
deviation_mp3_target <- sd(normalized_mp3_target)

mean_aac_actual <- mean(normalized_aac_actual)
mean_opus_actual <- mean(normalized_opus_actual)
mean_mp3_actual <- mean(normalized_opus_actual)

deviation_aac_actual <- sd(normalized_aac_actual)
deviation_opus_actual <- sd(normalized_opus_actual)
deviation_mp3_actual <- sd(normalized_mp3_actual)

#create dataframe for plot
df <- data.frame(
    Mean = c(mean_aac_target, mean_aac_actual, mean_opus_target,
             mean_opus_actual, mean_mp3_target, mean_mp3_actual),
    sd = c(deviation_aac_target, deviation_aac_actual,
            deviation_opus_target, deviation_opus_actual,
            deviation_mp3_target, deviation_mp3_actual),
    Category = c("AAC[target]", "AAC[actual]", "OPUS[target]",
                  "OPUS[actual]", "MP3[target]", "MP3[actual]")
)

#"tell" ggplot that Data does not need to be sorted / is already sorted
df$Category <- factor(df$Category, levels = df$Category)

library(ggplot2)

ggplot(df, aes(x = Category, y = Mean)) +
  geom_bar(position = position_dodge(), stat = "identity",
            colour = "black", fill = "turquoise") +
  geom_errorbar(aes(ymin = Mean - sd, ymax = Mean + sd))

#-------------------------------------------
#Inferenzstatistische Pruefung der Ergebnisse
#-------------------------------------------

#Create and fill dataframes for actual and target results
actual_dataframe <- data.frame(
  codec = character(), score = numeric(), stringsAsFactors = FALSE)
target_dataframe <- data.frame(
  codec = character(), score = numeric(), stringsAsFactors = FALSE)


append_to_frame <- function(frame, values, codec) {
    for (current_value in values) {
      frame[nrow(frame) + 1, ] <- c(codec, current_value)
    }
    return(frame)
}

actual_dataframe <- append_to_frame(
  actual_dataframe, normalized_aac_actual, "AAC")
target_dataframe <- append_to_frame(
  target_dataframe, normalized_aac_target, "AAC")
actual_dataframe <- append_to_frame(
  actual_dataframe, normalized_mp3_actual, "MP3")
target_dataframe <- append_to_frame(
  target_dataframe, normalized_mp3_target, "MP3")
actual_dataframe <- append_to_frame(
  actual_dataframe, normalized_opus_actual, "OPUS")
target_dataframe <- append_to_frame(
  target_dataframe, normalized_opus_target, "OPUS")

actual_dataframe$score <- as.numeric(actual_dataframe$score)
target_dataframe$score <- as.numeric(target_dataframe$score)

library(car)

#P > 0.05 (0.7246) => Varianzhomogenitaet kann angenommen werden
#==> Die Gruppen haben in etwa dieselbe Varianz
leveneTest(target_dataframe$score, target_dataframe$codec)
#P > 0.05 (0.342) => Mittelwerte der einzelnen Gruppen sind
#nicht signifikant verschieden
aov_result <- aov(score ~ codec, data = target_dataframe)
summary(aov_result)
#P zwar < 0.05 (0.03954), kann aber ignoriert werden, da Anova robust
#gegenueber der Verletzung der Annahme der Normalverteilung
shapiro.test(residuals(aov_result))


#P > 0.05 (0.7062) => Varianzhomogenitaet kann angenommen werden
#==> Die Gruppen haben in etwa dieselbe Varianz
leveneTest(actual_dataframe$score, actual_dataframe$codec)
#P > 0.05 (0.436) => Mittelwerte der einzelnen Gruppen sind
#nicht signifikant verschieden
aov_result <- aov(score ~ codec, data = actual_dataframe)
summary(aov_result)
#P zwar < 0.05 (0.0005424), kann aber ignoriert werden, da Anova robust
#gegenueber der Verletzung der Annahme der Normalverteilung
shapiro.test(residuals(aov_result))