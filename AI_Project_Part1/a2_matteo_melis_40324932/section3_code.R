require("rstudioapi")
require("stringr")
require("raster")
require("rmarkdown")
require("ggplot2")
require("gridExtra")
require("dplyr")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("section2_code.r")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#1 (a), (b), (c)
data <- read.csv(paste(getwd(), "/40324932_features.csv", sep=""))
living_df <- filter(data, labels == "banana" | labels == "cherry" | labels == "lemon" | labels == "tree")
non_living_df <- filter(data, labels == "envelope" | labels == "golfclub" | labels == "pencil" | labels == "wineglass")

ggplot(data = non_living_df, aes(x=nr_pix)) + 
  geom_histogram(fill="red", color="darkred", bins=30) + ylab('Frequency') + xlab('Number of black pixels in image') + ggtitle('Black Pixels for non-living things') -> p1

#living
ggplot(data = living_df, aes(x=nr_pix)) + 
  geom_histogram(fill="green", color="darkgreen", bins=30) + ylab('Frequency') + xlab('Number of black pixels in image') + ggtitle('Black Pixels for living things') -> p2

#all
ggplot(data = csv_df, aes(x=nr_pix)) + 
  geom_histogram(fill="lightblue", color="darkblue", bins=30) + ylab('Frequency') + xlab('Number of black pixels in image') + ggtitle('Black Pixels for all images') -> p3

grid.arrange(p1, p2, p3, ncol = 3)

#height
#non-living
ggplot(data = non_living_df, aes(x=height)) + 
  geom_histogram(fill="red", color="darkred", bins=30) + ylab('Frequency') + xlab('Height of image') + ggtitle('Heights of non-living things') -> p1

#living
ggplot(data = living_df, aes(x=height)) + 
  geom_histogram(fill="green", color="darkgreen", bins=30) + ylab('Frequency') + xlab('Height of image') + ggtitle('Heights of living things') -> p2

#all
ggplot(data = csv_df, aes(x=height)) + 
  geom_histogram(fill="lightblue", color="darkblue", bins=30) + ylab('Frequency') + xlab('Height of image') + ggtitle('Heights of all images') -> p3

grid.arrange(p1, p2, p3, ncol = 3)

#width
#non-living
ggplot(data = non_living_df, aes(x=width)) + 
  geom_histogram(fill="red", color="darkred", bins=30) + ylab('Frequency') + xlab('Width of image') + ggtitle('Widths of non-living things') -> p1

#living
ggplot(data = living_df, aes(x=width)) + 
  geom_histogram(fill="green", color="darkgreen", bins=30) + ylab('Frequency') + xlab('Width of image') + ggtitle('Widths of living things') -> p2

#all
ggplot(data = csv_df, aes(x=width)) + 
  geom_histogram(fill="lightblue", color="darkblue", bins=30) + ylab('Frequency') + xlab('Width of image') + ggtitle('Widths of all images') -> p3

grid.arrange(p1, p2, p3, ncol = 3)

#aspect_ratio
#non-living
ggplot(data = non_living_df, aes(x=aspect_ratio)) + 
  geom_histogram(fill="red", color="darkred", bins=30) + ylab('Frequency') + xlab('Aspect ratio of image') + ggtitle('Aspect ratios of non-living things') -> p1

#living
ggplot(data = living_df, aes(x=aspect_ratio)) + 
  geom_histogram(fill="green", color="darkgreen", bins=30) + ylab('Frequency') + xlab('Aspect ratio of image') + ggtitle('Aspect ratios of living things') -> p2

#all
ggplot(data = csv_df, aes(x=aspect_ratio)) + 
  geom_histogram(fill="lightblue", color="darkblue", bins=30) + ylab('Frequency') + xlab('Aspect ratio of image') + ggtitle('Aspect ratios of all images') -> p3

grid.arrange(p1, p2, p3, ncol = 3)


#3.2
#calculating mean, var and sd
mean_nr_pix <- mean(nr_pix)
var_nr_pix <- var(nr_pix)
sd_nr_pix <- sqrt(var_nr_pix)

#setting range to 3 sd above and below mean
x = (mean_nr_pix - 3*sd_nr_pix):(mean_nr_pix + 3*sd_nr_pix)
#normal densities
densities = dnorm(x, mean_nr_pix, sd_nr_pix)

#plotting normal distribution
plot(x, densities, type="l", col = "red", lty=1, lwd=3, xlab ="Pixels", ylab="Density", main="Normal Distribution using Mean and SD of nr_pix (i.e mean = 120, sd = 31.5)")

ggplot(data = csv_df, aes(x=nr_pix))  + 
  geom_histogram(fill="lightblue", color="darkblue", bins=30) +  ylab('Frequency') + xlab('Number of black pixels in image') +
  stat_function(fun = function(x) dnorm(x, mean = mean_nr_pix, sd = sd_nr_pix) * 300 * 25/7, color = "darkred", linewidth = 2)


#3.3
rtail = qnorm(0.95, mean_nr_pix, sd_nr_pix)
print(rtail)


#3.4
library(knitr)
all_funcs <- list(nr_pix, rows_with_1, cols_with_1, rows_with_2, cols_with_2, 
                  rows_with_3p, cols_with_3p, height, width, aspect_ratio, maxrow, maxcol,
                  connected_areas, eyes, hollowness, custom)

names(all_funcs) <- c("nr_pix", "rows_with_1", "cols_with_1", "rows_with_2", "cols_with_2", 
                      "rows_with_3p", "cols_with_3p", "height", "width", "aspect_ratio", "maxrow",
                      "maxcol", "connected_areas", "eyes", "hollowness", "custom")

#keep track of plots and index
index <- 1 
num_of_plot <- 1
plots <- list()
p_vals <- c()
for(f in all_funcs){
  
  # Seperate the living things and place them in a list
  l1 <- f[1:28]
  l2 <- f[57:70]
  l3 <- f[85:98]
  living_things <- c(l1, l2, l3)
  
  #Seperate the nonliving things and place them in a list
  nl1 <- f[29:56]
  nl2 <- f[71:84]
  nl3 <- f[99:112]
  non_living_things <- c(nl1, nl2, nl3)
  
  # Observed difference in means between living and non-living:
  obsdiff = mean(living_things)-mean(non_living_things)
  nsims = 10000
  
  # Randomization test for difference in means
  ############################################
  All <- c(living_things,non_living_things) # put all values in a single vector
  
  nulldiffs <- rep(0,nsims) 
  # nulldiffs will contain all our simulated differences in means (assuming the null hypothesis is true). 
  # Initialise with 0s.
  
  for (ni in 1:nsims) {
    # randomly reorder `All`
    randomA = sample(All) 
    # create the random group partition
    randomLiving = randomA[1:length(living_things)]
    randomNonLiving = randomA[(length(living_things)+1):length(All)]
    # calculate the differences in means for the two groups
    # and add it to our result vector
    nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
  }
  
  # What proportion of the differences in means calculated with randomization
  # are greater in magnitude than than the observed, true difference in means?
  pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims
  
  # 95% of null-distribution differences lie in this range:
  quantile(nulldiffs, 0.025)
  quantile(nulldiffs, 0.975)
  
  # data frame for plotting:
  df <- as.data.frame(nulldiffs)
  df$group <- "randomization"
  colnames(df) <- c("test.statistic", "group")
  df$group <- as.factor(df$group)
  
  #get the current variable name we are analysing
  name <- names(all_funcs[index])
  index <- index + 1
  
  # Generate Histograms of the data, by group
  p1 <- ggplot(df, aes(x=test.statistic, fill=group, color=group)) +
    geom_histogram(position="identity", colour="darkgreen", fill="green", bins=30)+
    geom_vline(xintercept = obsdiff, linetype="dotted", color = "red", linewidth=1)+ xlab(name) + theme(legend.position = "none") # no legend
  
  if(pvalue < 0.05){
    p_vals <- append(p_vals, pvalue)
  }
  #placing the plots in a list so we can produce a grid arranged version later on.
  plots[[num_of_plot]] <- p1
  num_of_plot <- num_of_plot + 1
}

#arranged output of all plots
grid.arrange(grobs=plots[1:16], ncol=4)

#creating a p-value of all significant features
results <- data.frame(c("rows_with_1", "cols_with_1", "rows_with_2", "rows_with_3p", "maxrow", "maxcol", "eyes", "hollowness", "custom"), p_vals)

kable(results, col.names = c("feature", "p-value"), caption="A table describing the p-value in each of the above graphs for each significant feature:")




########PLOTS OF ALL SIGNIFICANT#########



##### ROWS WITH 1 ######
#Seperate the living things and place them in a list
l1 <- rows_with_1[1:28]
l2 <- rows_with_1[57:70]
l3 <- rows_with_1[85:98]
living_things <- c(l1, l2, l3)

# Seperate the non-living things and place them in a list
nl1 <- rows_with_1[29:56]
nl2 <- rows_with_1[71:84]
nl3 <- rows_with_1[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group

plot1 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="darkblue", fill="blue")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of rows_with_1 for H0"))+
  theme(legend.position = "none") # no legend


##### COLS WITH 1 ######
#Seperate the living things and place them in a list
l1 <- cols_with_1[1:28]
l2 <- cols_with_1[57:70]
l3 <- cols_with_1[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- cols_with_1[29:56]
nl2 <- cols_with_1[71:84]
nl3 <- cols_with_1[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group
plot2 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="darkgreen", fill="green")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of cols_with_1 for H0"))+
  theme(legend.position = "none") # no legend


##### ROWS WITH 2 ######

#Seperate the living things and place them in a list
l1 <- rows_with_2[1:28]
l2 <- rows_with_2[57:70]
l3 <- rows_with_2[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- rows_with_2[29:56]
nl2 <- rows_with_2[71:84]
nl3 <- rows_with_2[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group
plot3 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="orange", fill="yellow")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of rows_with_2 for H0"))+
  theme(legend.position = "none") # no legend


##### ROWS WITH 3P ######

#Seperate the living things and place them in a list
l1 <- rows_with_3p[1:28]
l2 <- rows_with_3p[57:70]
l3 <- rows_with_3p[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- rows_with_3p[29:56]
nl2 <- rows_with_3p[71:84]
nl3 <- rows_with_3p[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group
plot4 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="purple", fill="purple")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of rows_with_3p for H0"))+
  theme(legend.position = "none") # no legend


grid.arrange(plot1, plot2, plot3, plot4, nrow=2)






##### EYES ######

#Seperate the living things and place them in a list
l1 <- eyes[1:28]
l2 <- eyes[57:70]
l3 <- eyes[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- eyes[29:56]
nl2 <- eyes[71:84]
nl3 <- eyes[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group

plot1 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="darkblue", fill="blue")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of eyes for H0"))+
  theme(legend.position = "none") # no legend


##### HOLLOWNESS ######

#Seperate the living things and place them in a list
l1 <- hollowness[1:28]
l2 <- hollowness[57:70]
l3 <- hollowness[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- hollowness[29:56]
nl2 <- hollowness[71:84]
nl3 <- hollowness[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group
plot2 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="darkgreen", fill="green")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of hollowness for H0"))+
  theme(legend.position = "none") # no legend



##### CUSTOM ######

#Seperate the living things and place them in a list
l1 <- custom[1:28]
l2 <- custom[57:70]
l3 <- custom[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- custom[29:56]
nl2 <- custom[71:84]
nl3 <- custom[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group

#get the current variable name we are analysing
plot3 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="orange", fill="yellow")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of white-pix per eye for H0"))+
  theme(legend.position = "none") # no legend

grid.arrange(plot1, plot2, plot3, nrow=1)







##### MAXROW ######

#Seperate the living things and place them in a list
l1 <- maxrow[1:28]
l2 <- maxrow[57:70]
l3 <- maxrow[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- maxrow[29:56]
nl2 <- maxrow[71:84]
nl3 <- maxrow[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group

plot1 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="darkblue", fill="blue")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of maxrow for H0"))+
  theme(legend.position = "none") # no legend



##### MAXCOL ######

#Seperate the living things and place them in a list
l1 <- maxcol[1:28]
l2 <- maxcol[57:70]
l3 <- maxcol[85:98]
living_things <- c(l1, l2, l3)

#Seperate the nonliving things and place them in a list
nl1 <- maxcol[29:56]
nl2 <- maxcol[71:84]
nl3 <- maxcol[99:112]
non_living_things <- c(nl1, nl2, nl3)

# Observed difference in means:
obsdiff = mean(living_things)-mean(non_living_things)

nsims = 10000

# Randomization test for difference in means
############################################

All <- c(living_things,non_living_things) # put all values in a single vector

nulldiffs <- rep(0,nsims) 
# nulldiffs will contain all our simulated differences in means
# (assuming the null hypothesis is true). 
# Initialise with 0s.

for (ni in 1:nsims) {
  # randomly reorder `All`
  randomA = sample(All) 
  # create the random group partition
  randomLiving = randomA[1:length(living_things)]
  randomNonLiving = randomA[(length(living_things)+1):length(All)]
  # calculate the differences in means for the two groups
  # and add it to our result vector
  nulldiffs[ni] <- mean(randomLiving) - mean(randomNonLiving)
}

# What proportion of the differences in means calculated with randomization
# are greater in magnitude than than the observed, true difference in means?
pvalue = sum(abs(nulldiffs)>abs(obsdiff))/nsims

# data frame for plotting:
df <- as.data.frame(nulldiffs)
df$group <- "randomization"
colnames(df) <- c("test.statistic", "group")
df$group <- as.factor(df$group)

# Generate plots of the data, by group
plot2 <- ggplot(df, aes(x=test.statistic, color=group, fill=group)) +
  geom_density(alpha=0.5, colour="darkgreen", fill="green")+
  geom_vline(xintercept = obsdiff, linetype="dotted", 
             color = "red", linewidth=1)+
  xlab(paste("Sampling distribution of maxcol for H0"))+
  theme(legend.position = "none") # no legend

grid.arrange(plot1, plot2, nrow=1)



#3.5
require("GGally")
five_features <- list(nr_pix, height, width, aspect_ratio, hollowness)

names(five_features) <- c("nr_pix", "height", "width", "aspect_ratio", "hollowness")

five_features_df <- data.frame(nr_pix, height, width, aspect_ratio, hollowness)

feature1 <- c()
feature2 <- c()
correlation <- c()
p_value <- c()

plots <- list()
index <- 1

for(i in 1:length(five_features)-1){
  
  for(j in (i+1):(length(five_features))){
    
    if(j==1){
      break
    } 
    
    nameX <- names(five_features[i])
    nameY <- names(five_features[j])
    
    feature1 <- append(feature1, nameX)
    feature2 <- append(feature2, nameY)
    
    cor_test<- cor.test(five_features_df[[nameX]], five_features_df[[nameY]])
    correlation <- append(correlation, cor_test$estimate)
    p_value <- append(p_value, cor_test$p.value)
    
    p1 <- ggplot(data = five_features_df, aes(x=.data[[nameX]], y=.data[[nameY]])) +
      geom_point(aes(x=.data[[nameX]], y=.data[[nameY]]), size = 2, shape=23, colour="darkred", fill="red")
    
    plots[[index]] <- p1
    index <- index + 1
    
  }
  
}
ggpairs(five_features_df, title="Correlogram of all five features correlations") 

grid.arrange(grobs=plots, ncol=2)

results <- data.frame(feature1, feature2, correlation, p_value)

kable(results, caption="A table describing the correlation between each pair of features:")


######### PLOTS OF SIGNIFICANT CORRELATIONS ########

p1 <- ggplot(data = five_features_df, aes(x=.data[["nr_pix"]], y=.data[["width"]])) + geom_point(aes(x=.data[["nr_pix"]], y=.data[["width"]]), size = 2, shape=23, colour="darkred", fill="red") + geom_smooth(method=lm, colour="blue")

p2 <- ggplot(data = five_features_df, aes(x=.data[["aspect_ratio"]], y=.data[["height"]])) + geom_point(aes(x=.data[["aspect_ratio"]], y=.data[["height"]]), size = 2, shape=23, colour="darkgreen", fill="green") + geom_smooth(method=lm, colour="blue")

grid.arrange(p1, p2, nrow=1)


