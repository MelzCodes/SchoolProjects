require("rstudioapi")
require("stringr")
require("raster")
require("rmarkdown")
require("ggplot2")
require("gridExtra")
require("MASS")

#set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("section3_code.r")


#4.1
#backwards selection
lmfitted = lm(aspect_ratio ~ nr_pix + rows_with_1 + cols_with_1 + rows_with_2 +cols_with_2 +
                rows_with_3p + cols_with_3p + height + width + maxrow + maxcol +
                connected_areas + eyes + hollowness + custom, data=csv_df)

backwards_selection <- step(lmfitted, direction = "backward", trace = 0)
print(formula(backwards_selection))

#forward selection
fitstart = lm(aspect_ratio ~ 1, data=csv_df)

forwards_selection <- step(fitstart, direction = "forward", scope = formula(lmfitted), trace = 0)

print(formula(forwards_selection))

summary(lm(backwards_selection$call$formula, data = csv_df))
summary(lm(forwards_selection$call$formula, data = csv_df))

new_aspects <- data.frame(aspect = vector( "integer" , 20 ))

predicted <- predict(forwards_selection, newdata = new_aspects)[0:20]

actual <- csv_df$aspect_ratio[0:20]

results <- data.frame(actual, predicted)

kable(results, caption="A table showing the actual vs predicted aspect ratios:")


#4.2


# Make a livingThing variable that discriminates livingThings from the nonLivingThings: 
csv_df$livingThing <- 0
csv_df$livingThing[csv_df$labels %in% c('lemon', 'cherry', 'tree', 'banana')] <- 1  

set.seed(2062)


# make training and test datasets
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# randomly shuffle rows:
csv_shuffled <- csv_df[sample(nrow(csv_df)),]

# first 80% will be training data:
training_data = csv_shuffled[1:90,]
test_data = csv_shuffled[91:112,]


# Logistic regression with our custom feature of curvature
# ========================================================

glmfit<-glm(livingThing ~ custom, 
            data = training_data, 
            family = 'binomial') 

x.range = range(training_data[["custom"]])

x.values = seq(x.range[1],x.range[2],length.out=1000)

fitted.curve <- data.frame(custom = x.values)
fitted.curve[["livingThing"]] = predict(glmfit, fitted.curve, type="response")

# Plot the training data and the fitted curve:
plt <-ggplot(training_data, aes(x=custom, y=livingThing)) + 
  geom_point(aes(colour = factor(livingThing)), 
             show.legend = T)+
  geom_line(data=fitted.curve, colour="orange", linewidth=1)

plt


# Assuming a p>0.5 cut-off, calculate accuracy on the training data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

training_data[["predicted_val"]] = predict(glmfit, training_data, type="response")
training_data[["predicted_class"]] = 0
training_data[["predicted_class"]][training_data[["predicted_val"]] > 0.5] = 1



correct_items = training_data[["predicted_class"]] == training_data[["livingThing"]] 

# proportion correct:
nrow(training_data[correct_items,])/nrow(training_data)

# proportion incorrect:
nrow(training_data[!correct_items,])/nrow(training_data)



# Assuming a p>0.5 cut-off, calculate accuracy on the test data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

test_data[["predicted_val"]] = predict(glmfit, test_data, type="response")
test_data[["predicted_class"]] = 0
test_data[["predicted_class"]][test_data[["predicted_val"]] > 0.5] = 1

correct_items = test_data[["predicted_class"]] == test_data[["livingThing"]] 

# proportion correct:
nrow(test_data[correct_items,])/nrow(test_data)

# proportion incorrect:
nrow(test_data[!correct_items,])/nrow(test_data)



