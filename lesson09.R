#' ## Assignment Set 3: Lesson 9
#'
#' Add R Code under the questions.
#' If you are done, 'knit' the document, commit and push to GitHub.
#' You can use GitHub Pages if you like, add links to README.md
#' to make it easier to find the pretty version.
#'
#' Back to the `CASchools` dataset on test performance, school characteristics
#' and student demographic backgrounds for school districts in California. As
#' before, we will enhance the dataset by defining two new variables,
#' `student_teacher_ratio`, the student-teacher ratio, and `test_score`, an
#' average of two underlying test scores:
library(dplyr)
library(ggplot2)
library(AER)
data(CASchools)

caschools <-
  CASchools %>%
  as_tibble() %>%
  mutate(student_teacher_ratio = students / teachers) %>%
  mutate(test_score = (read + math) / 2) %>%
  select(-read, -math, -students, -teachers, -district, -school, -county, -grades)

caschools

#' 1. Separate the data set into a training and a test set. Make sure the
#' training set contains 75% of the available observations.
#'
set.seed(75)
caschools_train <- sample_n(caschools, 315)
caschools_test <- anti_join(caschools, caschools_train)

#' 2. Build a decision tree to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Use the defaults of `rpart`. Draw the
#' resulting decision tree. Store the model as `m_tree`.
#'
library(rpart)
m_tree <- rpart(student_teacher_ratio ~ ., data = caschools_train, method = "anova", maxdepth = 1)
m_tree
plot(m_tree)
text(m_tree)
summary(m_tree)

caschools_test %>%
  mutate(predict = predict(m_tree, newdata = caschools_test)) %>%
  ggplot(aes(x = expenditure, y = student_teacher_ratio)) +
  geom_point() +
  geom_point(mapping = aes(y = predict), shape = 21) +
  geom_segment(mapping = aes(xend = expenditure, yend = predict), alpha = 0.2) +
  geom_vline(aes(xintercept = 9.95))

#' 3. From the documentation, `?rpart`, can you figure out how the depth of a
#' tree is determined? Which one is the most important variable?
#'
?rpart

#' "maxdepth" is a special argument of rpart function and its documentation can be found under
#' "rpart.control" in ?rpart.
#'
#'  The most important variable in the decision tree model is "expenditure" (summary(m_tree))

#' 4. Estimate an OLS model to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Which one is the most important variable?
#' Store the model as `m_ols`.
#'
m_ols <- lm(student_teacher_ratio ~ ., data = caschools_train)
summary(m_ols)


#' 5. Grow a random forest to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Use the defaults of `randomForest`. Store the
#' model as `m_forest`.
#'
library(randomForest)
set.seed(01)

m_forest <- randomForest(student_teacher_ratio ~ ., data = caschools_train)
plot(m_forest)


#' 6. Plot the variable importance for `m_forest`. Which one is the most
#' imporant?
#'
varImpPlot(m_forest)


#' 7. Using the test data, can you compute RMSE measures for `m_ols`, `m_tree`,
#' and `m_forest`? Which performs best?
#'
rmse_tree <- sqrt(mean((caschools_test$student_teacher_ratio - predict(m_tree, newdata = caschools_test))^2))
rmse_ols <- sqrt(mean((caschools_test$student_teacher_ratio - predict(m_ols, newdata = caschools_test))^2))
rmse_forest <- sqrt(mean((caschools_test$student_teacher_ratio - predict(m_forest, newdata = caschools_test))^2))

#' The OLS model performs best since it has the smallest Root Mean Square Error (RMSE)


