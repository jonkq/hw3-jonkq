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

#' 2. Build a decision tree to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Use the defaults of `rpart`. Draw the
#' resulting decision tree. Store the model as `m_tree`.
#'

#' 3. From the documentation, `?rpart`, can you figure out how the depth of a
#' tree is determined? Which one is the most important variable?
#'

#' 4. Estimate an OLS model to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Which one is the most important variable?
#' Store the model as `m_ols`.
#'

#' 5. Grow a random forest to predict `student_teacher_ratio`, using all
#' variables in `caschools_train`. Use the defaults of `randomForest`. Store the
#' model as `m_forest`.
#'

#' 6. Plot the variable importance for `m_forest`. Which one is the most
#' imporant?
#'

#' 7. Using the test data, can you compute RMSE measures for `m_ols`, `m_tree`,
#' and `m_forest`? Which performs best?
#'

