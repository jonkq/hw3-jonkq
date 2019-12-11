#' ## Assignment Set 3: Lesson 8
#'
#' Add R Code under the questions.
#' If you are done, 'knit' the document, commit and push to GitHub.
#' You can use GitHub Pages if you like, add links to README.md
#' to make it easier to find the pretty version.
#'
#'
#' 1. Joins: Flattening
#'
#' From all 5 datasets in the {nycflights13} package,
#' create one wide table that contains the data from the `flights` table
#' with the following details:
#'
#' - Name of the airline
#' - Name, latitute and longitude of the origin and destination airports
#'   (if available)
#' - Manufacturer, model and seats for the airplane used (if available)
#' - Temperature and humidity at the airport of origin during departure time
#'
#' The data you create must contain one row for each observation in `flights`.
#' Ensure that the columns are named in a way that allows to uniquely define
#' the purpose.
#' Hint: Implement this as a sequence of `left_join()` operations.
#' For `weather` use the `time_hour` column.
#'
#'
#' 2. Joins: Filtering
#'
#' Reduce the result of the first exercise from this lesson
#' to exclude all flights
#' where no detailed information on the destination airport is available.
#' Show a solution that uses `filter()`,
#' and one that uses a different kind of join.
#' Explain.
#'
#' If you're not sure about your results for the first exercise,
#' use the following as a starting point:
#'
#' flights %>%
#'   left_join(airports, by = c("faa" = "dest"))
#'
#'
#' 3. Joins: Application
#'
#' Show the number of flights per carrier and origin airport as a heat map.
#'
#' flights %>%
#'   ...(...) %>%
#'   ...(...) %>%
#'   ggplot(aes(...)) +
#'   geom_tile(stat = "bin2d")
#'
#' Instead of `stat = "bin2d"` you can also use a `count()` after `flights`.
#'
#'
#' Resources:
#'
#' - Chapter 13 of r4ds, https://r4ds.had.co.nz/relational-data.html
