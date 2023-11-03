    library("testthat")
    library("dplyr")
    library("palmerpenguins")
    library("usethis")

# Function

    #' @title group_and_count
    #' @details 
    #' Group the input data with specified column and count each group's occurrence
    #' @param data Input data frame
    #' @param ... Multiple value, the group column
    #' @param count_c The count number column name, with default value cnt.
    #'
    #' @return A tibble with the count result
    #' @md
    group_and_count <- function(data, ..., count_c = "cnt") {
      group_columns <- enquos(...)
      result <- data %>%
        group_by(!!!group_columns) %>%
        summarize(!!count_c := n())
      return(result)
    }

# Example

    test1 <- group_and_count(penguins, island)
    print(test1)

    ## # A tibble: 3 × 2
    ##   island      cnt
    ##   <fct>     <int>
    ## 1 Biscoe      168
    ## 2 Dream       124
    ## 3 Torgersen    52

    test2 <- group_and_count(penguins, island, sex)

    ## `summarise()` has grouped output by 'island'. You can override using the
    ## `.groups` argument.

    print(test2)

    ## # A tibble: 9 × 3
    ## # Groups:   island [3]
    ##   island    sex      cnt
    ##   <fct>     <fct>  <int>
    ## 1 Biscoe    female    80
    ## 2 Biscoe    male      83
    ## 3 Biscoe    <NA>       5
    ## 4 Dream     female    61
    ## 5 Dream     male      62
    ## 6 Dream     <NA>       1
    ## 7 Torgersen female    24
    ## 8 Torgersen male      23
    ## 9 Torgersen <NA>       5

# Test

    test_that("group and count correctly count data", {
      data_df <- data.frame(
        Category = c("a", "a", "b", "b", "a", "b"),
        Product = c("m", "n", "m", "m", "n", "n")
      )
      
      result <- group_and_count(data_df, Category)
      expect_equal(result$Category, c("a", "b"))
      expect_equal(result$cnt, c(3, 3))
    })

    ## Test passed 😀

    test_that("group and count correctly count data", {
      data_df <- data.frame(
        Category = c("a", "a", "b", "b", "a", NA),
        Product = c("m", "n", "m", "m", "n", "n")
      )
      
      result <- group_and_count(data_df, Category)
      expect_equal(result$Category, c("a", "b", NA))
      expect_equal(result$cnt, c(3, 2, 1))
    })

    ## Test passed 😸

    test_that("group and count correctly count data", {
      data_df <- data.frame(
        Category = c("a", "a", "b", "b", "a", "a"),
        Product = c(1, 2, 3, 1, 2, 3)
      )
      
      result <- group_and_count(data_df, Category, Product)
      print(result)
      expect_equal(result$Category, c("a", "a", "a", "b", "b"))
      expect_equal(result$Product, c(1, 2, 3, 1, 3))
      expect_equal(result$cnt, c(1, 2, 1, 1, 1))
    })

    ## # A tibble: 5 × 3
    ## # Groups:   Category [2]
    ##   Category Product   cnt
    ##   <chr>      <dbl> <int>
    ## 1 a              1     1
    ## 2 a              2     2
    ## 3 a              3     1
    ## 4 b              1     1
    ## 5 b              3     1
    ## Test passed 🌈
