#' Read object from disk or run the expression creating it
#' 
#' When you have a long-running operation, it can be helpful to save the result
#' so you don't have to run it again and again. E.g. a trained ML model.
#' This function checks if an object has been saved as an .RDS file.
#' If it has, load the RDS file.
#' If it has not, run the expr block of code. The function returns the
#' result of the expression, as well as saving a .RDS file to the provided
#' filename.
#'
#' @param filename string, where is the object to be checked stored? If it doesn't
#' exist, a .RDS file will be created at this location.
#' @param expr expression. Code that will create the object, MUST end with returning
#' the object. Saving the object is done automatically, shouldn't be included in expr.
#' @param force_recreate bool. Skip checking if the object is saved to disk and
#' recreate it from stratch?
#'
#' @return object stored in RDS file/created in expr
expr_saved_on_disk <- function(filename,
                           expr,
                           force_recreate = F) {
  
  file_saved <- file.exists(filename)
  
  if (file_saved & !force_recreate) {
    message(
      paste0(filename, " exists, reading from disk")
    )
  # read the object from disk
      return(
      readRDS(filename)
    )
  }
  
  # Evaluate expression and create an object from its output
  final_object <- force(expr)
  
  # Save with the provided filename
  saveRDS(final_object, filename)
  # Return the object itself to be used outside the function
  final_object
}


# EXAMPLES ------

my_obj <- expr_saved_on_disk(
  filename = "saved_objects/test3.RDS",
  expr = {
    head(mtcars, 10)
  },
  force_recreate = F
)


my_random_nums <- rnorm(1000)

rand_obj <- expr_saved_on_disk(
  filename = "saved_objects/test_rnorm.RDS",
  expr = {
    rand_obj <- data.frame(col1 = my_random_nums,
                     col2 = my_random_nums + 10)
    
    # if the object has a different name than the we assign the result of
    # `expr_save_on_disk` from, we'll end up with both of those objects
    # in memory
    rand_obj
  }
)
