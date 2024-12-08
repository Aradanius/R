generateMatrix <- function(x = matrix()) {
  matrix_data <- list(
    matrix = x,
    inverse = NULL
  )
  return(matrix_data)
}

cacheInverseMatrix <- function(x, ...) {
  if (!is.null(x$inverse)) {
    message("Получение обратной матрицы из кэша.")
    return(x$inverse)
  }
  x$inverse <- solve(x$matrix)
  message("Обратная матрица вычислена и кэширована.")
  return(x$inverse)
}

matr <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
matr_obr <- generateMatrix(matr)

inv_mat <- cacheInverseMatrix(matr_obr)
print(inv_mat)

matr_obr <- generateMatrix(matr_obr$matrix) 
matr_obr$inverse <- cacheInverseMatrix(matr_obr) 
print(matr_obr$inverse)

matr_obr <- generateMatrix(matr_obr$matrix) 
matr_obr$inverse <- cacheInverseMatrix(matr_obr) 
print(matr_obr$inverse)


matr_obr$matrix <- matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2)
inv_mat3 <- cacheInverseMatrix(matr_obr)
print(inv_mat3)
