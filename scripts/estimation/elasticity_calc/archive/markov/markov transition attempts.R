matrix <- matrix(c(0.75,0.001, 0.25, .999), ncol = 2)
sum(matrix)

matrix_steady <- matrix %*% matrix
(matrix_steady <- matrix_steady %*% matrix)
sum(matrix_steady)



matrix <- matrix(c(0.71652222,0.00000001, 0.28347775, .99999999), ncol = 2)
sum(matrix)

matrix_steady <- matrix %*% matrix
(matrix_steady <- matrix_steady %*% matrix)
sum(matrix_steady)
