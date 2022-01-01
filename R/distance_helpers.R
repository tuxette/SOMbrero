# Helpers for distance computation ############################################

## distance between rows of two matrices

matrix.dist <- function(mat1, mat2) {
  outer(1:nrow(mat1), 1:nrow(mat2), Vectorize(function(r1, r2) {
    dist(rbind(mat1[r1, ], mat2[r2, ]))
  }))
}
