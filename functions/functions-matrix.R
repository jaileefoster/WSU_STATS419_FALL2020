# this transformation matrix reverses the columns of a matrix when the matrix 
# is multiplied by it
transformationMatrix = matrix(c(0, 0, 1,
                                0, 1, 0,
                                1, 0, 0), nrow=3, byrow=T)


# transposeMatrix is a function that simply transposes a matrix
transposeMatrix = function(mat)
{
  t(mat);
}


# to rotate a matrix 90 degrees, first take the transpose of the matrix, 
# then multiply it by the transformation matrix specified above
rotateMatrix90 = function(mat)
{
  matrix = t(mat) %*% transformationMatrix;
  matrix;
}


# to rotate a matrix 180 degrees, apply the rotateMatrix90() function
# written above to the matrix twice
rotateMatrix180 = function(mat)
{
  matrix = rotateMatrix90(rotateMatrix90(mat));
  matrix;
}


# to rotate a matrix 270 degrees, first multiply it by the transformation matrix 
# specified above, then take the transpose of the resulting matrix
rotateMatrix270 = function(mat)
{
  matrix = t(mat %*% transformationMatrix);
  matrix;
}