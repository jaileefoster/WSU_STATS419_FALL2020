transformationMatrix = matrix(c(0, 0, 1,
                                0, 1, 0,
                                1, 0, 0), nrow=3, byrow=T)

transposeMatrix = function(mat)
{
  t(mat);
}

rotateMatrix90 = function(mat)
{
  matrix = t(mat) %*% transformationMatrix;
  matrix;
}

rotateMatrix180 = function(mat)
{
  matrix = rotateMatrix90(rotateMatrix90(mat));
  matrix;
}

rotateMatrix270 = function(mat)
{
  matrix = t(mat %*% transformationMatrix);
  matrix;
}