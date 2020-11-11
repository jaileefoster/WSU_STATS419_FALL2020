addFactorColumns = function(x) # x is a dataframe
{
  x$height.as.foot.length = NA
  x$height.as.foot.length = x$height / x$foot.length
  
  x$height.as.hand.length = NA
  x$height.as.hand.length = x$height / x$hand.length
  
  x$height.as.head.height = NA
  x$height.as.head.height = x$height / x$head.height
}

prepareMeasureData = function(measure.df) # x is a dataframe
{
  # create a data frame that only holds measurements for adults (18+)
  measure.df = subset(measure.df, measure.df$age > 18)
  
  # create a data frame that only holds measurements we are interested in
  measure.df = na.omit(subset(measure.df, select = c(height, foot.length, hand.length, head.height)))
  
  # identify any extreme outliers and remove them from the dataset
  findOutliersUsingZscores(measure.df$height)
  findOutliersUsingZscores(measure.df$hand.length)
  findOutliersUsingZscores(measure.df$foot.length)
  findOutliersUsingZscores(measure.df$head.height)
  
  measure.clean = subset(measure.df, (measure.df$hand.length < 50 & measure.df$hand.length > 5) & (measure.df$foot.length > 5.315 & measure.df$foot.length < 14.9) & (measure.df$head.height < 12))
  
  # add columns that contain height as a factor of other variables
  measure.clean$height.as.foot.length = NA
  measure.clean$height.as.foot.length = measure.clean$height / measure.clean$foot.length
  
  measure.clean$height.as.hand.length = NA
  measure.clean$height.as.hand.length = measure.clean$height / measure.clean$hand.length
  
  measure.clean$height.as.head.height = NA
  measure.clean$height.as.head.height = measure.clean$height / measure.clean$head.height
  
  measure.clean;
}

averageDataQuality = function(x) # x is a dataframe
{
  quality = mean(x$quality);
  quality;
}

performTTestOnHeightAsFactorOfHandLength = function(x)
{
  t.test(measure.clean$height.as.hand.length, mu = 10, conf.level = 0.95)
}

performTTestOnHeightAsFactorOfFootLength = function(x)
{
  t.test(measure.clean$height.as.foot.length, mu = 7, conf.level = 0.95)
}

performTTestOnHeightAsFactorOfHeadHeight = function(x)
{
  t.test(measure.clean$height.as.head.height, mu = 8, conf.level = 0.95)
}