doSummary = function(x) # x is a vector of data
{
  length = length(x);
  na.count = sum(is.na(x));
  # na.rm=T does not include NAs in the calculation for mean and median
  # this allows for a numeric answer, rather than NA.
  mean = mean(x, na.rm=T)
  median = median(x, na.rm=T);
  mode = doMode(x)
  variance.naive = doSampleVariance(x, "naive");
  variance.2pass = doSampleVariance(x, "asdf");
  sd.builtin = sd(x, na.rm=T);
  sd.custom = sqrt(variance.naive);
  data.frame(length=length, na.count=na.count, mean=mean, median=mean, mode=mode, var.naive=variance.naive$var, var.2pass=variance.2pass$var, sd.builtin=sd.builtin, sd.custom=sd.custom$var);
}


doSampleVariance = function(x, method)
{
  x=x[!is.na(x)];
  if(method=="naive")
  {
    n=0;
    sum=0;
    sum.squared=0;
    for (i in 1:length(x))
    {
      n=n+1;
      sum=sum+x[i];
      sum.squared=sum.squared+(x[i]*x[i]);
    }
    naive.variance=(sum.squared-(sum^2)/n)/(n-1);
    data.frame(sum=sum, sum.squared=sum.squared, var=naive.variance);
  }
  else
  {
    sum1=0;
    sum2=0;
    n=0;
    for (i in 1:length(x))
    {
      n=n+1;
      sum1=sum1+x[i];
    }
    mean=sum1/n;
    for (i in length(x))
    {
      sum2=sum2+((x[i]-mean)^2);
    }
    variance=sum2/(n-1);
    data.frame(sum=sum1, sum2=sum2, var=variance);
  }
}


doMode = function(x)
{
  freq = table(x);
  freq.df = as.data.frame(freq);
  result = as.numeric(as.vector(freq.df[freq.df$Freq == max(freq.df$Freq),]$x))
  result;
}


zScores = function(x)
{
  z.scores = c();
  mean = sum(x)/length(x);
  sd = sd(x, na.rm=T);
  for (i in 1:length(x))
  {
    z = (x[i]-mean)/sd;
    z.scores[i] = z;
  }
  z.scores;
}

