### Question 1

transformationMatrix = matrix(c(0, 0, 1,
                                0, 1, 0,
                                1, 0, 0), nrow=3, byrow=T)

myMatrix = matrix(c(1, 0, 2, 0, 3, 0, 4, 0, 5), nrow=3, byrow=T)

transposeMatrix = function(mat)
{
  t(mat);
}

rotateMatrix90 = function(mat)
{
  matrix = t(mat) %*% transformationMatrix;
  matrix;
}

rotateMatrix90(myMatrix);

rotateMatrix180 = function(mat)
{
  matrix = rotateMatrix90(rotateMatrix90(mat));
  matrix;
}

rotateMatrix180(myMatrix)

rotateMatrix270 = function(mat)
{
  matrix = t(mat %*% transformationMatrix);
  matrix;
}

rotateMatrix270(myMatrix)

### Question 2

data(iris)

pairs(iris[, 1:4], main="Iris Data (red=setosa,green=versicolor,blue=virginica)",
      bg=c("red", "springgreen3", "blue")[iris$Species], col="black", pch=21, 
      cex.labels = 1.5, cex.axis=1.5, cex.main=1)

## Question 4

personality.data = read.csv("personality-raw.txt", header=T, sep="|")
personality.data = subset(personality.data, select=-c(V00))

# https://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date
personality.data=personality.data[rev(order(as.Date(personality.data$date_test, format='%m/%d/%Y %H:%M'))),]

# this code 
date = strptime(personality.data$date_test, format='%m/%d/%Y %H:%M');
year = as.numeric(strftime(date, format="%Y"));
week = as.numeric(strftime(date, format="%W"));

personality.data$year = year;
personality.data$week = week;

personality.data = subset(personality.data, select=-c(date_test))

# https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
unique.by.email = unique(personality.data["md5_email"])
personality.data.clean = personality.data[!duplicated(personality.data["md5_email"]),]

dim(personality.data)
dim(unique.by.email)
dim(personality.data.clean)

# https://www.rdocumentation.org/packages/utils/versions/3.6.2/topics/write.table
write.table(personality.data.clean, "personality-clean.txt", sep="|")

## Question 5

doSummary = function(x) # x is a vector of data
{
  length = length(x);
  na.count = sum(is.na(x));
  # na.rm=T does not include NAs in the calculation for mean and median
  # this allows for a numeric answer, rather than NA.
  mean = mean(x, na.rm=T)
  median = median(x, na.rm=T);
  mode = doMode(x)
  variance = sampleVariance(x, "naive");
  sd.builtin = sd(x, na.rm=T);
  sd.custom = sqrt(variance);
  data.frame(length=length, na.count=na.count, mean=mean, median=mean, mode=mode, var=variance, sd.builtin=sd.builtin, sd.custom=sd.custom);
}

sampleVariance = function(x, method)
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

################################# 

personality.vec = as.vector(personality.data.clean[1,])
personality.vec = as.numeric(subset(personality.vec, select=-c(md5_email, year, week)))

doSummary(personality.vec)
sampleVariance(personality.vec, "naive")
sampleVariance(personality.vec, "na")
doMode(personality.vec)


z.scores = scale(personality.vec, center=T, scale=T)
z.score.vec = as.vector(z.scores)
plot(personality.vec, z.score.vec)

## Question 6

## functions

education = function(one)
{
  result = list();
  result$who 		= one;
  result$think 	= c("intensitively", "critically");
  result$goal 	= "intelligences + character";
  result;	
}


me = education("monte");

# n > 1 ... 

# Research question:  who is a better actor?  Will Smith?  Denzel Washington?

##
myMatrix = matrix ( c (
  1, 0, 2,
  0, 3, 0,
  4, 0, 5
), nrow=3, byrow=T);

transposeMatrix = function(mat)
{
  t(mat);	
}

#rotateMatrix90(mat)	
#rotateMatrix180(mat)	
#rotateMatrix270(mat)
# 3x3 matrix ... ## matrix multiplication

# install.packages("stringr", dependencies=T);
library(stringr);	
# install.packages("rvest", dependencies=T);
library(rvest);	

# Research question:  who is a better actor?  Will Smith?  Denzel Washington?

## actor ... person_id
##			movie_id ... details
##			name ... count movies

# https://rvest.tidyverse.org/index.html

## Denzel Washington [nm0000243] vs Will Smith [nm0000226]

## https://www.imdb.com/filmosearch/?explore=title_type&role=nm0000243&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie

# R , javascript, php, (c/c++)

# imdb ... 


grabFilmInfoFromFilmsPage = function(page)
{
  # 50 elements
  # # title = id = rank = year = rating = minutes = genre = votes = metascore = desc = millions
  
  movies = page %>%
    html_nodes(".mode-detail");
  
  
  
  pagecount = length(movies);
  
  results = data.frame( 			matrix(ncol = 11,nrow = pagecount) );
  # a matrix-type form with lots of NA values ...
  
  colnames(results) = c("rank", "title", "ttid", "year", "rated", "minutes", "genre", "ratings", "metacritic", "votes", "millions"); 
  
  
  for(i in 1:pagecount)
  {
    movie = movies[i];
    
    rank = movie %>%
      html_node(".lister-item-index") %>%
      html_text() %>%
      as.numeric();
    results$rank[i] = rank;
    
    title = movie %>%
      html_node(".lister-item-header a") %>%
      html_text();
    results$title[i] = title;
    
    ttid = movie %>%
      html_node(".lister-item-header a") %>%
      html_attr("href");
    
    temp = strsplit(ttid,"/",fixed=T);
    ttid = temp[[1]][3];
    results$ttid[i] = ttid;
    
    year = movie %>%
      html_node(".lister-item-year") %>%
      html_text();
    year = cleanupYear(year);
    results$year[i] = year;
    
    rated = movie %>%
      html_node(".certificate") %>%
      html_text();
    results$rated[i] = rated;
    
    minutes = movie %>%
      html_node(".runtime") %>%
      html_text();
    minutes = cleanupMinutes(minutes);
    results$minutes[i] = minutes;		
    
    genre = movie %>%
      html_node(".genre") %>%
      html_text();
    genre = str_trim(genre);
    results$genre[i] = genre;
    
    ratings = movie %>%
      html_node("div .rating-list") %>%
      html_attr("title");
    temp = strsplit(ratings,"/",fixed=T);
    temp = gsub("Users rated this","",temp[[1]][1],fixed=T);	
    temp = str_trim(temp);
    ratings = as.numeric(temp);
    results$ratings[i] = ratings;
    
    metacritic = movie %>%
      html_node(".ratings-metascore span") %>%
      html_text();
    metacritic = as.numeric(str_trim(metacritic));
    results$metacritic[i] = metacritic;
    
    # para ... +5 EASTER EGG ...
    
    info = movie %>%
      html_nodes(".lister-item-content p span") %>%
      html_text();
    
    votes = as.numeric(gsub(",","",info[8],fixed=T));
    results$votes[i] = votes;
    
    millions = cleanupMillions(info[11]);
    results$millions[i] = millions;			
  }
  
  #str(results);
  
  results;
}







cleanupMillions = function(millions)
{
  millions = gsub('$','',millions, fixed=T);
  millions = gsub('M','',millions, fixed=T);
  
  millions = as.numeric(millions);
  millions;
}

cleanupMinutes = function(minutes)
{
  minutes = gsub('min','',minutes, fixed=T);
  
  minutes = as.numeric(minutes);
  minutes;
}

cleanupYear = function(year)
{
  year = gsub('(','',year, fixed=T);
  year = gsub(')','',year, fixed=T);
  year = gsub('I','',year, fixed=T);
  year = as.numeric(year);
  year;
}

grabNameFromFilmsPage = function(page)
{
  name = page %>%
    html_node(".header") %>%
    html_text();
  
  name = gsub("Most Rated Feature Films With","",name,fixed=T);
  name = str_trim(name);
  
  name;
}


grabFilmCountFromFilmsPage = function(page)
{
  totalcount = page %>%
    html_nodes(".desc") %>%
    html_text();
  
  temp = strsplit(totalcount,"of",fixed=T);
  temp2 = strsplit(temp[[1]][2],"titles", fixed=T);
  
  totalcount = str_trim(temp2[[1]][1]);
  totalcount = as.numeric(totalcount);
  
  temp2 = strsplit(temp[[1]][1],"to", fixed=T);
  
  pagecount = str_trim(temp2[[1]][2]);
  pagecount = as.numeric(pagecount);
  
  results = list();
  
  results$totalcount = totalcount;
  results$pagecount = pagecount;
  
  results;
}


#   nmid = "nm0000226";
# 	will = grabFilmsForPerson(nmid);
# 	plot(will$movies.50[,c(1,6,7:10)]);
#  	boxplot(will$movies.50$millions);

#   nmid = "nm0000243";
# 	denzel = grabFilmsForPerson(nmid);
# 	plot(denzel$movies.50[,c(1,6,7:10)]);
#  	boxplot(denzel$movies.50$millions);


# https://www.imdb.com/title/tt0466839/?ref_=filmo_li_tt ... get box office budget/gross if NA ... on millions. ..

grabFilmsForPerson = function(nmid)
{
  results = list();
  
  url = paste("https://www.imdb.com/filmosearch/?explore=title_type&role=",nmid,"&ref_=filmo_ref_typ&sort=num_votes,desc&mode=detail&page=1&title_type=movie", sep="");
  
  page1 = read_html(url);
  
  ## useful for other data purposes
  results$nmid = nmid;
  
  ## name of person
  results$name = grabNameFromFilmsPage(page1);
  results$countfilms = grabFilmCountFromFilmsPage(page1);
  
  results$movies.50 = grabFilmInfoFromFilmsPage(page1);
  
  
  
  
  ##  parallel format ...
  # ranks = page1 %>%
  # html_nodes(".lister-item-index") %>%
  # html_text() %>%
  # as.numeric();	
  
  # ranks;
  
  # years = page1 %>%
  # html_nodes(".lister-item-year") %>%
  # html_text();
  
  # years = gsub('(','',years, fixed=T);
  # years = gsub(')','',years, fixed=T);
  # years = gsub('I','',years, fixed=T);
  # years = as.numeric(years);
  
  # titles = page1 %>%	
  # html_nodes(".lister-item-header a") %>%
  # html_text();
  
  # titles;
  
  
  results;
}

###### 

nmid = "nm0000226";
will = grabFilmsForPerson(nmid);
plot(will$movies.50[,c(1,6,7:10)]);
boxplot(will$movies.50$millions);
widx =  which.max(will$movies.50$millions);
will$movies.50[widx,];
summary(will$movies.50$year);  # bad boys for life ... did data change?

nmid = "nm0000243";
denzel = grabFilmsForPerson(nmid);
plot(denzel$movies.50[,c(1,6,7:10)]);
boxplot(denzel$movies.50$millions);
didx =  which.max(denzel$movies.50$millions);
denzel$movies.50[didx,];
summary(denzel$movies.50$year);

par(mfrow=c(1,2));
boxplot(will$movies.50$millions, main=will$name, ylim=c(0,360), ylab="Raw Millions" );
boxplot(denzel$movies.50$millions, main=denzel$name, ylim=c(0,360), ylab="Raw Millions" );

par(mfrow=c(1,1));

######### inflation

# https://www.in2013dollars.com/us/inflation/2000?endYear=1982&amount=100
# create variable $millions.2000 to convert all money to 2000 dollars ... based on year

# data from 1920 to 2020 ... 101 years ...
infl = "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000";
# bigger dollar gives a more accurate percent ...

# read the values in "year"/"dollars" using rvest ...
library(rvest);	

infl.html = read_html(infl);

infl.table = infl.html %>%
  html_node(".expand-table-parent") %>%
  html_node(".table-striped") %>%
  html_node("tbody") %>%
  html_nodes("tr");

result = data.frame( matrix(nrow=length(infl.table), ncol=3));
colnames(result) = c("year","dollar","inflation");

for(i in 1:length(infl.table) )
{
  infl.row = infl.table[i]	%>% 
    html_nodes("td") %>%
    html_text();
  
  year = as.numeric(infl.row[1]);
  temp = gsub('$','',infl.row[2],fixed=T);
  temp = gsub(',','',temp,fixed=T);
  dollar = as.numeric(temp);
  temp = gsub('%','',infl.row[3],fixed=T);
  inflation = as.numeric(temp);	
  
  result$year[i] = year;
  result$dollar[i] = dollar;
  result$inflation[i] = inflation;
  
}
result;	
# setwd("mypath"); # set where you want to store the file ...
# write.table(result,file="inflation.txt",sep="|",row.names=F);

# this data starts with 1920 at $1,000,000 dollars
# how can you restructure it so it is in $1,000,000 dollars in the year 2000... I would suggest using the dollars to build the inflation rate, as it will be more accurate ... 
# e.g., around 1973 ... 
# 2220000/2090000 - 1;  #  0.06220096  # vs ... 6.22%
#  options(digits=22);  # the computer is storing even more accuracy under the hood ...


# then you can use it to answer your questions for will vs denzel.








inflation.decimal=c(0.1561)

for (i in 2:101)
{
  inflation.decimal[i]=result$dollar[i]/result$dollar[i-1] - 1
}

result$inflation.decimal = inflation.decimal

dollars.2000=c()
dollars.2000[81] = 1000000

for (i in 82:101)
{
  dollars.2000[i] = dollars.2000[i-1]*(1+result$inflation.decimal[i])
}

for (i in 1:80)
{
  dollars.2000[81-i] = dollars.2000[82-i]/(1+result$inflation.decimal[82-i])
}

result$dollars.2000 = dollars.2000


###########################

will.df = as.data.frame(will)
denzel.df = as.data.frame(denzel)

will.millions.2000 = c()

for (i in 1:50)
{
  line = as.numeric(will.df$movies.50.year) - 1919
  will.millions.2000[i] = will.df$movies.50.millions[i] * (result$dollars.2000[line[i]])/1000000
}

will.df$millions.2000 = will.millions.2000
will$millions.2000=will.millions.2000

denzel.millions.2000 = c()

for (i in 1:50)
{
  line = as.numeric(denzel.df$movies.50.year) - 1919
  denzel.millions.2000[i] = denzel.df$movies.50.millions[i] * (result$dollars.2000[line[i]])/1000000
}

denzel.df$millions.2000 = denzel.millions.2000
denzel$millions.2000=denzel.millions.2000

############### 

###################
boxplot(will$millions.2000);
widx =  which.max(will$millions.2000);
will$movies.50[widx,];

boxplot(denzel$millions.2000);
didx =  which.max(denzel$millions.2000);
denzel$movies.50[didx,];

par(mfrow=c(1,2));
boxplot(will$millions.2000, main=will$name, ylim=c(0,360), ylab="Millions reflecting Inflation", main.cex=0.5);
boxplot(denzel$millions.2000, main=denzel$name, ylim=c(0,360), ylab="Millions reflecting Inflation", main.cex=0.5);

par(mfrow=c(1,1))

par(mfrow=c(1,2));
boxplot(will$movies.50$year, main=will$name, ylim=c(1980, 2030), ylab="Years of Movie Release");
boxplot(denzel$movies.50$year, main=denzel$name, ylim=c(1980, 2030), ylab="Years of Movie Release");

par(mfrow=c(1,1))

par(mfrow=c(1,2));
boxplot(will$movies.50$minutes, main=will$name, ylim=c(50, 225), ylab="Length of Movie (minutes)");
boxplot(denzel$movies.50$minutes, main=denzel$name, ylim=c(50, 225), ylab="Length of Movie (minutes)");

par(mfrow=c(1,1))