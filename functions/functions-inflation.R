# the code in this function was provided by Dr. Shaffer to grab inflation data off of the internet
grabInflationData = function()
{
  infl = "https://www.officialdata.org/us/inflation/2000?endYear=1920&amount=1000000";
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
}


# The following function calculates the inflation rate as a decimal that goes out further than the inflation 
# rate that was provided in the inflation table to allow for more accurate calculations. It then calculates
# each value with regard to what $1,000,000 was in 2000 to account for inflation.
inflation = function()
{
  inflation.table = grabInflationData();
  inflation.decimal=c(0.1561)
  for (i in 2:101)
  {
    inflation.decimal[i]=inflation.table$dollar[i]/inflation.table$dollar[i-1] - 1
  }
  # The new column of inflation decimal numbers is added to the inflation table as the column “inflation.decimal”
  inflation.table$inflation.decimal = inflation.decimal
  # The following restructures the inflation table so that there is a column with respect to $1,000,000 in the year 2000.
  dollars.2000=c()
  dollars.2000[81] = 1000000
  # if the year falls after 2000
  for (i in 82:101)
  {
    dollars.2000[i] = dollars.2000[i-1]*(1+inflation.table$inflation.decimal[i])
  }
  # if the year falls before 2000
  for (i in 1:80)
  {
    dollars.2000[81-i] = dollars.2000[82-i]/(1+inflation.table$inflation.decimal[82-i])
  }
  # The new column of values in relation to the value of $1,000,000 in 2000 is added to the inflation table as the column “dollars.2000”.
  inflation.table$dollars.2000 = dollars.2000;
  inflation.table[is.na(inflation.table)] = 0
  inflation.table;
}