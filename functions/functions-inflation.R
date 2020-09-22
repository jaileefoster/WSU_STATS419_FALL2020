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

inflation = function()
{
  inflation.table = grabInflationData();
  inflation.decimal=c(0.1561)
  
  for (i in 2:101)
  {
    inflation.decimal[i]=inflation.table$dollar[i]/inflation.table$dollar[i-1] - 1
  }
  
  inflation.table$inflation.decimal = inflation.decimal
  
  dollars.2000=c()
  dollars.2000[81] = 1000000
  
  for (i in 82:101)
  {
    dollars.2000[i] = dollars.2000[i-1]*(1+inflation.table$inflation.decimal[i])
  }
  
  for (i in 1:80)
  {
    dollars.2000[81-i] = dollars.2000[82-i]/(1+inflation.table$inflation.decimal[82-i])
  }
  
  inflation.table$dollars.2000 = dollars.2000;
  inflation.table[is.na(inflation.table)] = 0
  inflation.table;
}