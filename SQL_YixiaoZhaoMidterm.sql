use wormsley_lawncare;

# 1. In August 2012, the organization’s Total Expenses were the largest of any month. How many transactions
#    in August 2012 had expenses larger than the average for that month?

select count(Expenses) from Transactions
where YEAR(TransDate) = 2012 AND MONTH(TransDate) = 8 AND 
Expenses > (select Avg(Expenses)  from Transactions
where YEAR(TransDate) = 2012 AND MONTH(TransDate) = 8);

# 2. List the Sales Representatives (Reps) according to their Total Sales in the Western market over the entire
#    two‐year period.

select R.RepName,T.RepID, T.Market,sum(T.Sales) 'Total Sales' 
from Transactions T left join Reps R
ON T.RepID = R.RepID
where Market = 'West'
group by RepID, Market;

select R.RepName,sum(T.Sales) 'TotalSales' 
from Transactions T left join Reps R
ON T.RepID = R.RepID
where Market = 'West'
group by T.RepID;

# 3. List the Sales Representatives (Reps) according to who generated the least “Total Profit” (Sales minus
#    Expenses) over the entire two‐year period. (sort from lowest to highest)

select R.RepName,sum(T.Sales)-sum(T.Expenses) 'TotalProfit'
from Transactions T left join Reps R
ON T.RepID = R.RepID
group by T.RepID
order by TotalProfit;

# 4. List the Customers who generated the most Profit (Sales minus Expenses) over the entire two‐year period
#    for the “Mowers” category. (sort highest to lowest)

select C.CustName,sum(T.Sales)-sum(T.Expenses) 'TotalProfit'
from Transactions T left join Customers C
ON T.CustID = C.CustID
where T.ProductLine = 'Mowers'
group by T.CustID
order by TotalProfit desc;

# 5. The Product Manager responsible for “Mowers” is interested in Sales of Mowers across all the states. Which
#    state has the highest mower sales? Can you identify the top states for the highest mower sales?
select C.State, sum(T.Sales) 'TotalSales'
from Transactions T left join Customers C
ON T.CustID = C.CustID
where T.ProductLine = 'Mowers'
group by C.State
order by TotalSales desc;

select D.State as 'TopSalesState', D.TotalSales
from (select C.State, sum(T.Sales) 'TotalSales'
from Transactions T left join Customers C
ON T.CustID = C.CustID
where T.ProductLine = 'Mowers'
group by C.State) as D
where D.TotalSales = (select max(D.TotalSales)
from (select C.State, sum(T.Sales) 'TotalSales'
from Transactions T left join Customers C
ON T.CustID = C.CustID
where T.ProductLine = 'Mowers'
group by C.State) as D);


# 6. List the Male Sales Representative and his Sales, who generated the highest individual sale for customers in
#    Arkansas, in the year of 2012 inclusive.

select R.RepName, sum(T.Sales) as 'TotalSales'
from Transactions T left join Reps R 
on T.RepID = R.RepID left join Customers C 
on T.CustID = C.CustID
where C.State = 'Arkansas' and YEAR(TransDate) = 2012 and R.Gender = 'M'
group by R.RepName
order by TotalSales desc;

select D.RepName, D.TotalSales
from (select R.RepName, sum(T.Sales) as 'TotalSales'
from Transactions T left join Reps R 
on T.RepID = R.RepID left join Customers C 
on T.CustID = C.CustID
where C.State = 'Arkansas' and YEAR(TransDate) = 2012 and R.Gender = 'M'
group by R.RepName) as D 
where D.TotalSales = (select max(D.TotalSales) from (select R.RepName, sum(T.Sales) as 'TotalSales'
from Transactions T left join Reps R 
on T.RepID = R.RepID left join Customers C 
on T.CustID = C.CustID
where C.State = 'Arkansas' and YEAR(TransDate) = 2012 and R.Gender = 'M'
group by R.RepName) as D);

# 7. Your VP is interested in finding out if Total Sales by Product Line is impacted by Market Size. Which
#    Product Lines are doing better in Small Markets vs. Large Markets? Show the result as a single query
#    showing most Sales by Product Line and Market size.
#    (Hint: there should only be two results)

