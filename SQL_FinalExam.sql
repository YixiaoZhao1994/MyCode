use foodmart;
# A1. What are the total sales of “Potato Chips” including the quantity sold and sales by store? 
#     (Sort the results by Total Sales)?

select store_name, sum(store_sales) as 'TotalSales', sum(unit_sales) as 'TotalQuantity'
from product p join sales_fact s
on p.product_id = s.product_id join store st 
on s.store_id = st.store_id
where p.product_name like '%Potato Chips'
group by st.store_name
order by sum(store_sales) desc;

# A2. What are total sales of all products, per month, for all stores? (Sort the results by Month)
select month_of_year, sum(store_sales) as 'TotalSales'
from time_by_day t, sales_fact s 
where t.time_id = s.time_id
group by t.month_of_year
order by t.month_of_year;

select the_month, sum(store_sales) as 'TotalSales'
from time_by_day t, sales_fact s 
where t.time_id = s.time_id
group by the_month
order by month(str_to_date(the_month,'%M'));

select store_name, month(the_date), sum(store_sales) as 'TotalSales'
from time_by_day t, sales_fact s, store st
where t.time_id = s.time_id and st.store_id = s.store_id
group by month(the_date), st.store_name
order by month(the_date);


# A3. What are the total sales	of	each store,	by region?	(Sort the results by Total Sales)
select sales_region, store_name, sum(store_sales) as 'TotalSales'
from store st, region r, sales_fact s
where st.region_id = r.region_id and
st.store_id = s.store_id
group by r.sales_region, st.store_name
order by sum(store_sales) desc;



# A4. Which	product	category sells the	most? (Hint: there	can	only	be	one	answer	and	your query	must show this)
select T.product_category as 'Max product_category', max(T.TotalSales) as 'TotalSales' from
(select product_category, sum(store_sales) as 'TotalSales'
from product_class pc, sales_fact s, product p
where pc.product_class_id = p.product_class_id and
p.product_id = s.product_id
group by pc.product_category
order by sum(store_sales) desc) as T;

#### Optional ####
select product_category, sum(store_sales) as 'TotalSales'
from product_class pc, sales_fact s, product p
where pc.product_class_id = p.product_class_id and
p.product_class_id = s.product_id
group by pc.product_category
order by sum(store_sales) desc limit 1;

select product_category, sum(store_sales) as 'TotalSales'
from product_class pc, sales_fact s, product p
where pc.product_class_id = p.product_class_id and
p.product_class_id = s.product_id
group by pc.product_category
order by sum(store_sales) desc;

# A5. Which	days of	the	week do	customers prefer to	go shopping?	
# (Sort the	results	by Total Sales) (Hint: for this	query, you will need to use	the	Dayname	function and your query	must show this)
SELECT DAYNAME(the_date) as 'Days of Week', sum(store_sales) as 'TotalSales'
from time_by_day t, sales_fact s
where t.time_id = s.time_id
group by DAYNAME(the_date)
order by sum(store_sales) desc;

