USE Northwind;
/*--------------------------------------------------------- PART 1 -----------------------------------------------------------*/

/* Q1: */
SELECT TerritoryDescription 
FROM Territories
WHERE TerritoryDescription LIKE '%Santa%';

/* Q2: */
SELECT TerritoryDescription 
FROM Territories
WHERE TerritoryDescription LIKE 'C%';

/* Q3: */
SELECT TerritoryDescription 
FROM Territories
WHERE TerritoryDescription LIKE '%boro%';

/* Q4: */
SELECT ContactName, ContactTitle 
FROM Customers
WHERE ContactTitle = 'Owner';

/* Q5: */
SELECT CompanyName, HomePage 
FROM Suppliers
WHERE HomePage IS NOT NULL;

/* Q6: */
SELECT CompanyName, HomePage 
FROM Suppliers
WHERE HomePage IS NULL;

/* Q7: */
SELECT ProductName, UnitPrice
FROM Products
WHERE UnitPrice >= 50;

/* Q8: */
SELECT ProductName, UnitPrice
FROM Products
WHERE UnitPrice < 10;

/* Q9: */
SELECT ProductName, UnitPrice
FROM Products
WHERE UnitPrice <= 10;

/* Q10: */
SELECT ProductName, UnitPrice
FROM Products
WHERE UnitPrice >= 15 AND UnitPrice <= 20;

/* Q11: */
SELECT ProductName, UnitsOnOrder
FROM Products
WHERE UnitsOnOrder > 0;

/* Q12: */
SELECT FirstName, LastName, YEAR(GETDATE())- YEAR(BirthDate) Age
FROM Employees;

/* Q13: */
SELECT FirstName, LastName, YEAR(GETDATE())- YEAR(HireDate) 'Years with Northwind'
FROM Employees;

/* Q14: */
SELECT SUM(Freight) 'Freight Total'
FROM Orders;

/* Q15: */
SELECT SUM(Freight) 'Freight Total'
FROM Orders
WHERE YEAR(OrderDate) = 1996;

/* Q16: */
SELECT SUM(Freight) 'Freight Total'
FROM Orders
WHERE YEAR(OrderDate) = 1997;

/* Q17: */
SELECT SUM(Freight) 'Freight Total'
FROM Orders
WHERE YEAR(OrderDate) = 1998;

/* Q18: */
SELECT OrderID, ShipCountry 
FROM Orders
WHERE ShipCountry = 'Switzerland' OR ShipCountry = 'Venezuela'OR ShipCountry = 'Austria';

/* Q19: */
SELECT DISTINCT(ShipCity)
FROM Orders
WHERE ShipCountry = 'Belgium' OR ShipCountry = 'Mexico'OR ShipCountry = 'Sweden ';

/* Q20: */
SELECT C.CategoryName, P.ProductName
FROM Products P INNER JOIN Categories C
ON P.CategoryID = C.CategoryID
WHERE CategoryName = 'Dairy Products' OR CategoryName = 'Seafood' OR CategoryName = 'Beverages';





/*--------------------------------------------------------- PART 2 -----------------------------------------------------------*/

/* Q1: Get USA and UK Customer's List and their Contact Information. (Table: Customers) */
SELECT CompanyName, ContactName, Phone, Fax
FROM Customers
WHERE Country = 'USA' OR Country = 'UK';


/* Q2: Get Customer List who are from USA or from SP Region of Brazil. (Table: Customers) */
SELECT CompanyName, ContactName, Country, Region
FROM Customers
WHERE Country = 'USA' OR (Country = 'Brazil' AND Region = 'SP');

/* Q3: Get all the CustomerID and OrderID for order placed in third quarter of 1997. (Table: Orders) */
SELECT CustomerID, OrderID
FROM Orders
WHERE YEAR(OrderDate) = 1997 AND (MONTH(OrderDate) = '7' OR MONTH(OrderDate) = '8' OR MONTH(OrderDate) = '9');

/* Q4: Get the Customer who's Company Name starts with either A, B, C, D, E, F, or G and ends with N or E. (Table: Customers) */
SELECT CompanyName, ContactName
FROM Customers
WHERE CompanyName LIKE '[ABCDEFG]%' AND CompanyName LIKE '%[NE]';

/* Q5: Get the list of Customers where Company Name's 2nd letter is Consonant. (Table: Customers) */
SELECT CompanyName, ContactName
FROM Customers
WHERE CompanyName LIKE '_[^AEIOU]%';


/* Q7: We acquire new Shipper and it does not ship to PO Box. Generate the list of Customers which has PO Box address. (Table: Customers) */
SELECT CompanyName, ContactName, Address
FROM Customers
WHERE Address LIKE 'P.O.%';

/* Q8: Get the Customer Count by (Table: Customers)
            i. Country
            ii. Country, Region
            iii. Country, Region, City */

SELECT Country, COUNT(CustomerID) CountCustomers
FROM Customers
Group By Country;

SELECT Country, Region, COUNT(CustomerID) CountCustomers
FROM Customers
Group By Country, Region;

SELECT Country, Region, City, COUNT(CustomerID) CountCustomers
FROM Customers
Group By Country, Region, City;

/* Q9: The Company wants to increase its relationship with higher raking officers of Customer. 
       Get the Count of Contact's person by their title. (Table: Customers) */
SELECT ContactTitle, COUNT(CustomerID) CountCustomers
FROM Customers
Group By ContactTitle;

/* Q10: Get the list of Customers and Number of Orders placed by each customer till date. (Table: Orders) */
SELECT C.CompanyName, C.ContactName, O.CustomerID, COUNT(O.OrderID) 'Numbers of Order Made'
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
Group By C.CompanyName, C.ContactName, O.CustomerID;

/* Q11: Get the top 10 Customers who has placed most order till date. */
SELECT TOP 10 C.CompanyName, C.ContactName, O.CustomerID, COUNT(O.OrderID) 'Numbers of Order Made'
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
Group By C.CompanyName, C.ContactName, O.CustomerID
Order By COUNT(O.OrderID) DESC;
-- OR --
SELECT TOP 10 CustomerID, COUNT(OrderID) 'Numbers of Order Made'
From Orders 
Group By CustomerID
Order By COUNT(OrderID) DESC;


/* Q12: Get Customers list who has placed 5 or more Orders. (Table:Orders) */
SELECT C.CompanyName, C.ContactName, O.CustomerID, COUNT(O.OrderID) 'Numbers of Order Made'
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
Group By C.CompanyName, C.ContactName, O.CustomerID
HAVING COUNT(O.OrderID) >= 5;

/* Q13: Get the Order Count by (Table:Orders)
            I. Each Year (Hint: YEAR() function)
          ii. Each quarter in each year (Hint: research on DATEPART() function)
         iii. Each Month in each year (Hint: research on DATEPART() function) */
SELECT YEAR(OrderDate) 'Year', COUNT(OrderID) 'Numbers of Order Made'
FROM Orders
Group By YEAR(OrderDate)
Order By YEAR(OrderDate);

SELECT YEAR(OrderDate) 'Year', DATEPART(quarter,OrderDate) 'Quarter', COUNT(OrderID) 'Numbers of Order Made'
FROM Orders
Group By YEAR(OrderDate), DATEPART(quarter,OrderDate)
Order By YEAR(OrderDate);

/* Q14: Calculate Average, Total, Minimum, and Maximum Frieght paid (Table:Orders)
            i. For each Order
            ii. For each Company
            iii. For each Country on all orders
            iiv. for Each Carrier (ShipVia) */
SELECT OrderID, AVG(Freight) AverageFreight, SUM(Freight) TotalFreight, 
       MIN(Freight) MinFreight, MAX(Freight) MaxFreight
From Orders
Group By OrderID;

SELECT C.CompanyName, C.ContactName, O.CustomerID, AVG(O.Freight) AverageFreight, 
	   SUM(O.Freight) TotalFreight, MIN(O.Freight) MinFreight, MAX(O.Freight) MaxFreight
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
Group By C.CompanyName, C.ContactName, O.CustomerID;

SELECT C.Country, AVG(O.Freight) AverageFreight, SUM(O.Freight) TotalFreight, 
	   MIN(O.Freight) MinFreight, MAX(O.Freight) MaxFreight
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
Group By C.Country;

SELECT ShipVia, AVG(Freight) AverageFreight, SUM(Freight) TotalFreight, 
       MIN(Freight) MinFreight, MAX(Freight) MaxFreight
From Orders
Group By ShipVia
Order by ShipVia;

/* Q15: List Total Sale for each Product in each Order (Hint: UnitPrice * Quantity - UnitPrice * Quantity * Discount) */
SELECT ProductID, OrderID, (UnitPrice * (Quantity - Discount)) TotalSales
FROM [Order Details];

/* Q16: For each Order Calculate (Table: [Orders Details])
            i. Types of Products Ordered (Hint: Count on Product)
            ii. Total Sale for each Order */
SELECT OrderID, COUNT(ProductID) 'Types of Products Ordered'
FROM [Order Details]
GROUP BY OrderID;

SELECT OrderID, SUM(UnitPrice * (Quantity - Discount)) TotalSales
FROM [Order Details]
GROUP BY OrderID;

/* Q17: List total Quantity Ordered for Each Product on all orders. (Table: [Orders Details]) */
SELECT ProductID, SUM(Quantity) TotalQuantity
FROM [Order Details]
GROUP BY ProductID;

/* Q18: List top 10 Most Sold products in quantity in an order. (Table: [Orders Details]) */
SELECT TOP 10 ProductID, SUM(Quantity) TotalQuantity
FROM [Order Details]
GROUP BY ProductID
ORDER BY TotalQuantity DESC;

/* Q19: Provide a SQL statement and sample result set that will count orders for all Customers 
		within the SP (Brazil) state during the 1997 calendar year sorted by Customer name. */

-- IF think CompanyName as Customer name --
SELECT C.CompanyName, C.ContactName, C.Country, C.Region, YEAR(O.OrderDate) OrderYear, COUNT(O.OrderID) 'Numbers of Order Made'
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
WHERE C.Country = 'Brazil' AND C.Region = 'SP' AND YEAR(O.OrderDate) = 1997
Group By C.CompanyName, C.ContactName, C.Country, C.Region, YEAR(O.OrderDate)
ORDER BY C.CompanyName;

-- IF think ContactName as Customer name --
SELECT C.CompanyName, C.ContactName, C.Country, C.Region, YEAR(O.OrderDate) OrderYear, COUNT(O.OrderID) 'Numbers of Order Made'
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
WHERE C.Country = 'Brazil' AND C.Region = 'SP' AND YEAR(O.OrderDate) = 1997
Group By C.CompanyName, C.ContactName, C.Country, C.Region, YEAR(O.OrderDate)
ORDER BY C.ContactName;

/* Q20: Provide a SQL statement and sample result set that will list all Customers 
        within the SP (Brazil) state that have placed 7 or more orders during the 1998 calendar year. */
SELECT C.CompanyName, C.ContactName, C.Country, C.Region, YEAR(O.OrderDate) OrderYear, COUNT(O.OrderID) 'Numbers of Order Made'
From Orders O JOIN Customers C
ON O.CustomerID = C.CustomerID
WHERE C.Country = 'Brazil' AND C.Region = 'SP' AND YEAR(O.OrderDate) = 1998
Group By C.CompanyName, C.ContactName, C.Country, C.Region, YEAR(O.OrderDate)
HAVING COUNT(O.OrderID) > = 7;
