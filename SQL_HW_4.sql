Use mydb_yzhao77;

# 6‐92a. List the agreement number, agreement date, and gross amount for each agreement that has a gross
#        amount greater than or equal to $1,500 and less than or equal to $2,000.
select AgreementDate, AgreementNbr, GrossAmount
from AGREEMENT_T
where GrossAmount between 1500 and 2000;

# 6‐92b. Provide a list of those customers (include their ID and name) the name of which includes the word “Arts”.
select CustomerID, CustomerName
from CUSTOMER_T
where CustomerName LIKE '%Arts%';

# 6‐92c. List the artist ID, last name, first name, year of birth, and artist type for all female artists who live
#        either in Pennsylvania or New Jersey.
select ArtistID, LastName, FirstName, YearOfBirth, ArtistType
from ARTIST_T
where State IN ('PA','NJ') AND Gender = 'F';

# 6‐92d. List the artist ID, start date, end date, and royalty percentage for all currently valid contracts that
#        have a royalty percentage greater than 20%. Sort the list in the ascending order by artist ID.
select ArtistID, StartDate, EndDate, RoyaltyPerc
from CONTRACT_T
where StartDate < now() and EndDate > now() and RoyaltyPerc > 20
order by ArtistID;

# 6‐92e. What is the total amount of those customer payments that were made in either February 2015 or
#        March 2015?
select sum(Amount)
from CUSTOMERPAYMENT_T
where year(CPaymentDate) =2015 and month(CPaymentDate) between 02 and 03;

# 6‐92f. List the IDs of those artists that were paid at least $2000 during the first three months of 2015.
SELECT ArtistID, sum(Amount)
FROM ARTISTPAYMENT_T
WHERE APaymentDate BETWEEN '2015-01-01' AND '2015-03-31'
group by ArtistID
having sum(Amount) >= 2000 ;

# 6-92g. List the number of events organized at each of the venues (for those included in the Event table).
SELECT VenueID, COUNT(EventID)
FROM EVENT_T
GROUP BY VenueID;

# 6-92h. Modify the previous query so that you include only those venues at which at least two events have
#        been organized.
SELECT VenueID, COUNT(EventID)
FROM EVENT_T
GROUP BY VenueID
HAVING COUNT(EventID) >= 2;

# 6-92i. List the ID, description, amount, and expense type for all manager expense items the amount of
#        which is less than $100 and for all artist expense items the amount of which is less than $50.
SELECT ExpenseID, Description, Amount, ExpenseType
FROM EXPENSE_T
WHERE (Amount < 100 AND ExpenseType = 'M') OR
(Amount < 50 AND ExpenseType = 'A');



# 7-77a. List the artist IDs, full names (in the format of ‘FirstName LastName’) , years of birth and current
#        royalty percentages for all artists that currently have a contract.
SELECT A.ArtistID, Concat(FirstName, ' ', LastName), YearOfBirth, RoyaltyPerc, EndDate 
FROM ARTIST_T A, CONTRACTEDARTIST_T CA, CONTRACT_T C
WHERE A.ArtistID = CA.ArtistID AND
C.ArtistID = CA.ArtistID AND
C.EndDate >= curdate();

SELECT distinct(concat(a.FirstName, ' ', a.LastName)) Fullname, a.ArtistID, a.YearOfBirth, c.RoyaltyPerc 
FROM ARTIST_T a, CONTRACT_T c
WHERE a.ArtistID=c.ArtistID
AND a.ArtistID in (select ArtistID from CONTRACTEDARTIST_T where EndDate >=now());

# 7‐77b. List the event ID, event description, date, and time for all events at which artist Juan Becker is scheduled to perform. 
#        Make sure that each event is included only once. 

SELECT distinct(EventID), EventDesc, DateTime 
FROM EVENT_T
WHERE EventID in (SELECT EVENTID from AGREEMENT_T 
WHERE ContractID in (SELECT ContractID from CONTRACT_T
WHERE ArtistID in (Select ArtistID from ARTIST_T
WHERE FirstName='Juan' and LastName = 'Becker')));

# 7‐77c. List the artist name, event ID, event description, date, time, and gross amount earned for all events at which manager 
#        #1’s artists have performed at (based on event dates) in December 2014 and January 2015.

SELECT a.FirstName, a.LastName, e.EventID, e.DateTime, g.GrossAmount, e.EventDesc
FROM ARTIST_T AS a, EVENT_T AS e, CONTRACT_T AS c, AGREEMENT_T AS g, CONTRACTEDARTIST_T AS con
WHERE a.ArtistID=con.ArtistID 
AND con.ArtistID=c.ArtistID
AND c.ContractID=g.ContractID
AND g.EventID= e.EventID
AND con.AManagerID=1 AND e.DateTime BETWEEN "2014-12-01" AND "2015-01-31";

# 7‐77d. For the events specified in the query above, determine the artist’s share, manager’s share, and 
#        FAME share (assume an event split (50/50) between manager and FAME).

SELECT AR.LastName, AR.FirstName,
round(SUM(GrossAmount)) AS TotalGross,
round(sum(GrossAmount*(1-RoyaltyPerc/100))) AS ArtistShare,
round(0.5*SUM(GrossAmount*RoyaltyPerc/100)) AS ManagerShare,
round(0.5*SUM(GrossAmount*RoyaltyPerc/100)) AS FAMEShare
FROM EVENT_T E, AGREEMENT_T A, CONTRACT_T C, CONTRACTEDARTIST_T CA, ARTIST_T AR
WHERE E.EventID = A.EventID 
AND A.ContractID = C.ContractID 
AND C.ArtistID = CA.ArtistID 
AND CA.ArtistID = AR.ArtistID 
AND CA.AManagerID = 1 
AND E.DateTime BETWEEN '2014-12-01' AND '2015-01-31';

# 7‐77e. For artist Juan Becker, list event ID, event description, date and time, gross revenue earned, and 
#        artist’s share of the gross revenue for all events at that took place from December 1, 2014 to March 2015.

SELECT e.EventID, e.EventDesc, e.DateTime, ag.GrossAmount, round(((100 - c.RoyaltyPerc)/100)*ag.GrossAmount) as ArtistShare
FROM EVENT_T e, AGREEMENT_T ag, CONTRACT_T c, ARTIST_T a
WHERE e.EventID = ag.EventID
AND ag.ContractID = c.ContractID
AND c.ArtistID = a.ArtistID
AND a.LastName = "Becker" and a.FirstName = "Juan" 
AND e.DateTime between "2014-12-01" and "2015-03-31";

# 7‐77f. For artist Pat Jiminez, list all commitments (including category information and event description when appropriate) for December 2014.

SELECT ARTISTCOMMITMENT_T.StartDateTime, ARTISTCOMMITMENT_T.EndDateTime,
ARTISTCOMMITMENT_T.CommitmentType,
PERFORMANCERELATEDC_T.PRCCategory, EVENT_T.EventDesc, VENUE_T.VenueName
FROM ((PERFORMANCERELATEDC_T 
RIGHT JOIN ((ARTIST_T JOIN CONTRACTEDARTIST_T ON ARTIST_T.ArtistID = CONTRACTEDARTIST_T.ArtistID)
JOIN ARTISTCOMMITMENT_T ON CONTRACTEDARTIST_T.ArtistID = ARTISTCOMMITMENT_T.ArtistID) 
ON PERFORMANCERELATEDC_T.ACommitmentID = ARTISTCOMMITMENT_T.ACommitmentID)
LEFT JOIN EVENT_T ON PERFORMANCERELATEDC_T.EventID = EVENT_T.EventID)
LEFT JOIN VENUE_T ON EVENT_T.VenueID = VENUE_T.VenueID
GROUP BY ARTISTCOMMITMENT_T.StartDateTime, ARTISTCOMMITMENT_T.EndDateTime,
ARTISTCOMMITMENT_T.CommitmentType, PERFORMANCERELATEDC_T.PRCCategory,
EVENT_T.EventDesc, ARTIST_T.ArtistID, VENUE_T.VenueName
HAVING (((ARTIST_T.ArtistID)='10'));




