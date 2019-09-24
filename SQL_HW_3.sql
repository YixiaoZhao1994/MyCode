use mydb_yzhao77;
/* 7‐77a. List the artist IDs, full	names (in the format of	 ‘FirstName	LastName’),	years	
of	birth and current  royalty	percentages	for	all	artists	that currently have	a contract.*/	

SELECT distinct(concat(a.FirstName, ' ', a.LastName)) Fullname, a.ArtistID, a.YearOfBirth, c.RoyaltyPerc, c.EndDate 
from ARTIST_T a, CONTRACT_T c
WHERE a.ArtistID=c.ArtistID and c.EndDate >= curdate()
and a.ArtistID in
(select ArtistID from CONTRACTEDARTIST_T);


/* 7‐77b. List the event ID, event description,	date, and time for all events at	
which artist Juan Becker is	scheduled to perform. Make sure	that	
each event is included only once.*/
SELECT distinct(EventID), EventDesc, DateTime 
FROM EVENT_T
WHERE EventID in
(SELECT EVENTID from AGREEMENT_T 
WHERE ContractID in 
(SELECT ContractID from CONTRACT_T
WHERE ArtistID in 
(Select ArtistID from ARTIST_T
WHERE FirstName='Juan' and LastName = 'Becker')));


/* 7‐77c. List the artist name,	event ID, event	description, date, time, and gross amount earned for	
all	events at which	manager	#1’s artists have performed	at (based on event dates) in December 2014 and	
January	2015. */	
SELECT a.FirstName, a.LastName, e.EventID, e.DateTime, g.GrossAmount
FROM ARTIST_T as a, EVENT_T as e, CONTRACT_T as c, AGREEMENT_T as g, CONTRACTEDARTIST_T as con
	WHERE a.ArtistID=con.ArtistID 
		and con.ArtistID=c.ArtistID
			and c.ContractID=g.ContractID
				and g.EventID= e.EventID
			and con.AManagerID=1 and e.DateTime BETWEEN "2014-12-01" and "2015-01-31";

/* 7‐77d. For the events specified	in the query above,	determine the artist’s share, manager’s share,	
and	FAME share (assume an event split (50/50) between manager and FAME). */	

SELECT AR.LastName, AR.FirstName,
SUM(GrossAmount) AS TotalGross,
SUM(GrossAmount)-SUM(GrossAmount*RoyaltyPerc/100) AS ArtistShare,
0.5*SUM(GrossAmount*RoyaltyPerc/100) AS ManagerShare,
0.5*SUM(GrossAmount*RoyaltyPerc/100) AS FAMEShare
FROM EVENT_T E, AGREEMENT_T A, CONTRACT_T C, CONTRACTEDARTIST_T CA,
ARTIST_T AR
WHERE E.EventID = A.EventID AND
A.ContractID = C.ContractID AND
C.ArtistID = CA.ArtistID AND
CA.ArtistID = AR.ArtistID AND
CA.AManagerID = 1 AND
E.DateTime BETWEEN '2014-12-01' AND '2015-01-31';





/* 7‐77e. For artist Juan Becker, list event ID, event description,	date and time, gross revenue	
earned,	and	artist’s share of the gross	revenue	for	all	events at that	
took place from	December 1, 2014 to	March 2015. */
SELECT e.EventID, e.EventDesc, e.DateTime, ag.GrossAmount, round((c.RoyaltyPerc/100)*ag.GrossAmount) as ArtistShare
	FROM EVENT_T e, AGREEMENT_T ag, CONTRACT_T c, ARTIST_T a
		WHERE e.EventID = ag.EventID
			and ag.ContractID = c.ContractID
				and c.ArtistID = a.ArtistID
			and a.LastName = "Becker" and a.FirstName = "Juan" 
		and e.DateTime between "2014-12-01" and "2015-03-31";
            

/* 7‐77f. For artist Pat Jiminez, list all commitments (including category information	
and	event description when appropriate) for	December 2014. */
SELECT AC.StartDateTime,AC.EndDateTime,
AC.CommitmentType,PRC.PRCCategory,
E.EventDesc,V.VenueName
FROM ARTIST_T AR, CONTRACTEDARTIST_T CA,
ARTISTCOMMITMENT_T AC,
PERFORMANCERELATEDC_T PRC, EVENT_T E,VENUE_T V
WHERE AR.ArtistID = CA.ArtistID AND
CA.ArtistID = AC.ArtistID AND
AC.ACommitmentID = PRC.ACommitmentID AND
AC.CommitmentType = 'PR' AND
PRC.PRCCategory = 'P' AND
PRC.EventID = E.EventID AND
E.VenueID = V.VenueID AND
AR.ArtistID = 10

UNION
SELECT AC.StartDateTime,AC.EndDateTime,
AC.CommitmentType,PRC.PRCCategory,
E.EventDesc,V.VenueName
FROM ARTIST_T AR, CONTRACTEDARTIST_T CA, ARTISTCOMMITMENT_T AC,
PERFORMANCERELATEDC_T PRC, EVENT_T E,VENUE_T V
WHERE AR.ArtistID = CA.ArtistID AND
CA.ArtistID = AC.ArtistID AND
AC.ACommitmentID = PRC.ACommitmentID AND
AC.CommitmentType = 'PR' AND
PRC.PRCCategory IN ('R','T') AND
PRC.EventID = E.EventID AND
E.VenueID = V.VenueID AND
AR.ArtistID = 10

UNION
SELECT AC.StartDateTime,AC.EndDateTime,
AC.CommitmentType,NULL,NULL,NULL
FROM ARTIST_T AR, CONTRACTEDARTIST_T CA, ARTISTCOMMITMENT_T AC
WHERE AR.ArtistID = CA.ArtistID AND
CA.ArtistID = AC.ArtistID AND
AC.CommitmentType = 'PE' AND
AR.ArtistID = 10;



SELECT ac.CommitmentType, ac.ACommitmentID, ac.EndDateTime, e.EventDesc, a.LastName
	FROM ARTISTCOMMITMENT_T ac, EVENT_T e, ARTIST_T a, PERFORMANCERELATEDC_T p
		WHERE a.ArtistID=ac.ArtistID
			and p.ACommitmentID=ac.ACommitmentID
				and p.EventID=e.EventID
			and ac.EndDateTime between "01-DEC-2014" and "31-DEC-2014"
		and a.LastName="Jiminez";
UNION
SELECT ac.CommitmentType, ac.ACommitmentID, a.LastName, NULL, Null
	FROM ARTISTCOMMITMENT_T ac, EVENT_T e, ARTIST_T a, PERSONALC_T pc
		WHERE a.ArtistID=ac.ArtistID
			and pc.ACommitmentID=ac.ACommitmentID
				and ac.EndDateTime between "01-DEC-2014" and "31-DEC-2014"
		and a.LastName="Jiminez";