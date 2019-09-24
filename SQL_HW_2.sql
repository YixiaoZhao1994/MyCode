
/* Question a */

SELECT  A.ArtistID, FirstName||' '||LastName, YearOfBirth, RoyaltyPerc
FROM    Artist_T A, ContractedArtist_T CA, Contract_T C
WHERE   A.ArtistID = CA.ArtistID AND
        C.ArtistID = CA.ArtistID;
        
/* Question b*/

SELECT  DISTINCT E.EventID, E.EventDesc, E.DateTime
FROM    Event_T E, Agreement_T A, Contract_T C, ContractedArtist_T CA,
        Artist_T A
WHERE   E.EventID = A.EventID AND
        A.ContractID = C.ContractID AND
        C.ArtistID = CA.ArtistID AND
        CA.ArtistID = A.ArtistID AND
        A.LastName = 'Becker' AND A.FirstName = 'Juan';

/* Question c*/

SELECT  A.LastName, A.FirstName, E.EventID, 
        E.EventDesc,E.DateTime,A.GrossAmount
FROM    Event_T E, Agreement_T A, Contract_T C, ContractedArtist_T CA,
        Artist_T AR 
WHERE   E.EventID = A.EventID AND
        A.ContractID = C.ContractID AND
        C.ArtistID = CA.ArtistID AND
        CA.ArtistID = A.ArtistID AND
        CA.AManagerID = 1 AND
        E.DateTime BETWEEN '1-DEC-2014' AND '31-JAN-2015';

/* Question d*/

SELECT  SUM(GrossAmount)-SUM(GrossAmount*RoyaltyPerc/100) AS ArtistShare, 
        0.5*SUM(GrossAmount*RoyaltyPerc/100) AS ManagerShare,
        0.5*SUM(GrossAmount*RoyaltyPerc/100) AS FAMEShare
FROM    Event_T E, Agreement_T A, Contract_T C, ContractedArtist_T CA,
        Artist_T A 
WHERE   E.EventID = A.EventID AND
        A.ContractID = C.ContractID AND
        C.ArtistID = CA.ArtistID AND
        CA.ArtistID = A.ArtistID AND
        CA.AManagerID = 1 AND
        E.DateTime BETWEEN '1-DEC-2014' AND '31-JAN-2015';

/* Question e */

SELECT  E.EventID, E.EventDesc,E.DateTime,
        A.GrossAmount, A.GrossAmount*(100-C.RoyaltyPerc)/100 "Artist Share"
FROM    Event_T E, Agreement_T A, Contract_T C, ContractedArtist_T CA,
        Artist_T AR 
WHERE   E.EventID = A.EventID AND
        A.ContractID = C.ContractID AND
        C.ArtistID = CA.ArtistID AND
        CA.ArtistID = AR.ArtistID AND
        AR.LastName = 'Becker' AND AR.FirstName = 'Juan' AND
        E.DateTime BETWEEN '1-DEC-2014' AND '31-JAN-2015';
        
/* Question f */

SELECT  AC.StartDateTime,AC.EndDateTime,
        AC.CommitmentType,PRC.PRCCategory,
        E.EventDesc,V.VenueName
FROM    Artist_T AR, ContractedArtist_T CA, 
        ArtistCommitment_T AC, 
        PerformancerelatedC_T PRC, Event_T E,Venue_T V
WHERE   AR.ArtistID = CA.ArtistID AND
        CA.ArtistID = AC.ArtistID AND
        AC.ACommitmentID = PRC.ACommitmentID AND
        AC.CommitmentType = 'PR' AND
        PRC.PRCCategory = 'P' AND
        PRC.EventID = E.EventID AND
        E.VenueID = V.VenueID AND
        AR.ArtistID = 10
      
UNION

SELECT  AC.StartDateTime,AC.EndDateTime,
        AC.CommitmentType,PRC.PRCCategory,
        E.EventDesc,V.VenueName
FROM    Artist_T AR, ContractedArtist_T CA, ArtistCommitment_T AC, 
        PerformancerelatedC_T PRC, Event_T E,Venue_T V
WHERE   AR.ArtistID = CA.ArtistID AND
        CA.ArtistID = AC.ArtistID AND
        AC.ACommitmentID = PRC.ACommitmentID AND
        AC.CommitmentType = 'PR' AND
        PRC.PRCCategory IN ('R','T') AND
        PRC.EventID = E.EventID AND
        E.VenueID = V.VenueID AND
        AR.ArtistID = 10

UNION

SELECT  AC.StartDateTime,AC.EndDateTime,
        AC.CommitmentType,NULL,NULL,NULL
FROM    Artist_T AR, ContractedArtist_T CA, ArtistCommitment_T AC 
WHERE   AR.ArtistID = CA.ArtistID AND
        CA.ArtistID = AC.ArtistID AND
        AC.CommitmentType = 'PE' AND
        AR.ArtistID = 10;

    