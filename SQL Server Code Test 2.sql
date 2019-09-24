Create Table EmpDemo
(
EmpNo int identity Primary Key,
Ename Varchar(15),
Salary Money,
DeptNo int
)


Create Table EmpDemo_Log
(
EmpNo int ,
Ename Varchar(15),
Salary Money,
DeptNo int,
[Action] varchar(15),
DateCreated DateTime
)
GO

/* Q1: 1.	Create a trigger for EmpDemo table so that when a record is inserted/updated and deleted 
            it get inserted into the EmpDemo_Log table. */
CREATE TRIGGER keepRecord
ON EmpDemo
AFTER INSERT
AS 
declare @EmpNo int,@Ename Varchar(15), @Salary Money, @DeptNo int, @Date date, @Action Varchar(20);
if exists(SELECT * from inserted) and exists (SELECT * from deleted)
BEGIN
    SET @Action = 'UPDATE';
    SET @Date = GETDATE();
    SELECT @EmpNo = i.EmpNo, @Ename = i.Ename, @Salary= i.Salary, @DeptNo = i.DeptNo from inserted i;
    INSERT INTO EmpDemo_Log
	(EmpNo,Ename, Salary, DeptNo, Action, DateCreated) 
	values 
	(@EmpNo,@Ename,@Salary, @DeptNo, @Action, @Date);
end
If exists (Select * from inserted) and not exists(Select * from deleted)
begin
    SET @Action = 'INSERT';
    SET @Date = GETDATE();
    SELECT @EmpNo = i.EmpNo, @Ename = i.Ename, @Salary= i.Salary, @DeptNo = i.DeptNo from inserted i;
    INSERT INTO EmpDemo_Log
	(EmpNo,Ename, Salary, DeptNo, Action, DateCreated) 
	values 
	(@EmpNo,@Ename,@Salary, @DeptNo, @Action, @Date);
end
If exists(select * from deleted) and not exists(Select * from inserted)
begin 
    SET @Action = 'DELETE';
    SET @Date = GETDATE();
    SELECT @EmpNo = d.EmpNo, @Ename = d.Ename, @Salary= d.Salary, @DeptNo = d.DeptNo from deleted d;
    INSERT INTO EmpDemo_Log
	(EmpNo,Ename, Salary, DeptNo, Action, DateCreated) 
	values 
	(@EmpNo,@Ename,@Salary, @DeptNo, @Action, @Date);
end;
GO



/* Q2: Write a procedure to get employee data based on DeptNo. 
       If value for DeptNo  is not given get all employees (Using Optional parameter) */
CREATE PROCEDURE GetEmpDataN
(
	@DeptNo int = null
)
AS
IF @DeptNo is null 
BEGIN
	SELECT * FROM EmpDemo
END
IF @DeptNo is not null
BEGIN
	SELECT * FROM EmpDemo
	WHERE DeptNo = @DeptNo
END;

EXEC GetEmpDataN;
EXEC GetEmpDataN 10000;
GO


/* Q3: Write a procedure to insert data into EmpDemo table and get new EmpNo generated as Output parameter */
CREATE PROCEDURE GetEmpNoA
(
	@Ename Varchar(15) = NULL,
    @Salary Money = NULL,
	@DeptNo int = NULL,
	@EmpNo int = NULL OUTPUT
)
AS
BEGIN 
SET NOCOUNT ON 
INSERT INTO EmpDemo(Ename, Salary, DeptNo)
VALUES ( @Ename, @Salary, @DeptNo)

SELECT @EmpNo = EmpNo 
FROM EmpDemo 
WHERE Ename = @Ename
PRINT @EmpNo
END;
GO


/* Q4: Write a user defined function to get total salary of given department (user transaction and error handling) */
CREATE FUNCTION TotalSalary (@DeptNo int)  
RETURNS int  
AS  
BEGIN  
    DECLARE @TotalSalary int;  
    SELECT @TotalSalary = SUM(E.Salary)   
    FROM EmpDemo E  
    WHERE E.DeptNo = @DeptNo    
    RETURN @TotalSalary;  
END; 
GO



/* Q5: Write a user defined function to get department and total salary */
CREATE FUNCTION TotalSalaryTable (@DeptNo int)  
RETURNS TABLE  
AS  
RETURN
(   
    SELECT E.DeptNo, SUM(E.Salary) AS 'TotalSalary'
    FROM EmpDemo E  
    WHERE E.DeptNo = @DeptNo  
	GROUP BY E.DeptNo		 
  );
GO 

/* Q7: Create a view for EmpDemo table and try to Insert records into the view */
CREATE VIEW EmpTV 
AS
SELECT Ename, Salary, DeptNo 
FROM EmpDemo;
GO

INSERT INTO EmpTV(
    Ename, Salary, DeptNo)
VALUES ( 
   'HARD', 
    111111, 
    111100);

SELECT * FROM EmpTV;
