-- ========================================================
-- Copyright � 2002 Sun Microsystems, Inc. All rights reserved.
-- ========================================================

drop table coffee;

drop SPECIFIC PROCEDURE COUNTCOFFEE RESTRICT;
drop SPECIFIC PROCEDURE INSERTCOFFEE RESTRICT;

create table coffee (name varchar(32), qty integer);

create procedure COUNTCOFFEE (OUT n INT)
LANGUAGE JAVA
SPECIFIC COUNTCOFFEE
READS SQL DATA
EXTERNAL NAME "SampleExternalMethods::countCoffee"
PARAMETER STYLE SQL;


create procedure INSERTCOFFEE (IN name VARCHAR, IN qty INT)
LANGUAGE JAVA
SPECIFIC INSERTCOFFEE
MODIFIES SQL DATA
EXTERNAL NAME "SampleExternalMethods::insertCoffee"
PARAMETER STYLE SQL;

