/* 0. Last date Modif:  2019 07 25 
--                      2019 10 15 14:35  (Only change to prod connection)
--  1. Server Name:     LACARGLXAP020
--  2. Database Name:     HR_PROD   -    PSS3
--  3. Schema(s) Name:    BASEARG
--  
--  -----------------------------------------------------------------------------------------------
--  1) Use this Connection
--    HR_PROD = (DESCRIPTION = (ADDRESS_LIST = (ADDRESS =(PROTOCOL = TCP)(HOST = LACARGLXAP020.lac.nsroot.net)(PORT = 2432 ))) (CONNECT_DATA = (  SID = PSS3)))
-- 
--  2) Change current schema
*/
    ALTER SESSION SET CURRENT_SCHEMA = "BASEARG";
-- ----------------------------------------------------------------------------------------------

	GRANT 	DEBUG 		ON 	BASEARG.PKG_BXSCITI_DATOS_CONT 	TO 	BXUSER; 
	GRANT 	EXECUTE 	ON 	BASEARG.PKG_BXSCITI_DATOS_CONT 	TO 	BXUSER; 

--  Solo para tablas: https://stackoverflow.com/questions/34429497/how-to-grant-privileges-on-trigger-and-synonyms-in-oracle-11g
--	ALTER PUBLIC SYNONYM BASEARG.PKG_BXSCITI_DATOS_CONT COMPILE;