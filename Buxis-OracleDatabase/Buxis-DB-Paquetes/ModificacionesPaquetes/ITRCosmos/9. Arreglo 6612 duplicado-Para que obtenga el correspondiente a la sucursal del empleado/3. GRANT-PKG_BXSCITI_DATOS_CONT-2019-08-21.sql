
-- 0. Last date Modif:  2019 07 25 
-- 1. Server Name:      LACARGLXAP019
-- 2. Database Name:    HRTEST_11G
-- 3. Schema(s) Name:   BASEARG
-- 
-- ----------------------------------------------------------------------------------------------
-- 1) Use this Connection
--   HR_UAT = (DESCRIPTION = (ADDRESS = (PROTOCOL = TCP)(HOST = LACARGLXAP019)(PORT = 2432)) (CONNECT_DATA = (SID = HRTEST)) )
-- 
-- 2) Change current schema
    ALTER SESSION SET CURRENT_SCHEMA = "BASEARG";
-- ----------------------------------------------------------------------------------------------

	GRANT 	DEBUG 		ON 	BASEARG.PKG_BXSCITI_DATOS_CONT 	TO 	BXUSER; 
	GRANT 	EXECUTE 	ON 	BASEARG.PKG_BXSCITI_DATOS_CONT 	TO 	BXUSER; 

--  Solo para tablas: https://stackoverflow.com/questions/34429497/how-to-grant-privileges-on-trigger-and-synonyms-in-oracle-11g
--	ALTER PUBLIC SYNONYM BASEARG.PKG_BXSCITI_DATOS_CONT COMPILE;