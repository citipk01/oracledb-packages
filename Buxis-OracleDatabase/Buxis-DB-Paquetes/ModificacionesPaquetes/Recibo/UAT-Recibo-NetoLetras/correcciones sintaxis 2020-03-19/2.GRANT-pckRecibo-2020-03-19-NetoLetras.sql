/*	0. Last Date Modified:	2020 03 19	12:35
--	1. Server Name: 		• UAT: LACARGLXAP019		• Production: LACARGLXAP020
--	2. Database Name: 		• UAT: HRTEST_11G			• Production: HR_PROD   -    PSS3
--	3. Schema(s) Name:		BASEARG
--	
--	-----------------------------------------------------------------------------------------------
--	1) Use this Connection
--		HR_PROD = (DESCRIPTION = (ADDRESS_LIST = (ADDRESS =(PROTOCOL = TCP)(HOST = LACARGLXAP020.lac.nsroot.net)(PORT = 2432 ))) (CONNECT_DATA = (	SID = PSS3)))
--		HR_UAT 	= (DESCRIPTION = (ADDRESS = (PROTOCOL = TCP)(HOST = LACARGLXAP019)(PORT = 2432)) (CONNECT_DATA = (SID = HRTEST)) )
*/

/* 2) Change current schema ********************************************************************************************************************************** */

    ALTER SESSION SET CURRENT_SCHEMA = "BASEARG";
-- ----------------------------------------------------------------------------------------------

	GRANT 	DEBUG 		ON 	BASEARG.PKG_BXSCITI_DATOS_RECIBO 	TO 	BXUSER; 
	GRANT 	EXECUTE 	ON 	BASEARG.PKG_BXSCITI_DATOS_RECIBO 	TO 	BXUSER; 

--  Solo para tablas: https://stackoverflow.com/questions/34429497/how-to-grant-privileges-on-trigger-and-synonyms-in-oracle-11g
--	ALTER PUBLIC SYNONYM BASEARG.PKG_BXSCITI_DATOS_CONT COMPILE;