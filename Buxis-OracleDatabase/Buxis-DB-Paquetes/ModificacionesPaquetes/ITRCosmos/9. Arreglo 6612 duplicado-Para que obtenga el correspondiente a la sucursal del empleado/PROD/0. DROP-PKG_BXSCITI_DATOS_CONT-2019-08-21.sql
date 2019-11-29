/* 0. Last date Modif:  2019 07 25 
--						2019 10 15 14:35 	(Only change to prod connection)
--	1. Server Name: 		LACARGLXAP020
--	2. Database Name: 		HR_PROD   -    PSS3
--	3. Schema(s) Name:		BASEARG
--	
--	-----------------------------------------------------------------------------------------------
--	1) Use this Connection
--		HR_PROD = (DESCRIPTION = (ADDRESS_LIST = (ADDRESS =(PROTOCOL = TCP)(HOST = LACARGLXAP020.lac.nsroot.net)(PORT = 2432 ))) (CONNECT_DATA = (	SID = PSS3)))
-- 
--	2) Change current schema
*/
    ALTER SESSION SET CURRENT_SCHEMA = "BASEARG";
-- ----------------------------------------------------------------------------------------------

	DROP PACKAGE BASEARG.PKG_BXSCITI_DATOS_CONT;
