/* 0. Last date Modif:  2019 08 22 - 12:00
-- 1. Server Name:      LACARGLXAP019
-- 2. Database Name:    HRTEST_11G
-- 3. Schema(s) Name:   BASEARG
-- 
-- ----------------------------------------------------------------------------------------------
-- 1) Use this Connection
--   HR_UAT = (DESCRIPTION = (ADDRESS = (PROTOCOL = TCP)(HOST = LACARGLXAP019)(PORT = 2432)) (CONNECT_DATA = (SID = HRTEST)) )
-- 
-- 2) Change current schema
*/
    ALTER SESSION SET CURRENT_SCHEMA = "BASEARG";
-- ----------------------------------------------------------------------------------------------

	DROP PACKAGE BASEARG.PKG_INT_HRMS_VALIDA;
