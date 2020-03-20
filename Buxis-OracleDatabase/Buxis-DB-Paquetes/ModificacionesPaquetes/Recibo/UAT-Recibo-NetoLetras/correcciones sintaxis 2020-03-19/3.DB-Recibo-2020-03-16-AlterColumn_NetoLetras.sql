/*	0. Last Date Modified:	2020 03 16	11:12
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

    --DROP INDEX "BASEARG"."CB_VAC_SOL_WEB";
/* 3) Run Script */

	ALTER TABLE "BASEARG"."CB_REC_MF"
	MODIFY NETO_LETRAS VARCHAR2 (125);


--	4) Grants/Synonym to BXUSER
		-- Grants
		-- GRANT select, insert, update, delete, alter ON "BASEARG"."CB_VAC_SOL_WEB"   		TO   BXUSER;  
		-- GRANT select								ON "BASEARG"."SEQ_CB_VAC_SOL_WEB_ID"   	TO   BXUSER;  
		-- GRANT CREATE ANY SEQUENCE, ALTER ANY SEQUENCE, DROP ANY SEQUENCE, SELECT ANY SEQUENCE TO BXUSER;

		-- Syonyms
		-- CREATE OR REPLACE PUBLIC SYNONYM "CB_VAC_SOL_WEB"   FOR "BASEARG"."CB_VAC_SOL_WEB";       		
		-- CREATE OR REPLACE PUBLIC SYNONYM "SEQ_CB_VAC_SOL_WEB_ID" FOR "BASEARG"."SEQ_CB_VAC_SOL_WEB_ID";   
		
--	5) If all instructions have ran successfully:
--		* Save changes:
--		
		COMMIT;	
		/
--	
--		* In case of failing:
--			ROLLBACK TO backup_db_before_ran_script;
--	
--	
--	
--	Thanks & Regards