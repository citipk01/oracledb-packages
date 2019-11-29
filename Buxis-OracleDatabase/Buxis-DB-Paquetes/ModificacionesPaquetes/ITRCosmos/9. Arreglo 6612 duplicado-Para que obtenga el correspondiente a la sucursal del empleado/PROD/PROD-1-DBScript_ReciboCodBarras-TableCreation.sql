/* 0. Last date Modif:  2019 10 21 
--                      2019 10 31 17:05  (Only change to prod connection)
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
--	3) Execute Script

/* 3) Create "cb_devolucion_recibos" table  **************************************************************************************************** */
		CREATE TABLE BASEARG.CB_DEVOLUCION_RECIBOS
		(
		  ID          NUMBER(10)                        NOT NULL,
		  COD_LQ      NUMBER(10)                        NOT NULL,
		  COD_MF      NUMBER(10)                        NOT NULL,
		  QS_A_FECHA  DATE                              NOT NULL,
		  FIRMA       CHAR(2 BYTE),
		  HORA        CHAR(5 BYTE),
		  INGRESO     CHAR(1 BYTE),
		  USUARIO     CHAR(20 BYTE)
		)
		/
		
	    CREATE UNIQUE INDEX "BASEARG"."CB_DEVOLUCION_RECIBOS" ON "BASEARG"."CB_DEVOLUCION_RECIBOS" ("ID") 
	    PCTFREE 10 INITRANS 2 MAXTRANS 255 COMPUTE STATISTICS 
	    STORAGE(INITIAL 262144 NEXT 262144 MINEXTENTS 1 MAXEXTENTS 2147483645
	            PCTINCREASE 0 FREELISTS 1 FREELIST GROUPS 1
	            BUFFER_POOL DEFAULT FLASH_CACHE DEFAULT CELL_FLASH_CACHE DEFAULT
	            )
	    TABLESPACE "QSRRHH" ;
	    /



		CREATE SEQUENCE  "BASEARG"."SEQ_CB_DEVOLUCION_RECIBOS_ID"  MINVALUE 1 MAXVALUE 99999999999999999999 
		INCREMENT BY 1 START WITH 1 CACHE 20 ORDER  NOCYCLE ;
		/

--	4) Grants/Synonym to BXUSER
		-- Grants
		GRANT select, insert, update, delete, alter ON "BASEARG"."CB_DEVOLUCION_RECIBOS"   			TO   BXUSER;  


		-- Syonyms
		CREATE OR REPLACE PUBLIC SYNONYM "CB_DEVOLUCION_RECIBOS"   FOR "BASEARG"."CB_DEVOLUCION_RECIBOS";       	
		CREATE OR REPLACE PUBLIC SYNONYM "SEQ_CB_DEVOLUCION_RECIBOS_ID" FOR "BASEARG"."SEQ_CB_DEVOLUCION_RECIBOS_ID";  



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