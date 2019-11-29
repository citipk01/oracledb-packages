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
create or replace PACKAGE           "PKG_INT_HRMS_VALIDA" AS
/******************************************************************************
 NAME: PKG_INTERFAZ_HRMS_VALIDA
 PURPOSE: Valida la informacion de las tablas de Interfaz y arma la tabla CB_MAE
FUNC_STG

 REVISIONS:
 Ver Date Author Description
 --------- ---------- --------------- ------------------------------------
 1.0 01/06/2015 Carlos Amar 1. Created this package.
 2.0 13/06/2019 Se agregan nuevos campos a la tabla CB_JOB_JR_INT
 ******************************************************************************/
g_cod_int INTEGER;
g_proc_int VARCHAR2(30);
g_err_descr VARCHAR(254);
g_res_log VARCHAR(254);
g_nivel INTEGER := 0;

type g_mf_list IS TABLE OF CB_MAEFUNC_STG%ROWTYPE;

 PROCEDURE Validacion_Interfaz(Cod_int IN INTEGER,Resultado OUT VARCHAR2);
 --PROCEDURE Valida_Tabla_Int (l_tabla IN VARCHAR2 ,l_res OUT VARCHAR2);
/** 0220**/ PROCEDURE Valida_CB_JOB_INT (l_res OUT VARCHAR2);
/** 0300**/ PROCEDURE Valida_CB_JOB_JR_INT (l_res OUT VARCHAR2);
/** 0130**/ PROCEDURE Valida_CB_PERS_DT_INT (l_res OUT VARCHAR2);
/** 0160**/ PROCEDURE Valida_CB_ADDR_INT (l_res OUT VARCHAR2);
/** 0230**/ PROCEDURE Valida_CB_EMPL_INT (l_res OUT VARCHAR2);
/** 0110**/ PROCEDURE Valida_CB_PERS_INT (l_res OUT VARCHAR2);
/** 0120**/ PROCEDURE Valida_CB_NAMES_INT (l_res OUT VARCHAR2);
/** 0140**/ PROCEDURE Valida_CB_PERS_NID_INT (l_res OUT VARCHAR2);


 PROCEDURE Obtener_Tipo_Trans (l_audit_action IN VARCHAR2
                            ,l_tipo_tran OUT VARCHAR2);
 -- EGV 12FEB2016 Inicio
 -- PROCEDURE Armar_Cod_Mf (
 -- l_emplid IN VARCHAR2, l_empl_rcd VARCHAR2, l_cod_mf OUT INTEGER);
 PROCEDURE Armar_Cod_Mf (
         l_emplid IN VARCHAR2, l_empl_rcd VARCHAR2, l_region VARCHAR2
         , l_cod_mf OUT INTEGER);
 -- EGV 12FEB2016 Fin

 -- EGV 01Jul2015 - Inicio
 --PROCEDURE Buscar_Emplid (l_emplid IN VARCHAR2
 --                         , l_empl_rcd VARCHAR2,l_cod_mf OUT INTEGER);
 PROCEDURE Buscar_Emplid (
            l_emplid IN VARCHAR2, l_empl_rcd VARCHAR2, l_region VARCHAR2
            , l_cod_mf OUT INTEGER);

 PROCEDURE TraeRegionDeEmplid(
                l_emplid IN VARCHAR2, l_empl_rcd IN INTEGER
                , l_effdt IN DATE, l_effseq IN INTEGER
                , l_region OUT VARCHAR2);

 --PROCEDURE Buscar_reg_MAEFUNC_STG (
 --         l_cod_int IN INTEGER,l_cod_mf IN INTEGER
 --         ,l_effdt IN DATE ,l_effseq IN INTEGER,l_tipo VARCHAR2
 --         ,l_reg OUT g_mf_list,l_existe OUT INTEGER);
 PROCEDURE Buscar_reg_MAEFUNC_STG (
            l_cod_int IN INTEGER,l_cod_mf IN INTEGER
            ,l_effdt IN DATE ,l_effseq IN INTEGER,l_tipo VARCHAR2
            ,l_emplid VARCHAR2
            ,l_reg OUT g_mf_list,l_existe OUT INTEGER);
 -- EGV 01Jul2015 - Fin

 PROCEDURE Buscar_reg_MAEFUNC_TBL (
            l_cod_int IN INTEGER,l_cod_mf IN INTEGER
            ,l_effdt IN DATE ,l_effseq IN INTEGER,l_tipo VARCHAR2
            ,l_reg OUT g_mf_list,l_existe OUT INTEGER);

 PROCEDURE Impacta_reg_MAEFUNC_STG(
         l_accion IN VARCHAR2,l_reg IN g_mf_list
         ,l_res OUT VARCHAR2);

 PROCEDURE Valida_reg_MAEFUNC_STG (
         l_cod_int IN INTEGER,l_res OUT VARCHAR2);

-- EGV 22SEP2015 Inicio
 FUNCTION CalculaNuevaAccion(
         l_action VARCHAR2, l_action_reason VARCHAR2
         , l_old_action VARCHAR2, l_emplid VARCHAR2
         , l_empl_rcd INTEGER, l_effdt DATE
         , l_effseq INTEGER) RETURN VARCHAR2;
            --QUITO IMG 21.08.2019
-- FUNCTION TraeCuotaClub(l_grado VARCHAR2) RETURN CHAR;
-- EGV 22SEP2015 Fin

-- egv 05oct2015 Inicio
--QUITO IMG 21.08.2019
--FUNCTION TraeCatCont(l_grado VARCHAR2) RETURN CHAR;
FUNCTION TraeBanking(l_cencos varchar2, l_emp varchar2) RETURN CHAR;
-- egv 05oct2015 Fin

END PKG_INT_HRMS_VALIDA;