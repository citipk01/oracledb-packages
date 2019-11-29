-- 0. Last date Modif:  2019 08 15 - 11:20
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
CREATE OR REPLACE PACKAGE         PKG_BXSCITI_DATOS_CONT
AS

  /****************************************************************************
   NAME: BASEARG.PKG_BXSCITI_DATOS_CONT
   PURPOSE:
  
   REVISIONS:
   Ver Date Author Description
   --------- ---------- --------------- ------------------------------------
   1.0    30/04/2015 CSA 1.  Se procesan los datos contables de una liquidacion
   1.1    15/06/2015 EGV Correcciones Varias
   1.2    2019/07/16 10:12 : Obtener sucursal del Centro de Costos
   1.2.1  2019/08/15 11:20 : Corrección de espaciado en banking
  ****************************************************************************/

   g_id_proc        INTEGER;
   g_proc_int       VARCHAR (100);
   g_descr_det      VARCHAR2 (100);
   g_estado         VARCHAR2 (2);
   g_estado_det     VARCHAR2 (254);
   g_resultado      VARCHAR2 (50);
   g_errores        INTEGER := 0;

   g_cod_mf         number(10) := 0;
   g_banking_mf     BASEARG.CB_CENCOS.CBHR_BANKING%TYPE;-- VARCHAR2 (4);
   g_cat_cont_mf    VARCHAR2 (2);
   g_secc_mf        VARCHAR2 (5);
   g_cta_mf         VARCHAR2 (8);

   g_cod_lq         INTEGER;
   g_desc_lq        VARCHAR2 (300);
   g_emp_lq         INTEGER;
   g_company_lq     VARCHAR2 (5);
   g_emp_decr       VARCHAR2 (300);
   g_pais_lq        VARCHAR2 (5);
   g_mes_lq         VARCHAR2 (2);
   g_fhas_lq        DATE;   -- 1.1  15/06/2015 EGV

   TYPE g_cb_asiento_list IS TABLE OF CB_ASIENTO%ROWTYPE;

   g_cb_asiento_t   g_cb_asiento_list;


   PROCEDURE datos_contabilidad (p_cod_lq        IN     INTEGER,
                                 p_rep_errores   IN     VARCHAR2,
                                 p_legajos       IN     VARCHAR2,
                                 p_id_proc       IN     INTEGER,
                                 p_resultado        OUT VARCHAR2);

   PROCEDURE Verificar_Balanceo;

   PROCEDURE Buscar_Datos_IMP_CONT (c_cod_mv      IN MOVHD.COD_MV%TYPE,
                                    c_importe     IN MOVHD.IMPTOT_HD%TYPE,
                                    c_cencos_hd   IN MOVHD.CENCOS_HD%TYPE);
END PKG_BXSCITI_DATOS_CONT; 
/

