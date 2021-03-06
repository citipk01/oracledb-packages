﻿/*
-- 0. Last date Modif:  2019 10 31  13:49
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
/*-------------------------------------------------------------------------------------------------
  -- CREATE OR REPLACE PACKAGE         PKG_BXSCITI_DATOS_CONT
  -- AS
  --    /****************************************************************************
  --     NAME: BASEARG.PKG_BXSCITI_DATOS_CONT
  --     PURPOSE:
  -- 
  --     REVISIONS:
  --     Ver Date Author Description
  --     --------- ---------- --------------- ------------------------------------
  --     1.0  30/04/2015 CSA 1.  Se procesan los datos contables de una liquidacion
  --     1.1  15/06/2015 EGV Correcciones Varias
  --     1.2  2019/07/16 10:12 : Obtener sucursal del Centro de Costos
  --    ****************************************************************************/
/*
  -- 
  -- 
  --    g_id_proc        INTEGER;
  --    g_proc_int       VARCHAR (100);
  --    g_descr_det      VARCHAR2 (100);
  --    g_estado         VARCHAR2 (2);
  --    g_estado_det     VARCHAR2 (254);
  --    g_resultado      VARCHAR2 (50);
  --    g_errores        INTEGER := 0;
  -- 
  --    g_cod_mf         number(10) := 0;
  --    g_banking_mf     BASEARG.CB_CENCOS.CBHR_BANKING%TYPE;-- VARCHAR2 (4);
  --    g_cat_cont_mf    VARCHAR2 (2);
  --    g_secc_mf        VARCHAR2 (5);
  --    g_cta_mf         VARCHAR2 (8);
  -- 
  --    g_cod_lq         INTEGER;
  --    g_desc_lq        VARCHAR2 (300);
  --    g_emp_lq         INTEGER;
  --    g_company_lq     VARCHAR2 (5);
  --    g_emp_decr       VARCHAR2 (300);
  --    g_pais_lq        VARCHAR2 (5);
  --    g_mes_lq         VARCHAR2 (2);
  --    g_fhas_lq        DATE;   -- 1.1  15/06/2015 EGV
  -- 
  --    TYPE g_cb_asiento_list IS TABLE OF CB_ASIENTO%ROWTYPE;
  -- 
  --    g_cb_asiento_t   g_cb_asiento_list;
  -- 
  -- 
  --    PROCEDURE datos_contabilidad (p_cod_lq        IN     INTEGER,
  --                                  p_rep_errores   IN     VARCHAR2,
  --                                  p_legajos       IN     VARCHAR2,
  --                                  p_id_proc       IN     INTEGER,
  --                                  p_resultado        OUT VARCHAR2);
  -- 
  --    PROCEDURE Verificar_Balanceo;
  -- 
  --    PROCEDURE Buscar_Datos_IMP_CONT (c_cod_mv      IN MOVHD.COD_MV%TYPE,
  --                                     c_importe     IN MOVHD.IMPTOT_HD%TYPE,
  --                                     c_cencos_hd   IN MOVHD.CENCOS_HD%TYPE);
  -- END PKG_BXSCITI_DATOS_CONT; 
*/



create or replace PACKAGE BODY         PKG_BXSCITI_DATOS_CONT
AS
   /****************************************************************************
    NAME: BASEARG.PKG_BXSCITI_DATOS_CONT
    PURPOSE:

    REVISIONS:
    Ver Date Author Description
    --------- ---------- --------------- ------------------------------------
    1.0  30/04/2015 CSA 1.  Se procesan los datos contables de una liquidacion
    1.1  15/06/2015 EGV Correcciones Varias
   ****************************************************************************/

   PROCEDURE datos_contabilidad (p_cod_lq        IN     INTEGER,
                                 p_rep_errores   IN     VARCHAR2,
                                 p_legajos       IN     VARCHAR2,
                                 p_id_proc       IN     INTEGER,
                                 p_resultado        OUT VARCHAR2)
   IS
      l_cod_proc      VARCHAR2 (50) := 'DATOS CONT';
      l_descr         VARCHAR2 (50) := 'Proceso de datos de contabilidad';

      l_temp          INTEGER := 0;
      l_cab           VARCHAR2 (2) := 'N';

      -- EGV 15Jun2015 1.1 - Inicio
      l_fhas_lq       DATE;
      -- EGV 15Jun2015 1.1 - Fin
        -- 2019 08 23 - Repetidos
        count_cb_asiento  INTEGER := 0;

      l_datos_mf      MAEFUNC_TBL%ROWTYPE;

      l_legajos_tbl   var_array;

      TYPE l_cod_mf_list IS TABLE OF INTEGER;

      TYPE l_empl_rcdn_list IS TABLE OF INTEGER;

      TYPE l_cod_mv_list IS TABLE OF INTEGER;

      TYPE l_cencos_hd_list IS TABLE OF VARCHAR (7);

      TYPE l_imptot_hd_list IS TABLE OF NUMBER;

      l_cod_mf_t      l_cod_mf_list;
      l_empl_rcdn_t   l_empl_rcdn_list;
      l_cod_mv_t      l_cod_mv_list;
      l_cencos_hd_t   l_cencos_hd_list;
      l_imptot_hd_t   l_imptot_hd_list;


      ERRORES_CONT    EXCEPTION;
      CUSTOM          EXCEPTION;
      NEXT_VAL        EXCEPTION;



      CURSOR CUR_MOVHD (
         -- EGV 15Jun2015 1.1 - Inicio
         --c_legajos_tbl var_array)
         c_legajos_tbl var_array, p_fhas date)
         -- EGV 15Jun2015 1.1 - Inicio
      IS
           --> Se obtienen datos basicos de la MOVHD

           SELECT MOV.COD_MF,
                  NVL (MOV.EMPL_RCDN, 0) EMPL_RCDN,
                  MOV.COD_MV,
                  MAX(MOV.CENCOS_HD) as CENCOS_HD,
                  SUM (MOV.IMPTOT_HD) AS IMPTOT_HD
             FROM BASEARG.MOVHD MOV
            WHERE MOV.COD_LQ = p_cod_lq
                  -- EGV 01Dic2015 Inicio
                  --AND MOV.COD_MF IN (SELECT * FROM TABLE (c_legajos_tbl))
                  AND to_char(MOV.COD_MF)
                        IN (SELECT * FROM TABLE (c_legajos_tbl))
                  -- EGV 01Dic2015 Fin
                  AND EXISTS
                         (SELECT 1
                            -- EGV 15Jun2015 1.1 - Inicio
                            --FROM BASEARG.ACUMULAN A
                            --WHERE A.COD_MV = MOV.COD_MV
                            -- AND A.ACUMULADOR = 'con')
                            FROM BASEARG.ACUMULAN_TBL A
                            WHERE A.COD_MV = MOV.COD_MV AND A.ACUMULADOR = 'con'
                            AND A.FECHA_EFECTIVA= (SELECT MAX(AE.FECHA_EFECTIVA)
                                      FROM BASEARG.ACUMULAN_TBL AE
                                      WHERE A.ACUMULADOR = AE.ACUMULADOR
                                      AND AE.FECHA_EFECTIVA <= p_fhas)
                            AND A.BAJA_EFECTIVA IS NULL)
                            -- EGV 15Jun2015 1.1  - Fin
         GROUP BY MOV.COD_MF,
                  NVL (MOV.EMPL_RCDN, 0),
                  MOV.COD_MV;
   /***** PROCESO PRINCIPAL *****/
   BEGIN
      g_id_proc := p_id_proc;
      g_cod_mf := 0;
      g_errores := 0;

      --> Se loguea el inicio del proceso y devuelve el Id Proc

      BEGIN
         g_estado := 'P';

         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('I',
                                                        g_id_proc,
                                                        l_cod_proc,
                                                        l_descr,
                                                        g_estado,
                                                        g_resultado);
      END;


      -->Busco la Empresa y el Pais de la liquidacion.

      BEGIN
         SELECT L.COD_LQ,
                L.DESC_LQ,
                L.COD_EMP,
                E.NOM_EMP_L,
                E.REGION,
                E.COMPANY,
                L.FHAS_LQ,    -- EGV 15Jun2015 1.1
                TO_CHAR (L.FHAS_LQ, 'MM')
           INTO g_cod_lq,
                g_desc_lq,
                g_emp_lq,
                g_emp_decr,
                g_pais_lq,
                g_company_lq,
                g_fhas_lq,    -- EGV 15Jun2015 1.1
                g_mes_lq
           FROM BASEARG.LIQUIDAC L, BASEARG.QSEMPRESA E
          WHERE L.COD_EMP = E.COD_EMP AND L.COD_LQ = p_cod_lq;
      EXCEPTION
         WHEN OTHERS
         THEN
            p_resultado :=
               'Ha ocurrido un error obteniendo
               los datos para el codigo de liquidacion '
               || p_cod_lq
               || '.';

            RAISE CUSTOM;
      END;


      --> Busco los empleados involucrados 
      -- dependiendo de los parametros ingresados



      CASE p_rep_errores
         WHEN 'T'
         THEN                     --> Se procesa/reprocesa toda la liquidacion
            SELECT DISTINCT M.COD_MF
              BULK COLLECT INTO l_legajos_tbl
              FROM MOVHD M
             WHERE m.cod_lq = g_cod_lq;
         WHEN 'E'
         THEN                      --> Se reprocesan los registros con errores
            SELECT DISTINCT A.COD_MF
              BULK COLLECT INTO l_legajos_tbl
              FROM BASEARG.CB_ASIENTO A
             WHERE A.COD_LQ = g_cod_lq AND A.PROCESADO <> 'S';
         WHEN 'C'
         THEN --> Se procesa/reprocesa los legajos que
                -- vienen en el parametro p_legajos
            l_legajos_tbl :=
               PKG_BXSCITI_GENERAL.convertir_lista (p_legajos, ';');
         ELSE
            p_resultado :=
                  'El tipo de proceso/reproceso '
               || p_rep_errores
               || ' no es valido.';

            RAISE CUSTOM;
      END CASE;

      --> Se verifica que haya empleados para procesar.

      IF l_legajos_tbl.COUNT () = 0
      THEN
         p_resultado :=
            'No se han encontrado empleados para los parametros ingresados ';
         RAISE CUSTOM;
      END IF;

      --> Se inserta la cabecera

      BEGIN
         SELECT COUNT (*)
           INTO l_temp
           FROM BASEARG.CB_ASIENTO_CAB
          WHERE COD_LQ = g_cod_lq;



         IF l_temp = 0
         THEN
            INSERT INTO BASEARG.CB_ASIENTO_CAB (COD_LQ, PROCESADO)
                 VALUES (g_cod_lq, 'N');
         ELSE
            UPDATE BASEARG.CB_ASIENTO_CAB
               SET PROCESADO = 'N'
             WHERE COD_LQ = g_cod_lq;
         END IF;

         l_cab := 'Y';
      EXCEPTION
         WHEN OTHERS
         THEN
            p_resultado :=
               'Error procesando la cabecera de
               los asientos para la liquidacion : '
               || g_cod_lq;

            RAISE CUSTOM;
      END;


      -->Comienza el proceso

      g_proc_int := 'Procesa Asientos';

      BEGIN
         --> Se deben borrar los registros de la CB_ASIENTO corrsepondientes
         --> los empleados procesados y los asientos de ajuste.


        -- EGV 01Dic2015 Inicio
        IF p_rep_errores = 'T' THEN
            DELETE FROM BASEARG.CB_ASIENTO A
                   WHERE A.COD_LQ = p_cod_lq;
        ELSE
        -- EGV 01Dic2015 Fin
             DELETE FROM BASEARG.CB_ASIENTO A
                   WHERE A.COD_LQ = p_cod_lq
                         -- EGV 01Dic2015 Inicio
                         -- AND A.COD_MF IN (
                            -- SELECT * FROM TABLE (l_legajos_tbl));
                         AND to_char(A.COD_MF)
                                IN (SELECT * FROM TABLE (l_legajos_tbl));
                         -- EGV 01Dic2015 Fin


             DELETE FROM BASEARG.CB_ASIENTO A
                   WHERE A.COD_LQ = p_cod_lq
                   AND A.CUENTA LIKE 'AJUSTE00099999%';
        -- EGV 01Dic2015 Inicio
        END IF;
        -- EGV 01Dic2015 Fin
      EXCEPTION
         WHEN OTHERS
         THEN
            p_resultado :=
                  'Error eliminando los asientos para la liquidacion : '
               || g_cod_lq
               || CHR (13)
               || 'Error : '
               || SQLCODE
               || ' -- '
               || SQLERRM;

            RAISE CUSTOM;
      END;


      --> Inicializo la tabla que contiene los asientos

      g_cb_asiento_t := g_cb_asiento_list ();



      --> Recorro los conceptos de MOVHD

       -- EGV 15Jun2015 1.1 - Inicio
      --OPEN CUR_MOVHD (l_legajos_tbl);
      OPEN CUR_MOVHD (l_legajos_tbl, g_fhas_lq);
       -- EGV 15Jun2015 1.1 - Fin



      LOOP
         FETCH CUR_MOVHD
         BULK COLLECT INTO l_cod_mf_t,
              l_empl_rcdn_t,
              l_cod_mv_t,
              l_cencos_hd_t,
              l_imptot_hd_t
         LIMIT 50;



         EXIT WHEN l_cod_mf_t.COUNT = 0;

         FOR i IN l_cod_mf_t.FIRST .. l_cod_mf_t.LAST
         LOOP
            BEGIN
               g_estado_det := 'C';

               g_descr_det := ' ';

               --> Busco los datos del empleado cuando viene uno nuevo

               IF g_cod_mf = 0 OR g_cod_mf <> l_cod_mf_t (i)
               THEN
                  g_proc_int := 'Datos MAEFUNC';


                  BASEARG.PKG_BXSCITI_GENERAL.
                   Obtener_Datos_MAEFUNC (l_cod_mf_t (i),
                                          g_fhas_lq,   -- EGV 1.1 15/06/2015
                                          g_estado_det,
                                          l_datos_mf);


                  IF g_estado_det <> 'E'  THEN

                  g_cod_mf := l_datos_mf.cod_mf;
                  -- EGV 15/06/2015 1.1 - Inicio
                  --g_banking_mf := l_datos_mf.banking;
                  --g_cat_cont_mf := l_datos_mf.cat_cont;
                  --g_secc_mf := l_datos_mf.secc_mf;
                  --g_cta_mf := l_datos_mf.cr_acct_resv;
                  g_banking_mf := rtrim(l_datos_mf.banking,' ');
                  g_cat_cont_mf := rtrim(l_datos_mf.cat_cont,' ');
                  g_secc_mf := rtrim(l_datos_mf.secc_mf,' ');
                  g_cta_mf := rtrim(l_datos_mf.cr_acct_resv,' ');
                  -- EGV 15/06/2015 1.1 - Fin

                  ELSE

                     g_descr_det :=
                        'Error obteniendo los datos del empleado  : '
                        || l_cod_mf_t (i);

                     RAISE NEXT_VAL;
                  END IF;
               END IF;

               g_proc_int := 'Datos Imp Contable';

               Buscar_Datos_IMP_CONT (l_cod_mv_t (i),
                                      l_imptot_hd_t (i),
                                      l_cencos_hd_t (i));


               IF g_estado_det = 'E'
               THEN
                  g_descr_det :=
                     'Existen datos inconsistentes en la
                        imputacion contable para el concepto : '
                     || l_cod_mv_t (i)
                     || ' empleado : '
                     || l_cod_mf_t (i);

                  RAISE NEXT_VAL;
               END IF;
            EXCEPTION
               WHEN NEXT_VAL
               THEN
                  g_errores := g_errores + 1;

                  g_resultado := NULL;

                  PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                      g_proc_int,
                                                      g_estado_det,
                                                      g_descr_det,
                                                      g_resultado);
               WHEN OTHERS
               THEN
                  g_errores := g_errores + 1;

                  g_estado_det := 'E';


                   -- EGV 09Dic2015 Inicio
                  /*g_descr_det :=
                        'Error desconocido procesando el Legajo : '
                     || l_cod_mf_t (i)
                     || ' Concepto : '
                     || l_cod_mv_t (i);*/
                   /*g_descr_det := 'Error con Legajo: ' || l_cod_mf_t (i)
                   || ' Concepto: '  || l_cod_mv_t(i) || ' '
                   || substr(replace(replace(sqlerrm,chr(10),'-')
                   ,chr(13),'-'),1,100);*/
                   g_descr_det := 'Error con Legajo: ' || l_cod_mf_t (i)
                                    || ' Concepto: ' || l_cod_mv_t(i)
                                    || ' Error:' || sqlcode;
                   -- EGV 09Dic2015 Fin

                  g_resultado := NULL;

                  PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                      g_proc_int,
                                                      g_estado_det,
                                                      g_descr_det,
                                                      g_resultado);
            END;
         END LOOP;
      END LOOP;

    -- 2019 08 23 - Repetidos
      -- FORALL X IN g_cb_asiento_t.FIRST .. g_cb_asiento_t.LAST  

        FOR X IN g_cb_asiento_t.FIRST .. g_cb_asiento_t.LAST 
        LOOP
            SELECT COUNT(*)
            INTO count_cb_asiento
            FROM CB_ASIENTO
            WHERE  COD_MV = g_cb_asiento_t (x).cod_mv
            AND COD_MF = g_cb_asiento_t (x).cod_mf
            AND CUENTA = g_cb_asiento_t (x).cuenta
            AND COD_LQ = g_cb_asiento_t (x).cod_lq
            AND COD_SUC = g_cb_asiento_t (x).COD_SUC
            AND CENCOS_MF = g_cb_asiento_t (x).CENCOS_MF;

            IF count_cb_asiento > 0 THEN
                UPDATE CB_ASIENTO 
                SET MONTO_DEB = MONTO_DEB + g_cb_asiento_t (x).MONTO_DEB
                    ,MONTO_CRED = MONTO_CRED + g_cb_asiento_t (x).MONTO_CRED
                
                WHERE  COD_MV = g_cb_asiento_t (x).cod_mv
                AND COD_MF = g_cb_asiento_t (x).cod_mf
                AND CUENTA = g_cb_asiento_t (x).cuenta
                AND COD_LQ = g_cb_asiento_t (x).cod_lq
                AND COD_SUC = g_cb_asiento_t (x).COD_SUC
                AND CENCOS_MF = g_cb_asiento_t (x).CENCOS_MF
                ;
            ELSE   
                INSERT INTO CB_ASIENTO VALUES g_cb_asiento_t (x);

            END IF;
        END LOOP;

      Verificar_Balanceo;



      IF g_errores <> 0
      THEN
         RAISE ERRORES_CONT;
      ELSE
         BEGIN
            g_estado := 'C';

            PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                                g_id_proc,
                                                l_cod_proc,
                                                l_descr,
                                                g_estado,
                                                g_resultado);



            UPDATE BASEARG.CB_ASIENTO_CAB
               SET PROCESADO = 'S'
             WHERE COD_LQ = p_cod_lq;
         EXCEPTION
            WHEN OTHERS
            THEN
               NULL;
         END;
      END IF;



      p_resultado :=
         'Procesamiento de datos contables finalizado correctamente';
   EXCEPTION
      WHEN ERRORES_CONT
      THEN
         g_estado := 'E';

         PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                             g_id_proc,
                                             l_cod_proc,
                                             l_descr,
                                             g_estado,
                                             g_resultado);



         UPDATE BASEARG.CB_ASIENTO_CAB
            SET PROCESADO = 'N'
          WHERE COD_LQ = p_cod_lq;



         p_resultado :=
               'Procesamiento de datos contables finalizado con '
            || TO_CHAR (g_errores)
            || ' errores';
      WHEN CUSTOM
      THEN
         g_estado := 'E';

         PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                             g_id_proc,
                                             l_cod_proc,
                                             l_descr,
                                             g_estado,
                                             g_resultado);



         IF l_cab = 'Y'
         THEN
            UPDATE BASEARG.CB_ASIENTO_CAB
               SET PROCESADO = 'N'
             WHERE COD_LQ = p_cod_lq;
         END IF;
      WHEN OTHERS
      THEN
         g_estado := 'E';

         PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                             g_id_proc,
                                             l_cod_proc,
                                             l_descr,
                                             g_estado,
                                             g_resultado);



         p_resultado :=
               'Se ha producido un error procesando los datos contables'
            || CHR (13)
            || 'Error : '
            || SQLCODE
            || ' -- '
            || SQLERRM;
   END;

   PROCEDURE Buscar_Datos_IMP_CONT (c_cod_mv      IN MOVHD.COD_MV%TYPE,
                                    c_importe     IN MOVHD.IMPTOT_HD%TYPE,
                                    c_cencos_hd   IN MOVHD.CENCOS_HD%TYPE)
   IS
      c_index            INTEGER;
      c_index_2          INTEGER;

      c_procesado        VARCHAR2 (1) := ' ';

      TYPE c_cuenta_imput_list IS TABLE OF VARCHAR2 (25);

      TYPE c_deb_cred_list IS TABLE OF VARCHAR2 (1);

      TYPE c_cencos_imput_list IS TABLE OF VARCHAR2 (7);

      TYPE c_suc_imput_list IS TABLE OF VARCHAR2 (5);

      TYPE c_signo_list IS TABLE OF VARCHAR2 (1);

      c_cuenta_imput_t   c_cuenta_imput_list;
      c_deb_cred_t       c_deb_cred_list;
      c_cencos_imput_t   c_cencos_imput_list;
      c_suc_imput_t      c_suc_imput_list;
      c_signo_t          c_signo_list;

      c_sucursal         CB_IMP_CONT.COD_SUC%TYPE;
   BEGIN
      c_cuenta_imput_t := c_cuenta_imput_list ();
      c_deb_cred_t := c_deb_cred_list ();
      c_cencos_imput_t := c_cencos_imput_list ();
      c_suc_imput_t := c_suc_imput_list ();
      c_signo_t := c_signo_list ();

      c_index := g_cb_asiento_t.COUNT;
      c_index_2 := c_cuenta_imput_t.COUNT;



      --> Si El Concepto Procesado Es Falla De Caja


      IF g_pais_lq = 'ARG' AND c_cod_mv IN ('6610', '6617')
      THEN
         --> Obtener CuentaCajero. 
         -- Si no tiene cuenta no se procesa el registro

         IF g_cta_mf IS NULL
         THEN
            --> Logueo la falta de cuenta del cajero.

            g_estado_det := 'W';

            g_descr_det :=
                  'Concepto '
               || c_cod_mv
               || ' - El Cajero '
               || g_cod_mf
               || ' No tiene cargada la cuenta.';

            g_resultado := NULL;

            BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                           g_proc_int,
                                                           g_estado_det,
                                                           g_descr_det,
                                                           g_resultado);
         ELSE
            c_cuenta_imput_t.EXTEND;
            c_deb_cred_t.EXTEND;
            c_cencos_imput_t.EXTEND;
            c_suc_imput_t.EXTEND;
            c_signo_t.EXTEND;

            c_index_2 := c_index_2 + 1;

            c_cuenta_imput_t (c_index_2) := g_cta_mf;

            c_suc_imput_t (c_index_2) := 'XXX';

            c_signo_t (c_index_2) := 'A';

            --> Centros De Costo Para Consumer/Emerging

            IF g_banking_mf = 'GCIB'
            THEN
               c_cencos_imput_t (c_index_2) := '0001005';
            ELSE
               c_cencos_imput_t (c_index_2) := '0005800';
            END IF;



            --> 6610 = D // 6617 = A

            IF c_cod_mv = '6610'
            THEN
               c_deb_cred_t (c_index_2) := 'D';
            ELSE
               c_deb_cred_t (c_index_2) := 'A';
            END IF;
         END IF;
      ELSE

         -- EGV 26Jul2017 Inicio
         --IF c_cod_mv = '6612' AND g_banking_mf = 'GCB'
         IF c_cod_mv = '6612'
         -- EGV 26Jul2017 Fin
         THEN
            c_sucursal := g_secc_mf;
         ELSE
            c_sucursal := ' ';
            /*
              2019/07/25
              la asignacion de esta variable es 
              usada para el filtro de cod_suc
              en la carga de datos de la tabla CB_IMP_CONT
            */
         END IF;




         SELECT DISTINCT IC.CUENTA,
                IC.DEBT_CRED,
                IC.CENCOS_MF,
                'XXX' AS COD_SUC,
                IC.SIGNO
           BULK COLLECT INTO c_cuenta_imput_t,
                c_deb_cred_t,
                c_cencos_imput_t,
                c_suc_imput_t,
                c_signo_t
           FROM BASEARG.CB_IMP_CONT IC
          -- EGV 1.1 15Jun2015 Inicio
          --WHERE     IC.COD_EMP = g_cod_mf
          WHERE     IC.COD_EMP = g_emp_lq
          -- EGV 1.1 15Jun2015 Fin
                AND IC.COD_MV = c_cod_mv
                -- EGV 1.1 15Jun2015 Inicio
                --AND IC.BANKING = g_banking_mf
                --AND (IC.CAT_CONT = g_cat_cont_mf OR IC.CAT_CONT = 'T')
                --AND (IC.COD_SUC = c_sucursal OR c_sucursal = ' ');
                AND rtrim(IC.BANKING,' ') = rtrim(g_banking_mf)
                AND (rtrim(IC.CAT_CONT,' ')=g_cat_cont_mf OR IC.CAT_CONT = 'T')
                -- 2019/07/17 Se comenta linea 26/07/2019
                -- AND (rtrim(IC.COD_SUC,' ') = c_sucursal OR c_sucursal = ' ');
                -- EGV 1.1 15Jun2015 Inicio
                /* 2019/10/31 13:49 - Arreglo de repeticion de registros
                  conceptos citiclub 6612*/
                AND ((IC.COD_MV = 6612 
                      and rtrim(IC.COD_SUC) = rtrim(c_sucursal)) 
                      or ( c_cod_mv <> 6612)
                    )
                ;





         IF c_cuenta_imput_t.COUNT = 0
         THEN
            c_cuenta_imput_t.EXTEND;
            c_deb_cred_t.EXTEND;
            c_cencos_imput_t.EXTEND;
            c_suc_imput_t.EXTEND;
            c_signo_t.EXTEND;

            c_index_2 := c_index_2 + 1;

            c_cuenta_imput_t (c_index_2) := ' ';
            c_suc_imput_t (c_index_2) := ' ';
            c_signo_t (c_index_2) := ' ';
            c_deb_cred_t (c_index_2) := ' ';
            c_cencos_imput_t (c_index_2) := ' ';
         END IF;
      END IF;


      IF c_cuenta_imput_t.COUNT > 0
      THEN
         FOR z IN c_cuenta_imput_t.FIRST .. c_cuenta_imput_t.LAST
         LOOP
            g_cb_asiento_t.EXTEND;

            c_index := c_index + 1;

            g_cb_asiento_t (c_index).cod_lq := g_cod_lq;
            g_cb_asiento_t (c_index).cod_mv := c_cod_mv;
            g_cb_asiento_t (c_index).cod_mf := g_cod_mf;
            g_cb_asiento_t (c_index).cuenta := c_cuenta_imput_t (z);

            IF g_pais_lq = 'URY' AND g_company_lq <> 'FUY'
            THEN

               IF g_mes_lq = '12'
               THEN
                 -- EGV 09DIC2015 Inicio
                 /*
                 CASE c_cod_mv
                     WHEN '52'
                     THEN
                        IF c_deb_cred_t (z) = 'A'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9228240009';
                        END IF;
                     WHEN = '59'
                     THEN
                        IF c_deb_cred_t (z) = 'A'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9342200009';
                        END IF;

                     WHEN 62'
                     THEN
                        IF c_deb_cred_t (z) = 'D'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9342070008';
                        END IF;

                        IF c_deb_cred_t (z) = 'A'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9230340002';
                        END IF;
                     WHEN '64'
                     THEN
                        IF c_deb_cred_t (z) = 'D'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9342010005';
                        END IF;

                        IF c_deb_cred_t (z) = 'A'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9230330007';
                        END IF;
                  END CASE;
                  */
                  IF c_cod_mv = '52' THEN
                    IF c_deb_cred_t (z) = 'A'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9228240009';
                    END IF;
                  END IF;

                  IF c_cod_mv = '59' THEN
                    IF c_deb_cred_t (z) = 'A'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9342200009';
                    END IF;
                  END IF;

                  IF c_cod_mv = '62' THEN
                    IF c_deb_cred_t (z) = 'D'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9342070008';
                    END IF;

                    IF c_deb_cred_t (z) = 'A'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9230340002';
                    END IF;
                  END IF;

                  IF c_cod_mv = '64' THEN
                    IF c_deb_cred_t (z) = 'D'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9342010005';
                    END IF;

                    IF c_deb_cred_t (z) = 'A'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9230330007';
                    END IF;
                  END IF;
                  -- EGV 09DIC2015 Fin
               END IF;


               IF g_mes_lq = '01'
               THEN
                  -- EGV 09DIC2015 Inicio
                  /*
                  CASE c_cod_mv
                     WHEN '10052'
                     THEN
                        IF c_deb_cred_t (z) = 'D'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9342040001';
                        END IF;
                     WHEN '10059'
                     THEN
                        IF c_deb_cred_t (z) = 'D'
                        THEN
                           g_cb_asiento_t (c_index).cuenta := '9342200009';
                        END IF;
                  END CASE;
                  */
                  IF c_cod_mv = '10052' THEN
                    IF c_deb_cred_t (z) = 'D'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9342040001';
                    END IF;
                  END IF;

                  IF c_cod_mv = '10059' THEN
                    IF c_deb_cred_t (z) = 'D'
                    THEN
                       g_cb_asiento_t (c_index).cuenta := '9342200009';
                    END IF;
                  END IF;
                  -- EGV 09DIC2015 Fin
               END IF;
            END IF;

            /*  2019/07/25: se reemplaza logica de asignacion de sucursal*/
            /*  2019/07/25: se comenta logica de asignacion de sucursal
            IF c_suc_imput_t (z) LIKE '%XXX%'
            THEN
               g_cb_asiento_t (c_index).cod_suc := g_secc_mf;
            ELSE
               g_cb_asiento_t (c_index).cod_suc := c_suc_imput_t (z);
            END IF;
            */


            -- EGV 15/06/2015 1.1 - Inicio
            --IF c_cencos_imput_t (z) IS NOT NULL
            IF rtrim(c_cencos_imput_t (z),' ') IS NOT NULL
            -- EGV 15/06/2015 1.1 - Fin
            THEN
               g_cb_asiento_t (c_index).cencos_mf := c_cencos_imput_t (z);
            ELSE
               g_cb_asiento_t (c_index).cencos_mf := c_cencos_hd;
            END IF;

            IF g_pais_lq = 'URY'
               AND SUBSTR (g_cb_asiento_t (c_index).cuenta, 1, 4) NOT IN
                      ('9342', '9344', '9347', '9348', '9345')
            THEN
               g_cb_asiento_t (c_index).cencos_mf := '0000000';
            END IF;

            IF c_deb_cred_t (z) = 'D'
            THEN
               g_cb_asiento_t (c_index).monto_cred := 0;

               g_cb_asiento_t (c_index).monto_deb :=
                  CASE c_signo_t (z)
                     WHEN 'S' THEN (c_importe * -1)
                     ELSE c_importe
                  END;
           ELSE
               g_cb_asiento_t (c_index).monto_cred :=
                  CASE c_signo_t (z)
                     WHEN 'S' THEN (c_importe * -1)
                     ELSE c_importe
                  END;

               g_cb_asiento_t (c_index).monto_deb := 0;
            END IF;

  
            IF g_emp_lq = 15
            THEN
               g_cb_asiento_t (c_index).banking := ' ';
            ELSE
               g_cb_asiento_t (c_index).banking := g_banking_mf;
            END IF;



            /*  2019/07/25: se comenta logica de asignacion de sucursal*/
            --IF c_suc_imput_t (z) LIKE '%XXX%'
            IF c_cod_mv = '6612'
            THEN
                g_cb_asiento_t (c_index).cod_suc := g_secc_mf;
            ELSE
                IF g_emp_lq = 15
                THEN
                      SELECT NVL(MAX(SUCURSAL), '')
                        INTO g_cb_asiento_t (c_index).cod_suc
                      FROM CB_CENCOS
                      WHERE LTRIM(QS_CENCOS, '0') = LTRIM(g_cb_asiento_t (c_index).cencos_mf, '0')
                        -- AND rtrim(CBHR_BANKING)   = rtrim(g_cb_asiento_t (c_index).banking)
                        AND COD_EMP       = g_emp_lq
                      ;

                ELSE                  
                    ----g_cb_asiento_t (c_index).cod_suc := c_suc_imput_t (z);
                    -- 2019/07/25 12:00 : Obtener sucursal del Centro de Costos
                      SELECT NVL(MAX(SUCURSAL), '')
                        INTO g_cb_asiento_t (c_index).cod_suc
                      FROM CB_CENCOS
                      WHERE LTRIM(QS_CENCOS, '0') = LTRIM(g_cb_asiento_t (c_index).cencos_mf, '0')
                        AND rtrim(CBHR_BANKING)   = rtrim(g_cb_asiento_t (c_index).banking)
                        AND COD_EMP       = g_emp_lq
                      ;
                    -- 2019/07/25 12:00 : Obtener sucursal del Centro de Costos
                END IF;
            END IF;
            /* 2019/07/25: se reemplaza logica de asignacion de sucursal*/
            /*Asignacion de estado de procesamiento */
            IF    g_cb_asiento_t (c_index).cuenta = ' '
               OR g_cb_asiento_t (c_index).cencos_mf = ' '
               OR g_cb_asiento_t (c_index).cod_suc = '  '
            THEN
               c_procesado := 'N';

               g_estado_det := 'E';
            ELSE
               c_procesado := 'S';
            END IF;

            g_cb_asiento_t (c_index).procesado := c_procesado;
         END LOOP;
      END IF;


   EXCEPTION
      WHEN OTHERS
      THEN
         g_errores := 'E';

   END Buscar_Datos_IMP_CONT;

   PROCEDURE Verificar_Balanceo
   IS
      c_imp_cred   NUMBER;
      c_imp_deb    NUMBER;
      c_banking    VARCHAR2 (5);
      c_cuenta     VARCHAR2 (50);
   BEGIN
      IF g_company_lq = 'IS'
      THEN
         c_banking := ' ';
         c_cuenta := 'AJUSTE000999999';

         SELECT SUM (A.MONTO_CRED), SUM (A.MONTO_DEB)
           INTO c_imp_cred, c_imp_deb
           FROM BASEARG.CB_ASIENTO A
          WHERE A.COD_LQ = g_cod_lq;



         IF c_imp_cred <> c_imp_deb
         THEN
            IF c_imp_cred > c_imp_deb
            THEN
               --> GENERO UN DEBITO
               c_imp_deb := c_imp_cred - c_imp_deb;
               c_imp_cred := 0;
            ELSE
               --> GENERO UN CREDITO
               c_imp_cred := c_imp_deb - c_imp_cred;
               c_imp_deb := 0;
            END IF;

            INSERT INTO BASEARG.CB_ASIENTO (COD_LQ,
                                               COD_MV,
                                               COD_MF,
                                               CUENTA,
                                               COD_SUC,
                                               CENCOS_MF,
                                               MONTO_CRED,
                                               MONTO_DEB,
                                               BANKING,
                                               PROCESADO)
                 VALUES (g_cod_lq,
                         0,
                         0,
                         c_cuenta,
                         ' ',
                         '0000000',
                         c_imp_cred,
                         c_imp_deb,
                         c_banking,
                         'S');
         END IF;
      ELSE
         c_banking := 'GCB';
         c_cuenta := 'AJUSTE00099999';

         SELECT SUM (A.MONTO_CRED), SUM (A.MONTO_DEB)
           INTO c_imp_cred, c_imp_deb
           FROM BASEARG.CB_ASIENTO A
          WHERE A.COD_LQ = g_cod_lq AND A.BANKING = c_banking;


         IF c_imp_cred <> c_imp_deb
         THEN
            IF c_imp_cred > c_imp_deb
            THEN
               --> GENERO UN DEBITO
               c_imp_deb := c_imp_cred - c_imp_deb;
               c_imp_cred := 0;
            ELSE
               --> GENERO UN CREDITO
               c_imp_cred := c_imp_deb - c_imp_cred;
               c_imp_deb := 0;
            END IF;

            INSERT INTO BASEARG.CB_ASIENTO (COD_LQ,
                                               COD_MV,
                                               COD_MF,
                                               CUENTA,
                                               COD_SUC,
                                               CENCOS_MF,
                                               MONTO_CRED,
                                               MONTO_DEB,
                                               BANKING,
                                               PROCESADO)
                 VALUES (g_cod_lq,
                         0,
                         0,
                         c_cuenta,
                         ' ',
                         '0000000',
                         c_imp_cred,
                         c_imp_deb,
                         c_banking,
                         'S');
         END IF;


         c_banking := 'GCIB';
         c_cuenta := 'AJUSTE000999999';

         SELECT SUM (A.MONTO_CRED), SUM (A.MONTO_DEB)
           INTO c_imp_cred, c_imp_deb
           FROM BASEARG.CB_ASIENTO A
          WHERE A.COD_LQ = g_cod_lq AND A.BANKING = c_banking;


         IF c_imp_cred <> c_imp_deb
         THEN
            IF c_imp_cred > c_imp_deb
            THEN
               --> GENERO UN DEBITO
               c_imp_deb := c_imp_cred - c_imp_deb;
               c_imp_cred := 0;
            ELSE
               --> GENERO UN CREDITO
               c_imp_cred := c_imp_deb - c_imp_cred;
               c_imp_deb := 0;
            END IF;

            INSERT INTO BASEARG.CB_ASIENTO (COD_LQ,
                                               COD_MV,
                                               COD_MF,
                                               CUENTA,
                                               COD_SUC,
                                               CENCOS_MF,
                                               MONTO_CRED,
                                               MONTO_DEB,
                                               BANKING,
                                               PROCESADO)
                 VALUES (g_cod_lq,
                         0,
                         0,
                         c_cuenta,
                         ' ',
                         '0000000',
                         c_imp_cred,
                         c_imp_deb,
                         c_banking,
                         'S');
         END IF;
      END IF;
   END Verificar_Balanceo;
END PKG_BXSCITI_DATOS_CONT;