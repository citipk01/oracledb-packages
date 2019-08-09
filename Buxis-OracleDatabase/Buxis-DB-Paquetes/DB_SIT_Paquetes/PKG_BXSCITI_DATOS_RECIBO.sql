CREATE OR REPLACE PACKAGE         PKG_BXSCITI_DATOS_RECIBO
AS
   /******************************************************************************
    NAME: BASEARG.PKG_BXSCITI_DATOS_RECIBO
    PURPOSE:

    REVISIONS:
    Ver Date Author Description
    --------- ---------- --------------- ------------------------------------
    1.0  30/04/2015 CSA 1.  Se procesan los datos para la generacion de los Recibos de Sueldo
   ******************************************************************************/


        g_id_proc      INTEGER;
        g_proc_int     VARCHAR (100);
        g_descr_det    VARCHAR2 (100);
        g_estado       VARCHAR2 (2);
        g_estado_det   VARCHAR2 (254);
        g_resultado    VARCHAR2 (50);
        g_errores      INTEGER := 0;
   
       --> Variables Empleado
   
        g_cod_mf           INTEGER := 0;
        g_emp_mf           INTEGER := 0;
        g_nom_mf        VARCHAR (50);
        g_cat_conve_mf VARCHAR2(3);
        g_cb_geid_mf VARCHAR2(11);
        g_respons_mf VARCHAR2(5);
        g_fec_ing_vl_mf DATE;
        g_fec_hire_mf DATE;
        g_fec_ant_mf DATE;
        -- EGV 05Jun2017 Inicio
        --g_cta_acredit_mf  VARCHAR(17);
        g_cta_acredit_mf  VARCHAR(22);
        -- EGV 05Jun2017 Fin
        g_cuil_mf VARCHAR2(20);
        g_tipo_doc_mf VARCHAR2(4);
        g_num_doc_mf VARCHAR2(15);
        g_osocial_mf VARCHAR(6); 
        g_secc_mf VARCHAR2 (5);
        g_fpago_mf VARCHAR2(4);
        g_grado_mf  VARCHAR2(3);         -- EGV 16OCT2015
        g_full_part_mf CHAR(1);         -- EGV 16OCT2015
        
   
      --> Variables Datos Liquidacion
      
      g_cod_lq INTEGER;
      g_fec_lq         DATE;
      g_desc_lq        VARCHAR2 (50);
      g_cod_pr          VARCHAR(5);
      g_desc_pr        VARCHAR2 (30);
      g_cod_emp        INTEGER;
      g_fec_desde      DATE;
      g_fec_hasta      DATE;
      g_fec_pago       DATE;
      g_fec_acum       DATE;
      g_per_liq        VARCHAR2 (15);

      --> Variables Empresa
      g_nom_emp        VARCHAR2 (30);
      g_dire_emp       VARCHAR2 (35);
      g_cuit_emp       VARCHAR2 (20);
      g_pais_emp       VARCHAR2 (5);
      g_comp_emp       VARCHAR2 (3);
      g_convenio_emp   VARCHAR2 (4);
      g_bse_emp        VARCHAR2 (15) := '';
      g_bps_emp        VARCHAR2 (15) := '';
      

   PROCEDURE datos_recibos (p_cod_lq       IN     VARCHAR2,
                            p_ultimo_dep   IN     VARCHAR2,
                            p_legajos      IN     VARCHAR2,
                            p_id_proc      IN     INTEGER,
                            p_resultado       OUT VARCHAR2);
    PROCEDURE obtener_datos_liquidacion (p_resultado OUT VARCHAR2);
    PROCEDURE obtener_datos_empresa (p_resultado OUT VARCHAR2);
    PROCEDURE obtener_descr_categoria (p_resultado OUT VARCHAR2,p_desc_cat OUT  VARCHAR2);
    FUNCTION  numero_a_letras(p_numero in number) return varchar2;
    FUNCTION  numero_menor_mil(p_numero in number) return varchar2 ;
    FUNCTION SumaGanancias(l_conc in number) return integer;      -- EGV 16OCT2015
    FUNCTION TraeSegur return number;      -- EGV 16OCT2015
    FUNCTION TraeMedic return number;      -- EGV 16OCT2015
    
END PKG_BXSCITI_DATOS_RECIBO; 
/


CREATE OR REPLACE PACKAGE BODY         PKG_BXSCITI_DATOS_RECIBO
AS
   /******************************************************************************
    NAME: BASEARG.PKG_BXSCITI_DATOS_RECIBO
    PURPOSE:

    REVISIONS:
    Ver Date Author Description
    --------- ---------- --------------- ------------------------------------
    1.0  30/04/2015 CSA 1.  Se procesan los datos para la generacion de los Recibos de Sueldo
   ******************************************************************************/


   PROCEDURE datos_recibos (p_cod_lq       IN     VARCHAR2,
                            p_ultimo_dep   IN     VARCHAR2,
                            p_legajos      IN     VARCHAR2,
                            p_id_proc      IN     INTEGER,
                            p_resultado       OUT VARCHAR2)
   IS
      l_cod_proc         VARCHAR2 (50) := 'DATOS RECIBO';
      l_descr            VARCHAR2 (50)
                            := 'Generación de datos para recibos de sueldo';
      l_contador         INTEGER := 0;
      l_indx_det         INTEGER := 0;

      l_errores          INTEGER := 0;
      l_existe_det       VARCHAR2 (1);


      l_datos_mf         MAEFUNC_TBL%ROWTYPE;


      --> Variables para Cabecera Empleado

      l_business_title   VARCHAR2 (30);
      l_desc_cat         VARCHAR2 (30);
      l_desc_suc         VARCHAR2 (30);
      l_num_rec          INTEGER;
      l_desc_osoc        VARCHAR2 (30);
      l_direc_suc        VARCHAR2 (35);

      l_rem_suj_ret      NUMBER (24, 8);
      l_con_no_grav      NUMBER (24, 8);
      l_reten            NUMBER (24, 8);
      l_neto             NUMBER (24, 8);
      l_neto_letras      VARCHAR2 (100);
      l_gan_liq          NUMBER (24, 8);
      l_gan_rec          NUMBER (24, 8);


      --> Variables para Detalle Empleado
        l_por_banco NUMBER(24,8);
        l_efectivo NUMBER(24,8);
        
        l_d1242_clasif   NUMBER (24, 8);
        l_d1242_porc  NUMBER (24, 8);
        
        l_monto6693 NUMBER(24,8);
        l_monto6678 NUMBER(24,8);
        l_ajuste6678 VARCHAR2(1);

      --> Variables para Dorso Empleado
      l_indx_dor         INTEGER := 0;  
      l_ordinal NUMBER(24,8);
      l_sob_linea VARCHAR2(100);  
      l_mesanio INTEGER :=0;        
      
      --> No se realiza por el momento
      
      l_comentario CB_COMENTARIO.DESCRLONG%TYPE;            -- EGV 02Oct2015
      l_espacios_dorso VARCHAR2(50);            -- EGV 16Oct2015
      l_impuesto_ret_mf MOVHD.IMPTOT_HD%TYPE;         -- EGV 16OCT2015
      l_segur_mf MOVHD.IMPTOT_HD%TYPE;         -- EGV 16OCT2015
      l_medic_mf MOVHD.IMPTOT_HD%TYPE;         -- EGV 16OCT2015


      l_legajos_tbl      var_array;

      TYPE l_recibo_mf_list IS TABLE OF BASEARG.CB_REC_MF%ROWTYPE;

      l_recibo_mf_t      l_recibo_mf_list;

      TYPE l_recibo_det_list IS TABLE OF BASEARG.CB_REC_DET%ROWTYPE;

      l_recibo_det_t     l_recibo_det_list;
      
      TYPE l_recibo_dor_list IS TABLE OF BASEARG.CB_REC_DOR%ROWTYPE;      
      
      l_recibo_dor_t     l_recibo_dor_list;


      CUSTOM             EXCEPTION;
      NEXT_VAL           EXCEPTION;

      CURSOR CUR_DATOS_REC (
         -- EGV 18AGO2015 Inicio
         --c_legajo VARCHAR2)
         c_legajo VARCHAR2,
         c_fhas   DATE)
         -- EGV 18AGO2015 Fin
      IS
           SELECT MHD.COD_MF,
                  MHD.COD_MV,
                  MHD.JORNALES_HD,
                  MHD.HORAS_HD,
                  MHD.UNID_HD,
                  MHD.IMPTOT_HD,
                  MHD.HADES_HD,
                  MHD.IMPUNI_HD,
                  MHD.AJUSTE_HD,
                  MCO.DESC_MV,
                  MCO.ORPROC_MV,
                  MCO.UNID_MV,
                  MCO.JOR_PR_MV,
                  MCO.HOR_PR_MV,
                  MCO.UNID_PR_MV,
                  ACU.ACUMULADOR
             FROM BASEARG.MOVHD MHD,
                  -- EGV 18AGO2015 Inicio
                  --BASEARG.ACUMULAN ACU,
                  --BASEARG.MOVCOD MCO
                  BASEARG.ACUMULAN_TBL ACU,
                  BASEARG.MOVCOD_TBL MCO
                  -- EGV 18AGO2015 Fin
            -- PS_PERSONAL_DATA --> no hace falta aca
            --PS_EMPLOYMENT EMP,
            --PS_CBHR_RESPON_TBL RESP,
            --PS_CBHR_CARACT_TBL CARACT
            WHERE     MHD.COD_LQ = p_cod_lq
                  AND MHD.COD_MF = c_legajo
                  -- EGV 18AGO2015 Inicio
                  --AND MHD.COD_MV = ACU.COD_MV(+)
                  AND MHD.COD_MV = ACU.COD_MV
                  -- EGV 18AGO2015 Fin
                  AND ( (ACU.ACUMULADOR IN ('hcd', 'hsd', 'des', 'deb')
                         AND MCO.INFORMA_MV = 0)
                       OR (ACU.ACUMULADOR = 'inf'))
                  AND MHD.COD_MV = MCO.COD_MV
                  -- EGV 18AGO2015 Inicio
                  AND ACU.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM ACUMULAN_TBL WHERE ACU.ACUMULADOR = ACUMULADOR AND FECHA_EFECTIVA <= c_fhas)
                  AND ACU.BAJA_EFECTIVA IS NULL
                  AND MCO.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM MOVCOD_TBL WHERE MCO.COD_MV = COD_MV AND FECHA_EFECTIVA <= c_fhas)
                  AND MCO.BAJA_EFECTIVA IS NULL                  
                  -- EGV 18AGO2015 Fin
         ORDER BY MCO.COD_MV;
         
      CURSOR CUR_DATOS_DOR (
         c_legajo VARCHAR2)
      IS
           SELECT ORDINAL,
                  -- EGV 18Ago2015 Inicio
                  --SOB_LINEA
                  TRIM(SOB_LINEA) AS SOB_LINEA
                  -- EGV 18Ago2015 Fin
             FROM BASEARG.QS4TASOBRES 
            WHERE     COD_LQ = p_cod_lq
                  AND COD_MF = c_legajo
         ORDER BY ORDINAL;
         
      -- EGV 19OCT2015 Inicio   
      CURSOR CUR_NUM_REC(l_liq NUMBER) IS
        SELECT COD_EMP, COD_MF, RESPONS, NOMBRE
        FROM CB_REC_MF
        WHERE COD_LQ = l_liq
        ORDER BY COD_EMP, RESPONS, NOMBRE;
      -- EGV 19OCT2015 Fin
         
   BEGIN
      g_cod_lq := p_cod_lq;
      g_cod_mf := 0;
      g_id_proc := p_id_proc;
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

      --> Se Obtienen los datos generales patra la liquidacion

      PKG_BXSCITI_DATOS_RECIBO.obtener_datos_liquidacion (g_estado_det);

      IF g_estado_det = 'E'
      THEN
         g_estado := 'E';
         p_resultado :=
               'Error obteniendo los datos de la liquidacion '
            || p_cod_lq
            || ' Error : '
            || SQLCODE
            || ' -- '
            || SQLERRM;
         RAISE CUSTOM;
      END IF;


      PKG_BXSCITI_DATOS_RECIBO.obtener_datos_empresa (g_estado_det);

      IF g_estado_det = 'E'
      THEN
         g_estado := 'E';
         g_resultado :=
               'Error obteniendo los datos de la empresa '
            || g_cod_emp
            || ' Error : '
            || SQLCODE
            || ' -- '
            || SQLERRM;
         RAISE CUSTOM;
      END IF;

      --DBMS_OUTPUT.PUT_LINE ('g_cuit_emp : ' || g_cuit_emp);
      
      -- EGV 18AGO2015 Inicio
      IF p_legajos IS NULL
      THEN
           DELETE FROM CB_REC_CAB WHERE COD_LQ = g_cod_lq AND COD_EMP = g_cod_emp;
      END IF;
      -- EGV 18AGO2015 Inicio

      BEGIN
         --> Completo la tabla CB_REC_CAB

         INSERT INTO BASEARG.CB_REC_CAB (COD_LQ,
                                            COD_EMP,
                                            NOM_EMP,
                                            DIRECCION,
                                            CUIT,
                                            BSE,
                                            BPS,
                                            DESCR_PR,
                                            PER_LIQ,
                                            ULTI_DEP,
                                            FPAGO_LQ)
              VALUES (g_cod_lq,
                      g_cod_emp,
                      g_nom_emp,
                      g_dire_emp,
                      g_cuit_emp,
                      g_bse_emp,
                      g_bps_emp,
                      g_desc_pr,
                      g_per_liq,
                      p_ultimo_dep,
                      g_fec_pago);
      EXCEPTION
         WHEN DUP_VAL_ON_INDEX
         THEN
            NULL;
         WHEN OTHERS
         THEN
            g_estado := 'E';
            p_resultado :=
                  'Error insertando la cabecera de la liquidacion: '
               || p_cod_lq
               || ' Error : '
               || SQLCODE
               || ' -- '
               || SQLERRM;
            RAISE CUSTOM;
      END;

      --> Lleno la tabla legagos_tbl con los que se van a procesar
      IF p_legajos IS NULL
      THEN
           SELECT DISTINCT MHD2.COD_MF
             BULK COLLECT INTO l_legajos_tbl
            -- EGV 18AGO2015 Inicio
            -- FROM MOVHD MHD2, ACUMULAN ACU2, MOVCOD MCO2
            --WHERE     MHD2.COD_LQ = p_cod_lq
            --      AND MHD2.COD_MV = ACU2.COD_MV(+)
            --      AND ACU2.ACUMULADOR IN ('hcd', 'hsd', 'des', 'deb')
            --      AND MHD2.COD_MV = MCO2.COD_MV
            --      AND MCO2.INFORMA_MV = 0
              FROM MOVHD MHD2, ACUMULAN_TBL ACU2, MOVCOD_TBL MCO2
            WHERE     MHD2.COD_LQ = p_cod_lq
                  AND MHD2.COD_MV = ACU2.COD_MV
                  AND ACU2.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM ACUMULAN_TBL WHERE ACU2.ACUMULADOR = ACUMULADOR AND FECHA_EFECTIVA <= g_fec_hasta)
                  AND ACU2.BAJA_EFECTIVA IS NULL
                  AND MHD2.COD_MV = MCO2.COD_MV
                  AND MCO2.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM MOVCOD_TBL WHERE MCO2.COD_MV = COD_MV AND FECHA_EFECTIVA <= g_fec_hasta)
                  AND MCO2.BAJA_EFECTIVA IS NULL
                  AND ( (ACU2.ACUMULADOR IN ('hcd', 'hsd', 'des', 'deb')
                         AND MCO2.INFORMA_MV = 0)
                       OR (ACU2.ACUMULADOR = 'inf'))
            -- EGV 18AGO2015 Fin
         ORDER BY MHD2.COD_MF;
         
         -- EGV 18AGO2015 Inicio
         DELETE BASEARG.CB_REC_MF
         WHERE     COD_LQ = p_cod_lq
               AND COD_EMP = g_cod_emp;

        DELETE BASEARG.CB_REC_DET
         WHERE     COD_LQ = p_cod_lq
               AND COD_EMP = g_cod_emp;

        DELETE BASEARG.CB_REC_DOR
         WHERE     COD_LQ = p_cod_lq
               AND COD_EMP = g_cod_emp;
         -- EGV 18AGO2015 Fin
         
      ELSE
         l_legajos_tbl :=
            BASEARG.PKG_BXSCITI_GENERAL.convertir_lista (p_legajos, ';');
      END IF;

      IF l_legajos_tbl.COUNT = 0
      THEN
         g_estado := 'E';
         p_resultado :=
            'No se han encontrado legajos a procesar en la liquidacion : '
            || p_cod_lq;
         RAISE CUSTOM;
      END IF;

      --DBMS_OUTPUT.PUT_LINE ('Cant Empleados : ' || l_legajos_tbl.COUNT);

      --> Por cada legajo, genero los datos del recibo
      FOR i IN l_legajos_tbl.FIRST .. l_legajos_tbl.LAST
      LOOP
         --> Blanqueo variables de cabecera

         l_rem_suj_ret := 0;
         l_con_no_grav := 0;
         l_reten := 0;
         l_neto := 0;
         l_neto_letras := ' ';
         l_gan_liq := 0;
         l_gan_rec := 0;

         l_business_title := ' ';
         l_desc_cat := ' ';
         l_desc_suc := ' ';
         l_desc_osoc := ' ';
         l_direc_suc := ' ';
         
        l_por_banco :=0;
        l_efectivo :=0;
                    
        l_d1242_clasif :=0;
        l_d1242_porc  :=0;
                    
        l_monto6693 :=0;
        l_monto6678 :=0;
        l_ajuste6678 :='N';

         l_contador := l_contador + 1;
         l_indx_det := 0;
         l_num_rec:=0;
         
         l_indx_dor := 0;       -- EGV 18Ago2015
         l_comentario := null;    -- EGV 0Oct2015
         l_impuesto_ret_mf := 0;    -- EGV 16OCT2015
         l_segur_mf := 0;    -- EGV 16OCT2015
         l_medic_mf := 0;    -- EGV 16OCT2015

         --DBMS_OUTPUT.PUT_LINE ('Legajo: ' || l_legajos_tbl (i));


         BEGIN
            COMMIT;
            SET TRANSACTION READ WRITE NAME 'DATOS_RECIBO';

            SAVEPOINT INICIO;


            --> ELIMINO LA INFORMACION DEL EMPLEADO QUE ESTOY PROCESANDO
            -- EGV 18AGO2015 Inicio
            IF p_legajos IS NOT NULL
            THEN
            -- EGV 18AGO2015 Fin
            
                -- EGV 19OCT2015 Inicio
                l_num_rec := 0;
                
                BEGIN
                SELECT NUM_REC
                    Into l_num_rec
                FROM BASEARG.CB_REC_MF
                WHERE COD_LQ = p_cod_lq
                   AND COD_EMP = g_cod_emp
                   AND COD_MF = l_legajos_tbl (i);
                EXCEPTION
                    WHEN OTHERS THEN
                        NULL;
                END;
                -- EGV 19OCT2015 Fin
            
                DELETE BASEARG.CB_REC_MF
                 WHERE     COD_LQ = p_cod_lq
                       AND COD_EMP = g_cod_emp
                       AND COD_MF = l_legajos_tbl (i);

                DELETE BASEARG.CB_REC_DET
                 WHERE     COD_LQ = p_cod_lq
                       AND COD_EMP = g_cod_emp
                       AND COD_MF = l_legajos_tbl (i);

                DELETE BASEARG.CB_REC_DOR
                 WHERE     COD_LQ = p_cod_lq
                       AND COD_EMP = g_cod_emp
                       AND COD_MF = l_legajos_tbl (i);
            -- EGV 18AGO2015 Inicio
            END IF;
            -- EGV 18AGO2015 Fin
            
            --DBMS_OUTPUT.PUT_LINE ('Borrado de datos finalizado');
            --DBMS_OUTPUT.PUT_LINE ('g_cod_mf : ' || g_cod_mf);

            IF g_cod_mf = 0 OR g_cod_mf <> l_legajos_tbl (i)
            THEN
               g_proc_int := 'Datos MAEFUNC';

               BASEARG.PKG_BXSCITI_GENERAL.
                Obtener_Datos_MAEFUNC (l_legajos_tbl (i),
                                       g_fec_hasta,         -- EGV 18Ago2015
                                       g_estado_det,
                                       l_datos_mf);
                                   

               IF g_estado_det <> 'E'
               THEN
                  g_cod_mf := TRIM (l_datos_mf.cod_mf);
                  g_emp_mf := TRIM (l_datos_mf.cod_emp);
                  -- EGV 02OCT2015 Inicio
                  g_nom_mf := SUBSTR(TRIM(TRIM (l_datos_mf.pri_ape_mf) || ' ' || TRIM(l_datos_mf.seg_ape_mf)) || ',' || TRIM(TRIM (l_datos_mf.pri_nom_mf) || ' ' || trim(l_datos_mf.seg_nom_mf)),1,50);
                  -- EGV 02OCT2015 Fin
                  g_cat_conve_mf := TRIM (l_datos_mf.cat_conve);
                  g_cb_geid_mf := TRIM (l_datos_mf.cb_geid);
                  g_respons_mf := TRIM (l_datos_mf.respons);
                  g_fec_ing_vl_mf := TRIM (l_datos_mf.fec_ing_vl);
                  g_fec_hire_mf := TRIM (l_datos_mf.fec_hire);
                  g_fec_ant_mf := TRIM (l_datos_mf.fec_antig);
                  g_cta_acredit_mf := TRIM (l_datos_mf.identfp_mf);
                  g_cuil_mf := TRIM (l_datos_mf.cuil);
                  g_cuil_mf :=
                        SUBSTR (g_cuil_mf, 1, 2)
                     || '-'
                     || SUBSTR (g_cuil_mf, 3, 8)
                     || '-'
                     || SUBSTR (g_cuil_mf, 11, 1);
                  -- EGV 30SEP2015 Inicio
                  --g_tipo_doc_mf := TRIM (l_datos_mf.tipo_doc);
                  g_tipo_doc_mf := 'DU';
                  -- EGV 30SEP2015 Fin
                  g_num_doc_mf := TRIM (l_datos_mf.cedide_mf);
                  g_osocial_mf := TRIM (l_datos_mf.osocial_citi);
                  g_secc_mf := TRIM (l_datos_mf.secc_mf);
                  g_fpago_mf := TRIM(l_datos_mf.fpago_mf);
                  g_grado_mf := TRIM(l_datos_mf.grado); -- EGV 16OCT2015
                  -- EGV 16OCT2015 Inicio
                  if l_datos_mf.std_hours = 162.5 then
                    g_full_part_mf := 'F';
                  elsif l_datos_mf.std_hours = 130 then
                    g_full_part_mf := '6';
                  elsif l_datos_mf.std_hours = 108.3 then
                    g_full_part_mf := '5';
                  elsif l_datos_mf.std_hours = 86.6 then
                    g_full_part_mf := '4';
                  else
                    g_full_part_mf := 'P';
                  end if;
                  -- EGV 16OCT2015 Fin
                  
                  --l_business_title := l_datos_mf.business_title;
                   
               ELSE
                  g_descr_det :=
                     'Error obteniendo los datos del empleado  : '
                     || l_legajos_tbl (i);
                     
                  RAISE NEXT_VAL;
               END IF;

               --DBMS_OUTPUT.PUT_LINE ('g_nom_mf : ' || g_nom_mf);
            END IF;


            -->Busco los datos de Business Title ,Categoria, Obra Social, Sucursal

            --> Business Title
            
            l_business_title := 'BUS_TITTLE ';
            
           IF l_business_title = 'Non-Officer' THEN
             l_business_title := ' ';
           END IF;


            --> Categoria

            BASEARG.PKG_BXSCITI_DATOS_RECIBO.obtener_descr_categoria (g_estado_det, l_desc_cat);
            IF g_estado_det = 'E' THEN l_desc_cat := ' '; END IF;
            

            --> Obra Social

            BEGIN
               SELECT TRIM (OS.DESCRIP)
                 INTO l_desc_osoc
                 FROM BASEARG.CB_QS_OSOCIAL OS
                WHERE TRIM (OS.QS_OSOCIAL) = g_osocial_mf;
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  l_desc_osoc := ' ';
            END;

            --> Sucursal

            BEGIN
               SELECT TRIM (S.DESCRIP), TRIM (S.DIRECCION)
                 INTO l_desc_suc, l_direc_suc
                 FROM CB_SUCURSAL S
                WHERE TRIM (S.COD_SUC) = g_secc_mf
                      AND S.FECHA_EFECTIVA =
                             (SELECT MAX (SE.FECHA_EFECTIVA)
                                FROM CB_SUCURSAL SE
                               WHERE SE.COD_SUC = S.COD_SUC
                                     AND SE.FECHA_EFECTIVA <= g_fec_hasta);
            EXCEPTION
               WHEN NO_DATA_FOUND
               THEN
                  l_desc_suc := ' ';
                  l_direc_suc := ' ';
            END;


            --> Numero de Recibo
            -- EGV 19OCT2015 Inicio
            --l_num_rec := l_contador;
            -- EGV 19OCT2015 Fin


            --> Genero la cabecera del empleado
            l_recibo_mf_t := l_recibo_mf_list ();
            l_recibo_mf_t.EXTEND;

            l_recibo_mf_t (1).cod_lq := p_cod_lq;
            l_recibo_mf_t (1).cod_emp := g_cod_emp;
            l_recibo_mf_t (1).cod_mf := g_cod_mf;
            l_recibo_mf_t (1).nombre := g_nom_mf;
            l_recibo_mf_t (1).tipo_doc := g_tipo_doc_mf;
            l_recibo_mf_t (1).cedide_mf := g_num_doc_mf;

            IF g_pais_emp = 'URY'
            THEN
               l_recibo_mf_t (1).fec_ing_vl := g_fec_ing_vl_mf;
            ELSE
               l_recibo_mf_t (1).fec_ing_vl := NULL;
            END IF;

            l_recibo_mf_t (1).business_title := l_business_title;
            l_recibo_mf_t (1).desc_cat := l_desc_cat;
            l_recibo_mf_t (1).fec_hire := g_fec_hire_mf;
            l_recibo_mf_t (1).desc_suc := l_desc_suc;
            l_recibo_mf_t (1).cuil := g_cuil_mf;
            l_recibo_mf_t (1).num_rec := l_num_rec;
            l_recibo_mf_t (1).cb_geid := g_cb_geid_mf;
            l_recibo_mf_t (1).desc_osoc := l_desc_osoc;
            l_recibo_mf_t (1).direc_suc := l_direc_suc;
            l_recibo_mf_t (1).identfp_mf := g_cta_acredit_mf;
            l_recibo_mf_t (1).respons := g_respons_mf;


            -->  Los datos de los totales se calculan cuando se procesa el detalle
            -- > Los comentarios se procesan al final


            --> Recorro los conceptos que van en el detalle del recibo
            g_proc_int := 'Detalle Recibo';

            l_recibo_det_t := l_recibo_det_list ();


            -- EGV 18AGO2015 Inicio
            --FOR X IN CUR_DATOS_REC (g_cod_mf)
            FOR X IN CUR_DATOS_REC (g_cod_mf, g_fec_hasta)
            -- EGV 18AGO2015 Fin
            LOOP
               l_indx_det := l_indx_det + 1;
               l_recibo_det_t.EXTEND;

               --DBMS_OUTPUT.PUT_LINE ('cod_mv : ' || X.COD_MV);
               
               l_recibo_det_t (l_indx_det).cod_lq := g_cod_lq;
               l_recibo_det_t (l_indx_det).cod_emp := g_cod_emp;
               l_recibo_det_t (l_indx_det).cod_mf := g_cod_mf;
              -- EGV 12Abr2016 Inicio
              --SELECT decode (X.COD_MV,2051,100+l_indx_det,2059,100+l_indx_det,l_indx_det)  INTO     l_recibo_det_t (l_indx_det).sec FROM DUAL;
              SELECT decode (X.COD_MV,2051,900+l_indx_det,2059,900+l_indx_det,l_indx_det)  INTO     l_recibo_det_t (l_indx_det).sec FROM DUAL;
              -- EGV 12Abr2016 Fin
               l_recibo_det_t (l_indx_det).cod_mv := X.COD_MV;
               l_recibo_det_t (l_indx_det).descrip := X.DESC_MV;
               
               
               
                IF X.ACUMULADOR = 'deb' THEN --> Se procesa por separado 
                    IF X.IMPTOT_HD <>0 THEN
                            l_por_banco  := 0;
                            l_efectivo   := 0;
                            
                            --> Buscar Cuenta
                    END IF;
                               
                ELSE

                    -- Pluriempleo
                    -- Hire Date
                    -- Re Hire Date
                    
                        IF X.COD_MV = 6678 THEN
                             l_monto6678 := X.IMPTOT_HD ;
                             IF  X.AJUSTE_HD <> 0 THEN l_ajuste6678 := 'Y'; END IF;
                        END IF;            
                        
                        IF X.COD_MV = 6693 THEN
                             l_monto6693 := X.IMPTOT_HD ;
                        END IF;            
                                     


                        IF NOT(( EXTRACT (MONTH FROM G_FEC_LQ)IN (6,12))
                          AND (X.COD_MV >= 10089 AND X.COD_MV <= 10100)
                          AND (INSTR(G_COD_PR, 'SAC',1) <> 0)) THEN
                                      
                              IF X.HADES_HD = 'h' THEN
                                    IF X.ACUMULADOR = 'hcd' THEN

                                         l_recibo_det_t (l_indx_det).rem_suj_ret := X.IMPTOT_HD;
                                        
                                        l_rem_suj_ret := l_rem_suj_ret + X.IMPTOT_HD;

                                    ELSE 
                                        IF X.ACUMULADOR = 'hsd' THEN

                                        l_recibo_det_t (l_indx_det).con_no_grav := X.IMPTOT_HD;
                                              
                                        l_con_no_grav := l_con_no_grav +  X.IMPTOT_HD;
                                            

                                        END IF;

                                    END IF;

                              ELSE

                                   IF X.HADES_HD = 'd' THEN
                                   
                                       l_recibo_det_t (l_indx_det).reten := X.IMPTOT_HD;
                                       
                                    l_reten := l_reten +  X.IMPTOT_HD;    

                                  END IF;

                              END IF;
                              
                              IF X.JORNALES_HD > 0  AND TRIM(X.JOR_PR_MV) IN ('1','2') THEN
                                  l_recibo_det_t (l_indx_det).unidades := X.JORNALES_HD;

                              ELSE
                                -- EGV 30SEP2015 Inicio
                                --IF X.HORAS_HD > 0 AND TRIM(X.JOR_PR_MV) IN ('1','2') THEN
                                IF X.HORAS_HD > 0 AND TRIM(X.HOR_PR_MV) IN ('1','2') THEN
                                -- EGV 30SEP2015 Fin
                                    l_recibo_det_t (l_indx_det).unidades := X.HORAS_HD;
                                ELSE
                                    -- EGV 30SEP2015 Inicio
                                    --IF X.UNID_HD > 0  AND TRIM(X.JOR_PR_MV) IN ('1','2') THEN
                                    IF X.UNID_HD > 0  AND TRIM(X.UNID_PR_MV) IN ('1','2') THEN
                                    -- EGV 30SEP2015 Fin
                                         l_recibo_det_t (l_indx_det).unidades := X.UNID_HD;
                                    ELSE
                                         l_recibo_det_t (l_indx_det).unidades := 0;
                                    END IF;
                                END IF;
                                    
                              END IF;    
                              
                           --> VERIFICAR ESTA CONDICION
                           -->NO IMPRIME EL IMPUNI_HD SI EL PROCESO DE LA LIQUIDACION NO ES 'MEN'

                           -- IF X.IMPUNI_HD <>0 AND INSTR(XCOD_PR, 'MEN',1) = 0 THEN 
                                       l_recibo_det_t (l_indx_det).impuni_hd := X.IMPUNI_HD ;
                           -- END IF;    

                            --> Para Uruguay los conceptos informativos que se informen en el recibo serán también informados como conceptos no gravados.
                            
                              IF X.ACUMULADOR=  'inf' and g_pais_emp ='URY' THEN
                                      l_recibo_det_t (l_indx_det).con_no_grav := X.IMPTOT_HD;
                              END IF;
                              
                          
                        END IF;


                END IF;              
               
            END LOOP;
            
                      

            IF l_indx_det = 0
            THEN
               g_estado_det := 'E';
               g_descr_det :=
                     'No existen datos para la Liquidacion '
                  || p_cod_lq
                  || ' Empleado '
                  || g_cod_mf;
               RAISE NEXT_VAL;
            END IF;

            --> Busco los comentarios
            -- EGV 16OCT2015 Inicio
            --l_recibo_mf_t (1).coment1 := ' ';
            --l_recibo_mf_t (1).coment2 := ' ';
            --l_recibo_mf_t (1).coment3 := ' ';
            --l_recibo_mf_t (1).coment4 := ' ';
            --l_recibo_mf_t (1).coment5 := ' ';
            BEGIN
            SELECT B.DESCRLONG
                Into l_comentario
            FROM CB_COM_RECIBO A, CB_COMENTARIO B
            WHERE A.CB_COMENT = B.CB_COMENT
            AND A.GRADO = g_grado_mf
            AND A.FULL_PART_TIME = g_full_part_mf
            AND A.QS_CONVENIO = g_convenio_emp;
            EXCEPTION 
                WHEN OTHERS THEN
                    NULL;
            END;

            l_recibo_mf_t (1).coment1 := trim(to_char(substr(l_comentario,1,195)));
            l_recibo_mf_t (1).coment2 := trim(to_char(substr(l_comentario,196,195)));
            l_recibo_mf_t (1).coment3 := trim(to_char(substr(l_comentario,391,195)));
            l_recibo_mf_t (1).coment4 := trim(to_char(substr(l_comentario,586,195)));
            l_recibo_mf_t (1).coment5 := trim(to_char(substr(l_comentario,781,195)));
            -- EGV 16OCT2015 Fin
                        
            
            --> Inserto decreto   PEN 1242/2013'
            
            l_d1242_clasif := 4;
            l_d1242_porc := 0;
            
            BEGIN
                -- EGV 30SEP2015 Inicio
                --SELECT MH.IMPUNI_HD,MH.IMPTOT_HD INTO l_d1242_clasif,l_d1242_porc FROM BASEARG.MOVHD MH WHERE  MH.COD_LQ = g_cod_lq AND MH.COD_MF = g_cod_mf AND MH.COD_MV=1015;
                SELECT MH.IMPUNI_HD,MH.IMPTOT_HD INTO l_d1242_porc,l_d1242_clasif FROM BASEARG.MOVHD MH WHERE  MH.COD_LQ = g_cod_lq AND MH.COD_MF = g_cod_mf AND MH.COD_MV=1015;
                -- EGV 30SEP2015 Fin
            EXCEPTION
            WHEN OTHERS THEN
                NULL;
            END;
            
            IF  l_d1242_clasif = 1 THEN

               -- EGV 30SEP2015 Inicio
               l_indx_det := l_indx_det + 1;
               l_recibo_det_t.EXTEND;
               
               l_recibo_det_t(l_indx_det).cod_lq := g_cod_lq;
               l_recibo_det_t(l_indx_det).cod_emp :=g_cod_emp;
               l_recibo_det_t(l_indx_det).cod_mf :=g_cod_mf;
               l_recibo_det_t(l_indx_det).sec :=200+l_indx_det;
               l_recibo_det_t(l_indx_det).descrip :=  ' ';
               -- EGV 30SEP2015 Inicio

            
               l_indx_det := l_indx_det + 1;
               l_recibo_det_t.EXTEND;
               
               l_recibo_det_t(l_indx_det).cod_lq := g_cod_lq;
               l_recibo_det_t(l_indx_det).cod_emp :=g_cod_emp;
               l_recibo_det_t(l_indx_det).cod_mf :=g_cod_mf;
               l_recibo_det_t(l_indx_det).sec :=200+l_indx_det;
               l_recibo_det_t(l_indx_det).descrip :=  'Remuneración y/o Haber no sujeto al Impuesto a las Ganancias - Beneficio Decreto PEN 1242/2013';
               
            
            END IF;
            
            
            --> Completo los campos calculados de la cabecera
            
            l_neto := l_rem_suj_ret + l_con_no_grav - l_reten;
            l_neto_letras := PKG_BXSCITI_DATOS_RECIBO.numero_a_letras(l_neto);
            
            l_recibo_mf_t (1).rem_suj_ret := l_rem_suj_ret;
            l_recibo_mf_t (1).con_no_grav := l_con_no_grav;
            l_recibo_mf_t (1).reten := l_reten;
            l_recibo_mf_t (1).neto := l_neto;
            l_recibo_mf_t (1).neto_letras := l_neto_letras;
            l_recibo_mf_t (1).gan_liq := l_monto6678;
            l_recibo_mf_t (1).gan_rec := l_monto6678;
            
            
            --> Completo los campos del dorso del recibo

            l_recibo_dor_t := l_recibo_dor_list ();
            
               l_indx_dor := l_indx_dor + 1;
               l_recibo_dor_t.EXTEND;
               
               l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
               l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
               l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
               l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
               l_recibo_dor_t (l_indx_dor).descrip := '                        Listado Detallado Ganancias                         Generado : ' || to_date(SYSDATE,'DD/MM/YY');                   


               l_indx_dor := l_indx_dor + 1;
               l_recibo_dor_t.EXTEND;
               
               l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
               l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
               l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
               l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
               l_recibo_dor_t (l_indx_dor).descrip := 'Empleado :       '||to_char(g_cod_mf)||', '||trim(g_nom_mf);



            l_espacios_dorso := '';         -- EGV 16OCT2015

            --> ignoro los registros del detalle mensual y grossing
            FOR X IN CUR_DATOS_DOR (g_cod_mf)
            LOOP
               -- EGV 16OCT2015 Inicio
               /*
               IF UPPER(X.SOB_LINEA) like 'TOTAL%' then
                l_mesanio := 0;
               END IF;
               IF l_mesanio = 0 THEN
                   l_indx_dor := l_indx_dor + 1;
                   l_recibo_dor_t.EXTEND;
                   
                   l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
                   l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
                   l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
                   l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
                   IF UPPER(X.SOB_LINEA) like 'MES/AÑO%' then
                    l_mesanio := 1;
                   END IF;
                   
                    --> Se remplaza la negrita por mayúsuclas
                   IF UPPER(X.SOB_LINEA) like 'MES/AÑO%' OR UPPER(X.SOB_LINEA) LIKE 'COBRADO%' OR UPPER(X.SOB_LINEA) LIKE 'MONTO%' OR UPPER(X.SOB_LINEA) LIKE 'DEDUCCIONES FIJAS%' OR UPPER(X.SOB_LINEA) LIKE 'TOTAL DEDUCCIONES%' THEN
                    -- EGV 25Ago2015 Inicio
                    --l_recibo_dor_t (l_indx_dor).descrip := REPLACE(UPPER(X.SOB_LINEA),'MES/AÑO','');
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(UPPER(X.SOB_LINEA),'MES/AÑO','       ');
                    -- EGV 25Ago2015 Fin
                   ELSIF UPPER(X.SOB_LINEA) like 'GANANCIA NETA%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Ganancia Neta','GANANCIA NETA');
                   ELSIF UPPER(X.SOB_LINEA) like 'ESCALA A%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Escala a Aplicar','ESCALA A APLICAR');   
                   ELSIF UPPER(X.SOB_LINEA) like 'IMPORTE :%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Importe','IMPORTE');     
                   ELSIF UPPER(X.SOB_LINEA) like 'IMPORTE GANANCIAS%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Importe Ganancias','IMPORTE GANANCIAS'); 
                   ELSE                                         
                    l_recibo_dor_t (l_indx_dor).descrip := X.SOB_LINEA;
                   END IF;
               END IF;
               */
               
               IF UPPER(NVL(X.SOB_LINEA,' ')) NOT LIKE 'LIQUIDACIÓN :%' THEN
               
                   l_indx_dor := l_indx_dor + 1;
                   l_recibo_dor_t.EXTEND;
                           
                   l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
                   l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
                   l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
                   l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
                           
                   IF UPPER(X.SOB_LINEA) like 'MES/AÑO%' then
                    l_recibo_dor_t (l_indx_dor).descrip := 'Periodo          Haberes     Descuentos    Haberes.Pr      Desc.Pr       Neto.Pr       Reg.Prom      Ret.4ta'; 
                   ELSIF UPPER(X.SOB_LINEA) like 'GANANCIA NETA%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Ganancia Neta','GANANCIA NETA');
                    l_espacios_dorso := '                          ';
                   ELSIF UPPER(X.SOB_LINEA) like 'ESCALA A%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Escala a Aplicar','ESCALA A APLICAR');
                    l_espacios_dorso := '                          ';   
                   ELSIF UPPER(X.SOB_LINEA) like 'IMPORTE :%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Importe','IMPORTE');
                    l_espacios_dorso := '                            ';     
                   ELSIF UPPER(X.SOB_LINEA) like 'IMPORTE GANANCIAS%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := REPLACE(X.SOB_LINEA,'Importe Ganancias','IMPORTE GANANCIAS');
                    l_espacios_dorso := '                      ';
                   ELSIF UPPER(X.SOB_LINEA) like 'AC. GR:%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := replace(X.SOB_LINEA,'Ac. GR:','Ac. GR:     ');
                   ELSIF UPPER(X.SOB_LINEA) like 'TOTAL DEDUCCIONES ESPECIALES :%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := replace(X.SOB_LINEA,'Total deducciones especiales :','TOTAL DEDUCCIONES ESPECIALES :                      ');
                   ELSIF UPPER(X.SOB_LINEA) LIKE 'COBRADO%' OR UPPER(X.SOB_LINEA) LIKE 'MONTO%' OR UPPER(X.SOB_LINEA) LIKE 'DEDUCCIONES FIJAS%' THEN
                    l_recibo_dor_t (l_indx_dor).descrip := upper(X.SOB_LINEA); 
                   ELSE                                         
                    l_recibo_dor_t (l_indx_dor).descrip := l_espacios_dorso || X.SOB_LINEA;
                    l_espacios_dorso := '';
                   END IF;
               
                   l_recibo_dor_t (l_indx_dor).descrip := REPLACE(l_recibo_dor_t (l_indx_dor).descrip,'é','e');
               
               END IF;                       
               -- EGV 16OCT2015 Fin
            END LOOP;
            
           -- EGV 16OCT2015 Inicio 
           l_indx_dor := l_indx_dor + 1;
           l_recibo_dor_t.EXTEND;
           l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
           l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
           l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
           l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
           
           
           l_impuesto_ret_mf := 0;
           
           FOR l_idx IN l_recibo_det_t.FIRST .. l_recibo_det_t.LAST
           LOOP
             l_impuesto_ret_mf := l_impuesto_ret_mf + (NVL(l_recibo_det_t(l_idx).RETEN,0) * SumaGanancias(l_recibo_det_t(l_idx).cod_mv));
           END LOOP;
           

           l_indx_dor := l_indx_dor + 1;
           l_recibo_dor_t.EXTEND;
           l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
           l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
           l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
           l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
           l_recibo_dor_t (l_indx_dor).descrip := 'IMPUESTO RETENIDO DEL MES :    ' || TO_CHAR(NVL(l_impuesto_ret_mf,0),'999,999,990.00');


           l_segur_mf := TraeSegur();
           l_medic_mf := TraeMedic();
           
           IF l_segur_mf <> 0 THEN
               l_indx_dor := l_indx_dor + 1;
               l_recibo_dor_t.EXTEND;
               l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
               l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
               l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
               l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
               l_recibo_dor_t (l_indx_dor).descrip := '* Seguro de vida automatico:   ' || TO_CHAR(l_segur_mf,'999,999,999.00') ;
           END IF;

           IF l_medic_mf <> 0 THEN
               l_indx_dor := l_indx_dor + 1;
               l_recibo_dor_t.EXTEND;
               l_recibo_dor_t (l_indx_dor).cod_lq := g_cod_lq;
               l_recibo_dor_t (l_indx_dor).cod_emp := g_cod_emp;
               l_recibo_dor_t (l_indx_dor).cod_mf := g_cod_mf;
               l_recibo_dor_t (l_indx_dor).sec :=  l_indx_dor;
               l_recibo_dor_t (l_indx_dor).descrip := '* Medicina prepaga automatica: ' || TO_CHAR(l_medic_mf,'999,999,999.00');
           END IF;
           -- EGV 16OCT2015 Fin
         
                      

            IF l_indx_dor = 0
            THEN
               g_estado_det := 'E';
               g_descr_det :=
                     'No existen datos para el dorso '
                  || p_cod_lq
                  || ' Empleado '
                  || g_cod_mf;
               RAISE NEXT_VAL;
            END IF;


            --> Inserto en las tablas fisicas todos los datos del empleado

            -->Cabecera
            INSERT INTO BASEARG.CB_REC_MF
                 VALUES l_recibo_mf_t (1);

            -->Detalle
            FORALL j IN l_recibo_det_t.FIRST .. l_recibo_det_t.LAST
               INSERT INTO BASEARG.CB_REC_DET
                    VALUES l_recibo_det_t (j);


            -->Dorso
            FORALL k IN l_recibo_dor_t.FIRST .. l_recibo_dor_t.LAST
               INSERT INTO BASEARG.CB_REC_DOR
                    VALUES l_recibo_dor_t (k);

            COMMIT;
            --DBMS_OUTPUT.PUT_LINE ('COMMIT');
            
         EXCEPTION
            WHEN NEXT_VAL
            THEN
               ROLLBACK TO SAVEPOINT INICIO;
               --DBMS_OUTPUT.PUT_LINE ('ROLLBACK');

               l_errores := l_errores + 1;
               g_resultado := NULL;


               BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                              g_proc_int,
                                                              g_estado_det,
                                                              g_descr_det,
                                                              g_resultado);
         END;
      END LOOP;
      
     -- EGV 19OCT2015 Inicio
     IF p_legajos IS NULL THEN
         BEGIN
         
         l_num_rec := 0;
         
         FOR rec in CUR_NUM_REC(g_cod_lq)
    
         LOOP
            
             l_num_rec := l_num_rec + 1;

             UPDATE CB_REC_MF
             SET NUM_REC = l_num_rec          
             WHERE COD_LQ = g_cod_lq
             AND COD_EMP = rec.COD_EMP
             AND COD_MF = rec.COD_MF;
                
         END LOOP;
         
         COMMIT;
         
         
         EXCEPTION
            WHEN OTHERS THEN
               l_errores := l_errores + 1;
               g_resultado := NULL;

               BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                              g_proc_int,
                                                              g_estado_det,
                                                              'Error al asignar el numero de Recibo.',
                                                              g_resultado);
               BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                              g_proc_int,
                                                              g_estado_det,
                                                              SUBSTR(SQLERRM, 1, 254),
                                                              g_resultado);                                                      
                                                                                                                    
         END;
     END IF;
     -- EGV 19OCT2015 Fin

      IF l_errores = 0
      THEN
         -- EGV 18Ago2015 Inicio
         --p_resultado :=
         --   'Proceso de generación de datos para recibos de sueldo finalizo correctamente';
         p_resultado :=
            'OK';         
         -- EGV 18Ago2015 Fin
         g_estado := 'C';
      ELSE
         p_resultado :=
            'Proceso de generación de datos para recibos de sueldo finalizo con errores';
         g_estado := 'E';
      END IF;

      BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                                     g_id_proc,
                                                     l_cod_proc,
                                                     l_descr,
                                                     g_estado,
                                                     g_resultado);
   EXCEPTION
      WHEN CUSTOM
      THEN
         g_estado := 'E';
         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                                        g_id_proc,
                                                        l_cod_proc,
                                                        l_descr,
                                                        g_estado,
                                                        g_resultado);
      WHEN OTHERS
      THEN
         p_resultado :=
            'Error en generación de datos para recibos de sueldo para la liquidacion : '
            || p_cod_lq
            || ' Error : '
            || SQLCODE
            || ' -- '
            || SQLERRM;
         g_estado := 'E';
         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                                        g_id_proc,
                                                        l_cod_proc,
                                                        l_descr,
                                                        g_estado,
                                                        g_resultado);
   END;

   PROCEDURE obtener_datos_liquidacion (p_resultado OUT VARCHAR2)
   IS
      l_mes   VARCHAR2 (10) :=' ';
   BEGIN
      p_resultado := 'C';

      SELECT L.FDES_LQ,
             TRIM(L.DESC_LQ),
             L.COD_PR,
             TRIM(P.DESC_PR),
             L.COD_EMP,
             L.FDES_LQ,
             L.FHAS_LQ,
             L.FPAGO_LQ,
             L.ACUM_LQ
        INTO g_fec_lq,
             g_desc_lq,
             g_cod_pr,
             g_desc_pr,
             g_cod_emp,
             g_fec_desde,
             g_fec_hasta,
             g_fec_pago,
             g_fec_acum
       -- EGV 18AGO2015 Inicio
       -- FROM BASEARG.LIQUIDAC L, BASEARG.PROCESOS P
       --WHERE L.COD_PR = P.COD_PR AND L.COD_LQ = g_cod_lq;
        FROM BASEARG.LIQUIDAC L, BASEARG.PROCESOS_TBL P
       WHERE L.COD_PR = P.COD_PR AND L.COD_LQ = g_cod_lq
       AND P.FECHA_EFECTIVA =
               (SELECT   MAX (P2.FECHA_EFECTIVA)
                  FROM   PROCESOS_TBL P2
                 WHERE   P2.COD_PR = P.COD_PR
                         AND P2.FECHA_EFECTIVA <= L.FHAS_LQ)
            AND P.BAJA_EFECTIVA IS NULL;       
       -- EGV 18AGO2015 Fin

      CASE EXTRACT (MONTH FROM g_fec_lq)
         WHEN 1
         THEN
            l_mes := 'Enero';
         WHEN 2
         THEN
            l_mes := 'Febrero';
         WHEN 3
         THEN
            l_mes := 'Marzo';
         WHEN 4
         THEN
            l_mes := 'Abril';
         WHEN 5
         THEN
            l_mes := 'Mayo';
         WHEN 6
         THEN
            l_mes := 'Junio';
         WHEN 7
         THEN
            l_mes := 'Julio';
         WHEN 8
         THEN
            l_mes := 'Agosto';
         WHEN 9
         THEN
            l_mes := 'Septiembre';
         WHEN 10
         THEN
            l_mes := 'Octubre';
         WHEN 11
         THEN
            l_mes := 'Noviembre';
         WHEN 12
         THEN
            l_mes := 'Diciembre';
      END CASE;

      g_per_liq := CONCAT (l_mes, TO_CHAR (g_fec_lq, ' YYYY'));
      
   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado := 'E';
      
   END obtener_datos_liquidacion;

   PROCEDURE obtener_datos_empresa (p_resultado OUT VARCHAR2)
   IS
   BEGIN
      p_resultado := 'C';

      SELECT TRIM (E.NOM_EMP),
             TRIM (E.DIRECCION),
             TRIM (E.CUIT),
             TRIM (E.REGION),
             TRIM (E.COMPANY),
             TRIM (E.CONVENIO)
        INTO g_nom_emp,
             g_dire_emp,
             g_cuit_emp,
             g_pais_emp,
             g_comp_emp,
             g_convenio_emp
        FROM QSEMPRESA E
       WHERE E.COD_EMP = g_cod_emp;

      IF g_pais_emp <> 'URY'
      THEN
         g_cuit_emp := 'CUIT: ' || g_cuit_emp;
      ELSE
         g_cuit_emp := 'RUC: ' || g_cuit_emp;

         CASE g_comp_emp
            WHEN 'CUY'
            THEN
               g_bps_emp := '5252921';
               g_bse_emp := '86278';
            WHEN 'FUY'
            THEN
               g_bps_emp := '3512966';
               g_bse_emp := ' ';
            WHEN 'UUY'
            THEN
               g_bps_emp := '5158396';
               g_bse_emp := '150805';
               g_dire_emp := 'Colonia 1329';
            ELSE
               g_bps_emp := '5381103';
               g_bse_emp := '483871';
         END CASE;
      END IF;
   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado := 'E';
   END obtener_datos_empresa;

   PROCEDURE obtener_descr_categoria (p_resultado   OUT VARCHAR2,
                                      p_desc_cat    OUT VARCHAR2)
   IS
      l_desc_cat   VARCHAR2 (30);
      l_anos_ant   VARCHAR (10);
      l_anos_ant_num    INTEGER;        -- EGV 16OCT2015
   BEGIN
      p_resultado := 'C';

      SELECT TRIM (CC.DESCRIP)
        INTO l_desc_cat
        FROM CB_CATCNV CC
       WHERE CC.QS_CONVENIO = g_convenio_emp
             AND CC.CAT_CONVE = g_cat_conve_mf;

      IF INSTR (l_desc_cat, 'INICIAL', 1) > 0
      THEN
         l_desc_cat :=
            SUBSTR (l_desc_cat, 1, INSTR (l_desc_cat, 'INICIAL', 1) - 1);
      END IF;


      IF g_cat_conve_mf = '100'
      THEN
         l_desc_cat := 'AUX.ADM.';
      END IF;


      -- EGV 16OCT2015 Inicio
      /*
      IF g_cat_conve_mf = '100' AND l_anos_ant > 24
      THEN
         IF l_anos_ant < 30
         THEN
            l_desc_cat := '2° Jefe de División de 3era';
         ELSE
            IF l_anos_ant < 35
            THEN
               l_desc_cat := '2° Jefe de División de 2da';
            ELSE
               l_desc_cat := '2° Jefe de División de 1era';
            END IF;
         END IF;
      END IF;
      */
      l_anos_ant_num := 0;
      -- EGV 16OCT2015 Fin


      IF (g_cat_conve_mf = '100')
         OR (g_convenio_emp = 'COME' AND g_cat_conve_mf NOT IN ('053', '054'))
      THEN
         -- EGV 16OCT2015 Inicio
         /*
         l_anos_ant := TRUNC (MONTHS_BETWEEN (g_fec_hasta, g_fec_ant_mf) / 12);

         IF l_anos_ant > 1
         THEN
            l_anos_ant := ' - ' || l_anos_ant || ' años';
         ELSE
            IF l_anos_ant = 1
            THEN
               l_anos_ant := ' - ' || l_anos_ant || ' año';
            ELSE
               l_anos_ant := ' - INICIAL';
            END IF;
         END IF;
         */
         l_anos_ant_num := TRUNC (MONTHS_BETWEEN (g_fec_hasta, g_fec_ant_mf) / 12);
         
         IF l_anos_ant_num > 1
         THEN
            l_anos_ant := ' - ' || l_anos_ant_num || ' años';
         ELSE
            IF l_anos_ant_num = 1
            THEN
               l_anos_ant := ' - ' || l_anos_ant_num || ' año';
            ELSE
               l_anos_ant := ' - INICIAL';
            END IF;
         END IF;         
         -- EGV 16OCT2015 Fin
      ELSE
         l_anos_ant := ' ';
      END IF;

      p_desc_cat := l_desc_cat || l_anos_ant;
      
      -- EGV 16OCT2015 Inicio
      IF g_cat_conve_mf = '100' AND l_anos_ant_num > 24
      THEN
         IF l_anos_ant_num < 30
         THEN
            p_desc_cat := '2° Jefe de División de 3era';
         ELSE
            IF l_anos_ant_num < 35
            THEN
               p_desc_cat := '2° Jefe de División de 2da';
            ELSE
               p_desc_cat := '2° Jefe de División de 1era';
            END IF;
         END IF;
      END IF;
      -- EGV 16OCT2015 Fin      
      
   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado := 'E';
   END;
 function numero_a_letras(p_numero in number) return varchar2 is



        fuera_de_rango EXCEPTION;

        millares_de_millon number;
        millones number;
        millares number;
        centenas number;
        centimos number;

        en_letras varchar2(200);
        entero number;

        aux varchar2(15);
        
        l_numero        number;    -- EGV 19OCT2015
        
    begin

        -- EGV 19OCT2015 Inicio
        --if p_numero < 0 or p_numero > 999999999999.99 then
        if p_numero > 999999999999.99 then
        -- EGV 19OCT2015 Fin
            raise fuera_de_rango;
        end if;
        
        -- EGV 19OCT2015 Inicio
        l_numero := p_numero;
        
        IF l_numero < 0 THEN
            l_numero := l_numero * -1;
        END IF;
        -- EGV 19OCT2015 Fin
        
        -- EGV 19OCT2015 Inicio
        --entero := trunc(p_numero);
        entero := trunc(l_numero);
        -- EGV 19OCT2015 Fin

        millares_de_millon := trunc(entero / 1000000000);

        millones := trunc((entero mod 1000000000) / 1000000);

        millares := trunc((entero mod 1000000) / 1000);

        centenas := entero mod 1000;

        -- EGV 19OCT2015 Inicio
        --centimos := (round(p_numero,2) * 100) mod 100;
        centimos := (round(l_numero,2) * 100) mod 100;
        -- EGV 19OCT2015 Fin


        -- MILLARES DE MILLON
        if millares_de_millon = 1 then
            if millones = 0 then
                en_letras := 'mil millones ';
            else
                en_letras := 'mil ';
            end if;
        elsif millares_de_millon > 1 then

            en_letras := PKG_BXSCITI_DATOS_RECIBO.numero_menor_mil(millares_de_millon);

            if millones = 0 then
                en_letras := en_letras || 'mil millones ';
            else
                en_letras := en_letras || 'mil ';
            end if;
        end if;

        -- MILLONES
        if millones = 1 and  millares_de_millon = 0 then
            en_letras := 'un millón ';
        elsif millones > 0 then
            en_letras := en_letras || PKG_BXSCITI_DATOS_RECIBO.numero_menor_mil(millones) || 'millones ';
        end if;

        -- MILLARES
        if millares = 1 and millares_de_millon = 0 and millones = 0 then
            en_letras := 'mil ';
        elsif millares > 0 then
            en_letras := en_letras || PKG_BXSCITI_DATOS_RECIBO.numero_menor_mil(millares) || 'mil ';
        end if;

        -- CENTENAS
        if centenas > 0 or (entero = 0 and centimos = 0) then
            en_letras := en_letras || PKG_BXSCITI_DATOS_RECIBO.numero_menor_mil(centenas);
        end if;

        if centimos > 0 then
            if centimos = 1 then
                aux := 'centavo';
            else
                aux := 'centavos';
            end if;
            if entero > 0 then
                en_letras := en_letras || 'con ' || replace(PKG_BXSCITI_DATOS_RECIBO.numero_menor_mil(centimos),'uno ','un ') || aux;
            else
                en_letras := en_letras || replace(PKG_BXSCITI_DATOS_RECIBO.numero_menor_mil(centimos),'uno','un') || aux;
            end if;
        end if;
        
        -- EGV 19OCT2015 Inicio
        IF p_numero < 0 THEN
            en_letras := '-' || en_letras;
        END IF;
        -- EGV 19OCT2015 Fin

        return(en_letras);


    EXCEPTION
        when fuera_de_rango then
            return('Error: entrada fuera de rango');
        when others then
            raise;
    end;

  function numero_menor_mil(p_numero in number) return varchar2 is


        fuera_de_rango EXCEPTION;
        no_entero EXCEPTION;

        centenas number;
        decenas number;
        unidades number;

        en_letras varchar2(100);
        unir varchar2(2);

    begin

        if trunc(p_numero) <> p_numero then
            raise no_entero;
        end if;

        if p_numero < 0 or p_numero > 999 then
            raise fuera_de_rango;
        end if;


        if p_numero = 100 then
            return ('cien ');
        elsif p_numero = 0 then
            return ('cero ');
        elsif p_numero = 1 then
            return ('uno ');
        else
            centenas := trunc(p_numero / 100);
            decenas  := trunc((p_numero mod 100)/10);
            unidades := p_numero mod 10;
            unir := 'y ';

            -- CENTENAS
            if centenas = 1 then
                en_letras := 'ciento ';
            elsif centenas = 2 then
                en_letras := 'doscientos ';
            elsif centenas = 3 then
                en_letras := 'trescientos ';
            elsif centenas = 4 then
                en_letras := 'cuatrocientos ';
            elsif centenas = 5 then
                en_letras := 'quinientos ';
            elsif centenas = 6 then
                en_letras := 'seiscientos ';
            elsif centenas = 7 then
                en_letras := 'setecientos ';
            elsif centenas = 8 then
                en_letras := 'ochocientos ';
            elsif centenas = 9 then
                en_letras := 'novecientos ';
            end if;



            -- DECENAS
            if decenas = 3 then
                en_letras := en_letras || 'treinta ';
            elsif decenas = 4 then
                en_letras := en_letras || 'cuarenta ';
            elsif decenas = 5 then
                en_letras := en_letras || 'cincuenta ';
            elsif decenas = 6 then
                en_letras := en_letras || 'sesenta ';
            elsif decenas = 7 then
                en_letras := en_letras || 'setenta ';
            elsif decenas = 8 then
                en_letras := en_letras || 'ochenta ';
            elsif decenas = 9 then
                en_letras := en_letras || 'noventa ';
            elsif decenas = 1 then
                if unidades < 6 then
                    if unidades = 0 then
                        en_letras := en_letras || 'diez ';
                    elsif unidades = 1 then
                        en_letras := en_letras || 'once ';
                    elsif unidades = 2 then
                        en_letras := en_letras || 'doce ';
                    elsif unidades = 3 then
                        en_letras := en_letras || 'trece ';
                    elsif unidades = 4 then
                        en_letras := en_letras || 'catorce ';
                    elsif unidades = 5 then
                        en_letras := en_letras || 'quince ';
                    end if;
                    unidades := 0;
                else
                    en_letras := en_letras || 'dieci';
                    unir := null;
                end if;
            elsif decenas = 2 then
                if unidades = 0 then
                    en_letras := en_letras || 'veinte ';
                else
                    en_letras := en_letras || 'veinti';
                end if;
                unir := null;
            elsif decenas = 0 then
                unir := null;
            end if;

            -- UNIDADES
            if unidades = 1 then
                en_letras := en_letras || unir || 'uno ';
            elsif unidades = 2 then
                en_letras := en_letras || unir || 'dos ';
            elsif unidades = 3 then
                en_letras := en_letras || unir || 'tres ';
            elsif unidades = 4 then
                en_letras := en_letras || unir || 'cuatro ';
            elsif unidades = 5 then
                en_letras := en_letras || unir || 'cinco ';
            elsif unidades = 6 then
                en_letras := en_letras || unir || 'seis ';
            elsif unidades = 7 then
                en_letras := en_letras || unir || 'siete ';
            elsif unidades = 8 then
                en_letras := en_letras || unir || 'ocho ';
            elsif unidades = 9 then
                en_letras := en_letras || unir || 'nueve ';
            end if;
        end if;

        return(en_letras);

    EXCEPTION
        when no_entero then
            return('Error: entrada no es un número entero');
        when fuera_de_rango then
            return('Error: entrada fuera de rango');
        when others then
            raise;

    end;
    
    
    -- EGV 16OCT2015 Inicio
    function SumaGanancias(l_conc in number) return integer is
        l_salida integer;
    begin
        
        l_salida := 0;
        
        begin
        select decode(a.signo,'-',-1,1)
            Into l_salida 
        from acumulan_tbl a
        where a.fecha_efectiva = (select max(fecha_efectiva) from acumulan_tbl where acumulador = a.acumulador and fecha_efectiva <= g_fec_hasta)
        and a.cod_mv = l_conc
        and a.acumulador = 'x9';
        exception
            when others then
                null;
        end;
    
        return l_salida;
    end;
    
    function TraeSegur return number is
        l_salida number;
    begin
        
        l_salida := 0;
        
        begin
        select sum(IMPTOT_HD)
            Into l_salida
        from movhd
        where cod_mf = g_cod_mf
        and cod_lq = g_cod_lq
        and cod_mv in (6601,6599);
        exception
            when others then
                null;
        end;
        
        return l_salida;    
    
    end; 
    
    function TraeMedic return number is
        l_salida number;
    begin
        
        l_salida := 0;
        
        begin
        select sum(IMPTOT_HD)
            Into l_salida
        from movhd
        where cod_mf = g_cod_mf
        and cod_lq = g_cod_lq
        and cod_mv in (6618, 6619, 6620, 6622, 6623);
        exception
            when others then
                null;
        end;
        
        return l_salida;    
    
    end;    
    -- EGV 16OCT2015 Fin      

END PKG_BXSCITI_DATOS_RECIBO; 
/
