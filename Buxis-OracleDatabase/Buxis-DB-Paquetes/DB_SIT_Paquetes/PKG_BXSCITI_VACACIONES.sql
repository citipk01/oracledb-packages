CREATE OR REPLACE PACKAGE         PKG_BXSCITI_VACACIONES
AS
   /******************************************************************************
    NAME: BASEARG.PKG_BXSCITI_VACACIONES
    PURPOSE:

    REVISIONS:
    Ver Date Author Description
    --------- ---------- --------------- ------------------------------------
    1.0  30/04/2015 CSA 1.  Se procesan los datos para la generacion de las vacaciones
   ******************************************************************************/


        /*g_id_proc      INTEGER;
        g_proc_int     VARCHAR (100);
        g_descr_det    VARCHAR2 (100);
        g_estado       VARCHAR2 (2);
        g_estado_det   VARCHAR2 (254);
        g_resultado    VARCHAR2 (50);
        g_errores      INTEGER := 0;*/
   
       --> Variables Empleado
   
        /*g_cod_mf           INTEGER := 0;
        g_emp_mf           INTEGER := 0;
        g_nom_mf        VARCHAR (50);
        g_cat_conve_mf VARCHAR2(3);
        g_cb_geid_mf VARCHAR2(11);
        g_respons_mf VARCHAR2(5);
        g_fec_ing_vl_mf DATE;
        g_fec_hire_mf DATE;
        g_fec_ant_mf DATE;
        g_cta_acredit_mf  VARCHAR(17);
        g_cuil_mf VARCHAR2(20);
        g_tipo_doc_mf VARCHAR2(4);
        g_num_doc_mf VARCHAR2(15);
        g_osocial_mf VARCHAR(6); 
        g_secc_mf VARCHAR2 (5);
        g_fpago_mf VARCHAR2(4);
        g_grado_mf  VARCHAR2(3);         -- EGV 16OCT2015
        g_full_part_mf CHAR(1);         -- EGV 16OCT2015*/
        
   
      --> Variables Datos Liquidacion
      
      /*g_cod_lq INTEGER;
      g_fec_lq         DATE;
      g_desc_lq        VARCHAR2 (50);
      g_cod_pr          VARCHAR(5);
      g_desc_pr        VARCHAR2 (30);
      g_cod_emp        INTEGER;
      g_fec_desde      DATE;
      g_fec_hasta      DATE;
      g_fec_pago       DATE;
      g_fec_acum       DATE;
      g_per_liq        VARCHAR2 (15);*/

      --> Variables Empresa
      /*g_nom_emp        VARCHAR2 (30);
      g_dire_emp       VARCHAR2 (35);
      g_cuit_emp       VARCHAR2 (20);
      g_pais_emp       VARCHAR2 (5);
      g_comp_emp       VARCHAR2 (3);
      g_convenio_emp   VARCHAR2 (4);
      g_bse_emp        VARCHAR2 (15) := '';
      g_bps_emp        VARCHAR2 (15) := '';*/
      
       l_tipo_proc     VARCHAR2(5);        -- T: Recalcula Todo
                                        -- N: Calcula solo los nuevos (los que no tienen calculado vacaciones para el periodo)

    l_fecha_vac     VARCHAR2(10);--DATE;
    
    l_cod_emp       VARCHAR2(10);
    p_id_proc INTEGER;
    
    l_cod_mf        CB_VAC.COD_MF%TYPE;
    l_grado         CB_GRADO.GRADO%TYPE;
    l_fec_antig     DATE;
    l_meses         INTEGER;
    l_dias_acord    INTEGER;
    l_dias_calc     INTEGER;
    l_max_frag_calc INTEGER;
    l_tomados_calc  INTEGER;
      

   PROCEDURE datos_vacaciones (l_tipo_proc       IN     VARCHAR2,
                            --l_fecha_vac   IN     DATE,
                            l_cod_emp      IN     VARCHAR2,
                            l_fecha_vac   IN     VARCHAR2,
                            --l_cod_emp      IN     INTEGER,
                            p_id_proc      IN     INTEGER,
                            p_resultado       OUT VARCHAR2);    
END PKG_BXSCITI_VACACIONES; 
/


CREATE OR REPLACE PACKAGE BODY         PKG_BXSCITI_VACACIONES 
AS
   /******************************************************************************
   NAME: BASEARG.PKG_BXSCITI_VACACIONES
   PURPOSE:

   REVISIONS:
   Ver Date Author Description
   --------- ---------- --------------- ------------------------------------
   1.0  30/04/2015 CSA 1.  Se procesan los datos para las vacaciones
   1.1  17/05/2015 EGV  Correcciones varias
   ******************************************************************************/

   PROCEDURE datos_vacaciones (l_tipo_proc       IN     VARCHAR2,
                            --l_fecha_vac   IN     DATE,
                            l_cod_emp      IN     VARCHAR2,
                            l_fecha_vac   IN     VARCHAR2,
                            p_id_proc      IN     INTEGER,
                            --l_cod_emp      IN     INTEGER,
                            p_resultado       OUT VARCHAR2) 
   IS
  l_errores          INTEGER := 0;

    CURSOR ANTIG_FUNC(l_emp integer, l_fec date, l_tipo VARCHAR2)
        IS
    SELECT M.COD_MF, M.GRADO, M.FEC_ANTIG, TRUNC(MONTHS_BETWEEN(l_fec,FEC_ANTIG)) MESES, NVL(M.CBHR_DIAS_ACORD,0) CBHR_DIAS_ACORD
    FROM MAEFUNC_TBL M
    WHERE M.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM MAEFUNC_TBL WHERE COD_MF = M.COD_MF AND FECHA_EFECTIVA <= l_fec)
    AND M.SEC_EFECTIVA = (SELECT MAX(SEC_EFECTIVA) FROM MAEFUNC_TBL WHERE COD_MF = M.COD_MF AND FECHA_EFECTIVA = M.FECHA_EFECTIVA)
    AND (M.FEC_EGR_MF IS NULL OR M.FEC_EGR_MF BETWEEN to_date('01/10/'|| to_char(l_fec,'yyyy'),'dd/mm/yyyy') AND l_fec)
    AND M.COD_EMP = l_emp
    AND (l_tipo = 'T' OR (l_tipo <> 'T' AND NOT EXISTS (SELECT 1 FROM CB_VAC Z WHERE Z.COD_MF = M.COD_MF AND Z.COD_EMP = M.COD_EMP AND Z.PERIODO = l_fec))) 
    ORDER BY M.COD_MF;
    
    
BEGIN

    --l_tipo_proc := 'T';
    --l_fecha_vac = to_date(l_fecha_vac,'dd/mm/yy');
    --l_cod_emp := 1;
    
    --IF TO_CHAR(l_fecha_vac,'DD/MM') <> '31/12' THEN
    --    RAISE 'Fecha del periodo de vacaciones debe ser fin de año.';
    --END IF;
    
    --SAVEPOINT VAC;
    
    BEGIN    
    
    --insert into cb_sucursal (COD_SUC, FECHA_EFECTIVA, DIRECCION) VALUES ('TEST1',SYSDATE,l_fecha_vac);
      --insert into cb_area (CB_AREA, DESCRIP, DESCRIP_CORTA, LAST_USR, LAST_HOST) VALUES ('999',l_fecha_vac,l_tipo_proc, p_id_proc,l_cod_emp);
    COMMIT;
    
        IF l_tipo_proc = 'T' THEN
        
            INSERT INTO A_CB_VAC(QS_A_FECHA, QS_A_USUARIO, QS_A_HOST, QS_A_ACCION, COD_MF, PERIODO, COD_EMP, DIAS_ARRASTRE, DIAS_CALCULADOS, DIAS_TOMADOS, MAX_FRAG)
            SELECT SYSDATE, 'CICLOVAC', NULL, 'D', COD_MF, PERIODO, COD_EMP, DIAS_ARRASTRE, DIAS_CALCULADOS, DIAS_TOMADOS, MAX_FRAG
            FROM CB_VAC
            WHERE COD_EMP = l_cod_emp AND PERIODO = to_date(l_fecha_vac,'dd/mm/yy');
        
            DELETE FROM CB_VAC WHERE COD_EMP = l_cod_emp AND PERIODO = to_date(l_fecha_vac,'dd/mm/yy');
            
        END IF;
        
        FOR l_legajos in ANTIG_FUNC(l_cod_emp, to_date(l_fecha_vac,'dd/mm/yy'), l_tipo_proc)
        LOOP
        
            l_cod_mf := l_legajos.COD_MF;
            l_grado := l_legajos.GRADO;
            l_fec_antig := l_legajos.FEC_ANTIG;
            l_meses := l_legajos.MESES;
            l_dias_acord := l_legajos.CBHR_DIAS_ACORD;
            l_dias_calc := 0;
            l_max_frag_calc := 0;
            l_tomados_calc := 0;
            
            IF l_meses = 5 and to_char(l_fec_antig,'dd') in ('01','02','03','04','05') THEN
                l_meses := 6;
            END IF;
            
            -- Busca días calculados según grado y antigüedad        
            BEGIN
            SELECT nvl(A.DIAS_VAC,0), nvl(CASE WHEN A.DIAS_VAC <= 7 THEN 0 WHEN A.DIAS_VAC <= 17 THEN 1 WHEN A.DIAS_VAC <= 21 THEN 2 WHEN A.DIAS_VAC <= 24 THEN 3 ELSE 4 END,0) MAX_FRAG
                Into l_dias_calc, l_max_frag_calc
            FROM CB_GRADO_VAC A, CB_GRADO B
            WHERE A.GRADO = B.GRADO
            AND A.FECHA_EFECTIVA = B.FECHA_EFECTIVA
            AND B.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM CB_GRADO WHERE GRADO = B.GRADO AND FECHA_EFECTIVA <= to_date(l_fecha_vac,'dd/mm/yy'))
            AND B.BAJA_EFECTIVA IS NULL
            AND A.GRADO = l_grado
            AND A.ANTIG = (SELECT MAX(ANTIG) FROM CB_GRADO_VAC WHERE GRADO = A.GRADO AND FECHA_EFECTIVA = A.FECHA_EFECTIVA AND ANTIG <= l_meses);
             
            EXCEPTION
                WHEN OTHERS THEN
                    NULL;
            END;
            

            -- Busca los días tomados para el presente período
            BEGIN
            --SELECT NVL(SUM(DIA<ES),0)
            SELECT NVL(SUM(DIAS),0)
                Into l_tomados_calc
            FROM CB_VAC_SOL
            WHERE COD_MF = l_cod_mf
            AND PERIODO = to_date(l_fecha_vac,'dd/mm/yy')
            AND COD_EMP = l_cod_emp;
     
            EXCEPTION
                WHEN OTHERS THEN
                    NULL;
            END;        
            
            
            IF l_dias_acord > l_dias_calc THEN
                l_dias_calc := l_dias_acord;
            END IF;
            
            INSERT INTO CB_VAC(COD_MF, PERIODO, COD_EMP, DIAS_ARRASTRE, DIAS_CALCULADOS, DIAS_TOMADOS, MAX_FRAG, LAST_USR, LAST_HOST)
            VALUES(l_cod_mf, to_date(l_fecha_vac,'dd/mm/yy'), l_cod_emp, 0, l_dias_calc, l_tomados_calc, l_max_frag_calc, 'CICLOVAC', NULL);
            
        
        END LOOP;
        
    EXCEPTION
        WHEN OTHERS THEN
            l_errores := 1;
            p_resultado := NULL;
            --ROLLBACK TO VAC;
      /*      BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                              g_proc_int,
                                                              g_estado_det,
                                                              'Error al asignar el numero de Recibo.',
                                                              g_resultado);*/
               BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_det (p_id_proc,
                                                              0,
                                                              '0',
                                                              SUBSTR(SQLERRM, 1, 254),
                                                              p_resultado);    
    END; 
    
    IF l_errores = 0
      THEN
         p_resultado :=
            'OK';         
      ELSE
         p_resultado :=
            'Proceso de generación de vacaciones finalizo con errores';
      END IF;    
    
    COMMIT;

END;   
END PKG_BXSCITI_VACACIONES; 
/
