CREATE OR REPLACE PACKAGE         PKG_BXSCITI_DATOS_ACRED
AS
   /******************************************************************************
   NAME: BASEARG.PKG_BXSCITI_DATOS_ACRED
   PURPOSE:

   REVISIONS:
   Ver Date Author Description
   --------- ---------- --------------- ------------------------------------
   1.0  30/04/2015 CSA 1.  Se procesan los datos para la Acreditacion
   ******************************************************************************/


   g_id_proc          INTEGER;
   g_proc_int         VARCHAR (100);
   g_descr_det        VARCHAR2 (100);
   g_estado           VARCHAR2 (2);
   g_estado_det       VARCHAR2 (254);
   g_resultado        VARCHAR2 (50);
   g_errores          INTEGER := 0;

   g_cod_mf           INTEGER := 0;
   g_emp_mf           INTEGER := 0;
   g_banking_mf       VARCHAR2 (4);
   g_secc_mf          VARCHAR2 (5);
   -- EGV  10/03/2017 Inicio - Acreditacion por CBU a otros bancos
   --g_cta_acredit_mf   VARCHAR (17);
   g_cta_acredit_mf   VARCHAR (30);
   g_cuil_mf          VARCHAR2(15);
   -- EGV  10/03/2017 Fin
   g_fpago_mf         VARCHAR (4);



   g_rem_suj_ret      NUMBER := 0;
   g_con_no_grab      NUMBER := 0;
   g_retenciones      NUMBER := 0;
   g_neto             NUMBER := 0;
   g_por_banco        NUMBER := 0;
   g_efectivo         NUMBER := 0;
   g_monto_de         NUMBER := 0;
   g_contador         INTEGER := 0;
   g_insertados       INTEGER := 0;

   g_cod_lq           INTEGER;

   TYPE g_cod_lq_list IS TABLE OF INTEGER;

   TYPE g_fec_lq_list IS TABLE OF DATE;

   TYPE g_desc_lq_list IS TABLE OF VARCHAR2 (35);

   TYPE g_emp_lq_list IS TABLE OF INTEGER;

   TYPE g_d_emp_lq_list IS TABLE OF VARCHAR2 (100);

   g_cod_lq_t         g_cod_lq_list;
   g_fec_lq_t         g_fec_lq_list;
   g_desc_lq_t        g_desc_lq_list;
   g_emp_lq_t         g_emp_lq_list;
   g_d_emp_lq_t       g_d_emp_lq_list;


   TYPE g_cb_acredit_list IS TABLE OF BASEARG.CB_ACREDITA%ROWTYPE;

   g_cb_acredit_t     g_cb_acredit_list;


   PROCEDURE datos_acreditacion (p_cod_lq      IN     VARCHAR2,
                                 p_id_proc     IN     INTEGER,
                                 p_resultado      OUT VARCHAR2);

   PROCEDURE datos_liquidaciones (c_liquidaciones   IN     var_array,
                                  c_resultado          OUT VARCHAR2);

   PROCEDURE inserta_datos_acredit (c_cod_lq      IN     INTEGER,
                                    c_resultado      OUT VARCHAR2);
END PKG_BXSCITI_DATOS_ACRED; 
/


CREATE OR REPLACE PACKAGE BODY         PKG_BXSCITI_DATOS_ACRED
AS
   /******************************************************************************
   NAME: BASEARG.PKG_BXSCITI_DATOS_ACRED
   PURPOSE:

   REVISIONS:
   Ver Date Author Description
   --------- ---------- --------------- ------------------------------------
   1.0  30/04/2015 CSA 1.  Se procesan los datos para la Acreditacion
   1.1  17/05/2015 EGV  Correcciones varias
   ******************************************************************************/

   PROCEDURE datos_acreditacion (p_cod_lq      IN     VARCHAR2,
                                 p_id_proc     IN     INTEGER,
                                 p_resultado      OUT VARCHAR2)
   IS
      l_cod_proc        VARCHAR2 (50) := 'DATOS ACRED';
      l_descr           VARCHAR2 (50) := 'Proceso de datos de acreditaci�n';
      l_descr_det       VARCHAR2 (100);
      l_proc_int        VARCHAR2 (50);
      l_estado          VARCHAR2 (2);
      l_estado_det      VARCHAR2 (254);
      l_resultado       VARCHAR2 (50);


      l_contador        INTEGER := 0;
      l_insertados      INTEGER := 0;

      l_datos_mf        MAEFUNC_TBL%ROWTYPE;

      l_liquidaciones   var_array;

      TYPE l_cod_mf_list IS TABLE OF INTEGER;

      TYPE l_imptot_hd_list IS TABLE OF NUMBER;

      TYPE l_hades_hd_list IS TABLE OF VARCHAR2 (1);

      TYPE l_acumulador_list IS TABLE OF VARCHAR (3);

      l_cod_mf_t        l_cod_mf_list;
      l_imptot_hd_t     l_imptot_hd_list;
      l_hades_hd_t      l_hades_hd_list;
      l_acumulador_t    l_acumulador_list;

      CUSTOM            EXCEPTION;
      NEXT_VAL          EXCEPTION;
      ERRORES_ACRED     EXCEPTION;


      -- EGV 15OCT015 Inicio - Se readapta la consulta por performance
      /*
      CURSOR CUR_MOVHD (
         c_cod_lq INTEGER)
      IS
           SELECT MH.COD_MF,
                  MH.IMPTOT_HD,
                  MH.HADES_HD,
                  AC.ACUMULADOR
             FROM BASEARG.MOVHD MH,
                  -- EGV 21Jul2015 Inicio
                  --BASEARG.MOVCOD MC,
                  --BASEARG.ACUMULAN AC
                  BASEARG.LIQUIDAC LQ,
                  BASEARG.MOVCOD_TBL MC,
                  BASEARG.ACUMULAN_TBL AC
                  -- EGV 21Jul2015 Fin
            WHERE     MH.COD_LQ = c_cod_lq
                  AND MH.COD_LQ = LQ.COD_LQ         -- EGV 21Jul2015
                  AND MH.COD_MV = MC.COD_MV
                  -- EGV 21Jul2015 Inicio
                  AND MC.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM BASEARG.MOVCOD_TBL WHERE MC.COD_MV = COD_MV AND FECHA_EFECTIVA <= LQ.FHAS_LQ)
                  AND MC.BAJA_EFECTIVA IS NULL
                  -- EGV 21Jul2015 Fin
                  AND MH.COD_MV = AC.COD_MV
                  -- EGV 21Jul2015 Inicio
                  AND AC.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM BASEARG.ACUMULAN_TBL WHERE ACUMULADOR = AC.ACUMULADOR AND FECHA_EFECTIVA <= LQ.FHAS_LQ)
                  -- EGV 21Jul2015 Fin
                  AND (MC.INFORMA_MV = '0' OR AC.ACUMULADOR = 'deb')
                  -- EGV 21Jul2015 Inicio
                  --AND MH.COD_MV = AC.COD_MV
                  -- EGV 21Jul2015 Fin
                  AND AC.ACUMULADOR IN ('hcd', 'hsd', 'des', 'deb')
         ORDER BY MH.COD_MF;
         */
      CURSOR CUR_MOVHD (
         c_cod_lq INTEGER)
      IS
        SELECT MH.COD_MF,
            MH.IMPTOT_HD,
            MH.HADES_HD,
            AC.ACUMULADOR
        FROM BASEARG.MOVHD MH,
            BASEARG.LIQUIDAC LQ,
            BASEARG.ACUMULAN_TBL AC
        WHERE     MH.COD_LQ = c_cod_lq
            AND MH.COD_LQ = LQ.COD_LQ 
            AND MH.COD_MV = AC.COD_MV
            AND AC.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM BASEARG.ACUMULAN_TBL WHERE ACUMULADOR = AC.ACUMULADOR AND FECHA_EFECTIVA <= LQ.FHAS_LQ)
            AND AC.ACUMULADOR IN ('hcd', 'hsd', 'des')
            AND MH.COD_MV IN (SELECT MC.COD_MV FROM MOVCOD_TBL MC WHERE MH.COD_MV = MC.COD_MV AND MC.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM BASEARG.MOVCOD_TBL WHERE MC.COD_MV = COD_MV AND FECHA_EFECTIVA <= LQ.FHAS_LQ) AND MC.BAJA_EFECTIVA IS NULL AND MC.INFORMA_MV = '0') 
        UNION ALL
        SELECT MH.COD_MF,
            MH.IMPTOT_HD,
            MH.HADES_HD,
            AC.ACUMULADOR
        FROM BASEARG.MOVHD MH,
            BASEARG.LIQUIDAC LQ,
            BASEARG.ACUMULAN_TBL AC
        WHERE     MH.COD_LQ = c_cod_lq
            AND MH.COD_LQ = LQ.COD_LQ 
            AND MH.COD_MV = AC.COD_MV
            AND AC.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM BASEARG.ACUMULAN_TBL WHERE ACUMULADOR = AC.ACUMULADOR AND FECHA_EFECTIVA <= LQ.FHAS_LQ)
            AND AC.ACUMULADOR = 'deb'
        ORDER BY 1;
      -- EGV 15OCT015 Fin
   BEGIN
    
      -- EGV 20Jul2015 Inicio
      p_resultado := 'ERROR';
      -- EGV 20Jul2015 Fin
   
      g_id_proc := p_id_proc;
      g_cod_mf := 0;
      g_errores := 0;

      --> Se loguea el inicio del proceso y devuelve el Id Proc
      BEGIN
         l_estado := 'P';

         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('I',
                                                        g_id_proc,
                                                        l_cod_proc,
                                                        l_descr,
                                                        l_estado,
                                                        l_resultado);


      END;


      l_liquidaciones :=
         BASEARG.PKG_BXSCITI_GENERAL.convertir_lista (p_cod_lq, ';');

      IF l_liquidaciones.COUNT () = 0
      THEN
         l_resultado :=
            'Por lo menos tiene que haber una liquidacion parametrizada para ejecutar el proceso';
         RAISE CUSTOM;
      END IF;

      --> cargo los datos de las liquidaciones en memoria
      PKG_BXSCITI_DATOS_ACRED.
       Datos_Liquidaciones (l_liquidaciones, l_resultado);

      IF l_resultado = 'E'
      THEN
         RAISE CUSTOM;
      END IF;

      g_cb_acredit_t := g_cb_acredit_list ();

      --> para cada una de las liquidaciones ingresadas por parametro
      FOR i IN g_cod_lq_t.FIRST .. g_cod_lq_t.LAST
      LOOP
         BEGIN
            --> Borro los registros de la tabla de acreditacion por si se trata de un reproceso
            DELETE BASEARG.CB_ACREDITA
             WHERE COD_LQ = g_cod_lq_t (i);

            l_contador := 0;
            l_insertados := 0;
            
            g_cod_mf := 0;      -- EGV 07/08/2015

            OPEN CUR_MOVHD (g_cod_lq_t (i));

            LOOP
               FETCH CUR_MOVHD
               BULK COLLECT INTO l_cod_mf_t,
                    l_imptot_hd_t,
                    l_hades_hd_t,
                    l_acumulador_t
               LIMIT 50;

               EXIT WHEN l_cod_mf_t.COUNT = 0;

               FOR z IN l_cod_mf_t.FIRST .. l_cod_mf_t.LAST
               LOOP
                  BEGIN
                  
                     l_contador := l_contador + 1;

                     --   DBMS_OUTPUT.PUT_LINE('Contador' || l_contador || ' : cod mf ' || l_cod_mf_t (z) || ' Importe : ' || l_imptot_hd_t(z));

                     IF g_cod_mf = 0
                     THEN
                     
                        -- EGV - 15Oct2015 - Inicio
                        --> Blanqueo los importes
                        g_rem_suj_ret := 0;
                        g_con_no_grab := 0;
                        g_retenciones := 0;

                        g_neto := 0;
                        g_por_banco := 0;
                        g_efectivo := 0;
                        g_monto_de := 0;
                        -- EGV - 15Oct2015 - Fin                     
                     
                        g_cod_mf := l_cod_mf_t (z);                 -- EGV 03/08/2015
                        l_proc_int := 'Datos MAEFUNC';
                        PKG_BXSCITI_GENERAL.
                         Obtener_Datos_MAEFUNC (l_cod_mf_t (z),
                                                g_fec_lq_t (i),     -- EGV 1.1 17/06/2015
                                                l_estado_det,
                                                l_datos_mf);

                        IF l_estado_det <> 'E'
                        THEN
                           g_cod_mf := l_datos_mf.cod_mf;
                           g_emp_mf := l_datos_mf.cod_emp;
                           -- EGV 05/10/2015 - Inicio
                           --g_banking_mf := l_datos_mf.banking;
                           --g_secc_mf := l_datos_mf.secc_mf;
                           --g_cta_acredit_mf := l_datos_mf.identfp_mf;
                           --g_fpago_mf := l_datos_mf.fpago_mf;
                           g_banking_mf := trim(l_datos_mf.banking);
                           g_secc_mf := trim(l_datos_mf.secc_mf);
                           g_cta_acredit_mf := trim(l_datos_mf.identfp_mf);
                           g_fpago_mf := trim(l_datos_mf.fpago_mf);
                           -- EGV 05/10/2015 - Fin
                           g_cuil_mf := trim(l_datos_mf.cuil);      -- EGV 10/03/2017 - Acreditacion por cbu
                        ELSE
                           l_descr_det :=
                              'Error obteniendo los datos  de acreditacion para el empleado : '
                              || l_cod_mf_t (z);
                           RAISE NEXT_VAL;
                        END IF;
                     END IF;
                     
                     IF g_cod_mf <> 0 AND (g_cod_mf <> l_cod_mf_t (z))
                     THEN
                        --> Inserta lo valores procesados para el empleado
                        PKG_BXSCITI_DATOS_ACRED.
                         inserta_datos_acredit (g_cod_lq_t (i), l_estado_det);

                        -- EGV - 03/08/2015 - Inicio
                        --> Blanqueo los importes
                        g_rem_suj_ret := 0;
                        g_con_no_grab := 0;
                        g_retenciones := 0;

                        g_neto := 0;
                        g_por_banco := 0;
                        g_efectivo := 0;
                        g_monto_de := 0;
                        -- EGV - 03/08/2015 - Fin

                        IF l_estado_det <> 'E'
                        THEN
                           l_insertados := l_insertados + 1;
                        ELSE
                           l_descr_det :=
                              'Error insertando datos de acreditacion para el empleado : '
                              || g_cod_mf;
                           RAISE NEXT_VAL;
                        END IF;

                        g_cod_mf := l_cod_mf_t (z);                 -- EGV 03/08/2015
                        l_proc_int := 'Datos MAEFUNC';
                        PKG_BXSCITI_GENERAL.
                         Obtener_Datos_MAEFUNC (l_cod_mf_t (z),
                                                g_fec_lq_t (i),     -- EGV 1.1 17/06/2015
                                                l_estado_det,
                                                l_datos_mf);

                        IF l_estado_det <> 'E'
                        THEN
                           g_cod_mf := l_datos_mf.cod_mf;
                           g_emp_mf := l_datos_mf.cod_emp;
                           -- EGV 05/10/2015 - Inicio
                           --g_banking_mf := l_datos_mf.banking;
                           --g_secc_mf := l_datos_mf.secc_mf;
                           --g_cta_acredit_mf := l_datos_mf.identfp_mf;
                           --g_fpago_mf := l_datos_mf.fpago_mf;
                           g_banking_mf := trim(l_datos_mf.banking);
                           g_secc_mf := trim(l_datos_mf.secc_mf);
                           g_cta_acredit_mf := trim(l_datos_mf.identfp_mf);
                           g_fpago_mf := trim(l_datos_mf.fpago_mf);
                           -- EGV 05/10/2015 - Fin
                           g_cuil_mf := trim(l_datos_mf.cuil);      -- EGV 10/03/2017 - Acreditacion por cbu
                        ELSE
                           l_descr_det :=
                              'Error obteniendo los datos  de acreditacion para el empleado : '
                              || l_cod_mf_t (z);
                           RAISE NEXT_VAL;
                        END IF;



                        -- EGV - 03/08/2015 - Inicio - se mueve un poco m�s arriba
                        --> Blanqueo los importes
                        --g_rem_suj_ret := 0;
                        --g_con_no_grab := 0;
                        --g_retenciones := 0;

                        --g_neto := 0;
                        --g_por_banco := 0;
                        --g_efectivo := 0;
                        --g_monto_de := 0;
                        -- EGV - 03/08/2015 - Fin
                     END IF;
                     
                     IF l_acumulador_t (z) = 'deb'
                     THEN
                        IF l_imptot_hd_t (z) <> 0
                        THEN
                           g_monto_de := g_monto_de + l_imptot_hd_t (z);
                        END IF;
                     ELSE
                        IF l_hades_hd_t (z) = 'h'
                        THEN
                           IF l_acumulador_t (z) = 'hcd'
                           THEN
                              g_rem_suj_ret :=
                                 g_rem_suj_ret + l_imptot_hd_t (z);
                           ELSE
                              IF l_acumulador_t (z) = 'hsd'
                              THEN
                                 g_con_no_grab :=
                                    g_con_no_grab + l_imptot_hd_t (z);
                              END IF;
                           END IF;
                        ELSE
                           IF l_hades_hd_t (z) = 'd'
                           THEN
                              g_retenciones :=
                                 g_retenciones + l_imptot_hd_t (z);
                           END IF;
                        END IF;
                     END IF;
                     
                  EXCEPTION
                     WHEN NEXT_VAL
                     THEN
                        g_errores := g_errores + 1;
                        l_resultado := NULL;

                        PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                            l_proc_int,
                                                            l_estado_det,
                                                            l_descr_det,
                                                            l_resultado);
                     WHEN OTHERS
                     THEN
                        -- EGV 04/08/2015 - Inicio
                        g_errores := g_errores + 1;
                        l_resultado := NULL;

                        PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                                            l_proc_int,
                                                            l_estado_det,
                                                            SUBSTR(SQLERRM,0,254),
                                                            l_resultado);
                        -- EGV 04/08/2015 - Fin                                    
                        NULL;
                  END;
               END LOOP;
            END LOOP;

            CLOSE CUR_MOVHD;

            IF g_cod_mf <> 0
            THEN
               --> Se inserta el ultimo empleado procesado
               inserta_datos_acredit (g_cod_lq_t (i), l_resultado);

               NULL;
            END IF;
         END;
      END LOOP;

      FORALL x IN g_cb_acredit_t.FIRST .. g_cb_acredit_t.LAST
         INSERT INTO CB_ACREDITA
              VALUES g_cb_acredit_t (x);

      IF g_errores <> 0
      THEN
         RAISE ERRORES_ACRED;
      ELSE
         BEGIN
            -- EGV 20Jul2015 Inicio
            --p_resultado :=
            p_resultado := 'OK';
            
            l_descr_det :=
            -- EGV 20Jul2015 Fin
                  ' Proceso de acreditacion termino correctamente con '
               || l_contador
               || ' registros procesados y '
               || l_insertados
               || ' registros insertados.';
            l_estado := 'C';
            PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                                g_id_proc,
                                                l_cod_proc,
                                                -- EGV 20Jul2015 Inicio
                                                --l_descr,
                                                l_descr_det,
                                                -- EGV 20Jul2015 Fin
                                                l_estado,
                                                l_resultado);
         EXCEPTION
            WHEN OTHERS
            THEN
               NULL;
         END;
      END IF;
   EXCEPTION
      WHEN ERRORES_ACRED
      THEN
         l_estado := 'E';
         -- EGV 07/08/2015 - Inicio
         --p_resultado := substr(SQLERRM, 0, 255);
         p_resultado := 'El proceso finaliz� con Errores. Ver Log.';
         
         PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                            l_proc_int,
                                            l_estado_det,
                                            'Hay ' || g_errores || ' registros con error.',
                                            l_resultado);         
         
         -- EGV 07/08/2015 - Fin
         PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                             g_id_proc,
                                             l_cod_proc,
                                             l_descr,
                                             l_estado,
                                             l_resultado);
      WHEN CUSTOM
      THEN
         l_estado := 'E';
         -- EGV 07/08/2015 - Inicio
         --p_resultado := substr(SQLERRM, 0, 255);
         p_resultado := 'El proceso finaliz� con Errores. Ver Log.';
         
         if l_resultado <> 'E' then
            PKG_BXSCITI_GENERAL.GRABAR_LOG_det (g_id_proc,
                                            l_proc_int,
                                            l_estado_det,
                                            l_resultado,
                                            l_resultado);         
         end if;
         -- EGV 07/08/2015 - Fin         
         
         PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                             g_id_proc,
                                             l_cod_proc,
                                             l_descr,
                                             l_estado,
                                             l_resultado);
      WHEN OTHERS
      THEN
         -- EGV 21Jul2015 Inicio
         --null
         p_resultado := substr(SQLERRM, 0, 255);
         -- EGV 21Jul2015 Fin
   END;

   PROCEDURE Datos_Liquidaciones (c_liquidaciones   IN     var_array,
                                  c_resultado          OUT VARCHAR2)
   IS
   BEGIN
      c_resultado := 'E';

      SELECT LIQ.COD_LQ,
             LIQ.FHAS_LQ,
             LIQ.DESC_LQ,
             E.COD_EMP,
             E.NOM_EMP
        BULK COLLECT INTO g_cod_lq_t,
             g_fec_lq_t,
             g_desc_lq_t,
             g_emp_lq_t,
             g_d_emp_lq_t
        FROM BASEARG.LIQUIDAC LIQ, BASEARG.QSEMPRESA E
       WHERE LIQ.COD_EMP = E.COD_EMP
             AND LIQ.COD_LQ IN (SELECT * FROM TABLE (c_liquidaciones));

      c_resultado := 'C';
   EXCEPTION
      WHEN OTHERS
      THEN
         c_resultado := 'E';
   END Datos_Liquidaciones;



   PROCEDURE inserta_datos_acredit (c_cod_lq      IN     INTEGER,
                                    c_resultado      OUT VARCHAR2)
   IS
      c_index   INTEGER;
   BEGIN
      g_neto := g_rem_suj_ret + g_con_no_grab - g_retenciones;

      -- EGV  10/03/2017 Inicio - Acreditacion por CBU a otros bancos
      -- EGV 03/08/2015 Inicio
      --IF g_fpago_mf = 'CAHO'
      --IF trim(g_fpago_mf) = 'CAHO'
      -- EGV 03/08/2015 Fin
      IF trim(g_fpago_mf) = 'CAHO' or trim(g_fpago_mf) = 'CBU' 
      -- EGV  10/03/2017 Fin
      THEN                                                  --- CAJA DE AHORRO
         g_por_banco := g_neto;
         g_efectivo := 0;
      ELSE
         g_por_banco := 0;
         g_efectivo := g_neto;
      END IF;


      g_cb_acredit_t.EXTEND;
      c_index := g_cb_acredit_t.COUNT;

      g_cb_acredit_t (c_index).COD_LQ := c_cod_lq;
      g_cb_acredit_t (c_index).COD_MF := g_cod_mf;
      -- EGV  10/03/2017 Inicio - Acreditacion por CBU a otros bancos
      --g_cb_acredit_t (c_index).IDENTFP_MF := g_cta_acredit_mf;
      if trim(g_fpago_mf) = 'CBU' then
        g_cb_acredit_t (c_index).IDENTFP_MF := null;
      else
        g_cb_acredit_t (c_index).IDENTFP_MF := g_cta_acredit_mf;
      end if;
      -- EGV  10/03/2017 Fin
      g_cb_acredit_t (c_index).COD_EMP := g_emp_mf;
      g_cb_acredit_t (c_index).COD_SUC := g_secc_mf;
      g_cb_acredit_t (c_index).BANKING := g_banking_mf;
      g_cb_acredit_t (c_index).SUELDO_NTO_BCO := g_por_banco;
      g_cb_acredit_t (c_index).SUELDO_NTO_EF := g_efectivo;
      g_cb_acredit_t (c_index).DEBITO := g_monto_de;
      -- EGV  10/03/2017 Inicio - Acreditacion por CBU a otros bancos
      if trim(g_fpago_mf) = 'CBU' then
        g_cb_acredit_t (c_index).CBU := g_cta_acredit_mf;
      else
        g_cb_acredit_t (c_index).CBU := null;
      end if;
      g_cb_acredit_t (c_index).CUIL := g_cuil_mf;
      -- EGV  10/03/2017 Fin      


      c_resultado := 'C';
   EXCEPTION
      WHEN OTHERS
      THEN
         c_resultado := 'E';
   END;
END PKG_BXSCITI_DATOS_ACRED; 
/
