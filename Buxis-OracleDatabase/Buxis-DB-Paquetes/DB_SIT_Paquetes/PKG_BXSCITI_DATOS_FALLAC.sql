CREATE OR REPLACE PACKAGE         PKG_BXSCITI_DATOS_FALLAC
AS
   /******************************************************************************
    NAME: BASEARG.PKG_BXSCITI_DATOS_FALLAC
    PURPOSE:

    REVISIONS:
    Ver Date Author Description
    --------- ---------- --------------- ------------------------------------
    1.0  30/04/2015 CSA 1.  Se procesan los datos de actualizacion de reserva de Falla de Caja
   ******************************************************************************/


   g_id_proc        INTEGER;
   g_proc_int       VARCHAR (100);
   g_descr_det      VARCHAR2 (100);
   g_estado         VARCHAR2 (2);
   g_estado_det     VARCHAR2 (254);
   g_resultado      VARCHAR2 (50);
   g_errores        INTEGER := 0;



   PROCEDURE actualizacion_reserva_fc (p_anio        IN     INTEGER,
                                       p_mes         IN     INTEGER,
                                       p_id_proc     IN     INTEGER,
                                       p_resultado      OUT VARCHAR2);

   PROCEDURE inserta_saldo_cero (l_cod_mf      INTEGER,
                                 l_anio_n      INTEGER,
                                 l_mes_n       INTEGER,
                                 l_faltante    NUMBER,
                                 l_reserva     NUMBER,
                                 l_saldo       NUMBER);

   PROCEDURE inserta_falla (l_cod_mf      INTEGER,
                            l_anio_n      INTEGER,
                            l_mes_n       INTEGER,
                            l_faltante    NUMBER,
                            l_reserva     NUMBER,
                            l_saldo       NUMBER);

   PROCEDURE inserta_falla_cero (l_cod_mf      INTEGER,
                                 l_anio_n      INTEGER,
                                 l_mes_n       INTEGER,
                                 l_faltante    NUMBER,
                                 l_reserva     NUMBER,
                                 l_saldo       NUMBER);

   PROCEDURE modifica_falla (l_cod_mf      INTEGER,
                             l_anio_n      INTEGER,
                             l_mes_n       INTEGER,
                             l_faltante    NUMBER,
                             l_reserva     NUMBER,
                             l_saldo       NUMBER);
END PKG_BXSCITI_DATOS_FALLAC; 
/


CREATE OR REPLACE PACKAGE BODY                 PKG_BXSCITI_DATOS_FALLAC AS
/******************************************************************************
 NAME: BASEARG.PKG_BXSCITI_DATOS_FALLAC
 PURPOSE:

 REVISIONS:
 Ver Date Author Description
 --------- ---------- --------------- ------------------------------------
 1.0  30/04/2015 CSA 1.  Se procesan los datos de actualizacion de reserva de Falla de Caja
******************************************************************************/

PROCEDURE actualizacion_reserva_fc (p_anio        IN     INTEGER,
                                       p_mes         IN     INTEGER,
                                       p_id_proc     IN     INTEGER,
                                       p_resultado      OUT VARCHAR2)
   IS
      l_cod_proc      VARCHAR2 (50) := 'FALLA CAJA';
      l_descr         VARCHAR2 (50) := 'Actualiza las reservas de falla de caja';


      l_valor_falla   NUMBER := 0;
      l_procesados    NUMBER := 0;

      l_anio_p        INTEGER;
      l_mes_p         INTEGER;

      l_saldo_n       NUMBER := 0;
      l_reserva_n     NUMBER := 0;
      l_faltante_n    NUMBER := 0;

      CURSOR CUR_SALDO_FALLAS (c_anio INTEGER, c_mes INTEGER)
      IS
         SELECT *
           FROM CB_FALLAS F
          WHERE F.ANIO = c_anio AND F.MES = c_mes;

   BEGIN
      --> Se loguea el inicio del proceso y devuelve el Id Proc

         g_id_proc := p_id_proc;


      BEGIN
         g_estado := 'P';

         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('I',
                                                   g_id_proc,
                                                   l_cod_proc,
                                                   l_descr,
                                                   g_estado,
                                                   g_resultado);


      EXCEPTION
         WHEN OTHERS
         THEN
            NULL;
      END;

      --> Obtengo Valor de Falla de Caja
      SELECT SUM (PL.VALOR)
        INTO l_valor_falla
        FROM CB_PARAM_LIQ PL
       WHERE PL.PARAM_LIQ IN ('FCDIV', 'FCCON')
             AND PL.FECHA_EFECTIVA =
                    (SELECT MAX (PL2.FECHA_EFECTIVA)
                       FROM CB_PARAM_LIQ PL2
                      WHERE PL.PARAM_LIQ = PL2.PARAM_LIQ
                            AND PL2.FECHA_EFECTIVA <= SYSDATE);


      --> Calculo mes y año destino de los datos
      IF p_mes = 12
      THEN
         l_anio_p := p_anio + 1;
         l_mes_p := 1;
      ELSE
         l_anio_p := p_anio;
         l_mes_p := p_mes + 1;
      END IF;


      BEGIN
         DELETE FROM CB_FALLHD
               WHERE ANIO = l_anio_p AND MES = l_mes_p;

         INSERT INTO CB_FALLHD (ANIO, MES, PROC_EJECUTADO)
              VALUES (l_anio_p, l_mes_p, 'Y');
      END;


      FOR X IN CUR_SALDO_FALLAS (p_anio, p_mes)
      LOOP
         l_faltante_n := 0;
         l_reserva_n := 0;
         l_saldo_n := 0;

         IF X.SALDO <> 0
         THEN
            --> calculo nuevo saldo

            IF X.SALDO < l_valor_falla
            THEN
               --> Si El Saldo Es Menor Al Monto Que El Banco Paga Por Falla De Cajero

               l_saldo_n := 0;
               l_reserva_n := X.ACU_RESV + X.SALDO;
            ELSE
               l_saldo_n := X.SALDO - l_valor_falla;
               l_reserva_n := X.ACU_RESV + l_valor_falla;
            END IF;

            modifica_falla (X.COD_MF,
                            X.ANIO,
                            X.MES,
                            X.ACU_FALT,
                            l_reserva_n,
                            l_saldo_n);

            -- EGV 21OCT2015 Inicio
            --IF l_mes_p = 1
            --THEN
            IF l_mes_p > 1
            THEN
            -- EGV 21OCT2015 Fin
               inserta_falla (X.COD_MF,
                              l_anio_p,
                              l_mes_p,
                              X.ACU_FALT,
                              l_reserva_n,
                              l_saldo_n);
            ELSE
               inserta_falla_cero (X.COD_MF,
                                   l_anio_p,
                                   l_mes_p,
                                   X.ACU_FALT,
                                   l_reserva_n,
                                   l_saldo_n);
            END IF;
         ELSE
            --> Ingreso Saldo Cero
            Inserta_saldo_cero (X.COD_MF,
                                l_anio_p,
                                l_mes_p,
                                X.ACU_FALT,
                                X.ACU_RESV,
                                X.SALDO);
         END IF;


         l_procesados := l_procesados + 1;
      END LOOP;


      p_resultado :=
         ' Proceso de actualizacion de fallas de caja termino correctamente con '
         || l_procesados
         || ' registros procesados.';

      g_estado := 'C';
      BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_CAB ('F',
                                     g_id_proc,
                                     l_cod_proc,
                                     l_descr,
                                     g_estado,
                                     g_resultado);
      p_resultado := 'OK'; -- EGV 11Feb2016 Inicio
      
   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado :=
               'Error Procesando las fallas de cajas del anio : '
            || p_anio
            || ' mes : '
            || p_mes
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

      PROCEDURE inserta_saldo_cero (l_cod_mf      INTEGER,
                                    l_anio_n      INTEGER,
                                    l_mes_n       INTEGER,
                                    l_faltante    NUMBER,
                                    l_reserva     NUMBER,
                                    l_saldo       NUMBER)
      IS
      BEGIN
         DELETE CB_FALLAS
          WHERE ANIO = l_anio_n AND MES = l_mes_n AND COD_MF = l_cod_mf;

         IF l_mes_n = 1
         THEN
            INSERT INTO CB_FALLAS (ANIO,
                                   MES,
                                   COD_MF,
                                   ACU_FALT,
                                   ACU_RESV,
                                   SALDO)
                 VALUES (l_anio_n,
                         l_mes_n,
                         l_cod_mf,
                         0,
                         0,
                         0);
         ELSE
            INSERT INTO CB_FALLAS (ANIO,
                                   MES,
                                   COD_MF,
                                   ACU_FALT,
                                   ACU_RESV,
                                   SALDO)
                 VALUES (l_anio_n,
                         l_mes_n,
                         l_cod_mf,
                         l_faltante,
                         l_reserva,
                         l_saldo);
         END IF;
      END Inserta_saldo_cero;

      PROCEDURE inserta_falla (l_cod_mf      INTEGER,
                               l_anio_n      INTEGER,
                               l_mes_n       INTEGER,
                               l_faltante    NUMBER,
                               l_reserva     NUMBER,
                               l_saldo       NUMBER)
      IS
      BEGIN
         DELETE CB_FALLAS
          WHERE ANIO = l_anio_n AND MES = l_mes_n AND COD_MF = l_cod_mf;

         INSERT INTO CB_FALLAS (ANIO,
                                MES,
                                COD_MF,
                                ACU_FALT,
                                ACU_RESV,
                                SALDO)
              VALUES (l_anio_n,
                      l_mes_n,
                      l_cod_mf,
                      l_faltante,
                      l_reserva,
                      l_saldo);
      END inserta_falla;

      PROCEDURE inserta_falla_cero (l_cod_mf      INTEGER,
                                    l_anio_n      INTEGER,
                                    l_mes_n       INTEGER,
                                    l_faltante    NUMBER,
                                    l_reserva     NUMBER,
                                    l_saldo       NUMBER)
      IS
      BEGIN
         DELETE CB_FALLAS
          WHERE ANIO = l_anio_n AND MES = l_mes_n AND COD_MF = l_cod_mf;

         INSERT INTO CB_FALLAS (ANIO,
                                MES,
                                COD_MF,
                                ACU_FALT,
                                ACU_RESV,
                                SALDO)
              VALUES (l_anio_n,
                      l_mes_n,
                      l_cod_mf,
                      l_saldo,
                      0,
                      0);
      END inserta_falla_cero;

      PROCEDURE modifica_falla (l_cod_mf      INTEGER,
                                l_anio_n      INTEGER,
                                l_mes_n       INTEGER,
                                l_faltante    NUMBER,
                                l_reserva     NUMBER,
                                l_saldo       NUMBER)
      IS
      BEGIN
         UPDATE CB_FALLAS F
            SET F.ACU_FALT = l_faltante,
                F.ACU_RESV = l_reserva,
                F.SALDO = l_saldo
          WHERE F.ANIO = l_anio_n AND F.MES = l_mes_n AND F.COD_MF = l_cod_mf;
      END modifica_falla;

   END PKG_BXSCITI_DATOS_FALLAC;
/
