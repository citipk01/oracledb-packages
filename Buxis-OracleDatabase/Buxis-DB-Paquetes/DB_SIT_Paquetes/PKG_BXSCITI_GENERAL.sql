CREATE OR REPLACE PACKAGE         PKG_BXSCITI_GENERAL AS
/******************************************************************************
 NAME: BASEARG.PKG_BXSCITI_GENERAL
 PURPOSE:

 REVISIONS:
 Ver Date Author Description
 --------- ---------- --------------- ------------------------------------
 1.0  30/04/2015 CSA 1.  Contiene las rutinas reutilizables por los otros procesos de la decomision
 1.1  15/06/2015 EGV  Correcciones varias
******************************************************************************/


FUNCTION   convertir_lista(p_lista IN VARCHAR2,p_separador IN VARCHAR2) RETURN VAR_ARRAY;
 -- EGV 1.1 15/06/2015 - Inicio
 --PROCEDURE obtener_datos_MAEFUNC (c_cod_mf IN INTEGER, p_resultado OUT VARCHAR2,p_datos out MAEFUNC_TBL%ROWTYPE)  ;
 PROCEDURE obtener_datos_MAEFUNC (c_cod_mf IN INTEGER, c_fec_efec IN DATE,p_resultado OUT VARCHAR2,p_datos out MAEFUNC_TBL%ROWTYPE)  ;
 -- EGV 1.1 15/06/2015 - Fin
 PROCEDURE grabar_log_cab(p_accion IN VARCHAR2, p_id_proc IN INTEGER, p_cod_proc IN VARCHAR2,p_descr IN VARCHAR2,p_estado IN VARCHAR2, p_resultado OUT VARCHAR2);
 PROCEDURE grabar_log_det( p_id_proc IN INTEGER,p_proc_int VARCHAR2,p_estado IN VARCHAR2,p_descr IN VARCHAR2, p_resultado OUT VARCHAR2);

END PKG_BXSCITI_GENERAL; 
/


CREATE OR REPLACE PACKAGE BODY         PKG_BXSCITI_GENERAL
AS
   /******************************************************************************
    NAME: BASEARG.PKG_BXSCITI_GENERAL
    PURPOSE:

    REVISIONS:
    Ver Date Author Description
    --------- ---------- --------------- ------------------------------------
    1.0  30/04/2015 CSA 1.  Contiene las rutinas reutilizables por los otros procesos de la decomision
    1.1  15/06/2015 EGV  Correcciones Varias
   ******************************************************************************/
   PROCEDURE obtener_datos_MAEFUNC (c_cod_mf      IN     INTEGER,
                                    c_fec_efec    IN     DATE,      -- EGV 1.1 15/06/2015
                                    p_resultado      OUT VARCHAR2,p_datos  OUT MAEFUNC_TBL%ROWTYPE)
   IS
   BEGIN

     p_resultado := 'C';

      SELECT m.*
          INTO p_datos
        FROM maefunc_tbl m
       WHERE M.COD_MF = c_cod_mf
             AND M.FECHA_EFECTIVA =
                    (SELECT MAX (M2.FECHA_EFECTIVA)
                       FROM MAEFUNC_TBL M2
                      WHERE M2.COD_MF = M.COD_MF
                            -- EGV 1.1 15/06/2015 - Inicio
                            --AND M2.FECHA_EFECTIVA <= SYSDATE)
                            AND M2.FECHA_EFECTIVA <= c_fec_efec)
                            -- EGV 1.1 15/06/2015 - Fin
             -- EGV 15/06/2015 1.1 - Inicio
             AND M.SEC_EFECTIVA = (SELECT MAX(M3.SEC_EFECTIVA) FROM MAEFUNC_TBL M3 WHERE M3.COD_MF = M.COD_MF AND M3.FECHA_EFECTIVA = M.FECHA_EFECTIVA) 
             -- EGV 15/06/2015 1.1 - Fin                            
             AND NVL (M.BAJA_EFECTIVA, 0) = 0;


   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado := 'E';
   END Obtener_Datos_MAEFUNC;


   FUNCTION convertir_lista (p_lista IN VARCHAR2, p_separador IN VARCHAR2)
      RETURN var_array
   AS
      l_cadena        VARCHAR2 (4000) := p_lista || p_separador;
      l_comma_index   INTEGER;
      l_index         INTEGER := 1;
      p_table         var_array := var_array ();
   BEGIN
      IF TRIM (p_lista) IS NOT NULL
      THEN
         LOOP
            l_comma_index := INSTR (l_cadena, p_separador, l_index);

            EXIT WHEN l_comma_index = 0;

            p_table.EXTEND ();

            p_table (p_table.COUNT) :=
               SUBSTR (l_cadena, l_index, l_comma_index - l_index);

            l_index := l_comma_index + 1;
         END LOOP;
      END IF;

      RETURN p_table;
   END convertir_lista;

   PROCEDURE Grabar_Log_Cab (p_accion      IN     VARCHAR2,
                             p_id_proc     IN     INTEGER,
                             p_cod_proc    IN     VARCHAR2,
                             p_descr       IN     VARCHAR2,
                             p_estado      IN     VARCHAR2,
                             p_resultado      OUT VARCHAR2)
   IS
   BEGIN
      CASE p_accion
         WHEN 'I'
         THEN                                    -- Inicio de un nuevo proceso
            INSERT INTO CB_LOG_CAB (ID_PROC,
                                    COD_PROC,
                                    DESCRIPCION,
                                    FEC_INI,
                                    ESTADO)
                 VALUES (p_id_proc,
                         p_cod_proc,
                         p_descr,
                         SYSDATE,
                         p_estado);

            p_resultado := 'Proceso Iniciado';
         WHEN 'F'
         THEN                                 -- Finaliza un proceso existente
            UPDATE CB_LOG_CAB
               SET FEC_FIN = SYSDATE, ESTADO = p_estado
             WHERE ID_PROC = p_id_proc;

            p_resultado := 'Proceso finalizado';
         ELSE
            p_resultado := 'Codigo de accion incorrecto';
      END CASE;

      COMMIT;
   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado := 'Error : ' || SQLCODE || ' -- ' || SQLERRM;
   END;


   PROCEDURE Grabar_Log_Det (p_id_proc     IN     INTEGER,
                             p_proc_int    IN     VARCHAR2,
                             p_estado      IN     VARCHAR2,
                             p_descr       IN     VARCHAR2,
                             p_resultado      OUT VARCHAR2)
   IS
      l_fila   INTEGER;
   BEGIN
      SELECT (NVL (MAX (L.FILA), 0) + 1)
        INTO l_fila
        FROM CB_LOG L
       WHERE L.ID_PROC = p_id_proc;



      INSERT INTO CB_LOG (ID_PROC,
                          FILA,
                          PROC_INT,
                          ESTADO,
                          DESCRIPCION,
                          FEC_LOG)
           VALUES (p_id_proc,
                   l_fila,
                   p_proc_int,
                   p_estado,
                   p_descr,
                   SYSDATE);

      COMMIT;

      p_resultado :=
         'Log (' || p_id_proc || ' , ' || l_fila || ') grabado correctamente';

   EXCEPTION
      WHEN OTHERS
      THEN
         p_resultado := 'Error : ' || SQLCODE || ' -- ' || SQLERRM;
   END;
END PKG_BXSCITI_GENERAL; 
/
