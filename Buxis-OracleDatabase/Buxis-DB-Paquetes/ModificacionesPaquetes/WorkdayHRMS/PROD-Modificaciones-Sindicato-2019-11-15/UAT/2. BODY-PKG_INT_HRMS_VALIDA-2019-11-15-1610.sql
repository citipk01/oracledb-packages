/* 0. Last date Modif:  2019 11 20 - 10:20
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
-------------------------------------------------------------------------------------------------


CREATE OR REPLACE PACKAGE BODY           "PKG_INT_HRMS_VALIDA"
AS
 /******************************************************************************

 NAME: PKG_INTERFAZ_HRMS_VALIDA
 PURPOSE: Valida la informacion de las tablas de Interfaz y arma
 la tabla CB_MAEFUNC_STG

 REVISIONS:
 Ver Date Author Description
 --------- ---------- --------------- ------------------------------------
 1.0 01/06/2015 Carlos Amar 1. Created this package.
 2.0 13/06/2019 Se agregan nuevos campos a la tabla CB_JOB_JR_INT
 2.2 2019-11-15 Se quita asignación de sindicato y ticketcan por interfaz
 ******************************************************************************/


--  BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET
-- (g_cod_int, g_proc_int, 'I', 'Mensaje de log', g_res_log);

 PROCEDURE Obtener_tipo_trans (l_audit_action IN VARCHAR2,
 l_tipo_tran OUT VARCHAR2)
 AS
 BEGIN
 CASE
     WHEN l_audit_action IN ('D', 'K', 'F')
     THEN
        l_tipo_tran := 'D';
     ELSE
        l_tipo_tran := ' ';
 END CASE;
 END;

-- EGV 01Jul2015 - Inicio
 PROCEDURE TraeRegionDeEmplid(l_emplid IN VARCHAR2, l_empl_rcd IN INTEGER,
      l_effdt IN DATE, l_effseq IN INTEGER, l_region OUT VARCHAR2) AS
 l_cod_mf NUMBER(10) := 0;
 BEGIN
 BEGIN
 SELECT B.REGION
    into l_region
 FROM CB_JOB_INT A, QSEMPRESA B
 WHERE A.COD_INTFC = g_cod_int
 AND A.EMPLID = l_emplid
 AND A.EMPL_RCD = l_empl_rcd
 AND A.EFFDT = (SELECT MAX(EFFDT) FROM CB_JOB_INT WHERE A.COD_INTFC =
      COD_INTFC
      AND A.EMPLID = EMPLID
      AND A.EMPL_RCD = EMPL_RCD
      AND EFFDT <= l_effdt)
 AND A.EFFSEQ = (SELECT MAX(EFFSEQ) FROM CB_JOB_INT WHERE A.COD_INTFC =
      COD_INTFC
      AND A.EMPLID = EMPLID
      AND A.EMPL_RCD = EMPL_RCD
      AND A.EFFDT = EFFDT
      -- EGV 19OCT2015 Inicio
      --AND EFFSEQ <= l_effseq)
      AND (EFFDT < l_effdt OR EFFSEQ <= l_effseq))
 AND A.NRO_LINEA = (SELECT MAX(NRO_LINEA)
                    FROM CB_JOB_INT
                    WHERE A.COD_INTFC = COD_INTFC
                      AND A.EMPLID = EMPLID
                      AND A.EMPL_RCD = EMPL_RCD
                      AND A.EFFDT = EFFDT
                      AND A.EFFSEQ = EFFSEQ)
      -- EGV 19OCT2015 Fin
 -- EGV 07SEP2015 Inicio
 --AND A.COMPANY = TRIM(B.COMPANY);
 AND case when A.COMPANY in ('UUY','CUY','FUY','WUY') then A.COMPANY else
      substr(A.COMPANY,2,2)
      end = TRIM(B.COMPANY);
 -- EGV 07SEP2015 Fin
 EXCEPTION
    WHEN NO_DATA_FOUND THEN
        BEGIN
        SELECT DISTINCT M.COD_MF
            Into l_cod_mf
        FROM MAEFUNC_TBL M
        WHERE LTRIM(trim(M.CB_GEID) , '0') = LTRIM(l_emplid, '0') 
        AND M.CB_EMPL_RCD = l_empl_rcd;

        EXCEPTION
            WHEN NO_DATA_FOUND THEN
                BEGIN
                SELECT DISTINCT M.COD_MF
                    Into l_cod_mf
                FROM MAEFUNC_TBL M
                WHERE LTRIM(trim(M.CB_GEID) , '0') =  LTRIM(l_emplid, '0')
                AND M.CB_EMPL_RCD = 0;
                EXCEPTION
                    WHEN OTHERS THEN
                        RAISE;
                END;
            WHEN OTHERS THEN
                RAISE;
        END;

        IF l_cod_mf <> 0 THEN
            BEGIN
            SELECT B.REGION
                Into l_region
            FROM MAEFUNC_TBL A, QSEMPRESA B
            WHERE A.COD_MF = l_cod_mf
            AND A.FECHA_EFECTIVA = (SELECT MAX(FECHA_EFECTIVA) FROM
      MAEFUNC_TBL
      WHERE A.COD_MF = COD_MF
      AND FECHA_EFECTIVA <= l_effdt)
            AND A.SEC_EFECTIVA = (SELECT MAX(SEC_EFECTIVA) FROM MAEFUNC_TBL
      WHERE A.COD_MF = COD_MF
      AND A.FECHA_EFECTIVA = FECHA_EFECTIVA
      -- EGV 19OCT2015 Inicio
      --AND SEC_EFECTIVA <= l_effseq)
      AND (FECHA_EFECTIVA < l_effdt OR SEC_EFECTIVA <= l_effseq))
      -- EGV 19OCT2015 Fin
            AND A.COD_EMP = B.COD_EMP;
            EXCEPTION
                WHEN OTHERS THEN
                    RAISE;
            END;
        END IF;

    WHEN OTHERS THEN
        RAISE;
 END;

 EXCEPTION
    WHEN OTHERS THEN
        BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
         g_proc_int,
         'E',
         SUBSTR('Error al buscar la region de ' || l_emplid || ' ' || to_char(
      l_empl_rcd) || ' ' || SQLERRM,1,254),
         g_res_log);
 END;
-- EGV 01Jul2015 - Fin

-- EGV 22SEP2015 Inicio
 FUNCTION CalculaNuevaAccion(l_action VARCHAR2, l_action_reason VARCHAR2,
      l_old_action VARCHAR2, l_emplid VARCHAR2, l_empl_rcd INTEGER, l_effdt
      DATE, l_effseq INTEGER) RETURN VARCHAR2
 IS
    l_sit_revista      VARCHAR2(2);
 BEGIN

    IF l_action <> 'LOA' or l_action_reason <> 'MAT' THEN
        RETURN l_action;
    END IF;

    l_sit_revista := null;

    BEGIN
    SELECT A.CGAR_SIT_REVISTA into l_sit_revista
    FROM CB_JOB_JR_INT A
    WHERE A.COD_INTFC = g_cod_int
    AND A.EMPLID = l_emplid
    AND A.EMPL_RCD = l_empl_rcd
    AND A.EFFDT = l_effdt
    AND A.EFFSEQ = l_effseq
    AND A.NRO_LINEA = (SELECT MAX(B.NRO_LINEA)
                        FROM CB_JOB_JR_INT B
                        WHERE B.COD_INTFC = A.COD_INTFC
                            AND B.EMPLID = A.EMPLID
                            AND B.EMPL_RCD = A.EMPL_RCD
                            AND B.EFFDT = A.EFFDT
                            AND B.EFFSEQ = A.EFFSEQ);
    EXCEPTION
        WHEN OTHERS THEN
            null;
    END;

    IF l_sit_revista IS NOT NULL THEN
        IF l_sit_revista = '5' THEN
            RETURN 'PLA';
        END IF;
        RETURN l_action;
    END IF;

    IF trim(l_old_action) = 'PLA' THEN
        RETURN trim(l_old_action);
    END IF;

    RETURN l_action;
 END;

--QUITO IMG 21.08.2019
 /*FUNCTION TraeCuotaClub(l_grado VARCHAR2) RETURN CHAR
 IS
   l_cuota_club CB_GRADO.CUOTA_CLUB%TYPE;

 BEGIN

    l_cuota_club := NULL;

    BEGIN
    SELECT A.CUOTA_CLUB Into l_cuota_club
    FROM CB_GRADO A
    WHERE TRIM(A.GRADO) = TRIM(l_grado)
    AND A.FECHA_EFECTIVA = (SELECT MAX(B.FECHA_EFECTIVA)
            FROM CB_GRADO B WHERE
            A.GRADO = B.GRADO
            AND B.FECHA_EFECTIVA <= SYSDATE)
    AND A.BAJA_EFECTIVA IS NULL;

    EXCEPTION
        WHEN OTHERS THEN
            NULL;
    END;

    RETURN l_cuota_club;
 END;*/
-- EGV 22SEP2015 Fin

-- EGV 05OCT2015 Inicio
 /*FUNCTION TraeCatCont(l_grado VARCHAR2) RETURN CHAR
 IS
   l_cat_cont CB_GRADO.CAT_CONT%TYPE;

 BEGIN

    -- EGV 26JUL2016 Inicio
    --l_cat_cont := NULL;
    l_cat_cont := 'O';
    -- EGV 26JUL2016 Fin

    BEGIN
    SELECT A.CAT_CONT Into l_cat_cont
    FROM CB_GRADO A
    WHERE TRIM(A.GRADO) = TRIM(l_grado)
    AND A.FECHA_EFECTIVA = (
        SELECT MAX(B.FECHA_EFECTIVA)
        FROM CB_GRADO B WHERE
        A.GRADO = B.GRADO
        AND B.FECHA_EFECTIVA <= SYSDATE
        )
    AND A.BAJA_EFECTIVA IS NULL;

    EXCEPTION
        WHEN OTHERS THEN
            NULL;
    END;

    RETURN l_cat_cont;
 END;*/


 FUNCTION TraeBanking(l_cencos varchar2, l_emp varchar2) RETURN CHAR
 IS
   l_banking CB_CENCOS.CBHR_BANKING%TYPE;

 BEGIN

    l_banking := NULL;

    BEGIN
    SELECT A.CBHR_BANKING Into l_banking
    FROM CB_CENCOS A
    WHERE TRIM(A.COD_EMP) = TRIM(l_emp)
    AND TRIM(A.QS_CENCOS)= TRIM(l_cencos);

    EXCEPTION
        WHEN OTHERS THEN
            NULL;
    END;

    RETURN l_banking;
 END;
-- EGV 05OCT2015 Fin


PROCEDURE Buscar_Emplid (l_emplid IN VARCHAR2,
 l_empl_rcd VARCHAR2,
 l_region VARCHAR2,  -- EGV 01Jul2015
 l_cod_mf OUT INTEGER)
AS
  l_new_empl_rcd VARCHAR2(3) := ' ';
BEGIN

 -- EGV 12FEB2016 Inicio
 -- EGV 01Jul2015 - Inicio
 --l_new_empl_rcd := l_empl_rcd;
 --IF l_region = 'ARG' AND l_empl_rcd <> ' ' THEN
 --   l_new_empl_rcd := '0';
 --END IF;
 -- EGV 01Jul2015 - Fin


 l_cod_mf := 0;

 BEGIN
   SELECT DISTINCT MM.COD_MF
   INTO l_cod_mf
   FROM (SELECT DISTINCT M.COD_MF
          FROM MAEFUNC_TBL M
          WHERE LTRIM(trim(M.CB_GEID) , '0') = LTRIM(l_emplid, '0')
          AND (to_char(M.CB_EMPL_RCD) = l_empl_rcd 
                OR l_empl_rcd = ' ')
        UNION
          SELECT DISTINCT MS.COD_MF
          FROM CB_MAEFUNC_STG MS
          WHERE MS.COD_INTFC = g_cod_int
          AND LTRIM(trim(MS.CB_GEID) , '0') = LTRIM(l_emplid, '0')
          AND (to_char(MS.CB_EMPL_RCD) = l_empl_rcd 
                OR l_empl_rcd = ' ')
          ) MM;

  EXCEPTION
    WHEN NO_DATA_FOUND
    THEN
        l_cod_mf := 0;
    WHEN OTHERS
    THEN
      g_err_descr :=
        'Error Obteniendo el COD_MF para EMPLID : '
        || l_emplid
        || ' EMPL_RCD : '
        || l_empl_rcd;
      BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
        g_proc_int,
        'E',
        g_err_descr,
        g_res_log);
      l_cod_mf := -1;
  END;

 IF l_cod_mf = 0 AND l_region = 'ARG'
     AND l_empl_rcd <> ' '
     AND l_empl_rcd <> '0' THEN
 BEGIN
    l_new_empl_rcd := '0';
    -- EGV 12FEB2016 Fin

 SELECT DISTINCT MM.COD_MF
 INTO l_cod_mf
 FROM (
      SELECT DISTINCT M.COD_MF
      FROM MAEFUNC_TBL M
      -- EGV 26Jun2015 - Inicio
      --WHERE M.CB_GEID = l_emplid
      --AND (M.CB_EMPL_RCD = l_empl_rcd OR l_empl_rcd = ' ')
      WHERE trim(M.CB_GEID) = l_emplid
      AND (to_char(M.CB_EMPL_RCD) = l_new_empl_rcd 
        OR l_new_empl_rcd = ' ')
      -- EGV 26Jun2015 - Fin
    UNION
       SELECT DISTINCT MS.COD_MF
       FROM CB_MAEFUNC_STG MS
       WHERE MS.COD_INTFC = g_cod_int
       -- EGV 26Jun2015 - Inicio
       --AND MS.CB_GEID = l_emplid
        --AND (MS.CB_EMPL_RCD = l_empl_rcd OR l_empl_rcd = ' ')) MM;
       AND trim(MS.CB_GEID) = l_emplid
       AND (to_char(MS.CB_EMPL_RCD) = l_new_empl_rcd 
          OR l_new_empl_rcd = ' ')
  ) MM;
 -- EGV 26Jun2015 - Fin
 EXCEPTION
    WHEN NO_DATA_FOUND
    THEN
        l_cod_mf := 0;
    WHEN OTHERS
    THEN
         g_err_descr :=
         'Error Obteniendo el COD_MF para EMPLID : '
         || l_emplid
         || ' EMPL_RCD : '
         || l_empl_rcd;
         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
         g_proc_int,
         'E',
         g_err_descr,
         g_res_log);
         l_cod_mf := -1;
 END;
  -- EGV 12FEB2016 Inicio
  END IF;
 END;
   -- EGV 12FEB2016 Fin

 PROCEDURE Armar_Cod_Mf (l_emplid IN VARCHAR2,
 l_empl_rcd VARCHAR2,
 l_region VARCHAR2,     -- EGV 12FEB2016
 l_cod_mf OUT INTEGER)
 AS
 BEGIN
 -- EGV 12FEB2016 Inicio
 --IF TO_NUMBER (l_empl_rcd) >= 1
 IF TO_NUMBER (l_empl_rcd) >= 1 AND l_region = 'URY'
 -- EGV 12FEB2016 Fin
 THEN
     l_cod_mf :=
     (100000000 * TO_NUMBER (l_empl_rcd)) + TO_NUMBER (l_emplid);
 ELSE
     l_cod_mf := TO_NUMBER (l_emplid);
 END IF;
 END;

 PROCEDURE Buscar_reg_MAEFUNC_STG (l_cod_int IN INTEGER,
 l_cod_mf INTEGER,
 l_effdt IN DATE,
 l_effseq IN INTEGER,
 l_tipo VARCHAR2,
 l_emplid VARCHAR2,         -- EGV 01Jul2015
 l_reg OUT g_mf_list,
 l_existe OUT INTEGER)
 AS
 l_existe_2 INTEGER;
 BEGIN
 /* l_tipo :
 FSE+ : Busca todos los registros de la fecha efectiva / seq efectiva pasada por
 parametro para adelante.
 Si no existe esa fecha/sequencia busca el registro anterior ( si existe) el que
 sera usado para cambiarle los datos de fecha/sequencia e insertarlo como nuevo
registro.
 FE+ :Busca todos los registros de la fecha efectiva pasada por parametro para a
delante.
 TO : Busca todos los registros correspondiente al empleado pasado por parametro

 TO+ : Busca todos los registros correspondiente al empleado pasado por parametr
o sin importar la empresa. Se compara directamente con el CB_GEID
 DE : Busca los registros a ser borrados.
 */

 l_reg := g_mf_list ();



 CASE l_tipo
     WHEN 'TO+'
     THEN
         BEGIN
         l_existe := 0;

         SELECT DISTINCT 1
         INTO l_existe
         FROM (SELECT DISTINCT 1
         FROM MAEFUNC_TBL M
         -- EGV 01Jul2015 Inicio
         --WHERE M.CB_GEID = l_cod_mf
         WHERE trim(M.CB_GEID) = l_emplid
         -- EGV 01Jul2015 Fin
         UNION
         SELECT DISTINCT 1
         FROM CB_MAEFUNC_STG S
         -- EGV 01Jul2015 Inicio
         --WHERE S.CB_GEID = l_cod_mf);
         WHERE trim(S.CB_GEID) = l_emplid
         AND S.COD_INTFC = l_cod_int);
         -- EGV 01Jul2015 Fin
         EXCEPTION
             WHEN OTHERS
             THEN
                -- EGV 07SEP2015 Inicio
                --dbms_output.put_line(sqlerrm);
                l_existe := 0;
                -- EGV 07SEP2015 Fin
                NULL;
         END;

         IF l_existe = 1
         THEN
             SELECT D.*
             BULK COLLECT INTO l_reg
             FROM (SELECT S.*
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int
             -- EGV 01Jul2015 Inicio
             --AND S.CB_GEID = l_cod_mf
             AND trim(S.CB_GEID) = l_emplid
             -- EGV 01Jul2015 Fin
             --AND S.ESTADO_STG = 'P'
             UNION
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'U'
             FROM MAEFUNC_TBL M
             WHERE NOT EXISTS
             (SELECT 1
             FROM CB_MAEFUNC_STG S1
             WHERE M.COD_MF = S1.COD_MF
             AND M.FECHA_EFECTIVA =
             S1.FECHA_EFECTIVA
             AND M.SEC_EFECTIVA =
             S1.SEC_EFECTIVA)
             -- EGV 01Jul2015 Inicio
             --AND M.CB_GEID = l_cod_mf) D
             AND trim(M.CB_GEID) = l_emplid) D
             -- EGV 01Jul2015 Fin
             ORDER BY D.COD_INTFC,
             D.COD_MF,
             D.FECHA_EFECTIVA,
             D.SEC_EFECTIVA;
         END IF;
     WHEN 'TO'
     THEN
         BEGIN
         l_existe := 0;

         SELECT DISTINCT 1
         INTO l_existe
         FROM (SELECT DISTINCT 1
         FROM MAEFUNC_TBL M
         WHERE M.COD_MF = l_cod_mf
         UNION
         SELECT DISTINCT 1
         FROM CB_MAEFUNC_STG S
         -- EGV 07SEP2015 Inicio
         --WHERE S.COD_MF = l_cod_mf);
         WHERE S.COD_INTFC = l_cod_int
         AND S.COD_MF = l_cod_mf);
         -- EGV 07SEP2015 Fin
         EXCEPTION
            WHEN OTHERS
            THEN
                -- EGV 07SEP2015 Inicio
                l_existe := 0;
                -- EGV 07SEP2015 Fin
                NULL;
         END;

         IF l_existe = 1
         THEN
             SELECT D.*
             BULK COLLECT INTO l_reg
             FROM (SELECT S.*
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
             --AND S.ESTADO_STG = 'P'
             UNION
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'U'
             FROM MAEFUNC_TBL M
             WHERE NOT EXISTS
             (SELECT 1
             FROM CB_MAEFUNC_STG S1
             WHERE M.COD_MF = S1.COD_MF
             -- EGV 22SEP2015 Inicio
             AND S1.COD_INTFC = l_cod_int
             -- EGV 22SEP2015 Fin
             AND M.FECHA_EFECTIVA =
             S1.FECHA_EFECTIVA
             AND M.SEC_EFECTIVA =
             S1.SEC_EFECTIVA)
             AND M.COD_MF = l_cod_mf) D
             ORDER BY D.COD_INTFC,
             D.COD_MF,
             D.FECHA_EFECTIVA,
             D.SEC_EFECTIVA;
         END IF;
     WHEN 'DE'
     THEN
         BEGIN
         l_existe := 0;

         SELECT DISTINCT 1
         INTO l_existe
         FROM CB_MAEFUNC_STG S
         WHERE S.COD_INTFC = l_cod_int
         AND S.COD_MF = l_cod_mf
         AND S.CB_HRMS_EFFDT = l_effdt
         --AND S.ESTADO_STG = 'P'
         AND S.CB_HRMS_EFFSEQ = l_effseq;

         SELECT DISTINCT 2
         INTO l_existe
         FROM CB_MAEFUNC_STG S
         WHERE S.COD_INTFC = l_cod_int
         AND S.COD_MF = l_cod_mf
         AND S.CB_HRMS_EFFDT = l_effdt
         AND S.CB_HRMS_EFFSEQ = l_effseq
         --AND S.ESTADO_STG = 'P'
         AND S.CB_BX_CHANGE = 1;


         EXCEPTION
            WHEN OTHERS
            THEN
                NULL;
         END;

         IF l_existe = 1
         THEN -- No hay registros modificados x Buxis
             SELECT S.*
             BULK COLLECT INTO l_reg
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int
             AND S.COD_MF = l_cod_mf
             AND S.CB_HRMS_EFFDT = l_effdt
             AND S.CB_HRMS_EFFSEQ = l_effseq
             --AND S.ESTADO_STG = 'P'
             ORDER BY S.COD_INTFC,
             S.COD_MF,
             S.FECHA_EFECTIVA,
             S.SEC_EFECTIVA;
         END IF;

         IF l_existe = 2
         THEN
             /* Hay registros modificados x Buxis.
             Tengo que buscar el registro anterior
             para tomar como base de actualizacion.
             */
             BEGIN
             SELECT DISTINCT 1
             INTO l_existe_2
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int
             AND S.COD_MF = l_cod_mf
             AND S.CB_HRMS_EFFDT = l_effdt
             AND S.CB_HRMS_EFFSEQ < l_effseq --AND S.ESTADO_STG = 'P'
             ;
             EXCEPTION
             WHEN OTHERS
             THEN
             l_existe_2 := 0;
             END;

             IF l_existe_2 = 1
             THEN
              /* Existe esa fecha efectiva pero con unas EFFSEQ menor.
              traigo ese registro + el registro a eliminar
              */
                 SELECT S.*
                 BULK COLLECT INTO l_reg
                 FROM CB_MAEFUNC_STG S
                 WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
                 --AND S.ESTADO_STG = 'P'
                 AND ( (S.CB_HRMS_EFFDT = l_effdt
                 AND S.CB_HRMS_EFFSEQ =
                 (SELECT MAX (S2.CB_HRMS_EFFSEQ)
                 FROM CB_MAEFUNC_STG S2
                 WHERE S.COD_INTFC = S2.COD_INTFC
                 AND S.COD_MF = S2.COD_MF
                 AND S.CB_HRMS_EFFDT =
                 S2.CB_HRMS_EFFDT
                 AND S2.CB_HRMS_EFFSEQ <
                 l_effseq))
                 OR (S.CB_HRMS_EFFDT = l_effdt
                 AND S.CB_HRMS_EFFSEQ = l_effseq))
                 ORDER BY S.COD_INTFC,
                 S.COD_MF,
                 S.FECHA_EFECTIVA,
                 S.SEC_EFECTIVA;
             ELSE
                 SELECT S.*
                 BULK COLLECT INTO l_reg
                 FROM CB_MAEFUNC_STG S
                 WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
                 -- AND S.ESTADO_STG = 'P'
                 AND ( (S.CB_HRMS_EFFDT =
                 (SELECT MAX (S2.CB_HRMS_EFFDT)
                 FROM CB_MAEFUNC_STG S2
                 WHERE S.COD_INTFC = S2.COD_INTFC
                 AND S.COD_MF = S2.COD_MF
                 AND S2.CB_HRMS_EFFDT < l_effdt)
                 AND S.CB_HRMS_EFFSEQ =
                 (SELECT MAX (S3.CB_HRMS_EFFSEQ)
                 FROM CB_MAEFUNC_STG S3
                 WHERE S.COD_INTFC = S3.COD_INTFC
                 AND S.COD_MF = S3.COD_MF
                 AND S.CB_HRMS_EFFDT =
                 S3.CB_HRMS_EFFDT))
                 OR (CB_HRMS_EFFDT = l_effdt
                 AND S.CB_HRMS_EFFSEQ = l_effseq))
                 ORDER BY S.COD_INTFC,
                 S.COD_MF,
                 S.FECHA_EFECTIVA,
                 S.SEC_EFECTIVA;
             END IF;
         END IF;
     WHEN 'FSE+'
     THEN
         BEGIN
         SELECT DISTINCT 1
         INTO l_existe
         FROM CB_MAEFUNC_STG S
         WHERE S.COD_INTFC = l_cod_int
         AND S.COD_MF = l_cod_mf
         AND S.CB_HRMS_EFFDT = l_effdt
         AND S.CB_HRMS_EFFSEQ = l_effseq --AND S.ESTADO_STG = 'P'
         ;
         EXCEPTION
            WHEN OTHERS
            THEN
                l_existe := 0;
         END;


         IF l_existe = 1
         THEN -- Existe la clave en la tabla
             SELECT S.*
             BULK COLLECT INTO l_reg
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
             AND ( (S.CB_HRMS_EFFDT = l_effdt
             AND S.CB_HRMS_EFFSEQ >= l_effseq)
             OR (S.CB_HRMS_EFFDT > l_effdt))
             --AND S.ESTADO_STG = 'P'
             ORDER BY S.COD_INTFC,
             S.COD_MF,
             S.FECHA_EFECTIVA,
             S.SEC_EFECTIVA;
         ELSE -- NO Existe la clave en la tabla
             BEGIN
             SELECT DISTINCT 1
             INTO l_existe_2
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int
             AND S.COD_MF = l_cod_mf
             AND S.CB_HRMS_EFFDT = l_effdt
             AND S.CB_HRMS_EFFSEQ < l_effseq --AND S.ESTADO_STG = 'P'
             ;
             EXCEPTION
                 WHEN OTHERS
                 THEN
                    l_existe_2 := 0;
             END;


             IF l_existe_2 = 1
             THEN -- Existe esa fecha efectiva pero con unas EFFSEQ menor
                 SELECT S.*
                 BULK COLLECT INTO l_reg
                 FROM CB_MAEFUNC_STG S
                 WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
                 --AND S.ESTADO_STG = 'P'
                 AND ( (S.CB_HRMS_EFFDT = l_effdt
                 AND S.CB_HRMS_EFFSEQ >=
                 (SELECT MAX (S2.CB_HRMS_EFFSEQ)
                 FROM CB_MAEFUNC_STG S2
                 WHERE S.COD_INTFC = S2.COD_INTFC
                 AND S.COD_MF = S2.COD_MF
                 AND S.CB_HRMS_EFFDT =
                 S2.CB_HRMS_EFFDT
                 AND S2.CB_HRMS_EFFSEQ <
                 l_effseq))
                 OR (S.CB_HRMS_EFFDT > l_effdt))
                 ORDER BY S.COD_INTFC,
                 S.COD_MF,
                 S.FECHA_EFECTIVA,
                 S.SEC_EFECTIVA;
             ELSE
                /* NO Existe la clave en la tabla y NO Existe esa
                fecha efectiva pero conunas EFFSEQ menor
                */
                 SELECT S.*
                 BULK COLLECT INTO l_reg
                 FROM CB_MAEFUNC_STG S
                 WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
                 --AND S.ESTADO_STG = 'P'
                 AND ( (S.CB_HRMS_EFFDT =
                 (SELECT MAX (S2.CB_HRMS_EFFDT)
                 FROM CB_MAEFUNC_STG S2
                 WHERE S.COD_INTFC = S2.COD_INTFC
                 AND S.COD_MF = S2.COD_MF
                 AND S2.CB_HRMS_EFFDT < l_effdt)
                 AND S.CB_HRMS_EFFSEQ =
                 (SELECT MAX (S3.CB_HRMS_EFFSEQ)
                 FROM CB_MAEFUNC_STG S3
                 WHERE S.COD_INTFC = S3.COD_INTFC
                 AND S.COD_MF = S3.COD_MF
                 AND S.CB_HRMS_EFFDT =
                 S3.CB_HRMS_EFFDT))
                 OR (S.CB_HRMS_EFFDT > l_effdt))
                 ORDER BY S.COD_INTFC,
                 S.COD_MF,
                 S.FECHA_EFECTIVA,
                 S.SEC_EFECTIVA;
             END IF;
         END IF;
     WHEN 'FE+'
     THEN
         BEGIN
         SELECT DISTINCT 1
         INTO l_existe
         FROM CB_MAEFUNC_STG S
         WHERE S.COD_INTFC = l_cod_int
         AND S.COD_MF = l_cod_mf
         AND S.CB_HRMS_EFFDT = l_effdt --AND S.ESTADO_STG = 'P'
         ;
         EXCEPTION
             WHEN OTHERS
             THEN
                l_existe := 0;
         END;


         IF l_existe = 1
         THEN -- Existe la clave en la tabla
             SELECT S.*
             BULK COLLECT INTO l_reg
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int
             AND S.COD_MF = l_cod_mf
             AND S.CB_HRMS_EFFDT >= l_effdt
             --AND S.ESTADO_STG = 'P'
             ORDER BY S.COD_INTFC,
             S.COD_MF,
             S.FECHA_EFECTIVA,
             S.SEC_EFECTIVA;
         ELSE -- NO Existe la clave en la tabla
             SELECT S.*
             BULK COLLECT INTO l_reg
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
             --AND S.ESTADO_STG = 'P'
             AND ( (S.CB_HRMS_EFFDT =
             (SELECT MAX (S2.CB_HRMS_EFFDT)
             FROM CB_MAEFUNC_STG S2
             WHERE S.COD_INTFC = S2.COD_INTFC
             AND S.COD_MF = S2.COD_MF
             AND S2.CB_HRMS_EFFDT < l_effdt)
             AND S.CB_HRMS_EFFSEQ =
             (SELECT MAX (S3.CB_HRMS_EFFSEQ)
             FROM CB_MAEFUNC_STG S3
             WHERE S.COD_INTFC = S3.COD_INTFC
             AND S.COD_MF = S3.COD_MF
             AND S.CB_HRMS_EFFDT =
             S3.CB_HRMS_EFFDT))
             OR (S.CB_HRMS_EFFDT > l_effdt))
             ORDER BY S.COD_INTFC,
             S.COD_MF,
             S.FECHA_EFECTIVA,
             S.SEC_EFECTIVA;
         END IF;
     -- EGV 07SEP2015 Inicio
     WHEN 'FSE'
     THEN
         BEGIN

         l_existe := 0;

         SELECT DISTINCT 1
         INTO l_existe
         FROM CB_MAEFUNC_STG S
         WHERE S.COD_INTFC = l_cod_int
         AND S.COD_MF = l_cod_mf
         AND S.CB_HRMS_EFFDT = l_effdt
         AND S.CB_HRMS_EFFSEQ = l_effseq --AND S.ESTADO_STG = 'P'
         ;
         EXCEPTION
            WHEN OTHERS
            THEN
                l_existe := 0;
         END;


         IF l_existe = 1
         THEN -- Existe la clave en la tabla
             SELECT S.*
             BULK COLLECT INTO l_reg
             FROM CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_cod_int AND S.COD_MF = l_cod_mf
             AND S.CB_HRMS_EFFDT = l_effdt
             AND S.CB_HRMS_EFFSEQ = l_effseq
             --AND S.ESTADO_STG = 'P'
             ORDER BY S.COD_INTFC,
             S.COD_MF,
             S.FECHA_EFECTIVA,
             S.SEC_EFECTIVA;
         END IF;
     -- EGV 07SEP2015 Fin
 END CASE;
 END;

 PROCEDURE Buscar_reg_MAEFUNC_TBL (l_cod_int IN INTEGER,
 l_cod_mf INTEGER,
 l_effdt IN DATE,
 l_effseq IN INTEGER,
 l_tipo VARCHAR2,
 l_reg OUT g_mf_list,
 l_existe OUT INTEGER)
 AS
 l_existe_2 INTEGER;
 -- EGV 07SEP2015 Inicio
 l_from_effdt DATE;
 l_from_effseq INTEGER;
 -- EGV 07SEP2015 Fin
 BEGIN
 /* l_tipo :
 FSE+ : Busca todos los registros de la fecha efectiva / seq efectiva pasada por
 parametro para adelante.
 Si no existe esa fecha/sequencia busca el registro anterior ( si existe) el que
 sera usado para cambiarle los datos de fecha/sequencia e insertarlo como nuevo
registro.
 FE + :Busca todos los registros de la fecha efectiva pasada por parametro para
adelante.
 TO : Busca todos los registros correspondiente al empleado pasado por parametro

 DE : Busca los registros a ser borrados.
 */

 l_reg := g_mf_list ();

 CASE l_tipo
     WHEN 'DE'
     THEN
         BEGIN
         l_existe := 0;

         SELECT DISTINCT 1
         INTO l_existe
         FROM MAEFUNC_TBL M
         WHERE M.COD_MF = l_cod_mf
         AND M.CB_HRMS_EFFDT = l_effdt
         AND M.CB_HRMS_EFFSEQ = l_effseq;


         SELECT DISTINCT 2
         INTO l_existe
         FROM MAEFUNC_TBL M
         WHERE M.COD_MF = l_cod_mf
         AND M.CB_HRMS_EFFDT = l_effdt
         AND M.CB_HRMS_EFFSEQ = l_effseq
         AND M.CB_BX_CHANGE = 1;
         EXCEPTION
             WHEN OTHERS
             THEN
                NULL;
         END;

         IF l_existe = 1
         THEN -- No hay registros modificados x Buxis
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'D'
             BULK COLLECT INTO l_reg
             FROM MAEFUNC_TBL M
             WHERE NOT EXISTS
             (SELECT 1
             FROM CB_MAEFUNC_STG S1
             WHERE M.COD_MF = S1.COD_MF
             AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
             AND M.FECHA_EFECTIVA = S1.FECHA_EFECTIVA
             AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
             AND M.COD_MF = l_cod_mf
             AND M.CB_HRMS_EFFDT = l_effdt
             AND M.CB_HRMS_EFFSEQ = l_effseq
             ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;
         END IF;

         IF l_existe = 2
         THEN
              /*Hay registros modificados x Buxis.
              Tengo que buscar el registro anterior
              para tomar como base de actualizacion.
              */
             BEGIN
             SELECT DISTINCT 1
             INTO l_existe_2
             FROM MAEFUNC_TBL M
             WHERE M.COD_MF = l_cod_mf
             AND M.CB_HRMS_EFFDT = l_effdt
             AND M.CB_HRMS_EFFSEQ < l_effseq;
             EXCEPTION
                WHEN OTHERS
                THEN
                    l_existe_2 := 0;
             END;

             IF l_existe_2 = 1
             THEN
                /* Existe esa fecha efectiva pero con unas EFFSEQ menor.
                traigo ese registro + el registro a eliminar
                */
                 SELECT l_cod_int,
                 M.*,
                 'P',
                 '',
                 (CASE
                 WHEN M.CB_HRMS_EFFSEQ < l_effseq
                 THEN
                 'B'
                 WHEN M.CB_HRMS_EFFSEQ = l_effseq
                 AND M.CB_BX_CHANGE = 0
                 THEN
                 'D'
                 ELSE
                 'U'
                 END)
                 BULK COLLECT INTO l_reg
                 FROM MAEFUNC_TBL M
                 WHERE NOT EXISTS
                 (SELECT 1
                 FROM CB_MAEFUNC_STG S1
                 WHERE M.COD_MF = S1.COD_MF
                 AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
                 AND M.FECHA_EFECTIVA =
                 S1.FECHA_EFECTIVA
                 AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
                 AND M.COD_MF = l_cod_mf
                 AND ( (M.CB_HRMS_EFFDT = l_effdt
                 AND M.CB_HRMS_EFFSEQ =
                 (SELECT MAX (M2.CB_HRMS_EFFSEQ)
                 FROM MAEFUNC_TBL M2
                 WHERE M.COD_MF = M2.COD_MF
                 AND M.CB_HRMS_EFFDT =
                 M2.CB_HRMS_EFFDT
                 AND M2.CB_HRMS_EFFSEQ <
                 l_effseq))
                 OR (M.CB_HRMS_EFFDT = l_effdt
                 AND M.CB_HRMS_EFFSEQ = l_effseq))
                 ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;
             ELSE
      -- Traigo la fecha efectiva anterior + el registro a eliminar
                 SELECT l_cod_int,
                 M.*,
                 'P',
                 '',
                 (CASE
                 WHEN M.CB_HRMS_EFFDT < l_effdt
                 THEN
                 'B'
                 WHEN M.CB_HRMS_EFFDT = l_effdt
                 AND M.CB_BX_CHANGE = 0
                 THEN
                 'D'
                 ELSE
                 'U'
                 END)
                 BULK COLLECT INTO l_reg
                 FROM MAEFUNC_TBL M
                 WHERE NOT EXISTS
                 (SELECT 1
                 FROM CB_MAEFUNC_STG S1
                 WHERE M.COD_MF = S1.COD_MF
                 AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
                 AND M.FECHA_EFECTIVA =
                 S1.FECHA_EFECTIVA
                 AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
                 AND M.COD_MF = l_cod_mf
                 AND ( (M.CB_HRMS_EFFDT =
                 (SELECT MAX (M2.CB_HRMS_EFFDT)
                 FROM MAEFUNC_TBL M2
                 WHERE M.COD_MF = M2.COD_MF
                 AND M2.CB_HRMS_EFFDT < l_effdt)
                 AND M.CB_HRMS_EFFSEQ =
                 (SELECT MAX (M3.CB_HRMS_EFFSEQ)
                 FROM MAEFUNC_TBL M3
                 WHERE M.COD_MF = M3.COD_MF
                 AND M.CB_HRMS_EFFDT =
                 M3.CB_HRMS_EFFDT))
                 OR (M.CB_HRMS_EFFDT = l_effdt
                 AND M.CB_HRMS_EFFSEQ = l_effseq))
                 ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;
             END IF;
         END IF;
     WHEN 'FSE+'
     THEN

         BEGIN
         SELECT DISTINCT 1
         INTO l_existe
         FROM MAEFUNC_TBL M
         WHERE M.COD_MF = l_cod_mf
         AND M.CB_HRMS_EFFDT = l_effdt
         AND M.CB_HRMS_EFFSEQ = l_effseq;
         EXCEPTION
            WHEN OTHERS
            THEN
                l_existe := 0;
         END;


         IF l_existe = 1
         THEN -- Existe la clave en la tabla
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'U'
             BULK COLLECT INTO l_reg
             FROM MAEFUNC_TBL M
             WHERE NOT EXISTS
             (SELECT 1
             FROM CB_MAEFUNC_STG S1
             WHERE M.COD_MF = S1.COD_MF
             AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
             AND M.FECHA_EFECTIVA = S1.FECHA_EFECTIVA
             AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
             AND M.COD_MF = l_cod_mf
             AND ( (M.CB_HRMS_EFFDT = l_effdt
             AND M.CB_HRMS_EFFSEQ >= l_effseq)
             OR (M.CB_HRMS_EFFDT > l_effdt));

         ELSE -- NO Existe la clave en la tabla
             BEGIN
             SELECT DISTINCT 1
             INTO l_existe_2
             FROM MAEFUNC_TBL M
             WHERE M.COD_MF = l_cod_mf
             AND M.CB_HRMS_EFFDT = l_effdt
             AND M.CB_HRMS_EFFSEQ < l_effseq;
             EXCEPTION
                 WHEN OTHERS
                 THEN
                    l_existe_2 := 0;
             END;


             IF l_existe_2 = 1
             THEN -- Existe esa fecha efectiva pero con unas EFFSEQ menor
                 SELECT l_cod_int,
                 M.*,
                 'P',
                 '',
                 'U'
                 BULK COLLECT INTO l_reg
                 FROM MAEFUNC_TBL M
                 WHERE NOT EXISTS
                 (SELECT 1
                 FROM CB_MAEFUNC_STG S1
                 WHERE M.COD_MF = S1.COD_MF
                 AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
                 AND M.FECHA_EFECTIVA =
                 S1.FECHA_EFECTIVA
                 AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
                 AND M.COD_MF = l_cod_mf
                 AND ( (M.CB_HRMS_EFFDT = l_effdt
                 AND M.CB_HRMS_EFFSEQ >=
                 (SELECT MAX (M2.CB_HRMS_EFFSEQ)
                 FROM MAEFUNC_TBL M2
                 WHERE M.COD_MF = M2.COD_MF
                 AND M.CB_HRMS_EFFDT =
                 M2.CB_HRMS_EFFDT
                 AND M2.CB_HRMS_EFFSEQ <
                 l_effseq))
                 OR (M.CB_HRMS_EFFDT > l_effdt));

             ELSE
                /*NO Existe la clave en la tabla y NO Existe esa
                fecha efectiva pero con unas EFFSEQ menor
                */
                 SELECT l_cod_int,
                 M.*,
                 'P',
                 '',
                 'U'
                 BULK COLLECT INTO l_reg
                 FROM MAEFUNC_TBL M
                 WHERE NOT EXISTS
                 (SELECT 1
                 FROM CB_MAEFUNC_STG S1
                 WHERE M.COD_MF = S1.COD_MF
                 AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
                 AND M.FECHA_EFECTIVA =
                 S1.FECHA_EFECTIVA
                 AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
                 AND M.COD_MF = l_cod_mf
                 AND ( (M.CB_HRMS_EFFDT =
                 (SELECT MAX (M2.CB_HRMS_EFFDT)
                 FROM MAEFUNC_TBL M2
                 WHERE M.COD_MF = M2.COD_MF
                 AND M2.CB_HRMS_EFFDT < l_effdt)
                 AND M.CB_HRMS_EFFSEQ =
                 (SELECT MAX (M3.CB_HRMS_EFFSEQ)
                 FROM MAEFUNC_TBL M3
                 WHERE M.COD_MF = M3.COD_MF
                 AND M.CB_HRMS_EFFDT =
                 M3.CB_HRMS_EFFDT))
                 OR (M.CB_HRMS_EFFDT > l_effdt))
                 ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;

             END IF;
         END IF;
     WHEN 'FE+'
     THEN
         BEGIN
         SELECT DISTINCT 1
         INTO l_existe
         FROM MAEFUNC_TBL M
         WHERE M.COD_MF = l_cod_mf AND M.CB_HRMS_EFFDT = l_effdt
      --AND M.ESTADO_STG = 'P'
         ;
         EXCEPTION
             WHEN OTHERS
             THEN
                l_existe := 0;
         END;


         IF l_existe = 1
         THEN -- Existe la clave en la tabla
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'U'
             BULK COLLECT INTO l_reg
             FROM MAEFUNC_TBL M
             WHERE NOT EXISTS
             (SELECT 1
             FROM CB_MAEFUNC_STG S1
             WHERE M.COD_MF = S1.COD_MF
             AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
             AND M.FECHA_EFECTIVA = S1.FECHA_EFECTIVA
             AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
             AND M.COD_MF = l_cod_mf
             AND M.CB_HRMS_EFFDT >= l_effdt
             --AND M.ESTADO_STG = 'P'
             ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;
         ELSE -- NO Existe la clave en la tabla
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'U'
             BULK COLLECT INTO l_reg
             FROM MAEFUNC_TBL M
             WHERE NOT EXISTS
             (SELECT 1
             FROM CB_MAEFUNC_STG S1
             WHERE M.COD_MF = S1.COD_MF
             AND S1.COD_INTFC = l_cod_int       -- EGV 16Jul2015
             AND M.FECHA_EFECTIVA = S1.FECHA_EFECTIVA
             AND M.SEC_EFECTIVA = S1.SEC_EFECTIVA)
             AND M.COD_MF = l_cod_mf
             --AND M.ESTADO_STG = 'P'
             AND ( (M.CB_HRMS_EFFDT =
             (SELECT MAX (M2.CB_HRMS_EFFDT)
             FROM MAEFUNC_TBL M2
             WHERE M.COD_MF = M2.COD_MF
             AND M2.CB_HRMS_EFFDT < l_effdt)
             AND M.CB_HRMS_EFFSEQ =
             (SELECT MAX (M3.CB_HRMS_EFFSEQ)
             FROM MAEFUNC_TBL M3
             WHERE M.COD_MF = M3.COD_MF
             AND M.CB_HRMS_EFFDT =
             M3.CB_HRMS_EFFDT))
             OR (M.CB_HRMS_EFFDT > l_effdt))
             ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;
         END IF;
     -- EGV 07SEP2015 Inicio
     WHEN 'FSE'
     THEN
         BEGIN
         SELECT DISTINCT 1
         INTO l_existe
         FROM MAEFUNC_TBL M
         WHERE M.COD_MF = l_cod_mf
         AND M.CB_HRMS_EFFDT = l_effdt
         AND M.CB_HRMS_EFFSEQ = l_effseq;
         EXCEPTION
            WHEN OTHERS
            THEN
                l_existe := 0;
         END;


         IF l_existe = 1
         THEN -- Existe la clave en la tabla
             SELECT l_cod_int,
             M.*,
             'P',
             '',
             'U'
             BULK COLLECT INTO l_reg
             FROM MAEFUNC_TBL M
             WHERE M.COD_MF = l_cod_mf
             AND M.CB_HRMS_EFFDT = l_effdt
             AND M.CB_HRMS_EFFSEQ = l_effseq
             ORDER BY M.COD_MF, M.FECHA_EFECTIVA, M.SEC_EFECTIVA;

         ELSE -- NO Existe la clave en la tabla

             l_existe_2 := 0;

             BEGIN
             SELECT 1, M.FECHA_EFECTIVA, M.SEC_EFECTIVA
             INTO l_existe_2, l_from_effdt, l_from_effseq
             FROM MAEFUNC_TBL M
             WHERE M.COD_MF = l_cod_mf
             AND M.FECHA_EFECTIVA = (SELECT MAX(M2.FECHA_EFECTIVA)
                                    FROM MAEFUNC_TBL M2
                                        WHERE M.COD_MF = M2.COD_MF
                                        AND M2.FECHA_EFECTIVA <= l_effdt)
             AND M.SEC_EFECTIVA = (SELECT MAX(M3.SEC_EFECTIVA)
                                    FROM MAEFUNC_TBL M3
                                   WHERE M.COD_MF = M3.COD_MF
                                   AND M.FECHA_EFECTIVA = M3.FECHA_EFECTIVA
                                   AND ((M3.FECHA_EFECTIVA = l_effdt
                                   AND M3.SEC_EFECTIVA <= l_effseq)
                                                OR M3.FECHA_EFECTIVA < l_effdt
      ));
             EXCEPTION
                WHEN OTHERS
                THEN
                    l_existe_2 := 0;
             END;

             IF l_existe_2 = 1 THEN

                SELECT D.*
                BULK COLLECT INTO l_reg
                FROM (
                SELECT S.*
                FROM CB_MAEFUNC_STG S
                WHERE S.COD_INTFC = l_cod_int
                AND S.COD_MF = l_cod_mf
                AND ((S.FECHA_EFECTIVA = l_from_effdt AND S.SEC_EFECTIVA >=
                l_from_effseq) OR S.FECHA_EFECTIVA > l_from_effdt)
                AND (S.CB_HRMS_EFFDT < l_effdt
                  OR (S.CB_HRMS_EFFDT = l_effdt
                AND S.CB_HRMS_EFFSEQ <= l_effseq))
                UNION
                SELECT l_cod_int,
                  M.*,
                 'P',
                 '',
                 'U'
                FROM MAEFUNC_TBL M
                WHERE M.COD_MF = l_cod_mf
                AND ((M.FECHA_EFECTIVA = l_from_effdt
                  AND M.SEC_EFECTIVA >= l_from_effseq)
                  OR M.FECHA_EFECTIVA > l_from_effdt)
                AND (M.CB_HRMS_EFFDT < l_effdt
                  OR (M.CB_HRMS_EFFDT = l_effdt
                    AND M.CB_HRMS_EFFSEQ <= l_effseq))
                AND NOT EXISTS (SELECT 1 FROM CB_MAEFUNC_STG S2
                                WHERE S2.COD_INTFC = l_cod_int
                                AND S2.COD_MF = M.COD_MF
                                AND S2.FECHA_EFECTIVA = M.FECHA_EFECTIVA
                                AND S2.SEC_EFECTIVA = M.SEC_EFECTIVA)
                ) D
                ORDER BY D.COD_INTFC,
                D.COD_MF,
                D.FECHA_EFECTIVA,
                D.SEC_EFECTIVA;

             END IF;

         END IF;
     -- EGV 07SEP2015 Fin
 END CASE;
 END;

 PROCEDURE Valida_reg_MAEFUNC_STG (l_cod_int IN INTEGER,
 l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER := 0;
 l_errores INTEGER := 0;
 l_det VARCHAR2 (400);
 l_cant_orig INTEGER := 0;      -- EGV 07SEP2015
 l_region VARCHAR2(5);      -- EGV 15OCT2015

 EXC_LOG EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_reg_MAEFUNC_STG';

 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 SELECT M.*
 BULK COLLECT INTO t_mf
 FROM CB_MAEFUNC_STG M
 WHERE M.COD_INTFC = l_cod_int AND M.ESTADO_STG = 'P';

 IF t_mf.COUNT > 0
 THEN
     FOR X IN t_mf.FIRST .. t_mf.LAST
     LOOP
         l_det :=
         'COD_MF : '
         || t_mf (x).cod_mf
         || ' EFFDT : '
         || t_mf (x).FECHA_EFECTIVA
         || ' EFFSEQ : '
         || t_mf (x).SEC_EFECTIVA;

         BEGIN
         -- Verifico la empresa
         IF t_mf (X).COD_EMP IS NOT NULL
         THEN
             SELECT DISTINCT 1
             INTO l_existe
             FROM QSEMPRESA E
             WHERE E.COD_EMP = t_mf (X).COD_EMP;
             END IF;
         EXCEPTION
             WHEN NO_DATA_FOUND
             THEN
                 l_errores := l_errores + 1;
                 t_mf (X).ESTADO_STG := 'E';
                 g_err_descr :=
                 l_det
                 || ' --> No existe la empresa : '
                 || t_mf (X).COD_EMP;
                 BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);
         END;

         BEGIN
         -- Verifico el SEXO
         IF t_mf (X).SEXO_MF IS NOT NULL
         THEN
             SELECT DISTINCT 1
             INTO l_existe
             FROM QSCODIGOS C
             WHERE C.GRUPO = '15'
             AND TRIM (C.CODIGO) = TRIM (t_mf (X).SEXO_MF);
         END IF;
         EXCEPTION
             WHEN NO_DATA_FOUND
             THEN
                 l_errores := l_errores + 1;
                 t_mf (X).ESTADO_STG := 'E';
                 g_err_descr :=
                 l_det
                 || ' --> No existe el sexo : '
                 || TRIM (t_mf (X).SEXO_MF);
                 BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);
         END;

         BEGIN
         -- Verifico el Estado Civil
         IF t_mf (X).EST_CIV_MF IS NOT NULL
         THEN
             SELECT DISTINCT 1
             INTO l_existe
             FROM QSCODIGOS C
             -- EGV 07SEP2015 Inicio
             --WHERE C.GRUPO = '10'
             WHERE C.GRUPO = '30001'
             -- EGV 07SEP2015 Fin
             AND TRIM (C.CODIGO) = TRIM (t_mf (X).EST_CIV_MF);
         END IF;
         EXCEPTION
             WHEN NO_DATA_FOUND
             THEN
                 l_errores := l_errores + 1;
                 t_mf (X).ESTADO_STG := 'E';
                 g_err_descr :=
                 l_det
                 || ' --> No existe el Estado Civil : '
                 || TRIM (t_mf (X).EST_CIV_MF);
                 BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);
         END;

         BEGIN
         -- Verifico la Sucursal
         IF t_mf (X).SECC_MF IS NOT NULL
         THEN
             SELECT DISTINCT 1
             INTO l_existe
             FROM CB_SUCURSAL S
             WHERE TRIM (S.COD_SUC) = TRIM (t_mf (X).SECC_MF);
         END IF;
         EXCEPTION
             WHEN NO_DATA_FOUND
             THEN
                 l_errores := l_errores + 1;
                 t_mf (X).ESTADO_STG := 'E';
                 g_err_descr :=
                 l_det
                 || ' --> No existe la Sucursal : '
                 || TRIM (t_mf (X).SECC_MF);
                 BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);
         END;

         BEGIN
         -- Verifico el Grado
         IF t_mf (X).GRADO IS NOT NULL
         THEN
             SELECT DISTINCT 1
             INTO l_existe
             FROM CB_GRADO G
             WHERE TRIM (G.GRADO) = TRIM (t_mf (X).GRADO);
         END IF;
         EXCEPTION
             WHEN NO_DATA_FOUND
             THEN

                -- EGV 07SEP2015 Inicio
                --l_errores := l_errores + 1;
                --t_mf (X).ESTADO_STG := 'E';

                --g_err_descr :=
                -- l_det
                -- || ' --> No existe el Grado : '
                -- || TRIM (t_mf (X).GRADO);
                -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                -- g_proc_int,
                -- 'E',
                -- g_err_descr,
                -- g_res_log);

                -- EGV 19OCT2015 Inicio
                l_region := 'ARG';

                BEGIN
                SELECT trim(REGION)
                    Into l_region
                FROM QSEMPRESA
                WHERE COD_EMP = t_mf(X).COD_EMP;
                EXCEPTION
                    WHEN OTHERS THEN
                        NULL;
                END;

                IF  l_region = 'URY' THEN
                    g_err_descr :=
                     l_det
                     || ' --> No existe el Grado : '
                     || TRIM (t_mf (X).GRADO)
                     || ' no invalida el registro.';
                     BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                     g_proc_int,
                     'I',
                     g_err_descr,
                     g_res_log);
                ELSE
                -- EGV 19OCT2015 Fin

                    l_cant_orig := 0;

                    SELECT COUNT(*)
                    INTO l_cant_orig
                    FROM CB_JOB_INT
                    WHERE COD_INTFC = l_cod_int
                    AND EMPLID = TRIM(t_mf (X).CB_GEID)
                    AND EMPL_RCD = t_mf (X).CB_EMPL_RCD
                    AND EFFDT = t_mf (X).CB_HRMS_EFFDT
                    AND EFFSEQ = t_mf (X).CB_HRMS_EFFSEQ;

                    IF l_cant_orig > 0 THEN
                        l_errores := l_errores + 1;
                        t_mf (X).ESTADO_STG := 'E';

                        g_err_descr :=
                         l_det
                         || ' --> No existe el Grado : '
                         || TRIM (t_mf (X).GRADO);
                         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                         g_proc_int,
                         'E',
                         g_err_descr,
                         g_res_log);

                    ELSE

                        g_err_descr :=
                         l_det
                         || ' --> No existe el Grado : '
                         || TRIM (t_mf (X).GRADO)
                         || ' no invalida el registro.';
                         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                         g_proc_int,
                         'I',
                         g_err_descr,
                         g_res_log);

                    END IF;
                    -- EGV 07SEP2015 Fin
                -- EGV 19OCT2015 Inicio
                END IF;
                -- EGV 19OCT2015 Fin

         END;


         BEGIN
         -- Verifico el Centro de costos
         IF t_mf (X).CENCOS_MF IS NOT NULL
         THEN
             SELECT DISTINCT 1
             INTO l_existe
             FROM CB_CENCOS C
             WHERE TRIM(C.COD_EMP) =t_mf (X).COD_EMP
             AND TRIM (C.QS_CENCOS) = TRIM (t_mf (X).CENCOS_MF);
         END IF;
         EXCEPTION
             WHEN NO_DATA_FOUND
             THEN

                 -- EGV 07SEP2015 Inicio
                 /*
                 l_errores := l_errores + 1;
                 t_mf (X).ESTADO_STG := 'E';
                 g_err_descr :=
                 l_det
                 || ' --> No existe el Centro de Costos : '
                 || TRIM (t_mf (X).CENCOS_MF)
                 ||  ' para la empresa ' || t_mf (X).COD_EMP;
                 BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);
                 */

                 l_cant_orig := 0;

                 SELECT COUNT(*)
                 INTO l_cant_orig
                 FROM CB_JOB_INT
                 WHERE COD_INTFC = l_cod_int
                 AND EMPLID = TRIM(t_mf (X).CB_GEID)
                 AND EMPL_RCD = t_mf (X).CB_EMPL_RCD
                 AND EFFDT = t_mf (X).CB_HRMS_EFFDT
                 AND EFFSEQ = t_mf (X).CB_HRMS_EFFSEQ;

                 IF l_cant_orig > 0 THEN
                     l_errores := l_errores + 1;
                     t_mf (X).ESTADO_STG := 'E';

                     g_err_descr :=
                      l_det
                      || ' --> No existe el Centro de Costos : '
                      || TRIM (t_mf (X).CENCOS_MF)
                      ||  ' para la empresa ' || t_mf (X).COD_EMP;
                      BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                      g_proc_int,
                      'E',
                      g_err_descr,
                      g_res_log);

                 ELSE

                     g_err_descr :=
                      l_det
                      || ' --> No existe el Centro de Costos : '
                      || TRIM (t_mf (X).CENCOS_MF) ||  ' para la empresa '
                      || t_mf (X).COD_EMP || ' no invalida registro.';
                      BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
                      g_proc_int,
                      'I',
                      g_err_descr,
                      g_res_log);

                 END IF;
                 -- EGV 07SEP2015 Fin
         END;



         BEGIN
         -- Verifico NULOS
         IF TRIM (t_mf (X).PRI_NOM_MF) IS NULL
         OR t_mf (X).COD_MF IS NULL
         OR t_mf (X).COD_EMP IS NULL
         OR t_mf (X).NIVEL IS NULL
         OR TRIM (t_mf (X).PRI_APE_MF) IS NULL
         THEN
             l_errores := l_errores + 1;
             t_mf (X).ESTADO_STG := 'E';
             g_err_descr :=
             l_det
             || ' --> No pueden estar en Nulo los siguientes
             campos : COD_MF , COD_EMP , NIVEL , PRI_NOM_MF , PRI_APE_MF ';
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
         END IF;
         END;
     END LOOP;

     Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);

     IF l_errores = 0
     THEN
        l_res := 'OK';
     ELSE
         l_res :=
         'Se han encontrado ' || l_errores || ' registros con errores';
     END IF;
 ELSE
     l_res := 'OK';
     g_err_descr := 'No existen registros pendientes de validar';
     BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
     g_proc_int,
     'I',
     g_err_descr,
     g_res_log);
 END IF;
 EXCEPTION
     WHEN OTHERS
     THEN
         g_err_descr := ' ERROR : ' || SQLERRM;
         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (l_cod_int,
         g_proc_int,
         'E',
         g_err_descr,
         g_res_log);
 END;

 PROCEDURE Impacta_reg_MAEFUNC_STG (l_accion IN VARCHAR2,
 l_reg IN g_mf_list,
 l_res OUT VARCHAR2)
 AS
 l_det VARCHAR2 (400) := ' ';
 BEGIN
 g_proc_int := 'Impacta_reg_MAEFUNC_STG';

 CASE l_accion
     WHEN 'I'
     THEN
         FOR X IN l_reg.FIRST .. l_reg.LAST
         LOOP
             BEGIN
             l_det :=
             'COD_MF : '
             || l_reg (x).cod_mf
             || ' AUDIT_ACTN : '
             || l_reg (x).tipo_tran
             || ' EFFDT : '
             || l_reg (x).FECHA_EFECTIVA
             || ' EFFSEQ; : '
             || l_reg (x).SEC_EFECTIVA;

             -- EGV 20Dic2016 Inicio
             IF l_reg (x).TIPO_TRAN <> 'X'
             THEN
             -- EGV 20Dic2016 Fin
                 INSERT INTO CB_MAEFUNC_STG
                 VALUES l_reg (x);
             END IF;    -- EGV 20Dic2016
             EXCEPTION
                 WHEN OTHERS
                 THEN
                     BASEARG.PKG_BXSCITI_GENERAL.
                     GRABAR_LOG_DET (g_cod_int,
                     g_proc_int,
                     'E',
                     l_det || ' ERROR : ' || SQLERRM,
                     g_res_log);
             END;
         END LOOP;
     WHEN 'D'
     THEN
         NULL;
     WHEN 'U'
     THEN
         FOR X IN l_reg.FIRST .. l_reg.LAST
         LOOP
             BEGIN
             l_det :=
             'COD_MF : '
             || l_reg (x).cod_mf
             || ' AUDIT_ACTN : '
             || l_reg (x).tipo_tran
             || ' EFFDT : '
             || l_reg (x).FECHA_EFECTIVA
             || ' EFFSEQ; : '
             || l_reg (x).SEC_EFECTIVA;

             DELETE CB_MAEFUNC_STG S
             WHERE S.COD_INTFC = l_reg (x).COD_INTFC
             AND S.COD_MF = l_reg (x).COD_MF
             AND S.FECHA_EFECTIVA = l_reg (x).FECHA_EFECTIVA
             AND S.SEC_EFECTIVA = l_reg (x).SEC_EFECTIVA;

             -- EGV 20Dic2016 Inicio
             IF l_reg (x).TIPO_TRAN <> 'X'
             THEN
             -- EGV 20Dic2016 Fin
                 INSERT INTO CB_MAEFUNC_STG
                 VALUES l_reg (x);
             END IF;    -- EGV 20Dic2016
             EXCEPTION
                 WHEN OTHERS
                 THEN
                     BASEARG.PKG_BXSCITI_GENERAL.
                     GRABAR_LOG_DET (g_cod_int,
                     g_proc_int,
                     'E',
                     l_det || ' ERROR : ' || SQLERRM,
                     g_res_log);
             END;
         END LOOP;
     ELSE
         l_res := 'ERROR';
 END CASE;
 END;


PROCEDURE Validacion_Interfaz (Cod_int IN INTEGER, Resultado OUT VARCHAR2)
AS
  l_estado VARCHAR2 (2);
  l_error BOOLEAN;
  l_res VARCHAR2 (500);
  BEGIN
    /*SE EJECUTAN LAS VALIDACIONES CORRESPONDIENTE
    Y SE LLENAN LAS TABLAS DE STAGING
    */

    g_cod_int := Cod_int;
    l_error := FALSE;


    Valida_CB_JOB_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_JOB_JR_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_PERS_DT_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_ADDR_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_EMPL_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_PERS_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_NAMES_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_CB_PERS_NID_INT (l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    Valida_reg_MAEFUNC_STG (Cod_int, l_res);
    IF l_res <> 'OK'
    THEN
       l_error := TRUE;
    END IF;


    g_proc_int := 'Validacion Interfaz HRMS';

    BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (Cod_int,
    g_proc_int,
    'I',
    'Resultado : ' || l_res,
    g_res_log);


    IF l_error = TRUE
    THEN
        l_estado := 'VE';
        Resultado := 'Ocurrio un Error validando la interfaz';
    ELSE
        l_estado := 'VO';
        Resultado := 'Validacion OK';
    END IF;

    UPDATE CB_INTFC I
    SET I.ESTADO_INTFC = l_estado,
    I.FEC_STG = SYSDATE,
    OBSERVACIONES = Resultado
    WHERE I.COD_INTFC = Cod_Int;

    UPDATE CB_LOG_CAB L
    SET L.ESTADO = l_estado, L.FEC_FIN = SYSDATE
    WHERE L.ID_PROC = Cod_Int;

  COMMIT;
END;

/** 0220**/
PROCEDURE Valida_CB_JOB_INT (l_res OUT VARCHAR2)
AS
  t_mf g_mf_list;
  l_existe INTEGER;
  l_cod_mf INTEGER;
  l_t_trans VARCHAR2 (1);
  l_index INTEGER := 0;
  l_errores INTEGER;



  l_COD_EMP NUMBER (5);
  l_REGION VARCHAR2 (3);
  -- EGV 15SEP2015 Inicio
  --l_SINDICATO VARCHAR (20);
  -- EGV 15SEP2015 Fin

  CURSOR C_DATOS
  IS
    SELECT *
    FROM CB_JOB_INT
    WHERE COD_INTFC = g_cod_int
      -- EGV 07SEP2015 Inicio
      --ORDER BY EMPLID,
    ORDER BY NRO_LINEA, EMPLID,
      -- EGV 07SEP2015 Fin
      EMPL_RCD,
      EFFDT,
      EFFSEQ;


  NEXT_VAL EXCEPTION;
  LOG_EXC EXCEPTION;
BEGIN
    g_proc_int := 'Valida_CB_JOB_INT';

    BASEARG.PKG_BXSCITI_GENERAL.
    GRABAR_LOG_DET (g_cod_int,
    g_proc_int,
    'I',
    'Inicio de proceso : ' || g_proc_int,
    g_res_log);

    FOR J IN C_DATOS
    LOOP
        BEGIN
        l_existe := 0;
        l_COD_EMP := NULL;
        l_REGION := '';
        -- EGV 15SEP2015 Inicio
        --l_SINDICATO := '';
        -- EGV 15SEP2015 Fin

        t_mf := g_mf_list ();

        g_err_descr := '';


        -- EGV 01Jul2015 - Inicio
        BEGIN
          SELECT E.COD_EMP, TRIM (E.REGION)
          INTO l_COD_EMP, l_REGION
          FROM QSEMPRESA E
          WHERE TRIM (E.COMPANY) = 
            case when J.COMPANY in ('UUY','CUY','FUY','WUY')
              then J.COMPANY
              else substr(J.COMPANY,2,2)
            end;
          EXCEPTION
             WHEN OTHERS
             THEN
              l_errores := l_errores + 1;
              l_COD_EMP := NULL;
              l_REGION := ' ';
              g_err_descr :=
              ' No existe conversion para la empresa :'
              || J.COMPANY;
              BASEARG.PKG_BXSCITI_GENERAL.
              GRABAR_LOG_DET (g_cod_int,
              g_proc_int,
              'E',
              g_err_descr,
              g_res_log);
        END;
        -- EGV 01Jul2015 - Fin

        -- EGV 07SEP2015 Inicio
        IF l_REGION <> 'URY' THEN
           J.CGG_EXPENSE := LPAD(J.CGG_EXPENSE,7,'0');
        END IF;
        -- EGV 07SEP2015 Fin


        -- EGV 01Jul2015 - Inicio
        --Buscar_Emplid (J.EMPLID, J.EMPL_RCD, l_cod_mf);
        Buscar_Emplid (J.EMPLID, J.EMPL_RCD, l_REGION, l_cod_mf);
        -- EGV 01Jul2015 - Fin

        IF l_cod_mf = 0
        THEN
          -- EGV 12FEB2016 Inicio
          --Armar_Cod_Mf (J.EMPLID, J.EMPL_RCD, l_cod_mf);
          Armar_Cod_Mf (J.EMPLID, J.EMPL_RCD, l_REGION, l_cod_mf);
          -- EGV 12FEB2016 Fin
        ELSE
          IF l_cod_mf = -1
          THEN
               g_err_descr := 
                  'Error buscando el empleado : ' 
                  || J.EMPLID;
               l_errores := l_errores + 1;
               RAISE LOG_EXC;
          END IF;
        END IF;

        Obtener_Tipo_Trans (J.AUDIT_ACTN, l_t_trans);

        -- g_err_descr :=
        -- 'COD_MF : '
        -- || l_cod_mf
        -- || ' AUDIT_ACTN : '
        -- || l_t_trans
        -- || ' EFFDT : '
        -- || J.EFFDT
        -- || ' EFFSEQ; : '
        -- || J.EFFSEQ;
        -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
        -- g_proc_int,
        -- 'I',
        -- g_err_descr,
        -- g_res_log);

        IF l_t_trans = 'D'
        THEN --Es una Eliminacion
          -- Busco si existen registros en MAEFUNC_STG

          BASEARG.PKG_INT_HRMS_VALIDA.
          BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
            l_cod_mf,
            J.EFFDT,
            J.EFFSEQ,
            'DE',
            J.EMPLID,   -- EGV 01Jul2015
            t_mf,
            l_existe);
          IF l_existe > 0
            THEN --Existen registros en la tabla MAEFUNC_STG para procesar
                IF l_existe = 1
                THEN -- No hay registros modificados por Buxis
                    IF t_mf.COUNT > 0
                    THEN
              /*Existen registros en 
                        la tabla MAEFUNC_STG para eliminar
                        */
                        FOR X IN t_mf.FIRST .. t_mf.LAST
                        LOOP
                          -- EGV 20Dic2016 Inicio
                          IF t_mf (X).TIPO_TRAN = 'I'
                          THEN
                            t_mf (X).TIPO_TRAN := 'X';
                          ELSE
                            -- EGV 20Dic2016 Fin
                            t_mf (X).TIPO_TRAN := 'D';
                            -- EGV 20Dic2016 Inicio
                          END IF;
                          -- EGV 20Dic2016 Fin
                        END LOOP;

                        Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                    END IF;
                END IF;

                IF l_existe = 2
                THEN -- Hay registros modificados por Buxis
                    IF t_mf.COUNT > 0
                    THEN
              /*
                        --Existen registros en la tabla 
                        MAEFUNC_STG para procesar
                        */
                        FOR X IN t_mf.FIRST .. t_mf.LAST
                        LOOP
                            IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
                            OR t_mf (X).CB_HRMS_EFFSEQ <> J.EFFSEQ
                            THEN
                                l_index := X;
                                t_mf (X).TIPO_TRAN := 'B';
                            END IF;
                        END LOOP;

                        FOR X IN t_mf.FIRST .. t_mf.LAST
                        LOOP
                            IF X <> l_index
                            THEN
                                IF t_mf (X).CB_BX_CHANGE = 0
                                THEN
                                   -- EGV 20Dic2016 Inicio
                                   IF t_mf (X).TIPO_TRAN = 'I'
                                   THEN
                                       t_mf (X).TIPO_TRAN := 'X';
                                   ELSE
                                   -- EGV 20Dic2016 Fin
                                       t_mf (X).TIPO_TRAN := 'D';
                                   -- EGV 20Dic2016 Inicio
                                   END IF;
                                   -- EGV 20Dic2016 Fin
                                ELSE
                                  /*
                                        -- Si no fue un cambio 
                                        -- por el portal (CB_BX_CHANGE=1)
                                        */
                                    t_mf (X).CB_DEPT := 
                                            t_mf (l_index).CB_DEPT;
                                    t_mf (X).COD_CATE := 
                                            t_mf (l_index).COD_CATE;
                                    t_mf (X).ACCION := 
                                            t_mf (l_index).ACCION;
                                    t_mf (X).ACCION_DT :=
                                    t_mf (l_index).ACCION_DT;
                                    t_mf (X).ACCION_RAZON :=
                                    t_mf (l_index).ACCION_RAZON;
                                    t_mf (X).LUGAR_TJO :=
                                    t_mf (l_index).LUGAR_TJO;
                                    t_mf (X).CONTRATO :=
                                            t_mf (l_index).CONTRATO;
                                    t_mf (X).COD_EMP := 
                                            t_mf (l_index).COD_EMP;
                                    t_mf (X).TIPOREM_MF :=
                                    t_mf (l_index).TIPOREM_MF;
                                    t_mf (X).SJH_MF := 
                                            t_mf (l_index).SJH_MF;
                                    -- EGV 15SEP2015 Inicio
                                    --t_mf (X).SINDICATO :=
                                    --t_mf (l_index).SINDICATO;
                                    IF l_REGION = 'URY' THEN
                                       t_mf (X).SINDICATO :=
                                       t_mf (l_index).SINDICATO;
                                    END IF;
                                    -- EGV 15SEP2015 Fin
                                    t_mf (X).SECC_MF := 
                                            t_mf (l_index).SECC_MF;
                                    t_mf (X).CENCOS_MF :=
                                    t_mf (l_index).CENCOS_MF;
                                    t_mf (X).BANKING := 
                                            t_mf (l_index).BANKING;
                                            --QUITO IMG 21.08.2019
                                    --t_mf (X).GRADO := t_mf (l_index).GRADO;
                                    t_mf (X).STD_HOURS := 
                                            t_mf (l_index).STD_HOURS;
                                    -- EGV 15SEP2015
                                END IF;
                            END IF;
                        END LOOP;

                        t_mf.DELETE (l_index);
                        /* Borro el registro tomado como base para la
                        actualizacion de los registros con CB_BX_CHANGE=1
                        */
                        IF t_mf.COUNT > 0
                        THEN
                           Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                        END IF;
                    END IF;
                END IF;
            ELSE -- No Existen registros en la 
                    -- tabla MAEFUNC_STG para procesar
                -- Busco si existen registros en MAEFUNC_TBL
                BASEARG.PKG_INT_HRMS_VALIDA.
                BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
                l_cod_mf,
                J.EFFDT,
                J.EFFSEQ,
                'DE',
                t_mf,
                l_existe);


                IF l_existe > 0
                THEN /* Existen registros en la tabla 
                            MAEFUNC_TBL para procesar
                            */
                    IF l_existe = 1
                    THEN -- No hay registros modificados x Buxis
                        IF t_mf.COUNT > 0
                        THEN
                            /* Existen registros en la tabla
                            MAEFUNC_TBL para eliminar
                            */
                            FOR X IN t_mf.FIRST .. t_mf.LAST
                            LOOP
                               t_mf (X).TIPO_TRAN := 'D';
                            END LOOP;

                            Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                        END IF;
                    END IF;

                    IF l_existe = 2
                    THEN -- Hay registros modificados x Buxis
                        IF t_mf.COUNT > 0
                        THEN
                           /* Existen registros en la tabla
                            MAEFUNC_TBL para procesar
                            */
                            FOR X IN t_mf.FIRST .. t_mf.LAST
                            LOOP
                                IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
                                OR t_mf (X).CB_HRMS_EFFSEQ <> J.EFFSEQ
                                THEN
                                    l_index := X;
                                    t_mf (X).TIPO_TRAN := 'B';
                                END IF;
                            END LOOP;

                            FOR X IN t_mf.FIRST .. t_mf.LAST
                            LOOP
                                IF X <> l_index
                                THEN
                                    IF t_mf (X).CB_BX_CHANGE = 0
                                    THEN
                                       t_mf (X).TIPO_TRAN := 'D';
                                    ELSE
                                        t_mf (X).CB_DEPT := 
                                                t_mf (l_index).CB_DEPT;
                                        t_mf (X).COD_CATE :=
                                        t_mf (l_index).COD_CATE;
                                        t_mf (X).ACCION := 
                                                t_mf (l_index).ACCION;
                                        t_mf (X).ACCION_DT :=
                                        t_mf (l_index).ACCION_DT;
                                        t_mf (X).ACCION_RAZON :=
                                        t_mf (l_index).ACCION_RAZON;
                                        t_mf (X).LUGAR_TJO :=
                                        t_mf (l_index).LUGAR_TJO;
                                        t_mf (X).CONTRATO :=
                                        t_mf (l_index).CONTRATO;
                                        t_mf (X).COD_EMP := 
                                                t_mf (l_index).COD_EMP;
                                        t_mf (X).TIPOREM_MF :=
                                        t_mf (l_index).TIPOREM_MF;
                                        t_mf (X).SJH_MF := 
                                                t_mf (l_index).SJH_MF;
                                        -- EGV 15SEP2015 Inicio
                                        --t_mf (X).SINDICATO :=
                                        --t_mf (l_index).SINDICATO;
                                        IF l_REGION = 'URY' THEN
                                           t_mf (X).SINDICATO :=
                                           t_mf (l_index).SINDICATO;
                                        END IF;
                                        -- EGV 15SEP2015 Fin
                                        t_mf (X).SECC_MF :=
                                        t_mf (l_index).SECC_MF;
                                        t_mf (X).CENCOS_MF :=
                                        t_mf (l_index).CENCOS_MF;
                                        t_mf (X).BANKING :=
                                        t_mf (l_index).BANKING;
                                        --QUITO IMG 21.08.2019
                                           /* t_mf (X).GRADO := 
                                                t_mf (l_index).GRADO;*/
                                        t_mf (X).STD_HOURS :=
                                        t_mf (l_index).STD_HOURS;
                                        -- EGV 15SEP2015
                                    END IF;
                                END IF;
                            END LOOP;

                            t_mf.DELETE (l_index);
                            /* Borro el registro tomado como base para
                            la actualizacion de los 
                                registros con CB_BX_CHANGE=1
                            */
                            IF t_mf.COUNT > 0
                            THEN
                               Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                            END IF;
                        END IF;
                    END IF;
               ELSE -- Existe = 0 en MAEFUNC_TBL

                    t_mf.EXTEND (1);

                    t_mf (1).NIVEL := g_nivel;

                    t_mf (1).COD_INTFC := g_cod_int;
                    t_mf (1).COD_MF := l_cod_mf;
                    t_mf (1).FECHA_EFECTIVA := J.EFFDT;
                    t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
                    t_mf (1).SEC_EFECTIVA := J.EFFSEQ;
                    t_mf (1).CB_HRMS_EFFSEQ := J.EFFSEQ;
                    t_mf (1).CB_EMPL_RCD := J.EMPL_RCD;
                    t_mf (1).CB_GEID := J.EMPLID;
                    t_mf (1).CB_DEPT := J.DEPTID;
                    t_mf (1).COD_CATE := J.JOBCODE;
                    -- EGV 22SEP2015 Inicio
                    --t_mf (1).ACCION := J.ACTION;
                    t_mf (1).ACCION := CalculaNuevaAccion(
                       J.ACTION, J.ACTION_REASON, t_mf (1).ACCION
                       , J.EMPLID, J.EMPL_RCD, J.EFFDT, J.EFFSEQ);
                    -- EGV 22SEP2015 Fin
                    t_mf (1).ACCION_DT := J.ACTION_DT;
                    t_mf (1).ACCION_RAZON := J.ACTION_REASON;
                    t_mf (1).LUGAR_TJO := J.LOCATION;
                    -- EGV 19Jun2017 Inicio
                    --t_mf (1).CONTRATO := J.REG_TEMP;
                    t_mf (1).CONTRATO := CASE J.EMPL_CLASS
                    WHEN 'M' THEN 'P' ELSE J.REG_TEMP END;
                    -- EGV 19Jun2017 Fin
                    t_mf (1).COD_EMP := l_COD_EMP;
                    t_mf (1).TIPOREM_MF := J.COMP_FREQUENCY;
                    t_mf (1).SJH_MF := J.COMPRATE;
                    -- EGV 15SEP2015 Inicio
                    --t_mf (1).SINDICATO := l_SINDICATO;
                    IF l_REGION = 'URY' THEN
                       t_mf (1).SINDICATO := J.UNION_CD;
                    END IF;
                    -- EGV 15SEP2015 Fin
                    t_mf (1).SECC_MF := J.ESTABID;
                    t_mf (1).CENCOS_MF := J.CGG_EXPENSE;
                    -- EGV 05OCT2015 Inicio
                    --t_mf (1).BANKING := J.CGH_BUSINESS;
                    -- EGV 05OCT2015 Fin
                    -- EGV 23Ago2017 Inicio
                    --t_mf (1).GRADO := J.CG_MAPPEDGRADE;
                   --QUITO IMG 21.08.2019
                        /*t_mf (1).GRADO := CASE J.EMPL_CLASS
                    WHEN 'M' THEN '4' ELSE J.CG_MAPPEDGRADE END;*/
                    -- EGV 23Ago2017 Fin
                    --t_mf (1).CUOTA_CLUB := TraeCuotaClub(t_mf (1).GRADO);
                    -- EGV 22SEP2015
                    --t_mf (1).CAT_CONT := TraeCatCont(t_mf (1).GRADO);
                    -- EGV 05OCT2015
                    t_mf (1).BANKING := TraeBanking(
                       t_mf (1).CENCOS_MF
                       , t_mf (1).COD_EMP);
                    -- EGV 05OCT2015
                    -- EGV 24NOV2015 Inicio
                    --t_mf (1).STD_HOURS := J.STD_HOURS;-- EGV 15SEP2015
                    -- EGV 12FEB2016 Inicio
                    --IF J.FULL_PART_TIME = 'F' THEN
                    IF J.FULL_PART_TIME = 'F' and l_REGION = 'ARG' THEN
                    -- EGV 12FEB2016 Fin
                       t_mf (1).STD_HOURS := 162.5;
                    ELSE
                       -- EGV 26JUL2016 Inicio
                       --t_mf (1).STD_HOURS := J.STD_HOURS;
                       CASE TRUNC(J.STD_HOURS)
                           WHEN 86 THEN
                               t_mf (1).STD_HOURS := 86.6;
                           WHEN 108 THEN
                               t_mf (1).STD_HOURS := 108.33;
                           ELSE
                               t_mf (1).STD_HOURS := J.STD_HOURS;
                       END CASE;
                       -- EGV 26JUL2016 Fin
                    END IF;
                    -- EGV 24NOV2015 Fin
                    t_mf (1).CB_BX_CHANGE := 0;
                    t_mf (1).TIPO_TRAN := 'D';
                    t_mf (1).ESTADO_STG := 'E';

                    l_errores := l_errores + 1;

                    g_err_descr :=
                    'Error procesando COD_MF : '
                    || t_mf (1).COD_MF
                    || ' FECHA_EFF : '
                    || t_mf (1).FECHA_EFECTIVA
                    || ' SEC_EFF : '
                    || t_mf (1).SEC_EFECTIVA
                    || ' ACCION : '
                    || t_mf (1).TIPO_TRAN
                    || ' ERROR : No existe el registro a borrar';
                    BASEARG.PKG_BXSCITI_GENERAL.
                    GRABAR_LOG_DET (g_cod_int,
                    g_proc_int,
                    'E',
                    g_err_descr,
                    g_res_log);

                    -- EGV 07SEP2015 Inicio
                    --Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                    -- EGV 07SEP2015 Fin
               END IF;
            END IF;
        ELSE --NO Es una Eliminacion
            -- EGV 01Jul2015 - Inicio Pasa al principio del procedure
            --BEGIN
            --SELECT E.COD_EMP, TRIM (E.REGION)
            --INTO l_COD_EMP, l_REGION
            --FROM QSEMPRESA E
            --WHERE TRIM (E.COMPANY) = J.COMPANY;
            --EXCEPTION
            --   WHEN OTHERS
            --   THEN
            --    l_errores := l_errores + 1;
            --    l_COD_EMP := NULL;
            --    l_REGION := ' ';
            --   g_err_descr :=
            --    ' No existe conversion para la empresa :'
            --    || J.COMPANY;
            --    BASEARG.PKG_BXSCITI_GENERAL.
            --    GRABAR_LOG_DET (g_cod_int,
            --    g_proc_int,
            --    'E',
            --    g_err_descr,
            --    g_res_log);
            --END;
            -- EGV 01Jul2015 - Fin

            IF l_REGION <> 'URY'
            THEN
               J.UNION_CD := ' ';
            END IF;

            IF J.ACTION_REASON = 'CI'
            THEN
               J.ACTION_REASON := ' ';
            END IF;

            IF SUBSTR (J.JOBCODE, 1, 2) <> 'LF'
            THEN
               J.JOBCODE := SUBSTR (J.JOBCODE, 3, 4);
            END IF;

            -- Busco si existen registros en MAEFUNC_STG
            BASEARG.PKG_INT_HRMS_VALIDA.
            BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
                l_cod_mf,
                J.EFFDT,
                J.EFFSEQ,
                -- EGV 07SEP2015 Inicio
                --'FSE+',
                'FSE',
                -- EGV 07SEP2015 Fin
                J.EMPLID,   -- EGV 01Jul2015
                t_mf,
                l_existe);

            IF t_mf.COUNT = 0
            THEN --NO existen registros en la tabla MAEFUNC_STG
                -- Busco si existen registros en MAEFUNC_TBL
                BASEARG.PKG_INT_HRMS_VALIDA.
                BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
                  l_cod_mf,
                  J.EFFDT,
                  J.EFFSEQ,
                  -- EGV 07SEP2015 Inicio
                  --'FSE+',
                  'FSE',
                  -- EGV 07SEP2015 Fin
                  t_mf,
                  l_existe);

                IF t_mf.COUNT = 0
                THEN --NO existen registros en la tabla MAEFUNC_TBL
                    t_mf.EXTEND (1);

                    t_mf (1).NIVEL := g_nivel;
                    t_mf (1).COD_INTFC := g_cod_int;
                    t_mf (1).COD_MF := l_cod_mf;
                    t_mf (1).FECHA_EFECTIVA := J.EFFDT;
                    t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
                    t_mf (1).SEC_EFECTIVA := J.EFFSEQ;
                    t_mf (1).CB_HRMS_EFFSEQ := J.EFFSEQ;
                    t_mf (1).CB_EMPL_RCD := J.EMPL_RCD;
                    t_mf (1).CB_GEID := J.EMPLID;
                    t_mf (1).CB_DEPT := J.DEPTID;
                    t_mf (1).COD_CATE := J.JOBCODE;
                    -- EGV 22SEP2015 Inicio
                    --t_mf (1).ACCION := J.ACTION;
                    t_mf (1).ACCION := CalculaNuevaAccion(
                       J.ACTION, J.ACTION_REASON, t_mf (1).ACCION
                       , J.EMPLID, J.EMPL_RCD, J.EFFDT, J.EFFSEQ);
                    -- EGV 22SEP2015 Fin
                    t_mf (1).ACCION_DT := J.ACTION_DT;
                    t_mf (1).ACCION_RAZON := J.ACTION_REASON;
                    t_mf (1).LUGAR_TJO := J.LOCATION;
                    -- EGV 19Jun2017 Inicio
                    --t_mf (1).CONTRATO := J.REG_TEMP;
                    t_mf (1).CONTRATO := CASE J.EMPL_CLASS
                           WHEN 'M' THEN 'P'
                           ELSE J.REG_TEMP END;
                    -- EGV 19Jun2017 Fin
                    t_mf (1).COD_EMP := l_COD_EMP;
                    t_mf (1).TIPOREM_MF := J.COMP_FREQUENCY;
                    t_mf (1).SJH_MF := J.COMPRATE;
                    -- EGV 15SEP2015 Inicio
                    --t_mf (1).SINDICATO := l_SINDICATO;
                    IF l_REGION = 'URY' THEN
                       t_mf (1).SINDICATO := J.UNION_CD;
                    END IF;
                    -- EGV 15SEP2015 Fin
                    t_mf (1).SECC_MF := J.ESTABID;
                    t_mf (1).CENCOS_MF := J.CGG_EXPENSE;
                    -- EGV 05OCT2015 Inicio
                    --t_mf (1).BANKING := J.CGH_BUSINESS;
                    -- EGV 05OCT2015 Fin
                    -- EGV 23Ago2017 Inicio
                    --t_mf (1).GRADO := J.CG_MAPPEDGRADE;
                    --QUITO IMG 21.08.2019
                        /*t_mf (1).GRADO := CASE J.EMPL_CLASS
                           WHEN 'M' THEN '4'
                           ELSE J.CG_MAPPEDGRADE END;*/
                    -- EGV 23Ago2017 Fin
                    --t_mf (1).CUOTA_CLUB := TraeCuotaClub(t_mf (1).GRADO);
                   -- EGV 22SEP2015
                    --t_mf (1).CAT_CONT := TraeCatCont(t_mf (1).GRADO);
                   -- EGV 05OCT2015
                    t_mf (1).BANKING := TraeBanking(t_mf (1).CENCOS_MF
                    , t_mf (1).COD_EMP);
                    -- EGV 05OCT2015
                    -- EGV 24NOV2015 Inicio
                    --t_mf (1).STD_HOURS := J.STD_HOURS;
                    -- EGV 15SEP2015
                    -- EGV 12FEB2016 Inicio
                    --IF J.FULL_PART_TIME = 'F' THEN
                    IF J.FULL_PART_TIME = 'F' and l_REGION = 'ARG' THEN
                    -- EGV 12FEB2016 Fin
                       t_mf (1).STD_HOURS := 162.5;
                    ELSE
                       -- EGV 26JUL2016 Inicio
                       --t_mf (1).STD_HOURS := J.STD_HOURS;
                       CASE TRUNC(J.STD_HOURS)
                           WHEN 86 THEN
                               t_mf (1).STD_HOURS := 86.6;
                           WHEN 108 THEN
                               t_mf (1).STD_HOURS := 108.33;
                           ELSE
                               t_mf (1).STD_HOURS := J.STD_HOURS;
                       END CASE;
                       -- EGV 26JUL2016 Fin
                    END IF;
                    -- EGV 24NOV2015 Fin

                    -- EGV 23Ago2017 Inicio
                    /* 
                    2019-09-02-1515*/ 
                    
                    t_mf (1).MODCON := CASE J.EMPL_CLASS
                               WHEN 'M' THEN '27' ELSE '8' END;
                    
                    /* 2019-09-02-1515
                    */                               
                    t_mf (1).OSOCIAL_CITI := '01';
                    -- EGV 23Ago2017 Fin

                    

                    t_mf (1).CB_BX_CHANGE := 0;
                    t_mf (1).TIPO_TRAN := 'I';
                    t_mf (1).ESTADO_STG := 'P';

                    Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                    ELSE --Existen registros en la tabla MAEFUNC_TBL
                    FOR X IN t_mf.FIRST .. t_mf.LAST
                    LOOP
                        BEGIN

                        -- EGV 07SEP2015 Inicio
                        /*
                        IF X = t_mf.FIRST
                        THEN
                            IF l_existe = 0
                            THEN
                                IF (t_mf (X).CB_HRMS_EFFDT < J.EFFDT)
                                OR (t_mf (X).CB_HRMS_EFFDT = J.EFFDT
                                AND t_mf (X).CB_HRMS_EFFSEQ <
                                J.EFFSEQ)
                                THEN
                                    t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                                    t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                                    t_mf (X).SEC_EFECTIVA := J.EFFSEQ;
                                    t_mf (X).CB_HRMS_EFFSEQ := J.EFFSEQ;
                                    t_mf (X).CB_EMPL_RCD := J.EMPL_RCD;
                                    t_mf (X).CB_GEID := J.EMPLID;
                                    t_mf (X).CB_BX_CHANGE := 0;
                                    t_mf (X).TIPO_TRAN := 'I';
                                    t_mf (X).ESTADO_STG := 'P';
                                ELSE
                                    g_err_descr :=
                                    'Existe una inconsistencia con
                                    el registro a procesar 1'
                                    || ' COD_MF : '
                                    || t_mf (X).COD_MF
                                    || ' FECHA_EFF : '
                                    || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
                                    || ' SEC_EFF : '
                                    || J.EFFSEQ    --|| t_mf (X).SEC_EFECTIVA
                                    || ' ACCION : '
                                    || t_mf (X).TIPO_TRAN;

                                    l_errores := l_errores + 1;

                                    RAISE LOG_EXC;
                                END IF;
                            END IF;
                        END IF;
                        */
                        IF l_existe = 0
                        THEN
                           t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                           t_mf (X).CB_HRMS_EFFSEQ := J.EFFSEQ;
                           t_mf (X).CB_EMPL_RCD := J.EMPL_RCD;
                           t_mf (X).CB_GEID := J.EMPLID;

                           -- if t_mf(X).FECHA_EFECTIVA <> J.EFFDT
                           -- or t_mf (X).SEC_EFECTIVA <> J.EFFSEQ then
                           if t_mf(X).FECHA_EFECTIVA < J.EFFDT
                               or (t_mf(X).FECHA_EFECTIVA = J.EFFDT
                               and t_mf (X).SEC_EFECTIVA < J.EFFSEQ)then
                                t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                                t_mf (X).SEC_EFECTIVA := J.EFFSEQ;
                                --t_mf (X).CB_EMPL_RCD := J.EMPL_RCD;
                                t_mf (X).CB_BX_CHANGE := 0;
                                t_mf (X).TIPO_TRAN := 'I';
                                t_mf (X).ESTADO_STG := 'P';
                           end if;
                        END IF;
                        -- EGV 07SEP2015 Fin

                        t_mf (X).CB_DEPT := J.DEPTID;
                        t_mf (X).COD_CATE := J.JOBCODE;
                        -- EGV 22SEP2015 Inicio
                        --t_mf (X).ACCION := J.ACTION;
                        t_mf (X).ACCION := CalculaNuevaAccion(J.ACTION
                        , J.ACTION_REASON, t_mf (X).ACCION, J.EMPLID
                        , J.EMPL_RCD, J.EFFDT, J.EFFSEQ);
                        -- EGV 22SEP2015 Fin
                        -- EGV 26JUL2016 Inicio
                        --t_mf (X).ACCION_DT := J.ACTION_DT;
                        if nvl(t_mf (X).TIPO_TRAN,'U') = 'I' then
                           t_mf (X).ACCION_DT := J.ACTION_DT;
                        end if;
                        -- EGV 26JUL2016 Fin
                        t_mf (X).ACCION_RAZON := J.ACTION_REASON;
                        t_mf (X).LUGAR_TJO := J.LOCATION;
                        -- EGV 19Jun2017 Inicio
                        --t_mf (X).CONTRATO := J.REG_TEMP;
                        t_mf (X).CONTRATO := CASE J.EMPL_CLASS
                           WHEN 'M' THEN 'P' ELSE J.REG_TEMP END;
                        -- EGV 19Jun2017 Fin
                        t_mf (X).COD_EMP := l_COD_EMP;
                        t_mf (X).TIPOREM_MF := J.COMP_FREQUENCY;
                        t_mf (X).SJH_MF := J.COMPRATE;
                        -- EGV 15SEP2015 Inicio
                        --t_mf (X).SINDICATO := l_SINDICATO;
                        IF l_REGION = 'URY' THEN
                           t_mf (X).SINDICATO := J.UNION_CD;
                        END IF;
                        -- EGV 15SEP2015 Fin
                        t_mf (X).SECC_MF := J.ESTABID;
                        t_mf (X).CENCOS_MF := J.CGG_EXPENSE;
                        -- EGV 05OCT2015 Inicio
                        --t_mf (X).BANKING := J.CGH_BUSINESS;
                        -- EGV 05OCT2015 Fin
                        -- EGV 23Ago2017 Inicio
                        --t_mf (X).GRADO := J.CG_MAPPEDGRADE;
                        --QUITO IMG 21.08.2019
                           /* t_mf (X).GRADO := CASE J.EMPL_CLASS
                           WHEN 'M' THEN '4' ELSE J.CG_MAPPEDGRADE END;*/
                        -- EGV 23Ago2017 Fin
                       -- t_mf (X).CUOTA_CLUB := TraeCuotaClub(t_mf (X).GRADO);
                        -- EGV 22SEP2015
                        --t_mf (X).CAT_CONT := TraeCatCont(t_mf (X).GRADO);
                        -- EGV 05OCT2015
                        t_mf (X).BANKING := TraeBanking(t_mf (X).CENCOS_MF
                        , t_mf(X).COD_EMP);
                        -- EGV 05OCT2015
                        -- EGV 24NOV2015 Inicio
                        --t_mf (X).STD_HOURS := J.STD_HOURS;
                        -- EGV 12FEB2016 Inicio
                        --IF J.FULL_PART_TIME = 'F' THEN
                        IF J.FULL_PART_TIME = 'F' and l_REGION = 'ARG' THEN
                        -- EGV 12FEB2016 Fin
                           t_mf (X).STD_HOURS := 162.5;
                        ELSE
                           -- EGV 26JUL2016 Inicio
                           --t_mf (X).STD_HOURS := J.STD_HOURS;
                           CASE TRUNC(J.STD_HOURS)
                               WHEN 86 THEN
                                   t_mf (X).STD_HOURS := 86.6;
                               WHEN 108 THEN
                                   t_mf (X).STD_HOURS := 108.33;
                               ELSE
                                   t_mf (X).STD_HOURS := J.STD_HOURS;
                           END CASE;
                           -- EGV 26JUL2016 Fin
                        END IF;
                        -- EGV 24NOV2015 Fin
                       -- EGV 15SEP2015
                        EXCEPTION
                            WHEN LOG_EXC
                            THEN
                                BASEARG.PKG_BXSCITI_GENERAL.
                                GRABAR_LOG_DET (g_cod_int,
                                g_proc_int,
                                'E',
                                g_err_descr,
                                g_res_log);
                        END;
                    END LOOP;

                    -- EGV 07SEP2015 Inicio
                    --Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                    Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                    -- EGV 07SEP2015 Fin
                END IF;
            ELSE --Existen registros en la tabla MAEFUNC_STG
                FOR X IN t_mf.FIRST .. t_mf.LAST
                LOOP
                    BEGIN

                    IF X = t_mf.FIRST
                    THEN
                        IF l_existe = 0
                        THEN
                            IF (t_mf (X).CB_HRMS_EFFDT < J.EFFDT)
                            OR (t_mf (X).CB_HRMS_EFFDT = J.EFFDT
                            AND t_mf (X).CB_HRMS_EFFSEQ < J.EFFSEQ)
                            THEN
                                t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                                t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                                t_mf (X).SEC_EFECTIVA := J.EFFSEQ;
                                t_mf (X).CB_HRMS_EFFSEQ := J.EFFSEQ;
                                t_mf (X).CB_EMPL_RCD := J.EMPL_RCD;
                                t_mf (X).CB_GEID := J.EMPLID;
                                t_mf (X).CB_BX_CHANGE := 0;
                                t_mf (X).TIPO_TRAN := 'I';
                                t_mf (X).ESTADO_STG := 'P';
                            ELSE
                                g_err_descr :=
                                'Existe una inconsistencia
                                con el registro a procesar 2'
                                || ' COD_MF : '
                                || t_mf (X).COD_MF
                                || ' FECHA_EFF : '
                                || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
                                || ' SEC_EFF : '
                                || J.EFFSEQ    --|| t_mf (X).SEC_EFECTIVA
                                || ' ACCION : '
                                || t_mf (X).TIPO_TRAN;

                                l_errores := l_errores + 1;
                                -- EGV 07SEP2015

                                RAISE LOG_EXC;
                            END IF;
                        END IF;
                    END IF;

                    t_mf (X).CB_DEPT := J.DEPTID;
                    t_mf (X).COD_CATE := J.JOBCODE;
                    -- EGV 22SEP2015 Inicio
                    --t_mf (X).ACCION := J.ACTION;
                    t_mf (X).ACCION := CalculaNuevaAccion(J.ACTION
                           , J.ACTION_REASON, t_mf (X).ACCION
                           , J.EMPLID, J.EMPL_RCD, J.EFFDT, J.EFFSEQ);
                    -- EGV 22SEP2015 Fin
                    -- EGV 26JUL2016 Inicio
                    --t_mf (X).ACCION_DT := J.ACTION_DT;
                    if nvl(t_mf (X).TIPO_TRAN,'U') = 'I' then
                       t_mf (X).ACCION_DT := J.ACTION_DT;
                    end if;
                    -- EGV 26JUL2016 Fin
                    t_mf (X).ACCION_RAZON := J.ACTION_REASON;
                    t_mf (X).LUGAR_TJO := J.LOCATION;
                    -- EGV 19Jun2017 Inicio
                    --t_mf (X).CONTRATO := J.REG_TEMP;
                    t_mf (X).CONTRATO := CASE J.EMPL_CLASS
                       WHEN 'M' THEN 'P' ELSE J.REG_TEMP END;
                    -- EGV 19Jun2017 Fin
                    t_mf (X).COD_EMP := l_COD_EMP;
                    t_mf (X).TIPOREM_MF := J.COMP_FREQUENCY;
                    t_mf (X).SJH_MF := J.COMPRATE;
                    -- EGV 15SEP2015 Inicio
                    --t_mf (X).SINDICATO := l_SINDICATO;
                    IF l_REGION = 'URY' THEN
                       t_mf (X).SINDICATO := J.UNION_CD;
                    END IF;
                    -- EGV 15SEP2015 Fin
                    t_mf (X).SECC_MF := J.ESTABID;
                    t_mf (X).CENCOS_MF := J.CGG_EXPENSE;
                    -- EGV 05OCT2015 Inicio
                    --t_mf (X).BANKING := J.CGH_BUSINESS;
                    -- EGV 05OCT2015 Fin
                    -- EGV 23Ago2017 Inicio
                    --t_mf (X).GRADO := J.CG_MAPPEDGRADE;
                    --QUITO IMG 21.08.2019
                        /*t_mf (X).GRADO := CASE J.EMPL_CLASS
                       WHEN 'M' THEN '4' ELSE J.CG_MAPPEDGRADE END;*/
                    -- EGV 23Ago2017 Fin
                    --t_mf (X).CUOTA_CLUB := TraeCuotaClub(t_mf (X).GRADO);
                    -- EGV 22SEP2015
                    --t_mf (X).CAT_CONT := TraeCatCont(t_mf (X).GRADO);
                    -- EGV 05OCT2015
                    t_mf (X).BANKING := TraeBanking(t_mf (X).CENCOS_MF
                       , t_mf (X).COD_EMP);
                    -- EGV 05OCT2015
                    -- EGV 24NOV2015 Inicio
                    --t_mf (X).STD_HOURS := J.STD_HOURS;    -- EGV 15SEP2015
                    -- EGV 12FEB2016 Inicio
                    --IF J.FULL_PART_TIME = 'F' THEN
                    IF J.FULL_PART_TIME = 'F' and l_REGION = 'ARG' THEN
                    -- EGV 12FEB2016 Fin
                       t_mf (X).STD_HOURS := 162.5;
                    ELSE
                       -- EGV 26JUL2016 Inicio
                       --t_mf (X).STD_HOURS := J.STD_HOURS;
                       CASE TRUNC(J.STD_HOURS)
                           WHEN 86 THEN
                               t_mf (X).STD_HOURS := 86.6;
                           WHEN 108 THEN
                               t_mf (X).STD_HOURS := 108.33;
                           ELSE
                               t_mf (X).STD_HOURS := J.STD_HOURS;
                       END CASE;
                       -- EGV 26JUL2016 Fin
                    END IF;
                    -- EGV 24NOV2015 Fin

                    IF t_mf (X).ESTADO_STG = 'E'
                    AND t_mf (X).TIPO_TRAN = 'D'
                    THEN
                        t_mf (X).TIPO_TRAN := 'I';
                        t_mf (X).ESTADO_STG := 'P';
                    END IF;

                    IF t_mf (X).ESTADO_STG = 'P'
                    AND t_mf (X).TIPO_TRAN = 'D'
                    THEN
                        t_mf (X).TIPO_TRAN := 'U';
                    END IF;
                    EXCEPTION
                        WHEN LOG_EXC
                        THEN
                            BASEARG.PKG_BXSCITI_GENERAL.
                            GRABAR_LOG_DET (g_cod_int,
                            g_proc_int,
                            'E',
                            g_err_descr,
                            g_res_log);
                    END;
                END LOOP;

                Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
            END IF;
        END IF;
        EXCEPTION
           WHEN NEXT_VAL
           THEN
               NULL;
           WHEN LOG_EXC
           THEN
                BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
                g_proc_int,
                'E',
                g_err_descr,
                g_res_log);
           WHEN OTHERS
           THEN
                g_err_descr := SQLERRM;
                BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
                g_proc_int,
                'E',
                g_err_descr,
                g_res_log);
        END;
    END LOOP;


    SELECT COUNT (*)
    INTO l_errores
    FROM CB_MAEFUNC_STG S
    WHERE S.COD_INTFC = g_cod_int AND S.ESTADO_STG = 'E';

    IF l_errores > 0
    THEN
       l_res := 'ERRORES : ' || l_errores;
    ELSE
       l_res := 'OK';
    END IF;
    EXCEPTION
       WHEN OTHERS
       THEN
           l_res := 'ERROR : ' || SQLERRM;
END;

 /** 0120**/
 PROCEDURE Valida_CB_NAMES_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_effseq INTEGER := 0;
 l_empl_rcd INTEGER := 0;
 l_effdt DATE := TO_DATE ('01/01/1900', 'DD/MM/YYYY');
 -- EGV 25Ago2015 Inicio
 l_iLast INTEGER := 0;
 l_last_name VARCHAR2(20) := '';
 l_first_name VARCHAR2(20) := '';
 l_middle_name VARCHAR2(20) := '';
 l_second_last_name VARCHAR2(20) := '';
 -- EGV 25Ago2015 Fin

 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_NAMES_INT
 WHERE COD_INTFC = g_cod_int
 AND NAME_TYPE = 'PRI'      -- EGV 16Dic2016
 ORDER BY EMPLID, EFFDT;

 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_NAMES_INT';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR N IN C_DATOS
 LOOP
     BEGIN
     l_existe := 0;

     t_mf := g_mf_list ();

     g_err_descr := '';

     Obtener_Tipo_Trans (N.AUDIT_ACTN, l_t_trans);

     -- g_err_descr :=
     -- 'COD_MF : '
     -- || l_cod_mf
     -- || ' AUDIT_ACTN : '
     -- || l_t_trans;
     -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     -- g_proc_int,
     -- 'I',
     -- g_err_descr,
     -- g_res_log);

     IF l_t_trans <> 'D'
     THEN --No Es una Eliminacion
         -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (N.COD_INTFC,
         N.EMPLID,
         l_effdt,
         l_effseq,
         'TO+',
         N.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF l_existe = 1
         THEN --Existen registros en la tabla MAEFUNC_STG para procesar
            -- EGV 25Ago2015 Inicio
            l_iLast := t_mf.LAST;

            l_last_name := substr(rtrim(N.LAST_NAME),1,20);
            l_first_name := substr(rtrim(N.FIRST_NAME),1,20);
            l_middle_name := substr(rtrim(N.MIDDLE_NAME),1,20);
            l_second_last_name := substr(rtrim(N.SECOND_LAST_NAME),1,20);

            IF (nvl(rtrim(t_mf(l_iLast).PRI_APE_MF),' ')
            <> nvl(rtrim(l_last_name),' ')
            OR nvl(rtrim(t_mf(l_iLast).PRI_NOM_MF),' ')
            <> nvl(rtrim(l_first_name),' ')
            OR nvl(rtrim(t_mf(l_iLast).SEG_NOM_MF),' ')
            <> nvl(rtrim(l_middle_name),' ')
            OR nvl(rtrim(t_mf(l_iLast).SEG_APE_MF),' ')
            <> nvl(rtrim(l_second_last_name),' '))
      THEN
            -- EGV 25Ago2015 Fin

             FOR X IN t_mf.FIRST .. t_mf.LAST
             LOOP
             -- EGV 25Ago2015 Inicio
             --t_mf (X).PRI_APE_MF := N.LAST_NAME;
             --t_mf (X).PRI_NOM_MF := N.FIRST_NAME;
             --t_mf (X).SEG_NOM_MF := N.MIDDLE_NAME;
             --t_mf (X).SEG_APE_MF := N.SECOND_LAST_NAME;
             t_mf (X).PRI_APE_MF := l_last_name;
             t_mf (X).PRI_NOM_MF := l_first_name;
             t_mf (X).SEG_NOM_MF := l_middle_name;
             t_mf (X).SEG_APE_MF := l_second_last_name;
             -- EGV 25Ago2015 Fin
             END LOOP;

             Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
            END IF;     -- EGV 25Ago2015
         ELSE
             g_err_descr :=
             'No existen Registros para el EMPLID : ' || N.EMPLID;
             l_errores := l_errores + 1;
             RAISE LOG_EXC;
             NULL;
         END IF;
     END IF;
     EXCEPTION
         WHEN NEXT_VAL
         THEN
             NULL;
             WHEN LOG_EXC
             THEN
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
         WHEN OTHERS
         THEN
             g_err_descr := SQLERRM;
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
     END;
 END LOOP;

 IF l_errores > 0
 THEN
    l_res := 'ERRORES : ' || l_errores;
 ELSE
    l_res := 'OK';
 END IF;
 EXCEPTION
     WHEN OTHERS
     THEN
        l_res := 'ERROR : ' || SQLERRM;
 END;

 /** 0140**/
 PROCEDURE Valida_CB_PERS_NID_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 --l_cod_mf INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_effseq INTEGER := 0;
 l_empl_rcd INTEGER := 0;
 l_effdt DATE := TO_DATE ('01/01/1900', 'DD/MM/YYYY');
 l_iLast INTEGER := 0;      -- EGV 25Ago2015


 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_PERS_NID_INT
 WHERE COD_INTFC = g_cod_int
 ORDER BY EMPLID;

 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_PERS_NID_INT';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR PN IN C_DATOS
 LOOP
     BEGIN
     l_existe := 0;

     t_mf := g_mf_list ();

     g_err_descr := '';

     Obtener_Tipo_Trans (PN.AUDIT_ACTN, l_t_trans);

     -- g_err_descr :=
     -- 'COD_MF : '
     -- || l_cod_mf
     -- || ' AUDIT_ACTN : '
     -- || l_t_trans;
     -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     -- g_proc_int,
     -- 'I',
     -- g_err_descr,
     -- g_res_log);

     IF l_t_trans <> 'D'
     THEN --No Es una Eliminacion
         -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (PN.COD_INTFC,
         PN.EMPLID,
         l_effdt,
         l_effseq,
         'TO+',
         PN.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF l_existe = 1
         THEN --Existen registros en la tabla MAEFUNC_STG para procesar
            -- EGV 25Ago2015 Inicio
            l_iLast := t_mf.LAST;
            IF ((PN.PRIMARY_NID = 'Y'
                AND (nvl(rtrim(t_mf(l_iLast).TIPO_DOC),' ')
                <> nvl(rtrim(PN.NATIONAL_ID_TYPE),' ')
                OR nvl(rtrim(t_mf(l_iLast).CEDIDE_MF),' ')
                <> nvl(rtrim(PN.NATIONAL_ID),' ')))
            OR (PN.NATIONAL_ID_TYPE = 'CUIL'
                AND nvl(rtrim(t_mf(l_iLast).CUIL),' ')
                <> nvl(rtrim(PN.NATIONAL_ID),' ')
                ))
            then
            -- EGV 25Ago2015 Fin
             FOR X IN t_mf.FIRST .. t_mf.LAST
             LOOP
                 IF PN.PRIMARY_NID = 'Y'
                 THEN
                     t_mf (X).TIPO_DOC := PN.NATIONAL_ID_TYPE;
                     t_mf (X).CEDIDE_MF := PN.NATIONAL_ID;
                 END IF;

                 -- EGV 25Ago2015 Inicio
                 --IF t_mf (X).TIPO_DOC = 'CUIL'
                 IF PN.NATIONAL_ID_TYPE = 'CUIL'
                 -- EGV 25Ago2015 Fin
                 THEN
                     t_mf (X).CUIL := PN.NATIONAL_ID;
                 END IF;
             END LOOP;

             Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
            END IF;     -- EGV 25Ago2015
         ELSE
             g_err_descr :=
             'No existen Registros para el EMPLID : ' || PN.EMPLID;
             l_errores := l_errores + 1;
             RAISE LOG_EXC;
             NULL;
         END IF;
     END IF;
     EXCEPTION
         WHEN NEXT_VAL
         THEN
             NULL;
         WHEN LOG_EXC
         THEN
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
         WHEN OTHERS
         THEN
             g_err_descr := SQLERRM;
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
     END;
 END LOOP;

 IF l_errores > 0
 THEN
     l_res := 'ERRORES : ' || l_errores;
 ELSE
     l_res := 'OK';
 END IF;
 EXCEPTION
     WHEN OTHERS
     THEN
         l_res := 'ERROR : ' || SQLERRM;
 END;

 /** 0110**/
 PROCEDURE Valida_CB_PERS_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_effseq INTEGER := 0;
 l_empl_rcd INTEGER := 0;
 l_effdt DATE := TO_DATE ('01/01/1900', 'DD/MM/YYYY');
 l_iLast INTEGER := 0;  -- EGV 25Ago2015
 l_existe_soeid INTEGER;  -- EGV 12FEB2016


 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_PERS_INT
 WHERE COD_INTFC = g_cod_int
 -- EGV 12FEB2016 Inicio
 --ORDER BY EMPLID;
 ORDER BY NRO_LINEA;
 -- EGV 12FEB2016 Fin
 -- EGV 12FEB2016 Inicio
 CURSOR C_MAEFUNC2(l_geid varchar2)
 IS
 SELECT DISTINCT COD_MF
 FROM MAEFUNC2
 WHERE trim(CB_GEID) = l_geid
 --* 2019 04 17 *: para legajos que comienzan con ceros 
 UNION
 SELECT DISTINCT COD_MF
 FROM CB_MAEFUNC_STG
 -- WHERE trim(CB_GEID) = l_geid
 WHERE ltrim(CB_GEID, '0') =  ltrim(l_geid, '0')
 --* 2019 04 17 *: para legajos que comienzan con ceros 
 AND COD_INTFC = g_cod_int;
 -- EGV 12FEB2016 Fin


 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_PERS_INT';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR P IN C_DATOS
 LOOP
     BEGIN
     l_existe := 0;

     t_mf := g_mf_list ();

     g_err_descr := '';

     Obtener_Tipo_Trans (P.AUDIT_ACTN, l_t_trans);

     -- g_err_descr :=
     -- 'COD_MF : '
     -- || l_cod_mf
     -- || ' AUDIT_ACTN : '
     -- || l_t_trans;
     -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     -- g_proc_int,
     -- 'I',
     -- g_err_descr,
     -- g_res_log);

     IF l_t_trans <> 'D'
     THEN --No Es una Eliminacion

         -- EGV 12FEB2016 Inicio
         -- TABLA SOEID

         BEGIN

         FOR M in C_MAEFUNC2(P.EMPLID)
         LOOP

            l_existe_soeid := 0;

            BEGIN

            BEGIN
            SELECT 1 into l_existe_soeid
            FROM CB_SOEID_STG
            WHERE COD_INTFC = g_cod_int
            AND COD_MF = M.COD_MF;
            EXCEPTION
                WHEN NO_DATA_FOUND THEN
                    NULL;
                WHEN OTHERS THEN
                    RAISE;
            END;

            IF l_existe_soeid = 1 THEN

                UPDATE CB_SOEID_STG
                SET SOEID = P.SOEID
                WHERE COD_INTFC = g_cod_int
                    AND COD_MF = M.COD_MF;
            ELSE

                l_existe_soeid := 0;

                BEGIN
                SELECT 1 into l_existe_soeid
                FROM CB_SOEID
                WHERE COD_MF = M.COD_MF;
                EXCEPTION
                    WHEN NO_DATA_FOUND THEN
                        NULL;
                    WHEN OTHERS THEN
                        RAISE;
                END;

                IF l_existe_soeid = 1 THEN
                    INSERT INTO CB_SOEID_STG(COD_INTFC, COD_MF, SOEID
                        , ESTADO_STG, COD_ERROR, TIPO_TRAN)
                    VALUES (g_cod_int, M.COD_MF, P.SOEID, 'P',' ', 'U');
                ELSE
                    INSERT INTO CB_SOEID_STG(COD_INTFC, COD_MF, SOEID
                    , ESTADO_STG, COD_ERROR, TIPO_TRAN)
                    VALUES (g_cod_int, M.COD_MF, P.SOEID, 'P',' ', 'I');
                END IF;

            END IF;

            EXCEPTION
                WHEN OTHERS THEN
                    BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
                    g_proc_int,
                    'E',
                    'SOEID: ' || substr(SQLERRM,1,247),
                    g_res_log);
            END;
         END LOOP;
         EXCEPTION
            WHEN OTHERS THEN
                    BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
                    g_proc_int,
                    'E',
                    'SOEID: ' || substr(SQLERRM,1,247),
                    g_res_log);
         END;
         -- EGV 12FEB2016 Fin


     -- Busco si existen registros en MAEFUNC_STG

         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (P.COD_INTFC,
         P.EMPLID,
         l_effdt,
         l_effseq,
         'TO+',
         P.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF l_existe = 1
         THEN --Existen registros en la tabla MAEFUNC_STG para procesar

            -- EGV 25Ago2015 Inicio
            l_iLast := t_mf.LAST;
            IF (nvl(t_mf(l_iLast).FECNAC_MF,'01-jan-1800')
                <> nvl(P.BIRTHDATE,'01-jan-1800')
                OR nvl(rtrim(t_mf(l_iLast).NACIONALIDAD),' ')
                <> nvl(rtrim(P.BIRTHCOUNTRY),' '))
            THEN
            -- EGV 25Ago2015 Fin

             FOR X IN t_mf.FIRST .. t_mf.LAST
             LOOP
                 t_mf (X).FECNAC_MF := P.BIRTHDATE;
                 t_mf (X).NACIONALIDAD := P.BIRTHCOUNTRY;
             END LOOP;

             Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);

            END IF;     -- EGV 25Ago2015


         ELSE
             g_err_descr :=
             'No existen Registros para el EMPLID : ' || P.EMPLID;
             l_errores := l_errores + 1;
             RAISE LOG_EXC;
             NULL;
         END IF;


     END IF;
     EXCEPTION
         WHEN NEXT_VAL
         THEN
             NULL;
         WHEN LOG_EXC
         THEN
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
         WHEN OTHERS
         THEN
             g_err_descr := SQLERRM;
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
     END;
 END LOOP;

 IF l_errores > 0
 THEN
     l_res := 'ERRORES : ' || l_errores;
 ELSE
     l_res := 'OK';
 END IF;
 EXCEPTION
     WHEN OTHERS
     THEN
         l_res := 'ERROR : ' || SQLERRM;
 END;

 /** 0230**/
 PROCEDURE Valida_CB_EMPL_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 l_cod_mf INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_effseq INTEGER := 0;
 l_effdt DATE := TO_DATE ('01/01/1900', 'DD/MM/YYYY');
 l_region_busq VARCHAR2(3) := ' ';      -- EGV 01Jul2015
 l_iLast INTEGER := 0;      -- EGV 25Ago2015


 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_EMPL_INT
 WHERE COD_INTFC = g_cod_int
 ORDER BY EMPLID;

 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_EMPL_INT';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR E IN C_DATOS
 LOOP
     BEGIN
     l_existe := 0;

     t_mf := g_mf_list ();

     g_err_descr := '';

     -- EGV 07SEP2015 Inicio
     l_index := 0;
     -- EGV 07SEP2015 Fin

     -- EGV 01Jul2015 - Inicio
     IF E.EMPL_RCD > 0 THEN
        TraeRegionDeEmplid(E.EMPLID, E.EMPL_RCD, SYSDATE, 999, l_region_busq);
     END IF;
     -- EGV 01Jul2015 - Fin

     -- EGV 01Jul2015 - Inicio
     --Buscar_Emplid (E.EMPLID, E.EMPL_RCD, l_cod_mf);
     Buscar_Emplid (E.EMPLID, E.EMPL_RCD, l_region_busq, l_cod_mf);
     -- EGV 01Jul2015 - Fin


     -- EGV 27Ago2015 - Inicio
     /*IF l_cod_mf = 0
     THEN
        Armar_Cod_Mf (E.EMPLID, E.EMPL_RCD, l_cod_mf);
     ELSE
         IF l_cod_mf = -1
         THEN
             g_err_descr := 'Error buscando el empleado : ' || E.EMPLID;
             l_errores := l_errores + 1;
             RAISE LOG_EXC;
         END IF;
     END IF;*/
     IF l_cod_mf <= 0
     THEN
         g_err_descr := 'Error buscando el empleado : ' || E.EMPLID;
         l_errores := l_errores + 1;
         RAISE LOG_EXC;
     END IF;
     -- EGV 27Ago2015 - Fin

     Obtener_Tipo_Trans (E.AUDIT_ACTN, l_t_trans);

     -- g_err_descr :=
     -- 'COD_MF : '
     -- || l_cod_mf
     -- || ' AUDIT_ACTN : '
     -- || l_t_trans;
     -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     -- g_proc_int,
     -- 'I',
     -- g_err_descr,
     -- g_res_log);

     IF l_t_trans <> 'D'
     THEN --No Es una Eliminacion
         -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (E.COD_INTFC,
         l_cod_mf,
         l_effdt,
         l_effseq,
         'TO',
         E.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF l_existe = 1
         THEN --Existen registros en la tabla MAEFUNC_STG para procesar

            -- EGV 14OCT2015 Inicio
            /* Si la antiguedad reconocida es nula, asignarle el menor*/
            IF E.PROF_EXPERIENCE_DT IS NULL THEN
                E.PROF_EXPERIENCE_DT :=
                    LEAST(E.HIRE_DT,E.CMPNY_SENIORITY_DT,E.SERVICE_DT);
            END IF;
            -- EGV 14OCT2015 Fin


            -- EGV 25Ago2015 Inicio
            /* Se compara la ?ltima fila de MAEFUNC
                contra lo que vino por interfaz
            -- Para los campos de que se impactan de EMPLOYMENT
            -- Si hay diferencias se procesa, sino no
            -- Se toma la ultima fila de MAEFUNC solo por elegir una
            -- ya que estos campos deberaan ser identicos en todas las fechas
            -- El campo que varia de fecha a fecha es el de
            -- fecha de baja, por eso tambien se toma la ultima maefunc
            */
            l_iLast := t_mf.LAST;
            if (nvl(t_mf (l_iLast).FEC_DESV_MF,'01-jan-1800')
                <> nvl(E.TERMINATION_DT,'01-jan-1800')
                OR nvl(t_mf(l_iLast).FEC_HIRE,'01-jan-1800')
                <> nvl(E.HIRE_DT,'01-jan-1800')
                OR nvl(t_mf(l_iLast).FEC_REHIRE,'01-jan-1800')
                <> nvl(E.REHIRE_DT,'01-jan-1800')
                OR nvl(t_mf(l_iLast).FEC_ING_MF,'01-jan-1800')
                <> nvl(E.CMPNY_SENIORITY_DT,'01-jan-1800')
                  OR nvl(t_mf(l_iLast).FEC_ANTIG,'01-jan-1800')
                  <> nvl(E.SERVICE_DT,'01-jan-1800')
                  OR nvl(t_mf(l_iLast).FEC_INDEM_MF,'01-jan-1800')
                  <> nvl(E.PROF_EXPERIENCE_DT,'01-jan-1800')
                  OR nvl(rtrim(t_mf(l_iLast).BUSINESS_TITLE),' ')
                  <> nvl(rtrim(E.BUSINESS_TITLE),' ')
            )
            THEN
            -- EGV 25Ago2015 Fin
             FOR X IN t_mf.FIRST .. t_mf.LAST
             LOOP

                 -- EGV 22SEP2015 Inicio
                 /* Al final la fecha de baja la
                 afectaremos a todos los registros
                 */
                 /*
                 --IF E.TERMINATION_DT IS NOT NULL
                 --THEN
                 --    IF t_mf (X).ACCION IN ('TER', 'TDL', 'TWB', 'TWP')
                 --    THEN
                 --        IF l_index = 0
                 --        THEN
                 --            l_index := X;
                 --        ELSE
                 --            IF (E.TERMINATION_DT - t_mf
                            (X).FECHA_EFECTIVA) <
                            (E.TERMINATION_DT
                 --            - t_mf (l_index).FECHA_EFECTIVA)
                 --            THEN
                 --               l_index := X;
                 --            END IF;
                 --        END IF;
                 --    END IF;
                 --END IF;
                 */
                 -- EGV 22SEP2015 Fin

                 t_mf (X).FEC_HIRE := E.HIRE_DT;
                 t_mf (X).FEC_REHIRE := E.REHIRE_DT;
                 t_mf (X).FEC_ING_MF := E.CMPNY_SENIORITY_DT;
                 t_mf (X).FEC_ANTIG := E.SERVICE_DT;
                 t_mf (X).FEC_INDEM_MF := E.PROF_EXPERIENCE_DT;
                 t_mf (X).BUSINESS_TITLE := E.BUSINESS_TITLE;

                 -- EGV 22SEP2015 Inicio
                 /*Al final la fecha de baja la
                 afectaremos a todos los registros*/
                 --IF x = t_mf.LAST AND l_index <> 0
                 --THEN
                 --   t_mf (l_index).FEC_DESV_MF := E.TERMINATION_DT;
                 --   t_mf (l_index).FEC_EGR_MF := E.TERMINATION_DT;
                 -- EGV 25Ago2015 Inicio
                 --END IF;
                 t_mf (X).FEC_DESV_MF := E.TERMINATION_DT;
                 t_mf (X).FEC_EGR_MF := E.TERMINATION_DT;
                    -- EGV 25Ago2015 Inicio
                 -- EGV 22SEP2015 Inicio
                 /*Al final la fecha de baja la
                 afectaremos a todos los registros*/
             END LOOP;

             Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
            -- EGV 25Ago2015 Inicio
            END IF;
            -- EGV 25Ago2015 Fin
         ELSE
             g_err_descr :=
             'No existen Registros para el COD_MF : ' || l_cod_mf;
             l_errores := l_errores + 1;
             RAISE LOG_EXC;
             NULL;
         END IF;
     END IF;
     EXCEPTION
         WHEN NEXT_VAL
         THEN
             NULL;
         WHEN LOG_EXC
         THEN
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
         WHEN OTHERS
         THEN
             g_err_descr := SQLERRM;
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
     END;
 END LOOP;

 IF l_errores > 0
 THEN
     l_res := 'ERRORES : ' || l_errores;
 ELSE
     l_res := 'OK';
 END IF;
 EXCEPTION
     WHEN OTHERS
     THEN
        l_res := 'ERROR : ' || SQLERRM;
 END;

 /** 0300**/
 PROCEDURE Valida_CB_JOB_JR_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 l_cod_mf INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_Region VARCHAR (5) := ' ';
 l_region_busq VARCHAR2(3) := ' ';      -- EGV 01Jul2015

 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_JOB_JR_INT
 WHERE COD_INTFC = g_cod_int
 -- EGV 07SEP2015 Inicio
 --ORDER BY EMPLID,
 ORDER BY NRO_LINEA, EMPLID,
 -- EGV 07SEP2015 Fin
 EMPL_RCD,
 EFFDT,
 EFFSEQ;

 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_JOB_INT_JR';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR J IN C_DATOS
 LOOP
     BEGIN
     l_existe := 0;

     t_mf := g_mf_list ();

     g_err_descr := '';

      -- EGV 01Jul2015 - Inicio
     IF J.EMPL_RCD > 0 THEN
        TraeRegionDeEmplid(J.EMPLID, J.EMPL_RCD, J.EFFDT, J.EFFSEQ,
      l_region_busq);
     END IF;
     -- EGV 01Jul2015 - Fin

     -- EGV 01Jul2015 - Inicio
     --Buscar_Emplid (J.EMPLID, J.EMPL_RCD, l_cod_mf);
     Buscar_Emplid (J.EMPLID, J.EMPL_RCD, l_region_busq, l_cod_mf);
     -- EGV 01Jul2015 - Fin

     -- EGV 27Ago2015 - Inicio
     /*IF l_cod_mf = 0
     THEN
     Armar_Cod_Mf (J.EMPLID, J.EMPL_RCD, l_cod_mf);
     ELSE
     IF l_cod_mf = -1
     THEN
     g_err_descr := 'Error buscando el empleado : ' || J.EMPLID;
     l_errores := l_errores + 1;
     RAISE LOG_EXC;
     END IF;
     END IF;*/
     IF l_cod_mf <= 0
     THEN
         g_err_descr := 'Error buscando el empleado : ' || J.EMPLID;
         l_errores := l_errores + 1;
         RAISE LOG_EXC;
     END IF;
     -- EGV 27Ago2015 - Fin

     Obtener_Tipo_Trans (J.AUDIT_ACTN, l_t_trans);

     -- g_err_descr :=
     -- 'COD_MF : '
     -- || l_cod_mf
     -- || ' AUDIT_ACTN : '
     -- || l_t_trans
     -- || ' EFFDT : '
     -- || J.EFFDT
     -- || ' EFFSEQ; : '
     -- || J.EFFSEQ;
     -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     -- g_proc_int,
     -- 'I',
     -- g_err_descr,
     -- g_res_log);

     -- EGV 20Dic2016 Inicio
     /*No se procesan las Eliminaciones,
     ya que se procesan con la Tabla JOB */
     /*
     IF l_t_trans = 'D'
     THEN --Es una Eliminacion
         -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
         l_cod_mf,
         J.EFFDT,
         J.EFFSEQ,
         'DE',
         J.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF l_existe > 0
         THEN --Existen registros en la tabla MAEFUNC_STG para procesar
             IF l_existe = 1
             THEN -- No hay registros modificados x Buxis
                 IF t_mf.COUNT > 0
                 THEN
                --Existen registros en la tabla MAEFUNC_STG para eliminar
                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                        t_mf (X).TIPO_TRAN := 'D';
                     END LOOP;

                     Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                 END IF;
             END IF;

             IF l_existe = 2
             THEN -- Hay registros modificados x Buxis
                 IF t_mf.COUNT > 0
                 THEN
                     --Existen registros en
                     -- la tabla MAEFUNC_STG para procesar
                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                         IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
                         OR t_mf (X).CB_HRMS_EFFSEQ <> J.EFFSEQ
                         THEN
                             l_index := X;
                             t_mf (X).TIPO_TRAN := 'B';
                         END IF;
                     END LOOP;

                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                         IF X <> l_index
                         THEN
                             IF t_mf (X).CB_BX_CHANGE = 0
                             THEN
                                 t_mf (X).TIPO_TRAN := 'D';
                             ELSE
                                 t_mf (X).TICKETCAN :=
                                 t_mf (l_index).TICKETCAN;
                                 t_mf (X).CAT_CONVE :=
                                 t_mf (l_index).CAT_CONVE;
                                 -- EGV 05OCT2015 Inicio
                                 --t_mf (X).CAT_CONT := t_mf (l_index).CAT_CONT;

                                 -- EGV 05OCT2015 Fin
                                 -- EGV 15SEP2015 Inicio
                                 --t_mf (X).STD_HOURS :=
                                 --t_mf (l_index).STD_HOURS;
                                 -- EGV 15SEP2015 Fin
                                 t_mf (X).CB_AREA := t_mf (l_index).CB_AREA;
                             END IF;
                         END IF;
                     END LOOP;

                     t_mf.DELETE (l_index);

                    -- Borro el registro tomado como base
                    -- para la actualizacion de los
                    -- registros con CB_BX_CHANGE=1


                     IF t_mf.COUNT > 0
                     THEN
                        Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                     END IF;
                 END IF;
             END IF;
         ELSE
             -- No Existen registros en la tabla MAEFUNC_STG para procesar
             -- Busco si existen registros en MAEFUNC_TBL
             BASEARG.PKG_INT_HRMS_VALIDA.
             BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
             l_cod_mf,
             J.EFFDT,
             J.EFFSEQ,
             'DE',
             t_mf,
             l_existe);

             IF l_existe > 0
             THEN
                --Existen registros en la tabla MAEFUNC_TBL para procesar
                 IF l_existe = 1
                 THEN -- No hay registros modificados x Buxis
                     IF t_mf.COUNT > 0
                     THEN
                        --Existen registros en
                        --la tabla MAEFUNC_TBL para eliminar
                         FOR X IN t_mf.FIRST .. t_mf.LAST
                         LOOP
                            t_mf (X).TIPO_TRAN := 'D';
                         END LOOP;

                         Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                     END IF;
                 END IF;

                 IF l_existe = 2
                 THEN -- Hay registros modificados x Buxis
                     IF t_mf.COUNT > 0
                     THEN
                        -- Existen registros en la
                        -- tabla MAEFUNC_TBL para procesar
                         FOR X IN t_mf.FIRST .. t_mf.LAST
                         LOOP
                             IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
                             OR t_mf (X).CB_HRMS_EFFSEQ <> J.EFFSEQ
                             THEN
                                 l_index := X;
                                 t_mf (X).TIPO_TRAN := 'B';
                             END IF;
                         END LOOP;

                         FOR X IN t_mf.FIRST .. t_mf.LAST
                         LOOP
                             IF X <> l_index
                             THEN
                                 IF t_mf (X).CB_BX_CHANGE = 0
                                 THEN
                                    t_mf (X).TIPO_TRAN := 'D';
                                 ELSE
                                     t_mf (X).TICKETCAN :=
                                     t_mf (l_index).TICKETCAN;
                                     t_mf (X).CAT_CONVE :=
                                     t_mf (l_index).CAT_CONVE;
                                     -- EGV 05OCT2015 Inicio
                                     --t_mf (X).CAT_CONT :=
                                     --t_mf (l_index).CAT_CONT;
                                     -- EGV 05OCT2015 Fin
                                     -- EGV 15SEP2015 Inicio
                                     --t_mf (X).STD_HOURS :=
                                     --t_mf (l_index).STD_HOURS;
                                     -- EGV 15SEP2015 Fin
                                     t_mf (X).CB_AREA :=
                                        t_mf (l_index).CB_AREA;
                                 END IF;
                             END IF;
                         END LOOP;

                         t_mf.DELETE (l_index);
      -- Borro el registro tomado como base
      -- para la actualizacion de los
      -- registros con CB_BX_CHANGE=1

                         IF t_mf.COUNT > 0
                         THEN
                            Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                         END IF;
                     END IF;
                 END IF;
             ELSE -- Existe = 0 en MAEFUNC_TBL
                 t_mf.EXTEND (1);

                 t_mf (1).NIVEL := g_nivel;
                 t_mf (1).COD_INTFC := g_cod_int;
                 t_mf (1).COD_MF := l_cod_mf;
                 t_mf (1).FECHA_EFECTIVA := J.EFFDT;
                 t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
                 t_mf (1).SEC_EFECTIVA := J.EFFSEQ;
                 t_mf (1).CB_HRMS_EFFSEQ := J.EFFSEQ;
                 t_mf (1).CB_EMPL_RCD := J.EMPL_RCD;
                 t_mf (1).CB_GEID := J.EMPLID;
                 t_mf (1).CAT_CONVE := J.CGAR_CAT_CONVE;
                 --t_mf (1).CAT_CONT := J.CGAR_CAT_CONT;
                 t_mf (1).TICKETCAN := J.CGAR_QSTICKETCAN;
                 -- EGV 22SEP2015 Inicio
                 IF TRIM(NVL(J.CGAR_CANASTA,'N')) <> 'Y' THEN
                    t_mf (1).TICKETCAN := 0;
                 END IF;
                 -- EGV 22SEP2015 Fin
                 -- EGV 15SEP2015 Inicio
                 --t_mf (1).STD_HOURS := J.CGAR_STD_HOURS;
                 -- EGV 15SEP2015 Fin
                 t_mf (1).CB_AREA := J.CGAR_AREA;
                 t_mf (1).CB_BX_CHANGE := 0;
                 t_mf (1).TIPO_TRAN := 'D';
                 t_mf (1).ESTADO_STG := 'E';

                 l_errores := l_errores + 1;

                 g_err_descr :=
                 'Error procesando COD_MF : '
                 || t_mf (1).COD_MF
                 || ' FECHA_EFF : '
                 || t_mf (1).FECHA_EFECTIVA
                 || ' SEC_EFF : '
                 || t_mf (1).SEC_EFECTIVA
                 || ' ACCION : '
                 || t_mf (1).TIPO_TRAN
                 || ' ERROR : No existe el registro a borrar';
                 BASEARG.PKG_BXSCITI_GENERAL.
                 GRABAR_LOG_DET (g_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);

                 -- EGV 07SEP2015 Inicio
                 --Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                 -- EGV 07SEP2015 Fin
             END IF;
         END IF;
     ELSE --NO Es una Eliminacion
     */
     IF l_t_trans <> 'D'
     THEN -- NO es una Eliminaci?n
     -- EGV 20Dic2016 Fin
         -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
         l_cod_mf,
         J.EFFDT,
         J.EFFSEQ,
         -- EGV 07SEP2015 Inicio
         --'FSE+',
         'FSE',
         -- EGV 07SEP2015 Fin
         J.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF t_mf.COUNT = 0
         THEN --NO existen registros en la tabla MAEFUNC_STG
             -- Busco si existen registros en MAEFUNC_TBL
             BASEARG.PKG_INT_HRMS_VALIDA.
             BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
             l_cod_mf,
             J.EFFDT,
             J.EFFSEQ,
             -- EGV 07SEP2015 Inicio
             --'FSE+',
             'FSE',
             -- EGV 07SEP2015 Fin
             t_mf,
             l_existe);

             -- EGV 20Dic2016 Inicio
             -- No se inserta m?s desde JOB_JR, solo desde JOB
             -- No hace falta el mensaje ya que no es un error
             --IF t_mf.COUNT = 0
             --THEN --NO existen registros en la tabla MAEFUNC_TBL

                 -- EGV 07SEP2015 Inicio
                 /*
                 t_mf.EXTEND (1);

                 t_mf (1).NIVEL := g_nivel;
                 t_mf (1).COD_INTFC := g_cod_int;
                 t_mf (1).COD_MF := l_cod_mf;
                 t_mf (1).FECHA_EFECTIVA := J.EFFDT;
                 t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
                 t_mf (1).SEC_EFECTIVA := J.EFFSEQ;
                 t_mf (1).CB_HRMS_EFFSEQ := J.EFFSEQ;
                 t_mf (1).CB_EMPL_RCD := J.EMPL_RCD;
                 t_mf (1).CB_GEID := J.EMPLID;
                 t_mf (1).CAT_CONVE := J.CGAR_CAT_CONVE;
                 --t_mf (1).CAT_CONT := J.CGAR_CAT_CONT;
                 t_mf (1).TICKETCAN := J.CGAR_QSTICKETCAN;
                 t_mf (1).STD_HOURS := J.CGAR_STD_HOURS;
                 t_mf (1).CB_AREA := J.CGAR_AREA;
                 t_mf (1).CB_BX_CHANGE := 0;
                 t_mf (1).TIPO_TRAN := 'I';
                 t_mf (1).ESTADO_STG := 'P';

                 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                 */
            /*
                 g_err_descr :=
                 'Se esta cargando un registro
                 JOB_JR para una fecha/sec inexsitente '
                 || ' COD_MF : '
                 || l_cod_mf
                 || ' FECHA_EFF : '
                 || J.EFFDT
                 || ' SEC_EFF : '
                 || J.EFFSEQ;

                 l_errores := l_errores + 1;

                 RAISE LOG_EXC;
                 -- EGV 20Dic2016 Fin
                 -- EGV 07SEP2015 Fin

             ELSE --Existen registros en la tabla MAEFUNC_TBL
             */
             IF t_mf.COUNT > 0
             THEN
             -- EGV 20Dic2016 Inicio

                -- EGV 20Dic2016 Inicio
                -- No hace falta el mensaje ya que no es un error
                /*
                -- EGV 07SEP2015 Inicio
                IF l_existe = 0
                THEN
                    g_err_descr :=
                     'Se esta cargando un registro JOB_JR
                     para una fecha/sec inexsitente '
                     || ' COD_MF : '
                     || l_cod_mf
                     || ' FECHA_EFF : '
                     || J.EFFDT
                     || ' SEC_EFF : '
                     || J.EFFSEQ;

                     l_errores := l_errores + 1;

                     RAISE LOG_EXC;
                END IF;
                -- EGV 07SEP2015 Fin
                */
                IF l_existe <> 0
                THEN
                -- EGV 20Dic2016 Fin

                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                         BEGIN
                         -- EGV 07SEP2015 Inicio
                         /*
                         IF X = t_mf.FIRST
                         THEN
                             IF l_existe = 0
                             THEN
                                 IF (t_mf (X).CB_HRMS_EFFDT < J.EFFDT)
                                 OR (t_mf (X).CB_HRMS_EFFDT = J.EFFDT
                                 AND t_mf (X).CB_HRMS_EFFSEQ <
                                 J.EFFSEQ)
                                 THEN
                                     t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                                     t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                                     t_mf (X).SEC_EFECTIVA := J.EFFSEQ;
                                     t_mf (X).CB_HRMS_EFFSEQ := J.EFFSEQ;
                                     t_mf (x).CB_EMPL_RCD := J.EMPL_RCD;
                                     t_mf (x).CB_GEID := J.EMPLID;
                                     t_mf (X).CB_BX_CHANGE := 0;
                                     t_mf (X).TIPO_TRAN := 'I';
                                     t_mf (X).ESTADO_STG := 'P';
                                 ELSE
                                     g_err_descr :=
                                     'Existe una inconsistencia
                                     con el registro a procesar 3'
                                     || ' COD_MF : '
                                     || t_mf (X).COD_MF
                                     || ' FECHA_EFF : '
                                     || J.EFFDT
                                     --|| t_mf (X).FECHA_EFECTIVA
                                     || ' SEC_EFF : '
                                     || J.EFFSEQ
                                     --|| t_mf (X).SEC_EFECTIVA
                                     || ' ACCION : '
                                     || t_mf (X).TIPO_TRAN;

                                     l_errores := l_errores + 1;

                                     RAISE LOG_EXC;
                                 END IF;
                             END IF;
                         END IF;
                         */
                         -- EGV 07SEP2015 Fin

                         t_mf (x).CAT_CONVE := J.CGAR_CAT_CONVE;

                         -- EGV 05OCT2015 Inicio
                         --BEGIN
                         --SELECT E.REGION
                         --INTO l_Region
                         --FROM QSEMPRESA E
                         --WHERE E.COD_EMP = t_mf (x).COD_EMP;

                         --IF l_Region = 'ARG'
                         --THEN
                         --    SELECT G.CAT_CONT
                         --    INTO t_mf (x).CAT_CONT
                         --    FROM CB_GRADO G
                         --    WHERE G.GRADO = t_mf (x).GRADO;
                         --END IF;
                         --EXCEPTION
                         --    WHEN OTHERS
                         --    THEN
                         --    NULL;
                         --END;
                         -- EGV 05OCT2015 Fin

                         -- 04OCT2018 BEGIN 01
                         BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET
            (g_cod_int, '*2019*' || g_proc_int, '*'
                    , 'No existen registros en la tabla MAEFUNC_STG'
                    || ', l_existe: ' || l_existe
                    , g_res_log
                    );
                        -- 2019-11-15 Se quita asignación de 
                        -- sindicato y ticketcan por interfaz 
                        --  t_mf (x).sindicato:= J.CGAR_QSSINDICATO;
                         t_mf (x).modcon:= J.CGAR_QSMODCON;
                         t_mf (x).vtocont:= J.CGAR_QSVTOCONT;
                         t_mf (x).fpago_mf:= J.CGAR_QSFPAGO_MF;
                         --QUITO IMG 21.08.2019
                         --t_mf (x).identfp_mf:= J.ACCOUNT;
                         -- 04OCT2018 END 01

                         -- 2019-11-15 Se quita asignación de 
                         -- sindicato y ticketcan por interfaz 
                         -- t_mf (x).TICKETCAN := J.CGAR_QSTICKETCAN;
                         -- EGV 22SEP2015 Inicio
                         IF TRIM(NVL(J.CGAR_CANASTA,'N')) <> 'Y' THEN
                            t_mf (x).TICKETCAN := 0;
                         END IF;
                         -- EGV 22SEP2015 Fin

                         -- EGV 15SEP2015 Inicio
                         --t_mf (x).STD_HOURS := J.CGAR_STD_HOURS;
                         -- EGV 15SEP2015 Fin
                         t_mf (x).CB_AREA := J.CGAR_AREA;
                         EXCEPTION
                             WHEN LOG_EXC
                             THEN
                             BASEARG.PKG_BXSCITI_GENERAL.
                             GRABAR_LOG_DET (g_cod_int,
                             g_proc_int,
                             'E',
                             g_err_descr,
                             g_res_log);
                         END;
                     END LOOP;

                     Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                 END IF;  -- EGV 20Dic2016
             END IF;
         ELSE --Existen registros en la tabla MAEFUNC_STG

             FOR X IN t_mf.FIRST .. t_mf.LAST
             LOOP

                 BEGIN
                 -- EGV 07SEP2015 Inicio
                 /*
                 IF X = t_mf.FIRST
                 THEN
                     IF l_existe = 0
                     THEN
                         IF (t_mf (X).CB_HRMS_EFFDT < J.EFFDT)
                         OR (t_mf (X).CB_HRMS_EFFDT = J.EFFDT
                         AND t_mf (X).CB_HRMS_EFFSEQ < J.EFFSEQ)
                         THEN
                             t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                             t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                             t_mf (X).SEC_EFECTIVA := J.EFFSEQ;
                             t_mf (X).CB_HRMS_EFFSEQ := J.EFFSEQ;
                             t_mf (x).CB_EMPL_RCD := J.EMPL_RCD;
                             t_mf (X).CB_GEID := J.EMPLID;
                             t_mf (X).CB_BX_CHANGE := 0;
                             t_mf (X).TIPO_TRAN := 'I';
                             t_mf (X).ESTADO_STG := 'P';
                         ELSE
                             g_err_descr :=
                             'Existe una inconsistencia
                             con el registro a procesar 4'
                             || ' COD_MF : '
                             || t_mf (X).COD_MF
                             || ' FECHA_EFF : '
                             || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
                             || ' SEC_EFF : '
                             || J.EFFSEQ        --|| t_mf (X).SEC_EFECTIVA
                             || ' ACCION : '
                             || t_mf (X).TIPO_TRAN;

                             l_errores := l_errores + 1;
                             -- EGV 07SEP2015


                             RAISE LOG_EXC;
                         END IF;
                     END IF;
                 END IF;
                 */
                 -- EGV 07SEP2015 Fin
                 -- 04OCT2018 BEGIN 01
            BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET
            (g_cod_int, '*2019*' || g_proc_int, '*'
                    , 'Existen registros en la tabla MAEFUNC_STG'
                    || ', l_existe: ' || l_existe
                    , g_res_log
                    );
                 -- 2019-11-15 Se quita asignación de 
                 -- sindicato y ticketcan por interfaz 
                 -- t_mf (x).sindicato:= J.CGAR_QSSINDICATO;
                    t_mf (x).modcon:= J.CGAR_QSMODCON;
                    t_mf (x).vtocont:= J.CGAR_QSVTOCONT;
                    t_mf (x).fpago_mf:= J.CGAR_QSFPAGO_MF;
                    --QUITO IMG 21.08.2019
                    --t_mf (x).identfp_mf:= J.ACCOUNT;
                 -- 04OCT2018 END 01
                 t_mf (x).CAT_CONVE := J.CGAR_CAT_CONVE;
                 -- EGV 05OCT2015 Inicio
                 --t_mf (x).CAT_CONT := J.CGAR_CAT_CONT;
                 -- EGV 05OCT2015 Fin
                 -- 2019-11-15 Se quita asignación de 
                 -- sindicato y ticketcan por interfaz 
                 -- t_mf (x).TICKETCAN := J.CGAR_QSTICKETCAN;
                 -- EGV 22SEP2015 Inicio
                 IF TRIM(NVL(J.CGAR_CANASTA,'N')) <> 'Y' THEN
                    t_mf (x).TICKETCAN := 0;
                 END IF;
                 -- EGV 22SEP2015 Fin
                 -- EGV 15SEP2015 Inicio
                 --t_mf (x).STD_HOURS := J.CGAR_STD_HOURS;
                 -- EGV 15SEP2015 Fin
                 t_mf (x).CB_AREA := J.CGAR_AREA;

                 IF t_mf (X).ESTADO_STG = 'E'
                 AND t_mf (X).TIPO_TRAN = 'D'
                 THEN
                     t_mf (X).TIPO_TRAN := 'I';
                     t_mf (X).ESTADO_STG := 'P';
                 END IF;

                 IF t_mf (X).ESTADO_STG = 'P'
                 AND t_mf (X).TIPO_TRAN = 'D'
                 THEN
                    t_mf (X).TIPO_TRAN := 'U';
                 END IF;
                 EXCEPTION
                     WHEN LOG_EXC
                     THEN
                     BASEARG.PKG_BXSCITI_GENERAL.
                     GRABAR_LOG_DET (g_cod_int,
                     g_proc_int,
                     'E',
                     g_err_descr,
                     g_res_log);
                 END;
             END LOOP;

             Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
         END IF;
     END IF;
     EXCEPTION
     WHEN NEXT_VAL
     THEN
     NULL;
     WHEN LOG_EXC
     THEN
     BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     g_proc_int,
     'E',
     g_err_descr,
     g_res_log);
     WHEN OTHERS
     THEN
     g_err_descr := SQLERRM;
     BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     g_proc_int,
     'E',
     g_err_descr,
     g_res_log);
     END;
 END LOOP;


 SELECT COUNT (*)
 INTO l_errores
 FROM CB_MAEFUNC_STG S
 WHERE S.COD_INTFC = g_cod_int AND S.ESTADO_STG = 'E';

 IF l_errores > 0
 THEN
 l_res := 'ERRORES : ' || l_errores;
 ELSE
 l_res := 'OK';
 END IF;
 EXCEPTION
 WHEN OTHERS
 THEN
 l_res := 'ERROR : ' || SQLERRM;
 END;

 /** 0130**/
 PROCEDURE Valida_CB_PERS_DT_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 l_cod_mf INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_effseq INTEGER := 0;
 l_empl_rcd INTEGER := 0;
 -- EGV 20Dic2016 Inicio
 l_effdt DATE := TO_DATE ('01/01/1900', 'DD/MM/YYYY');
 l_iLast INTEGER := 0;
 -- EGV 20Dic2016 Fin

 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_PERS_DT_INT
 WHERE COD_INTFC = g_cod_int
 ORDER BY EMPLID, EFFDT;

 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_PERS_DT_INT';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR J IN C_DATOS
 LOOP
     BEGIN
     l_existe := 0;

     t_mf := g_mf_list ();

     g_err_descr := '';

     -- EGV 20Dic2016 Inicio
     /*
     -- EGV 01Jul2015 - Inicio
     --Buscar_Emplid (J.EMPLID, ' ', l_cod_mf);
     Buscar_Emplid (J.EMPLID, ' ', ' ', l_cod_mf);
     -- EGV 01Jul2015 - Fin


     -- EGV 27Ago2015 - Inicio
     --IF l_cod_mf = 0
     IF l_cod_mf <= 0
     -- EGV 27Ago2015 - Fin
     THEN
         g_err_descr := 'Error buscando el empleado : ' || J.EMPLID;
         l_errores := l_errores + 1;
         RAISE LOG_EXC;
     END IF;
     */
     -- EGV 20Dic2016 Fin

     Obtener_Tipo_Trans (J.AUDIT_ACTN, l_t_trans);

     -- g_err_descr :=
     -- 'COD_MF : '
     -- || l_cod_mf
     -- || ' AUDIT_ACTN : '
     -- || l_t_trans
     -- || ' EFFDT : '
     -- || J.EFFDT
     -- || ' EFFSEQ; : '
     -- || J.EFFSEQ;
     -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     -- g_proc_int,
     -- 'I',
     -- g_err_descr,
     -- g_res_log);

     -- EGV 20Dic2016 Inicio -- Las eliminaciones se tratan con JOB
     /*
     IF l_t_trans = 'D'
     THEN --Es una Eliminacion
         -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
         l_cod_mf,
         J.EFFDT,
         l_effseq,
         'DE',
         J.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

         IF l_existe > 0
         THEN
             --Existen registros en la tabla MAEFUNC_STG para procesar
             IF l_existe = 1
             THEN -- No hay registros modificados x Buxis
                 IF t_mf.COUNT > 0
                 THEN
                    -- Existen registros en la
                    -- tabla MAEFUNC_STG para eliminar
                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                         t_mf (X).TIPO_TRAN := 'D';
                     END LOOP;

                     Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                 END IF;
             END IF;

             IF l_existe = 2
             THEN -- Hay registros modificados x Buxis
                 IF t_mf.COUNT > 0
                 THEN
                     -- Existen registros en la
                     -- tabla MAEFUNC_STG para procesar
                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                         IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
                         OR t_mf (X).CB_HRMS_EFFSEQ <> l_effseq
                         THEN
                             l_index := X;
                             t_mf (X).TIPO_TRAN := 'B';
                         END IF;
                     END LOOP;

                     FOR X IN t_mf.FIRST .. t_mf.LAST
                     LOOP
                         IF X <> l_index
                         THEN
                             IF t_mf (X).CB_BX_CHANGE = 0
                             THEN
                                 t_mf (X).TIPO_TRAN := 'D';
                             ELSE
                                 t_mf (X).EST_CIV_MF :=
                                 t_mf (l_index).EST_CIV_MF;
                                 t_mf (X).SEXO_MF := t_mf (l_index).SEXO_MF;
                             END IF;
                         END IF;
                     END LOOP;

                     t_mf.DELETE (l_index);
                     -- Borro el registro tomado como
                     -- base para la actualizacion
                     -- de los registros con CB_BX_CHANGE=1

                     IF t_mf.COUNT > 0
                     THEN
                        Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
                     END IF;
                 END IF;
             END IF;
         ELSE -- No Existen registros en la tabla MAEFUNC_STG para procesar
             -- Busco si existen registros en MAEFUNC_TBL
             BASEARG.PKG_INT_HRMS_VALIDA.
             BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
             l_cod_mf,
             J.EFFDT,
             l_effseq,
             'DE',
             t_mf,
             l_existe);

             IF l_existe > 0
             THEN
                -- Existen registros en la
                -- tabla MAEFUNC_TBL para procesar
                 IF l_existe = 1
                     THEN -- No hay registros modificados x Buxis
                     IF t_mf.COUNT > 0
                     THEN
                        -- Existen registros en la
                        -- tabla MAEFUNC_TBL para eliminar
                         FOR X IN t_mf.FIRST .. t_mf.LAST
                         LOOP
                             t_mf (X).TIPO_TRAN := 'D';
                         END LOOP;

                         Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                     END IF;
                 END IF;

                 IF l_existe = 2
                 THEN -- Hay registros modificados x Buxis
                     IF t_mf.COUNT > 0
                     THEN
                         -- Existen registros en
                         -- la tabla MAEFUNC_TBL para procesar
                         FOR X IN t_mf.FIRST .. t_mf.LAST
                         LOOP
                             IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
                             OR t_mf (X).CB_HRMS_EFFSEQ <> l_effseq
                             THEN
                                 l_index := X;
                                 t_mf (X).TIPO_TRAN := 'B';
                             END IF;
                         END LOOP;

                         FOR X IN t_mf.FIRST .. t_mf.LAST
                         LOOP
                             IF X <> l_index
                             THEN
                                 IF t_mf (X).CB_BX_CHANGE = 0
                                 THEN
                                     t_mf (X).TIPO_TRAN := 'D';
                                 ELSE
                                     t_mf (X).EST_CIV_MF :=
                                     t_mf (l_index).EST_CIV_MF;
                                     t_mf (X).SEXO_MF :=
                                         t_mf (l_index).SEXO_MF;
                                 END IF;
                             END IF;
                         END LOOP;

                         t_mf.DELETE (l_index);
                         -- Borro el registro tomado como base
                         -- para la actualizacion de los
                         -- registros con CB_BX_CHANGE=1

                         IF t_mf.COUNT > 0
                         THEN
                             Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                         END IF;
                     END IF;
                 END IF;
             ELSE -- Existe = 0 en MAEFUNC_TBL
                 t_mf.EXTEND (1);

                 t_mf (1).NIVEL := g_nivel;
                 t_mf (1).COD_INTFC := g_cod_int;
                 t_mf (1).COD_MF := l_cod_mf;
                 t_mf (1).FECHA_EFECTIVA := J.EFFDT;
                 t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
                 t_mf (1).SEC_EFECTIVA := l_effseq;
                 t_mf (1).CB_HRMS_EFFSEQ := l_effseq;
                 t_mf (1).CB_EMPL_RCD := l_empl_rcd;
                 t_mf (1).CB_GEID := J.EMPLID;
                 t_mf (1).EST_CIV_MF := J.MAR_STATUS;
                 t_mf (1).SEXO_MF := J.SEX;
                 t_mf (1).CB_BX_CHANGE := 0;
                 t_mf (1).TIPO_TRAN := 'D';
                 t_mf (1).ESTADO_STG := 'E';

                 l_errores := l_errores + 1;

                 g_err_descr :=
                 'Error procesando COD_MF : '
                 || t_mf (1).COD_MF
                 || ' FECHA_EFF : '
                 || t_mf (1).FECHA_EFECTIVA
                 || ' SEC_EFF : '
                 || t_mf (1).SEC_EFECTIVA
                 || ' ACCION : '
                 || t_mf (1).TIPO_TRAN
                 || ' ERROR : No existe el registro a borrar';
                 BASEARG.PKG_BXSCITI_GENERAL.
                 GRABAR_LOG_DET (g_cod_int,
                 g_proc_int,
                 'E',
                 g_err_descr,
                 g_res_log);

                 -- EGV 07SEP2015 Inicio
                 --Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
                 -- EGV 07SEP2015 Fin
             END IF;
         END IF;
     ELSE --NO Es una Eliminacion
     */
     IF l_t_trans <> 'D'
     THEN --NO es una Eliminacion
     -- EGV 20Dic2016 Fin

        -- EGV 20Dic2016 Inicio
        --tratar a PERS_DT de manera similar a PERS
        --osea no tener en cuenta la fecha efectiva y
        --actualizar todos los registros
        /*
        -- Busco si existen registros en MAEFUNC_STG
         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
         l_cod_mf,
         J.EFFDT,
         l_effseq,
         'FE+',
         J.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);


         IF t_mf.COUNT = 0
         THEN --NO existen registros en la tabla MAEFUNC_STG
             -- Busco si existen registros en MAEFUNC_TBL
             BASEARG.PKG_INT_HRMS_VALIDA.
             BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
             l_cod_mf,
             J.EFFDT,
             l_effseq,
             'FE+',
             t_mf,
             l_existe);

             IF t_mf.COUNT = 0
             THEN --NO existen registros en la tabla MAEFUNC_TBL
                 t_mf.EXTEND (1);

                 t_mf (1).NIVEL := g_nivel;
                 t_mf (1).COD_INTFC := g_cod_int;
                 t_mf (1).COD_MF := l_cod_mf;
                 t_mf (1).FECHA_EFECTIVA := J.EFFDT;
                 t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
                 t_mf (1).SEC_EFECTIVA := l_effseq;
                 t_mf (1).CB_HRMS_EFFSEQ := l_effseq;
                 t_mf (1).CB_EMPL_RCD := l_empl_rcd;
                 t_mf (1).CB_GEID := J.EMPLID;
                 t_mf (1).EST_CIV_MF := J.MAR_STATUS;
                 t_mf (1).SEXO_MF := J.SEX;
                 t_mf (1).CB_BX_CHANGE := 0;
                 t_mf (1).TIPO_TRAN := 'I';
                 t_mf (1).ESTADO_STG := 'P';

                 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
             ELSE --Existen registros en la tabla MAEFUNC_TBL
                 FOR X IN t_mf.FIRST .. t_mf.LAST
                 LOOP
                     BEGIN
                     IF X = t_mf.FIRST
                     THEN
                         IF l_existe = 0
                         THEN
                             IF (t_mf (X).CB_HRMS_EFFDT <= J.EFFDT)
                             THEN
                                 t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                                 t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                                 t_mf (X).SEC_EFECTIVA := l_effseq;
                                 t_mf (X).CB_HRMS_EFFSEQ := l_effseq;
                                 t_mf (X).CB_EMPL_RCD := l_empl_rcd;
                                 t_mf (X).CB_GEID := J.EMPLID;
                                 t_mf (X).CB_BX_CHANGE := 0;
                                 t_mf (X).TIPO_TRAN := 'I';
                                 t_mf (X).ESTADO_STG := 'P';
                             ELSE
                                 g_err_descr :=
                                 'Existe una inconsistencia
                                 con el registro a procesar 5'
                                 || ' COD_MF : '
                                 || t_mf (X).COD_MF
                                 || ' FECHA_EFF : '
                                 || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
                                 || ' SEC_EFF : '
                                 || l_effseq    --|| t_mf (X).SEC_EFECTIVA
                                 || ' ACCION : '
                                 || t_mf (X).TIPO_TRAN;

                                 l_errores := l_errores + 1;

                                 RAISE LOG_EXC;
                             END IF;
                         END IF;
                     END IF;

                     t_mf (x).EST_CIV_MF := J.MAR_STATUS;
                     t_mf (x).SEXO_MF := J.SEX;
                     EXCEPTION
                         WHEN LOG_EXC
                         THEN
                             BASEARG.PKG_BXSCITI_GENERAL.
                             GRABAR_LOG_DET (g_cod_int,
                             g_proc_int,
                             'E',
                             g_err_descr,
                             g_res_log);
                     END;
                 END LOOP;

                 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
             END IF;
         ELSE --Existen registros en la tabla MAEFUNC_STG
                 FOR X IN t_mf.FIRST .. t_mf.LAST
                 LOOP
                     BEGIN
                     IF X = t_mf.FIRST
                     THEN
                         IF l_existe = 0
                         THEN
                             IF (t_mf (X).CB_HRMS_EFFDT <= J.EFFDT)
                             THEN
                                 t_mf (X).FECHA_EFECTIVA := J.EFFDT;
                                 t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
                                 t_mf (X).SEC_EFECTIVA := l_effseq;
                                 t_mf (X).CB_HRMS_EFFSEQ := l_effseq;
                                 t_mf (X).CB_EMPL_RCD := l_empl_rcd;
                                 t_mf (X).CB_GEID := J.EMPLID;
                                 t_mf (X).CB_BX_CHANGE := 0;
                                 t_mf (X).TIPO_TRAN := 'I';
                                 t_mf (X).ESTADO_STG := 'P';
                             ELSE
                                 g_err_descr :=
                                 'Existe una inconsistencia con
                                 el registro a procesar 6'
                                 || ' COD_MF : '
                                 || t_mf (X).COD_MF
                                 || ' FECHA_EFF : '
                                 || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
                                 || ' SEC_EFF : '
                                 || l_effseq    --|| t_mf (X).SEC_EFECTIVA
                                 || ' ACCION : '
                                 || t_mf (X).TIPO_TRAN;

                                 l_errores := l_errores + 1;  -- EGV 07SEP2015

                                 RAISE LOG_EXC;
                             END IF;
                         END IF;
                     END IF;

                     t_mf (x).EST_CIV_MF := J.MAR_STATUS;
                     t_mf (x).SEXO_MF := J.SEX;

                     IF t_mf (X).ESTADO_STG = 'E'
                     AND t_mf (X).TIPO_TRAN = 'D'
                     THEN
                         t_mf (X).TIPO_TRAN := 'I';
                         t_mf (X).ESTADO_STG := 'P';
                     END IF;

                     IF t_mf (X).ESTADO_STG = 'P'
                     AND t_mf (X).TIPO_TRAN = 'D'
                     THEN
                         t_mf (X).TIPO_TRAN := 'U';
                     END IF;
                     EXCEPTION
                         WHEN LOG_EXC
                         THEN
                             BASEARG.PKG_BXSCITI_GENERAL.
                             GRABAR_LOG_DET (g_cod_int,
                             g_proc_int,
                             'E',
                             g_err_descr,
                             g_res_log);
                     END;
                 END LOOP;

                 Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
         END IF;
         */

         BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
         J.EMPLID,
         l_effdt,
         l_effseq,
         'TO+',
         J.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

        IF l_existe = 1
        THEN --Existen registros en la tabla MAEFUNC_STG para procesar

            l_iLast := t_mf.LAST;
            IF (nvl(rtrim(t_mf(l_iLast).EST_CIV_MF),' ')
                <> nvl(rtrim(J.MAR_STATUS),' ')
                OR nvl(rtrim(t_mf(l_iLast).SEXO_MF),' ')
                <> nvl(rtrim(J.SEX),' ')
            )
            THEN

                FOR X IN t_mf.FIRST .. t_mf.LAST
                LOOP
                    t_mf (X).EST_CIV_MF := J.MAR_STATUS;
                    t_mf (X).SEXO_MF := J.SEX;
                END LOOP;

                Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);

            END IF;
        ELSE
            g_err_descr :=
            'No existen Registros para el EMPLID : ' || J.EMPLID;
            l_errores := l_errores + 1;
            RAISE LOG_EXC;
            NULL;
        END IF;
        -- EGV 20Dic2016 Fin

     END IF;
     EXCEPTION
         WHEN NEXT_VAL
         THEN
             NULL;
         WHEN LOG_EXC
         THEN
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
         WHEN OTHERS
         THEN
             g_err_descr := SQLERRM;
             BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
             g_proc_int,
             'E',
             g_err_descr,
             g_res_log);
     END;
 END LOOP;


 SELECT COUNT (*)
 INTO l_errores
 FROM CB_MAEFUNC_STG S
 WHERE S.COD_INTFC = g_cod_int AND S.ESTADO_STG = 'E';

 IF l_errores > 0
 THEN
     l_res := 'ERRORES : ' || l_errores;
 ELSE
     l_res := 'OK';
 END IF;
 EXCEPTION
     WHEN OTHERS
     THEN
         l_res := 'ERROR : ' || SQLERRM;
 END;

 /** 0160**/
 PROCEDURE Valida_CB_ADDR_INT (l_res OUT VARCHAR2)
 AS
 t_mf g_mf_list;
 l_existe INTEGER;
 l_cod_mf INTEGER;
 l_t_trans VARCHAR2 (1);
 l_index INTEGER := 0;
 l_errores INTEGER := 0;
 l_effseq INTEGER := 0;
 l_empl_rcd INTEGER := 0;
 -- EGV 20Dic2016 Inicio
 l_effdt DATE := TO_DATE ('01/01/1900', 'DD/MM/YYYY');
 l_iLast INTEGER := 0;
 -- EGV 20Dic2016 Fin

 CURSOR C_DATOS
 IS
 SELECT *
 FROM CB_ADDR_INT
 WHERE COD_INTFC = g_cod_int
 -- EGV 20Dic2016 Inicio
 --ORDER BY EMPLID, EFFDT;
 ORDER BY NRO_LINEA;
 -- EGV 20Dic2016 Fin

 NEXT_VAL EXCEPTION;
 LOG_EXC EXCEPTION;
 BEGIN
 g_proc_int := 'Valida_CB_ADDR_INT';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'I',
 'Inicio de proceso : ' || g_proc_int,
 g_res_log);

 FOR J IN C_DATOS
 LOOP
 BEGIN
 l_existe := 0;

 t_mf := g_mf_list ();

 g_err_descr := '';

 -- EGV 20Dic2016 Inicio
 /*
 -- EGV 01Jul2015 - Inicio
 --Buscar_Emplid (J.EMPLID, ' ', l_cod_mf);
 Buscar_Emplid (J.EMPLID, ' ', ' ', l_cod_mf);
 -- EGV 01Jul2015 - Fin

 IF l_cod_mf <= 0
 THEN
 g_err_descr := 'Error buscando el empleado : ' || J.EMPLID;
 l_errores := l_errores + 1;
 RAISE LOG_EXC;
 END IF;
 */
 -- EGV 20Dic2016 Fin

 Obtener_Tipo_Trans (J.AUDIT_ACTN, l_t_trans);

 -- g_err_descr :=
 -- 'COD_MF : '
 -- || l_cod_mf
 -- || ' AUDIT_ACTN : '
 -- || l_t_trans
 -- || ' EFFDT : '
 -- || J.EFFDT
 -- || ' EFFSEQ; : '
 -- || J.EFFSEQ;
 -- BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
 -- g_proc_int,
 -- 'I',
 -- g_err_descr,
 -- g_res_log);

 -- EGV 20Dic2016 Inicio -- Las eliminaciones las trata JOB
 /*
 IF l_t_trans = 'D'
 THEN --Es una Eliminacion
 -- Busco si existen registros en MAEFUNC_STG
 BASEARG.PKG_INT_HRMS_VALIDA.
 BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
 l_cod_mf,
 J.EFFDT,
 l_effseq,
 'DE',
 J.EMPLID,   -- EGV 01Jul2015
 t_mf,
 l_existe);

 IF l_existe > 0
 THEN --Existen registros en la tabla MAEFUNC_STG para procesar
 IF l_existe = 1
 THEN -- No hay registros modificados x Buxis
 IF t_mf.COUNT > 0
 THEN --Existen registros en la tabla MAEFUNC_STG para eliminar
 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 t_mf (X).TIPO_TRAN := 'D';
 END LOOP;

 Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
 END IF;
 END IF;

 IF l_existe = 2
 THEN -- Hay registros modificados x Buxis
 IF t_mf.COUNT > 0
 THEN --Existen registros en la tabla MAEFUNC_STG para procesar
 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
 OR t_mf (X).CB_HRMS_EFFSEQ <> l_effseq
 THEN
 l_index := X;
 t_mf (X).TIPO_TRAN := 'B';
 END IF;
 END LOOP;

 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 IF X <> l_index
 THEN
 IF t_mf (X).CB_BX_CHANGE = 0
 THEN
 t_mf (X).TIPO_TRAN := 'D';
 ELSE
 t_mf (X).PAIS_MF := t_mf (l_index).PAIS_MF;
 t_mf (X).DOMIC_MF := t_mf (l_index).DOMIC_MF;
 t_mf (X).LOCALIDAD :=
 t_mf (l_index).LOCALIDAD;
 t_mf (X).PROVINCIA :=
 t_mf (l_index).PROVINCIA;
 t_mf (X).CODPOS := t_mf (l_index).CODPOS;
 END IF;
 END IF;
 END LOOP;

 t_mf.DELETE (l_index);
    -- Borro el registro tomado como base
    -- para la actualizacion de los registros con CB_BX_CHANGE=1

 IF t_mf.COUNT > 0
 THEN
 Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
 END IF;
 END IF;
 END IF;
 ELSE -- No Existen registros en la tabla MAEFUNC_STG para procesar
 -- Busco si existen registros en MAEFUNC_TBL
 BASEARG.PKG_INT_HRMS_VALIDA.
 BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
 l_cod_mf,
 J.EFFDT,
 l_effseq,
 'DE',
 t_mf,
 l_existe);

 IF l_existe > 0
 THEN --Existen registros en la tabla MAEFUNC_TBL para procesar
 IF l_existe = 1
 THEN -- No hay registros modificados x Buxis
 IF t_mf.COUNT > 0
 THEN --Existen registros en la tabla MAEFUNC_TBL para eliminar
 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 t_mf (X).TIPO_TRAN := 'D';
 END LOOP;

 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
 END IF;
 END IF;

 IF l_existe = 2
 THEN -- Hay registros modificados x Buxis
 IF t_mf.COUNT > 0
 THEN --Existen registros en la tabla MAEFUNC_TBL para procesar
 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 IF t_mf (X).CB_HRMS_EFFDT <> J.EFFDT
 OR t_mf (X).CB_HRMS_EFFSEQ <> l_effseq
 THEN
 l_index := X;
 t_mf (X).TIPO_TRAN := 'B';
 END IF;
 END LOOP;

 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 IF X <> l_index
 THEN
 IF t_mf (X).CB_BX_CHANGE = 0
 THEN
 t_mf (X).TIPO_TRAN := 'D';
 ELSE
 t_mf (X).PAIS_MF := t_mf (l_index).PAIS_MF;
 t_mf (X).DOMIC_MF :=
 t_mf (l_index).DOMIC_MF;
 t_mf (X).LOCALIDAD :=
 t_mf (l_index).LOCALIDAD;
 t_mf (X).PROVINCIA :=
 t_mf (l_index).PROVINCIA;
 t_mf (X).CODPOS := t_mf (l_index).CODPOS;
 END IF;
 END IF;
 END LOOP;

 t_mf.DELETE (l_index);
      -- Borro el registro tomado como base
      -- para la actualizacion de los registros con CB_BX_CHANGE=1

 IF t_mf.COUNT > 0
 THEN
 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
 END IF;
 END IF;
 END IF;
 ELSE -- Existe = 0 en MAEFUNC_TBL
 t_mf.EXTEND (1);

 t_mf (1).NIVEL := g_nivel;
 t_mf (1).COD_INTFC := g_cod_int;
 t_mf (1).COD_MF := l_cod_mf;
 t_mf (1).FECHA_EFECTIVA := J.EFFDT;
 t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
 t_mf (1).SEC_EFECTIVA := l_effseq;
 t_mf (1).CB_HRMS_EFFSEQ := l_effseq;
 t_mf (1).CB_EMPL_RCD := l_empl_rcd;
 t_mf (1).CB_GEID := J.EMPLID;
 t_mf (1).PAIS_MF := J.COUNTRY;
 t_mf (1).DOMIC_MF :=
 J.ADDRESS1
 || ' '
 || J.ADDRESS2
 || ' '
 || J.ADDRESS3
 || ' '
 || J.ADDRESS4;
 t_mf (1).LOCALIDAD := J.CITY;
 t_mf (1).PROVINCIA := J.STATE;
 t_mf (1).CODPOS := J.POSTAL;
 t_mf (1).CB_BX_CHANGE := 0;
 t_mf (1).TIPO_TRAN := 'D';
 t_mf (1).ESTADO_STG := 'E';

 l_errores := l_errores + 1;

 g_err_descr :=
 'Error procesando COD_MF : '
 || t_mf (1).COD_MF
 || ' FECHA_EFF : '
 || t_mf (1).FECHA_EFECTIVA
 || ' SEC_EFF : '
 || t_mf (1).SEC_EFECTIVA
 || ' ACCION : '
 || t_mf (1).TIPO_TRAN
 || ' ERROR : No existe el registro a borrar';
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'E',
 g_err_descr,
 g_res_log);

 -- EGV 07SEP2015 Inicio
 --Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
 -- EGV 07SEP2015 Fin
 END IF;
 END IF;
  ELSE --NO Es una Eliminacion
 */
 IF l_t_trans <> 'D'
 THEN --NO es una Eliminacion
-- EGV 20Dic2016 Fin

-- EGV 20Dic2016 Inicio -- Se asemeja el tratamiento a CB_PERS
-- Osea sin tener en cuenta la fecha efectiva,
-- se actualizan todos los registros

/*
 -- Busco si existen registros en MAEFUNC_STG
 BASEARG.PKG_INT_HRMS_VALIDA.
 BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
 l_cod_mf,
 J.EFFDT,
 l_effseq,
 'FE+',
 J.EMPLID,   -- EGV 01Jul2015
 t_mf,
 l_existe);


 IF t_mf.COUNT = 0
 THEN --NO existen registros en la tabla MAEFUNC_STG
 -- Busco si existen registros en MAEFUNC_TBL
 BASEARG.PKG_INT_HRMS_VALIDA.
 BUSCAR_REG_MAEFUNC_TBL (J.COD_INTFC,
 l_cod_mf,
 J.EFFDT,
 l_effseq,
 'FE+',
 t_mf,
 l_existe);

 IF t_mf.COUNT = 0
 THEN --NO existen registros en la tabla MAEFUNC_TBL
 t_mf.EXTEND (1);

 t_mf (1).NIVEL := g_nivel;
 t_mf (1).COD_INTFC := g_cod_int;
 t_mf (1).COD_MF := l_cod_mf;
 t_mf (1).FECHA_EFECTIVA := J.EFFDT;
 t_mf (1).CB_HRMS_EFFDT := J.EFFDT;
 t_mf (1).SEC_EFECTIVA := l_effseq;
 t_mf (1).CB_HRMS_EFFSEQ := l_effseq;
 t_mf (1).CB_EMPL_RCD := l_empl_rcd;
 t_mf (1).CB_GEID := J.EMPLID;
 t_mf (1).PAIS_MF := J.COUNTRY;
 t_mf (1).DOMIC_MF :=
 J.ADDRESS1
 || ' '
 || J.ADDRESS2
 || ' '
 || J.ADDRESS3
 || ' '
 || J.ADDRESS4;
 t_mf (1).LOCALIDAD := J.CITY;
 t_mf (1).PROVINCIA := J.STATE;
 t_mf (1).CODPOS := J.POSTAL;
 t_mf (1).CB_BX_CHANGE := 0;
 t_mf (1).TIPO_TRAN := 'I';
 t_mf (1).ESTADO_STG := 'P';

 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
 ELSE --Existen registros en la tabla MAEFUNC_TBL
 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 BEGIN
 IF X = t_mf.FIRST
 THEN
 IF l_existe = 0
 THEN
 IF (t_mf (X).CB_HRMS_EFFDT <= J.EFFDT)
 THEN
 t_mf (X).FECHA_EFECTIVA := J.EFFDT;
 t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
 t_mf (X).SEC_EFECTIVA := l_effseq;
 t_mf (X).CB_HRMS_EFFSEQ := l_effseq;
 t_mf (X).CB_EMPL_RCD := l_empl_rcd;
 t_mf (X).CB_GEID := J.EMPLID;
 t_mf (X).CB_BX_CHANGE := 0;
 t_mf (X).TIPO_TRAN := 'I';
 t_mf (X).ESTADO_STG := 'P';
 ELSE
 g_err_descr :=
 'Existe una inconsistencia con el registro a procesar 7'
 || ' COD_MF : '
 || t_mf (X).COD_MF
 || ' FECHA_EFF : '
 || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
 || ' SEC_EFF : '
 || l_effseq    --|| t_mf (X).SEC_EFECTIVA
 || ' ACCION : '
 || t_mf (X).TIPO_TRAN;

 l_errores := l_errores + 1;

 RAISE LOG_EXC;
 END IF;
 END IF;
 END IF;

 t_mf (X).PAIS_MF := J.COUNTRY;
 t_mf (X).DOMIC_MF :=
 J.ADDRESS1
 || ' '
 || J.ADDRESS2
 || ' '
 || J.ADDRESS3
 || ' '
 || J.ADDRESS4;
 t_mf (X).LOCALIDAD := J.CITY;
 t_mf (X).PROVINCIA := J.STATE;
 t_mf (X).CODPOS := J.POSTAL;
 EXCEPTION
 WHEN LOG_EXC
 THEN
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'E',
 g_err_descr,
 g_res_log);
 END;
 END LOOP;

 Impacta_reg_MAEFUNC_STG ('I', t_mf, l_res);
 END IF;
 ELSE --Existen registros en la tabla MAEFUNC_STG
 FOR X IN t_mf.FIRST .. t_mf.LAST
 LOOP
 BEGIN
 IF X = t_mf.FIRST
 THEN
 IF l_existe = 0
 THEN
 IF (t_mf (X).CB_HRMS_EFFDT <= J.EFFDT)
 THEN
 t_mf (X).FECHA_EFECTIVA := J.EFFDT;
 t_mf (X).CB_HRMS_EFFDT := J.EFFDT;
 t_mf (X).SEC_EFECTIVA := l_effseq;
 t_mf (X).CB_HRMS_EFFSEQ := l_effseq;
 t_mf (X).CB_EMPL_RCD := l_empl_rcd;
 t_mf (X).CB_GEID := J.EMPLID;
 t_mf (X).CB_BX_CHANGE := 0;
 t_mf (X).TIPO_TRAN := 'I';
 t_mf (X).ESTADO_STG := 'P';
 ELSE
 g_err_descr :=
 'Existe una inconsistencia con el registro a procesar 8'
 || ' COD_MF : '
 || t_mf (X).COD_MF
 || ' FECHA_EFF : '
 || J.EFFDT     --|| t_mf (X).FECHA_EFECTIVA
 || ' SEC_EFF : '
 || l_effseq    --|| t_mf (X).SEC_EFECTIVA
 || ' ACCION : '
 || t_mf (X).TIPO_TRAN;

 l_errores := l_errores + 1;        -- EGV 07SEP2015

 RAISE LOG_EXC;
 END IF;
 END IF;
 END IF;

 t_mf (X).PAIS_MF := J.COUNTRY;
 t_mf (X).DOMIC_MF :=
 J.ADDRESS1
 || ' '
 || J.ADDRESS2
 || ' '
 || J.ADDRESS3
 || ' '
 || J.ADDRESS4;
 t_mf (X).LOCALIDAD := J.CITY;
 t_mf (X).PROVINCIA := J.STATE;
 t_mf (X).CODPOS := J.POSTAL;


 IF t_mf (X).ESTADO_STG = 'E'
 AND t_mf (X).TIPO_TRAN = 'D'
 THEN
 t_mf (X).TIPO_TRAN := 'I';
 t_mf (X).ESTADO_STG := 'P';
 END IF;

 IF t_mf (X).ESTADO_STG = 'P'
 AND t_mf (X).TIPO_TRAN = 'D'
 THEN
 t_mf (X).TIPO_TRAN := 'U';
 END IF;
 EXCEPTION
 WHEN LOG_EXC
 THEN
 BASEARG.PKG_BXSCITI_GENERAL.
 GRABAR_LOG_DET (g_cod_int,
 g_proc_int,
 'E',
 g_err_descr,
 g_res_log);
 END;
 END LOOP;

 Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);
 END IF;
 */
        BASEARG.PKG_INT_HRMS_VALIDA.
         BUSCAR_REG_MAEFUNC_STG (J.COD_INTFC,
         J.EMPLID,
         l_effdt,
         l_effseq,
         'TO+',
         J.EMPLID,   -- EGV 01Jul2015
         t_mf,
         l_existe);

        IF l_existe = 1
        THEN --Existen registros en la tabla MAEFUNC_STG para procesar

            l_iLast := t_mf.LAST;
            IF (nvl(rtrim(t_mf(l_iLast).PAIS_MF),' ')
                <> nvl(rtrim(J.COUNTRY),'')
                OR nvl(rtrim(t_mf(l_iLast).DOMIC_MF),' ')
                <> nvl(rtrim(J.ADDRESS1 || ' '
                            || J.ADDRESS2 || ' '
                            || J.ADDRESS3 || ' '
                            || J.ADDRESS4),' ')
                OR nvl(rtrim(t_mf(l_iLast).LOCALIDAD),' ')
                <> nvl(rtrim(J.CITY),' ')
                OR nvl(rtrim(t_mf(l_iLast).PROVINCIA),' ')
                <> nvl(rtrim(J.STATE),' ')
                OR nvl(rtrim(t_mf(l_iLast).CODPOS),' ')
                <> nvl(rtrim(J.POSTAL),' ')
                )
            THEN

                FOR X IN t_mf.FIRST .. t_mf.LAST
                LOOP
                    t_mf (X).PAIS_MF := J.COUNTRY;
                    t_mf (X).DOMIC_MF := J.ADDRESS1 || ' '
                                    || J.ADDRESS2 || ' '
                                    || J.ADDRESS3 || ' '
                                    || J.ADDRESS4;
                     t_mf (X).LOCALIDAD := J.CITY;
                     t_mf (X).PROVINCIA := J.STATE;
                     t_mf (X).CODPOS := J.POSTAL;
                END LOOP;

                Impacta_reg_MAEFUNC_STG ('U', t_mf, l_res);

            END IF;
        ELSE
            g_err_descr :=
            'No existen Registros para el EMPLID : ' || J.EMPLID;
            l_errores := l_errores + 1;
            RAISE LOG_EXC;
            NULL;
        END IF;
 -- EGV20Dic2016 Fin

 END IF;
 EXCEPTION
 WHEN NEXT_VAL
 THEN
 NULL;
 WHEN LOG_EXC
 THEN
     BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     g_proc_int,
     'E',
     g_err_descr,
     g_res_log);
 WHEN OTHERS
 THEN
     g_err_descr := SQLERRM;
     BASEARG.PKG_BXSCITI_GENERAL.GRABAR_LOG_DET (g_cod_int,
     g_proc_int,
     'E',
     g_err_descr,
     g_res_log);
 END;
 END LOOP;


 SELECT COUNT (*)
 INTO l_errores
 FROM CB_MAEFUNC_STG S
 WHERE S.COD_INTFC = g_cod_int AND S.ESTADO_STG = 'E';

 IF l_errores > 0
 THEN
 l_res := 'ERRORES : ' || l_errores;
 ELSE
 l_res := 'OK';
 END IF;
 EXCEPTION
 WHEN OTHERS
 THEN
 l_res := 'ERROR : ' || SQLERRM;
 END;
END PKG_INT_HRMS_VALIDA;
