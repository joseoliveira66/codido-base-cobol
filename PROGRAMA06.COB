       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA06.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: VARIAVEL ESTRUTURAL (GRUPO) EXE02
      *== DATA: 11/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
      * 01 WRK-ENTRADA.
      *    05 WRK-CODIGO  PIC 9(04)  VALUE ZEROS.
      *    05 WRK-NOME    PIC X(15)  VALUE SPACES.
      *    05 WRK-SALARIO PIC 9(06)  VALUE ZEROS.

       COPY "#BOOKDAT".

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            DISPLAY "ENTRE COM A LINHA DE DADOS"
            ACCEPT WRK-ENTRADA.
       0200-MOSTRA                SECTION.
            DISPLAY "CODIGO   " WRK-CODIGO.
            DISPLAY "NOME     " WRK-NOME.
            DISPLAY "SALARIO  " WRK-SALARIO.
       0300-FINALIZAR             SECTION.
                        STOP RUN.
