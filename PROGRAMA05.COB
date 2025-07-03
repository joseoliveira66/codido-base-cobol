       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA05.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: VARIAVEL DE NIVEL (ESTRUTURAL E FILLER)
      *== DATA: 10/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       01 WRK-DATA.
          05 WRK-ANO  PIC X(04)   VALUE SPACES.
          05 WRK-MES  PIC X(02)   VALUE SPACES.
          05 WRK-DIA  PIC X(02)   VALUE SPACES.
      *INVERTENDO A VARIAVEL
       01 WRK-DATA2.
          05 WRK-DIA  PIC X(02)   VALUE SPACES.
          05 FILLER   PIC X(01)   VALUE "/".
          05 WRK-MES  PIC X(02)   VALUE SPACES.
          05 FILLER   PIC X(01)   VALUE "/".
          05 WRK-ANO  PIC X(04)   VALUE SPACES.

      *FILLER PODE SER USADO PARA SEPARADORES COMO /, -, = E OUTROS

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            ACCEPT WRK-DATA FROM DATE YYYYMMDD.
            MOVE CORR WRK-DATA TO WRK-DATA2.

       0200-MOSTRA                SECTION.
      *0    DISPLAY "A DATA ATUAL E: " WRK-DATA.
      *      DISPLAY "DIA " WRK-DATA(07:02).
      *      DISPLAY "MES " WRK-DATA(05:02).
      *      DISPLAY "ANO " WRK-DATA(01:04).

             DISPLAY "DIA " WRK-DIA OF WRK-DATA2
                     " MES " WRK-MES OF WRK-DATA2
                     " ANO " WRK-ANO OF WRK-DATA2.

      *IMPRIME A CONDICAO DA SEGUNDA VARIAVEL ESTRUTURADA

             DISPLAY "DATA " WRK-DATA2.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
