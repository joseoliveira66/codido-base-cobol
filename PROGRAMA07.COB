       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA07.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: VARIAVEL ESTRUTURAL (GRUPO) EXE
      *== DATA: 11/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       01 WRK-DADOS.
          05 WRK-NUM1   PIC 9(03)    VALUE ZEROS.
          05 WRK-NUM2   PIC 9(03)    VALUE ZEROS.

       77 WRK-RESULT    PIC 9(03)    VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            ACCEPT WRK-DADOS.
            COMPUTE WRK-RESULT = WRK-NUM1 + WRK-NUM2.

       0200-MOSTRA                SECTION.
            DISPLAY "RESULTADO " WRK-RESULT.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
