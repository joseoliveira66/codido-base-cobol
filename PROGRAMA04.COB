       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA04.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: MOVE (EXEMPLOS)
      *== DATA: 06/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE             SECTION.

       77 WRK-VALOR0                PIC X(05)      VALUE ZEROS.
       77 WRK-VALOR1                PIC 9(05)      VALUE ZEROS.
       77 WRK-VALOR2                PIC 9(05)      VALUE ZEROS.
       77 WRK-RESULTADO             PIC S9(05)V99  VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            DISPLAY "DIGITE O VALOR 1"
               ACCEPT WRK-VALOR0.
            DISPLAY "DIGITE O VALOR 2"
            MOVE WRK-VALOR0 TO WRK-VALOR1
               ACCEPT WRK-VALOR2.
            COMPUTE WRK-RESULTADO = WRK-VALOR1 - WRK-VALOR2.

       0200-MOSTRA                SECTION.
            DISPLAY "RESULTADO: " WRK-RESULTADO.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
