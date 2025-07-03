       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA02.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: VARIAVEL COM CASA DECIMAL
      *== DATA: 04/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE             SECTION.

       77 WRK-NOME                PIC X(30) VALUE SPACES.
       77 WRK-IDADE               PIC 9(03) VALUE ZEROS.
       77 WRK-SALARIO             PIC S9(08)V99 VALUE ZEROS.
       77 WRK-SALARIO-ED          PIC -ZZ.ZZ9,99 VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
            DISPLAY "DIGITE SEU NOME"
               ACCEPT WRK-NOME.
            DISPLAY "DIGITE SUA IDADE"
               ACCEPT WRK-IDADE.
            DISPLAY "DIGITE SEU SALARIO"
               ACCEPT WRK-SALARIO.
            MOVE WRK-SALARIO TO WRK-SALARIO-ED.

       0200-MOSTRA                SECTION.
            DISPLAY "NOME DIGITADO:" WRK-NOME.
            DISPLAY "IDADE DIGITADA:" WRK-IDADE.
            DISPLAY "SALARIO DIGITADO:" WRK-SALARIO-ED.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
