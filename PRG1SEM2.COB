       IDENTIFICATION              DIVISION.
       PROGRAM-ID. EXE1SEM2.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CALCULO DE SALARIO C/ DESC DE IR
      *== DATA: 16/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77 WRK-SALARIOBR     PIC 9(004)V99  VALUE ZEROS.
       77 WRK-IR            PIC 9(004)V99  VALUE ZEROS.
       77 WRK-SALARIOFINAL  PIC 9(005)V99  VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
           DISPLAY "DIGITE O SALARIO."
           ACCEPT WRK-SALARIOBR.

       0150-PROCESSA              SECTION.

           IF WRK-SALARIOBR GREATER THAN OR EQUAL 1000
               COMPUTE WRK-IR = WRK-SALARIOBR * 0,15
           ELSE
               COMPUTE WRK-IR = WRK-SALARIOBR * 0,10
           END-IF.

           COMPUTE WRK-SALARIOFINAL = WRK-SALARIOBR - WRK-IR.

       0200-MOSTRA                SECTION.
           DISPLAY "SALARIO BRUTO   ..." WRK-SALARIOBR
           DISPLAY "IMPOSTO DE RENDA..." WRK-IR
           DISPLAY "SALARIO FINAL   ..." WRK-SALARIOFINAL.

       0300-FINALIZAR             SECTION.
                        STOP RUN.
