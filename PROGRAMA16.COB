       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA13.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: CALCULAR MEDIA (EVALUATE TRUE)
      *== DATA: 13/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                       DIVISION.
       WORKING-STORAGE            SECTION.
       77 WRK-NOTA1  PIC 9(02)V99 VALUE ZEROS.
       77 WRK-NOTA2  PIC 9(02)V99 VALUE ZEROS.
       77 WRK-MEDIA  PIC 9(02)V99 VALUE ZEROS.

       PROCEDURE                  DIVISION.
       0100-RECEBE                SECTION.
           DISPLAY "NOTA 1"
             ACCEPT WRK-NOTA1

           DISPLAY "NOTA2"
             ACCEPT WRK-NOTA2.

       0150-PROCESSA              SECTION.
           COMPUTE WRK-MEDIA = (WRK-NOTA1 + WRK-NOTA2) / 2.

       0200-MOSTRA                SECTION.
           DISPLAY "A MEDIA E: " WRK-MEDIA.

           EVALUATE TRUE
              WHEN WRK-MEDIA GREATER 10
                DISPLAY "DIGITE UM NUMERO VALIDO"
              WHEN WRK-MEDIA GREATER THAN OR EQUAL 6 AND LESS 11
                DISPLAY "APROVADO"
              WHEN OTHER
                DISPLAY "REPROVADO"
           END-EVALUATE.


       0300-FINALIZAR             SECTION.
                        STOP RUN.
