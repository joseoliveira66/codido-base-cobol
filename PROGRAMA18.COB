       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA18.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: USO DO PERFORM COM TABUADA
      *== DATA: 18/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.

       77 WRK-NUMERO     PIC 9(02)    VALUE ZEROS.
       77 WRK-CONTADOR   PIC 9(02)    VALUE ZEROS.
       77 WRK-RESULTADO  PIC 9(03)    VALUE ZEROS.

       PROCEDURE                       DIVISION.
       0001-PRINCIPAL                  SECTION.
           PERFORM 0100-INICIALIZAR
           PERFORM 0200-PROCESSAR
           PERFORM 0300-FINALIZAR
           STOP RUN.

       0100-INICIALIZAR                SECTION.

           DISPLAY "DIGITE UM NUMERO PARA TABUADA"
           ACCEPT WRK-NUMERO.

       0200-PROCESSAR                   SECTION.

      *===================UNTIL===================================
      *     PERFORM 10 TIMES
      *     PERFORM UNTIL WRK-CONTADOR EQUAL 10
      *     ADD 1 TO WRK-CONTADOR
      *     COMPUTE WRK-RESULTADO = WRK-NUMERO * WRK-CONTADOR
      *     DISPLAY WRK-NUMERO " X " WRK-CONTADOR " = " WRK-RESULTADO
      *     END-PERFORM.

      *===================VARYING==================================
           PERFORM VARYING WRK-CONTADOR FROM 1 BY 1
                   UNTIL WRK-CONTADOR GREATER 10
           COMPUTE WRK-RESULTADO = WRK-NUMERO * WRK-CONTADOR
           DISPLAY WRK-NUMERO " X " WRK-CONTADOR " = " WRK-RESULTADO
           END-PERFORM.

       0300-FINALIZAR                   SECTION.
                DISPLAY "FIM DO PROGRAMA".
