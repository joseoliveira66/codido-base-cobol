       IDENTIFICATION              DIVISION.
       PROGRAM-ID. PROGRAMA21.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: TESTAR A FUNCIONALIDADE DO MODULO DATAMES
      *== DATA: 24/02/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA                           DIVISION.
       WORKING-STORAGE                SECTION.
       77 WRK-DATA     PIC X(34).

       PROCEDURE                       DIVISION.
           0001-PRINCIPAL              SECTION.
           CALL 'DATAMES' USING WRK-DATA.
           DISPLAY WRK-DATA.
           STOP RUN.
