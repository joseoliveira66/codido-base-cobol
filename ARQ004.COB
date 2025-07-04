       IDENTIFICATION              DIVISION.
       PROGRAM-ID. ARQ004.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: GRAVACAO DE REGISTRO NO ARQUIVO CLIENTE.DAT
      *== DATA: 04/03/2025
      *== OBSERVACOES: INFORMACOES RELEVANTES
      *========================================
       ENVIRONMENT                DIVISION.
       CONFIGURATION              SECTION.
       INPUT-OUTPUT               SECTION.
       FILE-CONTROL.
         SELECT CLIENTES ASSIGN TO "C:\COBOL\DADOS\CLIENTES.DAT"
         FILE STATUS IS FS-CLIENTES.

       DATA                       DIVISION.
       FILE                       SECTION.
       FD CLIENTES.
       01 RG-CLIENTES.
           05 REG-ID           PIC 9(04).
           05 REG-NOME         PIC X(20).
           05 REG-TELEFONE     PIC X(11).

       WORKING-STORAGE            SECTION.
       77 FS-CLIENTES   PIC 9(02) VALUE ZEROS.
       77 WRK-MSG-ERRO  PIC X(30) VALUE SPACES.

       PROCEDURE                  DIVISION.
       0000-PRINCIPAL             SECTION.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR           SECTION.
           OPEN OUTPUT CLIENTES.
           IF FS-CLIENTES EQUAL 35
               MOVE "O ARQUIVO NAO PODE SER ABERTO" TO WRK-MSG-ERRO
               PERFORM 9000-TRATA-ERRO
           END-IF.

       0200-PROCESSAR             SECTION.
               MOVE 0001                    TO REG-ID.
               MOVE 'JOSE                '  TO REG-NOME.
               MOVE '99999999999'           TO REG-TELEFONE.
              WRITE RG-CLIENTES.

               MOVE 0002                    TO REG-ID.
               MOVE 'IVAN                '  TO REG-NOME.
               MOVE '33333333333'           TO REG-TELEFONE.
              WRITE RG-CLIENTES.

               MOVE 0003                    TO REG-ID.
               MOVE 'TEKA                '  TO REG-NOME.
               MOVE '33333333333'           TO REG-TELEFONE.
              WRITE RG-CLIENTES.

       0300-FINALIZAR             SECTION.

       DISPLAY "FIM DE PROGRAMA".

       CLOSE CLIENTES.

       9000-TRATA-ERRO.
           DISPLAY WRK-MSG-ERRO.
           GOBACK.
