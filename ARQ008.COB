       IDENTIFICATION              DIVISION.
       PROGRAM-ID. ARQ008.
      *========================================
      *== AUTOR: JOSE OLIVEIRA    EMPRESA: CDB
      *== OBJETIVO: GRAVACAO DE REGISTRO ACCEPT CLIENTE.DAT
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

      *---------------VARIAVEIS DE ENTRADA DE DADOS------
       77 WRK-ID         PIC 9(04) VALUE ZEROS.
       77 WRK-NOME       PIC X(20) VALUE SPACES.
       77 WRK-TELEFONE   PIC X(11) VALUE SPACES.

       PROCEDURE                  DIVISION.
       0000-PRINCIPAL             SECTION.
           PERFORM 0100-INICIALIZAR.
           PERFORM 0200-PROCESSAR.
           PERFORM 0300-FINALIZAR.
           STOP RUN.

       0100-INICIALIZAR           SECTION.
           OPEN EXTEND CLIENTES.
           IF FS-CLIENTES EQUAL 35
               MOVE "O ARQUIVO NAO PODE SER ABERTO" TO WRK-MSG-ERRO
               PERFORM 9000-TRATA-ERRO
           END-IF.

               PERFORM 0110-RECEBER-DADOS.

       0110-RECEBER-DADOS         SECTION.
           DISPLAY 'DIGITE O CODIGO...'.
               ACCEPT WRK-ID.
           DISPLAY 'DIGITE O NOME.....'.
               ACCEPT WRK-NOME.
           DISPLAY 'DIGITE O TELEFONE.'.
               ACCEPT WRK-TELEFONE.


       0200-PROCESSAR             SECTION.

            MOVE WRK-ID         TO REG-ID.
            MOVE WRK-NOME       TO REG-NOME.
            MOVE WRK-TELEFONE   TO REG-TELEFONE.

            WRITE RG-CLIENTES.

       0300-FINALIZAR             SECTION.

       DISPLAY "FIM DE PROGRAMA".

       CLOSE CLIENTES.

       9000-TRATA-ERRO.
           DISPLAY WRK-MSG-ERRO.
           GOBACK.
