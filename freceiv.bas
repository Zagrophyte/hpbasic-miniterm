1 ! Simple file receiver program
4 DIM Line$[1000]
5 DIM B$[10000] BUFFER  ! You need a big buffer, as the program is slow
6 PURGE "TEST"          ! Remove this if the file does not exist
10 CREATE ASCII "TEST",20 ! You should know the file size in advance!
20 ASSIGN @Outf TO "TEST"
30 ASSIGN @Rs232 TO 9
40 ASSIGN @Buf TO BUFFER B$
50 TRANSFER @Rs232 TO @Buf;CONT
60 STATUS @Buf,4;I
70 IF I=0 THEN GOTO 60
80 ENTER @Buf USING "#,A";A$
90 !DISP A$;            ! This slows down the process
92 IF A$=CHR$(13) OR A$=CHR$(10) THEN
93   OUTPUT @Outf;Line$
94   PRINT Line$
96   Line$=""
97 ELSE
98   Line$=Line$&A$
99 END IF
100 GOTO 60
110 END