0     ! MiniTerm - A basic terminal client for the HP 9816
1     | by Davide Bucci (Twitter: @davbucci, Github: DarwinNE)
2     ! with contributions from Travis Rosenbaum (Twitter/GH: @Zagrophyte)
10    CLEAR SCREEN
20    CONTROL 9,12;128+32+16 !Deactivate RS232 HW Control
30    Action=0
40    ON KBD GOSUB Keyhandler
50    Oa=Action
60    Addcr=1
70    GOSUB Disptermline
80    Choice$=""
90    Warning=0
100   ASSIGN @Bufin TO BUFFER [1000]
110   ASSIGN @Rs232in TO 9
120   ASSIGN @Rs232out TO 9
130   TRANSFER @Rs232in TO @Bufin;CONT
140   ASSIGN @Bufout TO BUFFER [1000]
150   TRANSFER @Bufout TO @Rs232out;CONT
160   OUTPUT @Bufout;" "
170 Termloop:   ! Main terminal loop
180   IF Oa<>Action THEN
190     GOTO Exittermloop
200   END IF
210   IF LEN(Choice$)>0 THEN
220     SELECT Choice$
230     CASE CHR$(255)&"E" ! Enter Key
240       Choice$=CHR$(13)!&CHR$(10)
250     CASE CHR$(255)&"B" ! Backspace
260       Choice$=CHR$(8)
270     CASE CHR$(17) ! CTRL+Q
280       GOTO Exittermloop
290     CASE CHR$(4) ! CTRL+D
300       IF Addcr THEN
310         Addcr=0
320       ELSE
330         Addcr=1
340       END IF
350       GOSUB Disptermline
360     CASE CHR$(255)&"U" ! Caps lock
370       STATUS 2,0;I
380       IF I THEN
390         CONTROL 2,0;0
400       ELSE
410         CONTROL 2,0;1
420       END IF
430       GOTO Termloop
440     CASE ELSE
450       IF Choice$[1,1]=CHR$(255) THEN
460         Choice$=""
470         GOTO Termloop
480       END IF
490     END SELECT
500    !PRINT CHR$(132);Choice$;CHR$(128);
510     OUTPUT @Bufout;Choice$;
520     Choice$=""
530   END IF
540   STATUS @Bufin,4;I
550   IF I>0 THEN
560     IF I>900 THEN
570       DISP "WARNING: Receive buffer is more than 90% full!"
580       Warning=1
590     ELSE
600       IF Warning THEN
610         Warning=0
620         DISP ""
630       END IF
640     END IF
650     ENTER @Bufin USING "#,A";A$
660     IF A$=CHR$(13) AND Addcr THEN
670       A$=CHR$(13)&CHR$(10)
680     END IF
690     PRINT A$;
700   END IF
710   GOTO Termloop
720 Disptermline:     !
730   DISP "CTRL+Q to quit miniterm. ";
740   IF Addcr THEN
750     DISP "CTRL+D to disable CR+LF"
760   ELSE
770     DISP "CTRL+D to enable CR+LF"
780   END IF
790   RETURN
800 Exittermloop:   !
810   ABORTIO @Rs232in
820   ABORTIO @Rs232out
830   ASSIGN @Rs232 TO *
840   ASSIGN @Bufin TO *
850   ASSIGN @Bufout TO *
860   DISP "Exited."
870   STOP
880 Keyhandler:   !
890   Choice$=KBD$
900   RETURN
910   END
