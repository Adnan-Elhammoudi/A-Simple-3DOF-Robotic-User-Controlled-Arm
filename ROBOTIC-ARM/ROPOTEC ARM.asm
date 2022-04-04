


#INCLUDE<P16F877a.INC>  

	cblock		20h
servotime
 servotime2
   pulseB
	pulse
	 pulseA
      pulseC
	   timer1
	    timer2
	     timer3 
          msd
	       lsd
            TEMP
             COUNTER17
              COUNTER10
               RESULTA
                RESULTB
                 MUL_counter
                  T_A
                   T_B
                    T_C                
                     TMP 
                      TEMPN
                       hundreds
                        tens
                         units
                          PERV_VAL_A
                           PERV_VAL_B
                            PERV_VAL_C 
	endc             


	ORG 00H
	GOTO initial
	
	
	
initial

BANKSEL TRISA
MOVLW 0xff             ;POTENTIOMETER A>PORTA.0 B>PORTA.1  C>PORTA.2
MOVWF TRISA	
CLRF TRISC             ;SERVO A >>PORTC.0  B>>PORTC.1   C>>PORTC.2
CLRF TRISB
CLRF TRISD                ;LCD PORTD.0-PORTD.7
Banksel   PORTA
CLRF ADRESH 
CLRF PERV_VAL_A
CLRF PERV_VAL_B
CLRF PERV_VAL_C
Movlw     0x38                  ;8-bit mode, 2-line display, 5x7 dot format
Call      send_cmd
Movlw     0x0e                  ;Display on, Cursor Underline on, Blink off
Call      send_cmd
Movlw     0x02                  ;Display and cursor home
Call      send_cmd
Movlw     0x01                   ;clear display
Call      send_cmd
MOVLW     'A'                      
Call      send_char
movlw     81h
Call      send_cmd
MOVLW     ':'
Call      send_char
MOVLW     83H
CALL      send_cmd           ;display A AND B AND C INITIAL VALUES = '0'
movlw     '0'
CALL      send_char

movlw     8Bh
Call      send_cmd
MOVLW     'B'
Call      send_char
movlw     8Ch
Call      send_cmd
MOVLW     ':'
Call      send_char
MOVLW     8EH
CALL      send_cmd
movlw     '0'
CALL      send_char

movlw     0XC6
Call      send_cmd
MOVLW     'C'
Call      send_char
movlw     0XC7
Call      send_cmd
MOVLW     ':'
Call      send_char
MOVLW     0XC9
CALL      send_cmd
movlw     '0'
CALL      send_char

MOVLW     00H		                 ;A/D data LEFT justified
MOVWF     ADCON1 

;##################################################################################
start          
  Banksel   PORTA 

  
  		MOVLW     41H
  		MOVWF     ADCON0           ;select CLOCK is fosc/8,A/D enabled
  		CALL      DELAY           ;call delay program,ensure enough time to sampling
  		BSF       ADCON0,GO        ;startup ADC divert
WAIT1
  		BTFSS     PIR1,ADIF        ;is the convert have finished?
  		GOTO      WAIT1            ;wait for the convert finished
  		bcf		  PIR1, ADIF       ; Clear the A/D flag

MOVf ADRESH , 0
movwf	pulseA
MOVF PERV_VAL_A,W
SUBWF pulseA,W
BTFSS STATUS,Z
CALL DISPLAY_A
MOVF pulseA,W
MOVWF PERV_VAL_A
;###############################################################################

  		MOVLW     49H
  		MOVWF     ADCON0           ;select CLOCK is fosc/8,A/D enabled
  		CALL      DELAY           ;call delay program,ensure enough time to sampling
  		BSF       ADCON0,GO        ;startup ADC divert
WAIT2
  		BTFSS     PIR1,ADIF        ;is the convert have finished?
  		GOTO      WAIT2             ;wait for the convert finished
  		bcf		  PIR1, ADIF       ; Clear the A/D flag

    MOVf ADRESH , 0
	movwf	pulseB
    MOVF PERV_VAL_B,W
    SUBWF pulseB,W
    BTFSS STATUS,Z
    CALL DISPLAY_B
    MOVF pulseB,W
    MOVWF PERV_VAL_B


;####################################################################################
         
 Banksel   PORTA   

  		MOVLW     51H
  		MOVWF     ADCON0           ;select CLOCK is fosc/8,A/D enabled
  		CALL      DELAY           ;call delay program,ensure enough time to sampling
  		BSF       ADCON0,GO        ;startup ADC divert
WAIT3
  		BTFSS     PIR1,ADIF        ;is the convert have finished?
  		GOTO      WAIT3            ;wait for the convert finished
  		bcf		  PIR1, ADIF       ; Clear the A/D flag

MOVf ADRESH , 0
movwf	pulseC

MOVF PERV_VAL_C,W
SUBWF pulseC,W
BTFSS STATUS,Z
CALL DISPLAY_C
MOVF pulseC,W
 MOVWF PERV_VAL_C    	

;#######################################################################################
PWM_A 

	movlw	0xfa
	movwf	pulse
pulse_A
	bsf	PORTC,0             ;turn second on and wait for 1ms
	decfsz	pulse,1
	goto	pulse_A
servoA
	nop
	nop
	nop
	nop
	nop
	decfsz	pulseA,1             ;leave on for between 1-3ms
	goto	servoA
	bcf	PORTC,0 

;########################################################################################

PWM_B 

	movlw	0xfa
	movwf	pulse
pulse_B
	bsf	PORTC,1
	decfsz	pulse,1                      ;turn first on and wait for 1ms
	goto	pulse_B
servoB
	nop
	nop 
	nop
	nop
	nop
	decfsz	pulseB,1                        ;count down to make value
	goto	servoB                          ;between 1 and 3 ms
	bcf	PORTC,1
   
;########################################################################################

PWM_C 
	
    movlw	0xfa
	movwf	pulse
pulse_C
	bsf	PORTC,2
	decfsz	pulse,1                      ;turn first on and wait for 1ms
	goto	pulse_C
servoC
	nop
	nop 
	nop
	nop
	nop
	decfsz	pulseC,1                        ;count down to make value
	goto	servoC                          ;between 1 and 3 ms
	bcf	PORTC,2
    
	call	pause

	goto	start
;#####################################################################
pause
	movlw	0xff                  ;wait for the rest of the 17ms to pass
	movwf	timer2
LOOP2	movlw	0x12	
	movwf	timer1
LOOP1	decfsz	timer1,1
	goto	LOOP1
	decfsz	timer2,1
	goto	LOOP2	
	return
;###########################################################################
	DELAY
    BANKSEL PORTA
  	MOVLW    0x20                     ; delay program,ensure enough time to sampling
  	MOVWF    TEMP
L1 	DECFSZ   TEMP,1
  	GOTO     L1

  	RETURN
;############################################################################
CHANGE_To_BCD
gen_hunds
		MOVLW     .100	           ; sub 100,result keep in W
  	    SUBWF     TEMP,0
  		BTFSS     STATUS,C        ; judge if the result biger than 100
  		GOTO      gen_tens        ;; no,get the ten bit result
 		MOVWF     TEMP            ; yes,result keep in TEMP
  		INCF      hundreds,1      ; hundred bit add 1
  		GOTO      gen_hunds       ; continue to get hundred bit result
gen_tens
  		MOVLW     .10             ;;sub 10,result keep in W
  		SUBWF     TEMP,0          
  		BTFSS     STATUS,C       ; judge if the result biger than 10
 		GOTO      gen_ones        ;no,get the Entries bit result
  		MOVWF     TEMP            ;yes,result keep in TEMP
  		INCF      tens,1          ;ten bit add 1
  		GOTO      gen_tens        ;turn  to continue get ten bit
gen_ones
  		MOVF      TEMP,W
  		MOVWF     units             ; the value of Entries bit
 	 	RETURN
		
;#############################################################################
delay
movlw 0x10
movwf msd
clrf lsd
loop2                    ; delay program,ensure enough time to LCD
decfsz lsd,f
goto loop2
decfsz msd,f
endLcd
goto loop2
return
;############################################################################
send_cmd
movwf PORTD
 bcf PORTB, 0
bsf PORTB, 2 
nop                       ;  SEND COMMAND TO THE LCD
bcf PORTB, 2
bcf PORTB,1
call delay 
return
;##############################################################
send_char
movwf PORTD 
bsf PORTB,0
bsf PORTB,2
nop
bcf PORTB, 2                       ;  SEND CHARACTER TO THE LCD
bcf PORTB, 1
call delay 
return

;######################################################################################
DISPLAY_A                          ;     SHOW SERVO A ANGELE ON LCD  
clrf hundreds                               
 clrf tens                          ; CLEAR TEMPORARY REGESTERS
  clrf units
    clrf    TEMP 
     clrf    MUL_counter
      clrf    T_A                                
        MOVLW     0X82                ;SHOW '+' AS DEAFULT
         CALL      send_cmd
          movlw     '+'
           CALL      send_char
;*******
       
       
        MOVF pulseA,W            ;CALCULATE CORRECT ANGLE WITHIN THE RANGE -180_180 FROM THIS EQUATION :
                                                       ;(MAX-MIN)*(ADCREAD/255)+MIN
                                            ;ANGELE =     360*(pulseA/255)-180        
        CALL DIVISION_BY17                  ;360/255=24/17
   
        movwf	T_A                    ;   T_A TEMP REG
    	movlw	.23                    ;MUL BY 24
    	movwf	MUL_counter
    	movf	T_A, w          
 addA
    	addwf	T_A, w
	    decfsz	MUL_counter, f
    	goto	addA
        MOVWF   RESULTA
        MOVLW  .180                        
        SUBWF  RESULTA,w                  ; SUBTRACT RESULT BY 180
      
        BTFSS STATUS,C                    ; CHECK IF IT'S NEGATIVE OR POSITIVE 
      
        CALL  NEG_SIGN_A                  ; 2'S COMPLEMENT IF NEGATIVE RESULT AND SHOW '-' SIGN
       
      
        movwf TEMP
        CALL  CHANGE_To_BCD                    ;CHANGE THE RESULT TO BCD 
		
       movlw 83h
        Call send_cmd
        MOVF hundreds,W
        ADDLW 30h
        Call send_char                        ;SHOW THE HUNDREDS
		
        movlw 84h
        Call send_cmd
        MOVF tens,W                            ;SHOW THE TENS
        ADDLW 30h
        Call send_char
		
        movlw 85h
        Call send_cmd                          
        MOVF units ,W                         ;SHOW THE UNITS
        ADDLW 30h
        Call send_char
        
RETURN
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
DISPLAY_B                        ;     SHOW SERVO B ANGELE ON LCD 
clrf hundreds
 clrf tens                            ; CLEAR TEMPORARY REGESTERS
  clrf units
    clrf    TEMP
     clrf    MUL_counter
      clrf    T_B
        MOVLW     0X8D
         CALL      send_cmd            ;SHOW '+' AS DEAFULT
          movlw     '+'
           CALL      send_char
;*******
        MOVF pulseB,W          ;CALCULATE CORRECT ANGLE WITHIN THE RANGE -60_60 FROM THIS EQUATION :
                                                       ;(MAX-MIN)*(ADCREAD/255)+MIN
                                             ;ANGELE =     120*(pulseB/255)-60 
        CALL DIVISION_BY17            ;120/255=8/17
   
        movwf	T_B            ;   T_B TEMP REG
    	movlw	.7                    ;MUL BY 8
    	movwf	MUL_counter
    	movf	T_B, w          
 addB
    	addwf	T_B, w
	    decfsz	MUL_counter, f
    	goto	addB
        MOVWF   RESULTB
        MOVLW  .60 
        SUBWF  RESULTB,w                         ; SUBTRACT RESULT BY 60
      
        BTFSS STATUS,C                            ; CHECK IF IT'S NEGATIVE OR POSITIVE 
      
        CALL  NEG_SIGN_B                       ; 2'S COMPLEMENT IF NEGATIVE RESULT AND SHOW '-' SIGN
       
        movwf TEMP
        CALL  CHANGE_To_BCD                  ;CHANGE THE RESULT TO BCD 
		
        movlw 8Eh
        Call send_cmd                 
        MOVF tens,W                            ;SHOW THE TENS
        ADDLW 30h
        Call send_char
		
        movlw 8Fh
        Call send_cmd 
        MOVF units ,W                        ;SHOW THE UNITS
        ADDLW 30h
        Call send_char

RETURN
;###############################################################################
DISPLAY_C                          ;     SHOW SERVO C ANGELE ON LCD 
clrf hundreds
 clrf tens
  clrf units                         ; CLEAR TEMPORARY REGESTERS
   clrf    TEMP
    clrf    MUL_counter
      clrf    T_C
        MOVLW     0XC8
         CALL      send_cmd                    ;SHOW '+' AS DEAFULT
          movlw     '+'
           CALL      send_char
;*******
        MOVF pulseC,W          
                                              ;CALCULATE CORRECT ANGLE WITHIN THE RANGE 0_30 FROM THIS EQUATION :
                                                       ;(MAX-MIN)*(ADCREAD/255)+MIN
                                             
       CALL DIVISION_BY17               ;ANGELE =     30*(pulseC/255) 
                             ;30/255=2/17
   
        movwf	T_C           ;T_ TEMP REG
        BCF STATUS,C
        rlf   T_C,1           ; Mul by 2

                     
        movf T_C,w
        movwf TEMP
        CALL  CHANGE_To_BCD                           ;CHANGE THE RESULT TO BCD 
		
        movlw     0XC9
        Call     send_cmd
        MOVF tens,W                                      ;SHOW THE TENS
        ADDLW 0X30
        Call send_char
		
        movlw 0XCA
        Call send_cmd 
        MOVF units ,W                                ;SHOW THE UNITS
        ADDLW 0X30 
        Call send_char
RETURN
;#####################################################################################
DIVISION_BY17                     ;DIVISION_BY17        
 BANKSEL PORTA
         MOVWF TMP
REPEATED_SUB17
       MOVLW     .17      
  		SUBWF     TMP ,W          
  		BTFSS     STATUS,C     
 		GOTO      FINISH17           ;REPEATED_SUB 17 TIMES
  		MOVWF     TMP      
  		INCF      COUNTER17,1          
  		GOTO     REPEATED_SUB17

     FINISH17
      MOVF COUNTER17,W
      clrf    COUNTER17
     

      
      RETURN

     


;#####################################################################
NEG_SIGN_A              
movwf  TEMPN
COMF   TEMPN ,f         ;COMPLEMENT ALL BITS (ONE'S COMPLEMENT)
INCF   TEMPN ,f                   ;ADD ONE TO ONE'S COMPLEMENT
MOVLW     0X82
CALL      send_cmd              ;SHOW '-' SIGN
movlw     '-'
CALL      send_char
movf  TEMPN,w                       ;   TWO'S COMPLEMENT
CLRF TEMPN
RETURN
;#####################################################################
NEG_SIGN_B
movwf  TEMPN
COMF   TEMPN ,f                 ;COMPLEMENT ALL BITS (ONE'S COMPLEMENT)
INCF   TEMPN ,f                 ;ADD ONE TO ONE'S COMPLEMENT
MOVLW     0X8d
CALL      send_cmd              ;SHOW '-' SIGN
movlw     '-'
CALL      send_char
movf  TEMPN,w                   ;   TWO'S COMPLEMENT
CLRF TEMPN
RETURN

END

