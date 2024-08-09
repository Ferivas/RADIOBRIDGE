'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  RBrdg_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile
$projecttime = 47


'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Espera(byval Tespera As Word)
Declare Sub Gentrama(byval Tipotram As Byte)
'Declare Sub Rxdtmf()                                        'Recibe tonos DTMF
Declare Sub Txdtmf()                                        'Transmite tonos DTMF
Declare Sub Leersta()
Declare Sub Leeradc()
Declare Sub Defaultvalues()
Declare Sub Procgps()
Declare Sub Procdtmf()
Declare Sub Proctest()
Declare Sub Procmodo()
Declare Sub Txslaves()
Declare Sub Txalerta()
Declare Sub Txtest()
Declare Sub Txnormal()
Declare Sub Procmodbusreg()
Declare Sub Vertout()
Declare Sub Vercfg()
Declare Sub Txmsg()

'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte
Dim Tmpb2 As Byte
Dim Tmpb3 As Byte
Dim Tmpb4 As Byte
Dim Tmpb5 As Byte
Dim Tmpcntrsta As Byte
Dim Tmpl As Long
Dim Enabug As Byte
Dim Tmpprg As Byte

Dim Iniprog As Bit

Dim Ptrhdr As Byte
Dim Ptrbhdr As Byte
Dim Tmphdr As Byte
Dim Inileerhr As Bit
Dim Tonotest As Byte

Dim Modooff As Byte
Dim Modooffant As Byte
Dim Newseg As Bit

'Dim Modopinsta As Bit

'Dim Broadcast As Bit
Dim Inibroadcast As Bit
Dim Newbroadcast As Bit
Dim Cntrtxbroadcast As Byte

Dim Cntrseg2 As Byte

Dim Newtst As Bit
Dim Statst As Byte                                          ' Numero de estacion de prueba
Dim Estadotst As Byte                                       ' Estado de prueba
Dim Tmpstatus As Byte

Dim Statusrx As Byte
dim Inivariables as bit


Dim J As Byte , Jt As Byte , Kt As Byte
Dim Tmpw As Word

Dim Tmpdw As Dword
Dim Bdw1 As Byte At Tmpdw Overlay
Dim Bdw2 As Byte At Tmpdw + 1 Overlay
Dim Bdw3 As Byte At Tmpdw + 2 Overlay
Dim Bdw4 As Byte At Tmpdw + 3 Overlay

Dim Tmps As Single
Dim Bs1 As Byte At Tmps Overlay
Dim Bs2 As Byte At Tmps + 1 Overlay
Dim Bs3 As Byte At Tmps + 2 Overlay
Dim Bs4 As Byte At Tmps + 3 Overlay


Dim Cmdtmp As String * 6
Dim Atsnd As String * 160
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 82

Dim Trqststa As Word                                        'Tiempo de consulta a estaciones
Dim Trqs As Word

Dim Trqststaeep As Eram Word
Dim Cntrseg As Word

Dim Trqsnormal As Word                                      ' Tiempo de consultas periodico a estaciones
Dim Trqsnormaleep As Eram Word

Dim Trqsalarma As Word                                      ' Tiempo de consultas modo Alarma/Test a estaciones
Dim Trqsalarmaeep As Eram Word


Dim Cntrestaciones As Byte

Dim Cntrtx As Single                                        'Contador de transmisiones
Dim Cntrtxeep As Eram Single

Dim Tbl_adc1(numsta) As Single                              ' Para almacenar los valores de voltaje bateria de la estaciones remotas
Dim Tbl_adc2(numsta) As Single                              ' Para almacenar los valores de voltaje panel de la estaciones remotas
Dim Tbl_adc3(numsta) As Single

Dim Tbl_stain(numsta) As Byte                               ' Almacena estado de entrada digital
Dim Tbl_staout(numsta) As Byte                              ' Almacena estado de salida digital

Dim Tbl_status(numsta) As Byte                              ' Almacena estado de las estaciones (estatus y errores)
Dim Tbl_ctx(numsta) As Dword
Dim Tbl_cntrtout(numsta) As Word                            'Contador Timeout estaciones


Dim Status As Byte                                          'Almacena estado
Dim Statusant As Byte
Dim Tramatx(12) As Byte                                     'Trama de transmision
Dim Tramatxdtmf(24) As Byte                                 ' Alamcena los codigos para generar los DTMF

Dim Tonodtmf As Byte
Dim Ptrrxdtmf As Byte
Dim Tbl_rxdtmf(32) As Byte                                  'Tabla para alamcenar tonos recibidos
Dim Tbl_rxhex(16) As Byte

Dim Regb As Byte
Dim Newdv As Bit

Dim Lecsta As Byte

Dim Newtxalerta As Bit                                      'Bit para generar Tx de Alerta Naranja a todas las estaciones
Dim Newtxtest As Bit
Dim Newtxnormal As Bit

Dim D0ant As Bit                                            'Para verificar persistencia del dato
Dim D1ant As Bit


'Dim Cntrtxbroadcast As Byte


'Variables TIMER0
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim Cntrt0 As Byte
Dim Newtx As Bit                                            'Indica nueva tx
Dim Newtxper As Bit

Dim Cntrtxper As Word

'TIMER2
Dim Inidelay As Bit
Dim Cntrdelay As Word
Dim T0delay As Bit
Dim Topdelay As Word

Dim Cntrcrcok As Word
Dim Cntrcrcokeep As Eram Word

Dim Cntrcrcbad As Word
Dim Cntrcrcbadeep As Eram Word

Dim Inirst As Bit
Dim Cntrrst As Word

Dim Cntractremota As Word
Dim Cntractremotaeep As Eram Word

Dim Tmpltime As Long
Dim Horamin As Long
Dim Horamineep As Eram Long
Dim Lsyssec As Long

dim Cntrmodooff as word

'ADC
Dim Adccntr(numadc) As Single
Dim Cntrsmpl As Byte
Dim Adc_hex(numadc) As Single
Dim Tmpwadc As Word
Dim Adcn As Byte
Dim Adct As Byte
Dim Adck(numadc) As Single
Dim Adckeep(numadc) As Eram Single
Dim Adcval(numadc) As Single
Dim Newadc As Bit

Dim Vbat As Single
Dim Vps As Single

Dim Vbat1 As Byte At Vbat Overlay
Dim Vbat2 As Byte At Vbat + 1 Overlay
Dim Vbat3 As Byte At Vbat + 2 Overlay
Dim Vbat4 As Byte At Vbat + 3 Overlay

Dim Vps1 As Byte At Vps Overlay
Dim Vps2 As Byte At Vps + 1 Overlay
Dim Vps3 As Byte At Vps + 2 Overlay
Dim Vps4 As Byte At Vps + 3 Overlay
Dim Dispbug As Byte
Dim Cntripadc As Word

'************************************************************
' Variables definitions
'************************************************************


Dim Coil_status_table(5) As Byte                            ' One more than number_of_coils/8
Dim Discrete_inputs_table(6) As Byte                        ' One more than number_of_inputs/8
Dim Holding_registers_table(72) As Word
Dim Input_registers_table(32) As Word


Dim Dir_slave As Byte                                       'Mantiene la dirección del esclavo
Dim Dir_slave_eep As Eram Byte

' Hora GPS
Dim Timeini As Long                                         'Almacena hora de inicio en secofday
Dim Timeinieep As Eram Long

Dim Timefin As Long                                         'Almacena hora de inicio en secofday
Dim Timefineep As Eram Long

Dim Volumen As Byte                                         'Para indicar intensidad de la señal de audio en la salida de driver de audio
Dim Cntrini As Word
Dim Cntrinieep As Word
Dim Gpsnew As Bit
Dim Trygps As Byte
Dim Hora As String * 10
Dim Fecha As String * 10
Dim Gpsproc As String * 160

Dim Tactclk As Long
Dim Iniactclk As Bit
Dim Tactclkeep As Eram Long
Dim Tmptout As Word
Dim Toptout As Word
Dim Toptouteep As Eram Word

Dim Cfgokeep As Eram Byte
Dim Cfgok As Byte

Dim Msgtmp As Byte
Dim Newmsg As Bit
'

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(32) As String * 16
Dim Serdata As String * 160 , Serrx As Byte , Serproc As String * 160

'Variables SERIAL1
Dim Ser_ini1 As Bit , Sernew1 As Bit
Dim Serdata1 As String * 64 , Serrx1 As Byte , Serproc1 As String * 64



'*******************************************************************************
'* END public part                                                             *
'*******************************************************************************


Goto Loaded_arch

'*******************************************************************************
' INTERRUPCIONES
'*******************************************************************************

'*******************************************************************************
' Subrutina interrupcion de puerto serial 1
'*******************************************************************************
At_ser1:
   Serrx = Udr

   Select Case Serrx
      Case "$":
         Ser_ini = 1
         Serdata = ""

      Case 13:
         If Ser_ini = 1 Then
            Ser_ini = 0
            Serdata = Serdata + Chr(0)
            Serproc = Serdata
            Sernew = 1
            Enable Timer0
         End If

      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If

   End Select

Return


'*******************************************************************************
' Subrutina interrupcion de puerto serial 2
'******************************************S*************************************

At_ser2:
   Serrx1 = Udr1

   Select Case Serrx1
      Case "$":
         Ser_ini1 = 1
         Serdata1 = ""

      Case 13:
         If Ser_ini1 = 1 Then
            Ser_ini1 = 0
            Serdata1 = Serdata1 + Chr(0)
            Serproc1 = Serdata1
            Sernew1 = 1
            Enable Timer0
         End If

      Case Is > 31
         If Ser_ini1 = 1 Then
            Serdata1 = Serdata1 + Chr(serrx1)
         End If

   End Select

Return


'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = &H64
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      'Toggle Iluminar
      Led1 = Iluminar
      Incr Num_ventana
   End If

   If Inidelay = 1 Then
      Incr Cntrdelay
      If Cntrdelay = Topdelay Then
         Set T0delay
         Reset Inidelay
      End If
   End If

   If Inirst = 1 Then
      'Reset Inirst
      'Set Pinbug
      Incr Cntrrst
      'Cntrrst = Cntrrst Mod 420
      Cntrrst = Cntrrst Mod 500
      If Cntrrst = 0 Then
         Ptrrxdtmf = 0
         Regb = 0
         Reset Inirst
'         Reset Pinbug
      End If
   End If

Return


'*******************************************************************************
' TIMER1
'*******************************************************************************
Int_timer1:
   Timer1 = &H0BDC                                          ' Ints a 1 HZ si Prescale=256
'   Lsyssec = Syssec()
   Lsyssec = Syssec(time$ , Date$)
   Incr Lsyssec
   Time$ = Time(lsyssec)
   Date$ = Date(lsyssec)
   Set Newadc
   Set Newseg
   'Incr Cntrtxper

   Tmpltime = Lsyssec Mod Trqs
   If Tmpltime = 0 Then
      Set Newtxper
      'Incr Cntrtx
   End If

   Tmpltime = Lsyssec - 900
   Tmpltime = Tmpltime Mod Tactclk
   If Tmpltime = 0 Then
      Set Iniactclk
   End If

   If Newtxper = 1 Then
      'Incr Cntrseg
      Tmpltime = Lsyssec Mod Trqststa
      If Tmpltime = 0 Then
         Set Newtx
      End If

   End If

   If Inibroadcast = 1 Then
      Incr Cntrseg2
      Cntrseg2 = Cntrseg2 Mod 10
      If Cntrseg2 = 0 Then
         Set Newbroadcast
      End If
   End If
Return


'*******************************************************************************
' TIMER 2
'*******************************************************************************
Settime:
Return

Getdatetime:
Return

Setdate:
Return

Sectic:
   'Tmpltime = Syssec()
   'Incr Tmpltime
   'Time$ = Time(tmpltime)
   'Date$ = Date(tmpltime)
Return


Pcint_int:
   Incr Regb
   If Dv = 1 Then
      Set Ledrx
   Else
      Reset Ledrx
   End If
   'If Dv = 1 And Regb.0 = 1 Then
   If Dv = 1 Then
      Tonodtmf = Pind
      Shift Tonodtmf , Right , 4
'      Print #1 , "T=" ; Hex(tonodtmf)
      'Toggle Pinbug
      Incr Ptrrxdtmf
      Tbl_rxdtmf(ptrrxdtmf) = Tonodtmf
      Ptrrxdtmf = Ptrrxdtmf Mod 30
      If Ptrrxdtmf = 1 Then
         Set Inirst
      End If
      'Set Newdv
      If Ptrrxdtmf = 0 Then
         Set Newdv
      End If
   End If

Return

'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************

'*******************************************************************************
' Inicialización de variables
'*******************************************************************************
Sub Inivar()
   Reset Led1
   Reset Ptt
   Set Ce
   'Reset Enatx
   Statusrx = 1
   Print #1 , "SAT MASTER 2024"
   Print #1 , Version(1)
   Print #1 , Version(2)
   Print #1 , Version(3)
   Estado_led = 1
   Status = 1

   Trqststa = Trqststaeep
   Print #1 , "Tiempo de consulta a estaciones en seg =" ; Trqststa

   Trqsnormal = Trqsnormaleep
   Print #1 , "Tiempo de consulta modo Normal en seg =" ; Trqsnormal

   Trqsalarma = Trqsalarmaeep
   Print #1 , "Tiempo de consulta modo Alarma/test en seg =" ; Trqsalarma

   Cntrtx = Cntrtxeep
   Print #1 , "Contador de transmisiones =" ; Cntrtx

   Coil_status_table(1) = 0
   Coil_status_table(2) = 0
   Coil_status_table(3) = 0
   Coil_status_table(4) = 0
   Coil_status_table(5) = 0

   For Tmpb = 1 To 32
      Holding_registers_table(tmpb) = 0
      Input_registers_table(tmpb) = 0
   Next Tmpb

   Input_registers_table(numregtimeout) = &B0000111111111111

   Dir_slave = Dir_slave_eep
   Print #1 , "Dir_Slave=" ; Dir_slave
   Trqs = Trqsnormal

   'Set Newtxper

   Tonotest = 0

   Cntrcrcok = Cntrcrcokeep
   Print #1 , "CNTRCRCOK=" ; Cntrcrcok

   Cntrcrcbad = Cntrcrcbadeep
   Print #1 , "CNTRCRCBAD=" ; Cntrcrcbad

   Print #1 , "MODO OFF" ; Modooff

   Cntractremota = Cntractremotaeep
   Print #1 , "Cntractremota=";cntractremota
   Input_registers_table(13) = Cntractremota

   Horamin = Horamineep
   Print #1 , Time(horamin)
   Print #1 , Date(horamin)
   Time$ = Time(horamin)
   Date$ = Date(horamin)

   Print #1 , "Ultima Act. CLK"
   Print #1 , Date$
   Print #1 , Time$

   Cntripadc = Tactadc_10

   Timeini = Timeinieep
   Timefin = Timefineep
   Print #1 , "TIMEINI=" ; Timeini ; ",";
   Tmpstr52 = Time(timeini)
   Print #1 , Tmpstr52
   Print #1 , "TIMEFIN=" ; Timefin ; ",";
   Tmpstr52 = Time(timefin)
   Print #1 , Tmpstr52
   Modooffant = 99
   Cntrini = Cntrinieep
   Incr Cntrini
   Print #1 , "Cntrini=" ; Cntrini
   Tactclk = Tactclkeep
   Print #1 , "TactCLK=" ; Tactclk
   Toptout = Toptouteep
   Print #1 , "Toptout=" ; Toptout
   For Adcn = 1 To Numadc
      Adck(adcn) = Adckeep(adcn)
      Print #1 , "Adck(" ; Adcn ; ")=" ; Adck(adcn)
   Next



End Sub


Sub Vercfg()

   Print #1 , "Verificar Config. Inicial"
   Cfgok = Cfgokeep
   Tmpb2 = 0
   Estado_led = 8
   Do
      If Sernew = 1 Then                                       'DATOS SERIAL 1
         Reset Sernew
         Print #1 , "SER1=" ; Serproc
         Call Procser()
         Cfgok = Cfgokeep
      End If
      If Newseg = 1 Then
         Reset Newseg
         Incr Tmpb
         Incr Tmpb2
         Tmpb = Tmpb Mod 10
         If Tmpb = 0 Then
            Print #1 , "Ingrese valores de configuracion"
         End If
      End If
   Loop Until Cfgok = 1                                     'Cfgok = 1
   Print #1 , "CONFIG. INICIAL OK"
   Estado_led = 1

End Sub

' Subrutina para generar espera
'*******************************************************************************
Sub Espera(byval Tespera As Word)
   Topdelay = Tespera
   Cntrdelay = 0
   Reset T0delay
   Set Inidelay
   While T0delay = 0
      Reset Watchdog
   Wend

End Sub


Sub Gentrama(byval Tipotram As Byte)

   Tramatx(1) = &H24                                        '$ como identificador de cabecera
   If Tipotram = 0 Then
      Tramatx(2) = &H55                                     'Identificación del master
   End If

   If Tipotram = 1 Then
      Msgtmp = Msgtmp And &H0F
      Print #1 , Hex(msgtmp)

      Tramatx(2) = Msgtmp Or &HF0                           'Identificación del master
      Print #1 , Hex(tramatx(2))
   End If

   Tramatx(3).0 = Cntrestaciones.0
   Tramatx(3).1 = Cntrestaciones.1
   Tramatx(3).2 = Cntrestaciones.2
   Tramatx(3).3 = Cntrestaciones.3
   Tramatx(3).4 = Status.0
   Tramatx(3).5 = Status.1
   Tramatx(3).6 = Status.2

   If Modooff = 0 Then
      Tramatx(3).7 = 0
   Else
      Tramatx(3).7 = 1
   End If

   Tramatx(4) = Volumen

   Tmps = Cntrtx
   Tramatx(5) = Bs1
   Tramatx(6) = Bs2
   Tramatx(7) = Bs3
   Tramatx(8) = Bs4

   Tmpw = Crc16(tramatx , 8)
   Tramatx(9) = High(tmpw)
   Tramatx(10) = Low(tmpw)


   'Tramatxdtmf(1) = Cntrestaciones
'Tramatxdtmf(2) = Status

                                                           '   If Modooff = 0 Then
'      Tramatxdtmf(2).3 = 0
'   Else
'      Tramatxdtmf(2).3 = 1
'   End If


   Tmpb4 = 1
   For Tmpb = 0 To 9
      Tmpb2 = Tmpb + 1
      Tmpb3 = Tramatx(tmpb2)
      'Print #1 , "Tramatx(" ; Tmpb2 ; ")=" ; Hex(tmpb3) ; ";"
      Tramatxdtmf(tmpb4) = Tmpb3 And &H0F
      'Print #1 , "Tramatxdtmf(" ; Tmpb4 ; ")=" ; Hex(tramatxdtmf(tmpb4)) ; ","
      Incr Tmpb4
      Shift Tmpb3 , Right , 4
      Tramatxdtmf(tmpb4) = Tmpb3
      'Print #1 , "Tramatxdtmf(" ; Tmpb4 ; ")=" ; Hex(tramatxdtmf(tmpb4)) ; ","
      Incr Tmpb4
   Next

'(
   If Modooff = 0 Then
      Tramatxdtmf(6).3 = 0
   Else
      Tramatxdtmf(6).3 = 1
   End If
')


End Sub


'*******************************************************************************
' Subrutina de Tx de datos DTMF
'*******************************************************************************
Sub Txdtmf()

   Set Ptt
   Call Espera(100)
   For J = 1 To 20
      'Print #1 , J ; "," ; Hex(tramatxdtmf(j))
      Dout0 = Tramatxdtmf(j).0
      Dout1 = Tramatxdtmf(j).1
      Dout2 = Tramatxdtmf(j).2
      Dout3 = Tramatxdtmf(j).3
      Set Ledtx
      Reset Ce
      Call Espera(8)
      Reset Ledtx
      Set Ce
      Call Espera(4)
   Next

   Reset Ptt

End Sub



'*******************************************************************************
' Subrutina de rx de datos DTMF
'*******************************************************************************
'Sub Rxdtmf()

'   If Dv = 1 Then
'      Tonodtmf = Pind
'      Shift Tonodtmf , Right , 4
'      Print #1 , "T=" ; Hex(tonodtmf)
'      Incr Ptrrxdtmf
'      Tbl_rxdtmf(ptrrxdtmf) = Tonodtmf
'      Ptrrxdtmf = Ptrrxdtmf Mod 12
'      If Ptrrxdtmf = 0 Then
'         Print #1 , "rx="
'         For J = 1 To 12
'            Print #1 , J ; "," ; Hex(tbl_rxdtmf(j))
'         Next
'      End If
'   End If

'End Sub


'*******************************************************************************
' Subrutina para leer valores de entrada
'*******************************************************************************
Sub Leersta()
'If Statusrx = 1 Then
   Lecsta = 0
   Lecsta.0 = D0
   Lecsta.1 = D1

   'Lecsta.0 = Lecsta.0 Xor 1
   'Lecsta.1 = Lecsta.1 Xor 1

   If Lecsta.0 <> D0ant Then
      Print #1 , "D0=" ; Lecsta.0
      'Print #1 , Time$ ; ";" ; Date$
      Call Espera(100)                                      'Para probar en 2 segundos si hay cambio de estado
      If D0 = Lecsta.0 Then                                 ' Se verifica cambio de estado de entrada
         D0ant = Lecsta.0
         Print #1 , "D0 VAL"
      Else
         Print #1 , "D0 NO VAL"
         Lecsta.0 = D0
      End If
   End If

   If Lecsta.1 <> D1ant Then
      Print #1 , "D1=" ; Lecsta.1
      'Print #1 , Time$ ; ";" ; Date$
      Call Espera(100)                                      'Para probar en 2 segundos si hay cambio de estado
      If D1 = Lecsta.1 Then                                 ' Se verifica cambio de estado de entrada
         D1ant = Lecsta.1
         Print #1 , "D1 VAL"
      Else
         Print #1 , "D1 NO VAL"
         Lecsta.1 = D1
      End If
   End If

   Lecsta.0 = Lecsta.0 Xor 1
   Lecsta.1 = Lecsta.1 Xor 1

   Select Case Status
      Case 1:                                               'Normal
         If Lecsta = 2 Then
            Print #1 , "Paso a modo test"
            'Print #1 , Time$ ; ";" ; Date$
            Status = 2
            Estado_led = 2
            Trqs = Trqsalarma
            Set Newtxtest
            Reset Newtxnormal
            Reset Newtxalerta
            Reset Modooff
            Set Inibroadcast
            Set Newbroadcast
            Cntrtxbroadcast = 0
            Reset Newtxper
            Reset Newtx

         End If

         If Lecsta = 0 Or Lecsta = 1 Then
            Print #1 , "Paso a modo Alerta Naranja"
            'Print #1 , Time$ ; ";" ; Date$
            Status = 3
            Estado_led = 3
            Set Newtxalerta
            Reset Newtxnormal
            Reset Newtxtest
            Cntrtxbroadcast = 0
            Reset Newtxper
            Reset Newtx


            Trqs = Trqsalarma
            Reset Modooff
            Set Inibroadcast
            Set Newbroadcast
         End If


      Case 2:                                               'Test
         If Lecsta = 3 Then
            Print #1 , "A Normal desde Test"
            'Print #1 , Time$ ; ";" ; Date$
            Status = 1
            'Estado_led = 1
            Trqs = Trqsnormal
            Set Newtxnormal
            Reset Newtxalerta
            Reset Newtxtest
            Cntrtxbroadcast = 0
            Reset Newtxper
            Reset Newtx


            If Modooff = 1 Then
               Estado_led = 11
            Else
               Estado_led = 1
            End If
'(
            If Modopin = 0 Then
               Set Modooff
               Estado_led = 11
            Else
               Reset Modooff
               Estado_led = 1
            End If
')
            Set Inibroadcast
            Set Newbroadcast

         End If

      Case 3:                                               'Naranja
         If Lecsta = 3 Then
            Print #1 , "A Normal desde A. Naranja"
            'Print #1 , Time$ ; ";" ; Date$
            Status = 1
'            Estado_led = 1
            Trqs = Trqsnormal
            Set Newtxnormal
            Reset Newtxalerta
            Reset Newtxtest
            Cntrtxbroadcast = 0
            Reset Newtxper
            Reset Newtx


            If Modooff = 1 Then
               Estado_led = 11
            Else
               Estado_led = 1
            End If

'(
            If Modopin = 0 Then
               Set Modooff
               Estado_led = 11
            Else
               Reset Modooff
               Estado_led = 1
            End If
')
            Set Inibroadcast
            Set Newbroadcast

         End If

   End Select


'Else
'      Status = Statusrx
'      Estado_led = Status

'End If



End Sub



'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   Print #1 , Serproc
   'Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   'Print #1 , "numpar=" ; Numpar
'   If Numpar > 0 Then
'      For Tmpb = 1 To Numpar
'         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
'      Next
'   End If

   If Len(cmdsplit(1)) = 6 Then
      Cmdtmp = Cmdsplit(1)
      Cmdtmp = Ucase(cmdtmp)
      Cmderr = 255
      Select Case Cmdtmp
         Case "LEEVFW"
            Cmderr = 0
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">, Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"

         Case "RSTVAR"
            Cmderr = 0
            Call Defaultvalues()
            Set Inivariables

         Case "SETCFG"
            If Numpar = 2 Then
               Cfgok = Val(cmdsplit(2))
               Cfgokeep = Cfgok
               Cmderr = 0
               Atsnd = "Se configura CFGOK=" + Str(cfgok)
            Else
               Cmderr = 4
            End If

         Case "SETLED"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 17 Then
                  Cmderr = 0
                  Atsnd = "Se configura setled a " + Str(tmpb)
                  Estado_led = Tmpb
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETTXN"                                      ' Tiempo de consulta a estaciones en modo normal
            If Numpar = 2 Then
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               Trqsnormal = Tmpw
               Trqsnormaleep = Tmpw
               Trqs = Trqsnormal
               Atsnd = "Se configuro tiempo de consulta modo Normal a " + Str(trqsnormal) + " s"
            Else
               Cmderr = 4
            End If

         Case "SETTXA"                                      ' Tiempo de consulta a estaciones en modo Alarma/Test
            If Numpar = 2 Then
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               Trqsalarma = Tmpw
               Trqsalarmaeep = Tmpw
               Trqs = Trqsalarma
               Atsnd = "Se configuro tiempo de consulta modo Alarma/Test a " + Str(trqsalarma) + " s"
            Else
               Cmderr = 4
            End If

         Case "SETTTE"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               Trqststa = Tmpw
               Trqststaeep = Tmpw
               Atsnd = "Se configuro tiempo de tx entre estaciones a " + Str(trqststa) + " s"
            Else
               Cmderr = 4
            End If

         Case "SETCTX"
            If Numpar = 2 Then
               Cmderr = 0
               Tmps = Val(cmdsplit(2))
               Cntrtx = Tmps
               Cntrtxeep = Tmps
               Atsnd = "Se configuro contador tx=" + Str(cntrtx)
            Else
               Cmderr = 4
            End If

         Case "SETCAR"                                      ' Tiempo de consulta a estaciones en modo Alarma/Test
            If Numpar = 2 Then
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               Cntractremota = Tmpw
               Cntractremotaeep = Tmpw
               Atsnd = "Se configuro No de consulta remotas " + Str(cntractremota)
            Else
               Cmderr = 4
            End If

         Case "LEETXN"
            Cmderr = 0
            Atsnd = "Tiempo de consultas modo Normal =" + Str(trqsnormal) + " s"

         Case "LEETXA"
            Cmderr = 0
            Atsnd = "Tiempo de consultas modo Alarma/Test =" + Str(trqsalarma) + " s"

         Case "LEETTE"
            Cmderr = 0
            Atsnd = "Tiempo de tx entre estaciones =" + Str(trqststa) + " s"

         Case "LEECTX"
            Cmderr = 0
            Atsnd = "Contador de tx=" + Str(cntrtx)

         Case "LEECAR"
            Cmderr = 0
            Atsnd = "Contador de tx=" + Str(cntractremota)

         Case "TSTDEL"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               Atsnd = "Test Delay  " + Str(tmpw)
               Print #1 , "INI"
               Call Espera(tmpw)
               Print #1 , "FIN"
            Else
               Cmderr = 4
            End If

         Case "INITXP"
            Cmderr = 0
            Set Newtxper
            Atsnd = "Nueva tx Periodica"

         Case "INIPRG"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  Cmderr = 0
                  If Tmpb = 0 Then
                     Reset Iniprog
                  Else
                     Set Iniprog
                  End If
                  Atsnd = "Se config INIPRG a " + Str(tmpb)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETDIR"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               Cmderr = 0
               Dir_slave = Tmpb
               Dir_slave_eep = Dir_slave
               Atsnd = "Se configuro direccion de esclavo a " + Str(dir_slave)
            Else
               Cmderr = 4
            End If

         Case "LEEDIR"
            Cmderr = 0
            Atsnd = "Direccion de escalvo RTU " + Str(dir_slave)

         Case "RXDTMF"
            Cmderr = 0
            If Numpar = 31 Then
               Cmderr = 0
               Atsnd = "Simulacion de RX tonos DTMF"
               Tmpb = 0
               For J = 2 To 31
                  Incr Tmpb
                  Tmpb2 = Hexval(cmdsplit(j))
                  Tbl_rxdtmf(tmpb) = Tmpb2
               Next
               Set Newdv
            Else
               Cmderr = 4
            End If

         Case "LEEHDR"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb <= 72 Then
                  Cmderr = 0
                  Tmpw = Holding_registers_table(tmpb)
                  Atsnd = "HDR" + Str(tmpb) + "=" + Hex(tmpw)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETHDR"
            Cmderr = 0
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb <= 72 Then
                  Cmderr = 0
                  Tmpw = Hexval(cmdsplit(3))
                  Holding_registers_table(tmpb) = Tmpw
                  Atsnd = "Se configuro HDR" + Str(tmpb) + "=" + Hex(tmpw)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEERHR"
            Cmderr = 0
            Set Inileerhr
            Atsnd = "Leer HDR"

         Case "SETTON"
            If Numpar = 2 Then
               Cmderr = 0
               Tonotest = Val(cmdsplit(2))
               Atsnd = "Se configuro tono de prueba a=" + Str(tonotest)
            Else
               Cmderr = 5
            End If

         Case "SETCOK"
            If Numpar = 2 Then
               Cmderr = 0
               Cntrcrcok = Val(cmdsplit(2))
               Cntrcrcokeep = Cntrcrcok
               Atsnd = "Se configura CNTRCRCOK=" + Str(cntrcrcok)
            Else
               Cmderr = 4
            End If

         Case "SETBAD"
            If Numpar = 2 Then
               Cmderr = 0
               Cntrcrcbad = Val(cmdsplit(2))
               Cntrcrcbadeep = Cntrcrcbad
               Atsnd = "Se configura CNTRCRCBAD=" + Str(cntrcrcbad)
            Else
               Cmderr = 4
            End If

         Case "LEECRC"
            Cmderr = 0
            Atsnd = "CRCOK=" + Str(cntrcrcok) + " , CRCBAD=" + Str(cntrcrcbad)

         Case "SETMOD"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  Cmderr = 0
                  If Tmpb = 1 Then
                     Set Modooff
                     Atsnd = "Se configuro modo OFF=" + Str(modooff)
                  Else
                     Reset Modooff
                     Atsnd = "Se configuro modo OFF=" + Str(modooff)
                  End If
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEMOD"
            Cmderr = 0
            atsnd = "Modo OFF=" + Str(modooff)


         Case "SETTXE"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 12 Then
                  Cmderr = 0
                  Cntrestaciones = Tmpb - 1
                  Atsnd = "Tx Estacion=" + Str(tmpb)
                  Set Newtx
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If


         Case "TSTSTA"
            If Numpar = 4 Then
               Statst = Val(cmdsplit(2))
               If Statst < 12 Then
                  Estadotst = Val(cmdsplit(3))
                  If Estadotst < 4 Then
                     Volumen = Val(cmdsplit(4))
                     Cmderr = 0
                     Atsnd = "TST Tx Estacion=" + Str(statst) + " , STATUS=" + Str(estadotst) + ", VOL=" + Str(volumen)
                     Set Newtst
                  Else
                     Cmderr = 6
                  End If
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETMSG"
            If Numpar = 4 Then
               Statst = Val(cmdsplit(2))
               If Statst < 12 Then
                  Msgtmp = Val(cmdsplit(3))
                  If Msgtmp < Nummsg_masuno Then
                     Volumen = Val(cmdsplit(4))
                     Cmderr = 0
                     Set Newmsg
                     Atsnd = "Tx MSG No." + Str(msgtmp) + "a estacion " + Str(statst) + " con vol " + Str(volumen)
                  Else
                     Cmderr = 6
                  End If
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEERGB"
            Cmderr = 0
            Atsnd = "Reg B=" + Str(regb)

         Case "SETCLK"
            If Numpar = 2 Then
               If Len(cmdsplit(2)) = 10 Or Len(cmdsplit(2)) = 12 Then
                  Cmderr = 0
                  Tmpstr52 = Mid(cmdsplit(2) , 7 , 2) + "/" + Mid(cmdsplit(2) , 9 , 2) + "/" + Mid(cmdsplit(2) , 11 , 2)
'                  Print #1 , Tmpstr52
                  Time$ = Tmpstr52
'                  Print #1 , "T>" ; Time$
                  Tmpstr52 = Mid(cmdsplit(2) , 1 , 2) + ":" + Mid(cmdsplit(2) , 3 , 2) + ":" + Mid(cmdsplit(2) , 5 , 2)
'                  Print #1 , Tmpstr52
                  Date$ = Tmpstr52
'                  Print #1 , "D>" ; Date$
                  Atsnd = "WATCHING INFORMA. Se configuro reloj en " + Date$ + " a " + Time$
                  Horamin = Syssec()
                  Horamineep = Horamin
                  Set Gpsnew
                  'Set Actclkok
               Else
                  Cmderr = 6
               End If
            Else
               Cmderr = 4
            End If

         Case "SISCLK"
            Cmderr = 0
            Tmpstr52 = Time$
            'Print #1 , Tmpstr52
            'Print #1 , Time$
            Atsnd = "Hora actual=" + Tmpstr52 + ", Fecha actual="
            Tmpstr52 = Date$
            'Print #1 , Tmpstr52
            'Print #1 , Date$
            Atsnd = Atsnd + Tmpstr52

         Case "LEECLK"
            Cmderr = 0
            Hora = Time(horamin)
            Fecha = Date(horamin)
            ATSND="Ultima CLK "+ hora+ " de "+fecha

         Case "ACTCLK"
            Cmderr = 0
            Set Iniactclk
            Atsnd = "ACT CLK"

         Case "SETTAC"
            If Numpar = 2 Then
               Tactclk = Val(cmdsplit(2))
               Tactclkeep = Tactclk
               Atsnd = "Se configuro Tact CLK=" + Str(tactclk)
            Else
               Cmderr = 4
            End If

         Case "LEETAC"
            Cmderr = 0
            Atsnd = "Tact CLK=" + Str(tactclk)

         Case "SETDBG"
            Cmderr = 0
            Atsnd = "Se habilita display debug"
            Dispbug = 1

         Case "LEEBAT"
            Cmderr = 0
            Atsnd = "VBAT=" + Fusing(vbat , "#.##") + " , VPS=" + Fusing(vps , "#.##")

         Case "SETINI"
            If Numpar = 2 Then
               Tmpb = Len(cmdsplit(2))
               If Tmpb = 8 Then
                  Cmderr = 0
                  Tmpstr52 = Cmdsplit(2)
                  Timeini = Secofday(tmpstr52)
                  Timeinieep = Timeini
                  Atsnd = "Se configura INICIO Modo normal " + Str(timeini) + ", " + Cmdsplit(2)
               Else
                  Cmderr = 5
               End If
            End If

         Case "SETFIN"
            If Numpar = 2 Then
               Tmpb = Len(cmdsplit(2))
               If Tmpb = 8 Then
                  Cmderr = 0
                  Tmpstr52 = Cmdsplit(2)
                  Timefin = Secofday(tmpstr52)
                  Timefineep = Timefin
                  Atsnd = "Se configura FIN Modo normal " + Str(timefin) + ", " + Cmdsplit(2)
               Else
                  Cmderr = 5
               End If

            Else
               Cmderr = 4

            End If

         Case "LEEINI"
            Cmderr = 0
            Tmpstr52 = Time(timeini)
            Atsnd = "INICIO Modo Normal=" + Tmpstr52

         Case "LEEFIN"
            Cmderr = 0
            Tmpstr52 = Time(timefin)
            Atsnd = "FIN Modo Normal=" + Tmpstr52

         Case "ENABUG"
            If Numpar = 2 Then
               Cmderr = 0
               Enabug = Val(cmdsplit(2))
               Atsnd = "Se configuro ENABUG=" + Str(enabug)
            Else
               Cmderr = 5
            End If

         Case "LEEBUG"
            Cmderr = 0
            Atsnd = "ENABUG=" + Str(enabug)

         Case "LEETON"
            Cmderr = 0
            Atsnd = "Lee buffer de tonos recibidos"

            For Tmpb = 1 To 32
               Print #1 , Tmpb ; "," ; Tbl_rxdtmf(tmpb)
            Next

         Case "SETCIN"
            If Numpar = 2 Then
               Cntrini = Val(cmdsplit(2))
               Cntrinieep = Cntrini
               Atsnd = "Se configuro contador de inicios a " + Str(cntrini)
            Else
               Cmderr = 5
            End If

         Case "LEECIN"
            Cmderr = 0
            Atsnd = "Contador de inicios =" + Str(cntrini)

         Case "SETTOU"                                      ' Configura TImeout
            If Numpar = 2 Then
               Cmderr = 0
               Toptout = Val(cmdsplit(2))
               Toptouteep = Toptout
               Atsnd = "Se configuro Timeout máximo en " + Str(toptout)
            Else
               Cmderr = 5
            End If

         Case "LEETOU"
            Cmderr = 0
            Atsnd = "Timeout máximo en " + Str(toptout)

         Case Else
            Cmderr = 1

      End Select

   Else
        Cmderr = 2
   End If

   If Cmderr > 0 Then
      Atsnd = Lookupstr(cmderr , Tbl_err)
   End If

   Print #1 , Atsnd

End Sub


'*******************************************************************************
' LEER ADC
'*******************************************************************************
Sub Leeradc()
   For Adcn = 1 To Numadc
      Adct = Lookup(adcn , Tbl_adc)
      Tmpwadc = Getadc(adct)                                'Valor instantaneo
      'Print #1 , Adcn ; "," ; Tmpwadc
      Adccntr(adcn) = Adccntr(adcn) + Tmpwadc
   Next
   Incr Cntrsmpl
   Cntrsmpl = Cntrsmpl Mod Numsample
   If Cntrsmpl = 0 Then
      For Adcn = 1 To Numadc
         Adc_hex(adcn) = Adccntr(adcn) / Numsample
         Adccntr(adcn) = 0
         Adcval(adcn) = Adc_hex(adcn) * Adck(adcn)
         If Dispbug = 1 Then
            Print #1 , "ADC" ; Adcn ; "=" ; Fusing(adcval(adcn) , "#.#")
         End If
      Next
      Vbat = Adcval(1)
      Vps = Adcval(2)
      'Vdc3 = Adcval(3)
   End If
End Sub


Sub Defaultvalues()
   Trqststaeep = 10
   Trqsnormaleep = 600
   Trqsalarmaeep = 60
   Cntrtxeep = 0
   Dir_slave_eep = 1
   Cntrcrcokeep = 0
   Cntrcrcbadeep = 0
   Cntractremotaeep = 0
   Tmpstr52 = "06:00:00"
   Timeini = Secofday(tmpstr52)
   Timeinieep = Timeini
   Tmpstr52 = "18:00:00"
   Timefin = Secofday(tmpstr52)
   Timefineep = Timefin
   Cntrinieep = 0
   Tactclkeep = 3600
   Toptouteep = 3600
   Adckeep(1) = 0.05494499
   Adckeep(2) = 0.05494499
   Adckeep(3) = 0.05494499
End Sub

Sub Procgps()
   Reset Gpsnew
   Set Relegps
   Print #1 , "GPS1"
   Trygps = 0
   Do
      If Sernew = 1 Then                                 'DATOS SERIAL 1
         Reset Sernew
         If Mid(serproc , 1 , 5) = "GPRMC" Then
            'Set Gpsnew
            Disable Urxc
            Print #1 , "SER=" ; Serproc
            Gpsproc = Serproc
            Print #1 , "GPS=" ; Gpsproc
            Numpar = Split(gpsproc , Cmdsplit(1) , ",")
            If Numpar > 0 Then
               If Cmdsplit(3) = "A" Then
                  Set Gpsnew
                  Hora = Mid(cmdsplit(2) , 1 , 2) + ":" + Mid(cmdsplit(2) , 3 , 2) + ":" + Mid(cmdsplit(2) , 5 , 2)
                  Fecha = Mid(cmdsplit(10) , 1 , 2) + "/" + Mid(cmdsplit(10) , 3 , 2) + "/" + Mid(cmdsplit(10) , 5 , 2)
                  Print #1 , Hora
                  Print #1 , Fecha
                  Tmpl = Syssec(hora , Fecha)
                  Tmpl = Tmpl - 18000
                  Time$ = Time(tmpl)
                  Date$ = Date(tmpl)
                  Print #1 , Time$
                  Print #1 , Date$
               End If
            End If
         End If
         Call Procser()


         Incr Trygps
         Enable Urxc
      End If

   Loop Until Gpsnew = 1 Or Trygps = 200
   Reset Relegps
   Print #1 , "fIN gps ," ; Trygps
End Sub


Sub Procdtmf()
   Print #1 , "NEW DTMF"
   Print #1 , Time$ ; ";" ; Date$
   For J = 1 To 30
       Print #1 , Hex(tbl_rxdtmf(j)) ; ",";
   Next
   Print #1 ,
   Tmpb3 = 0
   For J = 1 To 30
       Print #1 , Hex(tbl_rxdtmf(j)) ; ",";
       If J.0 = 1 Then
         Tmpb = Tbl_rxdtmf(j)
       Else
         Tmpb2 = Tbl_rxdtmf(j)
         Shift Tmpb2 , Left , 4
         Incr Tmpb3
         Tbl_rxhex(tmpb3) = Tmpb Or Tmpb2
       End If
   Next
   Print #1 ,

   Print #1 , "hex dtmf"
   For J = 1 To 15
      Print #1 , Hex(tbl_rxhex(j)) ; ",";
   Next
   Print #1 ,
   Tmpb = Crc8(tbl_rxhex , 14)
   Print #1 , "CRC=" ; Hex(tmpb)

   If Tmpb = Tbl_rxhex(15) Then                          ' Si esta bien el CRC
      Print #1 , "CRC OK=";
      Incr Cntrcrcok
      Cntrcrcokeep = Cntrcrcok
      Print #1 , Cntrcrcok

       Tmpb = Tbl_rxdtmf(1)                                 'Numero de estacion
       Print #1 , "Estacion=" ; Tmpb
       Tmptout = Input_registers_table(numregtimeout)
       Kt = Tmpb - 1
       Reset Tmptout.kt
       Input_registers_table(Numregtimeout) = Tmptout
       J = numregtimeout
       Input_registers_table(Numregtimeout) = Tmptout
       Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
       Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
       Tbl_cntrtout(tmpb) = 0

       Atsnd = "$SETSAT," + Str(tmpb) + ","
       Tbl_status(tmpb) = Tbl_rxdtmf(2)

       If Tmpb = 10 Or Tmpb = 11 Then
         Tmpb5 = Tbl_rxdtmf(2)
         If Tmpb5 = 3 Then
            Incr Cntractremota
            Cntractremotaeep = Cntractremota
            Input_registers_table(13) = Cntractremota
         End If
       End If

       Tmphdr = Tbl_rxdtmf(2)                               'STATUS
       Ptrhdr = Tmpb - 1
       Ptrhdr = Ptrhdr \ 4
       Ptrhdr = Ptrhdr + 3

       Ptrbhdr = Tmpb - 1
       Ptrbhdr = Ptrbhdr Mod 4
       Ptrbhdr = Ptrbhdr * 4
       Print #1 , "STSTUS STA=" ; Tmphdr
       Print #1 , "INPUT PTRHDR=" ; Ptrhdr
       Print #1 , "Ptrbhdr=" ; Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.0
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.1
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.2
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.3
       Print #1 , "INPUTreg(" ; Ptrhdr ; ")=" ; Hex(input_registers_table(ptrhdr))
       'Print #1 , "$SETIPR," ; Ptrhdr ; "," ; Hex(input_registers_table(ptrhdr))
       Tmpstr52 = "$SETIPR," + Str(ptrhdr) + "," + Hex(input_registers_table(ptrhdr))
       Print #1 , Tmpstr52
       Print #2 , Tmpstr52

       Bs1 = Tbl_rxhex(2)
       Bs2 = Tbl_rxhex(3)
       Bs3 = Tbl_rxhex(4)
       Bs4 = Tbl_rxhex(5)
       Tbl_ctx(tmpb) = Tmps

       Ptrhdr = Tmpb - 1
       Ptrhdr = Ptrhdr * 6
       Ptrhdr = Ptrhdr + 1
       Print #1 , "PTRHDR=" ; Ptrhdr

       Tmpw = Makeint(bs3 , Bs4)
       Holding_registers_table(ptrhdr) = Tmpw
       'Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Tmpstr52 = "$SETHDR," + Str(ptrhdr) + "," + Hex(holding_registers_table(ptrhdr))
       Print #1 , Tmpstr52
       Print #2 , Tmpstr52

       Tmpw = Makeint(bs1 , Bs2)
       Incr Ptrhdr
       Holding_registers_table(ptrhdr) = Tmpw
       'Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Tmpstr52 = "$SETHDR," + Str(ptrhdr) + "," + Hex(holding_registers_table(ptrhdr))
       Print #1 , Tmpstr52
       Print #2 , Tmpstr52


       Print #1 , "Cntr_tx=" ; Tbl_ctx(tmpb) ; "," ; Hex(tmpdw)

       Atsnd = Atsnd + Str(tbl_ctx(tmpb)) + ","

       Bs1 = Tbl_rxhex(7)
       Bs2 = Tbl_rxhex(8)
       Bs3 = Tbl_rxhex(9)
       Bs4 = Tbl_rxhex(10)
       Tbl_adc1(tmpb) = Tmps                             'VBAT

       Ptrhdr = Tmpb - 1
       Ptrhdr = Ptrhdr * 6
       Ptrhdr = Ptrhdr + 3
       Print #1 , "PTRHDR=" ; Ptrhdr

       Tmpw = Makeint(bs3 , Bs4)
       Holding_registers_table(ptrhdr) = Tmpw
       Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Print #2 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Tmpw = Makeint(bs1 , Bs2)
       Incr Ptrhdr
       Holding_registers_table(ptrhdr) = Tmpw
       Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Print #2 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))

       'Atsnd = Atsnd + Fusing(tmps , "#.##") + ","
       Print #1 , "Vbat=" ; Tmps ; "," ; Hex(tmps)
       Bs1 = Tbl_rxhex(11)
       Bs2 = Tbl_rxhex(12)
       Bs3 = Tbl_rxhex(13)
       Bs4 = Tbl_rxhex(14)
       Tbl_adc2(tmpb) = Tmps                             'VPS
       Atsnd = Atsnd + Fusing(tbl_adc2(tmpb) , "#.##") + "," + Fusing(tbl_adc1(tmpb) , "#.##") + ","
       Print #1 , "Vps=" ; Tmps ; "," ; Hex(tmps)
       Ptrhdr = Tmpb - 1
       Ptrhdr = Ptrhdr * 6
       Ptrhdr = Ptrhdr + 5
       Print #1 , "PTRHDR=" ; Ptrhdr

       Tmpw = Makeint(bs3 , Bs4)
       Holding_registers_table(ptrhdr) = Tmpw
       Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Print #2 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Tmpw = Makeint(bs1 , Bs2)
       Incr Ptrhdr
       Holding_registers_table(ptrhdr) = Tmpw
       Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
       Print #2 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))


       Tbl_stain(tmpb) = Tbl_rxhex(6)
       Print #1 , "Tbl_rxhex(6)=" ; Bin(tbl_rxhex(6))
       Print #1 , "STAIN=" ; Hex(tbl_stain(tmpb))

       Atsnd = Atsnd + Str(tbl_rxdtmf(2)) + "," + Hex(tbl_stain(tmpb))
       Print #1 , Atsnd
       Print #2 , Atsnd

       Tmphdr = Tbl_rxhex(6)
       Ptrhdr = Tmpb - 1
       Ptrhdr = Ptrhdr \ 2
       Ptrhdr = Ptrhdr + 6

       Ptrbhdr = Tmpb - 1
       Ptrbhdr = Ptrbhdr Mod 2
       Ptrbhdr = Ptrbhdr * 8
       Print #1 , "INPUT PTRHDR=" ; Ptrhdr
       Print #1 , "Ptrbhdr=" ; Ptrbhdr

       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.0
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.1
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.2
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.3
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.4
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.5
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.6
       Incr Ptrbhdr
       Input_registers_table(ptrhdr).ptrbhdr = Tmphdr.7

       Print #1 , "INPUTreg(" ; Ptrhdr ; ")=" ; Hex(input_registers_table(ptrhdr))
       Print #1 , "$SETIPR," ; Ptrhdr ; "," ; Hex(input_registers_table(ptrhdr))
       Print #2 , "$SETIPR," ; Ptrhdr ; "," ; Hex(input_registers_table(ptrhdr))

      Tmps = Cntrtx


      Tmpw = Makeint(bs3 , Bs4)
      Input_registers_table(1) = Tmpw
      Print #1 , "$SETIPR," ; "1" ; "," ; Hex(input_registers_table(1))
      Print #2 , "$SETIPR," ; "1" ; "," ; Hex(input_registers_table(1))
      Tmpw = Makeint(bs1 , Bs2)
      Incr Ptrhdr
      Input_registers_table(2) = Tmpw
      Print #1 , "$SETIPR," ; "2" ; "," ; Hex(input_registers_table(2))
      Print #2 , "$SETIPR," ; "2" ; "," ; Hex(input_registers_table(2))
   Else
      Print #1 , "CRC fail=";
      Incr Cntrcrcbad
      Cntrcrcbadeep = Cntrcrcbad
      Print #1 , Cntrcrcbad
   End If
End Sub

Sub Proctest()
   Print #1 , "TEST ESTACION No. " ; Statst ; ", ESTADO=" ; Estadotst
   Print #1 , Time$ ; ";" ; Date$
   Tmpcntrsta = Cntrestaciones
   Cntrestaciones = Statst
   Tmpstatus = Status
   Status = Estadotst
   Call Gentrama(0)
   Print #1 , "Trama HEX"
   For J = 1 To 10
      Print #1 , Hex(tramatx(j)) ; ",";
   Next
   Print #1,
   Print #1 , "Trama DTMF"
   Print #1 , "$RXDTMF";
   For J = 1 To 20
      Print #1 , "," ; Hex(tramatxdtmf(j)) ;
   Next
   Print #1,
   Call Txdtmf()
   Cntrestaciones = Tmpcntrsta

   Statst = Cntrestaciones
   Status = Tmpstatus
   Cntrseg = 0
   Reset Newtx
   Incr Cntrtx
   Cntrtxeep = Cntrtx
   Print #1 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Statst ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
   Print #2 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Statst ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini

End Sub

Sub Procmodo()
   Tmpl = Secofday()
   If Enabug = 2 Then
      Print #1 , Timeini ; "," ; Tmpl ; "," ; Timefin
   End If
   If Tmpl > Timeini And Tmpl < Timefin Then
      Modooff = 0
      If Modooff <> Modooffant Then
         Print #1 , "MODO DIA " ; Time$
'            Modooff = 0
         Modooffant = Modooff
         Estado_led = 1
         Tmpstr52 = "$SETMOP,1"
         Print #1 , Tmpstr52
         Print #2 , Tmpstr52
      End If
   Else
      Estado_led = 11
      Modooff = 1
      If Modooff <> Modooffant Then
         Print #1 , "MODO NOCTURNO " ; Time$
         'Modooff = 1
         Modooffant = Modooff
         Estado_led = 11
         Tmpstr52 = "$SETMOP,0"
         Print #1 , Tmpstr52
         Print #2 , Tmpstr52
      End If
   End If

   Incr Cntrmodooff
   Cntrmodooff = Cntrmodooff Mod 300
   If Cntrmodooff = 0 Then
      If Modooff = 1 Then
         Tmpstr52 = "$SETMOP,0"
         Print #1 , Tmpstr52
         Print #2 , Tmpstr52
      Else
         Tmpstr52 = "$SETMOP,1"
         Print #1 , Tmpstr52
         Print #2 , Tmpstr52
      End If
   End If

End Sub

Sub Txslaves()
   Cntrestaciones = Cntrestaciones Mod Numsta
   Incr Cntrestaciones
   Print #1 , "NEW TX a Estacion " ; Cntrestaciones ; ", CNTRtx=" ; Cntrtx
   Print #1 , Time$ ; ";" ; Date$
   Input_registers_table(11) = Cntrestaciones
   Input_registers_table(12) = Status
   'Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
   Tmpstr52 = "$SETIPR," + "12" + "," + Hex(input_registers_table(12))
   Print #1 , Tmpstr52
   Print #2 , Tmpstr52

   If Cntrestaciones = Numsta Then
      Reset Newtxper
      Incr Cntrtx
      Cntrtxeep = Cntrtx
      Tmps = Cntrtx
      Tmpw = Makeint(bs3 , Bs4)
      Input_registers_table(1) = Tmpw
      'Print #1 , "$SETIPR," ; "1" ; "," ; Hex(input_registers_table(1))
      Tmpstr52 = "$SETIPR," + "1" + "," + Hex(input_registers_table(1))
      Print #1 , Tmpstr52
      Print #2 , Tmpstr52
      Tmpw = Makeint(bs1 , Bs2)
      Incr Ptrhdr
      Input_registers_table(2) = Tmpw
      'Print #1 , "$SETIPR," ; "2" ; "," ; Hex(input_registers_table(2))
      Tmpstr52 = "$SETIPR," + "2" + "," + Hex(input_registers_table(2))
      Print #1 , Tmpstr52
      Print #2 , Tmpstr52
   End If

   Call Gentrama(0)
   Print #1 , "Trama HEX"
   For J = 1 To 10
      Print #1 , Hex(tramatx(j)) ; ",";
   Next
   Print #1,
   Print #1 , "Trama DTMF"
   Print #1 , "$RXDTMF";
   For J = 1 To 20
      Print #1 , "," ; Hex(tramatxdtmf(j)) ;
   Next
   Print #1,
   Print #1,
   Call Txdtmf()

   Print #1 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Cntrestaciones ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
   Print #2 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Cntrestaciones ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini

End Sub

Sub Txalerta()
   If Newbroadcast = 1 Then
      Reset Newbroadcast
      Print #1 , "TX ALerta"
      Print #1 , Time$ ; ";" ; Date$
      Input_registers_table(12) = Status
      Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
      Print #2 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
      Tmpcntrsta = Cntrestaciones
      Cntrestaciones = &H0F
      Call Gentrama(0)
      Print #1 , "Trama HEX"
      For J = 1 To 10
         Print #1 , Hex(tramatx(j)) ; ",";
      Next
      Print #1,
      Print #1 , "Trama DTMF"
      Print #1 , "$RXDTMF";
      For J = 1 To 20
         Print #1 , "," ; Hex(tramatxdtmf(j)) ;
      Next
      Print #1,
      Call Txdtmf()
      Cntrestaciones = Tmpcntrsta
      Incr Cntrtxbroadcast
      Print #1 , "TX Broadcast " ; Cntrtxbroadcast
      Cntrtxbroadcast = Cntrtxbroadcast Mod 5
      If Cntrtxbroadcast = 0 Then
         Reset Newtxalerta
         Reset Inibroadcast
          Cntrseg = 0
          Reset Newtx
      End If

      Incr Cntrtx
      Cntrtxeep = Cntrtx
      Print #1 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; "15" ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
      Print #2 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; "15" ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
   End If

End Sub

Sub Txtest()
   If Newbroadcast = 1 Then
      Reset Newbroadcast
      Print #1 , "TX TEST"
      Print #1 , Time$ ; ";" ; Date$
      Tmpcntrsta = Cntrestaciones
      Cntrestaciones = &H0E
      Input_registers_table(12) = Status
      Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
      Print #2 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
      Call Gentrama(0)
      Print #1 , "Trama HEX"
      For J = 1 To 10
         Print #1 , Hex(tramatx(j)) ; ",";
      Next
      Print #1,
      Print #1 , "Trama DTMF"
      Print #1 , "$RXDTMF";
      For J = 1 To 20
         Print #1 , "," ; Hex(tramatxdtmf(j)) ;
      Next
      Print #1,
      Call Txdtmf()
      Cntrestaciones = Tmpcntrsta

      Incr Cntrtxbroadcast
      Print #1 , "TX Broadcast " ; Cntrtxbroadcast
      Cntrtxbroadcast = Cntrtxbroadcast Mod 5
      If Cntrtxbroadcast = 0 Then
         Reset Newtxtest
         Reset Inibroadcast
         Cntrseg = 0
         Reset Newtx
      End If
      Incr Cntrtx
      Cntrtxeep = Cntrtx
      Print #1 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; "14" ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
      Print #2 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; "14" ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
   End If

End Sub

Sub Txnormal()
   If Newbroadcast = 1 Then
      Reset Newbroadcast
      Print #1 , "TX NORMAL"
      Print #1 , Time$ ; ";" ; Date$
      Input_registers_table(12) = Status
      Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
      Print #2 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))

      Tmpcntrsta = Cntrestaciones

      If Modooff = 0 Then
         Cntrestaciones = &H0D
         Tmpstr8 = "13"
      Else
         Cntrestaciones = &H0C
         Tmpstr8 = "12"
      End If


      Call Gentrama(0)
      Print #1 , "Trama HEX"
      For J = 1 To 10
         Print #1 , Hex(tramatx(j)) ; ",";
      Next
      Print #1,
      Print #1 , "Trama DTMF"
      Print #1 , "$RXDTMF";
      For J = 1 To 20
         Print #1 , "," ; Hex(tramatxdtmf(j)) ;
      Next
      Print #1,
      Call Txdtmf()
      Cntrestaciones = Tmpcntrsta

      Incr Cntrtxbroadcast
      Print #1 , "TX Broadcast " ; Cntrtxbroadcast
      Cntrtxbroadcast = Cntrtxbroadcast Mod 5
      If Cntrtxbroadcast = 0 Then
         Reset Newtxnormal
         Reset Inibroadcast
         Cntrseg = 0
         Reset Newtx
      End If
      Incr Cntrtx
      Cntrtxeep = Cntrtx
      Print #1 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Tmpstr8 ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
      Print #2 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Tmpstr8 ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
   End If

End Sub

Sub Txmsg()
   Print #1 , "NEW TX MSG a Estacion " ; Statst
   Print #1 , Time$ ; ";" ; Date$
   Input_registers_table(11) = Statst
   Input_registers_table(12) = Status
   'Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
   Tmpstr52 = "$SETIPR," + "12" + "," + Hex(input_registers_table(12))
   Print #1 , Tmpstr52
   Print #2 , Tmpstr52
   Cntrestaciones = Statst
   Call Gentrama(1)
   Print #1 , "Trama HEX"
   For J = 1 To 10
      Print #1 , Hex(tramatx(j)) ; ",";
   Next
   Print #1,
   Print #1 , "Trama DTMF"
   Print #1 , "$RXDTMF";
   For J = 1 To 20
      Print #1 , "," ; Hex(tramatxdtmf(j)) ;
   Next
   Print #1,
   Call Txdtmf()
   Incr Cntrtx
   Cntrtxeep = Cntrtx
   Print #1 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Tmpstr8 ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini
   Print #2 , "$SETMST,," ; Status ; "," ; Cntrtx ; "," ; Tmpstr8 ; "," ; Cntrcrcok ; "," ; Cntrcrcbad ; "," ; Cntrini

End Sub


Sub Procmodbusreg()
   Incr Cntripadc
   Cntripadc = Cntripadc Mod 900
   If Cntripadc = 0 Then
      Print #1 , "VBAT=" ; Vbat
      J = 13
      Input_registers_table(j) = Vbat1
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      J = 14
      Input_registers_table(j) = Vbat2
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      J = 15
      Input_registers_table(j) = Vbat3
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      J = 16
      Input_registers_table(j) = Vbat4
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))

      Print #1 , "VPS=" ; Vps
      J = 17
      Input_registers_table(j) = Vps1
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      J = 18
      Input_registers_table(j) = Vps2
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      J = 19
      Input_registers_table(j) = Vps3
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      J = 20
      Input_registers_table(j) = Vps4
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))

      J = 21
      Input_registers_table(j) = Vps4
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
   End If

End Sub

Sub Vertout()
   Tmptout = Input_registers_table(numregtimeout)
   For Jt = 1 To Numsta
      Incr Tbl_cntrtout(jt)
      If Tbl_cntrtout(jt) > Toptout Then
         Kt = Jt - 1
         Set Tmptout.kt
         Tbl_cntrtout(jt) = 0
      End If
   Next
   If Tmptout <> Input_registers_table(numregtimeout) Then
      J = Numregtimeout
      Input_registers_table(Numregtimeout) = Tmptout
      Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
      Print #2 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
   End If

End Sub

'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************

Tbl_err:
Data "OK"                                                   '0
Data "Comando no reconocido"                                '1
Data "Longitud comando no valida"                           '2
Data "Numero de usuario no valido"                          '3
Data "Numero de parametros invalido"                        '4
Data "Error longitud parametro 1"                           '5
Data "Error longitud parametro 2"                           '6
Data "Parametro no valido"                                  '7
Data "ERROR8"                                               '8
Data "ERROR SD. Intente de nuevo"                           '9

Tabla_estado:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000011&                    'Estado 1
Data &B00000000000000000000000000110011&                    'Estado 2
Data &B00000000000000000000001100110011&                    'Estado 3
Data &B00000000000000000011001100110011&                    'Estado 4
Data &B00000000000000110011001100110011&                    'Estado 5
Data &B00000000000011001100000000110011&                    'Estado 6
Data &B00001111111111110000111111111111&                    'Estado 7
Data &B01010101010101010101010101010101&                    'Estado 8
Data &B00110011001100110011001100110011&                    'Estado 9
Data &B01110111011101110111011101110111&                    'Estado 10
Data &B11111111111111110000000000000000&                    'Estado 11
Data &B11111111111111000000000011001100&                    'Estado 12
Data &B11111111111111000000110011001100&                    'Estado 13
Data &B11111111111111001100110011001100&                    'Estado 14
Data &B11111111111111000000000000001100&                    'Estado 15
Data &B11111111111111111111111111110000&                    'Estado 16

Tbl_adc:
Data 0                                                      'dummy
Data 0
Data 1
Data 2
Data 3



Loaded_arch: