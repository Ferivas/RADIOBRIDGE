'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para manejar Radio Bridge Master que controla tx periodicas via
' radio VHF.
' El Radio Bridge Master consulta periodicamente a las estaciones remotas
' indicando estado actual, para lo cual envia un codigo hexadecimal que
' incluye:

' Numero de estacion - 4 bits
' Estado actual - 4 bits
' Numero de transmision - 32 bits (Indica contador que se incrmenta cada vez
' que se tx un dato)
' Checksum calculado con CRC8 - 8 bits
' Existen tres estados :
' Normal  - El Master consulta periodicamente estado de las estaciones
' Test    - El Master envia comando de configuracion de Test de Audio y DRVLED
' Naranja - El Master envia comando de configuracion de estado de alarma
' 20.01.17
' Se añaden subrutinas de fecha y hora para poder mejorar la depuración
' Se añade procesamiento de fecha y hora por el puertos serial 0 que se coencta
' a tarjeta de GPS
' Se conecta datlogger de Sparkfun para monitorear actividad en el puerto serial
' Se eliminan subrutinas de MODBUS porque este se esta procesando en otra tarjeta



$version 0 , 1 , 103
$regfile = "m128def.dat"
$crystal = 16000000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128
$projecttime = 24


'Declaracion de constantes
Const Numsta = 11                                           'Numero de estaciones
Const Numsample = 8                                         ' Numero de muestras para promediar los ADC
Const Numadc = 3
Const Tactadc = 900
Const Tactadc_10 = Tactadc - 10


'Const Modbus_slave_adress = 4
Const Maximum_coil_number = 2                               ' Keep in mind that modbus adress start at 0 so this is 16 coils !
Const Maximum_discrete_inputs = 16                          ' Keep in mind that modbus adress start at 0 so this is 16 inputs !
Const Maximum_holding_registers = 72                        ' Keep in mind that modbus adress start at 0 so this is 16 registers !
Const Maximum_input_registers = 15                          ' Keep in mind that modbus adress start at 0 so this is 16 registers !
Const Maximum_adc_channel = 7                               ' Keep in mind that modbus adress start at 0 so this is 8 channels !
'********************************************************************


'Configuracion de entradas/salidas
Led1 Alias Portc.5                                          'LED ROJO
Config Led1 = Output

Ledtx Alias Portc.6
Config Ledtx = Output

Ledrx Alias Portc.7
Config Ledrx = Output


Ptt Alias Portg.2                                           ' PTT Radio
Config Ptt = Output

'Pinbug Alias Portb.5                                        '
'Config Pinbug = Output

'CONTROL Generador DTMF HT9200B
Dout0 Alias Portb.4
Config Dout0 = Output
Dout1 Alias Portb.5
Config Dout1 = Output
Dout2 Alias Portb.6
Config Dout2 = Output
Dout3 Alias Portb.7
Config Dout3 = Output
Ce Alias Portb.3
Config Ce = Output

'Control Decodificador de tonos HT9170
Din0 Alias Pind.4
Config Din0 = Input
Din1 Alias Pind.5
Config Din1 = Input
Din2 Alias Pind.6
Config Din2 = Input
Din3 Alias Pind.7
Config Din3 = Input
Dv Alias Pine.4
Config Dv = Input

Set Portd.4
Set Portd.5
Set Portd.6
Set Portd.7


'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

'TIMER1
Config Timer1 = Timer , Prescale = 256                      'Ints a 1Hz
On Timer1 Int_timer1
Enable Timer1
Start Timer1

Dim Dummy As Byte
Config Date = Dmy , Separator = /
Config Clock = User                                         ', Gosub = Sectic

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

' Puerto serial 2
Open "com2:" For Binary As #2
On Urxc1 At_ser2
Enable Urxc1


Enable Int4
Config Int4 = Change
On Int4 Pcint_int


'Botones para Test, Normal , Alerta Naranja
D0 Alias Pinc.0
Config D0 = Input                                           'TEST
D1 Alias Pinc.1
Config D1 = Input                                           'ALERTA NARANJA

Set Portc.0
Set Portc.1

'D2 Alias Pinc.7
'Config D2 = Input                                           'ALERTA ROJA

'Modopin Alias Pinc.2
'Config Modopin = Input                                      'TAMPER
'Set Portc.2

Pinbug Alias Portb.5
Config Pinbug = Output

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "MASTER_archivos.bas"



'Programa principal

Call Inivar()


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
      'Print #1 , Time$ ; ";" ; Date$
   End If

   If Sernew1 = 1 Then                                      'DATOS SERIAL 1
      Reset Sernew1
      Print #1 , "KYBDATA"
      Print #1 , "SER2=" ; Serproc1
      Serproc = Serproc1
      Call Procser()
      'Print #1 , Time$ ; ";" ; Date$
   End If

   If Newseg = 1 Then
      Reset Newseg
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
            Print #1 , "$SETMOP,1"
         End If
      Else
         Estado_led = 11
         Modooff = 1
         If Modooff <> Modooffant Then
            Print #1 , "MODO NOCTURNO " ; Time$
            'Modooff = 1
            Modooffant = Modooff
            Estado_led = 11
            Print #1 , "$SETMOP,0"
         End If
      End If

      Incr Cntrmodooff
      Cntrmodooff = Cntrmodooff Mod 300
      If Cntrmodooff = 0 Then
         If Modooff = 1 Then
           Print #1 , "$SETMOP,0"
         Else
           Print #1 , "$SETMOP,1"
         End If

      End If

   End If



   If Newtx = 1 And Inibroadcast = 0 Then
      Reset Newtx
      Cntrestaciones = Cntrestaciones Mod Numsta
      Incr Cntrestaciones
      Print #1 , "NEW TX a Estacion " ; Cntrestaciones ; ", CNTRtx=" ; Cntrtx
      Print #1 , Time$ ; ";" ; Date$
      Input_registers_table(11) = Cntrestaciones
      Input_registers_table(12) = Status
      Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))

      If Cntrestaciones = Numsta Then
         Reset Newtxper
         Incr Cntrtx
         Cntrtxeep = Cntrtx
         Tmps = Cntrtx
         Tmpw = Makeint(bs3 , Bs4)
         Input_registers_table(1) = Tmpw
         Print #1 , "$SETIPR," ; "1" ; "," ; Hex(input_registers_table(1))
         Tmpw = Makeint(bs1 , Bs2)
         Incr Ptrhdr
         Input_registers_table(2) = Tmpw
         Print #1 , "$SETIPR," ; "2" ; "," ; Hex(input_registers_table(2))
      End If

      Call Gentrama()
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

   'Else
    '  If Inibroadcast = 1 Then
     '    Print #1 , " NO TX periodica por Broadcast"
      '   Reset Newtx

      'End If

   End If

   If Newtst = 1 Then
      Reset Newtst
      Print #1 , "TEST ESTACION No. " ; Statst ; ", ESTADO=" ; Estadotst
      Print #1 , Time$ ; ";" ; Date$
      Tmpcntrsta = Cntrestaciones
         Cntrestaciones = Statst
         Tmpstatus = Status
         Status = Estadotst
         Call Gentrama()
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

   End If


   'Call Rxdtmf()                                            ' Pruebo con DV si hay un dato valido

   If Newdv = 1 Then
      Reset Newdv
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

          Tmpb = Tbl_rxdtmf(1)                              'Numero de estacion
          Print #1 , "Estacion=" ; Tmpb
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

          Tmphdr = Tbl_rxdtmf(2)
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
          Print #1 , "$SETIPR," ; Ptrhdr ; "," ; Hex(input_registers_table(ptrhdr))

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
          Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))
          Tmpw = Makeint(bs1 , Bs2)
          Incr Ptrhdr
          Holding_registers_table(ptrhdr) = Tmpw
          Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))

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
          Tmpw = Makeint(bs1 , Bs2)
          Incr Ptrhdr
          Holding_registers_table(ptrhdr) = Tmpw
          Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))

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
          Tmpw = Makeint(bs1 , Bs2)
          Incr Ptrhdr
          Holding_registers_table(ptrhdr) = Tmpw
          Print #1 , "$SETHDR," ; Ptrhdr ; "," ; Hex(holding_registers_table(ptrhdr))


          Tbl_stain(tmpb) = Tbl_rxhex(6)
          Print #1 , "STAIN=" ; Hex(tbl_stain(tmpb))

          Atsnd = Atsnd + Str(tbl_rxdtmf(2)) + "," + Hex(tbl_stain(tmpb))
          Print #1 , Atsnd

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

         Tmps = Cntrtx


         Tmpw = Makeint(bs3 , Bs4)
         Input_registers_table(1) = Tmpw
         Print #1 , "$SETIPR," ; "1" ; "," ; Hex(input_registers_table(1))
         Tmpw = Makeint(bs1 , Bs2)
         Incr Ptrhdr
         Input_registers_table(2) = Tmpw
         Print #1 , "$SETIPR," ; "2" ; "," ; Hex(input_registers_table(2))



      Else
         Print #1 , "CRC fail=";
         Incr Cntrcrcbad
         Cntrcrcbadeep = Cntrcrcbad
         Print #1 , Cntrcrcbad
      End If


   End If

   Call Leersta()

   If Newtxalerta = 1 Then
      'Set Newtxbroadcast
      'Reset Newtxalerta
      If Newbroadcast = 1 Then
         Reset Newbroadcast
         Print #1 , "TX ALerta"
         Print #1 , Time$ ; ";" ; Date$
         Input_registers_table(12) = Status
         Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
         Tmpcntrsta = Cntrestaciones
         Cntrestaciones = &H0F
         Call Gentrama()
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
      End If
   End If

   If Newtxtest = 1 Then
         'Reset Newtxtest
      If Newbroadcast = 1 Then
         Reset Newbroadcast
         Print #1 , "TX TEST"
         Print #1 , Time$ ; ";" ; Date$
         Tmpcntrsta = Cntrestaciones
         Cntrestaciones = &H0E
         Input_registers_table(12) = Status
         Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))
         Call Gentrama()
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
      End If
   End If

   If Newtxnormal = 1 Then
         'Reset Newtxnormal
      If Newbroadcast = 1 Then
         Reset Newbroadcast
         Print #1 , "TX NORMAL"
         Print #1 , Time$ ; ";" ; Date$
         Input_registers_table(12) = Status
         Print #1 , "$SETIPR," ; "12" ; "," ; Hex(input_registers_table(12))

         Tmpcntrsta = Cntrestaciones

         If Modooff = 0 Then
            Cntrestaciones = &H0D
            Tmpstr8 = "13"
         Else
            Cntrestaciones = &H0C
            Tmpstr8 = "12"
         End If


         Call Gentrama()
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
      End If
   End If

   If Newadc = 1 Then
      Reset Newadc
      Call Leeradc()
      Incr Cntripadc
      Cntripadc = Cntripadc Mod 900
      If Cntripadc = 0 Then
         Print #1 , "VBAT=" ; Vbat
         J = 13
         Input_registers_table(j) = Vbat1
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
         J = 14
         Input_registers_table(j) = Vbat2
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
         J = 15
         Input_registers_table(j) = Vbat3
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
         J = 16
         Input_registers_table(j) = Vbat4
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))

         Print #1 , "VPS=" ; Vps
         J = 17
         Input_registers_table(j) = Vps1
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
         J = 18
         Input_registers_table(j) = Vps2
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
         J = 19
         Input_registers_table(j) = Vps3
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))
         J = 20
         Input_registers_table(j) = Vps4
         Print #1 , "$SETIPR," ; J ; "," ; Hex(input_registers_table(j))


      End If
   End If

   If Inileerhr = 1 Then
      Reset Inileerhr
      For J = 1 To 72
         Print #1 , J ; "," ; Hex(holding_registers_table(j))
      Next

   End If

'(
   If Modopin = 1 Then
      If Modopinsta <> Modopin Then
         Modopinsta = Modopin
         Print #1 , "MODOPIN=1"
         Reset Modooff
         Print #1 , "MODOOFF=0"
         Estado_led = 1
      End If
   Else
      If Modopinsta <> Modopin Then
         Modopinsta = Modopin
         Print #1 , "MODOPIN=0"
         If Status = 1 Then
            Set Modooff
            Print #1 , "MODOOFF=1"
            Estado_led = 11
         Else
            Print #1 , "No cambio MODOFF"
         End If
      End If
   End If
')
   If Inivariables = 1 Then
      Reset Inivariables
      Call Inivar()

   End If
Loop