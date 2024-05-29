
'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se reciben por el puerto serial a una
' memoria SD
'


$version 0 , 1 , 135
$regfile = "m1284pdef.dat"
$crystal = 7372800
'$crystal = 8000000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128


'Declaracion de constantes
Const Pcf8591write = &B10011100
Const Pcf8591read = &B10011101
Const Pcf8591dacconfig = &B01000000

'Configuracion de entradas/salidas
Led1 Alias Portb.2                                          'LED ROJO
Config Led1 = Output

Pwrmp3 Alias Portb.0
Config Pwrmp3 = Output

Ena1 Alias Portb.3
Config Ena1 = Output

Ena2 Alias Portb.4
Config Ena2 = Output

Dout Alias Portd.7
Config Dout = Output

D0 Alias Pinc.4
Config D0 = Input
Set Portc.4

D1 Alias Pinc.5
Config D1 = Input
Set Portc.5

D2 Alias Pinc.6
Config D2 = Input
Set Portc.6

Test Alias Pinb.5
Config Test = Input

Set Portb.5


Config Scl = Portc.1                                        ' we need to provide the SCL pin name
Config Sda = Portc.0                                        ' we need to provide the SDA pin name
'$lib "i2c_twi.lbx"
'Config Twi = 100000
I2cinit



'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

'TIMER0
Config Timer2 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer2 Int_timer2
Enable Timer2
Start Timer2

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

' Puerto serial 2
Open "com2:" For Binary As #2
On Urxc1 At_ser2
Enable Urxc1

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "DRVAUDIO_archivos.bas"

'Programa principal

Call Inivar()


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Sernew1 = 1 Then
      Reset Sernew1
      Call Procser1()

   End If

   If Inidac = 1 Then
      Reset Inidac
      Print #1 , "WRITE DAC"
      Call Wrdac(dacout)
   End If

   If Leeradc = 1 Then
      Reset Leeradc
      Print #1 , "READ ADC"
      Call Rdadc(numcanal)
   End If

   If Ininormal = 1 Then
      Reset Ininormal
      Print #1 , "NORMAL"
      Print #1 , "$DRVAUD,1"
      Print #1 , "$DRVAUD,1"
      Reset Ena1
      Reset Ena2
      Call Wrdac(0)                                         ' Silencio amplificador
      'Comando para silenciar reproduccion de audio
      Set Pwrmp3                                            ' off dfp

   End If


   If Initest = 1 Then
      Reset Initest
      Print #1 , "INITEST"
      Reset Pwrmp3                                          'MOdulo ON
      Set Ena1
      Call Wrdac(255)                                       ' Silencio amplificador

      T0rate = 200
      Reset T0tout
      T0cntr = 0
      Set T0ini
      Do

      Loop Until Sernew1 = 1 Or T0tout = 1
      Call Procser1()
      Reset Sernew1

      If Tblser1(4) = &H3F Then
         Print #1 , "Modulo ON"
         If Tblser1(7) = &H02 Then
            Print #1 , "SD OK"
            Print #1 , "$DRVAUD,2"
            Print #1 , "$DRVAUD,2"                          'NO SD presente
            Print #1 , "Wait 2"
            Call Espera(200)
            Print #1 , "FIN"
            Print #1 , "Vol=" ; Volumen
            Call Cmddfp(&H06 , Volumen)
            Call Cmddfp(&H0f , &H0106)
            Print #1 , "Tono Info TEST"
            T0rate = 2000
            Reset T0tout
            T0cntr = 0
            Set T0ini
            Do
               Datomp3 = Tblser1(4)
               If Sernew = 1 Then                           'DATOS SERIAL 1
                  Reset Sernew
                  Print #1 , "SER1=" ; Serproc
                  Call Procser()
               End If

               Tmpw = T0cntr Mod 100
               If Tmpw = 0 Then
                  Print #1 , "T0CNTR=" ; T0cntr
               End If


            Loop Until Datomp3 = &H3D Or T0tout = 1 Or Status = 1
            Print #1 , "Fin tono Test"
            Call Procser1()
            Print #1 , "Tono TEST"

            Call Cmddfp(&H0f , &H0106)
            Call Espera(100)
            Call Procser1()
            Call Cmddfp(&H08 , &H0000)                      ' En repeticion


            T0rate = 3000
            Reset T0tout
            T0cntr = 0
            Set T0ini

            Do
               If Sernew = 1 Then                           'DATOS SERIAL 1
                  Reset Sernew
                  Print #1 , "SER1=" ; Serproc
                  Call Procser()
               End If

               If Sernew1 = 1 Then
                  Reset Sernew1
                  Call Procser1()
               End If

               Tmpw = T0cntr Mod 100
               If Tmpw = 0 Then
                  Print #1 , "T0CNTR=" ; T0cntr
               End If

            Loop Until T0tout = 1 Or Status = 1

            Print #1 , "FIN TEST"

            Call Cmddfp(&H0e , &H0000)
            Call Espera(100)
            Call Procser1()
            Status = 1


         Else
            Print #1 , "NO SD"
            Print #1 , "$DRVAUD,4"
            Print #1 , "$DRVAUD,4"                          'NO SD presente
         End If

      Else
         Print #1 , "Modulo NO Presente"
         Reset Ena1
         Reset Ena2
         Call Wrdac(0)                                         ' Silencio amplificador
         Set Pwrmp3
         Set Initest

      End If

      'Comando para anuncio de modo test 1 sola vez

      'Comando para auncio continuo de modo test

      '
   End If

   If Anaranja = 1 Then
      Reset Anaranja
      Print #1 , "ALERTA NARANJA"
      'Comando para anuncio de modo Alerta Narnaja sola vez
      Reset Pwrmp3                                          'MOdulo ON
      Set Ena1
      Call Wrdac(255)                                       ' Silencio amplificador

      T0rate = 200
      Reset T0tout
      T0cntr = 0
      Set T0ini
      Do

      Loop Until Sernew1 = 1 Or T0tout = 1
      Call Procser1()
      Reset Sernew1

      'Comando para auncio continuo de modo alerta naranja
      If Tblser1(4) = &H3F Then
         Print #1 , "Modulo ON"
         If Tblser1(7) = &H02 Then
            Print #1 , "SD OK"
            Print #1 , "$DRVAUD,3"
            Print #1 , "$DRVAUD,3"                          'NO SD presente

            Call Cmddfp(&H0f , &H0108)
            Print #1 , "Tono Info ALARMA"

            T0rate = 2000
            Reset T0tout
            T0cntr = 0
            Set T0ini
            Do
               Datomp3 = Tblser1(4)
               If Sernew = 1 Then                           'DATOS SERIAL 1
                  Reset Sernew
                  Print #1 , "SER1=" ; Serproc
                  Call Procser()
               End If

               Tmpw = T0cntr Mod 100
               If Tmpw = 0 Then
                  Print #1 , "T0CNTR=" ; T0cntr
               End If

            Loop Until Datomp3 = &H3D Or T0tout = 1 Or Status = 1
            Print #1 , "Fin tono ALARMA"
            Call Procser1()
            Print #1 , "Tono ALARMA"

            Call Cmddfp(&H0f , &H0108)
            Call Espera(100)
            Call Procser1()
            Call Cmddfp(&H08 , &H0000)                      ' En repeticion

            Cntrmin = 0
            Do

               T0rate = 6000
               Reset T0tout
               T0cntr = 0
               Set T0ini

               Incr Cntrmin
               Do
                  If Sernew = 1 Then                           'DATOS SERIAL 1
                     Reset Sernew
                     Print #1 , "SER1=" ; Serproc
                     Call Procser()
                  End If

                  If Sernew1 = 1 Then
                     Reset Sernew1
                     Call Procser1()
                  End If

                  Tmpw = T0cntr Mod 100
                  If Tmpw = 0 Then
                     Print #1 , "T0CNTR=" ; T0cntr
                  End If


               Loop Until T0tout = 1 Or Status = 1
               Print #1 , "Cntrmin=" ; Cntrmin

            Loop Until Cntrmin = 30 Or Status = 1

            Print #1 , "FIN ALARMA"
            Call Cmddfp(&H0e , &H0000)
            Call Espera(100)
            Call Procser1()
            Status = 1

         Else
            Print #1 , "NO SD"
            Print #1 , "$DRVAUD,5"
            Print #1 , "$DRVAUD,5"                          'NO SD presente
         End If

      Else
        Print #1 , "Modulo NO Presente"
         Reset Ena1
         Reset Ena2
         Call Wrdac(0)                                         ' Silencio amplificador
         Set Pwrmp3
         Call Espera(100)
          'Call Procser1()

         Set Anaranja
      End If
     '
   End If

   If Test = 0 Then
      Print #1 , "TEST por ibutton"
      Reset Ininormal
      Reset Ena1
      Reset Ena2
      Call Wrdac(0)                                         ' Silencio amplificador
      'Comando para silenciar reproduccion de audio
      Set Pwrmp3                                            ' off dfp

      Incr Cntrtest

      If Cntrtest.0 = 1 Then
         Status = 2
      Else
         Status = 1

      End If

   End If


   Call Leersta()

Loop