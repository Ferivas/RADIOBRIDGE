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

$version 0 , 1 , 323
$regfile = "m128def.dat"
$crystal = 16000000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128
$projecttime = 6


'Declaracion de constantes
Const Numsta = 12                                           'Numero de estaciones
Const Numsample = 8                                         ' Numero de muestras para promediar los ADC
Const Numadc = 3
Const Numi2c = 4
Const Numi2c_masuno = Numi2c + 1
Const Cini = &H24                                           'Caracter de inicio
Const Idmaster = &H55




'Configuracion de entradas/salidas
Led1 Alias Portc.5                                          'LED ROJO
Config Led1 = Output

Ledtx Alias Portc.6
Config Ledtx = Output

Ledrx Alias Portc.7
Config Ledrx = Output


Ptt Alias Portg.2                                           ' PTT Radio
Config Ptt = Output

'Pinbug Alias Portb.1
'Config Pinbug = Output


'CONTROL Generador DTMF HT9200B
Ce Alias Portb.3
Config Ce = Output
Dout0 Alias Portb.4
Config Dout0 = Output
Dout1 Alias Portb.5
Config Dout1 = Output
Dout2 Alias Portb.6
Config Dout2 = Output
Dout3 Alias Portb.7
Config Dout3 = Output

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
'Set Porte.4

'Botones para Test, Normal , Alerta Naranja
D0 Alias Pinc.0
Config D0 = Input                                           'TEST
D1 Alias Pinc.1
Config D1 = Input                                           'ALERTA NARANJA

Tamper Alias Pinc.2
Config Tamper = Input                                       'TAMPER

Set Portc.0
Set Portc.1
Set Portc.2

'Pines de Salida para manejar driver led
Drv0 Alias Porta.4
Config Drv0 = Output
Drv1 Alias Porta.5
Config Drv1 = Output
Drv2 Alias Porta.6
Config Drv2 = Output
Drv3 Alias Porta.7
Config Drv3 = Output

'Rele
Rele Alias Portc.4
Config Rele = Output



'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

'TIMER0
'Config Timer2 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
'On Timer2 Int_timer2
'Enable Timer2
'Start Timer2

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

' Puerto serial 2
Open "com2:" For Binary As #2
On Urxc1 At_ser2
Enable Urxc1

'ADC
Config Adc = Single , Prescaler = Auto                      ', Reference = Internal
Enable Adc
Start Adc

'Pcmsk2 = &B00010000
Enable Int4
Config Int4 = Change
On Int4 Pcint_int

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "RadioBridge_VOL_archivos.bas"



'Programa principal

Call Inivar()


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Newdv = 1 Then
      Reset Newdv
      Call Procdtmf()
   End If

   Call Leersta()

   If Newtxalerta = 1 Then
      Reset Newtxalerta
      Print #1 , "TX ALerta"

   End If

   If Newadc = 1 Then
      Reset Newadc
      Call Leeradc()

      If Inileei2c = 1 Then
         'Print #1 , "I2C " ; Cntrleci2c
         If Cntrleci2c < 4 Then
            Tmpcanal = Cntrleci2c + 1
            Print #1 , "$LEEI2C," ; Tmpcanal
            Print #2 , "$LEEI2C," ; Tmpcanal
         End If
         Incr Cntrleci2c
         Cntrleci2c = Cntrleci2c Mod 10
         If Cntrleci2c = 0 Then
            Print #1 , "Cntrtryi2cA=" ; Cntrtryi2c
            Incr Cntrtryi2c
           ' Print #1 , "Cntrtryi2cB=" ; Cntrtryi2c
            Cntrtryi2c = Cntrtryi2c Mod 3
            If Cntrtryi2c = 0 Then
               Reset Inileei2c
'               Cntrtryi2c
               Print #1 , "FIN consulta ADC"
            End If
         End If
      End If
   End If


   If Tamper = 1 Then
      If Tampersta <> Tamper Then
         Print #1 , "ABIERTO"
         Tampersta = Tamper
         Insta = 0
      End If
   Else
      If Tampersta <> Tamper Then
         Print #1 , "CERRADO"
         Tampersta = Tamper
         Insta = 1
      End If
   End If

   If Newdrv = 1 Then
      Reset Newdrv
      If Drvaudiook = 0 Then
         Print #1 , "$DRVAUD," ; Status ; "," ; Volumen
         Print #2 , "$DRVAUD," ; Status ; "," ; Volumen
         Incr Cntrsubsta
         Cntrsubsta = Cntrsubsta Mod 10
         If Cntrsubsta = 0 Then
            Print #1 , "Sin respuesta DRVAUDIO"
            Substa = 1
         End If
      End If
   End If

   If Newmsg = 1 Then
      Reset Newmsg
      Print #1 , "$SETMSG," ; Tmpmsg ; "," ; Volumen
      Print #2 , "$SETMSG," ; Tmpmsg ; "," ; Volumen

   End If


   If Newsubsta = 1 Then
      Reset Newsubsta
      Incr Ptrani
      Ptrani = Ptrani Mod Numani

      If Modooff = 0 Then
         If Status = 1 Then
            Valsta = Lookup(ptrani , Tbl_normal)
         End If

         If Status = 2 Then
            Valsta = Lookup(ptrani , Tbl_test)
         End If

         If Status = 3 Then
            Valsta = Lookup(ptrani , Tbl_alarma)
         End If
      Else
         Valsta = 7
      End If
      If Enabug = 1 Then
         Print #1 , Ptrani ; "," ; Valsta ; "," ; "OFF=" ; Modooff
      End If
      Call Outsubsta(valsta)

   End If

   If Inivariables = 1 Then
      Reset Inivariables
      Call Inivar()
   End If

Loop