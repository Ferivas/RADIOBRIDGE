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



$version 0 , 1 , 178
$regfile = "m128def.dat"
$crystal = 16000000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128
$projecttime = 146


'Declaracion de constantes
Const Numsta = 11                                           'Numero de estaciones
Const Numsample = 8                                         ' Numero de muestras para promediar los ADC
Const Numadc = 3
Const Tactadc = 900
Const Tactadc_10 = Tactadc - 10

Const Numregtimeout = 20


'Const Modbus_slave_adress = 4
'Const Maximum_coil_number = 2                               ' Keep in mind that modbus adress start at 0 so this is 16 coils !
'Const Maximum_discrete_inputs = 16                          ' Keep in mind that modbus adress start at 0 so this is 16 inputs !
'Const Maximum_holding_registers = 72                        ' Keep in mind that modbus adress start at 0 so this is 16 registers !
'Const Maximum_input_registers = 15                          ' Keep in mind that modbus adress start at 0 so this is 16 registers !
'Const Maximum_adc_channel = 7                               ' Keep in mind that modbus adress start at 0 so this is 8 channels !
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

'Pinbug Alias Portb.5
'Config Pinbug = Output
Relegps Alias Portc.4
Config Relegps = Output
Reset Relegps

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "MASTER_SIM_archivos.bas"



'Programa principal

Call Inivar()
Call Vercfg()
'Call Procgps()


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

'   If Iniactclk = 1 Then
'      Print #1 , "Act. CLK," ; Time$ ; "," ; Date$
'      Reset Iniactclk
'      Call Procgps()
'      Print #1 , "CLK>" ; Time$ ; "," ; Date$
'   End If



   If Iniprog = 1 Then
      Print #1 , "Ini modo Prog"
      Tmpprg = Estado_led
      Estado_led = 16
      Do
         If Sernew = 1 Then                                 'DATOS SERIAL 1
            Reset Sernew
            Print #1 , "SER1=" ; Serproc
            Call Procser()
         End If
      Loop Until Iniprog = 0
      Print #1 , "Fin modo Prog"
      Estado_led = Tmpprg
   End If

   If Newseg = 1 Then
      Reset Newseg
      Call Procmodo()
      Call Vertout()
   End If

   If Newtx = 1 And Inibroadcast = 0 Then
      Reset Newtx
      Call Txslaves()
   End If

   If Newtst = 1 Then
      Reset Newtst
      Call Proctest()
   End If

   If Newdv = 1 Then
      Reset Newdv
      Call Procdtmf()
   End If

   'Call Leersta()

'   If Newtxalerta = 1 Then
'      Call Txalerta()
'   End If

'   If Newtxtest = 1 Then
'      Call Txtest()
'   End If

   If Newtxnormal = 1 Then
      Call Txnormal()
   End If

   If Newadc = 1 Then
      Reset Newadc
      Call Leeradc()
      Call Procmodbusreg()
   End If

   If Inileerhr = 1 Then
      Reset Inileerhr
      For J = 1 To 72
         Print #1 , J ; "," ; Hex(holding_registers_table(j))
      Next

   End If

   If Inivariables = 1 Then
      Reset Inivariables
      Call Inivar()
   End If
Loop