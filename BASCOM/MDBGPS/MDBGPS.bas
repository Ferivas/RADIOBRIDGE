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

$version 0 , 1 , 66
$regfile = "m1284pdef.dat"
$crystal = 16000000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128
$projecttime = 31


'Declaracion de constantes
Const Numsta = 11                                           'Numero de estaciones
'Const Modbus_slave_adress = 4
Const Maximum_coil_number = 2                               ' Keep in mind that modbus adress start at 0 so this is 16 coils !
Const Maximum_discrete_inputs = 16                          ' Keep in mind that modbus adress start at 0 so this is 16 inputs !
Const Maximum_holding_registers = 72                        ' Keep in mind that modbus adress start at 0 so this is 16 registers !
Const Maximum_input_registers = 15                          ' Keep in mind that modbus adress start at 0 so this is 16 registers !
Const Maximum_adc_channel = 7                               ' Keep in mind that modbus adress start at 0 so this is 8 channels !
'********************************************************************

F_read_coil_status Alias 1
F_read_discrete_inputs Alias 2
F_read_holding_registers Alias 3
F_read_input_registers Alias 4
F_write_single_coil Alias 5
F_write_single_register Alias 6
F_write_multiple_coils Alias 15
F_write_multiple_registers Alias 16                         ' Not implemented yet
F_read_single_adc Alias 65                                  ' Not defined in standard, user applicable

'Exception response codes
Illegal_function Alias 1
Illegal_data_address Alias 2
Illegal_data_value Alias 3

'Configuracion de entradas/salidas
Led1 Alias Portb.0                                          'LED ROJO
Config Led1 = Output

'Ledmdb Alias PortC.2
'Config Ledmdb = Output

Enagps Alias Portc.1
Config Enagps = Output
Reset Enagps

'Pinbug Alias Portb.5
'Config Pinbug = Output

'Pinbug2 Alias Portb.6
'Config Pinbug2 = Output

'***************************************************************
' I/O configuration
' This is user configurable.
' Config PORT'S pin's to match modbus client hardware
' Inputs must start with Input_
' Outputs must start with Coil_
Input_1 Alias Pind.4                                        ' Input 1
Input_2 Alias Pind.5                                        ' Input 2
Input_3 Alias Pind.6                                        ' Input 3
Input_4 Alias Pind.7                                        ' Input 4
'(
Input_5 Alias Pinc.4                                        ' Input 5
Input_6 Alias Pinc.5                                        ' Input 6
Input_7 Alias Pinc.6                                        ' Input 7
Input_8 Alias Pinc.7                                        ' Input 8

Input_9 Alias Pinc.0                                        ' Input 9
Input_10 Alias Pinc.1                                       ' Input 10
Input_11 Alias Pinc.2                                       ' Input 11
Input_12 Alias Pinc.3                                       ' Input 12
Input_13 Alias Pinc.4                                       ' Input 13
Input_14 Alias Pinc.5                                       ' Input 14
Input_15 Alias Pinc.6                                       ' Input 15
Input_16 Alias Pinc.7                                       ' Input 16
')

Coil_1 Alias Portb.1                                        ' Coil 1
'(
Coil_2 Alias Porta.1                                        ' Coil 2
Coil_3 Alias Porta.2                                        ' Coil 3
Coil_4 Alias Porta.3                                        ' Coil 4
Coil_5 Alias Porta.4                                        ' Coil 5
Coil_6 Alias Porta.5                                        ' Coil 6
Coil_7 Alias Porta.6                                        ' Coil 7
Coil_8 Alias Porta.7                                        ' Coil 8
Coil_9 Alias Porta.6                                        ' Coil 9
Coil_10 Alias Porta.7                                       ' Coil 10
')
'********************************************************************

'****** Remark putput lines which are not in modbus client
Config Coil_1 = Output                                      ' Coil 1
'(
Config Coil_2 = Output                                      ' Coil 2
Config Coil_3 = Output                                      ' Coil 3
Config Coil_4 = Output                                      ' Coil 4
Config Coil_5 = Output                                      ' Coil 5
Config Coil_6 = Output                                      ' Coil 6
Config Coil_7 = Output                                      ' Coil 7
Config Coil_8 = Output                                      ' Coil 8
Config Coil_9 = Output                                      ' Coil 9
Config Coil_10 = Output                                     ' Coil 10
')

'**********************************************************************


'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024                     'Ints a 100Hz si Timer0=184
On Timer0 Int_timer0
Enable Timer0
Start Timer0

' TIMER1 should be configured for 3.5 character space based on baudrate speed
Config Com2 = Dummy , Synchrone = 0 , Parity = None , Stopbits = 1 , Databits = 8 , Clockpol = 0
Config Serialin1 = Buffered , Size = 16 , Bytematch = All
Config Timer1 = Timer , Prescale = 256

'Const Load_timer = &H006C
'Const Load_timer = &H012D
Const Load_timer = &H1476
'Const Load_timer = &H011F

Load Timer1 , Load_timer                                    'cca 3.5 ms
On Timer1 Modbus_space

Enable Timer1
Start Timer1

Config Print1 = Portb.2 , Mode = Set

Dir176 Alias Portb.2
Config Dir176 = Output
Reset Dir176


' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

' Puerto serial 2
Open "com2:" For Binary As #2
'On Urxc1 At_ser2
Enable Urxc1


Clear Serialin1

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "MDBGPS_archivos.bas"
$lib "modbus.lbx"



'Programa principal

Call Inivar()


Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      'Print #1 , "SER1=" ; Serproc
      Call Procser()
   End If

   If Inileerhr = 1 Then
      Reset Inileerhr
      For J = 1 To 72
         Print #1 , J ; "," ; Hex(holding_registers_table(j))
      Next

   End If

Loop