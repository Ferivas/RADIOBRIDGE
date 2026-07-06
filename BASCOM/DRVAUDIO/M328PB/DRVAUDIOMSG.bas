
'Main.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para almacenar los datos que se reciben por el puerto serial a una
' memoria SD
'


$version 0 , 1 , 194
$regfile = "m328pBdef.dat"
$crystal = 16000000
'$crystal = 8000000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128
$projecttime = 38


'Declaracion de constantes
Const Pcf8591write = &B10011100
Const Pcf8591read = &B10011101
Const Pcf8591dacconfig = &B01000000

Const Numcanali2c = 4
Const Numcanali2c_masuno = Numcanali2c + 1

Const Nummsg = 11
Const Nummsg_masuno = Nummsg + 1

'Configuracion de entradas/salidas
Led1 Alias Portb.2                                          'LED ROJO
Config Led1 = Output

Pwrmp3 Alias Portb.0
Config Pwrmp3 = Output

Ena1 Alias Portd.3
Config Ena1 = Output

Ena2 Alias Portd.4
Config Ena2 = Output



Test Alias Pind.2
Config Test = Input

Set Portd.2


Config Scl = Portc.5                                        ' we need to provide the SCL pin name
Config Sda = Portc.4                                        ' we need to provide the SDA pin name
$lib "i2c_twi.lbx"
Config Twi = 100000
I2cinit



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

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************
$include "DRVAUDIOMSG_archivos.bas"

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

   If Leeri2c = 1 Then
      Reset Leeri2c
      Print #1 , "READ I2C ADC"
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

   If Inirespdrv = 1 Then
      Reset Inirespdrv
      Print #1 , "$DRVAUD," ; Str(status) ; "," ; Str(volumen)

   End If


   If Initest = 1 Then
      Reset Initest
      Call Proctest()
   End If

   If Anaranja = 1 Then
      Reset Anaranja
      Call Procalerta()
   End If

   If Test = 0 Then
      Print #1 , "TEST por button"
'      Reset Ininormal
'      Reset Ena1
'      Reset Ena2
'      Call Wrdac(0)                                         ' Silencio amplificador
'      'Comando para silenciar reproduccion de audio
'      Set Pwrmp3                                            ' off dfp
'      Incr Cntrtest
'      If Cntrtest.0 = 1 Then
'         Status = 2
'         Volumen = Volumentst
'         Print #1 , "VOLtst=" ; Volumen
'      Else
'         Status = 1
'      End If
      Set Inimsg
      Msgtmp = 10
      Volumen = Volumentst

   End If

   If Inimsg = 1 Then
      Reset Inimsg
      'Volumen = Volumentst
      Print #1 , "Nuevo MSG " ; Msgtmp ; " con Vol " ; Volumen
      Call Repmsg()
      Reset Ena1
      Reset Ena2
      Call Wrdac(0)
      Set Pwrmp3
   End If

   Call Leersta()

Loop