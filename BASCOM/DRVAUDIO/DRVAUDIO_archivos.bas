'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  SD_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile


'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Procser1()
Declare Sub Leersta()
Declare Sub Calcchk()
Declare Sub Wrdac(byval Dacval As Byte)
Declare Sub Rdadc(canal As Byte)
Declare Sub Cmddfp(byval Cmd As Byte , Byval Param As Word)
Declare Sub Espera(byval Tespera As Word)



'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte
Dim Tmpb2 As Byte
Dim Tmpb3 As Byte
Dim J As Byte
Dim Tmps As Single

Dim Datomp3 As Byte

Dim Tmpw As Word
Dim Tmpw2 As Word

Dim Cmdtmp As String * 6
Dim Atsnd As String * 200
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 52

Dim Status As Byte
Dim Statuseep As Eram Byte
Dim Statusant As Byte
Dim Cntrtest As Byte

Dim Initest As Bit
Dim Anaranja As Bit
Dim Ininormal As Bit
Dim Inisen As Bit

Dim Cntrmin As Byte


Dim Tmpchk As Word
Dim Tblcmdfp(12) As Byte


'Variables TIMER0
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim Inidelay As Bit
Dim Cntrdelay As Word
Dim T0delay As Bit
Dim Topdelay As Word


Dim T0cntr As Word
Dim T0tout As Bit , T0ini As Bit
Dim T0rate As Word
Dim T0cntrant As Word

'Variables TIMER2
Dim T2c As Byte
Dim Num_ventana2 As Byte
Dim Estado2 As Long
Dim Estado_dout As Byte
Dim Iluminar2 As Bit

'I2C
Dim Dacout As Byte
Dim Inidac As Bit
Dim Leeradc As Bit
Dim Adcin As Byte
Dim Numcanal As Byte

'Variables SERIAL1
Dim Ser_ini1 As Bit , Sernew1 As Bit
Dim Serrx1 As Byte , Ptrser1 As Byte
Const Numdata = 16
Dim Tblser1(numdata) As Byte
Dim Londata1 As Byte
Dim Loncmd As Byte

Dim Volumen As Byte

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(34) As String * 20
Dim Serdata As String * 200 , Serrx As Byte , Serproc As String * 200



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


Return

'*******************************************************************************
' Subrutina interrupcion de puerto serial 2
'*******************************************************************************
At_ser2:
   Serrx1 = Udr1

   Select Case Serrx1
      Case &H7E:
         Ser_ini1 = 1
         Ptrser1 = 0
         Incr Ptrser1
         Tblser1(ptrser1) = Serrx1


      Case &HEF:
            Incr Ptrser1
            Tblser1(ptrser1) = Serrx1
            Sernew1 = 1
            Londata1 = Ptrser1

      Case Else
         If Ser_ini1 = 1 Then
            Incr Ptrser1
            Tblser1(ptrser1) = Serrx1
            Ptrser1 = Ptrser1 Mod Numdata
         End If

   End Select

Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = 184
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      Toggle Iluminar
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

   If T0ini = 1 Then
      Incr T0cntr
      If T0cntr = T0rate Then
         Set T0tout
      End If
   Else
      T0cntr = 0
   End If

Return

'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer2:
   Timer2 = 184
   Incr T2c
   T2c = T2c Mod 100
   If T2c = 0 Then
      Num_ventana2 = Num_ventana2 Mod 32
      Estado2 = Lookup(estado_dout , Tabla_staout)
      Iluminar2 = Estado2.num_ventana2
      Toggle Iluminar2
      Dout = Iluminar2
      Incr Num_ventana2
   End If

Return




'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************

'*******************************************************************************
' Inicialización de variables
'*******************************************************************************
Sub Inivar()
Set Pwrmp3
Reset Led1
Reset Ena1
Reset Ena2

Print #1 , "************ DRIVER AUDIO ************"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)

   Tblcmdfp(1) = &H7E
   Tblcmdfp(2) = &HFF

Estado_led = 1
   Status = Statuseep
   Status = 1
   Print #1 , "Estado=" ; Status

End Sub

'*******************************************************************************
' WRITE DAC I2C
'*******************************************************************************

Sub Wrdac(byval Dacval As Byte)
   'I2cinit
   '   Waitms 500
   Print #1 , "wr dac"
   I2cstart
   I2cwbyte Pcf8591write
   I2cwbyte Pcf8591dacconfig
   I2cwbyte Dacval
   I2cstop
End Sub

Sub Rdadc(canal As Byte)
   Waitms 100
   Print #1 , "READ ADC"
   Print #1 ,"Canal=" ; Canal
   I2cstart
   ' I2cwbyte Pcf8591write
   I2cwbyte Canal
   I2cstart
   I2cwbyte Pcf8591read
   I2crbyte Adcin , Ack                                       ' Odczyt kilku bajtów
   I2crbyte Adcin , Nack                                      ' Odczyt ostatniego bajtu
   I2cstop
   Print #1 , "ADC" ; Canal ; "=" ; Hex(adcin) ; ",";
   Tmps = Adcin * 30
   Tmps = Tmps / 255
   Print #1 , Tmps
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


'*******************************************************************************
' Subrutina para enviar comandos al DFPlayer
Sub Cmddfp(byval Cmd As Byte , Byval Param As Word)
      Atsnd = "Comando a DFPlapyer=" + Hex(cmd) + ", PAR=" + Hex(param)
      Tblcmdfp(3) = 6
      Tblcmdfp(4) = Cmd
      Tblcmdfp(5) = 1
'               Print #1 , Hex(tmpw)
      Print #1 , Atsnd
      Tmpb3 = High(param)
      Print #1 , Hex(tmpb3)
      Tblcmdfp(6) = Tmpb3
      Tmpb3 = Low(param)
      Print #1 , Hex(tmpb3)
      Tblcmdfp(7) = Tmpb3
      Tblcmdfp(10) = &HEF
      Loncmd = 10
      Call Calcchk()
      For J = 1 To Loncmd
         Print #1 , Hex(tblcmdfp(j)) ; ";";
         Tmpb = Tblcmdfp(j)
         Printbin #2 , Tmpb
      Next
      Print #1,

End Sub



'*******************************************************************************
' SERIAL 1
'*******************************************************************************
Sub Procser1()
   Print #1 , "Resp. DFPlayer:"
   For Tmpb = 1 To Londata1
      'Print #1 , Tmpb ; "," ; Hex(tblser1(tmpb)) ; "," ; Tblser1(tmpb)
      Print #1 , Hex(tblser1(tmpb)) ; ",";
   Next
   Print #1,

End Sub


'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   Print #1 , "$" ; Serproc
   Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   If Numpar > 0 Then
      For Tmpb = 1 To Numpar
         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
      Next
   End If

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

         Case "ENAIN1"
            If Numpar = 1 Then
               Cmderr = 0
               Set Ena1
               Reset Ena2
               Atsnd = "Se habilito IN1 y deshabilito IN2"
            Else
               Cmderr = 4
            End If

         Case "ENAIN2"
            If Numpar = 1 Then
               Cmderr = 0
               Set Ena2
               Reset Ena1
               Atsnd = "Se habilito IN2 y deshabilito IN1"
            Else
               Cmderr = 4
            End If

         Case "DISIN1"
            If Numpar = 1 Then
               Cmderr = 0
               Reset Ena1
               Atsnd = "Se deshabilito IN1"
            Else
               Cmderr = 4
            End If

         Case "DISIN2"
            If Numpar = 1 Then
               Cmderr = 0
               Reset Ena2
               Atsnd = "Se deshabilito IN2"
            Else
               Cmderr = 4
            End If

         Case "SETDFP"
            If Numpar = 2 Then
               Cmderr = 0

               Tmpb2 = Hexval(cmdsplit(2))
               Atsnd = "Comando a DFPlapyer=" + Cmdsplit(2)
               Tblcmdfp(3) = 4
               Tblcmdfp(4) = Tmpb2
               Tblcmdfp(5) = 0
               Tblcmdfp(8) = &HEF
               Loncmd = 8
               Call Calcchk()
               For J = 1 To Loncmd
                  Print #1 , Hex(tblcmdfp(j)) ; ";";
                  Tmpb = Tblcmdfp(j)
                  Printbin #2 , Tmpb
               Next
               Print #1,

            End If

            If Numpar = 3 Then
               Cmderr = 0
               Tmpb2 = Hexval(cmdsplit(2))
               Tmpw = Hexval(cmdsplit(3))
               Atsnd = "Comando a DFPlapyer=" + Cmdsplit(2) + ", PAR=" + Cmdsplit(3)
               Tblcmdfp(3) = 6
               Tblcmdfp(4) = Tmpb2
               Tblcmdfp(5) = 1
               Print #1 , Hex(tmpw)
               Tmpb3 = High(tmpw)
               Print #1 , Hex(tmpb3)
               Tblcmdfp(6) = Tmpb3
               Tmpb3 = Low(tmpw)
               Print #1 , Hex(tmpb3)
               Tblcmdfp(7) = Tmpb3
               Tblcmdfp(10) = &HEF
               Loncmd = 10
               Call Calcchk()
               For J = 1 To Loncmd
                  Print #1 , Hex(tblcmdfp(j)) ; ";";
                  Tmpb = Tblcmdfp(j)
                  Printbin #2 , Tmpb
               Next
               Print #1,
            End If

         Case "SETI2C"
            If Numpar = 2 Then
               Cmderr = 0
               Dacout = Hexval(cmdsplit(2))
               Atsnd = " Salida DAC =" + Hex(dacout)
               Set Inidac
            Else
               Cmderr = 4
            End If

         Case "LEEADC"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 4 Then
                  Set Leeradc
                  Cmderr = 0
                  Numcanal = Tmpb
                  Atsnd = "Lectura ADC " + Str(numcanal)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETSTA"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 4 And Tmpb > 0 Then
                  Status = Tmpb
                  Statuseep = Tmpb
                  Atsnd = "Se configuro estado a " + Str(status)

               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETMP3"
            Cmderr = 0
            Atsnd = "Modulo  DXP ON"
            Reset Pwrmp3

         Case "RSTMP3"
            Cmderr = 0
            Atsnd = "Modulo  DXP OFF"
            Set Pwrmp3

         Case "SETSEN"
            Cmderr = 0
            Atsnd = "Test Onda 500 Hz"
            Set Inisen

         Case "RSTSEN"
            Cmderr = 0
            Atsnd = "Reset Onda 500 Hz"
            Reset Inisen

         Case "DRVAUD"
            Select Case Numpar
               Case 2:
                  Cmderr = 0
                  Tmpb = Val(cmdsplit(2))
                  Volumen = 20
                  If Tmpb < 4 And Tmpb > 0 Then
                     Status = Tmpb
                     Statuseep = Tmpb
                     Atsnd = "Se configuro estado DRVAUD a " + Str(status) + " con VOL=" + Str(volumen)
                  Else
                     Cmderr = 5
                  End If
               Case 3:
                  Cmderr = 0
                  Tmpb = Val(cmdsplit(2))
                  Volumen = Val(cmdsplit(3))
                  If Tmpb < 4 And Tmpb > 0 Then
                     Status = Tmpb
                     Statuseep = Tmpb
                     Atsnd = "Se configuro estado DRVAUD a " + Str(status) + " con VOL=" + Str(volumen)
                  Else
                     Cmderr = 5
                  End If
               Case Else
                  Cmderr = 4
            End Select

         Case "SETMIN"
            If Numpar = 2 Then
               Cmderr = 0
               Cntrmin = Val(cmdsplit(2))
               Atsnd = "Se configuro cntrmin " + Str(cntrmin)
            Else
               Cmderr = 4
            End If


         Case "LEEMIN"
            Cmderr = 0
            Atsnd = "Cntrmin " + Str(cntrmin)


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


'***********************************************************************************************
'Subrutina para leer estado asignado por PLC
'***********************************************************************************************
Sub Leersta()
'(
   Status = 0
   Status.0 = D0
   Status.1 = D1
   Status.2 = D2
')
   If Status <> Statusant Then
      Print #1 , "STATUS=" ; Status
      'Tmpb = Status + 1
      'Estado_led = Status
      Statusant = Status
      Select Case Status
         Case 1:                                            'NORMAL
            Estado_led = 1
            Reset Initest
            Reset Anaranja
            Set Ininormal

         Case 2:                                            ' TEST
            Estado_led = 2
            Set Initest
            Reset Anaranja
            Reset Ininormal

         Case 3:                                            'ALERTA NANANJA
            Estado_led = 3
            Reset Initest
            Set Anaranja
            Reset Ininormal

         Case Else
            Print #1 , "Estado no definido"

      End Select

   End If
End Sub

Sub Calcchk()
'7E FF 04 0E 00 FE EF EF
   If Loncmd = 8 Then
      Tmpchk = 0
      Tmpchk = Tmpchk + Tblcmdfp(2)                            'FF
      Tmpchk = Tmpchk + Tblcmdfp(3)                            '
      Tmpchk = Tmpchk + Tblcmdfp(4)
      Tmpchk = Tmpchk + Tblcmdfp(5)
      Tmpchk = 0 - Tmpchk
      Tblcmdfp(6) = High(tmpchk)
      Tblcmdfp(7) = Low(tmpchk)
   End If

   If Loncmd = 10 Then
      Tmpchk = 0
      Tmpchk = Tmpchk + Tblcmdfp(2)                            'FF
      Tmpchk = Tmpchk + Tblcmdfp(3)                            '
      Tmpchk = Tmpchk + Tblcmdfp(4)
      Tmpchk = Tmpchk + Tblcmdfp(5)
      Tmpchk = Tmpchk + Tblcmdfp(6)
      Tmpchk = Tmpchk + Tblcmdfp(7)
      Tmpchk = 0 - Tmpchk
      Tblcmdfp(8) = High(tmpchk)
      Tblcmdfp(9) = Low(tmpchk)
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
Data &B00000000001100110011001100110011&                    'Estado 6
Data &B00000001101100110011001100110011&                    'Estado 7
Data &B00011001101100110011001100110011&                    'Estado 8
Data &B00110011001100110011001100110011&                    'Estado 9
Data &B01110111011101110111011101110111&                    'Estado 10
Data &B11111111111111000000000000001100&                    'Estado 11
Data &B11111111111111000000000011001100&                    'Estado 12
Data &B11111111111111000000110011001100&                    'Estado 13
Data &B11111111111111001100110011001100&                    'Estado 14
Data &B11111111111111000000000000001100&                    'Estado 15
Data &B11111111111111111111111111110000&                    'Estado 16

Tabla_staout:
Data &B00000000000000000000000000000000&                    'Estado 0
Data &B00000000000000000000000000000001&                    'Estado 1
Data &B00000000000000000000000000000101&                    'Estado 2
Data &B00000000000000000000000000010101&                    'Estado 3
Data &B00000000000000000000000001010101&                    'Estado 4
Data &B00000000000000000000000101010101&                    'Estado 5
Data &B00000000000000000000010101010101&                    'Estado 6
Data &B00000000000000000001010101010101&                    'Estado 7
Data &B00000000000000000101010101010101&                    'Estado 8
Data &B00000000000000010101010101010101&                    'Estado 9
Data &B00000000000001110111011101110111&                    'Estado 10



Loaded_arch: