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
Declare Sub Espera(byval Tespera As Word)                   'Subrutina para delay en multiplos de 10 ms
Declare Sub Procser()
Declare Sub Diskinsertion()
Declare Sub Disp_buffer(byval Ptrb As Word , Byval Cntr_seg As Word)
Declare Sub Leerdata()
Declare Sub Leersta()
Declare Sub Wrbufdata()
Declare Sub Rdbufdata()
Declare Sub Gendig(byval Valdig As Byte , Byval Posdig As Byte)
Declare Sub Gendp(byval Posdp As Byte)
Declare Sub Rdlinesd()
' Variables para leer DS18B20
Declare Sub Temperatura()
Declare Sub Leer_ds18b20()

Declare Sub Gendigt(byval Valdig As Byte , Byval Posdig As Byte)
Declare Sub Gensp(byval Posdp As Byte)
Declare Sub Gengrado(byval Posdp As Byte)



'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
'Generales
Dim Tmpb As Byte
Dim Tmpb2 As Byte
Dim Tmpbyte As Byte
Dim Tmpl As Long
Dim Cntr2 As Byte
Dim Tmpstr As String * 2

Dim Tmpl2 As Long , Tmpl3 As Long
Dim Tmplsd As Long
Dim Lonfile As Long
Dim Temp As Byte
Dim K As Byte
Dim K1 As Byte
Dim Kt As Byte
Dim Tmp3 As Byte
Dim Dlsb As Byte , Dmsb As Byte
Dim Tmpw As Word
Dim Tmpw2 As Word
Dim Tmpw3 As Word
Dim Tmpw4 As Word
Dim Tt As Word
Dim Cc As Byte
Dim Nn As Word
Dim M As Byte
Dim T As Long
Dim Statusant As Byte
Dim Ptrdig As Word
Dim Ptrpos As Word
Dim Kw As Word
Dim Ptrtx As Word
Dim Tmpwf As Word
Dim Tmps As Single

Dim Tmpptr As Word
Dim Cntrframe As Byte
Dim Newframe As Bit
Dim Tmpptrtx As Word

Dim Tbl_hora(6) As Byte
Dim Tbl_horaant(6) As Byte


Dim Horamin As Long
Dim Horamineep As Eram Long

Dim Iniespera As Bit

'Temperatura
Dim Crc As Byte
Dim T1 As Single , Bint As Integer , Tr As Byte , Ti As Byte
Dim Bt(9) As Byte
Dim Tempestr4 As String * 4 , Signo As String * 1 , Tempe As Byte
Dim Tbltemp(6) As Byte

Dim Cntrtemp As Byte , Stemp As Bit
Dim Cntrerrortemp As Byte
Dim Errortemp As Bit
Dim Cntrsegtemp As Byte , Initemp As Bit
'Variables SD
Dim Sdinitok As Bit
Dim Dirstr As String * 16
'Matriz
Dim Dato8 As Byte
Dim Buffram(longbuf) As Byte
Dim Sample As Byte

'TIMER0
Dim Cntr_col As Byte
Dim Kk As Byte
Dim Tmpwtx As Word
'Dim Tmpw3 As Word
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estadoled As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim T0frame As Byte


'TIMER2
Dim T2c As Byte
Dim Tmpsec As Long
Dim Newseg As Bit
Dim Cntrmin As Byte
Dim Hora As String * 10
Dim Fecha As String * 10

Dim Cmdtmp As String * 6
Dim Atsnd As String * 255
Dim Cmderr As Byte
Dim Tmpstr2 As String * 2
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 52
Dim Debugsd As Byte

Dim Cntrdata As Word                                        'Variable para almacenar numero de líneas de un archivo
Dim Cntrdataeep As Eram Word
Dim Numfile As Word                                         ' Almacena numero de archivo
Dim Numfileeep As Eram Word
Dim Numlineas As Word                                       'Almacena numero máximo de lineas
Dim Numlineaseep As Eram Word
Dim Filename As String * 16
Dim Filenameeep As Eram String * 16
Dim Cfgok As Byte
Dim Cfgokeep As Eram Byte

Dim Status As Byte                                          'Variable para almacenar el estado
Dim Aniflag As Bit                                          'Bandera para configurar animaciones
Dim Horaflag As Bit
Dim Tempflag As Bit
Dim Leertemp As Bit

'Dim Statuseep As Eram Byte

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(34) As String * 20
Dim Serdata As String * 255 , Serrx As Byte , Serproc As String * 255

'Variables SERIAL1
Dim Gps_ini As Bit , Gpsnew As Bit
Dim Gpsdata As String * 200 , Serrx1 As Byte , Gpsproc As String * 200

Dim Ptrbufdata As Word
Dim Cntrbufdata As Byte

Dim Buf1(9985) As Byte
'Dim Buf2(11) As Byte

'Dim Bufdata(6913) As Byte




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
   Serrx = Udr0
   Disable Timer0
'   Toggle Led1

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
'*******************************************************************************
At_ser2:
   Serrx1 = Udr1
   'Disable Timer0
'   Toggle Led2
   Select Case Serrx1
      Case "$":
         Gps_ini = 1
         Gpsdata = ""

      Case 13:
         If Gps_ini = 1 Then
            Gps_ini = 0
            Gpsdata = Gpsdata + Chr(0)
            Gpsproc = Gpsdata
            Gpsdata = ""
            If Mid(gpsproc , 1 , 5) = "GPRMC" Then
               Gpsnew = 1
               Disable Urxc1
            End If
    '        Enable Timer0
         End If

      Case Is > 31
         If Gps_ini = 1 Then
            Gpsdata = Gpsdata + Chr(serrx1)
         End If

    End Select

Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Tim0_isr:
   Timer0 = &HB5                                            '240hz  CON 18432000
   'Reset Oena2
   Select Case Cntr_col
      Case 0:
         Reset Sela
         Reset Selb
      Case 1:
         Set Sela
         Reset Selb
      Case 2:
         Reset Sela
         Set Selb
      Case 3:
         Set Sela
         Set Selb
   End Select
   Cntr_col = Cntr_col + 1

      'Set Pinbug

   Reset Oena
   Reset Oena2
   For Kk = 1 To Numtxser_2
      Tmpwtx = Lookup(kk , Tbl_txser)
      Tmpwtx = Tmpwtx + Cntr_col
      Dato8 = Buffram(tmpwtx)
      Dato8 = Dato8 Xor &HFF
      Shiftout Datos , Sck , Dato8 , 1
   Next
   Set Lena
   Reset Lena
   'Set Oena

   'Reset Oena2
   For Kk = 1 To Numtxser_2
      Tmpwtx = Lookup(kk , Tbl_tx2)
      Tmpwtx = Tmpwtx + Cntr_col
      Dato8 = Buffram(tmpwtx)
      Dato8 = Dato8 Xor &HFF
      Shiftout Datos , Sck , Dato8 , 1
   Next
   Set Lena2
   Reset Lena2
   Set Oena
   Set Oena2
   'Reset Pinbug
   Cntr_col = Cntr_col Mod 4

'   Incr T0frame
'   T0frame = T0frame Mod 120
'   If T0frame = 0 Then
'      Set Newframe

'   End If

'   Reset Pinbug
Return

'*******************************************************************************
'TIMER 2


Sectic:
   Set Newseg
   Incr Num_ventana
   'Num_ventana = Num_ventana Mod 16
   'Estadoled = Lookup(estado_led , Tabla_estado)
   'Iluminar = Estadoled.num_ventana
   If Sdinitok = 1 Then
      Toggle Iluminar
      Ledout = Iluminar
   Else
      Reset Ledout
   endif
   'Staout = Iluminar
   Incr Cntrsegtemp
   Cntrsegtemp = Cntrsegtemp Mod 3
   If Cntrsegtemp = 0 Then Set Initemp

Return

'*******************************************************************************
' SUBRUTINAS
'*******************************************************************************
Sub Inivar()
  ' Set Buzzer
  Reset Pwrsd
Print #1 , "************ DRIVER matriz LEDs P10(1R)-V701C ************"
Print #1 , Version(1)
Print #1 , Version(3)
Print #1 , "Nummatriz=" ; Nummatriz
Print #1 , "Longbuf=" ; Longbuf
Print #1 , "Longdat=" ; Longdat
Print #1 , "Longdat_mas_uno=" ; Longdat_mas_uno
Print #1 , "Longbuf_mas_uno=" ; Longbuf_mas_uno
Print #1 , "Numtxser=" ; Numtxser

For Tmpw4 = 1 To Longbuf
   Buffram(tmpw4) = 0
Next

Buffram(576) = &B01111110
Buffram(193) = &B01111110
Buffram(177) = &B01111110
Buffram(592) = &B01111110



Estado_led = 2

   Horamin = Horamineep
   Print #1 , "Last act CLK " ; Date(horamin) ; "," ; Time(horamin)
   'Tmplntp = Syssec(horamin)
   Tmpstr8 = Time(horamin)
   Time$ = Tmpstr8
   Print #1 , "Ts:" ; Tmpstr8 ; " T:" ; Time$
   Tmpstr8 = Date(horamin)
   Date$ = Tmpstr8
   Print #1 , "Ds:" ; Tmpstr8 ; " D:" ; Date$
   Print #1 , "H:" ; Time$ ; " D:" ; Date$
   Tmpsec = Syssec()
   Statusant = 99
   'Status = Statuseep
   'Print #1 , "STATUS=" ; Estatus
   For Tmpb = 1 To 6
      Tbl_horaant(tmpb) = 99
   Next
   Tempestr4 = "0.0"
   Signo = "-"

End Sub


'*****************************************************************************
' Subrutina para leer temperatura del DS18B20
'*****************************************************************************
Sub Temperatura()
   Call Leer_ds18b20()
   If Err = 1 Then                                          ' No hay sensor
      Print #1 , "T=Err"
      Incr Cntrerrortemp
      If Cntrerrortemp = 5 Then
         Set Errortemp
      End If
   Else
    If Crc = 0 Then                                         ' Se verifica si hay CRC
       Tempestr4 = Fusing(t1 , "#.#")
       Tempe = Val(tempestr4)
       Print #1 , "T=" ; Tempestr4
       Errortemp = 0
       Cntrerrortemp = 0
    Else
      Print #1 , "T=**"
    End If
   End If

End Sub

'*****************************************************************************
' Subrutina para leer temperatura del DS18B20
'*****************************************************************************
Sub Leer_ds18b20()
1wreset
1wwrite &HCC
1wwrite &H44
Waitms 800
'Call Rdramds18b20()

 1wreset                                                    ' reset the bus
 1wwrite &HCC
 1wwrite &HBE                                               ' read 9 data bytest
 For Tr = 1 To 9
    Bt(tr) = 1wread()
 Next                                                       ' read bytes in array
 1wreset

'Call Crcit                                                    'Check CRC

 Crc = 0
 For Ti = 1 To 9
 Tr = Crc Xor Bt(ti)
 Crc = Lookup(tr , Crc8)
 Next

If Crc = 0 Then                                             ' if is OK, calculate for
  Bint = Makeint(bt(1) , Bt(2))
  If Bt(2).3 = 0 Then                                       'Temp postiva
   T1 = Bint / 16
   Signo = "+"
  Else
   Bint = Not Bint                                          ' Comprobar esta subrutina
   Bint = Bint + 1
   T1 = Bint / 16
   Signo = "-"
  End If
End If

End Sub
'*****************************************************************************

'***********************************************************************************************
'Subrutina para leer datos de memoria SD
'***********************************************************************************************
Sub Leerdata()
   'Set Pinbug

'   Local Sample As Byte
      Get #3 , Sample , Tmplsd
      Line Input #3 , Atsnd
      Tmplsd = Loc(#3)
      Tmpstr8 = Mid(atsnd , 2 , 2)
      Numpar = Val(tmpstr8)
      If Numpar > 0 Then
         Tmpw3 = Lookup(numpar , Tbl_ptrbuf)
         For K = 1 To 32
            Kt = Lookup(k , Tbl_mid)
            Tmpstr8 = Mid(atsnd , Kt , 2)
            Tmpbyte = Hexval(tmpstr8)
            Incr Tmpw3
            Buffram(tmpw3) = Tmpbyte
         Next
      Else
         'Set Iniespera
         If Lonfile =< Tmplsd Then
            Tmplsd = 1
            Get #3 , Sample , 1
         End If
      End If

    'Reset Pinbug
End Sub

'***********************************************************************************************
'Subrutina para leer estado asignado por PLC
'***********************************************************************************************
Sub Leersta()
   Status = 0
   Status.0 = Sta0
   Status.1 = Sta1
   Status.2 = Sta2
   Status.3 = Sta3
   'Toggle Status.0
   'Toggle Status.1
   'Toggle Status.2
   'Toggle Status.3


   If Status <> Statusant Then
      Print #1 , "STATUS=" ; Status
      'Tmpb = Status + 1
      'Estado_led = Status
      Statusant = Status
      Select Case Status
         Case 0:
            Filename = "Ani1.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 1:
            Filename = "Ani2.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 2:
            Filename = "Ani3.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 3:
            Filename = "Ani4.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 4:                                            'Muestra Temperatura
            Print #1 , "MUESTRA TEMPERATURA"
           Reset Aniflag
           Set Tempflag
           Reset Horaflag

         Case 5:
            Filename = "Ani5.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 6:                                            'Muestra la hora
            Print #1 , "MUESTRA HORA"
            Reset Aniflag
            Reset Tempflag
            Set Horaflag

         Case 7:                                            'Apaga todo
            Print #1 , "APAGA TODO"
            Reset Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 8:
            Filename = "Ani6.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 9:
            Filename = "Ani7.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 10:
            Filename = "Ani8.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 11:                                           'Apaga todo
            Print #1 , "APAGA TODO"
            Reset Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 12:
            Filename = "Ani9.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 13:
            Filename = "Ani10.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 14:
            Filename = "Ani11.txt"
            Set Aniflag
            Reset Tempflag
            Reset Horaflag

         Case 15:                                           'Apaga todo
            Print #1 , "APAGA TODO"
            Reset Aniflag
            Reset Tempflag
            Reset Horaflag

      End Select

      Close #3

      If Aniflag = 0 And Tempflag = 0 And Horaflag = 0 Then
         Disable Timer0
         Enable Urxc1
         Set Leertemp
         Reset Oena
         Reset Oena2

      Else
         Enable Timer0
         Disable Urxc1
         Reset Leertemp
      End If

      If Aniflag = 1 Then
         Disable Urxc1
         Open Filename For Binary As #3
         Lonfile = Lof(#3)
         Print #1 , "L>" ; Lonfile
         Print #1 , "L>" ; Filename

         T = Lof(#3)                                        'Returns the length of the File in Bytes
         Print #1 , "LOF:" ; T
         Tmplsd = 1
         Tmpptr = 0

         'Estado_led = Status + 1

         'Set Pinbug
         Print #1 , "INIwr"
         'Call Wrbufdata()
         'Reset Pinbug
         Print #1 , "FINwr"

      End If

      For Kw = 1 To 768
         Buffram(kw) = 0
      Next


      For Kw = 1 To 7681
         Buf1(kw) = 0
      Next

      For Tmpb = 1 To 6                                     ' Para que el reloj siempre empiece con datos
         Tbl_horaant(tmpb) = 99
      Next

      Tmpptrtx = 0

   End If



End Sub

'SUBRUTINA PARA ESCRIBIR A BUFDATA
Sub Wrbufdata()
   Close #3
   Open Filename For Binary As #3
   Lonfile = Lof(#3)
   'Print #1 , "Lw>" ; Lonfile
   'Print #1 , "Lw>" ; Filename

   T = Lof(#3)
   'Print #1 , "LOFw:" ; T
   Tmplsd = 1
   Tmpw3 = 0
   Tmpb = 0

   Do
      Get #3 , Sample , Tmplsd
      Line Input #3 , Atsnd
      'Print #1 , "ATSND=" ; Atsnd
      Tmplsd = Loc(#3)
      Tmpstr8 = Mid(atsnd , 2 , 2)
      Numpar = Val(tmpstr8)
      If Numpar > 0 Then
         For K = 1 To 32
            Kt = Lookup(k , Tbl_mid)
            Tmpstr8 = Mid(atsnd , Kt , 2)
            Tmpbyte = Hexval(tmpstr8)
            Incr Tmpw3
            'Bufdata(tmpw3) = Tmpbyte
         Next
      Else
         If Lonfile =< Tmplsd Then
            Tmplsd = 1
            Tmpb = 1
            Get #3 , Sample , 1
         End If
      End If
   Loop Until Tmpb = 1 Or Tmpw3 > 6144

End Sub

'SUBRUTINA PARA LEER UNA LINEA DE LA SD
Sub Rdlinesd()
   Get #3 , Sample , Tmplsd
   Line Input #3 , Atsnd
   Tmplsd = Loc(#3)                                         'Returns the position of last read or written Byte of the file
   If Lonfile =< Tmplsd Then
      Tmplsd = 1
      Tmpb = 1
      Get #3 , Sample , 1
   End If

   Tmpstr8 = Mid(atsnd , 2 , 2)
'   Print #1 , Atsnd

   Numpar = Val(tmpstr8)
   If Numpar > 0 Then
'      If Cntrframe.0 = 0 Then
        ' Set Pinbug
         Kt = 5
         For K = 1 To 32
            'Kt = Lookup(k , Tbl_mid)
            'Kt = Kt + 3
            Tmpstr8 = Mid(atsnd , Kt , 2)
            Kt = Kt + 3
            Tmpbyte = Hexval(tmpstr8)
            Tmpw3 = Tmpptr + K
            Buf1(tmpw3) = Tmpbyte
            'Print #1 , "B1(" ; Tmpw3 ; ")=" ; Hex(tmpbyte)
         Next
         'Reset Pinbug
'      Else
'         For K = 1 To 32
'            Kt = Lookup(k , Tbl_mid)
'            Tmpstr8 = Mid(atsnd , Kt , 2)
'            Tmpbyte = Hexval(tmpstr8)
'            Tmpw3 = Tmpptr + K
'            Buf2(tmpw3) = Tmpbyte
            'Print #1 , "B2(" ; Tmpw3 ; ")=" ; Hex(tmpbyte)
'         Next
'      End If
      Tmpptr = Tmpw3
      Tmpwf = Tmpw3 Mod Longbuf
      If Tmpwf = 0 Then
       'Toggle Pinbug2
       Set Newframe
      End If

      If Tmpptr = 9984 Then
         'Incr Cntrframe
         'Toggle Pinbug2
         'Set Newframe
         Tmpptr = 0
         'Tmpptrtx = 0
      End If


   End If


End Sub


'SUBRUTINA PARA ESCRIBIR A BUFDATA
Sub Rdbufdata()
   Ptrbufdata = Cntrbufdata * 768
   'Print #1 , "INI=" ; Ptrbufdata
   For Tmpw = 1 To 768
      Incr Ptrbufdata
      'Buffram(tmpw) = Bufdata(ptrbufdata)
   Next
  ' Print #1 , "FIN=" ; Ptrbufdata
   Incr Cntrbufdata
   Cntrbufdata = Cntrbufdata Mod 8
   'Set Iniespera
End Sub

'*******************************************************************************
'  Subrutina para pintar digito en posicion predeterminada
'  Los digitos son de 32x24 pixels y se puden posicionar en 6 posiciones
'  Valdig indica el valor del digito (1-9) y posdig la posicion (1-6)
'
Sub Gendig(byval Valdig As Byte , Byval Posdig As Byte)
   Local Kt As Byte
   Ptrdig = Valdig * 96
   Posdig = Posdig - 1
   Ptrpos = Posdig * 96

   For Kt = 1 To 96
      K = Kt - 1
      Tmpw3 = K + Ptrpos
      Tmpw = Lookup(tmpw3 , Tbl_posdig1)
      Tmpw2 = Ptrdig + K
      Tmpb2 = Lookup(tmpw2 , Tbl_char)
      Buffram(tmpw) = Tmpb2
   Next

End Sub


Sub Gendigt(byval Valdig As Byte , Byval Posdig As Byte)
   Local Kt As Byte
   Ptrdig = Valdig * 96
   Posdig = Posdig - 1
   Ptrpos = Posdig * 96

   For Kt = 1 To 96
      K = Kt - 1
      Tmpw3 = K + Ptrpos
      Tmpw = Lookup(tmpw3 , Tbl_post)
      Tmpw2 = Ptrdig + K
      Tmpb2 = Lookup(tmpw2 , Tbl_chart)
      Buffram(tmpw) = Tmpb2
   Next

End Sub

Sub Gendp(byval Posdp As Byte)
   Local Kp As Byte
   Posdp = Posdp - 1
   Ptrpos = Posdp * 32

   For Kp = 1 To 32
      K = Kp - 1
      Tmpw3 = K + Ptrpos
      Tmpw = Lookup(tmpw3 , Tbl_posdp)
      'Tmpw2 = Ptrdig + K
      Tmpb2 = Lookup(k , Tbl_dosp)
      Buffram(tmpw) = Tmpb2
   Next

End Sub

Sub Gengrado(byval Posdp As Byte)
   Local Kp As Byte
   Posdp = Posdp - 1
   Ptrpos = Posdp * 32

   For Kp = 1 To 32
      K = Kp - 1
      Tmpw3 = K + Ptrpos
      Tmpw = Lookup(tmpw3 , Tbl_posgrado)
      'Tmpw2 = Ptrdig + K
      Tmpb2 = Lookup(k , Tbl_grado)
      Buffram(tmpw) = Tmpb2
   Next

End Sub


Sub Gensp(byval Posdp As Byte)
   Local Kp As Byte
   Posdp = Posdp - 1
   Ptrpos = Posdp * 32

   For Kp = 1 To 32
      K = Kp - 1
      Tmpw3 = K + Ptrpos
      Tmpw = Lookup(tmpw3 , Tbl_possp)
      'Tmpw2 = Ptrdig + K
      Tmpb2 = Lookup(k , Tbl_sp)
      Buffram(tmpw) = Tmpb2
   Next

End Sub


'*******************************************************************************
Sub Disp_buffer(byval Ptrb As Word , Byval Cntr_seg As Word)
   Tt = Ptrb - 1
   Tt = Tt Mod 8
   Nn = Ptrb - 1
   For Cc = 1 To 16
      Tmpw2 = Nn / 8
      Tmpw2 = Tmpw2 * 16
      Tmpw2 = Tmpw2 + 17
      Tmpw2 = Tmpw2 - Cc
      M = Cc - 1
      If Cntr_seg.m = 1 Then
         Set Buffram(tmpw2). Tt
      Else
         Reset Buffram(tmpw2). Tt
      End If
   Next

End Sub



'*******************************************************************************
'Subrutina de espera
'*******************************************************************************
Sub Espera(byval Tespera As Word)
'(
   Topdelay = Tespera
   Cntrdelay = 0
   Reset T0delay
   Set Inidelay
   While T0delay = 0
      Reset Watchdog
   Wend
')
End Sub

'*******************************************************************************
'Subrutinas SD
'*******************************************************************************


Sub Diskinsertion()
   Local Errorcode As Byte
   If Debugsd = 1 Then
      Print #1 , "Ini SD"
   End If

   Gbdriveerror = Driveinit()
   If Gbdriveerror = 0 Then
         If Debugsd = 1 Then
           Print #1 , "Ini File System"
         End If

        ' Select partition 1 (or use 0 for drive without Master Boot Record)
        Errorcode = Initfilesystem(1)
        If Errorcode <> 0 Then
            Print #1 , "Error: " ; Errorcode ; " while initializing file system"
            Print #1 , "Errortext can't be saved at SD"
            Reset Sdinitok
        Else
         If Debugsd = 1 Then
            Print #1 , "Filesystem type: " ; Gbfilesystem
            Print #1 , "FAT Start Sector: " ; Glfatfirstsector
            Print #1 , "Root Start Sector: " ; Glrootfirstsector
            Print #1 , "Data First Sector: " ; Gldatafirstsector
            Print #1 , "Max. Cluster Nummber: " ; Glmaxclusternumber
            Print #1 , "Sectors per Cluster: " ; Gbsectorspercluster
            Print #1 , "Root Entries: " ; Gwrootentries
            Print #1 , "Sectors per FAT: " ; Glsectorsperfat
            Print #1 , "Number of FATs: " ; Gbnumberoffats
            Print #1 , "Disk size: " ; Disksize() ; " kB"
         Else
            Print #1 , "SD OK"
         End If
'            Diskinitialized = True
            Set Sdinitok
        End If
   Else
       Print #1 , "Error en Drive Init: " ; Gbdriveerror
       Print #1 , "Errortext can't be saved at SD"
       Reset Sdinitok
   End If
   Print #1 , ""



End Sub



'*******************************************************************************
'Subrutina de procesamiento de datos seriales
'*******************************************************************************
Sub Procser()
   Print #1 , "$" ; Serproc
   Locate 1 , 10
   Tmpstr52 = Mid(serproc , 1 , 6)
   Lcd Chr(3) ; Tmpstr52
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
            Atsnd = "Version FW: Fecha <"
            Tmpstr52 = Version(1)
            Atsnd = Atsnd + Tmpstr52 + ">, Archivo <"
            Tmpstr52 = Version(3)
            Atsnd = Atsnd + Tmpstr52 + ">"
            Cmderr = 0

         Case "LSTFIL"
            Cmderr = 0
            Call Diskinsertion()
            If Sdinitok = 1 Then
               Atsnd = "Archivos SD en DIR <" + Tmpstr52 + ">"
               'Print #1 , "Lista Archivos en DIR <" ; Tmpstr52 ; ">"
               Chdir "\"                                    ' Para ir al directorio raiz
               If Len(tmpstr52) > 0 Then
                  Chdir Tmpstr52
               End If
               Dirstr = Dir( "*.*")
               If Len(dirstr) > 0 Then
                  While Len(dirstr) > 0                     ' if there was a file found
                     Print #1 , Dirstr ; "," ; "LOF=" ; Filelen(dirstr) ; "," ; Filedate(dirstr) ; "," ; Filetime(dirstr)
                     Reset Watchdog
                     Dirstr = Dir()
                  Wend
               End If
            Else
               Cmderr = 9
            End If

            Case "LEELOG"
               If Numpar = 2 Then
                  Cmderr = 0
                  Print #1 , "LOG a " ; Date$ ; "," ; Time$
                  Disable Urxc
                  Call Diskinsertion()
                  If Sdinitok = 1 Then
                     Atsnd = "Lectura Log"
                     'Tmpstr8 = "LogSD.txt"
                     Tmpstr8 = Cmdsplit(2)
                     Open Tmpstr8 For Binary As #3
                     Tmpl2 = Lof(#3)
                     Print #1 , "LOF=" ; Tmpl2
                     Atsnd = "Datos SD"
                     For Tmpl3 = 1 To Tmpl2
                           Get #3 , Tmpb
                           Print #1 , Chr(tmpb);
                           'Call Espera(30)
                           Reset Watchdog
                     Next
                     Print #1,
                     Close #3
                     Enable Urxc
                  Else
                     Cmderr = 9
                  End If
               Else
                  Cmderr = 4
               End If

            Case "DELLOG"

               If Numpar = 2 Then
                  Cmderr = 0
                  Atsnd = "Borra Archivo Log"
                  'Tmpstr8 = "LogSD.txt"
                  Tmpstr8 = Cmdsplit(2)
                  Open Tmpstr8 For Binary As #3
                  Reset Watchdog
                  Tmpl2 = Lof(#3)
                  Close #3
                  Kill Tmpstr8
                  Flush #3
               Else
                  Cmderr = 4
                End If


            Case "SETBUG"
               Cmderr = 0
               Atsnd = "Se activa debug en SD"
               Debugsd = 1

            Case "RSTBUG"
               Cmderr = 0
               Atsnd = "Se desactiva debug en SD"
               Debugsd = 0

            Case "SETCLK"
               If Numpar = 2 Then
                  Cmderr = 0
                  Tmpstr8 = Mid(cmdsplit(2) , 7 , 2) + "/" + Mid(cmdsplit(2) , 9 , 2) + "/" + Mid(cmdsplit(2) , 11 , 2)
                  Time$ = Tmpstr8
                  Tmpstr8 = Mid(cmdsplit(2) , 1 , 2) + ":" + Mid(cmdsplit(2) , 3 , 2) + ":" + Mid(cmdsplit(2) , 5 , 2)
                  Date$ = Tmpstr8
                  'Print #1 , "D>" ; Date$
                  Tmpstr8 = ""
                  Atsnd = "WATCHING INFORMA. Se configuro reloj en " + Date$ + " a " + Time$

               Else
                  Cmderr = 4
               End If

            Case "LEECLK"
               Cmderr = 0
               Tmpstr8 = Time$
               Atsnd = "Reloj sistema actual " + Tmpstr8
               Tmpstr8 = Date$
               Atsnd = Atsnd + " de " + Tmpstr8

            Case "INITSD"
               Cmderr = 0
               Atsnd = "Inicializacion de la SD"
               Call Diskinsertion()

            Case "NUMLIN"
               If Numpar = 2 Then
                  If Val(cmdsplit(2)) < 1000 Then
                     Cmderr = 0
                     Numlineas = Val(cmdsplit(2))
                     Numlineaseep = Numlineas
                     Atsnd = "Se configuro numero maximo de lineas a " + Str(numlineas)

                  Else
                     Cmderr = 5

                  End If
               Else
                  Cmderr = 4
               End If

            Case "LEELIN"
               Cmderr = 0
               Atsnd = "Numero maximo de lineas=" + Str(numlineas)

            Case "NUMFIL"
               If Numpar = 2 Then
                  If Val(cmdsplit(2)) > 0 Then
                     Cmderr = 0
                     Numfile = Val(cmdsplit(2))
                     Numfileeep = Numfile
                     Atsnd = "Se configuro numero de archivo a " + Str(numfile)

                  Else
                     Cmderr = 5

                  End If
               Else
                  Cmderr = 4
               End If

            Case "LEEFIL"
               Cmderr = 0
               Atsnd = "Numero de archivo =" + Str(numfile)


            Case "RSTVAR"
               Cmderr = 0
               Numlineas = 1000
               Numlineaseep = Numlineas
               Numfile = 1
               Numfileeep = Numfile
               Atsnd = "Inicio variables: Numlineas=" + Str(numlineas) + ", Numfile=" + Str(numfile)
               Cntrdata = 0
               Cntrdataeep = Cntrdata
               Filename = "SD1.txt"
               Filenameeep = Filename
               Cfgok = 1
               Cfgokeep = Cfgok


            Case "SETBUF"
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               If Tmpw < Longbuf_mas_uno Then
                  Cmderr = 0
                  Tmpb = Hexval(cmdsplit(3))
                  Buffram(tmpw) = Tmpb
                  Atsnd = "Buffram(" + Str(tmpw) + ") = &H " + Hex(tmpb)
               Else
                  Cmderr = 5
               End If

            Case "SETCER"
               Cmderr = 0
               Atsnd = "Encerar buffer"
               For Tmpw4 = 1 To Longbuf
                  'Incr Tmpw3
                  'Call Disp_buffer(tmpw4 , Tmpw3)
                  Buffram(tmpw4) = 0
               Next

            Case "SETLED"
               Cmderr = 0
               Tmpb = Val(cmdsplit(2))
               Estado_led = Tmpb
               Atsnd = "Se configuro estado LED " + Str(tmpb)

            Case "SETDIG"
               If Numpar = 3 Then
                  Cmderr = 0
                  Tmpb = Val(cmdsplit(2))
                  Tmpb2 = Val(cmdsplit(3))
                  Atsnd = "Test Digito " + Str(tmpb)
                  Call Gendig(tmpb , Tmpb2)
               Else
                  Cmderr = 5
               End If

            Case "SETDOP"
               Cmderr = 0
               Tmpb = Val(cmdsplit(2))
               Atsnd = "Test DOS PUNTOS EN POS  " + Str(tmpb)
               Call Gendp(tmpb)

            Case "ENAGPS"
               Cmderr = 0
               Atsnd = "Habilita Ints. Serial 2 GPS"
               Enable Urxc1

            Case "DISGPS"
               Cmderr = 0
               Atsnd = "Deshabilita Ints. Serial 2 GPS"
               Disable Urxc1

            Case "SETTEM"
               If Numpar = 2 Then
                  Cmderr = 0
                  Tmps = Val(cmdsplit(2))
                  If Tmps > 0 Then
                     Signo = "+"
                  Else
                     Signo = "-"
                  End If
                  Tempestr4 = Fusing(tmps , "##.#")
                  Atsnd = "Se configura valor Temp=" + Tempestr4

               Else
                  Cmderr = 5
               End If


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
'TABLA DE DATOS
'*******************************************************************************
Tbl_ind:
Data 0                                                      '
Data 4                                                      '1
Data 8                                                      '2
Data 12                                                     '3
Data 16                                                     '4
Data 20                                                     '5
Data 24                                                     '6
Data 28                                                     '7
Data 32
Data 36
Data 40
Data 44
Data 48
Data 52                                                     '13
Data 56                                                     '14
Data 60                                                     '15
Data 64                                                     '16


'*******************************************************************************
'TABLA DE DATOS
'*******************************************************************************
Tabla_estado:
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

'******************************************************************************
' Tabla CRC Temperatura
Crc8:
Data 0 , 94 , 188 , 226 , 97 , 63 , 221 , 131 , 194 , 156
Data 126 , 32 , 163 , 253 , 31 , 65 , 157 , 195 , 33 , 127
Data 252 , 162 , 64 , 30 , 95 , 1 , 227 , 189 , 62 , 96
Data 130 , 220 , 35 , 125 , 159 , 193 , 66 , 28 , 254 , 160
Data 225 , 191 , 93 , 3 , 128 , 222 , 60 , 98 , 190 , 224
Data 2 , 92 , 223 , 129 , 99 , 61 , 124 , 34 , 192 , 158
Data 29 , 67 , 161 , 255 , 70 , 24 , 250 , 164 , 39 , 121
Data 155 , 197 , 132 , 218 , 56 , 102 , 229 , 187 , 89 , 7
Data 219 , 133 , 103 , 57 , 186 , 228 , 6 , 88 , 25 , 71
Data 165 , 251 , 120 , 38 , 196 , 154 , 101 , 59 , 217 , 135
Data 4 , 90 , 184 , 230 , 167 , 249 , 27 , 69 , 198 , 152
Data 122 , 36 , 248 , 166 , 68 , 26 , 153 , 199 , 37 , 123
Data 58 , 100 , 134 , 216 , 91 , 5 , 231 , 185 , 140 , 210
Data 48 , 110 , 237 , 179 , 81 , 15 , 78 , 16 , 242 , 172
Data 47 , 113 , 147 , 205 , 17 , 79 , 173 , 243 , 112 , 46
Data 204 , 146 , 211 , 141 , 111 , 49 , 178 , 236 , 14 , 80
Data 175 , 241 , 19 , 77 , 206 , 144 , 114 , 44 , 109 , 51
Data 209 , 143 , 12 , 82 , 176 , 238 , 50 , 108 , 142 , 208
Data 83 , 13 , 239 , 177 , 240 , 174 , 76 , 18 , 145 , 207
Data 45 , 115 , 202 , 148 , 118 , 40 , 171 , 245 , 23 , 73
Data 8 , 86 , 180 , 234 , 105 , 55 , 213 , 139 , 87 , 9
Data 235 , 181 , 54 , 104 , 138 , 212 , 149 , 203 , 41 , 119
Data 244 , 170 , 72 , 22 , 233 , 183 , 85 , 11 , 136 , 214
Data 52 , 106 , 43 , 117 , 151 , 201 , 74 , 20 , 246 , 168
Data 116 , 42 , 200 , 150 , 21 , 75 , 169 , 247 , 182 , 232
Data 10 , 84 , 215 , 137 , 107 , 53
'*****************************************************************************


Tbl_ptrbuf:
Data 0%                                                     '0
Data 0%                                                     '1
Data 32%                                                    '2
Data 64%
Data 96%
Data 128%
Data 160%
Data 192%
Data 224%
Data 256%
Data 288%
Data 320%
Data 352%
Data 384%
Data 416%
Data 448%
Data 480%
Data 512%
Data 544%
Data 576%
Data 608%
Data 640%
Data 672%
Data 704%
Data 736%


Tbl_mid:
Data 0
Data 5
Data 8
Data 11
Data 14
Data 17
Data 20
Data 23
Data 26
Data 29
Data 32
Data 35
Data 38
Data 41
Data 44
Data 47
Data 50
Data 53
Data 56
Data 59
Data 62
Data 65
Data 68
Data 71
Data 74
Data 77
Data 80
Data 83
Data 86
Data 89
Data 92
Data 95
Data 98

Tbl_txser:
Data 0%                                                     ' Dummy Data
Data 380%
Data 376%
Data 372%
Data 368%
Data 364%
Data 360%
Data 356%
Data 352%
Data 348%
Data 344%
Data 340%
Data 336%
Data 332%
Data 328%
Data 324%
Data 320%
Data 316%
Data 312%
Data 308%
Data 304%
Data 300%
Data 296%
Data 292%
Data 288%
Data 284%
Data 280%
Data 276%
Data 272%
Data 268%
Data 264%
Data 260%
Data 256%
Data 252%
Data 248%
Data 244%
Data 240%
Data 236%
Data 232%
Data 228%
Data 224%
Data 220%
Data 216%
Data 212%
Data 208%
Data 204%
Data 200%
Data 196%
Data 192%
Data 188%
Data 184%
Data 180%
Data 176%
Data 172%
Data 168%
Data 164%
Data 160%
Data 156%
Data 152%
Data 148%
Data 144%
Data 140%
Data 136%
Data 132%
Data 128%
Data 124%
Data 120%
Data 116%
Data 112%
Data 108%
Data 104%
Data 100%
Data 96%
Data 92%
Data 88%
Data 84%
Data 80%
Data 76%
Data 72%
Data 68%
Data 64%
Data 60%
Data 56%
Data 52%
Data 48%
Data 44%
Data 40%
Data 36%
Data 32%
Data 28%
Data 24%
Data 20%
Data 16%
Data 12%
Data 8%
Data 4%
Data 0%


Tbl_tx2:
Data 0%
Data 764%
Data 760%
Data 756%
Data 752%
Data 748%
Data 744%
Data 740%
Data 736%
Data 732%
Data 728%
Data 724%
Data 720%
Data 716%
Data 712%
Data 708%
Data 704%
Data 700%
Data 696%
Data 692%
Data 688%
Data 684%
Data 680%
Data 676%
Data 672%
Data 668%
Data 664%
Data 660%
Data 656%
Data 652%
Data 648%
Data 644%
Data 640%
Data 636%
Data 632%
Data 628%
Data 624%
Data 620%
Data 616%
Data 612%
Data 608%
Data 604%
Data 600%
Data 596%
Data 592%
Data 588%
Data 584%
Data 580%
Data 576%
Data 572%
Data 568%
Data 564%
Data 560%
Data 556%
Data 552%
Data 548%
Data 544%
Data 540%
Data 536%
Data 532%
Data 528%
Data 524%
Data 520%
Data 516%
Data 512%
Data 508%
Data 504%
Data 500%
Data 496%
Data 492%
Data 488%
Data 484%
Data 480%
Data 476%
Data 472%
Data 468%
Data 464%
Data 460%
Data 456%
Data 452%
Data 448%
Data 444%
Data 440%
Data 436%
Data 432%
Data 428%
Data 424%
Data 420%
Data 416%
Data 412%
Data 408%
Data 404%
Data 400%
Data 396%
Data 392%
Data 388%
Data 384%

Tbl_posdig1:
Data 737% , 721% , 705%,
Data 738% , 722% , 706%,
Data 739% , 723% , 707%,
Data 740% , 724% , 708%,
Data 741% , 725% , 709%,
Data 742% , 726% , 710%,
Data 743% , 727% , 711%,
Data 744% , 728% , 712%,
Data 745% , 729% , 713%,
Data 746% , 730% , 714%,
Data 747% , 731% , 715%,
Data 748% , 732% , 716%,
Data 749% , 733% , 717%,
Data 750% , 734% , 718%,
Data 751% , 735% , 719%,
Data 752% , 736% , 720%,
Data 353% , 337% , 321%,
Data 354% , 338% , 322%,
Data 355% , 339% , 323%,
Data 356% , 340% , 324%,
Data 357% , 341% , 325%,
Data 358% , 342% , 326%,
Data 359% , 343% , 327%,
Data 360% , 344% , 328%,
Data 361% , 345% , 329%,
Data 362% , 346% , 330%,
Data 363% , 347% , 331%,
Data 364% , 348% , 332%,
Data 365% , 349% , 333%,
Data 366% , 350% , 334%,
Data 367% , 351% , 335%,
Data 368% , 352% , 336%,

Tbl_posdig2:
Data        689%, 673%, 657%,
Data        690%, 674%, 658%,
Data        691%, 675%, 659%,
Data        692%, 676%, 660%,
Data        693%, 677%, 661%,
Data        694%, 678%, 662%,
Data        695%, 679%, 663%,
Data        696%, 680%, 664%,
Data        697%, 681%, 665%,
Data        698%, 682%, 666%,
Data        699%, 683%, 667%,
Data        700%, 684%, 668%,
Data        701%, 685%, 669%,
Data        702%, 686%, 670%,
Data        703%, 687%, 671%,
Data        704%, 688%, 672%,
Data        305%, 289%, 273%,
Data        306%, 290%, 274%,
Data        307%, 291%, 275%,
Data        308%, 292%, 276%,
Data        309%, 293%, 277%,
Data        310%, 294%, 278%,
Data        311%, 295%, 279%,
Data        312%, 296%, 280%,
Data        313%, 297%, 281%,
Data        314%, 298%, 282%,
Data        315%, 299%, 283%,
Data        316%, 300%, 284%,
Data        317%, 301%, 285%,
Data        318%, 302%, 286%,
Data        319%, 303%, 287%,
Data 320% , 304% , 288%,

Tbl_posdig3:
Data 609% , 593% , 577%,
Data 610% , 594% , 578%,
Data 611% , 595% , 579%,
Data 612% , 596% , 580%,
Data 613% , 597% , 581%,
Data 614% , 598% , 582%,
Data 615% , 599% , 583%,
Data 616% , 600% , 584%,
Data 617% , 601% , 585%,
Data 618% , 602% , 586%,
Data 619% , 603% , 587%,
Data 620% , 604% , 588%,
Data 621% , 605% , 589%,
Data 622% , 606% , 590%,
Data 623% , 607% , 591%,
Data 624% , 608% , 592%,
Data 225% , 209% , 193%,
Data 226% , 210% , 194%,
Data 227% , 211% , 195%,
Data 228% , 212% , 196%,
Data 229% , 213% , 197%,
Data 230% , 214% , 198%,
Data 231% , 215% , 199%,
Data 232% , 216% , 200%,
Data 233% , 217% , 201%,
Data 234% , 218% , 202%,
Data 235% , 219% , 203%,
Data 236% , 220% , 204%,
Data 237% , 221% , 205%,
Data 238% , 222% , 206%,
Data 239% , 223% , 207%,
Data 240% , 224% , 208%,

Tbl_posdig4:
Data 561% , 545% , 529%,
Data 562% , 546% , 530%,
Data 563% , 547% , 531%,
Data 564% , 548% , 532%,
Data 565% , 549% , 533%,
Data 566% , 550% , 534%,
Data 567% , 551% , 535%,
Data 568% , 552% , 536%,
Data 569% , 553% , 537%,
Data 570% , 554% , 538%,
Data 571% , 555% , 539%,
Data 572% , 556% , 540%,
Data 573% , 557% , 541%,
Data 574% , 558% , 542%,
Data 575% , 559% , 543%,
Data 576% , 560% , 544%,
Data 177% , 161% , 145%,
Data 178% , 162% , 146%,
Data 179% , 163% , 147%,
Data 180% , 164% , 148%,
Data 181% , 165% , 149%,
Data 182% , 166% , 150%,
Data 183% , 167% , 151%,
Data 184% , 168% , 152%,
Data 185% , 169% , 153%,
Data 186% , 170% , 154%,
Data 187% , 171% , 155%,
Data 188% , 172% , 156%,
Data 189% , 173% , 157%,
Data 190% , 174% , 158%,
Data 191% , 175% , 159%,
Data 192% , 176% , 160%,

Tbl_posdig5:
Data 481% , 465% , 449%,
Data 482% , 466% , 450%,
Data 483% , 467% , 451%,
Data 484% , 468% , 452%,
Data 485% , 469% , 453%,
Data 486% , 470% , 454%,
Data 487% , 471% , 455%,
Data 488% , 472% , 456%,
Data 489% , 473% , 457%,
Data 490% , 474% , 458%,
Data 491% , 475% , 459%,
Data 492% , 476% , 460%,
Data 493% , 477% , 461%,
Data 494% , 478% , 462%,
Data 495% , 479% , 463%,
Data 496% , 480% , 464%,
Data 97% , 81% , 65%,
Data 98% , 82% , 66%,
Data 99% , 83% , 67%,
Data 100% , 84% , 68%,
Data 101% , 85% , 69%,
Data 102% , 86% , 70%,
Data 103% , 87% , 71%,
Data 104% , 88% , 72%,
Data 105% , 89% , 73%,
Data 106% , 90% , 74%,
Data 107% , 91% , 75%,
Data 108% , 92% , 76%,
Data 109% , 93% , 77%,
Data 110% , 94% , 78%,
Data 111% , 95% , 79%,
Data 112% , 96% , 80%,

Tbl_posdig6:
Data 433% , 417% , 401%,
Data 434% , 418% , 402%,
Data 435% , 419% , 403%,
Data 436% , 420% , 404%,
Data 437% , 421% , 405%,
Data 438% , 422% , 406%,
Data 439% , 423% , 407%,
Data 440% , 424% , 408%,
Data 441% , 425% , 409%,
Data 442% , 426% , 410%,
Data 443% , 427% , 411%,
Data 444% , 428% , 412%,
Data 445% , 429% , 413%,
Data 446% , 430% , 414%,
Data 447% , 431% , 415%,
Data 448% , 432% , 416%,
Data 49% , 33% , 17%,
Data 50% , 34% , 18%,
Data 51% , 35% , 19%,
Data 52% , 36% , 20%,
Data 53% , 37% , 21%,
Data 54% , 38% , 22%,
Data 55% , 39% , 23%,
Data 56% , 40% , 24%,
Data 57% , 41% , 25%,
Data 58% , 42% , 26%,
Data 59% , 43% , 27%,
Data 60% , 44% , 28%,
Data 61% , 45% , 29%,
Data 62% , 46% , 30%,
Data 63% , 47% , 31%,
Data 64% , 48% , 32%,


Tbl_posdp:
Data 513%
Data 514%
Data 515%
Data 516%
Data 517%
Data 518%
Data 519%
Data 520%
Data 521%
Data 522%
Data 523%
Data 524%
Data 525%
Data 526%
Data 527%
Data 528%
Data 129%
Data 130%
Data 131%
Data 132%
Data 133%
Data 134%
Data 135%
Data 136%
Data 137%
Data 138%
Data 139%
Data 140%
Data 141%
Data 142%
Data 143%
Data 144%

Data 641%
Data 642%
Data 643%
Data 644%
Data 645%
Data 646%
Data 647%
Data 648%
Data 649%
Data 650%
Data 651%
Data 652%
Data 653%
Data 654%
Data 655%
Data 656%
Data 257%
Data 258%
Data 259%
Data 260%
Data 261%
Data 262%
Data 263%
Data 264%
Data 265%
Data 266%
Data 267%
Data 268%
Data 269%
Data 270%
Data 271%
Data 272%

Tbl_post:
Data 705% , 689% , 673%,                                    'Pos signo
Data 706% , 690% , 674%,
Data 707% , 691% , 675%,
Data 708% , 692% , 676%,
Data 709% , 693% , 677%,
Data 710% , 694% , 678%,
Data 711% , 695% , 679%,
Data 712% , 696% , 680%,
Data 713% , 697% , 681%,
Data 714% , 698% , 682%,
Data 715% , 699% , 683%,
Data 716% , 700% , 684%,
Data 717% , 701% , 685%,
Data 718% , 702% , 686%,
Data 719% , 703% , 687%,
Data 720% , 704% , 688%,
Data 321% , 305% , 289%,
Data 322% , 306% , 290%,
Data 323% , 307% , 291%,
Data 324% , 308% , 292%,
Data 325% , 309% , 293%,
Data 326% , 310% , 294%,
Data 327% , 311% , 295%,
Data 328% , 312% , 296%,
Data 329% , 313% , 297%,
Data 330% , 314% , 298%,
Data 331% , 315% , 299%,
Data 332% , 316% , 300%,
Data 333% , 317% , 301%,
Data 334% , 318% , 302%,
Data 335% , 319% , 303%,
Data 336% , 320% , 304%,

Data 657% , 641% , 625%,                                    'POS1
Data 658% , 642% , 626%,
Data 659% , 643% , 627%,
Data        660%, 644%, 628%,
Data        661%, 645%, 629%,
Data        662%, 646%, 630%,
Data        663%, 647%, 631%,
Data        664%, 648%, 632%,
Data        665%, 649%, 633%,
Data        666%, 650%, 634%,
Data        667%, 651%, 635%,
Data        668%, 652%, 636%,
Data        669%, 653%, 637%,
Data        670%, 654%, 638%,
Data        671%, 655%, 639%,
Data        672%, 656%, 640%,
Data        273%, 257%, 241%,
Data        274%, 258%, 242%,
Data        275%, 259%, 243%,
Data        276%, 260%, 244%,
Data        277%, 261%, 245%,
Data        278%, 262%, 246%,
Data        279%, 263%, 247%,
Data        280%, 264%, 248%,
Data        281%, 265%, 249%,
Data        282%, 266%, 250%,
Data        283%, 267%, 251%,
Data        284%, 268%, 252%,
Data        285%, 269%, 253%,
Data        286%, 270%, 254%,
Data        287%, 271%, 255%,
Data 288% , 272% , 256%,

Data 609% , 593% , 577%,                                    'POS 2
Data        610%, 594%, 578%,
Data        611%, 595%, 579%,
Data        612%, 596%, 580%,
Data        613%, 597%, 581%,
Data        614%, 598%, 582%,
Data        615%, 599%, 583%,
Data        616%, 600%, 584%,
Data        617%, 601%, 585%,
Data        618%, 602%, 586%,
Data        619%, 603%, 587%,
Data        620%, 604%, 588%,
Data        621%, 605%, 589%,
Data        622%, 606%, 590%,
Data        623%, 607%, 591%,
Data        624%, 608%, 592%,
Data        225%, 209%, 193%,
Data        226%, 210%, 194%,
Data        227%, 211%, 195%,
Data        228%, 212%, 196%,
Data        229%, 213%, 197%,
Data        230%, 214%, 198%,
Data        231%, 215%, 199%,
Data        232%, 216%, 200%,
Data        233%, 217%, 201%,
Data        234%, 218%, 202%,
Data        235%, 219%, 203%,
Data        236%, 220%, 204%,
Data        237%, 221%, 205%,
Data        238%, 222%, 206%,
Data        239%, 223%, 207%,
Data 240% , 224% , 208%,


Data 545% , 529% , 513%,                                    'POS 3
Data        546%, 530%, 514%,
Data        547%, 531%, 515%,
Data        548%, 532%, 516%,
Data        549%, 533%, 517%,
Data        550%, 534%, 518%,
Data        551%, 535%, 519%,
Data        552%, 536%, 520%,
Data 553% , 537% , 521%,
Data        554%, 538%, 522%,
Data        555%, 539%, 523%,
Data        556%, 540%, 524%,
Data        557%, 541%, 525%,
Data        558%, 542%, 526%,
Data        559%, 543%, 527%,
Data        560%, 544%, 528%,
Data        161%, 145%, 129%,
Data        162%, 146%, 130%,
Data        163%, 147%, 131%,
Data        164%, 148%, 132%,
Data        165%, 149%, 133%,
Data        166%, 150%, 134%,
Data        167%, 151%, 135%,
Data        168%, 152%, 136%,
Data        169%, 153%, 137%,
Data        170%, 154%, 138%,
Data        171%, 155%, 139%,
Data        172%, 156%, 140%,
Data        173%, 157%, 141%,
Data        174%, 158%, 142%,
Data        175%, 159%, 143%,
Data 176% , 160% , 144%,

Data 465% , 449% , 433%,                                    'POS 4
Data        466%, 450%, 434%,
Data        467%, 451%, 435%,
Data        468%, 452%, 436%,
Data        469%, 453%, 437%,
Data 470% , 454% , 438%,
Data        471%, 455%, 439%,
Data        472%, 456%, 440%,
Data        473%, 457%, 441%,
Data        474%, 458%, 442%,
Data        475%, 459%, 443%,
Data        476%, 460%, 444%,
Data        477%, 461%, 445%,
Data        478%, 462%, 446%,
Data        479%, 463%, 447%,
Data        480%, 464%, 448%,
Data        81%, 65%, 49%,
Data        82%, 66%, 50%,
Data        83%, 67%, 51%,
Data        84%, 68%, 52%,
Data        85%, 69%, 53%,
Data        86%, 70%, 54%,
Data        87%, 71%, 55%,
Data        88%, 72%, 56%,
Data        89%, 73%, 57%,
Data        90%, 74%, 58%,
Data        91%, 75%, 59%,
Data        92%, 76%, 60%,
Data        93%, 77%, 61%,
Data        94%, 78%, 62%,
Data        95%, 79%, 63%,
Data 96% , 80% , 64%,

Data 465% , 449% , 433%,                                    'POS 5
Data        466%, 450%, 434%,
Data        467%, 451%, 435%,
Data 468% , 452% , 436%,
Data        469%, 453%, 437%,
Data        470%, 454%, 438%,
Data        471%, 455%, 439%,
Data        472%, 456%, 440%,
Data        473%, 457%, 441%,
Data        474%, 458%, 442%,
Data        475%, 459%, 443%,
Data        476%, 460%, 444%,
Data        477%, 461%, 445%,
Data        478%, 462%, 446%,
Data        479%, 463%, 447%,
Data        480%, 464%, 448%,
Data        81%, 65%, 49%,
Data        82%, 66%, 50%,
Data        83%, 67%, 51%,
Data        84%, 68%, 52%,
Data        85%, 69%, 53%,
Data        86%, 70%, 54%,
Data        87%, 71%, 55%,
Data        88%, 72%, 56%,
Data        89%, 73%, 57%,
Data        90%, 74%, 58%,
Data        91%, 75%, 59%,
Data        92%, 76%, 60%,
Data        93%, 77%, 61%,
Data        94%, 78%, 62%,
Data        95%, 79%, 63%,
Data 96% , 80% , 64%,


Tbl_char:
' @0 '0' (21 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00111111 , &B10000000,                  '           #######
Data  &B00000000, &B11111111, &B11100000, '         ###########
Data  &B00000001, &B11111111, &B11100000, '        ############
Data  &B00000011, &B11000001, &B11110000, '       ####     #####
Data  &B00000111, &B10000000, &B11110000, '      ####       ####
Data  &B00001111, &B00000000, &B01111000, '     ####         ####
Data  &B00001110, &B00000000, &B01111000, '     ###          ####
Data  &B00011110, &B00000000, &B01111000, '    ####          ####
Data  &B00011110, &B00000000, &B01111000, '    ####          ####
Data  &B00111100, &B00000000, &B01111000, '   ####           ####
Data  &B00111100, &B00000000, &B01111000, '   ####           ####
Data  &B00111100, &B00000000, &B01111000, '   ####           ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B01111000, &B00000000, &B11111000, '  ####           #####
Data  &B01111000, &B00000000, &B11110000, '  ####           ####
Data  &B01111000, &B00000000, &B11110000, '  ####           ####
Data  &B01111000, &B00000000, &B11110000, '  ####           ####
Data  &B11110000, &B00000000, &B11110000, ' ####            ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000011, &B11000000, ' ####          ####
Data  &B11110000, &B00000011, &B11000000, ' ####          ####
Data  &B11110000, &B00000011, &B10000000, ' ####          ###
Data  &B11110000, &B00000111, &B10000000, ' ####         ####
Data  &B01111000, &B00001111, &B00000000, '  ####       ####
Data  &B01111100, &B00011110, &B00000000, '  #####     ####
Data  &B00111111, &B11111100, &B00000000, '   ############
Data  &B00111111, &B11111000, &B00000000, '   ###########
Data  &B00001111, &B11100000, &B00000000, '     #######
Data  &B00000000, &B00000000, &B00000000, '

' @93 '1' (17 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000011 , &B10000000,                  '               ###
Data &B00000000 , &B00000111 , &B10000000,                  '              ####
Data &B00000000 , &B00001111 , &B10000000,                  '             #####
Data &B00000000 , &B00011111 , &B10000000,                  '            ######
Data &B00000011 , &B11111111 , &B00000000,                  '       ##########
Data &B00000011 , &B11111111 , &B00000000,                  '       ##########
Data &B00000011 , &B11111111 , &B00000000,                  '       ##########
Data &B00000000 , &B00011111 , &B00000000,                  '            #####
Data &B00000000 , &B00011110 , &B00000000,                  '            ####
Data &B00000000 , &B00011110 , &B00000000,                  '            ####
Data &B00000000 , &B00011110 , &B00000000,                  '            ####
Data &B00000000 , &B00011110 , &B00000000,                  '            ####
Data &B00000000 , &B00111100 , &B00000000,                  '           ####
Data &B00000000 , &B00111100 , &B00000000,                  '           ####
Data &B00000000 , &B00111100 , &B00000000,                  '           ####
Data &B00000000 , &B00111100 , &B00000000,                  '           ####
Data &B00000000 , &B01111000 , &B00000000,                  '          ####
Data &B00000000 , &B01111000 , &B00000000,                  '          ####
Data &B00000000 , &B01111000 , &B00000000,                  '          ####
Data &B00000000 , &B01111000 , &B00000000,                  '          ####
Data &B00000000 , &B11110000 , &B00000000,                  '         ####
Data &B00000000 , &B11110000 , &B00000000,                  '         ####
Data &B00000000 , &B11110000 , &B00000000,                  '         ####
Data &B00000000 , &B11110000 , &B00000000,                  '         ####
Data &B00000001 , &B11100000 , &B00000000,                  '        ####
Data &B00000001 , &B11100000 , &B00000000,                  '        ####
Data &B01111111 , &B11111111 , &B10000000,                  '  ################
Data &B11111111 , &B11111111 , &B10000000,                  ' #################
Data &B11111111 , &B11111111 , &B00000000,                  ' ################
Data &B00000000 , &B00000000 , &B00000000,                  '

' @186 '2' (22 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00111111, &B11000000, '           ########
Data  &B00000001, &B11111111, &B11110000, '        #############
Data  &B00000011, &B11111111, &B11111000, '       ###############
Data  &B00000111, &B11000000, &B11111000, '      #####      #####
Data  &B00000111, &B00000000, &B01111100, '      ###         #####
Data  &B00000100, &B00000000, &B00111100, '      #            ####
Data  &B00000000, &B00000000, &B00111100, '                   ####
Data  &B00000000, &B00000000, &B00111100, '                   ####
Data  &B00000000, &B00000000, &B00111100, '                   ####
Data  &B00000000, &B00000000, &B00111100, '                   ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000001, &B11110000, '                #####
Data  &B00000000, &B00000011, &B11100000, '               #####
Data  &B00000000, &B00000111, &B11000000, '              #####
Data  &B00000000, &B00001111, &B10000000, '             #####
Data  &B00000000, &B00011111, &B00000000, '            #####
Data  &B00000000, &B00111110, &B00000000, '           #####
Data  &B00000000, &B01111100, &B00000000, '          #####
Data  &B00000000, &B11111000, &B00000000, '         #####
Data  &B00000001, &B11100000, &B00000000, '        ####
Data  &B00000111, &B11000000, &B00000000, '      #####
Data  &B00001111, &B10000000, &B00000000, '     #####
Data  &B00011111, &B00000000, &B00000000, '    #####
Data  &B00111100, &B00000000, &B00000000, '   ####
Data  &B01111000, &B00000000, &B00000000, '  ####
Data  &B01111111, &B11111111, &B11100000, '  ##################
Data  &B11111111, &B11111111, &B11100000, ' ###################
Data  &B11111111, &B11111111, &B11000000, ' ##################
Data  &B00000000, &B00000000, &B00000000, '

' @279 '3' (23 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00011111, &B11100000, '            ########
Data  &B00000000, &B11111111, &B11111000, '         #############
Data  &B00000001, &B11111111, &B11111100, '        ###############
Data  &B00000011, &B11100000, &B01111110, '       #####      ######
Data  &B00000011, &B10000000, &B00111110, '       ###         #####
Data  &B00000010, &B00000000, &B00011110, '       #            ####
Data  &B00000000, &B00000000, &B00011110, '                    ####
Data  &B00000000, &B00000000, &B00011110, '                    ####
Data  &B00000000, &B00000000, &B00011100, '                    ###
Data  &B00000000, &B00000000, &B00111100, '                   ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000001, &B11110000, '                #####
Data  &B00000000, &B00111111, &B11000000, '           ########
Data  &B00000000, &B01111111, &B00000000, '          #######
Data  &B00000000, &B01111111, &B11000000, '          #########
Data  &B00000000, &B00000011, &B11110000, '               ######
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B01000000, &B00000001, &B11100000, '  #             ####
Data  &B01100000, &B00000011, &B11100000, '  ##           #####
Data  &B11111100, &B00001111, &B11000000, ' ######      ######
Data  &B11111111, &B11111111, &B10000000, ' #################
Data  &B01111111, &B11111110, &B00000000, '  ##############
Data  &B00001111, &B11110000, &B00000000, '     ########
Data  &B00000000, &B00000000, &B00000000, '

' @372 '4' (23 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00000000, &B00000000, '
Data  &B00000000, &B00000000, &B00011110, '                    ####
Data  &B00000000, &B00000000, &B01111110, '                  ######
Data  &B00000000, &B00000000, &B11111100, '                 ######
Data  &B00000000, &B00000001, &B11111100, '                #######
Data  &B00000000, &B00000011, &B11111100, '               ########
Data  &B00000000, &B00000111, &B11111100, '              #########
Data  &B00000000, &B00001111, &B01111100, '             #### #####
Data  &B00000000, &B00011110, &B01111000, '            ####  ####
Data  &B00000000, &B00111100, &B01111000, '           ####   ####
Data  &B00000000, &B01111000, &B01111000, '          ####    ####
Data  &B00000000, &B11110000, &B11111000, '         ####    #####
Data  &B00000001, &B11100000, &B11110000, '        ####     ####
Data  &B00000111, &B11000000, &B11110000, '      #####      ####
Data  &B00001111, &B10000000, &B11110000, '     #####       ####
Data  &B00011111, &B00000001, &B11110000, '    #####       #####
Data  &B00111110, &B00000001, &B11100000, '   #####        ####
Data  &B01111100, &B00000001, &B11100000, '  #####         ####
Data  &B01110000, &B00000001, &B11100000, '  ###           ####
Data  &B01111111, &B11111111, &B11111110, '  ######################
Data  &B11111111, &B11111111, &B11111110, ' #######################
Data  &B11111111, &B11111111, &B11111100, ' ######################
Data  &B00000000, &B00000011, &B11000000, '               ####
Data  &B00000000, &B00000011, &B11000000, '               ####
Data  &B00000000, &B00000111, &B10000000, '              ####
Data  &B00000000, &B00000111, &B10000000, '              ####
Data  &B00000000, &B00000111, &B10000000, '              ####
Data  &B00000000, &B00000111, &B10000000, '              ####
Data  &B00000000, &B00001111, &B00000000, '             ####
Data  &B00000000, &B00001111, &B00000000, '             ####
Data  &B00000000, &B00000000, &B00000000, '

' @465 '5' (24 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00000000, &B00000000, '
Data  &B00000001, &B11111111, &B11111111, '        #################
Data  &B00000001, &B11111111, &B11111111, '        #################
Data  &B00000011, &B11111111, &B11111110, '       #################
Data  &B00000011, &B11000000, &B00000000, '       ####
Data  &B00000011, &B11000000, &B00000000, '       ####
Data  &B00000011, &B11000000, &B00000000, '       ####
Data  &B00000011, &B11000000, &B00000000, '       ####
Data  &B00000111, &B11000000, &B00000000, '      #####
Data  &B00000111, &B10000000, &B00000000, '      ####
Data  &B00000111, &B10000000, &B00000000, '      ####
Data  &B00000111, &B10000000, &B00000000, '      ####
Data  &B00000111, &B11111110, &B00000000, '      ##########
Data  &B00001111, &B11111111, &B10000000, '     #############
Data  &B00001111, &B11111111, &B11000000, '     ##############
Data  &B00001100, &B00000111, &B11100000, '     ##       ######
Data  &B00000000, &B00000001, &B11110000, '                #####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000000, &B11110000, '                 ####
Data  &B00000000, &B00000001, &B11100000, '                ####
Data  &B00000000, &B00000001, &B11100000, '                ####
Data  &B01000000, &B00000011, &B11000000, '  #            ####
Data  &B11100000, &B00000111, &B11000000, ' ###          #####
Data  &B11111000, &B00011111, &B10000000, ' #####      ######
Data  &B11111111, &B11111111, &B00000000, ' ################
Data  &B01111111, &B11111100, &B00000000, '  #############
Data  &B00001111, &B11110000, &B00000000, '     ########
Data  &B00000000, &B00000000, &B00000000, '

' @558 '6' (20 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00000111, &B11110000, '              #######
Data  &B00000000, &B00111111, &B11110000, '           ##########
Data  &B00000000, &B11111111, &B11110000, '         ############
Data  &B00000001, &B11111000, &B00100000, '        ######     #
Data  &B00000011, &B11100000, &B00000000, '       #####
Data  &B00000111, &B10000000, &B00000000, '      ####
Data  &B00001111, &B00000000, &B00000000, '     ####
Data  &B00011110, &B00000000, &B00000000, '    ####
Data  &B00011110, &B00000000, &B00000000, '    ####
Data  &B00111100, &B00000000, &B00000000, '   ####
Data  &B00111100, &B00000000, &B00000000, '   ####
Data  &B00111000, &B11111110, &B00000000, '   ###   #######
Data  &B01111011, &B11111111, &B00000000, '  #### ##########
Data  &B01111111, &B11111111, &B10000000, '  ################
Data  &B01111110, &B00000111, &B11000000, '  ######      #####
Data  &B11111000, &B00000011, &B11000000, ' #####         ####
Data  &B11110000, &B00000011, &B11100000, ' ####          #####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11110000, &B00000011, &B11000000, ' ####          ####
Data  &B11111000, &B00000011, &B11000000, ' #####         ####
Data  &B01111000, &B00000111, &B10000000, '  ####        ####
Data  &B01111100, &B00000111, &B10000000, '  #####       ####
Data  &B00111110, &B00011111, &B00000000, '   #####    #####
Data  &B00111111, &B11111110, &B00000000, '   #############
Data  &B00011111, &B11111100, &B00000000, '    ###########
Data  &B00000111, &B11110000, &B00000000, '      #######

' @651 '7' (24 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00000000, &B00000000, '
Data  &B00001111, &B11111111, &B11111111, '     ####################
Data  &B00011111, &B11111111, &B11111111, '    #####################
Data  &B00011111, &B11111111, &B11111110, '    ####################
Data  &B00000000, &B00000000, &B00011110, '                    ####
Data  &B00000000, &B00000000, &B00111110, '                   #####
Data  &B00000000, &B00000000, &B01111100, '                  #####
Data  &B00000000, &B00000000, &B01111000, '                  ####
Data  &B00000000, &B00000000, &B11111000, '                 #####
Data  &B00000000, &B00000001, &B11110000, '                #####
Data  &B00000000, &B00000011, &B11100000, '               #####
Data  &B00000000, &B00000011, &B11000000, '               ####
Data  &B00000000, &B00000111, &B11000000, '              #####
Data  &B00000000, &B00001111, &B10000000, '             #####
Data  &B00000000, &B00011111, &B00000000, '            #####
Data  &B00000000, &B00011110, &B00000000, '            ####
Data  &B00000000, &B00111110, &B00000000, '           #####
Data  &B00000000, &B01111100, &B00000000, '          #####
Data  &B00000000, &B11111000, &B00000000, '         #####
Data  &B00000000, &B11111000, &B00000000, '         #####
Data  &B00000001, &B11110000, &B00000000, '        #####
Data  &B00000011, &B11100000, &B00000000, '       #####
Data  &B00000011, &B11000000, &B00000000, '       ####
Data  &B00000111, &B11000000, &B00000000, '      #####
Data  &B00001111, &B10000000, &B00000000, '     #####
Data  &B00011111, &B00000000, &B00000000, '    #####
Data  &B00011111, &B00000000, &B00000000, '    #####
Data  &B00111110, &B00000000, &B00000000, '   #####
Data  &B01111100, &B00000000, &B00000000, '  #####
Data  &B11111000, &B00000000, &B00000000, ' #####
Data  &B00000000, &B00000000, &B00000000, '

' @744 '8' (22 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B00111111, &B10000000, '           #######
Data  &B00000000, &B11111111, &B11110000, '         ############
Data  &B00000011, &B11111111, &B11111000, '       ###############
Data  &B00000111, &B11000000, &B11111000, '      #####      #####
Data  &B00001111, &B10000000, &B01111100, '     #####        #####
Data  &B00001111, &B00000000, &B00111100, '     ####          ####
Data  &B00011110, &B00000000, &B00111100, '    ####           ####
Data  &B00011110, &B00000000, &B00111100, '    ####           ####
Data  &B00011110, &B00000000, &B00111100, '    ####           ####
Data  &B00011110, &B00000000, &B01111000, '    ####          ####
Data  &B00011111, &B00000000, &B01111000, '    #####         ####
Data  &B00001111, &B11000000, &B11110000, '     ######      ####
Data  &B00000111, &B11110001, &B11100000, '      #######   ####
Data  &B00000011, &B11111111, &B10000000, '       ###########
Data  &B00000111, &B11111111, &B00000000, '      ###########
Data  &B00001111, &B00111111, &B11000000, '     ####  ########
Data  &B00011100, &B00000111, &B11100000, '    ###       ######
Data  &B00111000, &B00000011, &B11100000, '   ###         #####
Data  &B01111000, &B00000001, &B11110000, '  ####          #####
Data  &B01110000, &B00000000, &B11110000, '  ###            ####
Data  &B11110000, &B00000000, &B11110000, ' ####            ####
Data  &B11110000, &B00000000, &B11110000, ' ####            ####
Data  &B11110000, &B00000000, &B11110000, ' ####            ####
Data  &B11110000, &B00000001, &B11100000, ' ####           ####
Data  &B11111000, &B00000001, &B11100000, ' #####          ####
Data  &B11111000, &B00000011, &B11000000, ' #####         ####
Data  &B01111110, &B00001111, &B10000000, '  ######     #####
Data  &B00111111, &B11111111, &B00000000, '   ##############
Data  &B00011111, &B11111110, &B00000000, '    ############
Data  &B00000111, &B11110000, &B00000000, '      #######
Data  &B00000000, &B00000000, &B00000000, '

' @837 '9' (21 pixels wide)
Data &B00000000 , &B00000000 , &B00000000,                  '
Data  &B00000000, &B01111111, &B00000000, '          #######
Data  &B00000001, &B11111111, &B11000000, '        ###########
Data  &B00000111, &B11111111, &B11100000, '      ##############
Data  &B00001111, &B10000011, &B11100000, '     #####     #####
Data  &B00011111, &B00000001, &B11110000, '    #####       #####
Data  &B00011110, &B00000000, &B11110000, '    ####         ####
Data  &B00111100, &B00000000, &B01111000, '   ####           ####
Data  &B00111100, &B00000000, &B01111000, '   ####           ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B01111000, &B00000000, &B01111000, '  ####            ####
Data  &B00111100, &B00000000, &B11111000, '   ####          #####
Data  &B00111111, &B00000011, &B11110000, '   ######      ######
Data  &B00011111, &B11111111, &B11110000, '    #################
Data  &B00001111, &B11111110, &B11110000, '     ########### ####
Data  &B00000011, &B11111000, &B11110000, '       #######   ####
Data  &B00000000, &B00000001, &B11100000, '                ####
Data  &B00000000, &B00000001, &B11100000, '                ####
Data  &B00000000, &B00000011, &B11000000, '               ####
Data  &B00000000, &B00000111, &B10000000, '              ####
Data  &B00000000, &B00000111, &B10000000, '              ####
Data  &B00000000, &B00011111, &B00000000, '            #####
Data  &B00000000, &B00111110, &B00000000, '           #####
Data  &B01000000, &B11111100, &B00000000, '  #      ######
Data  &B11111111, &B11110000, &B00000000, ' ############
Data  &B11111111, &B11100000, &B00000000, ' ###########
Data  &B11111111, &B00000000, &B00000000, ' ########
Data  &B00000000, &B00000000, &B00000000, '

' ESPACIO
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,

' .

' ESPACIO
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &HF0 , &H00,
Data &H00 , &HF0 , &H00,
Data &H00 , &H00 , &H00,



' @930 ':' (10 pixels wide)
Tbl_dosp:
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00011111                                             '      #####
Data &B00011111                                             '      #####
Data &B00011110                                             '      ####
Data &B00011110                                             '      ####
Data &B00111110                                             '     #####
Data &B00111110                                             '     #####
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B00000000                                             '
Data &B01111100                                             '  #####
Data &B01111100                                             '  #####
Data &B01111000                                             '  ####
Data &B01111000                                             '  ####
Data &B11111000                                             ' #####
Data &B11111000                                             ' #####
Data &B00000000                                             '
Data &B00000000                                             '

':

Tbl_chart:
        ' @160 '0' (20 pixels wide)
Data &B00000001, &B11111000, &B00000000, '        ######
Data &B00000111, &B00001110, &B00000000, '      ###    ###
Data &B00001110, &B00000111, &B00000000, '     ###      ###
Data &B00011100, &B00000011, &B10000000, '    ###        ###
Data &B00011000, &B00000001, &B10000000, '    ##          ##
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B01110000, &B00000000, &B11100000, '  ###            ###
Data &B01110000, &B00000000, &B11100000, '  ###            ###
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B01110000, &B00000000, &B11100000, '  ###            ###
Data &B01110000, &B00000000, &B11100000, '  ###            ###
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B00011000, &B00000001, &B10000000, '    ##          ##
Data &B00011100, &B00000011, &B10000000, '    ###        ###
Data &B00001110, &B00000111, &B00000000, '     ###      ###
Data &B00000111, &B00001110, &B00000000, '      ###    ###
Data &B00000001, &B11111000, &B00000000, '        ######

        ' @256 '1' (14 pixels wide)
Data &B00000000 , &B00011000 , &B00000000,                  '        ##
Data &B00000000 , &B00011000 , &B00000000,                  '        ##
Data &B00000000 , &B00111000 , &B00000000,                  '       ###
Data &B00000001 , &B11111000 , &B00000000,                  '    ######
Data &B00001111 , &B11111000 , &B00000000,                  ' #########
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B01111000 , &B00000000,                  '      ####
Data &B00000000 , &B11111000 , &B00000000,                  '     #####
Data &B00001111 , &B11111111 , &B11000000,                  ' ##############
Data &B00001111 , &B11111111 , &B11000000,                  ' ##############

        ' @320 '2' (19 pixels wide)
Data &B00000001, &B11111000, &B00000000, '        ######
Data &B00000110, &B00001110, &B00000000, '      ##     ###
Data &B00001000, &B00000111, &B10000000, '     #        ####
Data &B00010000, &B00000011, &B11000000, '    #          ####
Data &B00100000, &B00000001, &B11000000, '   #            ###
Data &B00100000, &B00000001, &B11100000, '   #            ####
Data &B01100000, &B00000001, &B11100000, '  ##            ####
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B01111110, &B00000001, &B11100000, '  ######        ####
Data &B01111110, &B00000001, &B11100000, '  ######        ####
Data &B01111110, &B00000011, &B11100000, '  ######       #####
Data &B00111110, &B00000011, &B11000000, '   #####       ####
Data &B00011100, &B00000011, &B11000000, '    ###        ####
Data &B00000000, &B00000111, &B10000000, '              ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00011110, &B00000000, '            ####
Data &B00000000, &B00111100, &B00000000, '           ####
Data &B00000000, &B01111000, &B00000000, '          ####
Data &B00000000, &B11100000, &B00000000, '         ###
Data &B00000000, &B11000000, &B00000000, '         ##
Data &B00000001, &B10000000, &B00000000, '        ##
Data &B00000011, &B00000000, &B00100000, '       ##          #
Data &B00000110, &B00000000, &B00100000, '      ##           #
Data &B00001100, &B00000000, &B01100000, '     ##           ##
Data &B00011000, &B00000000, &B01100000, '    ##            ##
Data &B00011100, &B00000000, &B11100000, '    ###          ###
Data &B00111111, &B11111111, &B11100000, '   #################
Data &B01111111, &B11111111, &B11100000, '  ##################
Data &B11111111, &B11111111, &B11100000, ' ###################
Data &B11111111, &B11111111, &B11100000, ' ###################
Data &B00000000, &B00000000, &B00000000, '

        ' @416 '3' (19 pixels wide)
Data &B00000011, &B11111000, &B00000000, '       #######
Data &B00001100, &B00011110, &B00000000, '     ##     ####
Data &B00010000, &B00001111, &B00000000, '    #        ####
Data &B00100000, &B00000111, &B10000000, '   #          ####
Data &B01100000, &B00000011, &B10000000, '  ##           ###
Data &B01110000, &B00000011, &B11000000, '  ###          ####
Data &B01111000, &B00000011, &B11000000, '  ####         ####
Data &B01111000, &B00000011, &B11000000, '  ####         ####
Data &B01111000, &B00000011, &B11000000, '  ####         ####
Data &B00110000, &B00000011, &B11000000, '   ##          ####
Data &B00000000, &B00000011, &B11000000, '               ####
Data &B00000000, &B00000011, &B10000000, '               ###
Data &B00000000, &B00000111, &B00000000, '              ###
Data &B00000000, &B00001110, &B00000000, '             ###
Data &B00000000, &B00011100, &B00000000, '            ###
Data &B00001111, &B11110000, &B00000000, '     ########
Data &B00000000, &B00001110, &B00000000, '             ###
Data &B00000000, &B00000111, &B00000000, '              ###
Data &B00000000, &B00000011, &B10000000, '               ###
Data &B00000000, &B00000011, &B11000000, '               ####
Data &B00000000, &B00000001, &B11000000, '                ###
Data &B01110000, &B00000001, &B11100000, '  ###           ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11110000, &B00000001, &B11100000, ' ####           ####
Data &B11100000, &B00000001, &B11000000, ' ###            ###
Data &B01000000, &B00000011, &B11000000, '  #            ####
Data &B01000000, &B00000011, &B10000000, '  #            ###
Data &B00100000, &B00000111, &B00000000, '   #          ###
Data &B00011000, &B00011110, &B00000000, '    ##      ####
Data &B00000111, &B11111000, &B00000000, '      ########

        ' @512 '4' (21 pixels wide)
Data &B00000000, &B00000011, &B00000000, '               ##
Data &B00000000, &B00000111, &B00000000, '              ###
Data &B00000000, &B00000111, &B00000000, '              ###
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00011111, &B00000000, '            #####
Data &B00000000, &B00011111, &B00000000, '            #####
Data &B00000000, &B00111111, &B00000000, '           ######
Data &B00000000, &B01101111, &B00000000, '          ## ####
Data &B00000000, &B01101111, &B00000000, '          ## ####
Data &B00000000, &B11001111, &B00000000, '         ##  ####
Data &B00000001, &B10001111, &B00000000, '        ##   ####
Data &B00000001, &B10001111, &B00000000, '        ##   ####
Data &B00000011, &B00001111, &B00000000, '       ##    ####
Data &B00000110, &B00001111, &B00000000, '      ##     ####
Data &B00000110, &B00001111, &B00000000, '      ##     ####
Data &B00001100, &B00001111, &B00000000, '     ##      ####
Data &B00011000, &B00001111, &B00000000, '    ##       ####
Data &B00011000, &B00001111, &B00000000, '    ##       ####
Data &B00110000, &B00001111, &B00000000, '   ##        ####
Data &B01100000, &B00001111, &B00000000, '  ##         ####
Data &B01100000, &B00001111, &B00000000, '  ##         ####
Data &B11000000, &B00001111, &B00000000, ' ##          ####
Data &B11111111, &B11111111, &B11111000, ' #####################
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00001111, &B00000000, '             ####
Data &B00000000, &B00011111, &B10000000, '            ######
Data &B00000000, &B11111111, &B11110000, '         ############
Data &B00000000, &B00000000, &B00000000, '

        ' @608 '5' (19 pixels wide)
Data &B00011100, &B00000001, &B10000000, '    ###         ##
Data &B00011111, &B11111111, &B10000000, '    ##############
Data &B00011111, &B11111111, &B00000000, '    #############
Data &B00011111, &B11111110, &B00000000, '    ############
Data &B00010011, &B11111000, &B00000000, '    #  #######
Data &B00010000, &B00000000, &B00000000, '    #
Data &B00010000, &B00000000, &B00000000, '    #
Data &B00010000, &B00000000, &B00000000, '    #
Data &B00010000, &B00000000, &B00000000, '    #
Data &B00100000, &B00000000, &B00000000, '   #
Data &B00100000, &B00000000, &B00000000, '   #
Data &B00100011, &B11110000, &B00000000, '   #   ######
Data &B00111100, &B00011100, &B00000000, '   ####     ###
Data &B00110000, &B00001111, &B00000000, '   ##        ####
Data &B01100000, &B00000111, &B10000000, '  ##          ####
Data &B01100000, &B00000011, &B10000000, '  ##           ###
Data &B01100000, &B00000011, &B11000000, '  ##           ####
Data &B00000000, &B00000001, &B11000000, '                ###
Data &B00000000, &B00000001, &B11100000, '                ####
Data &B00000000, &B00000001, &B11100000, '                ####
Data &B01110000, &B00000001, &B11100000, '  ###           ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11110000, &B00000001, &B11000000, ' ####           ###
Data &B11000000, &B00000011, &B11000000, ' ##            ####
Data &B01000000, &B00000011, &B10000000, '  #            ###
Data &B01100000, &B00000111, &B00000000, '  ##          ###
Data &B00100000, &B00001110, &B00000000, '   #         ###
Data &B00011000, &B00011100, &B00000000, '    ##      ###
Data &B00000111, &B11110000, &B00000000, '      #######

        ' @704 '6' (20 pixels wide)
Data &B00000000, &B01111110, &B00000000, '          ######
Data &B00000001, &B11000011, &B10000000, '        ###    ###
Data &B00000011, &B00000001, &B11000000, '       ##       ###
Data &B00000110, &B00000001, &B11000000, '      ##        ###
Data &B00001100, &B00000011, &B11100000, '     ##        #####
Data &B00011100, &B00000011, &B11100000, '    ###        #####
Data &B00011100, &B00000011, &B11100000, '    ###        #####
Data &B00111000, &B00000011, &B11100000, '   ###         #####
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B01111000, &B00000000, &B00000000, '  ####
Data &B01110000, &B00000000, &B00000000, '  ###
Data &B01110000, &B00000000, &B00000000, '  ###
Data &B11110000, &B00000000, &B00000000, ' ####
Data &B11110000, &B00000000, &B00000000, ' ####
Data &B11110000, &B11111110, &B00000000, ' ####    #######
Data &B11110011, &B00000111, &B10000000, ' ####  ##     ####
Data &B11110100, &B00000011, &B11000000, ' #### #        ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11111000, &B00000001, &B11100000, ' #####          ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B01110000, &B00000000, &B11110000, '  ###            ####
Data &B01110000, &B00000000, &B11110000, '  ###            ####
Data &B01110000, &B00000000, &B11110000, '  ###            ####
Data &B00110000, &B00000000, &B11100000, '   ##            ###
Data &B00110000, &B00000000, &B11100000, '   ##            ###
Data &B00011000, &B00000001, &B11100000, '    ##          ####
Data &B00011000, &B00000001, &B11000000, '    ##          ###
Data &B00001100, &B00000011, &B10000000, '     ##        ###
Data &B00000111, &B00000111, &B00000000, '      ###     ###
Data &B00000001, &B11111100, &B00000000, '        #######

        ' @800 '7' (20 pixels wide)
Data &B00000000, &B00000000, &B00000000, '
Data &B01111111, &B11111111, &B11110000, '  ###################
Data &B01111111, &B11111111, &B11110000, '  ###################
Data &B01111111, &B11111111, &B11100000, '  ##################
Data &B01111111, &B11111111, &B11100000, '  ##################
Data &B01100000, &B00000000, &B01000000, '  ##              #
Data &B11000000, &B00000000, &B10000000, ' ##              #
Data &B11000000, &B00000000, &B10000000, ' ##              #
Data &B11000000, &B00000001, &B00000000, ' ##             #
Data &B10000000, &B00000001, &B00000000, ' #              #
Data &B10000000, &B00000011, &B00000000, ' #             ##
Data &B00000000, &B00000110, &B00000000, '              ##
Data &B00000000, &B00000110, &B00000000, '              ##
Data &B00000000, &B00001100, &B00000000, '             ##
Data &B00000000, &B00001100, &B00000000, '             ##
Data &B00000000, &B00011100, &B00000000, '            ###
Data &B00000000, &B00011000, &B00000000, '            ##
Data &B00000000, &B00011000, &B00000000, '            ##
Data &B00000000, &B00111000, &B00000000, '           ###
Data &B00000000, &B00111000, &B00000000, '           ###
Data &B00000000, &B01111000, &B00000000, '          ####
Data &B00000000, &B01110000, &B00000000, '          ###
Data &B00000000, &B11110000, &B00000000, '         ####
Data &B00000000, &B11110000, &B00000000, '         ####
Data &B00000000, &B11110000, &B00000000, '         ####
Data &B00000000, &B11110000, &B00000000, '         ####
Data &B00000001, &B11110000, &B00000000, '        #####
Data &B00000001, &B11110000, &B00000000, '        #####
Data &B00000001, &B11110000, &B00000000, '        #####
Data &B00000001, &B11110000, &B00000000, '        #####
Data &B00000001, &B11110000, &B00000000, '        #####
Data &B00000000, &B11100000, &B00000000, '         ###

        ' @896 '8' (20 pixels wide)
Data &B00000001, &B11111100, &B00000000, '        #######
Data &B00000111, &B00001111, &B00000000, '      ###    ####
Data &B00001100, &B00000011, &B10000000, '     ##        ###
Data &B00011100, &B00000011, &B11000000, '    ###        ####
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B00111000, &B00000001, &B11100000, '   ###          ####
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B01111100, &B00000001, &B11000000, '  #####         ###
Data &B01111100, &B00000011, &B11000000, '  #####        ####
Data &B00111110, &B00000011, &B10000000, '   #####       ###
Data &B00111111, &B11000111, &B00000000, '   ########   ###
Data &B00011111, &B11111110, &B00000000, '    ############
Data &B00001111, &B11111110, &B00000000, '     ###########
Data &B00000011, &B11111111, &B00000000, '       ##########
Data &B00000011, &B11111111, &B10000000, '       ###########
Data &B00001110, &B00111111, &B11000000, '     ###   ########
Data &B00111100, &B00000111, &B11100000, '   ####       ######
Data &B01111000, &B00000011, &B11100000, '  ####         #####
Data &B01111000, &B00000001, &B11110000, '  ####          #####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11110000, ' ####            ####
Data &B11110000, &B00000000, &B11100000, ' ####            ###
Data &B01111000, &B00000001, &B11100000, '  ####          ####
Data &B00111000, &B00000001, &B11000000, '   ###          ###
Data &B00111100, &B00000011, &B10000000, '   ####        ###
Data &B00001110, &B00001111, &B00000000, '     ###     ####
Data &B00000011, &B11111100, &B00000000, '       ########

        ' @992 '9' (20 pixels wide)
Data &B00000001 , &B11111000 , &B00000000,                  '        ######
Data &B00000111 , &B00001110 , &B00000000,                  '      ###    ###
Data &B00011100 , &B00000111 , &B00000000,                  '    ###       ###
Data &B00111000 , &B00000011 , &B10000000,                  '   ###         ###
Data &B01111000 , &B00000001 , &B11000000,                  '  ####          ###
Data &B01110000 , &B00000001 , &B11000000,                  '  ###           ###
Data &B01110000 , &B00000000 , &B11100000,                  '  ###            ###
Data &B11110000 , &B00000000 , &B11100000,                  ' ####            ###
Data &B11110000 , &B00000000 , &B11100000,                  ' ####            ###
Data &B11110000 , &B00000000 , &B11110000,                  ' ####            ####
Data &B11110000 , &B00000000 , &B11110000,                  ' ####            ####
Data &B11110000 , &B00000000 , &B11110000,                  ' ####            ####
Data &B11110000 , &B00000000 , &B11110000,                  ' ####            ####
Data &B01111000 , &B00000001 , &B11110000,                  '  ####          #####
Data &B01111000 , &B00000001 , &B11110000,                  '  ####          #####
Data &B00111100 , &B00000011 , &B11110000,                  '   ####        ######
Data &B00001110 , &B00001100 , &B11110000,                  '     ###     ##  ####
Data &B00000011 , &B11110000 , &B11110000,                  '       ######    ####
Data &B00000000 , &B00000000 , &B11110000,                  '                 ####
Data &B00000000 , &B00000000 , &B11110000,                  '                 ####
Data &B00000000 , &B00000000 , &B11100000,                  '                 ###
Data &B00000000 , &B00000000 , &B11100000,                  '                 ###
Data &B00000000 , &B00000000 , &B11100000,                  '                 ###
Data &B00111000 , &B00000000 , &B11100000,                  '   ###           ###
Data &B01111100 , &B00000001 , &B11000000,                  '  #####         ###
Data &B01111100 , &B00000001 , &B11000000,                  '  #####         ###
Data &B01111100 , &B00000011 , &B10000000,                  '  #####        ###
Data &B01111100 , &B00000011 , &B10000000,                  '  #####        ###
Data &B01111000 , &B00000111 , &B00000000,                  '  ####        ###
Data &B00111000 , &B00001110 , &B00000000,                  '   ###       ###
Data &B00011100 , &B00011000 , &B00000000,                  '    ###     ##
Data &B00000111 , &B11100000 , &B00000000,                  '      ######

' ESPACIO      10
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,
Data &H00 , &H00 , &H00,


'11
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B11111111 , &B11111111 , &B11111100,                  ' ######################
Data &B11111111 , &B11111111 , &B11111100,                  ' ######################
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00110000 , &B00000000,                  '           ##
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '

'12 -
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B11111111 , &B11111111 , &B11111100,                  ' ######################
Data &B11111111 , &B11111111 , &B11111100,                  ' ######################
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '

Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B00000000 , &B00000000,                  '
Data &B00000000 , &B01111111 , &B00000100,                  '          #######     #
Data &B00000001 , &B11000001 , &B11000100,                  '        ###     ###   #
Data &B00000011 , &B00000000 , &B01111100,                  '       ##         #####
Data &B00000110 , &B00000000 , &B00111100,                  '      ##           ####
Data &B00001100 , &B00000000 , &B00011100,                  '     ##             ###
Data &B00011100 , &B00000000 , &B00001100,                  '    ###              ##
Data &B00111000 , &B00000000 , &B00001110,                  '   ###               ###
Data &B00111000 , &B00000000 , &B00000110,                  '   ###                ##
Data &B01111000 , &B00000000 , &B00000110,                  '  ####                ##
Data &B01110000 , &B00000000 , &B00000110,                  '  ###                 ##
Data &B01110000 , &B00000000 , &B00000010,                  '  ###                  #
Data &B11110000 , &B00000000 , &B00000010,                  ' ####                  #
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B11110000 , &B00000000 , &B00000000,                  ' ####
Data &B01110000 , &B00000000 , &B00000001,                  '  ###                   #
Data &B01111000 , &B00000000 , &B00000001,                  '  ####                  #
Data &B01111000 , &B00000000 , &B00000010,                  '  ####                 #
Data &B00111000 , &B00000000 , &B00000010,                  '   ###                 #
Data &B00011100 , &B00000000 , &B00000010,                  '    ###                #
Data &B00011100 , &B00000000 , &B00000100,                  '    ###               #
Data &B00001110 , &B00000000 , &B00001000,                  '     ###             #
Data &B00000111 , &B00000000 , &B00010000,                  '      ###           #
Data &B00000001 , &B11000000 , &B01100000,                  '        ###       ##
Data &B00000000 , &B00111111 , &B10000000,                  '           #######


Tbl_sp:
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000,                                            '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B00000000, '
Data &B01100000, '  ##
Data &B11110000, ' ####
Data &B11110000, ' ####
Data &B11110000, ' ####
Data &B01100000,                                            '  ##


Tbl_possp:
Data        561%
Data        562%
Data        563%
Data        564%
Data        565%
Data        566%
Data        567%
Data        568%
Data        569%
Data        570%
Data        571%
Data        572%
Data        573%
Data        574%
Data        575%
Data        576%
Data        177%
Data        178%
Data        179%
Data        180%
Data        181%
Data        182%
Data        183%
Data        184%
Data        185%
Data        186%
Data        187%
Data        188%
Data        189%
Data        190%
Data        191%
Data 192%

Tbl_grado:
Data &B00000000,                                            '
Data &B00111100,                                            '   ####
Data &B01000010,                                            '  #    #
Data &B11000011,                                            ' ##    ##
Data &B11000011,                                            ' ##    ##
Data &B11000011,                                            ' ##    ##
Data &B11000011,                                            ' ##    ##
Data &B01100010,                                            '  ##   #
Data &B00111100,                                            '   ####
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '
Data &B00000000,                                            '

Tbl_posgrado:
Data 481%
Data 482%
Data 483%
Data 484%
Data 485%
Data 486%
Data 487%
Data 488%
Data 489%
Data 490%
Data 491%
Data 492%
Data 493%
Data 494%
Data 495%
Data 496%
Data 97%
Data 98%
Data 99%
Data 100%
Data 101%
Data 102%
Data 103%
Data 104%
Data 105%
Data 106%
Data 107%
Data 108%
Data 109%
Data 110%
Data 111%
Data 112%



Loaded_arch: