'DRV32X192_SD.bas
'
'                 WATCHING Soluciones Tecnológicas
'                    Fernando Vásquez - 25.06.15
'
' Programa para manejar matriz de 32x192 pixels
' Puede manejar animaciones almacenadas en una memoria SD
' Reloj en tiempo real
' Incluye recepcion de datos de GPS por puerto serial 2

$version 0 , 1 , 70
$regfile = "m1284pdef.dat"
$crystal = 18432000
$baud = 9600
$baud1 = 9600

$hwstack = 128
$swstack = 128
$framesize = 128


'Declaracion de constantes


'Configuracion de entradas/salidas
Pwrsd Alias Portc.3
Config Pwrsd = Output

'Pinbug Alias Porta.4
'Config Pinbug = Output

'Pinbug2 Alias Porta.5
'Config Pinbug2 = Output


'Staout Alias Portb.2
'Config Staout = Output

Sta0 Alias Pind.4
Config Sta0 = Input
Sta1 Alias Pind.5
Config Sta1 = Input
Sta2 Alias Pind.6
Config Sta2 = Input
Sta3 Alias Pind.7
Config Sta3 = Input

Set Portd.4
Set Portd.5
Set Portd.6
Set Portd.7


Config 1wire = Portc.4



'Led1 Alias Portc.2                                          'LED VERDE
'Config Led1 = Output

'Led2 Alias Portc.1                                          'LED AMARILLO
'Config Led2 = Output

Const Nummatriz = 12                                        'Una matriz esunmodulo P10 de 16x32
Const Longbuf = Nummatriz * 64
Const Longdat = Nummatriz * 32
Const Longdat_mas_uno = Longdat + 1
Const Longbuf_mas_uno = Longbuf + 1
Const Numtxser = Longbuf / 4
Const Numtxser_2 = Numtxser / 2

Ledout Alias Portc.2
Config Ledout = Output



Sela Alias Portb.0
Config Sela = Output

Selb Alias Portb.1
Config Selb = Output

Sck Alias Portb.3
Config Sck = Output


Datos Alias Portb.2
Config Datos = Output


Lena2 Alias Porta.0
Config Lena2 = Output

Lena Alias Porta.1
Config Lena = Output

Oena2 Alias Porta.2
Config Oena2 = Output

Oena Alias Porta.3
Config Oena = Output


'Configuración de Interrupciones
'TIMER0
'Configuración de Interrupciones
'TIMER0
Config Timer0 = Timer , Prescale = 1024
On Timer0 Tim0_isr
Enable Timer0
Start Timer0

'Config Timer2 = Timer , Prescale = 1024
'On Timer2 Tim2_isr
'Enable Timer2
'Start Timer2

Config Date = Dmy , Separator = /
Config Clock = Soft , Gosub = Sectic

Assr = &H20                                                 ' See datasheet for details.
Tccr2a = &H00                                               ' See datasheet for details.
Tccr2b = &H05                                               ' See datasheet for details.
Tcnt2 = &H00

Date$ = "11/02/16"
Time$ = "15:02:00"

' Puerto serial 1
Open "com1:" For Binary As #1
On Urxc At_ser1
Enable Urxc

' Puerto serial 2
Open "com2:" For Binary As #2
On Urxc1 At_ser2
'Enable Urxc1

Enable Interrupts


'*******************************************************************************
'* Archivos incluidos
'*******************************************************************************

'$include "z:\EQUIPOS\Libs\Config_MMCSD_HC_M1284p.bas"
$include "Config_MMCSD_HC_M1284p.bas"
'$include "z:\EQUIPOS\Libs\Config_AVR-DOS.bas"
$include "Config_AVR-DOS.bas"
$include "DRV32X192_FINarchivos.bas"
'$include "SD_procesos.BAS"



Call Inivar()

Tmpb = 0
Do
   Call Diskinsertion()
   If Sdinitok = 0 Then
      Incr Tmpb
      print #1, "Try ";tmpb
      Call Espera(100)
      'Toggle Ledout
   End If
Loop Until Sdinitok = 1


   Call Leersta()

   'Call Wrbufdata()

'Reset Led1
'Set Led2

Do

   If Sernew = 1 Then                                       'DATOS SERIAL 1
      Reset Sernew
      Print #1 , "SER1=" ; Serproc
      Call Procser()
      Enable Timer0
   End If

   If Aniflag = 1 Then
       Call Rdlinesd()                                      ' mostrando animaciones
   End If


   If Horaflag = 1 Then                                     'Mostrando la Hora

      If Newseg = 1 Then
         Reset Newseg
         'Print #1 , Time$
         Tmpstr8 = Time$
         Tmpstr2 = Mid(tmpstr8 , 1 , 1)
         Tbl_hora(1) = Val(tmpstr2)
         Tmpstr2 = Mid(tmpstr8 , 2 , 1)
         Tbl_hora(2) = Val(tmpstr2)
         Tmpstr2 = Mid(tmpstr8 , 4 , 1)
         Tbl_hora(3) = Val(tmpstr2)
         Tmpstr2 = Mid(tmpstr8 , 5 , 1)
         Tbl_hora(4) = Val(tmpstr2)
         Tmpstr2 = Mid(tmpstr8 , 7 , 1)
         Tbl_hora(5) = Val(tmpstr2)
         Tmpstr2 = Mid(tmpstr8 , 8 , 1)
         Tbl_hora(6) = Val(tmpstr2)
          'Call Gendig(tbl_hora(2) , 1)

         For Tmpb = 1 To 6
            If Tbl_hora(tmpb) <> Tbl_horaant(tmpb) Then
               Tbl_horaant(tmpb) = Tbl_hora(tmpb)
               Call Gendig(tbl_hora(tmpb) , Tmpb)
            End If
         Next
'         Call Gendig(tmpb , 2)
         Call Gendp(2)
         Call Gendp(1)

      End If

   End If

   If Tempflag = 1 Then                                     ' Mostrando temperaura
      If Newseg = 1 Then

         If Len(tempestr4) = 4 Then
            Select Case Signo
               Case "+":
                  Tbltemp(1) = 11

               Case "-":
                  Tbltemp(1) = 12

               Case Else
                  Tbltemp(1) = 10

            End Select

            Tmpstr = Mid(tempestr4 , 1 , 1)
            Tbltemp(2) = Val(tmpstr)
            Tmpstr = Mid(tempestr4 , 2 , 1)
            Tbltemp(3) = Val(tmpstr)
            Tmpstr = Mid(tempestr4 , 4 , 1)
            Tbltemp(4) = Val(tmpstr)
            Tbltemp(5) = 13
         End If

         If Len(tempestr4) = 3 Then
            Tbltemp(1) = 10
            Select Case Signo
               Case "+":
                  Tbltemp(2) = 11

               Case "-":
                  Tbltemp(2) = 12

               Case Else
                  Tbltemp(2) = 10

            End Select

            Tmpstr = Mid(tempestr4 , 1 , 1)
            Tbltemp(3) = Val(tmpstr)
            Tmpstr = Mid(tempestr4 , 2 , 1)
            Tbltemp(4) = Val(tmpstr)
            Tbltemp(5) = 13
         End If

          For K1 = 1 To 5
               Call Gendigt(tbltemp(k1) , K1)
          Next

          Call Gensp(1)
          Call Gengrado(1)


      End If


   End If

   If Leertemp = 1 Then
      If Initemp = 1 Then
         Reset Initemp
         Call Temperatura()
      End If


   End If

    If Gpsnew = 1 Then
      Reset Gpsnew
      'Print #1 , Gpsproc
      Numpar = Split(gpsproc , Cmdsplit(1) , ",")
      If Numpar > 0 Then
         If Cmdsplit(3) = "A" Then
            'Print #1 , Cmdsplit(2) ; ", " ; Cmdsplit(10)
            Hora = Mid(cmdsplit(2) , 1 , 2) + ":" + Mid(cmdsplit(2) , 3 , 2) + ":" + Mid(cmdsplit(2) , 5 , 2)
            'Print #1 , Hora
            Fecha = Mid(cmdsplit(10) , 1 , 2) + "/" + Mid(cmdsplit(10) , 3 , 2) + "/" + Mid(cmdsplit(10) , 5 , 2)
            'Print #1 , Fecha
            Tmpl = Syssec(hora , Fecha)
            Tmpl = Tmpl - 18000
            Time$ = Time(tmpl)
            Date$ = Date(tmpl)
            Print #1 , Time$
            Print #1 , Date$

         Else
            Incr Cntr2
            Cntr2 = Cntr2 Mod 60
            If Cntr2 = 0 Then
             Print #1 , Cmdsplit(2) ; ", " ; Cmdsplit(10)
            End If

         End If
      End If
      Enable Urxc1
    End If

    Call Leersta()

    If Newframe = 1 Then
      Reset Newframe
      'Set Pinbug
      'If Status < 7 Then
'         If Cntrframe.0 = 0 Then
            For Kw = 1 To 768
               Ptrtx = Tmpptrtx + Kw
               Buffram(kw) = Buf1(ptrtx)
            Next
            'Print #1 , "B1, Ptr=" ; Ptrtx
'         Else
            'Print #1 , "B2"
'            For Kw = 1 To 768
'               Ptrtx = Tmpptrtx + Kw
'               Buffram(kw) = Buf2(ptrtx)
'            Next
            'Print #1 , "B2"
'            Print #1 , "B2, Ptr=" ; Ptrtx
'         End If
         Tmpptrtx = Ptrtx
         Tmpptrtx = Tmpptrtx Mod 9984
         'Print #1 , "Tmp=" ; Tmpptrtx ; "," ; Cntrframe.0
      'End If
      'Reset Pinbug
    End If

Loop