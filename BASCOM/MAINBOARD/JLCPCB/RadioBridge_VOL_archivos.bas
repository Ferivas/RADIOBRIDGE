'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  RBrdg_Archivos.bas                                                        *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*                                                                             *
'*  Variables, Subrutinas y Funciones                                          *
'* WATCHING SOLUCIONES TECNOLOGICAS                                            *
'* 25.06.2015                                                                  *
'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

$nocompile
$projecttime = 33


'*******************************************************************************
'Declaracion de subrutinas
'*******************************************************************************
Declare Sub Inivar()
Declare Sub Procser()
Declare Sub Espera(byval Tespera As Word)
Declare Sub Gentrama()
'Declare Sub Rxdtmf()                                        'Recibe tonos DTMF
Declare Sub Txdtmf()                                        'Transmite tonos DTMF
Declare Sub Leersta()
Declare Sub Leeradc()
Declare Sub Outsubsta(byval Datosta As Byte)
Declare Sub Procdtmf()
Declare Sub Proci2c()
Declare Sub Defaultvalues()

'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte
Dim Tmpb2 As Byte
Dim Tmpb3 As Byte
Dim Tmpb4 As Byte
'Dim Tmpcntrsta As Byte

Dim Tmps As Single
Dim Bs1 As Byte At Tmps Overlay
Dim Bs2 As Byte At Tmps + 1 Overlay
Dim Bs3 As Byte At Tmps + 2 Overlay
Dim Bs4 As Byte At Tmps + 3 Overlay


Dim Drvaudiook As Bit
Dim Newdrv As Bit

Dim J As Byte
Dim K As Byte
Dim Tmpw As Word
Dim Tmpw2 As Word
'Dim Tmpdw As Dword
Dim Enabug As Byte

Dim Dispbug As Byte
Dim Inirst As Bit
Dim Cntrrst As Word

Dim Cmdtmp As String * 6
Dim Atsnd As String * 200
Dim Cmderr As Byte
'Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 52

Dim Trqststa As Word                                        'Tiempo de consulta a estaciones
Dim Trqststaeep As Eram Word
Dim Cntrseg As Byte
'Dim Cntrmin As Byte
Dim Tani As Byte
Dim Numani As Word

Dim Trqsper As Word                                         ' Tiempo de consultas periodico a estaciones
Dim Trqspereep As Eram Word

Dim Numestacion As Byte                                     'NUmero de estacion
Dim Numestacioneep As Eram Byte


Dim Cntrtx As Single                                        'Contador de transmisiones
Dim Cntrtxeep As Eram Single

Dim Tampersta As Bit
Dim Inivariables As Bit

Dim Tmprxdtmf As Byte
Dim Tmpstatus As Byte

Dim Status As Byte                                          'Almacena estado
Dim Statusant As Byte
Dim Statusrx As Byte
Dim Substa As Byte                                          ' Alamcena subestado + errrores
Dim Substaant As Byte
Dim Insta As Bit
Dim Outsta As Bit
Dim Newsubsta As Bit
Dim Valsta As Byte
Dim Ptrani As Word

Dim Cntrsubsta As Byte
Dim Modooff As Bit
Dim Volumen As Byte
Dim Inileei2c As Bit
Dim Cntrleci2c As Byte
Dim Cntrtryi2c As Byte
Dim Vali2c(numi2c) As Byte

Dim Tramatx(16) As Byte                                     'Trama de transmision
Dim Tramatxdtmf(32) As Byte                                 ' Alamcena los codigos para generar los DTMF

Dim Tonodtmf As Byte
Dim Ptrrxdtmf As Byte
Dim Tbl_rxdtmf(22) As Byte                                  'Tabla para alamcenar tonos recibidos
Dim Tbl_rxhex(12) As Byte                                   'TABLA PARA ALMACENAR datos en Hex

Dim Regb As Byte
Dim Newdv As Bit

Dim Lecsta As Byte

Dim Newtxalerta As Bit                                      'Bit para generar Tx de Alerta Naranja a todas las estaciones

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
Dim Vdc3 As Single

Dim Vbat1 As Byte At Vbat Overlay
Dim Vbat2 As Byte At Vbat + 1 Overlay
Dim Vbat3 As Byte At Vbat + 2 Overlay
Dim Vbat4 As Byte At Vbat + 3 Overlay

Dim Vps1 As Byte At Vps Overlay
Dim Vps2 As Byte At Vps + 1 Overlay
Dim Vps3 As Byte At Vps + 2 Overlay
Dim Vps4 As Byte At Vps + 3 Overlay


'
Dim Cntrcrcok As Word
Dim Cntrcrcokeep As Eram Word

Dim Cntrcrcbad As Word
Dim Cntrcrcbadeep As Eram Word

Dim Topi2c(numi2c) As Byte
Dim Topi2ceep(numi2c) As Eram Byte
Dim Tmpi2c As Byte
Dim Tmpcanal As Byte
Dim Newmsg As Bit
Dim Tmpmsg As Byte
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
   Serrx = Udr1
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
         End If
      Case Is > 31
         If Ser_ini = 1 Then
            Serdata = Serdata + Chr(serrx)
         End If
   End Select
Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = &H64
   'Toggle Pinbug
   Incr T0c
   T0c = T0c Mod 8
   If T0c = 0 Then
      'Toggle Pinbug
      Num_ventana = Num_ventana Mod 32
      Estado = Lookup(estado_led , Tabla_estado)
      Iluminar = Estado.num_ventana
      'Toggle Iluminar
      Led1 = Iluminar
      'Pinbug = Iluminar
      Incr Num_ventana
   End If

   Incr Cntrt0
   Cntrt0 = Cntrt0 Mod 100
   If Cntrt0 = 0 Then
      Set Newadc                                            'Ints cada segundo
      Set Newdrv
      'Set Newmsg
      Incr Cntrseg
      Cntrseg = Cntrseg Mod Tani
      If Cntrseg = 0 Then
         'Incr Cntrmin
         'Cntrmin = Cntrmin Mod 4
         Set Newsubsta
      End If
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
      Cntrrst = Cntrrst Mod 500
      If Cntrrst = 0 Then
         Ptrrxdtmf = 0
         Reset Inirst
         'Reset Pinbug
      End If
   End If

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
      Incr Ptrrxdtmf
      Tbl_rxdtmf(ptrrxdtmf) = Tonodtmf
      Ptrrxdtmf = Ptrrxdtmf Mod 20
      If Ptrrxdtmf = 1 Then
         Set Inirst
      End If
      'Set Newdv
      If Ptrrxdtmf = 0 Then
         Set Newdv
      End If
      If Ptrrxdtmf > 14 Then
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
Reset Rele
Set Ce
Print #1 , "************ RADIOBRIDGE ************"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)
Estado_led = 1
Status = 1
Numani = 300
Statusrx = 1
Substa = 1                                                  'Se inicia sin comunicacion con el DRVAUDIO

Numestacion = Numestacioneep
Print #1 , "Numestacion=" ; Numestacion

For Adcn = 1 To Numadc
   Adck(adcn) = Adckeep(adcn)
   Print #1 , "Adck(" ; Adcn ; ")=" ; Adck(adcn)
Next


Cntrtx = Cntrtxeep
Print #1 , "Contador de transmisiones =" ; Cntrtx
Reset Drvaudiook
Tani = 10
Print #1 , "Tani=" ; Tani

Cntrcrcok = Cntrcrcokeep
Print #1 , "CNTRCRCOK=" ; Cntrcrcok

Cntrcrcbad = Cntrcrcbadeep
Print #1 , "CNTRCRCBAD=" ; Cntrcrcbad
Volumen = 20
Print #1 , "Vol=" ; Volumen

For Tmpb = 1 To Numi2c
   Topi2c(tmpb) = Topi2ceep(tmpb)
   Print #1 , "TopI2C" ; Tmpb ; "=" ; Topi2c(tmpb)
Next

End Sub

Sub Defaultvalues()
   Adckeep(1) = 0.05494499
   Adckeep(2) = 0.05494499
   Adckeep(3) = 0.05494499
   Numestacioneep = 14
   Cntrtxeep = 0
   Cntrcrcokeep = 0
   Cntrcrcbadeep = 0
   Topi2ceep(1) = 128
   Topi2ceep(2) = 128
   Topi2ceep(3) = 128
   Topi2ceep(4) = 128
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


Sub Proci2c()
   For Tmpb = 1 To Numi2c
      Tmpb2 = Tmpb - 1
      If Vali2c(tmpb) < Topi2c(tmpb) Then
         Reset Tmpi2c.tmpb2
      Else
         Set Tmpi2c.tmpb2
      End If
   Next
   Print #1 , Bin(tmpi2c)

End Sub

Sub Gentrama()
Print #1 , "Trama TX DTMF"
   Tramatx(1).0 = Numestacion.0
   Tramatx(1).1 = Numestacion.1
   Tramatx(1).2 = Numestacion.2
   Tramatx(1).3 = Numestacion.3
   Tramatx(1).4 = Status.0
   Tramatx(1).5 = Status.1
   Tramatx(1).6 = Status.2
   Tramatx(1).7 = Status.3

   Tmps = Cntrtx
   Tramatx(2) = Bs1
   Tramatx(3) = Bs2
   Tramatx(4) = Bs3
   Tramatx(5) = Bs4

   Tramatx(6).0 = Substa.0
   Tramatx(6).1 = Substa.1
   Tramatx(6).2 = Substa.2
   Tramatx(6).3 = Substa.3
   Call Proci2c()
   Tramatx(6).4 = Tmpi2c.0
   Tramatx(6).5 = Tmpi2c.1
   Tramatx(6).6 = Tmpi2c.2
   Tramatx(6).7 = Tmpi2c.3

   Tramatx(7) = Vbat1
   Tramatx(8) = Vbat2
   Tramatx(9) = Vbat3
   Tramatx(10) = Vbat4

   Tramatx(11) = Vps1
   Tramatx(12) = Vps2
   Tramatx(13) = Vps3
   Tramatx(14) = Vps4

   Tmpb = Crc8(tramatx(1) , 14)
   Tramatx(15) = Tmpb

   Tramatxdtmf(1) = Numestacion
   Tramatxdtmf(2) = Status
   Tmpb4 = 3
   For Tmpb = 1 To 14
      Tmpb2 = Tmpb + 1
      Tmpb3 = Tramatx(tmpb2)
'      Print #1 , "Tramatx(" ; Tmpb2 ; ")=" ; Hex(tmpb3) ; ";"
      Tramatxdtmf(tmpb4) = Tmpb3 And &H0F
'      Print #1 , "Tramatxdtmf(" ; Tmpb4 ; ")=" ; Hex(tramatxdtmf(tmpb4)) ; ","
      Incr Tmpb4
      Shift Tmpb3 , Right , 4
      Tramatxdtmf(tmpb4) = Tmpb3
'      Print #1 , "Tramatxdtmf(" ; Tmpb4 ; ")=" ; Hex(tramatxdtmf(tmpb4)) ; ","
      Incr Tmpb4
   Next



End Sub


'*******************************************************************************
' Subrutina de Tx de datos DTMF
'*******************************************************************************
Sub Txdtmf()
   Set Ptt
   Call Espera(100)
   For J = 1 To 30
      'Print #1 , J ; "," ; Hex(tramatxdtmf(j))
      Dout0 = Tramatxdtmf(j).0
      Dout1 = Tramatxdtmf(j).1
      Dout2 = Tramatxdtmf(j).2
      Dout3 = Tramatxdtmf(j).3
      Set Ledtx
      Reset Ce
'      Call Espera(8)
      Call Espera(8)
      Set Ce
      Reset Ledtx
'      Call Espera(4)
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
   If Statusrx = 1 Then
      Lecsta = 0
      Lecsta.0 = D0
      Lecsta.1 = D1
      Select Case Status
         Case 1:
            If Modooff = 0 Then
               Estado_led = 1                               'Normal
            Else
               Estado_led = 11
            End If


            If Lecsta = 2 Then
               Print #1 , "Modo test Local"
               Status = 2
               Reset Drvaudiook
               Estado_led = 2
               Numani = 30
            End If

            If Lecsta = 0 Or Lecsta = 1 Then
               Print #1 , "Modo Alerta Naranja Local"
               Status = 3
               Estado_led = 3
               Reset Drvaudiook
               Set Newtxalerta
               Numani = 30
            End If

         Case 2:                                            'Test
            If Lecsta = 3 Then
               Print #1 , "A Modo Normal desde Modo Test Local"
               Status = 1
               Estado_led = 1
               Reset Drvaudiook
               Numani = 60
               If Modooff = 1 Then
                  Estado_led = 11
               End If

            End If

         Case 3:                                            'Naranja
            If Lecsta = 3 Then
               Print #1 , "A Modo Normal desde A. Naranja Local"
               Status = 1
               Estado_led = 1
               Reset Drvaudiook
               Numani = 60

               If Modooff = 1 Then
                  Estado_led = 11
               End If

            End If

      End Select

      If Status <> Statusant Then
         Reset Drvaudiook
         Set Inileei2c
         Statusant = Status
         Print #1 , "Cambio de Status=" ; Status

         Select Case Status
            Case 1:
               Numani = 60
            Case 2:
               Numani = 30
            Case 3:
               Numani = 30
         End Select
         Ptrani = 0
      End If

   Else
      Status = Statusrx
      Estado_led = Status

      If Status = 1 And Modooff = 1 Then
         Estado_led = 11
      End If

      If Status <> Statusant Then
         Reset Drvaudiook
         Statusant = Status
         Print #1 , "Nuevo Status Recibido por Radio=" ; Status
         Set Inileei2c
         Select Case Status
            Case 1:
               Numani = 600
            Case 2:
               Numani = 30
            Case 3:
               Numani = 30
         End Select
         Ptrani = 0

      End If

   End If

   If Status = 1 Then
      Reset Rele
   End If

   If Status = 2 Then
      Set Rele
   End If

   If Status = 3 Then
      Set Rele
   End If

   If Status = 0 Then
      Reset Rele
   End If

   If Status > 3 Then
      Reset Rele
   End If

End Sub

'*******************************************************************************
' Subrutina para configurar valores de salida de pines de configuración
'*******************************************************************************
Sub Outsubsta(byval Datosta As Byte)
   Drv0 = Datosta.0
   Drv1 = Datosta.1
   Drv2 = Datosta.2
   Drv3 = Datosta.3
'   If Substa <> Substaant Then
'      Substaant = Substa
'      Print #1 , "SUBSTA=" ; Substa
'   End If


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
      Vdc3 = Adcval(3)

   End If


End Sub

'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   Print #1 , Serproc
   Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   'If Numpar > 0 Then
   '   For Tmpb = 1 To Numpar
   '      Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
   '   Next
   'End If

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

         Case "LEELED"
            Cmderr = 0
            Atsnd = "Estado LED=" + Str(estado_led)

         Case "SETTXP"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpw = Val(cmdsplit(2))
               Trqsper = Tmpw
               Trqspereep = Tmpw
               Atsnd = "Se configuro tiempo de tx periodicas a " + Str(trqsper) + " s"

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


         Case "LEETXP"
            Cmderr = 0
            Atsnd = "Tiempo de tx periodicas =" + Str(trqsper) + " s"

         Case "LEETTE"
            Cmderr = 0
            Atsnd = "Tiempo de tx entre estaciones =" + Str(trqststa) + " s"

         Case "LEECTX"
            Cmderr = 0
            Atsnd = "Contador de tx=" + Str(cntrtx)


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

         Case "SETSTA"
            If Numpar = 2 Then
               Cmderr = 0
               Numestacion = Val(cmdsplit(2))
               Numestacioneep = Numestacion
               Atsnd = "Se configuro numero de estacion =" + Str(numestacion)
            Else
               Cmderr = 4
            End If

         Case "LEESTA"
               Cmderr = 0
               Atsnd = "numero de estacion =" + Str(numestacion)

         Case "RXDTMF"
            'Cmderr = 0
            If Numpar = 21 Then
               Cmderr = 0
               Atsnd = "Simulacion de RX tonos DTMF"
               Print #1 , "Estado_led1=" ; Estado_led
               Tmpb = 0
               For J = 2 To 21
                  Incr Tmpb
                  Tmpb2 = Hexval(cmdsplit(j))
                  Tbl_rxdtmf(tmpb) = Tmpb2
               Next
               Set Newdv
               Print #1 , "Estado_led2=" ; Estado_led

            Else
               Cmderr = 4
            End If

         Case "SETADC"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb <= Numadc Then
                  Cmderr = 0
                  Tmps = Val(cmdsplit(3))
                  Adck(tmpb) = Tmps
                  Adckeep(tmpb) = Tmps
                  Atsnd = "Se configuro ADC " + Str(tmpb) + "=" + Str(adck(tmpb))

               Else
                  Cmderr = 5
               End If

            Else
               Cmderr = 4
            End If

         Case "LEEADC"
            Cmderr = 0
            Atsnd = "Valores ADC "
            For K = 1 To Numadc
               Atsnd = Atsnd + Str(k) + "=" + Str(adck(k)) + ","
            Next

         Case "SETDBG"
            Cmderr = 0
            Atsnd = "Se habilita display debug"
            Dispbug = 1

         Case "DRVAUD"
            Select Case Numpar
               Case 2:
                  Cmderr = 0
                  Atsnd = "Respuesta DRV AUDIO=" + Cmdsplit(2)
                  Set Drvaudiook
                  Substa = 0
               Case 3:
                  Cmderr = 0
                  Atsnd = "Respuesta DRV AUDIO=" + Cmdsplit(2) + "," + Cmdsplit(3)
                  Set Drvaudiook
                  Substa = 0
               Case Else:
                  Cmderr = 4
            End Select

         Case "SETANI"
            If Numpar = 2 Then
               Cmderr = 0
               Tani = Val(cmdsplit(2))
               Atsnd = "Se configura Tani=" + Str(tani)
            Else
               Cmderr = 4
            End If

         Case "LEEANI"
            Cmderr = 0
            Atsnd = "Tani=" + Str(tani)

         Case "SETPTR"
            If Numpar = 2 Then
               Cmderr = 0
               Ptrani = Val(cmdsplit(2))
               Atsnd = "Se configura Ptrani=" + Str(ptrani)
            Else
               Cmderr = 4
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

         Case "LEEBAT"
            Cmderr = 0
            Atsnd = "VBAT=" + Fusing(vbat , "#.##") + " , VPS=" + Fusing(vps , "#.##")

         Case "RSTVAR"
            Cmderr = 0
            Call Defaultvalues()
            Atsnd = "Reinicio a valores por defecto"
            Set Inivariables

         Case "INIVAR"
            Cmderr = 0
            Set Inivariables
            Atsnd = "Inicializa variables"


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
            Atsnd = "Modo OFF=" + Str(modooff)

         Case "SETDSP"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               Call Outsubsta(tmpb)
               Atsnd = "SE configura salida dels display a " + Str(tmpb)
               cmderr=0

            Else
               Cmderr = 4
            End If

         Case "SETREL"
            If Numpar = 2 Then
               Cmderr = 0
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  If Tmpb = 1 Then
                     Set Rele
                  Else
                     Reset Rele
                  End If
                  Atsnd = "rele=" + Str(tmpb)
                  Wait 3
                  print #1, "Fin rel"

               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETBUG"
            If Numpar = 2 Then
               Cmderr = 0
               Enabug = Val(cmdsplit(2))
               Atsnd = "Se config ENABUG=" + Str(enabug)
            Else
               Cmderr = 4
            End If

         Case "LEEBUG"
            Cmderr = 0
            Atsnd = "ENABUG=" + Str(enabug)

         Case "LEEVDC"
            Cmderr = 0
            Atsnd = "Vbat=" + Fusing(vbat , "#.#") + ", Vps=" + Fusing(vps , "#.#") + ", Vdc3=" + Fusing(vdc3 , "#.#")

         Case "LEEI2C"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numi2c_masuno Then
                  Tmpb2 = Val(cmdsplit(3))
                  'Incr Tmpb
                  Vali2c(tmpb) = Tmpb2
                  Atsnd = "Se conf I2C" + Str(tmpb) + "=" + Str(tmpb2)
                  Cmderr = 0
               Else
                  Cmderr = 5
               End If

            Else
               Cmderr = 4
            End If

         Case "VALI2C"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numi2c_masuno Then
                  Cmderr = 0
                  Atsnd = "ValI2C " + Str(tmpb) + "=" + Str(vali2c(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "TOPI2C"
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb > 0 And Tmpb < Numi2c_masuno Then
                  Cmderr = 0
                  Topi2c(tmpb) = Val(cmdsplit(3))
                  Topi2ceep(tmpb) = Topi2c(tmpb)
                  Atsnd = "Se config top lim I2C" + Str(tmpb) + " a " + Str(topi2c(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEETOP"
            If Numpar = 2 Then
               If Tmpb > 0 And Tmpb < Numi2c_masuno Then
                  Cmderr = 0
                  Atsnd = "Top lim I2C" + Str(tmpb) + "=" + Str(topi2c(tmpb))
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "TMPI2C"
            Cmderr = 0
            Atsnd = "TmpI2C=" + Bin(tmpi2c)

         Case "SETMSG"
            If Numpar = 3 Then
               Tmpmsg = Val(cmdsplit(2))
               If Tmpmsg < Nummsg_masuno Then
                  Cmderr = 0
                  Volumen = Val(cmdsplit(3))
                  Set Newmsg
                  Set Inileei2c
                  Atsnd = "Rep. MSG " + Str(tmpmsg) + " con Vol " + Str(volumen)

               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
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


Sub Procdtmf()
   Print #1 , "RX NEW DTMF"

   Tmprxdtmf = Tbl_rxdtmf(5)
   Tmpstatus = Tbl_rxdtmf(6)

   Tmpb3 = 0
   For J = 1 To 20
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
   For J = 1 To 10
      Print #1 , Hex(tbl_rxhex(j)) ; ",";
   Next
   Print #1 ,
   Tmpw = Crc16(tbl_rxhex , 8)
   Print #1 , "CRC=" ; Hex(tmpw)

   Tmpw2 = Makeint(tbl_rxhex(10) , Tbl_rxhex(9))
   Print #1 , "CRCcal=" ; Hex(tmpw2)


   If Tmpw = Tmpw2 Then
      Print #1 , "CRC OK=";
      Incr Cntrcrcok
      Cntrcrcokeep = Cntrcrcok
      Print #1 , Cntrcrcok

      If Tbl_rxhex(1) = Cini Then
         If Tbl_rxhex(2) = Idmaster Then
            Print #1 , "Trama valida"
            Print #1 , "Volumen=" ; Tbl_rxhex(4)
            Volumen = Tbl_rxhex(4)
            If Numestacion = Tmprxdtmf Then
               Incr Cntrtx
               Cntrtxeep = Cntrtx
               Print #1 , "Cntrtx=" ; Cntrtx
               Call Gentrama()
               Print #1 , "Trama HEX"
               For J = 1 To 15
                  Print #1 , Hex(tramatx(j)) ; ",";
               Next
               Print #1,
               'Statusrx = Tbl_rxdtmf(2)
               If Tmpstatus.3 = 0 Then
                  Reset Modooff
                  Print #1 , "MODOOFF=0," ; Modooff
                  Estado_led = 1
               Else
                  Set Modooff
                  Print #1 , "MODOOFF=1," ; Modooff
                  Estado_led = 11
               End If
               Reset Tmpstatus.3
               Statusrx = Tmpstatus
               Print #1 , "STATUS RX=" ; Statusrx
               Status = Statusrx

               Print #1,
               Print #1 , "Trama DTMF"
               Print #1 , "RXDTMF";
               'Print #2 , "$RXDTMF";
               For J = 1 To 30
                  Print #1 , "," ; Hex(tramatxdtmf(j)) ;
                  'Print #2 , "," ; Hex(tramatxdtmf(j)) ;
               Next
               Print #1,
               'Print #2,
               Call Espera(200)
               Call Txdtmf()
            Else

               If Tmprxdtmf = &H0F Then
                  Print #1 , "Trama ALARMA"
                  Statusrx = 3
                  Status = 3
                  Estado_led = 3
                  Print #1 , "STATUS RX=" ; Statusrx
                  Reset Modooff
               Else
                  If Tmprxdtmf = &H0E Then
                     Print #1 , "Trama TEST"
                     Statusrx = 2
                     Status = 2
                     Estado_led = 2
                     Reset Modooff
                     Print #1 , "STATUS RX=" ; Statusrx
                  Else
                     If Tmprxdtmf = &H0D Then                  'Trama de vuelta a Normal
                        Print #1 , "Trama NORMAL"
                        Reset Modooff
                        Statusrx = 1
                        Status = 1
                        Estado_led = 1
                     Else
                        If Tmprxdtmf = &H0C Then
                           Print #1 , "Modo OFF nocturno"
                           Statusrx = 1
                           Status = 1
                           Estado_led = 11
                           Set Modooff
                        Else
                           Print #1 , "NO es TEST ni alarma . No responde"
                        End If
                     End If
                  End If
               End If
            End If
         Else
            Tmpb = Tbl_rxhex(2) And &HF0
            If Tmpb = &HF0 Then
               Print #1 , "Trama MSG val"
               If Numestacion = Tmprxdtmf Then
                  Tmpmsg = Tbl_rxhex(2) And &H0F
                  Print #1 , "MSG=" ; Tmpmsg
                  Set Newmsg
                  Print #1 , "Volumen=" ; Tbl_rxhex(4)
                  Volumen = Tbl_rxhex(4)
                  Incr Cntrtx
                  Cntrtxeep = Cntrtx
                  Print #1 , "Cntrtx=" ; Cntrtx
                  Call Gentrama()
                  Print #1 , "Trama HEX"
                  For J = 1 To 15
                     Print #1 , Hex(tramatx(j)) ; ",";
                  Next
                  Print #1,
                  Print #1 , "Trama DTMF"
                  Print #1 , "RXDTMF";
                  'Print #2 , "$RXDTMF";
                  For J = 1 To 30
                     Print #1 , "," ; Hex(tramatxdtmf(j)) ;
                     'Print #2 , "," ; Hex(tramatxdtmf(j)) ;
                  Next
                  Print #1,
                  'Print #2,
                  Call Espera(200)
                  Call Txdtmf()
               Else
                  Print #1 , "MSG no para este esclavo"
               End If
            Else
               Print #1 , "Trama no impl"
            End If
         End If
      Else
         Print #1 , "FAIL ID Trama"
      End If
   Else
      Print #1 , "CRC fail=";
      Incr Cntrcrcbad
      Cntrcrcbadeep = Cntrcrcbad
      Print #1 , Cntrcrcbad

   End If
   Print #1,

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


Tbl_normal:
Data 0                                                      ' bievenidos
Data 0                                                      '1
Data 0
Data 0
Data 1                                                      'velocidad maxima
Data 1
Data 1
Data 1
Data 3                                                      'ETIQUETA TEMPERATURA
Data 4                                                      'VALOR temperatura
Data 4                                                      '
Data 4
Data 6                                                      'hora
Data 6
Data 6
Data 6
Data 6
Data 6
Data 7                                                      'apaga
Data 0                                                      ' Ani1
Data 0                                                      '1
Data 0                                                      '21
Data 0
Data 1
Data 1
Data 1
Data 1
Data 3                                                      'EVACUACION
Data 4                                                      'VELOCIDAD
Data 4
Data 4
Data 6                                                      '30
Data 6
Data 6
Data 6
Data 6
Data 6
Data 7
Data 0                                                      ' Ani1
Data 0                                                      '1
Data 0
Data 0                                                      '40
Data 1
Data 1
Data 1
Data 1
Data 3                                                      'EVACUACION
Data 4                                                      'VELOCIDAD
Data 4
Data 4
Data 6
Data 6                                                      '50
Data 6
Data 6
Data 6
Data 6
Data 4
Data 4
Data 6
Data 6
Data 7                                                      '60
Data 7
Data 7                                                      '60
Data 7
Data 7



Tbl_nocturna:
Data 6                                                      ' Ani1
Data 6                                                      '1
Data 6
Data 6
Data 6
Data 6
Data 4
Data 4
Data 6                                                      'EVACUACION
Data 6                                                      'VELOCIDAD
Data 6                                                      '10
Data 6
Data 6
Data 6
Data 4
Data 4
Data 7
Data 6
Data 6                                                      '18
Data 6                                                      ' Ani1
Data 6                                                      '1
Data 6                                                      '21
Data 6
Data 4
Data 4
Data 6
Data 6
Data 6                                                      'EVACUACION
Data 6                                                      'VELOCIDAD
Data 6
Data 6
Data 4                                                      '30
Data 4
Data 6
Data 6
Data 6
Data 6
Data 6
Data 6                                                      ' Ani1
Data 4                                                      '1
Data 4
Data 6                                                      '40
Data 6
Data 6
Data 6
Data 6
Data 6                                                      'EVACUACION
Data 4                                                      'VELOCIDAD
Data 4
Data 7
Data 6
Data 6                                                      '50
Data 6
Data 6
Data 6
Data 6
Data 4
Data 4
Data 6
Data 6
Data 6                                                      '60
Data 6
Data 6                                                      '60
Data 6
Data 6



Tbl_test:
Data 8
Data 8
Data 8
Data 8
Data 8
Data 8

Data 8
Data 8
Data 8
Data 8
Data 8
Data 8

Data 6
Data 6
Data 6
Data 8
Data 8
Data 8

Data 8
Data 8
Data 8
Data 8
Data 8
Data 8

Data 8
Data 8
Data 8
Data 6
Data 6
Data 6

Tbl_alarma:
Data 12
Data 12
Data 12
Data 12
Data 12
Data 12

Data 12
Data 12
Data 12
Data 12
Data 12
Data 12

Data 12
Data 12
Data 12
Data 12
Data 12
Data 12

Data 12
Data 12
Data 12
Data 12
Data 12
Data 12


Data 12
Data 12
Data 12
Data 12
Data 12
Data 12



Loaded_arch: