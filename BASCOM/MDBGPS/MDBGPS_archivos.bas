'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
'*  RBrdg_Archivos.bas                                                        *
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
Declare Sub Espera(byval Tespera As Word)

Declare Function Bit_index_byte(byval Bit_index As Word , Lista() As Byte) As Byte
Declare Sub Copy_inputs_to_table
Declare Sub Write_single_coil(byval Coil As Word , Byval Value As Byte)
Declare Sub Modbus_exec
Declare Sub Send_exception


'*******************************************************************************
'Declaracion de variables
'*******************************************************************************
Dim Tmpb As Byte
Dim Tmpb2 As Byte
Dim Tmpb3 As Byte
Dim Tmpb4 As Byte
Dim Tmpb5 As Byte
Dim Tmpcntrsta As Byte

Dim Ptrhdr As Byte
Dim Ptrbhdr As Byte
Dim Tmphdr As Byte
Dim Inileerhr As Bit
Dim Tonotest As Byte

'Dim Modooff As Bit
'Dim Modopinsta As Bit

'Dim Broadcast As Bit
'Dim Inibroadcast As Bit
'Dim Newbroadcast As Bit
'Dim Cntrtxbroadcast As Byte

'Dim Cntrseg2 As Byte

Dim Newtst As Bit
Dim Statst As Byte                                          ' Numero de estacion de prueba
Dim Estadotst As Byte                                       ' Estado de prueba
Dim Tmpstatus As Byte

Dim Statusrx As Byte


Dim J As Byte
Dim Tmpw As Word
Dim Tmpw2 As Word

Dim Tmpdw As Dword
Dim Bdw1 As Byte At Tmpdw Overlay
Dim Bdw2 As Byte At Tmpdw + 1 Overlay
Dim Bdw3 As Byte At Tmpdw + 2 Overlay
Dim Bdw4 As Byte At Tmpdw + 3 Overlay

Dim Tmps As Single
Dim Bs1 As Byte At Tmps Overlay
Dim Bs2 As Byte At Tmps + 1 Overlay
Dim Bs3 As Byte At Tmps + 2 Overlay
Dim Bs4 As Byte At Tmps + 3 Overlay


Dim Cmdtmp As String * 6
Dim Atsnd As String * 160
Dim Cmderr As Byte
Dim Tmpstr8 As String * 16
Dim Tmpstr52 As String * 82

'Dim Trqststa As Word                                        'Tiempo de consulta a estaciones
'Dim Trqs As Word

'Dim Trqststaeep As Eram Word
'Dim Cntrseg As Word

'Dim Trqsnormal As Word                                      ' Tiempo de consultas periodico a estaciones
'Dim Trqsnormaleep As Eram Word

'Dim Trqsalarma As Word                                      ' Tiempo de consultas modo Alarma/Test a estaciones
'Dim Trqsalarmaeep As Eram Word


'Dim Cntrestaciones As Byte

'Dim Cntrtx As Single                                        'Contador de transmisiones
'Dim Cntrtxeep As Eram Single

'Dim Tbl_adc1(numsta) As Single                              ' Para almacenar los valores de voltaje bateria de la estaciones remotas
'Dim Tbl_adc2(numsta) As Single                              ' Para almacenar los valores de voltaje panel de la estaciones remotas
'Dim Tbl_adc3(numsta) As Single

'Dim Tbl_stain(numsta) As Byte                               ' Almacena estado de entrada digital
'Dim Tbl_staout(numsta) As Byte                              ' Almacena estado de salida digital

'Dim Tbl_status(numsta) As Byte                              ' Almacena estado de las estaciones (estatus y errores)
'Dim Tbl_ctx(numsta) As Dword

Dim Status As Byte                                          'Almacena estado
'Dim Statusant As Byte
'Dim Tramatx(6) As Byte                                      'Trama de transmision
'Dim Tramatxdtmf(12) As Byte                                 ' Alamcena los codigos para generar los DTMF

'Dim Tonodtmf As Byte
'Dim Ptrrxdtmf As Byte
'Dim Tbl_rxdtmf(32) As Byte                                  'Tabla para alamcenar tonos recibidos
'Dim Tbl_rxhex(16) As Byte

'Dim Regb As Byte
'Dim Newdv As Bit
'Dim Cntrtxbroadcast As Byte



'Variables TIMER0
Dim T0c As Byte
Dim Num_ventana As Byte
Dim Estado As Long
Dim Estado_led As Byte
Dim Iluminar As Bit
Dim Cntrt0 As Byte
Dim Newtx As Bit                                            'Indica nueva tx
Dim Newtxper As Bit

'Dim Cntrtxper As Word

'TIMER2
Dim Inidelay As Bit
Dim Cntrdelay As Word
Dim T0delay As Bit
Dim Topdelay As Word


'************************************************************
' Variables definitions
'************************************************************
Dim Mbuf_rcv(128) As Byte
Dim Modbus_slave_adress_received As Byte At Mbuf_rcv(1) Overlay
Dim Modbus_function As Byte At Mbuf_rcv(2) Overlay
Dim Modbus_device_adress_hi As Byte At Mbuf_rcv(3) Overlay
Dim Modbus_device_adress_lo As Byte At Mbuf_rcv(4) Overlay
Dim Modbus_device_adress As Word
Dim Mb_number_of_devices_hi As Byte At Mbuf_rcv(5) Overlay
Dim Mb_number_of_devices_lo As Byte At Mbuf_rcv(6) Overlay
Dim Modbus_number_of_devices As Word
Dim Modbus_data_byte(8) As Byte At Mbuf_rcv(4) Overlay
Dim Modbus_register_read(maximum_holding_registers) As Word At Mbuf_rcv(4) Overlay
Dim Modbus_registers_write(maximum_holding_registers) As Word At Mbuf_rcv(5) Overlay
Dim Modbus_register_write_hi As Byte At Mbuf_rcv(5) Overlay
Dim Modbus_register_write_lo As Byte At Mbuf_rcv(6) Overlay
Dim Mb_f15_count As Byte At Mbuf_rcv(7) Overlay
Dim Mb_f15_data(8) As Byte At Mbuf_rcv(8) Overlay

Dim Coil_status_table(5) As Byte                            ' One more than number_of_coils/8
Dim Discrete_inputs_table(6) As Byte                        ' One more than number_of_inputs/8
Dim Holding_registers_table(numhr) As Word
Dim Input_registers_table(32) As Word

' Genereal program use
Dim For_loop As Byte
Dim Consultatmp As Word
Dim Volumentmp As Word

Dim Dir_slave As Byte                                       'Mantiene la dirección del esclavo
Dim Dir_slave_eep As Eram Byte

Dim Volumentmpeep As Eram Word

'

'Variables SERIAL0
Dim Ser_ini As Bit , Sernew As Bit
Dim Numpar As Byte
Dim Cmdsplit(32) As String * 10
Dim Serdata As String * 160 , Serrx As Byte , Serproc As String * 160



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

Serial1bytereceived:
   Load Timer1 , Load_timer
   Start Timer1
  ' Set Pinbug2
  ' Toggle Ledmdb
'   Set Inipol
Return

Modbus_space:
   'Set Pinbug
   Stop Timer1
   'Reset Pinbug2
   Call Modbus_exec
   'Reset Pinbug
Return

'*******************************************************************************



'*******************************************************************************
' TIMER0
'*******************************************************************************
Int_timer0:
   Timer0 = &H64
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

Return



Int_timer2:
   Timer0 = 184
   If Inidelay = 1 Then
      Incr Cntrdelay
      If Cntrdelay = Topdelay Then
         Set T0delay
         Reset Inidelay
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

Statusrx = 1
Print #1 , "MDBGPS RTU BRIDGE 2024"
Print #1 , Version(1)
Print #1 , Version(2)
Print #1 , Version(3)
Estado_led = 1
Status = 1



Coil_status_table(1) = 0
Coil_status_table(2) = 0
Coil_status_table(3) = 0
Coil_status_table(4) = 0
Coil_status_table(5) = 0

For For_loop = 1 To 32
   Holding_registers_table(for_loop) = For_loop
   Input_registers_table(for_loop) = For_loop
Next For_loop

Dir_slave = Dir_slave_eep
Print #1 , "Dir_Slave=" ; Dir_slave

Volumentmp = Volumentmpeep
Print #1 , "Volumen=" ; Volumentmp

If Volumentmp > 30 Then
   Volumentmp = 10
   Volumentmpeep = Volumentmp
End If

Holding_registers_table(numregvolumen) = Volumentmp



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




'************************************************************
'----------------- SUBS and Functions ---------------------
'************************************************************

Sub Send_exception
Local Crc As Word
   Mbuf_rcv(2) = Mbuf_rcv(2) + &H80
   Crc = Crcmb(mbuf_rcv(1) , 3)                             ' create checksum
   Mbuf_rcv(4) = Low(crc)                                   'add to buffer
   Mbuf_rcv(5) = High(crc)                                  'add to buffer
   'Set Dir176
   Printbin #2 , Mbuf_rcv(1) ; 5
   'Reset Dir176
End Sub

Sub Modbus_exec
 Local Chr_count As Byte
 Local Word_count As Byte
 Local Modbus_error As Byte
 Local Local_dummy_byte As Byte
 Local Local_dummy_byte_2 As Byte
 Local Local_dummy_word As Word
 Local Crc_received As Word
 Local Modbus_device_end_adress As Word
 Local No_bytes_to_send As Byte
 Local No_words_to_send As Byte
 Local Byte_to_send As Byte
 Local Word_to_send As Word

   ' Since this sub is executed after 3.5 character pause,
   ' all characters should be in receive buffer.
   ' Then let's put it in Modbus buffer
   Chr_count = 0
   While Ischarwaiting(#2) = 1
     Incr Chr_count
     Inputbin #2 , Local_dummy_byte
     Mbuf_rcv(chr_count) = Local_dummy_byte
     'Toggle Led1
   Wend
'   If Chr_count > 0 And Modbus_slave_adress_received = Modbus_slave_adress Then
   If Chr_count > 0 And Modbus_slave_adress_received = Dir_slave Then
         Local_dummy_byte = Chr_count - 1
         Crc_received = Makeint(mbuf_rcv(local_dummy_byte) , Mbuf_rcv(chr_count))       ' create word of received crc
         Decr Local_dummy_byte
         If Crc_received = Crcmb(mbuf_rcv(1) , Local_dummy_byte) Then
            Modbus_device_adress = Makeint(modbus_device_adress_lo , Modbus_device_adress_hi)
            Modbus_number_of_devices = Makeint(mb_number_of_devices_lo , Mb_number_of_devices_hi)
            Modbus_device_end_adress = Modbus_device_adress + Modbus_number_of_devices
            Modbus_error = 0
            ' First check if modbus packet is correct for this client
            Select Case Modbus_function
               Case F_read_coil_status :
                    If Modbus_device_end_adress > Maximum_coil_number Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                    End If
               Case F_read_discrete_inputs :
                     If Modbus_device_end_adress > Maximum_discrete_inputs Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                    End If
               Case F_read_holding_registers :
                     If Modbus_device_adress > Maximum_holding_registers Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                    End If
               Case F_read_input_registers :
                     If Modbus_device_adress > Maximum_input_registers Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                    End If
               Case F_write_single_coil :
                    If Modbus_device_adress > Maximum_coil_number Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                    End If
               Case F_write_multiple_coils:
                    If Modbus_device_end_adress > Maximum_coil_number Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                    End If
               Case F_write_single_register :
                     If Modbus_device_adress > Maximum_holding_registers Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                     End If
               Case F_read_single_adc:
                     If Modbus_device_adress > Maximum_adc_channel Then
                        Mbuf_rcv(3) = Illegal_data_address
                        Modbus_error = 1
                     End If
               Case Else                                    ' Ilegal fuction
                  Mbuf_rcv(3) = Illegal_function
                  Modbus_error = 1
            End Select
            If Modbus_error = 1 Then
               Send_exception
               Modbus_function = 0
            End If
            ' If packet is  is correct for this client then Modbus_function is <> 0
            Select Case Modbus_function
               Case 1 To 2 :
                  If Modbus_function = F_read_discrete_inputs Then Copy_inputs_to_table
                  No_bytes_to_send = Modbus_number_of_devices / 8
                  Local_dummy_byte = Modbus_number_of_devices Mod 8
                  If Local_dummy_byte > 0 Then Incr No_bytes_to_send
                  Chr_count = 0
                  For Local_dummy_word = Modbus_device_adress To Modbus_device_end_adress Step 8
                     Incr Chr_count
                     If Chr_count > No_bytes_to_send Then
                       Byte_to_send = 0
                     Else
                       Select Case Modbus_function
                           Case F_read_coil_status : Byte_to_send = Bit_index_byte(local_dummy_word , Coil_status_table(1))
                           Case F_read_discrete_inputs : Byte_to_send = Bit_index_byte(local_dummy_word , Discrete_inputs_table(1))
                        End Select
                     End If
                     Modbus_data_byte(chr_count) = Byte_to_send
                  Next
                  Mbuf_rcv(3) = No_bytes_to_send            ' byte count
                  Chr_count = 3 + No_bytes_to_send
                  Crc_received = Crcmb(mbuf_rcv(1) , Chr_count)       ' create checksum
                  Incr Chr_count
                  Mbuf_rcv(chr_count) = Low(crc_received)   'add to buffer
                  Incr Chr_count
                  Mbuf_rcv(chr_count) = High(crc_received)  'add to buffer
                  For Local_dummy_byte = 1 To Chr_count
                    'Set Dir176
                    Printbin #2 , Mbuf_rcv(local_dummy_byte) ; 1
                    'Reset Dir176
                  Next
               Case 3 To 4 :
                  No_words_to_send = Modbus_number_of_devices
                  Word_count = 0
                  Incr Modbus_device_adress                 ' ModBus start with adress 0, but table start with 1
                  Incr Modbus_device_end_adress
                  For Local_dummy_word = Modbus_device_adress To Modbus_device_end_adress
                     Incr Word_count
                     If Word_count > No_words_to_send Then
                       Word_to_send = 0
                     Else
                       Select Case Modbus_function
                           Case F_read_holding_registers : Word_to_send = Holding_registers_table(local_dummy_word)
                           Case F_read_input_registers : Word_to_send = Input_registers_table(local_dummy_word)
                        End Select
                     End If
                     Local_dummy_byte = High(word_to_send)
                     Local_dummy_byte_2 = Low(word_to_send)
                     Modbus_register_read(word_count) = Makeint(local_dummy_byte , Local_dummy_byte_2)
                  Next
                  Mbuf_rcv(3) = No_words_to_send * 2        ' byte count
                  Chr_count = Mbuf_rcv(3)
                  Chr_count = 3 + Chr_count
                  Crc_received = Crcmb(mbuf_rcv(1) , Chr_count)       ' create checksum
                  Incr Chr_count
                  Mbuf_rcv(chr_count) = Low(crc_received)   'add to buffer
                  Incr Chr_count
                  Mbuf_rcv(chr_count) = High(crc_received)  'add to buffer
                  For Local_dummy_byte = 1 To Chr_count
                     'Set Dir176
                    Printbin #2 , Mbuf_rcv(local_dummy_byte) ; 1
                    'Reset Dir176
                  Next
               Case F_write_single_coil :
                     If Mb_number_of_devices_hi = 255 Then
                        Call Write_single_coil(modbus_device_adress , 1)
                        'Set Dir176
                        Printbin #2 , Mbuf_rcv(1) ; 8
                        'Reset Dir176
                     Elseif Mb_number_of_devices_hi = 0 Then
                        Call Write_single_coil(modbus_device_adress , 0)
                        'Set Dir176
                        Printbin #2 , Mbuf_rcv(1) ; 8
                        'Reset Dir176
                     Else
                        Mbuf_rcv(3) = Illegal_data_value
                        Send_exception
                     End If
               Case F_write_multiple_coils:
                     Local_dummy_word = Modbus_device_adress
                     For Chr_count = 1 To Mb_f15_count
                        For Local_dummy_byte = 0 To 7
                           If Local_dummy_word < Modbus_device_end_adress Then
                              Call Write_single_coil(local_dummy_word , Mb_f15_data(chr_count).local_dummy_byte)
                              Incr Local_dummy_word
                           Else
                              Exit For
                           End If
                        Next
                     Next                                   'Local_dummy_word
                     Crc_received = Crcmb(mbuf_rcv(1) , 6)  ' create checksum
                     Mbuf_rcv(7) = Low(crc_received)        ' add to buffer
                     Mbuf_rcv(8) = High(crc_received)       ' add to buffer
                     'Set Dir176
                     Printbin #2 , Mbuf_rcv(1) ; 8
                     'Reset Dir176
               Case F_write_single_register:
                    Incr Modbus_device_adress               ' ModBus start with adress 0, but table start with 1
                    Holding_registers_table(modbus_device_adress) = Makeint(modbus_register_write_lo , Modbus_register_write_hi)
                    'Set Newmodbus
                    'Set Dir176
                    Printbin #2 , Mbuf_rcv(1) ; 8           ' Response is echo of query
                    'Reset Dir176
               Case F_read_single_adc:
                     ' This function is implemented only for demonstration purpose !!! It does nothing !
                     ' User can select and implement a function code that is not supported by specification
                     ' The query message specifies the channel in word Modbus_device_adress (bytes 3 and 4)
                     ' where ADC=0 mean ADC channel number 1,  etc ADC=1 mean ADC channel number 2 etc                    '
                     ' Byte 1: Slave ID
                     ' Byze 2: Function ADC read
                     ' Byte 3: ADC channel HI byte
                     ' Byte 4: ADC channel LO byte
                     ' Byte 5: CRC LO
                     ' Byte 6: CRC HI
                     '
                     ' HERE IMPLEMENT ADC conversion code
                     '
                     Mbuf_rcv(3) = 0                        ' Sample data
                     Mbuf_rcv(4) = 0                        ' sample data
                     '                  '
                     ' The response message contain 16bit ADC vaule in format
                     ' Byte 1: Slave ID
                     ' Byze 2: Function
                     ' Byte 3: ADC conversion value HI byte
                     ' Byte 4: ADC conversion value LO byte
                     ' Byte 5: CRC LO
                     ' Byte 6: CRC HI
                       Crc_received = Crcmb(mbuf_rcv(1) , 4)       ' create checksum
                      Mbuf_rcv(5) = Low(crc_received)       ' add to buffer
                      Mbuf_rcv(6) = High(crc_received)      ' add to buffer
                      'Set Dir176
                      Printbin #2 , Mbuf_rcv(1) ; 6
                      'Reset Dir176
            End Select
         End If
   End If
End Sub Modbus_exec

'*******************************************************************************
' This is user configurable.
' Remark lines which are not in modbus client
Sub Copy_inputs_to_table
   Discrete_inputs_table(1).0 = Input_1
   Discrete_inputs_table(1).1 = Input_2
   Discrete_inputs_table(1).2 = Input_3
   Discrete_inputs_table(1).3 = Input_4
'(
   Discrete_inputs_table(1).4 = Input_5
   Discrete_inputs_table(1).5 = Input_6
   Discrete_inputs_table(1).6 = Input_7
   Discrete_inputs_table(1).7 = Input_8

   Discrete_inputs_table(2).0 = Input_9
   Discrete_inputs_table(2).1 = Input_10
   Discrete_inputs_table(2).2 = Input_11
   Discrete_inputs_table(2).3 = Input_12
   Discrete_inputs_table(2).4 = Input_13
   Discrete_inputs_table(2).5 = Input_14
   Discrete_inputs_table(2).6 = Input_15
   Discrete_inputs_table(2).7 = Input_16
')
End Sub

' This is user configurable.
' Drive outputs to match modbus client hardware
Sub Write_single_coil(byval Coil As Word , Byval Value As Byte)
   Select Case Coil
      Case 0 :
         Coil_1 = Value : Coil_status_table(1).0 = Value.0
'(
      Case 1:
         Coil_2 = Value : Coil_status_table(1).1 = Value.0
      Case 2:
         Coil_3 = Value : Coil_status_table(1).2 = Value.0
      Case 3:
         Coil_4 = Value : Coil_status_table(1).3 = Value.0
      Case 4:
         Coil_5 = Value : Coil_status_table(1).4 = Value.0
      Case 5:
         Coil_6 = Value : Coil_status_table(1).5 = Value.0
      Case 6:
         Coil_7 = Value : Coil_status_table(1).6 = Value.0
      Case 7:
         Coil_8 = Value : Coil_status_table(1).7 = Value.0
      Case 8:
         Coil_9 = Value : Coil_status_table(2).0 = Value.0
      Case 9:
         Coil_10 = Value : Coil_status_table(2).1 = Value.0
')
   End Select
End Sub
'*******************************************************************************

' Return byte from array at adress of bit_index
Function Bit_index_byte(byval Bit_index As Word , Lista() As Byte) As Byte
   Local Dummy_byte_sub As Byte
   Local Rest As Byte
   Local Index_byte As Byte
   Local Start_byte As Byte

   Dummy_byte_sub = 0
   Rest = 0
   Index_byte = 0
   Start_byte = 0

   Rest = Bit_index Mod 8
   Start_byte = Bit_index / 8
   Incr Start_byte
   Index_byte = Lista(start_byte)
   Shift Index_byte , Left , Rest
   Incr Start_byte
   Dummy_byte_sub = Lista(start_byte)
   Rest = 8 - Rest
   Shift Dummy_byte_sub , Right , Rest
   Bit_index_byte = Index_byte Or Dummy_byte_sub
End Function



'*******************************************************************************
' Procesamiento de comandos
'*******************************************************************************
Sub Procser()
   'Print #1 , Serproc
   'Tmpstr52 = Mid(serproc , 1 , 6)
   Numpar = Split(serproc , Cmdsplit(1) , ",")
   'Print #1 , "numpar=" ; Numpar
'   If Numpar > 0 Then
'      For Tmpb = 1 To Numpar
'         Print #1 , Tmpb ; ":" ; Cmdsplit(tmpb)
'      Next
'   End If

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

         Case "SETDIR"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               Cmderr = 0
               Dir_slave = Tmpb
               Dir_slave_eep = Dir_slave
               Atsnd = "Se configuro direccion de esclavo a " + Str(dir_slave)
            Else
               Cmderr = 4
            End If

         Case "LEEDIR"
            Cmderr = 0
            Atsnd = "Direccion de escalvo RTU " + Str(dir_slave)

         Case "LEEHDR"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb <= 72 Then
                  Cmderr = 0
                  Tmpw = Holding_registers_table(tmpb)
                  Atsnd = "HDR" + Str(tmpb) + "=" + Hex(tmpw)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETHDR"
            Cmderr = 0
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb <= 72 Then
                  Cmderr = 0
                  Tmpw = Hexval(cmdsplit(3))
                  Holding_registers_table(tmpb) = Tmpw
                  Atsnd = "HDR" + Str(tmpb) + "=" + Hex(tmpw)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "SETIPR"
            Cmderr = 0
            If Numpar = 3 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb <= 32 Then
                  Cmderr = 0
                  Tmpw = Hexval(cmdsplit(3))
                  Input_registers_table(tmpb) = Tmpw
                  Atsnd = "IPR" + Str(tmpb) + "=" + Hex(tmpw)
               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If


         Case "LEERHR"
            Cmderr = 0
            Set Inileerhr
            Atsnd = "Leer HDR"

         Case "SETGPS"
            If Numpar = 2 Then
               Tmpb = Val(cmdsplit(2))
               If Tmpb < 2 Then
                  Cmderr = 0
                  If Tmpb = 1 Then
                     Set Enagps
                  Else
                     Reset Enagps
                  End If
                  Atsnd = "Enagps=" + Str(tmpb)

               Else
                  Cmderr = 5
               End If
            Else
               Cmderr = 4
            End If

         Case "LEEVOL"
            Cmderr = 0
            Atsnd = "Volumen=" + Str(volumentmp)

         Case "SETVOL"
            If Numpar = 2 Then
               Cmderr = 0
               Volumentmp = Val(cmdsplit(2))
               If Volumentmp > 30 Then
                  Volumentmp = 29
               End If
               Volumentmpeep = Volumentmp
               Atsnd = "Se config VOL=" + Str(volumentmp)

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



Loaded_arch: