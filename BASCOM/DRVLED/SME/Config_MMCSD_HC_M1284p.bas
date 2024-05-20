'-------------------------------------------------------------------------------
'                         Config_MMCSD_HC.BAS
'               Config File for MMC/SD/SDHC Flash Cards Driver
'        (c) 2003-2009 , MCS Electronics / V�gel Franz Josef
'-------------------------------------------------------------------------------
' Place MMCSD_HC.LIB in the LIB-Path of BASCOM-AVR installation
'
'Connection as following
'MMC    M128/M103
'1      MMC_CS PORTB.0
'2      MOSI PORTB.2
'3      GND
'4      +3.3V
'5      CLOCK PORTB.1
'6      GND
'7      MISO, PORTB.3

' you can vary MMC_CS on HW-SPI and all pins on SOFT-SPI, check settings
' ========== Start of user definable range =====================================

' you can use HW-SPI of the AVR (recommended) or a driver build in Soft-SPI, if
' the HW-SPI of the AVR is occupied by an other SPI-Device with different settings

' Declare here you SPI-Mode
' using HW-SPI:     cMMC_Soft = 0
' not using HW_SPI: cMMC_Soft = 1

Const Cmmc_soft = 0

#if Cmmc_soft = 0

' --------- Start of Section for HW-SPI ----------------------------------------

   ' define Chip-Select Pin
'   Config Pinb.0 = Output                                   ' define here Pin for CS of MMC/SD Card
   Mmc_cs Alias Portb.4
   Config Mmc_cs = Output
   Set Mmc_cs

   ' Define here SS Pin of HW-SPI of the CPU (f.e. Pinb.0 on M128)
'   Config Pinb.0 = Output                                   ' define here Pin of SPI SS
   Spi_ss Alias Portb.4
   Config Spi_ss = Output
   Set Spi_ss                                               ' Set SPI-SS to Output and High por Proper work of
                                                 ' SPI as Master

   ' HW-SPI is configured to highest Speed
   Config Spi = Hard , Interrupt = Off , Data Order = Msb , Master = Yes , Polarity = High , Phase = 1 , Clockrate = 4 , Noss = 1
'   Config Spi = Hard , Interrupt = Off , Data Order = Msb , Master = Yes , Polarity = High , Phase = 1 , Clockrate = 16 , Noss = 1
'   Spsr = 1                                     ' Double speed on ATMega128
   Spiinit


' --------- End of Section for HW-SPI ------------------------------------------

#else                                                       ' Config here SPI pins, if not using HW SPI

' --------- Start of Section for Soft-SPI --------------------------------------

   ' Chip Select Pin  => Pin 1 of MMC/SD
   Config Pinb.0 = Output
   Mmc_cs Alias Portb.0
   Set Mmc_cs

   ' MOSI - Pin  => Pin 2 of MMC/SD
   Config Pinb.2 = Output
   Set Pinb.2
   Mmc_portmosi Alias Portb
   Bmmc_mosi Alias 2

   ' MISO - Pin  => Pin 7 of MMC/SD
   Config Pinb.3 = Input
   Mmc_portmiso Alias Pinb
   Bmmc_miso Alias 3

   ' SCK - Pin  => Pin 1 of MMC/SD
   Config Pinb.1 = Output
   Set Pinb.1
   Mmc_portsck Alias Portb
   Bmmc_sck Alias 1

' --------- End of Section for Soft-SPI ----------------------------------------

#endif

' ========== End of user definable range =======================================


'==== Variables For Application ================================================
 Dim Mmcsd_cardtype As Byte                                 ' Information about the type of the Card
'   0 can't init the Card
'   1 MMC
'   2 SDSC Spec. 1.x
'   4 SDSC Spec. 2.0 or later
'  12 SDHC Spec. 2.0 or later

Dim Gbdriveerror As Byte                                    ' General Driver Error register
' Values see Error-Codes
'===============================================================================



' ==== Variables for Debug =====================================================
' You can remove remarks(') if you want check this variables in your application
Dim Gbdrivestatusreg As Byte                                ' Driver save here Card response
' Dim gbDriveErrorReg as Byte at GbdriveStatusReg overlay     '
' Dim gbDriveLastCommand as Byte                              ' Driver save here Last Command to Card
Dim Gbdrivedebug As Byte
' Dim MMCSD_Try As Byte                                        ' how often driver tried to initialized the card
'===============================================================================


'==== Driver internal variables ================================================
' You can remove remarks(') if you want check this variables in your application
' Dim _mmcsd_timer1 As Word
' Dim _mmcsd_timer2 As Word
'===============================================================================



' Error-Codes
Const Cperrdrivenotpresent = &HE0
Const Cperrdrivenotsupported = &HE1
Const Cperrdrivenotinitialized = &HE2

Const Cperrdrivecmdnotaccepted = &HE6
Const Cperrdrivenodata = &HE7

Const Cperrdriveinit1 = &HE9
Const Cperrdriveinit2 = &HEA
Const Cperrdriveinit3 = &HEB
Const Cperrdriveinit4 = &HEC
Const Cperrdriveinit5 = &HED
Const Cperrdriveinit6 = &HEE

Const Cperrdriveread1 = &HF1
Const Cperrdriveread2 = &HF2

Const Cperrdrivewrite1 = &HF5
Const Cperrdrivewrite2 = &HF6
Const Cperrdrivewrite3 = &HF7
Const Cperrdrivewrite4 = &HF8



$lib "MMCSD_HC.LIB"
$external _mmc
' Init the Card
Gbdriveerror = Driveinit()


' you can remark/remove following two Code-lines, if you dont't use MMCSD_GetSize()
$external Mmcsd_getsize
Declare Function Mmcsd_getsize() As Long


' you can remark/remove following two Code-lines, if you dont't use MMCSD_GetCSD()
' write result of function to an array of 16 Bytes
$external Mmcsd_getcsd
Declare Function Mmcsd_getcsd() As Byte


' you can remark/remove following two Code-lines, if you dont't use MMCSD_GetCID()
' write result of function to an array of 16 Bytes
$external Mmcsd_getcid
Declare Function Mmcsd_getcid() As Byte


' you can remark/remove following two Code-lines, if you dont't use MMCSD_GetOCR()
' write result of function to an array of 4 Bytes
$external Mmcsd_getocr
Declare Function Mmcsd_getocr() As Byte


' you can remark/remove following two Code-lines, if you dont't use MMCSD_GetSDStat
' write result of function to an array of 64 Bytes
$external Sd_getsd_status
Declare Function Sd_getsd_status() As Byte

' check the usage of the above functions in the sample MMCSD_Analysis.bas
' check also the MMC and SD Specification for the content of the registers CSD, CID, OCR and SDStat
