# SISTEMA DE ALERTA TEMPRANA
Documentación RadioBridge 

## TARJETA RADIOBRIDGE
### BUGS HARDWARE
En el header de programación JP1 hay que intercambiar los pines de programación JP1-5 y JP1-6 que corresponden a las líneas RXD0 y TXD0 para que sea compatible con la configuración estándar. Se soluciono este bug con un adaptador que intercambaio estas dos líneas.<br>

El mosfet Q1 (SI2301CDS) es de tipo P. Se cambia por un mosfet N (FDN357).

El conector CN5 esta invertido (el footprint en JLCPCB no sigue asignación utilizada). Se desmonta y se suelda uno de cinco pines.

Se montan CN2, CN3 y CN6 (no había stock en JLCPCB). Hay también que soldar un zocalo DIP de 18 pines (para el DTMF HT9170B) y montar el intergado en este zócalo.

<img width="1000" alt="Bugs HW" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/Bug_HW.jpg">

### PROGRAMACION BOOTLOADER
Utilizando un programador ISP compatibles con USBASP https://www.fischl.de/usbasp/  se carga este programa teniendo en cuenta que los pines de RXDO y TXDO se encuentran ionvertidos por los que se uyiliza un adapatador como el mostrado en https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/Bug_HW.jpg

El programa que hay que utilizar es https://github.com/Ferivas/RADIOBRIDGE/blob/main/BASCOM/Bootloader/BootLoaderATMEGA128A_PC5_16MHz.hex el cual es el bootloader de MCS electronics (AVR BAscom) configurado a 38400 bps.
En el BASCOM se escoge la opción de "USBASP programmer" y cuando se programa el micro también es necesario programar los fusibles como se indica en la figura siguiente:

<img width="1000" alt="Bugs HW" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/BASCOM/Bootloader/Fuses_USBASP.jpg">


### PROGRAMACION NORMAL
Las actualizaciones de programa se instalan por el puerto serial con un  cable USB micro en el conector USB1 de la tarjeta. Se utiliza el BASCOM o el programa Bootloader de MCSelectronics ( https://www.mcselec.com/index.php?option=com_docman&task=doc_download&gid=153&Itemid=54 ) 
Se escoge el puerto detectado en la PC a la velocidad de 38400bps y se descarga este programa https://github.com/Ferivas/RADIOBRIDGE/blob/main/BASCOM/MAINBOARD/RadioBridge_M128.hex

### COMANDOS IMPORTANTES RADIOBRIDGE
#### CONFIGURACION INICIAL
Es necesario inicializar el equipo a los valores por defecto. Para est se utiliza el puerto serial configurado a 9600, 8,N,1 y con un programa terminal (se recomienda Hterm) se ingresa el siguiente comando<br>
*$RSTVAR*

#### CONFIGURACION DEL NUMERO DE ESTACION
Una vez programada la tarjeta el número de estación se puede configurar por el puerto serial configurado a 9600, 8,N,1 con <br>

*$setsta,Numestacion*

#### ALMACENAMIENTO DE LOS CANALES ADC DEL DISPOSITIVO I2C DE DRVAUDIO
Para configurar los valores de los parámetros I2C se utiliza el comando

*LEEI2C,Numcanal;Valcanal*

Este comando se recibe de la tarjeta del DRVAUDIO y almacena los valores de los canlaes ADC del dispositvo I2C del DRVAUDIO en una tabla Vali2c(4) que alamcena estos valores para comparar. Numcanal varía entre 1 y 4

#### LECTURA DE LOS VALORES DE LOS CANALES ADC DEL DISPOSITIVO I2C DE DRVAUDIO
*VALI2C,Numcanal*

Numcanal varia entre 1 y 4

#### CONFIGURACION DE VALORES DE COMPARACION DE CANALES ADC
Para poder trasnmitir el estado de los canales ADC se compara el valor recibido del DRVAUDIO con un valor previamente configurado en el RADIOBRIDGE, de manera que se pueda configurar bits si el valor leído del DRVAUDIO es mayor o menor que este valor de compración. Para esto se utiliza el comando

*TOPI2C,Canal;Valor*

Y para leer estos valores configurados se utiliza el comando

*LEETOP,Canal*

### PRUEBA HW TX Y RX DE TONOS DTMF
Para este fin es necesario cortocircuitar temporalmente los terminales de los capacitores C27 y C29 en el caso del tarjeta RAdiobrige fabricada por JLCPCB en China (basada en el M128A).<br>
En el caso de las tarjetas antiguas se deben cortocircuitar los capacitores C18 (en la parte frontal) y C13 (en la parte poterior) de estas tarjetas.

## MASTER
El Master se implementa con un RadioBridge y una tarjeta adicional que maneja el Modbus
### PRUEBAS ESCLAVOS DESDE MASTER
Desde el Hterm enviar el siguiente comando a 9600,8,N,1

*$tststa,Numestacion,1,10*

donde Numestacion puede variar de 1 a 9 

El número de estación actual se puede consultar con <br>

*$leesta*

### CONFIGURACION TIEMPO DE TX EN ESTADO NORMAL
*$SETTXN,Tiempotxn* <br>
En donde Tiempotxn es el tiempo de tx en estado normal en segundos<br>
Para leer el valor configurado <br>
*$LEETXN,Tiempotxn*

  ### TRAMAS DE PRUEBA PARA EL MASTER
  Prueba estación 5

### PROGRAMACION CON AVRDUDE
Si se graba el bootloader URboot en los ATmega, se puede programarlos con un Raspberry donde se instalo avrdude utilizando esta línea de comando

*avrdude -p m1284p -c arduino -P /dev/ttyUSB0 -b 57600 -F -D -U flash:w:MBDGPS.hex -v*

En BASCOM se debe programar con Arduino a 57600 como se muestra en la figura siguiente 

<img width="600" alt="Prog BASCOM" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/Config_Prog_Urboot_M1284P.jpg">
  

  $RXDTMF,05,01,00,00,00,00,00,08,00,04,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,02

## TARJETA MODBUS
La tarjeta Modbus permite añadir un interfaz HMI para hacer test y señalizar el estado de las estaciones del SAT.
En estas tarjetas es necesario quitar el D9 y cambiar la resistencia R11 por una resistencia de 0 ohm
<img width="800" alt="Tarjeta Modbus" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/MODBUS/tarjeta_MBDGPS.jpg">



## DRVLED SAT
Se fabrico tarjeta en JLCPCB. se encontró que la posición del conector de temperatura y GPS estaban intercambiados por lo que fue necesario hacer los siguientes cambios:
* Remover R5
* Mover la R13 para desconectarla del pad inferior y unir el mismo al pin 2 de CN2
* Cortar la pista que llega al pin 2 de CN2
* Unir el extremo derecho de SB2 al pin 3 del conector CN4

Los cambios se muestran en las dos figuras siguientes:

<img width="1000" alt="Bugs DRVLED " src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/DRVLED_BUGHW1.jpg">

<img width="1000" alt="Bugs DRVLED 2" src="https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/DRVLED_BUGHW2.jpg">



## AMPLIFICADOR DE AUDIO
### Interfaz I2C
El integrado U5 es un interfaz I2C (PCF8591P) que tiene cuatro entradas analógicas y una salida analógica.
Las entradas analógicas se utilizan para:
* Analog 0 monitorea el voltaje de entrada a la unidad luego de ser encendida
* Analog 1 monitorea el sensor de temperatura para determinar sobretemperatura
* Analog 2 monitorea el drive de corriente de la señal de salida B
* Analog 2 monitorea el drive de corriente de la señal de salida A

  La salida analógica se utiliza para encender la salida del amplificador y configurar el modo sirena o el modo voz.
  * Si el voltaje de salida es 0V el amplificador está apagado
  * Si el voltaje de salida se configura a 3.5V se enciende el amplificador en modo sirena
  * Si el voltaje de salida es mayor a 4V el voltaje activa U1D y Q4. El interruptor del relé K4 y K6 el cual cambia las salidas al tap de  alto voltaje en la salida del transformador y pone al amplificador en modo voz.
 
 ### COMANDOS PARA LEER PARAMETROS I2C
 Para leer los canalaes ADC del interfaz I2C (PCF8591P) se utiliza el comando

 *LEEI2C,Numadc*

  En donde Numadc varai entre 1 y 4 para leer los canales 0 a 3

  Para escribir en el DAC se utiliza el comando

  *SETI2C,Valor*

  En donde valor varía entre 0 y 255

  ## REGISTROS MODBUS
  Los registros Modbus utilizados se detallan en el siguiente documento:

  https://github.com/Ferivas/RADIOBRIDGE/blob/main/DOCS/MODBUS%20PUNTEROS%20HOLDING%20REGISTERS.pdf

  ### SEÑALIZACION TIMEOUT EN REGISTRO MODBUS SATMASTER
  El SATMASTER permite señalizar si las estaciones han dejado de trasnmitir luego de un tiempo que se configura en la variable *Toptout** la cual se puede configurar desde el MASTER con el siguiente comando

  *SETTOU,TiempoTimeout* en donde el tiempo TiempoTout puede variar de 0 a 65535 segundos (3600s=1 hora es el valor por default)

  Si se reinicia el master el registro IPR(20) que almacena los timeout pone en 1 (timeout) cada uno de los bits que corresponden a las estaciones. Cada bit se resetea a 0 si se recibe una respuesta válida ded un esclavo.

  Para el leer el valor que está configurado por defecto se utiliza el comando

  *LEETOU*



  
  

  
 
    
