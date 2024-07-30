EESchema Schematic File Version 4
EELAYER 30 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 4
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Sheet
S 5700 2550 700  650 
U 65503ECD
F0 "matrix-2" 50
F1 "matrix.sch" 50
F2 "DIN" I L 5700 2650 50 
F3 "SCLK" I L 5700 2750 50 
$EndSheet
$Sheet
S 5700 3450 700  650 
U 65503F83
F0 "matrix-3" 50
F1 "matrix.sch" 50
F2 "DIN" I L 5700 3550 50 
F3 "SCLK" I L 5700 3650 50 
$EndSheet
$Comp
L power:+5V #PWR0101
U 1 1 65511450
P 1700 1100
F 0 "#PWR0101" H 1700 950 50  0001 C CNN
F 1 "+5V" H 1715 1273 50  0000 C CNN
F 2 "" H 1700 1100 50  0001 C CNN
F 3 "" H 1700 1100 50  0001 C CNN
	1    1700 1100
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0102
U 1 1 655120BF
P 1700 1400
F 0 "#PWR0102" H 1700 1150 50  0001 C CNN
F 1 "GND" H 1705 1227 50  0000 C CNN
F 2 "" H 1700 1400 50  0001 C CNN
F 3 "" H 1700 1400 50  0001 C CNN
	1    1700 1400
	1    0    0    -1  
$EndComp
Text GLabel 750  650  0    50   Input ~ 0
5V
Text GLabel 750  800  0    50   Input ~ 0
GND
Text Label 1000 650  0    50   ~ 0
5V
Text Label 1000 800  0    50   ~ 0
GND
Wire Wire Line
	750  650  1000 650 
Wire Wire Line
	750  800  1000 800 
Text Label 3600 1100 0    50   ~ 0
5V
Text Label 3600 1400 0    50   ~ 0
GND
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 6552655C
P 2200 1100
F 0 "#FLG0101" H 2200 1175 50  0001 C CNN
F 1 "PWR_FLAG" H 2200 1273 50  0000 C CNN
F 2 "" H 2200 1100 50  0001 C CNN
F 3 "~" H 2200 1100 50  0001 C CNN
	1    2200 1100
	1    0    0    -1  
$EndComp
Wire Wire Line
	1700 1100 2200 1100
Connection ~ 2200 1100
Wire Wire Line
	1700 1400 2200 1400
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 65559DF6
P 2200 1400
F 0 "#FLG0102" H 2200 1475 50  0001 C CNN
F 1 "PWR_FLAG" H 2200 1573 50  0000 C CNN
F 2 "" H 2200 1400 50  0001 C CNN
F 3 "~" H 2200 1400 50  0001 C CNN
	1    2200 1400
	1    0    0    -1  
$EndComp
Connection ~ 2200 1400
Wire Wire Line
	2050 6650 2150 6650
Wire Wire Line
	2050 7150 2050 7300
Wire Wire Line
	2150 7300 2050 7300
Text Label 2150 7300 0    50   ~ 0
GND
Text Label 2150 6650 0    50   ~ 0
5V
Wire Wire Line
	2050 6750 2050 6650
Wire Wire Line
	2050 7150 2650 7150
Connection ~ 2050 7150
Wire Wire Line
	2050 7050 2050 7150
Wire Wire Line
	2650 7150 2650 7050
Wire Wire Line
	1400 7150 2050 7150
Wire Wire Line
	1400 7050 1400 7150
Wire Wire Line
	2050 6750 2650 6750
Connection ~ 2050 6750
Wire Wire Line
	2050 6850 2050 6750
Wire Wire Line
	2650 6750 2650 6850
Wire Wire Line
	1400 6750 2050 6750
Wire Wire Line
	1400 6850 1400 6750
$Comp
L Device:CP_Small C?
U 1 1 66DF486E
P 2650 6950
AR Path="/64FB7FC8/66DF486E" Ref="C?"  Part="1" 
AR Path="/66DF486E" Ref="C3"  Part="1" 
F 0 "C3" H 2738 6996 50  0000 L CNN
F 1 "47u" H 2738 6905 50  0000 L CNN
F 2 "Capacitor_THT:CP_Radial_D10.0mm_P2.50mm_P5.00mm" H 2650 6950 50  0001 C CNN
F 3 "~" H 2650 6950 50  0001 C CNN
	1    2650 6950
	1    0    0    -1  
$EndComp
$Comp
L Device:CP_Small C?
U 1 1 66DF4874
P 2050 6950
AR Path="/64FB7FC8/66DF4874" Ref="C?"  Part="1" 
AR Path="/66DF4874" Ref="C2"  Part="1" 
F 0 "C2" H 2138 6996 50  0000 L CNN
F 1 "10u" H 2138 6905 50  0000 L CNN
F 2 "Capacitor_SMD:C_1206_3216Metric" H 2050 6950 50  0001 C CNN
F 3 "~" H 2050 6950 50  0001 C CNN
	1    2050 6950
	1    0    0    -1  
$EndComp
$Comp
L Device:CP_Small C?
U 1 1 66DF487A
P 1400 6950
AR Path="/64FB7FC8/66DF487A" Ref="C?"  Part="1" 
AR Path="/66DF487A" Ref="C1"  Part="1" 
F 0 "C1" H 1488 6996 50  0000 L CNN
F 1 "100n" H 1488 6905 50  0000 L CNN
F 2 "Capacitor_SMD:C_0805_2012Metric" H 1400 6950 50  0001 C CNN
F 3 "~" H 1400 6950 50  0001 C CNN
	1    1400 6950
	1    0    0    -1  
$EndComp
Wire Wire Line
	5700 1750 5600 1750
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E725E2
P 5400 1750
AR Path="/65503F83/66E725E2" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E725E2" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E725E2" Ref="S?"  Part="1" 
AR Path="/66E725E2" Ref="J1"  Part="1" 
F 0 "J1" H 5250 1600 50  0000 L CNN
F 1 "Conn_01x01" V 5363 1830 50  0001 L CNN
F 2 "sunflower:Link_Round" H 5400 1750 50  0001 C CNN
F 3 "~" H 5400 1750 50  0001 C CNN
	1    5400 1750
	-1   0    0    1   
$EndComp
Wire Wire Line
	5350 1850 5700 1850
$Comp
L Connector_Generic:Conn_01x01 J?
U 1 1 66E7F271
P 5150 1850
AR Path="/65503F83/66E7F271" Ref="J?"  Part="1" 
AR Path="/64FB7FC8/66E7F271" Ref="J?"  Part="1" 
AR Path="/65503ECD/66E7F271" Ref="J?"  Part="1" 
AR Path="/66E7F271" Ref="J2"  Part="1" 
F 0 "J2" H 5000 1700 50  0000 L CNN
F 1 "Conn_01x01" V 5113 1930 50  0001 L CNN
F 2 "sunflower:Link_Square" H 5150 1850 50  0001 C CNN
F 3 "~" H 5150 1850 50  0001 C CNN
	1    5150 1850
	-1   0    0    1   
$EndComp
$Sheet
S 5700 1650 700  650 
U 64FB7FC8
F0 "matrix-1" 50
F1 "matrix.sch" 50
F2 "DIN" I L 5700 1750 50 
F3 "SCLK" I L 5700 1850 50 
$EndSheet
Wire Wire Line
	5700 2650 5600 2650
$Comp
L Connector_Generic:Conn_01x01 J?
U 1 1 66E8797D
P 5400 2650
AR Path="/65503F83/66E8797D" Ref="J?"  Part="1" 
AR Path="/64FB7FC8/66E8797D" Ref="J?"  Part="1" 
AR Path="/65503ECD/66E8797D" Ref="J?"  Part="1" 
AR Path="/66E8797D" Ref="J3"  Part="1" 
F 0 "J3" H 5250 2500 50  0000 L CNN
F 1 "Conn_01x01" V 5363 2730 50  0001 L CNN
F 2 "sunflower:Link_Round" H 5400 2650 50  0001 C CNN
F 3 "~" H 5400 2650 50  0001 C CNN
	1    5400 2650
	-1   0    0    1   
$EndComp
Wire Wire Line
	5350 2750 5700 2750
$Comp
L Connector_Generic:Conn_01x01 J?
U 1 1 66E87988
P 5150 2750
AR Path="/65503F83/66E87988" Ref="J?"  Part="1" 
AR Path="/64FB7FC8/66E87988" Ref="J?"  Part="1" 
AR Path="/65503ECD/66E87988" Ref="J?"  Part="1" 
AR Path="/66E87988" Ref="J4"  Part="1" 
F 0 "J4" H 5000 2600 50  0000 L CNN
F 1 "Conn_01x01" V 5113 2830 50  0001 L CNN
F 2 "sunflower:Link_Square" H 5150 2750 50  0001 C CNN
F 3 "~" H 5150 2750 50  0001 C CNN
	1    5150 2750
	-1   0    0    1   
$EndComp
Wire Wire Line
	5700 3550 5600 3550
$Comp
L Connector_Generic:Conn_01x01 J?
U 1 1 66E89191
P 5400 3550
AR Path="/65503F83/66E89191" Ref="J?"  Part="1" 
AR Path="/64FB7FC8/66E89191" Ref="J?"  Part="1" 
AR Path="/65503ECD/66E89191" Ref="J?"  Part="1" 
AR Path="/66E89191" Ref="J5"  Part="1" 
F 0 "J5" H 5250 3400 50  0000 L CNN
F 1 "Conn_01x01" V 5363 3630 50  0001 L CNN
F 2 "sunflower:Link_Round" H 5400 3550 50  0001 C CNN
F 3 "~" H 5400 3550 50  0001 C CNN
	1    5400 3550
	-1   0    0    1   
$EndComp
Wire Wire Line
	5350 3650 5700 3650
$Comp
L Connector_Generic:Conn_01x01 J?
U 1 1 66E8919C
P 5150 3650
AR Path="/65503F83/66E8919C" Ref="J?"  Part="1" 
AR Path="/64FB7FC8/66E8919C" Ref="J?"  Part="1" 
AR Path="/65503ECD/66E8919C" Ref="J?"  Part="1" 
AR Path="/66E8919C" Ref="J6"  Part="1" 
F 0 "J6" H 5000 3500 50  0000 L CNN
F 1 "Conn_01x01" V 5113 3730 50  0001 L CNN
F 2 "sunflower:Link_Square" H 5150 3650 50  0001 C CNN
F 3 "~" H 5150 3650 50  0001 C CNN
	1    5150 3650
	-1   0    0    1   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E8B72A
P 2600 1600
AR Path="/65503F83/66E8B72A" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E8B72A" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E8B72A" Ref="S?"  Part="1" 
AR Path="/66E8B72A" Ref="GND1"  Part="1" 
F 0 "GND1" H 2450 1450 50  0000 L CNN
F 1 "Conn_01x01" V 2563 1680 50  0001 L CNN
F 2 "sunflower:Link_Square" H 2600 1600 50  0001 C CNN
F 3 "~" H 2600 1600 50  0001 C CNN
	1    2600 1600
	0    1    1    0   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E8D67E
P 2950 1600
AR Path="/65503F83/66E8D67E" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E8D67E" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E8D67E" Ref="S?"  Part="1" 
AR Path="/66E8D67E" Ref="GND2"  Part="1" 
F 0 "GND2" H 2800 1450 50  0000 L CNN
F 1 "Conn_01x01" V 2913 1680 50  0001 L CNN
F 2 "sunflower:Link_Square" H 2950 1600 50  0001 C CNN
F 3 "~" H 2950 1600 50  0001 C CNN
	1    2950 1600
	0    1    1    0   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E8D8DE
P 3300 1600
AR Path="/65503F83/66E8D8DE" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E8D8DE" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E8D8DE" Ref="S?"  Part="1" 
AR Path="/66E8D8DE" Ref="GND3"  Part="1" 
F 0 "GND3" H 3150 1450 50  0000 L CNN
F 1 "Conn_01x01" V 3263 1680 50  0001 L CNN
F 2 "sunflower:Link_Square" H 3300 1600 50  0001 C CNN
F 3 "~" H 3300 1600 50  0001 C CNN
	1    3300 1600
	0    1    1    0   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E8F49D
P 2600 900
AR Path="/65503F83/66E8F49D" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E8F49D" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E8F49D" Ref="S?"  Part="1" 
AR Path="/66E8F49D" Ref="VDD1"  Part="1" 
F 0 "VDD1" H 2450 750 50  0000 L CNN
F 1 "Conn_01x01" V 2563 980 50  0001 L CNN
F 2 "sunflower:Link_Round" H 2600 900 50  0001 C CNN
F 3 "~" H 2600 900 50  0001 C CNN
	1    2600 900 
	0    -1   -1   0   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E8FABB
P 2950 900
AR Path="/65503F83/66E8FABB" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E8FABB" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E8FABB" Ref="S?"  Part="1" 
AR Path="/66E8FABB" Ref="VDD2"  Part="1" 
F 0 "VDD2" H 2800 750 50  0000 L CNN
F 1 "Conn_01x01" V 2913 980 50  0001 L CNN
F 2 "sunflower:Link_Round" H 2950 900 50  0001 C CNN
F 3 "~" H 2950 900 50  0001 C CNN
	1    2950 900 
	0    -1   -1   0   
$EndComp
$Comp
L Connector_Generic:Conn_01x01 S?
U 1 1 66E90203
P 3300 900
AR Path="/65503F83/66E90203" Ref="S?"  Part="1" 
AR Path="/64FB7FC8/66E90203" Ref="S?"  Part="1" 
AR Path="/65503ECD/66E90203" Ref="S?"  Part="1" 
AR Path="/66E90203" Ref="VDD3"  Part="1" 
F 0 "VDD3" H 3150 750 50  0000 L CNN
F 1 "Conn_01x01" V 3263 980 50  0001 L CNN
F 2 "sunflower:Link_Round" H 3300 900 50  0001 C CNN
F 3 "~" H 3300 900 50  0001 C CNN
	1    3300 900 
	0    -1   -1   0   
$EndComp
Wire Wire Line
	2200 1100 2600 1100
Wire Wire Line
	2950 1100 3300 1100
Wire Wire Line
	3300 1100 3600 1100
Connection ~ 3300 1100
Wire Wire Line
	2950 1100 2600 1100
Connection ~ 2950 1100
Connection ~ 2600 1100
Wire Wire Line
	2200 1400 2600 1400
Wire Wire Line
	2600 1400 2950 1400
Connection ~ 2600 1400
Wire Wire Line
	2950 1400 3300 1400
Connection ~ 2950 1400
Wire Wire Line
	3300 1400 3600 1400
Connection ~ 3300 1400
$EndSCHEMATC
