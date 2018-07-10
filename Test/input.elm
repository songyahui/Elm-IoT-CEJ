buzzer_1 : Device;
buzzer_1 = {
     d_pin   = 18
    ,d_lib  = "onoff"
    ,d_func = "Gpio" 
    ,d_dir  = "out"
    };

fan_1 : Device ;
fan_1 = {
     d_pin  = 16
    ,d_lib  = "onoff"
    ,d_func = "Gpio"
    ,d_dir  = "out"
    };

bmp180 : Sensor;
bmp180 = {
     s_lib = "raspi-sensors"
    ,s_constFun = "Sensor"
    ,s_type = "BMP180"
    ,s_address = 0X77
    ,s_desc = "Temperature_sensor"
    } ;

tsl2561 : Sensor;
tsl2561 = {
     s_lib = "raspi-sensors"
    ,s_constFun = "Sensor"
    ,s_type = "TSL2561"
    ,s_address = 0X39
    ,s_desc = "LIGHT_sensor"
    };


iot_main = { model = model, view = view, update = update };

type TemperatureTyp  = HIGH|MEDIUM|LOW ;   --temperature type 
type LightTyp        = DAY|EVENING|NIGHT ;  --light type 

model : (TemperatureTyp , LightTyp);
model = (HIGH, DAY);

type Msg = Temperature Int | Light Int ;

update msg model = 
  case msg of 
    Temperature num -> 
             if num < 20 
             then (LOW, second model)
             else if num < 30 then (MEDIUM, second model)
             else (HIGH, second model)
    Light num -> 
             if num > 500 
             then (first model, DAY)
             else if num > 200 then (first model, EVENING)
             else (first model, NIGHT)
    otherwise -> model ;

view model = 
    iot [
        light onLightChange Light tsl2561 
        ,temperature onTemperatureChange Temperature bmp180 
    ]
    [
        fan control_fan model fan_1
        ,buzzer control_buzzer model buzzer_1
    ];

control_fan model = 
    case model of
        (HIGH, DAY)     -> 1 --SetHigh
        (HIGH, EVENING) -> 1 --SetHigh
        otherwise       -> 0 --SetLow;
;
control_buzzer model = 
    case model of
        (LOW, NIGHT)    -> 1 --SetHigh
        otherwise       -> 0 --SetLow;
;
