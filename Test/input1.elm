first (a, b) = a
second (a,b) = b

type SensorInput msg = SI msg Sensor
type IoTSystem   a b = IS a b  
type IOSensors   msg = IOS msg Sensor
type IODevices   msg = IOD msg Device
type IOSignal        = SetHigh | SetLow
type alias Sensor =  
    { s_type: String
    , s_address: Int
    }
type alias Device =  
    { d_pin : Int 
    , d_lib : Maybe String 
    , d_func: Maybe String 
    , d_dir : Maybe String 
    }

bmp180 : Sensor
bmp180 = {s_type = "BMP180", s_address = 77} 

tsl2561 : Sensor
tsl2561 = {s_type = "TSL2561", s_address = 39} 

light : a -> Sensor -> IOSensors a
light a b =  IOS a b

temperature : a -> Sensor -> IOSensors a 
temperature a b = IOS a b

buzzer : IOSignal -> Device-> IODevices IOSignal
buzzer a b = IOD a b

led : IOSignal -> Device-> IODevices IOSignal
led a b = IOD a b

fan : IOSignal -> Device-> IODevices IOSignal
fan a b = IOD a b
 
iot : List (IOSensors a) -> List (IODevices b) -> IoTSystem (List (IOSensors a))  (List (IODevices b) )
iot a b = IS a b 


buzzer_1 : Device 
buzzer_1 = {
     d_pin   = 18
    ,d_lib  = Just "onoff"
    ,d_func = Just "Gpio" 
    ,d_dir  = Just "out"
    }

led_1 : Device 
led_1 = {
     d_pin  = 12
    ,d_lib  = Just "onoff"
    ,d_func = Just "Gpio" 
    ,d_dir  = Just "out"
    }

fan_1 : Device 
fan_1 = {
     d_pin  = 16
    ,d_lib  = Just "onoff"
    ,d_func = Just "Gpio"
    ,d_dir  = Just "out"
    }

onLightChange : (Int -> Msg) -> Sensor ->  SensorInput Msg
onLightChange f s = let m = f 1 in SI m s

onTemperatureChange : (Int->Msg) -> Sensor ->  SensorInput Msg
onTemperatureChange f s = let m = f 1 in SI m s

------------------------------
iot_main = { model = model, view = view, update = update }

type TemperatureTyp  = HIGH|MEDIUM|LOW    --temperature type 
type LightTyp        = DAY|EVENING|NIGHT  --light type 

model : (TemperatureTyp , LightTyp)
model = (HIGH, DAY)

type Msg = Temperature Int | Light Int 

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

view model = 
    iot [
        light (onLightChange Light) tsl2561 
        ,temperature (onTemperatureChange Temperature) bmp180 
    ]
    [
        fan (control_fan model) fan_1
        ,buzzer (control_buzzer model) buzzer_1
    ]

control_fan model = 
    case model of
        (HIGH, DAY)     -> SetHigh
        (HIGH, EVENING) -> SetHigh
        otherwise       -> SetLow
control_buzzer model = 
    case model of
        (LOW, NIGHT)    -> SetHigh
        otherwise       -> SetLow