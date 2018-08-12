first (a, b) = a;

second (a,b) = b;

type alias Sensor =  {s_type: String, s_address:Int};

type alias Device =  {
    d_pin : Int -- 18
    ,d_lib: Maybe String -- onoff
    ,d_func : Maybe String --Gpio after onoff
    ,d_dir : Maybe String --in/out
    };

bmp180 : Sensor ;
bmp180 = {s_type = "BMP180", s_address = 77} ;

tsl2561 : Sensor;
tsl2561 = {s_type = "TSL2561", s_address = 39} ;

buzzer1 : Device ;
buzzer1 = {
    d_pin   = 18
    ,d_lib  = Just "onoff"-- onoff
    ,d_func = Just "Gpio" --
    ,d_dir  = Just "out" --out 
    };

--light1 : Device 
--light1 = {d_pin = 12}

--fan1 : Device 
--fan1 = {d_pin = 26}

--type Component = Sen Sensor | Dev Device
type SensorInput msg = LI msg Sensor;
--type TemperatureInput msg = TI msg Sensor
type IoTSystem a b = IS a b  ;
type IOSensors msg = IOS msg Sensor;
type IODevices msg = IOD msg Device;
type IOSignal = High | Low;

onLightChange: Int -> Msg -> Sensor ->  SensorInput Msg;
onLightChange f s = let m = f 1 in LI m s;

onTemperatureChange: Int->Msg -> Sensor ->  SensorInput Msg;
onTemperatureChange f s = let m = f 1 in LI m s;


--light : LightInput Msg  -> IOSensors (LightInput Msg)
light : a -> Sensor -> IOSensors a;
light a b =  IOS a b;

--temperature : TemperatureInput Msg -> IOSensors (TemperatureInput Msg)
temperature : a ->Sensor -> IOSensors a ;
temperature a b = IOS a b;

buzzer : IOSignal -> Device-> IODevices IOSignal;
buzzer a b = IOD a b;

iot : List IOSensors a -> List IODevices b -> IoTSystem List IOSensors a  List IODevices b;
iot a b = IS a b ;

iot_main : { model : ( TT, LT )
    , update : Msg -> ( TT, LT ) -> ( TT, LT )
    -- , view :
    --       ( TT, LT )
    --       -> IoTSystem
    --              List IOSensors Sensor -> SensorInput Msg
    --              List IODevices IOSignal
    };

iot_main = { model = model, view = view, update = update };

type TT = HIGH|MED|LOW;
type LT = DAY|EVE|NIGHT;

model: (TT , LT);
model = (HIGH, DAY);

type Msg = T Int | L Int ;

update msg m = 
  case msg of 
    T num -> if num < 27 
             then (LOW, second  m)
             else if num < 30 then (MED, second  m)
             else (HIGH, second  m)
    L num -> if num > 1000 
             then (first m , DAY)
             else if num > 500 then (first m , EVE)
             else (first m , NIGHT);

view m = 
    iot [
        light onLightChange L tsl2561 
        ,temperature onTemperatureChange  T bmp180 
    ]
    [
        buzzer alarm m buzzer1
    ]
    ;

alarm model = 
    case model of
        (LOW, DAY) -> High
        otherwise -> Low ;
