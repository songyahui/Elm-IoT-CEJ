main = {  view = view, model = model,update = update }

model = { 
      _sensor_name = NULL
    , _sensor_type = BMP180
    , _plug_type = IC2
    , _sensor_description = "Temperature_sensor"
    , _sensor_address = 0X77
    ------------------------------
    , _device_pin_number = 18 
    , _device_name = fan
    , _device_state = 0     
    , _device_log_info = "Written to pin 18"
    --------------------------------
    , _interval = 5000
    , _library = [ "onoff" ,"raspi-sensors"]
}

type Msg = SetHigh String->Int->Int | SetLow 


-- update : List a -> Int -> Int 
update msg m = 
  case msg of 
    SetHigh -> {model | _device_state = 1} 
    SetLow ->  {model | _device_state = 0} 

onChange = if data > 30 then SetHigh else SetLow

view model = 
    io [][
        buzzer  [onChange] []
    ]
