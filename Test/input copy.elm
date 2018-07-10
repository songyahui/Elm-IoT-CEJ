main = { model = model, view = view, update = update }

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
    , _library = [ "raspi-sensors"]
}
type Msg = SetHigh | SetLow 

-- update : List a -> Int -> Int 
update msg m = 
  case msg of 
    SetHigh -> {model | _device_state = 1} 
    SetLow ->  {model | _device_state = 0} 

yahui = if data > 30 then SetHigh else SetLow

view model = 
    io [][
        sensor  [onChange yahui] []
    ]


[
    Definition "main" [] (Record [("model",Var "model"),("view",Var "view"),("update",Var "update")]),
    
    Definition "model" [] (Record [("_sensor_name",Var "NULL"),("_sensor_type",Var "BMP180"),("_plug_type",Var "IC2"),("_sensor_description",Str "Temperature_sensor"),("_sensor_address",Int 119),("_device_pin_number",Int 18),("_device_name",Var "fan"),("_device_state",Int 0),("_device_log_info",Str "Written to pin 18"),("_interval",Int 5000),("_library",List [Str "onoff",Str "raspi-sensors"])]),
    
    Union "Msg" [] [("SetHigh",[TLambda (TVar "String") (TLambda (TVar "Int") (TVar "Int"))]),("SetLow",[])],
    
    Definition "update" [PVar "msg",PVar "m"] (Case (Var "msg") [(PVar "SetHigh",Update "model" [("_device_state",Int 1)]),(PVar "SetLow",Update "model" [("_device_state",Int 0)])]),
    
    Definition "onChange" [] (If [(Binops ">" (Var "data") (Int 30),Var "SetHigh")] (Var "SetLow")),
    
    Definition "view" [PVar "model"] (Call "io" [List [],List [Call "buzzer" [List [Call "onChange" []],List []]]])
    
]