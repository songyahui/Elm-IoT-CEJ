first (a, b) = a

second (a,b) = b 

type alias Sensor =  {s_type: String, s_address:Int}
type alias Device =  {
    d_pin : Int -- 18
    ,d_lib:  String -- onoff
    ,d_func :  String --Gpio after onoff
    ,d_dir :  String --in/out
    }