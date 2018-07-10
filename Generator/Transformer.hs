module Generator.Transformer where
import Validator.Validator
import Parser.AST 
import Generator.JSAST
import Data.Maybe
import Data.List.Split

lookup_record_by_first:: [(String,Expr)] -> String -> String
lookup_record_by_first [] _first = ""
lookup_record_by_first (x:xs) _first = 
    case x of
        (str , value) -> 
            if str == _first 
            then (case value of 
                    Var v ->  v
                    Int i ->  show i
                    Str s ->  s 
                    List l -> cat_libraries "" l 
                )
            else lookup_record_by_first xs _first
-------------------------------------------------------------------
cat_libraries :: String -> [Expr] -> String
cat_libraries temp [] = temp
cat_libraries temp (x:xs) = 
    case x of 
        Str s ->  if temp == "" 
                  then cat_libraries ( temp++s) xs
                  else cat_libraries ( temp++ "," ++ s) xs

generate_lib :: [JSState] -> [String] -> Maybe [JSState]
generate_lib temp [] = Just temp 
generate_lib temp (x:xs) = 
    let var_name = (case x of 
                    "system-sleep" -> "sleep"
                    "node-dht-sensor" -> "dht_sensor"
                    "raspi-sensors" -> "RaspiSensors" 
                    otherwise -> "no-defined library"
                   )
        state = [ JSVariable [(JSVarInitExpr (JSIdentifier var_name) (JSMemberExpr (JSIdentifier "require") [(JSStringLiteral x)]) )] JSSemiAuto]
    in generate_lib (temp ++ state) xs

generate_libs :: String -> [JSState]
generate_libs str = 
    let libs = splitOn "," str
    in case generate_lib [] libs of 
        Nothing -> []
        Just states -> states


generate_header:: [(String,Expr)]  -> [JSState]
generate_header li = generate_libs $ lookup_record_by_first li "_library" 
----------------------------------------------------------------

generate_device:: [(String,Expr)]  -> [JSState]
generate_device li = 
    let pin_num = lookup_record_by_first li "_device_pin_number"
        device_name = lookup_record_by_first li "_device_name"
        device_state = lookup_record_by_first li "_device_state"
    in case pin_num of 
        "NULL" -> []
        otherwise -> [(JSVariable [(JSVarInitExpr (JSIdentifier "Gpio") (JSCallExprDot (JSMemberExpr (JSIdentifier "require") [(JSStringLiteral "onoff")] ) (JSIdentifier "Gpio") ))] JSSemiAuto), (JSVariable [(JSVarInitExpr (JSIdentifier device_name)  (JSMemberNew (JSIdentifier "Gpio") [(JSDecimal pin_num),(JSStringLiteral "out")] ) )] JSSemiAuto),(JSMethodCall (JSMemberDot (JSIdentifier device_name) (JSIdentifier "writeSync")) [(JSDecimal device_state)] JSSemiAuto)]
----------------------------------------------------------------


declare_sensor :: [String] -> [JSState]
declare_sensor parm= 
    let s_name = (parm!!0)
        s_type = (parm!!1)
        s_desc = (parm!!2)
        s_addr = (parm!!3)
    in [JSVariable [(JSVarInitExpr (JSIdentifier s_name) (JSMemberNew (JSMemberDot (JSIdentifier "RaspiSensors") (JSIdentifier "Sensor") ) [(JSProperty [("type",(JSStringLiteral s_type)),
    ("address", (JSDecimal s_addr))]),(JSStringLiteral s_desc)] ) )] JSSemiAuto]
-----------------------------------------------------




while_loop_transform :: [String] -> [Decl] -> [String] -> [JSState]
while_loop_transform parm trees fun_names = 
    let interval = (parm!!4)
        loop_middle = []--[analyse_View fun_names trees]
    in [JSWhile (JSLiteral "true") (JSStateBlock (loop_middle ++ [(JSMethodCall (JSIdentifier "sleep") [(JSDecimal interval)] JSSemiAuto)]) JSSemiAuto) ]


--[_name,_type,_desc,_addr]
generate_sensor_IC2 :: [String] ->[Decl] -> [String] -> [JSState]
generate_sensor_IC2 parm trees fun_names = --"IC2"
    let declare = declare_sensor parm
        while_loop = while_loop_transform parm trees fun_names
    in declare ++ while_loop 


generate_sensor:: [(String,Expr)]  -> [Decl] -> [String] -> [JSState]
generate_sensor li trees fun_names = 
    let s_name = lookup_record_by_first li "_sensor_name" 
        s_type = lookup_record_by_first li "_sensor_type" 
        s_plug = lookup_record_by_first li "_plug_type" 
        s_desc = lookup_record_by_first li "_sensor_description" 
        s_addr = lookup_record_by_first li "_sensor_address"  
        interval = lookup_record_by_first li "_interval"
    in case s_addr of 
        "NULL" -> []
        otherwise -> 
            case s_plug of
                "IC2" -> generate_sensor_IC2 [s_name,s_type,s_desc,s_addr,interval] trees fun_names 
                "GPIO" -> []
        --[(JSVariable [JSIdentifier "hui"] JSSemiAuto)]




transform_Model :: [Decl] -> [String] -> JSAST
transform_Model trees fun_names = 
    let model_declare = find_func_by_name (fun_names!!0) trees
    in case model_declare of 
        Just (Definition a b (Record li)) -> 
            (let  header = generate_header li 
                  device = generate_device li
                  sensor = generate_sensor li trees fun_names 
             in JSAstProgram (header ++ device ++ sensor))
        Nothing -> JSAstProgram []


-----------------------------------------------------
transformer :: [Decl] -> [String] -> JSAST
transformer trees fun_names =  transform_Model trees fun_names
    
    
  --  JSAstProgram []