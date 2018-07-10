var RaspiSensors = require('raspi-sensors');

var Gpio = require('onoff').Gpio;
var fan = new Gpio(18, 'out');
fan.writeSync(0);

var BMP180 = new RaspiSensors.Sensor({
    type    : 'BMP180',
    address : 0X77
}, 'Temperature_sensor');  

while (true) {
    BMP180.fetch(function(err, data) {
        if(err) {
            process.exit(1)
        }
        if (data > 30){
            fan.writeSync(1);
        }
        else {
            fan.writeSync(0);
        }
    }); 
    sleep(5000); 
}






1. A parser of JS
2. AST -> AST
3. generate the code

//"Right

 (AstProgram 
    [
        Variable (VarInitExpr (Identifier 'RaspiSensors') [MemberExpr (Identifier 'require',Args (StringLiteral 'raspi-sensors'))])
        
        ,Variable (VarInitExpr (Identifier 'Gpio') [CallExprDot (MemberExpr (Identifier 'require',Args (StringLiteral 'onoff')),Identifier 'Gpio')])
        
        ,Variable (VarInitExpr (Identifier 'fan') [MemberNew (Identifier 'Gpio',Args (Decimal '18',StringLiteral 'out'))])
        
        ,Variable (VarInitExpr (Identifier 'BMP180') [MemberNew (MemberDot (Identifier 'RaspiSensors',Identifier 'Sensor'),Args (ObjectLiteral [PropertyNameandValue (Identifier 'type') [StringLiteral 'BMP180'],PropertyNameandValue (Identifier 'address') [HexInteger '0X77']],StringLiteral 'Temperature_sensor'))])
        
        ,While (Literal 'true') 

            (StatementBlock 
                
            [MethodCall (MemberDot (Identifier 'BMP180',Identifier 'fetch'),Args (FunctionExpr '' (Identifier 'err',Identifier 'data') 
            
            (Block [If (Identifier 'err') (StatementBlock [MethodCall (MemberDot (Identifier 'process',Identifier 'exit'),Args ())])
            
            ,IfElse (ExprBinary ('>',Identifier 'data',Decimal '30')) (StatementBlock [MethodCall (MemberDot (Identifier 'fan',Identifier 'writeSync'),Args (Decimal '1')),Semicolon]) (StatementBlock [MethodCall (MemberDot (Identifier 'fan',Identifier 'writeSync'),Args (Decimal '0')),Semicolon])])
        
        
        
        ))),Semicolon
            
            ,MethodCall (Identifier 'sleep',Args (Decimal '5000')),Semicolon
        ]
)
Definition "view" [PVar "model"] 
    (
        Call "io" 
        
        [
            List [],
            List [Call "buzzer1" [List [Var "onChange"],List []],Call "buzzer2" [List [Var "SetHigh"],List []]]
        
        ]
    )