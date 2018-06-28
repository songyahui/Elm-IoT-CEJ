var RaspiSensors = require('raspi-sensors');
var Gpio = require('onoff').Gpio;
var fan = new Gpio(18, 'out');

var BMP180 = new RaspiSensors.Sensor({
    type    : 'BMP180',
    address : 0X77
}, 'Temperature_sensor');  

while (true) {
    BMP180.fetch(function(err, data) {
        if(err) {
            process.exit()
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


"Right (JSAstProgram [JSVariable (JSVarInitExpression (JSIdentifier 'RaspiSensors') [JSMemberExpression (JSIdentifier 'require',JSArguments (JSStringLiteral 'raspi-sensors'))]),JSVariable (JSVarInitExpression (JSIdentifier 'Gpio') [JSCallExpressionDot (JSMemberExpression (JSIdentifier 'require',JSArguments (JSStringLiteral 'onoff')),JSIdentifier 'Gpio')]),JSVariable (JSVarInitExpression (JSIdentifier 'fan') [JSMemberNew (JSIdentifier 'Gpio',JSArguments (JSDecimal '18',JSStringLiteral 'out'))]),JSVariable (JSVarInitExpression (JSIdentifier 'BMP180') [JSMemberNew (JSMemberDot (JSIdentifier 'RaspiSensors',JSIdentifier 'Sensor'),JSArguments (JSObjectLiteral [JSPropertyNameandValue (JSIdentifier 'type') [JSStringLiteral 'BMP180'],JSPropertyNameandValue (JSIdentifier 'address') [JSHexInteger '0X77']],JSStringLiteral 'Temperature_sensor'))]),JSWhile (JSLiteral 'true') (JSStatementBlock [JSMethodCall (JSMemberDot (JSIdentifier 'BMP180',JSIdentifier 'fetch'),JSArguments (JSFunctionExpression '' (JSIdentifier 'err',JSIdentifier 'data') (JSBlock [JSIf (JSIdentifier 'err') (JSStatementBlock [JSMethodCall (JSMemberDot (JSIdentifier 'process',JSIdentifier 'exit'),JSArguments ())]),JSIfElse (JSExpressionBinary ('>',JSIdentifier 'data',JSDecimal '30')) (JSStatementBlock [JSMethodCall (JSMemberDot (JSIdentifier 'fan',JSIdentifier 'writeSync'),JSArguments (JSDecimal '1')),JSSemicolon]) (JSStatementBlock [JSMethodCall (JSMemberDot (JSIdentifier 'fan',JSIdentifier 'writeSync'),JSArguments (JSDecimal '0')),JSSemicolon])])))),JSSemicolon,JSMethodCall (JSIdentifier 'sleep',JSArguments (JSDecimal '5000')),JSSemicolon])])"
