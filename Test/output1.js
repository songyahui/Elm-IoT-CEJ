var Gpio = require('onoff').Gpio;
var buzzer_1 = new Gpio(18, 'out');

var Gpio = require('onoff').Gpio;
var fan_1 = new Gpio(16, 'out');

var RaspiSensors = require('raspi-sensors');
var BMP180 = new RaspiSensors.Sensor({
    type    : 'BMP180',
    address : 0X77
}, 'Temperature_sensor');  

var RaspiSensors = require('raspi-sensors');
var TSL2561 = new RaspiSensors.Sensor({
    type    : "TSL2561",
    address : 0X39
}, "LIGHT_sensor");  

TEMPERATURE = HIGH;
LIHGT = DAY;

while (true) {
    BMP180.fetch(function(err, num) {
        if(err) {
            process.exit(1)
        }
        if (num < 27) {
            TEMPERATURE = LOW; 
        }
        else if (num < 30 ){
            TEMPERATURE = MED;
        }
        else {
            TEMPERATURE = HIGH;
        }
    }); 
    TSL2561.fetch(function(err, num) {
        if(err) {
            process.exit(1)
        }
        if (num > 1000) {
            LIHGT = DAY; 
        }
        else if (num > 500 ){
            LIHGT = EVE;
        }
        else {
            LIHGT = NIGHT;
        }
    }); 
    control_fan(TEMPERATURE, LIGHT);
    control_buzzer(TEMPERATURE, LIGHT);
    // sleep(5000);  --interval
}

function control_fan(temp, light) {
    if (temp == HIGH && light == DAY) {
        buzzer_1.writeSync(1);
    }
    else if (temp == HIGH && light == EVENING) {
        buzzer_1.writeSync(1);
    }
    else {
        buzzer_1.writeSync(0);
    }
}

function control_fan(temp, light) {
    if (temp == LOW && light == NIGHT) {
        fan_1.writeSync(1);
    }
    else {
        fan_1.writeSync(0);
    }
}