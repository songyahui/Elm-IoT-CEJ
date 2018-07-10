var buzzer_1_lib = require( 'onoff' ).Gpio;

var buzzer_1 = new buzzer_1_lib(18, 'out');

var fan_1_lib = require( 'onoff' ).Gpio;

var fan_1 = new fan_1_lib(16, 'out');

var bmp180_lib = require( 'raspi-sensors' );

var bmp180 = new bmp180_lib.Sensor({
 type : 'BMP180', address : 119 }, 'Temperature_sensor');

var tsl2561_lib = require( 'raspi-sensors' );

var tsl2561 = new tsl2561_lib.Sensor({
 type : 'TSL2561', address : 57 }, 'LIGHT_sensor');

var model = [ 'HIGH', 'DAY' ];

function update(msg, model){
if (msg[0] == Temperature){
 if (num < 20){
 return [ 'LOW', model[1] ];
}
 else if (num < 30){
 return [ 'MEDIUM', model[1] ];
}
 else return [ 'HIGH', model[1] ];
}
 else if (msg[0] == Light){
 if (num > 500){
 return [ model[0], 'DAY' ];
}
 else if (num > 200){
 return [ model[0], 'EVENING' ];
}
 else return [ model[0], 'NIGHT' ];
}
 else return model;
}

while (true){
 tsl2561.fetch( function (err, num){
update( [ 'Light', num ], model );} );
bmp180.fetch( function (err, num){
update( [ 'Temperature', num ], model );} );
fan_1.writeSync( control_fan( model ) );
buzzer_1.writeSync( control_buzzer( model ) );
}

function control_fan(model){
if ((model[0] == 'HIGH') && (model[1] == 'DAY')){
 return 1;
}
 else if ((model[0] == 'HIGH') && (model[1] == 'EVENING')){
 return 1;
}
 else return 0;
}

function control_buzzer(model){
if ((model[0] == 'LOW') && (model[1] == 'NIGHT')){
 return 1;
}
 else return 0;
}

