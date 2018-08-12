var sleep = require('system-sleep');

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

//console.log(model);
//console.log(msg[1]);


if (msg[0] == 'Temperature'){
 if (msg[1] < 20){
 return [ 'LOW', model[1] ];
}
 else if (msg[1] < 30){
 return [ 'MEDIUM', model[1] ];
}
 else return [ 'HIGH', model[1] ];
}

else if (msg[0] == 'Light'){

 if (msg[1] > 500){

 return [ model[0], 'DAY' ];
}
 else if (msg[1] > 200){

 return [ model[0], 'EVENING' ];
}
 else return [ model[0], 'NIGHT' ];
}
 else return model;
}

while (true){
 tsl2561.fetch(function(err, data) {
    model = update( [ 'Light', data.value/100 ], model );
    console.log(data.value/100);
});
 bmp180.fetch( function (err, num) {
    model = update( [ 'Temperature', num.value ], model );
    console.log(num.value);
});


fan_1.writeSync( control_fan( model ) );
buzzer_1.writeSync( control_buzzer( model ) );

 sleep(5000); // 5 seconds
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