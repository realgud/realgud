//!/usr/bin/env node
var util = require("util");
require("console");

function ask(question, format, callback) {
 var stdin = process.stdin, stdout = process.stdout;

 stdin.resume();
 stdout.write(question + ": ");

 stdin.once('data', function(data) {
   data = data.toString().trim();

   if (format.test(data)) {
     callback(data);
   } else {
     stdout.write("It should match: "+ format +"\n");
     ask(question, format, callback);
   }
 });
}

// GCD. We assume positive numbers
function gcd(a, b) {
    // Make: a <= b
    if (a > b) {
	var temp = a;
	a = b;
	b = temp;
    }

    if (a <= 0) { return null };

    if (a == 1 || b-a == 0) {
	return a;
    }
    return gcd(b-a, a);
}

var a=24;

ask("GCD of 24 and", /^\d+$/, function(b_str) {
    var b = parseInt(b_str, 10);
    console.log(util.format("The GCD of %d and %d is %d", a, b,
			    gcd(a, b)));
    process.exit();
});
