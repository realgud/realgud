//!/usr/bin/env node
var util = require("util");
require("console");

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

var a=3;
var b=5;
console.log(util.format("The GCD of %d and %d is %d", a, b, gcd(a, b)));
