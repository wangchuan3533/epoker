var should = require('should');
var camelcase = require('uppercamelcase');
var decamelize = require('decamelize');

describe('camelcase', function () {
  it('camelcase "HELLO_WORLD" should return helloWorld', function () {
    camelcase('HELLO_WORLD').should.eql('HelloWorld');
  });
  it('decamelize "HelloWorld" \'s upper case should return HELLO_WORLD', function () {
    decamelize('HelloWorld', '_').toUpperCase().should.eql('HELLO_WORLD');
  });
});
