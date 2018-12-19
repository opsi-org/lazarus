unit networkcalctest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, osnetworkcalculator;

type

  NetworkCalculatorTest= class(TTestCase)
  published
    procedure When_isValidIP4_Expect_True;
    procedure When_isNotValidIP4_Expect_False;
    procedure When_getIP4NetworkByAdrAndMask_DottedDecimal_Then_ConvertToNetwrkadr;
    procedure When_getIP4NetworkByAdrAndMask_Cidr_Then_ConvertToNetwrkadr;
    procedure When_isValidIP4Network_DottedDecimal_Expect_True;
    procedure When_isNotValidIP4Network_DottedDecimal_Expect_False;
    procedure When_isValidIP4Network_Cidr_Expect_True;
    procedure When_isNotValidIP4Network_Cidr_Expect_False;
    procedure When_isValidIP4Host_DottedDecimal_Expect_True;
    procedure When_isNotValidIP4Host_DottedDecimal_Expect_False;
    procedure When_isValidIP4Host_Cidr_Expect_True;
    procedure When_isNotValidIP4Host_Cidr_Expect_False;
    procedure When_getDefaultNetmaskByIP4adr_Then_ProvideNetmask;
  end;

implementation

procedure NetworkCalculatorTest.When_isValidIP4_Expect_True;
begin
  AssertEquals('With a valid ip as input', True, osnetworkcalculator.isValidIP4('192.168.56.0'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4_Expect_False;
begin
  AssertEquals('With a non valid ip as input', False, osnetworkcalculator.isValidIP4('.168.56.4'));
end;

procedure NetworkCalculatorTest.When_getIP4NetworkByAdrAndMask_DottedDecimal_Then_ConvertToNetwrkadr;
begin
  AssertEquals('With ip and netmask in dotted decimal notation as input, provide network address', '198.48.0.0', osnetworkcalculator.getIP4NetworkByAdrAndMask('198.51.100.223', '255.240.0.0'));
end;

procedure NetworkCalculatorTest.When_getIP4NetworkByAdrAndMask_Cidr_Then_ConvertToNetwrkadr;
begin
  AssertEquals('With ip and netmask in cidr notation as input, , provide network address', '198.48.0.0', osnetworkcalculator.getIP4NetworkByAdrAndMask('198.51.100.223', '12'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Network_DottedDecimal_Expect_True;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid network', True, osnetworkcalculator.isValidIP4Network('192.168.0.0', '255.255.255.0'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Network_DottedDecimal_Expect_False;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid network', False, osnetworkcalculator.isValidIP4Network('198.51.100.223', '255.255.248.0'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Network_Cidr_Expect_True;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid network', True, osnetworkcalculator.isValidIP4Network('192.168.0.0', '24'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Network_Cidr_Expect_False;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid network', False, osnetworkcalculator.isValidIP4Network('198.51.100.223', '21'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Host_DottedDecimal_Expect_True;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid host', True, osnetworkcalculator.isValidIP4Host('198.51.104.254', '255.255.248.0'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Host_DottedDecimal_Expect_False;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid host', False, osnetworkcalculator.isValidIP4Host('192.168.0.0', '255.255.255.0'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Host_Cidr_Expect_True;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid host', True, osnetworkcalculator.isValidIP4Host('198.51.104.254', '21'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Host_Cidr_Expect_False;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid host', False, osnetworkcalculator.isValidIP4Host('192.168.0.0', '24'));
end;

procedure NetworkCalculatorTest.When_getDefaultNetmaskByIP4adr_Then_ProvideNetmask;
begin
  AssertEquals('For a given ip, provide default netmask', '255.255.0.0', osnetworkcalculator.getDefaultNetmaskByIP4adr('128.42.5.4'));
end;

initialization

  RegisterTest(NetworkCalculatorTest);
end.

