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
    procedure When_getIP4NetworkByAdrAndMask_DottedDecimal_withInvalidInputs_Then_ShowErrorMsg;
    procedure When_getIP4NetworkByAdrAndMask_Cidr_withInvalidInputs_Then_ShowErrorMsg;

    procedure When_isValidIP4Network_DottedDecimal_Expect_True;
    procedure When_isNotValidIP4Network_DottedDecimal_Expect_False;
    procedure When_isValidIP4Network_Cidr_Expect_True;
    procedure When_isNotValidIP4Network_Cidr_Expect_False;
    procedure When_isValidIP4Network_withInvalidInputs_Expect_False;

    procedure When_isValidIP4Host_DottedDecimal_Expect_True;
    procedure When_isNotValidIP4Host_DottedDecimal_Expect_False;
    procedure When_isValidIP4Host_Cidr_Expect_True;
    procedure When_isNotValidIP4Host_Cidr_Expect_False;
    procedure When_isValidIP4Host_withInvalidInputs_Expect_False;

    procedure When_getDefaultNetmaskByIP4adr_Then_ProvideNetmask;
    procedure When_getDefaultNetmaskByIP4adr_withInvalidInput_Then_ShowErrorMsg;
  end;

implementation

procedure NetworkCalculatorTest.When_isValidIP4_Expect_True;
begin
  AssertEquals('With a valid ip as input', True, isValidIP4('192.168.56.0'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4_Expect_False;
begin
  AssertEquals('With a non valid ip as input', False, isValidIP4('.168.56.4'));
end;


procedure NetworkCalculatorTest.When_getIP4NetworkByAdrAndMask_DottedDecimal_Then_ConvertToNetwrkadr;
begin
  AssertEquals('With ip and netmask in dotted decimal notation as input, provide network address', '198.48.0.0', getIP4NetworkByAdrAndMask('198.51.100.223', '255.240.0.0'));
end;

procedure NetworkCalculatorTest.When_getIP4NetworkByAdrAndMask_Cidr_Then_ConvertToNetwrkadr;
begin
  AssertEquals('With ip and netmask in cidr notation as input, provide network address', '198.48.0.0', getIP4NetworkByAdrAndMask('198.51.100.223', '12'));
end;

procedure NetworkCalculatorTest.When_getIP4NetworkByAdrAndMask_DottedDecimal_withInvalidInputs_Then_ShowErrorMsg;
begin
  AssertEquals('With invalid ip as input, expected error message', 'invalid inputs', getIP4NetworkByAdrAndMask('198..100.223', '255.240.0.0'));
end;

procedure NetworkCalculatorTest.When_getIP4NetworkByAdrAndMask_Cidr_withInvalidInputs_Then_ShowErrorMsg;
begin
  AssertEquals('With invalid netmask as input, expected error message', 'invalid inputs', getIP4NetworkByAdrAndMask('198.51.100.223', '42'));
end;


procedure NetworkCalculatorTest.When_isValidIP4Network_DottedDecimal_Expect_True;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid network', True, isValidIP4Network('192.168.0.0', '255.255.255.0'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Network_DottedDecimal_Expect_False;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid network', False, isValidIP4Network('198.51.100.223', '255.255.248.0'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Network_Cidr_Expect_True;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid network', True, isValidIP4Network('192.168.0.0', '24'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Network_Cidr_Expect_False;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid network', False, isValidIP4Network('198.51.100.223', '21'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Network_withInvalidInputs_Expect_False;
begin
  AssertEquals('With invalid ip or netmask as input, expected false', False, isValidIP4Network('198.51.100.223', '41'));
end;


procedure NetworkCalculatorTest.When_isValidIP4Host_DottedDecimal_Expect_True;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid host', True, isValidIP4Host('198.51.104.254', '255.255.248.0'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Host_DottedDecimal_Expect_False;
begin
  AssertEquals('For an ip with netmask in dotted decimal notation as input, check whether ip is a valid host', False, isValidIP4Host('192.168.0.0', '255.255.255.0'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Host_Cidr_Expect_True;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid host', True, isValidIP4Host('198.51.104.254', '21'));
end;

procedure NetworkCalculatorTest.When_isNotValidIP4Host_Cidr_Expect_False;
begin
  AssertEquals('For an ip with netmask in cidr notation as input, check whether ip is a valid host', False, isValidIP4Host('192.168.0.0', '24'));
end;

procedure NetworkCalculatorTest.When_isValidIP4Host_withInvalidInputs_Expect_False;
begin
  AssertEquals('With invalid ip or netmask as input, expected false', False, isValidIP4Host('192..0.1', '24'));
end;


procedure NetworkCalculatorTest.When_getDefaultNetmaskByIP4adr_Then_ProvideNetmask;
begin
  AssertEquals('For a given ip, provide default netmask', '255.255.0.0', getDefaultNetmaskByIP4adr('128.42.5.4'));
end;

procedure NetworkCalculatorTest.When_getDefaultNetmaskByIP4adr_withInvalidInput_Then_ShowErrorMsg;
begin
  AssertEquals('With an invalid ip as input, expected error message', 'IP is invalid', getDefaultNetmaskByIP4adr('128.256.5.4'));
end;

initialization

  RegisterTest(NetworkCalculatorTest);
end.

