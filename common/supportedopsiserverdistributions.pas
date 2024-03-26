unit SupportedOpsiServerDistributions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSupportedDistribution = (
    AlmaLinux_8,
    AlmaLinux_9,
    Debian_10,
    Debian_11,
    Debian_12,
    openSUSE_Leap_15_4,
    openSUSE_Leap_15_5,
    openSUSE_Leap_15_6,
    OracleLinux_8,
    OracleLinux_9,
    RHEL_8,
    RHEL_9,
    RockyLinux_8,
    RockyLinux_9,
    SLE_15_SP1,
    SLE_15_SP2,
    SLE_15_SP3,
    SLE_15_SP4,
    SLE_15_SP5,
    Univention_4_4,
    Univention_5_0,
    xUbuntu_18_04,
    xUbuntu_20_04,
    xUbuntu_22_04,
    xUbuntu_24_04,
    other
    );


function GetSupportedDistribution(DistroName: string;
  DistroRelease: string): TSupportedDistribution;

const
  SupportedDistributionsInfoString =
    'AlmaLinux 8, AlmaLinux 9,' +
    #10 + 'Debian 10, Debian 11, Debian 12,' +
    #10 + 'openSUSE 15.4, openSUSE 15.5, openSUSE 15.6,' +
    #10 + 'OracleServer 8, OL 9, RedHatEnterprise 8, RHEL 9, Rocky 8, Rocky 9,' +
    #10 + 'SUSE 15.1, SUSE 15.2, SUSE 15.3, SUSE 15.4, SUSE 15.5,' +
    #10 + 'Ubuntu 18.04, Ubuntu 20.04, Ubuntu 22.04, Ubuntu 24.04,' + #10 +
    'Univention 4.4, Univention 5.0';

implementation

function GetSupportedDistribution(DistroName: string;
  DistroRelease: string): TSupportedDistribution;
begin
  Result := other;
  case lowerCase(DistroName) of
    'almalinux': // AlmaLinux has releases like 8.x
    begin
      case DistroRelease[1] of
        '8': Result := AlmaLinux_8;
        '9': Result := AlmaLinux_9;
      end;
    end;

    'debian':
    begin
      if Pos('10', DistroRelease) = 1 then Result := Debian_10;
      if Pos('11', DistroRelease) = 1 then Result := Debian_11;
      if Pos('12', DistroRelease) = 1 then Result := Debian_12;
    end;

    'opensuse':
    begin
      case DistroRelease of
        '15.4': Result := openSUSE_Leap_15_4;
        '15.5': Result := openSUSE_Leap_15_5;
        '15.6': Result := openSUSE_Leap_15_6;
      end;
    end;

    'oracleserver':
    if DistroRelease[1] = '8' then Result := OracleLinux_8;
    'ol':
    if DistroRelease[1] = '9' then Result := OracleLinux_9;

    'redhatenterprise':
      if DistroRelease[1] = '8' then Result := RHEL_8;
    'rhel':
      if DistroRelease[1] = '9' then Result := RHEL_9;

    'rocky':
    begin
      case DistroRelease[1] of
        '8': Result := RockyLinux_8;
        '9': Result := RockyLinux_9;
      end;
    end;

    'suse':
    begin
      case DistroRelease of
        '15.1': Result := SLE_15_SP1;
        '15.2': Result := SLE_15_SP2;
        '15.3': Result := SLE_15_SP3;
        '15.4': Result := SLE_15_SP4;
        '15.5': Result := SLE_15_SP5;
      end;
    end;

    'ubuntu':
    begin
      case DistroRelease of
        '18.04': Result := xUbuntu_18_04;
        '20.04': Result := xUbuntu_20_04;
        '22.04': Result := xUbuntu_22_04;
        '24.04': Result := xUbuntu_24_04;
      end;
    end;

    'univention':
    begin
      if Pos('4.4', DistroRelease) = 1 then Result := Univention_4_4;
      //Univention 5.0 Distro Release is 10
      if Pos('10', DistroRelease) = 1 then Result := Univention_5_0;
    end;
  end;

end;


end.
