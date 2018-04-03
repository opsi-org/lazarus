# spec file for package opsi-script
#
# Copyright (c) 2018 uib GmbH.
# This file and all modifications and additions to the pristine
# package are under the same license as the package itself.
#

Name:           opsi-script
Url:            http://www.opsi.org
License:        AGPLv3+
Group:          Productivity/Networking/Opsi
AutoReqProv:    on
Version:        4.12.0.14
Release:        2
Summary:        opsi script
%define tarname opsi-script
Source:         opsi-script_4.12.0.14-2.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-build
BuildArch:      noarch

%define toplevel_dir %{name}-%{version}

# ===[ description ]================================
%description
The opsi script binaries to execute opsiscript files

# ===[ debug_package ]==============================
%debug_package

# ===[ prep ]=======================================
%prep

# ===[ setup ]======================================
%setup -n %{tarname}-%{version}

# ===[ build ]======================================
%build

# ===[ install ]==================================== 
%install

mkdir -p $RPM_BUILD_ROOT/usr/bin
cp -a opsi-script	$RPM_BUILD_ROOT/usr/bin/opsi-script
cp -a opsi-script-nogui	$RPM_BUILD_ROOT/usr/bin/opsi-script-nogui

# ===[ clean ]======================================
%clean
rm -rf $RPM_BUILD_ROOT

# ===[ post ]=======================================
%post

# ===[ postun ]=====================================
%postun

# ===[ files ]======================================
%files

# default attributes
%defattr(-,root,root)

/usr/bin/opsi-script
/usr/bin/opsi-script-nogui
# documentation
#%doc LICENSE README RELNOTES doc

# ===[ changelog ]==================================
%changelog
