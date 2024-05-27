Name:           asff
Version:        %{?VERSION}
Release:        1%{?dist}
Summary:        Ada executable and GNATstudio plugin
License:        GPL
URL:            http://example.com/
Source0:        %{name}-%{version}.tar.gz

%description
This package provides an Ada executable and a GNATstudio plugin.

%prep
%setup -q

%build
# No build step needed

%install
# Create installation directories
mkdir -p %{buildroot}%{_bindir}
mkdir -p %{buildroot}%{_datadir}/gnatstudio/plug-ins
mkdir -p %{buildroot}%{_sysconfdir}/bash_completion.d
mkdir -p %{buildroot}/etc/profile.d

# Copy the files to the installation directories
install -m 755 bin/asff %{buildroot}%{_bindir}/asff
install -m 644 plugin/asff.py %{buildroot}%{_datadir}/gnatstudio/plug-ins/asff.py
install -m 644 completion/asff_completion %{buildroot}%{_sysconfdir}/bash_completion.d/asff_completion
echo '#!/usr/bin/env bash' > %{buildroot}%{_sysconfdir}/profile.d/asff_env.sh
echo 'export GNATSTUDIO_CUSTOM_PATH+=":%{_datadir}/gnatstudio/plug-ins/"' >> %{buildroot}%{_sysconfdir}/profile.d/asff_env.sh


%files
%{_bindir}/asff
%{_datadir}/gnatstudio/plug-ins/asff.py
%{_sysconfdir}/bash_completion.d/asff_completion
%{_sysconfdir}/profile.d/asff_env.sh

%changelog
* Mon May 20 2024 Damien De Campos <damiensporting@gmail.com> - 1.0-1
- Initial package

